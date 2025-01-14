use crate::{JammedNoun, NounExt};
use bincode::config::{self, Configuration};
use bincode::{encode_to_vec, Decode, Encode};
use blake3::{Hash, Hasher};
use bytes::Bytes;
use std::path::PathBuf;
use sword::jets::cold::{Cold, Nounable};
use sword::mem::NockStack;
use sword::noun::{Noun, T};
use sword_macros::tas;
use thiserror::Error;
use tracing::{debug, error, info, warn};

#[derive(Clone)]
pub struct Checkpoint {
    /// Magic bytes to identify checkpoint format
    pub magic_bytes: u64,
    /// Version of checkpoint
    pub version: u32,
    /// The buffer that this checkpoint was saved to, either 0 or 1.
    pub buff_index: bool,
    /// Hash of the boot kernel
    pub ker_hash: Hash,
    /// Event number
    pub event_num: u64,
    /// State of the kernel
    pub ker_state: Noun,
    /// Cold state
    pub cold: Cold,
}

impl Checkpoint {
    pub fn load(stack: &mut NockStack, jam: JammedCheckpoint) -> Result<Self, CheckpointError> {
        let cell = <Noun as NounExt>::cue_bytes(stack, &jam.jam.0)
            .map_err(|_| CheckpointError::SwordInterpreterError)?
            .as_cell()?;

        let cold_mem = Cold::from_noun(stack, &cell.tail())?;
        let cold = Cold::from_vecs(stack, cold_mem.0, cold_mem.1, cold_mem.2);

        Ok(Self {
            magic_bytes: jam.magic_bytes,
            version: jam.version,
            buff_index: jam.buff_index,
            ker_hash: jam.ker_hash,
            event_num: jam.event_num,
            ker_state: cell.head(),
            cold,
        })
    }
}

#[derive(Encode, Decode, PartialEq, Debug)]
pub struct JammedCheckpoint {
    /// Magic bytes to identify checkpoint format
    pub magic_bytes: u64,
    /// Version of checkpoint
    pub version: u32,
    /// The buffer this checkpoint was saved to, either 0 or 1
    pub buff_index: bool,
    /// Hash of the boot kernel
    #[bincode(with_serde)]
    pub ker_hash: Hash,
    /// Checksum derived from event_num and jam (the entries below)
    #[bincode(with_serde)]
    pub checksum: Hash,
    /// Event number
    pub event_num: u64,
    /// Jammed noun of [kernel_state cold_state]
    pub jam: JammedNoun,
}

impl JammedCheckpoint {
    pub fn new(
        stack: &mut NockStack,
        version: u32,
        buff_index: bool,
        ker_hash: Hash,
        event_num: u64,
        cold: &Cold,
        ker_state: &Noun,
    ) -> Self {
        let cold_noun = (*cold).into_noun(stack);
        let cell = T(stack, &[*ker_state, cold_noun]);
        let jam = JammedNoun::from_noun(stack, cell);
        let checksum = Self::checksum(event_num, &jam.0);
        Self {
            magic_bytes: tas!(b"JAM"),
            version,
            buff_index,
            ker_hash,
            checksum,
            event_num,
            jam,
        }
    }
    pub fn validate(&self) -> bool {
        self.checksum == Self::checksum(self.event_num, &self.jam.0)
    }
    pub fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        encode_to_vec(self, config::standard())
    }
    fn checksum(event_num: u64, jam: &Bytes) -> Hash {
        let jam_len = jam.len();
        let mut hasher = Hasher::new();
        hasher.update(&event_num.to_le_bytes());
        hasher.update(&jam_len.to_le_bytes());
        hasher.update(&jam);
        hasher.finalize()
    }
}

#[derive(Error, Debug)]
pub enum CheckpointError<'a> {
    #[error("IO error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Bincode error: {0}")]
    DecodeError(#[from] bincode::error::DecodeError),
    #[error("Invalid checksum at {0}")]
    InvalidChecksum(&'a PathBuf),
    #[error("Sword noun error: {0}")]
    SwordNounError(#[from] sword::noun::Error),
    #[error("Sword cold error: {0}")]
    FromNounError(#[from] sword::jets::cold::FromNounError),
    #[error("Sword interpreter error")]
    SwordInterpreterError,
}

#[derive(Debug, Clone)]
pub struct JamPaths(pub PathBuf, pub PathBuf);

impl JamPaths {
    pub fn new(dir: &PathBuf) -> Self {
        let path_0 = dir.join("0.jam");
        let path_1 = dir.join("1.jam");
        Self(path_0, path_1)
    }

    pub fn checkpoint_exists(&self) -> bool {
        self.0.exists() || self.1.exists()
    }

    // TODO return checkpoint and which buffer is being loaded so we can set the buffer toggle
    pub fn load_checkpoint<'a>(
        &'a self,
        stack: &'a mut NockStack,
    ) -> Result<Checkpoint, CheckpointError<'a>> {
        let (chk_0, chk_1) = [&self.0, &self.1].map(Self::decode_jam).into();

        match (chk_0, chk_1) {
            (Ok(a), Ok(b)) => {
                let chosen = if a.event_num > b.event_num {
                    debug!(
                        "Loading checkpoint at: {}, checksum: {}",
                        self.0.display(),
                        a.checksum
                    );
                    a
                } else {
                    debug!(
                        "Loading checkpoint at: {}, checksum: {}",
                        self.1.display(),
                        b.checksum
                    );
                    b
                };
                Checkpoint::load(stack, chosen)
            }
            (Ok(c), Err(e)) | (Err(e), Ok(c)) => {
                warn!("{e}");
                info!("Loading checkpoint, checksum: {}", c.checksum);
                Checkpoint::load(stack, c)
            }
            (Err(e1), Err(e2)) => {
                error!("{e1}");
                error!("{e2}");
                panic!("Error loading both checkpoints");
            }
        }
    }

    pub fn decode_jam(jam_path: &PathBuf) -> Result<JammedCheckpoint, CheckpointError> {
        let jam: Vec<u8> = std::fs::read(jam_path.as_path())?;

        let config = bincode::config::standard();
        let (checkpoint, _) =
            bincode::decode_from_slice::<JammedCheckpoint, Configuration>(&jam, config)?;

        if checkpoint.validate() {
            Ok(checkpoint)
        } else {
            Err(CheckpointError::InvalidChecksum(jam_path))
        }
    }
}
