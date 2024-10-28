use crate::{JammedNoun, NounExt};
use bincode::{config::{self, Configuration}, encode_to_vec, Decode, Encode};
use bytes::Bytes;
use crc32fast::Hasher;
use sword::{jets::cold::{Cold, Nounable}, mem::NockStack, noun::{Noun, T}};
use thiserror::Error;
use std::path::PathBuf;
use tracing::{error, trace, warn};

pub struct Checkpoint {
    pub event_num: u64,
    pub arvo: Noun,
    pub cold: Cold,
}

impl Checkpoint {
    pub fn load(stack: &mut NockStack, jam: JammedCheckpoint) -> Result<Self, CheckpointError> {
        let cell = <Noun as NounExt>::cue_bytes(stack, &jam.jam.0).map_err(
            |_| CheckpointError::SwordInterpreterError
        )?.as_cell()?;
        let cold_mem = Cold::from_noun(stack, &cell.tail())?;
        let cold = Cold::from_vecs(stack, cold_mem.0, cold_mem.1, cold_mem.2);

        Ok(Self {
            event_num: jam.event_num,
            arvo: cell.head(),
            cold,
        })
    }
}

#[derive(Encode, Decode, PartialEq, Debug)]
pub struct JammedCheckpoint {
    pub checksum: u32,
    pub event_num: u64,
    pub jam: JammedNoun,
}

impl JammedCheckpoint {
    pub fn new(stack: &mut NockStack, event_num: u64, cold: &Cold, arvo: &Noun) -> Self {
        let cold_noun = cold.into_noun(stack);
        let cell = T(stack, &[*arvo, cold_noun]);
        let jam = JammedNoun::from_noun(stack, cell);
        let checksum = Self::checksum(event_num, &jam.0);
        Self { checksum, event_num, jam }
    }
    pub fn validate(&self) -> bool {
        self.checksum == Self::checksum(self.event_num, &self.jam.0)
    }
    pub fn encode(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        encode_to_vec(self, config::standard())
    }
    pub fn checksum(event_num: u64, jam: &Bytes) -> u32 {
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
    #[error("Sword interpret error")]
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

    pub fn get_checkpoint<'a>(&'a self, stack: &'a mut NockStack) -> Result<Checkpoint, CheckpointError> {
        let (chk_0, chk_1) = [&self.0, &self.1].map(Self::decode_jam).into();

        match (chk_0, chk_1) {
            (Ok(a), Ok(b)) => {
                let chosen = if a.event_num > b.event_num { a } else { b };
                trace!("Loading checkpoint with event_num: {}", chosen.event_num);
                Checkpoint::load(stack, chosen)
            }
            (Ok(c), Err(e)) | (Err(e), Ok(c)) => {
                warn!("{e}");
                Checkpoint::load(stack, c)
            }
            (Err(e1), Err(e2)) => {
                error!("{e1}");
                error!("{e2}");
                panic!("Error loading both checkpoints");
            },
        }
    }

    fn decode_jam(jam_path: &PathBuf) -> Result<JammedCheckpoint, CheckpointError> {
        let jam: Vec<u8> = std::fs::read(jam_path.as_path())?;

        let config = bincode::config::standard();
        let (checkpoint, _) = bincode::decode_from_slice::<JammedCheckpoint, Configuration>(&jam, config)?;

        if checkpoint.validate() {
            Ok(checkpoint)
        } else {
            Err(CheckpointError::InvalidChecksum(jam_path))
        }
    }

}
