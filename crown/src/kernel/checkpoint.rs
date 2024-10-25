use crate::{nockapp::NockAppError, JammedNoun, NounExt};
use bincode::{config, encode_to_vec, Decode, Encode};
use bytes::Bytes;
use crc32fast::Hasher;
use sword::{jets::cold::{Cold, Nounable}, mem::NockStack, noun::{Noun, T}};
use std::path::PathBuf;
use tracing::{trace, warn};

pub struct Checkpoint {
    pub event_num: u64,
    pub arvo: Noun,
    pub cold: Cold,
}

impl Checkpoint {
    pub fn load(stack: &mut NockStack, jam: JammedCheckpoint) -> Self {
        let cell = <Noun as NounExt>::cue_bytes(stack, &jam.jam.0).unwrap().as_cell().unwrap();
        let cold_mem = Cold::from_noun(stack, &cell.tail()).unwrap();
        let cold = Cold::from_vecs(stack, cold_mem.0, cold_mem.1, cold_mem.2);

        Self {
            event_num: jam.event_num,
            arvo: cell.head(),
            cold,
        }
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

#[derive(Debug, Clone)]
pub struct JamPaths(pub PathBuf, pub PathBuf);

impl JamPaths {
    pub fn new(dir: &PathBuf) -> Self {
        let path_0 = dir.join("0.jam");
        let path_1 = dir.join("1.jam");
        Self(path_0, path_1)
    }

    pub fn get_checkpoint(&self, stack: &mut NockStack) -> Option<Checkpoint> {
        let chk_0 = Self::decode_jam(&self.0);
        let chk_1 = Self::decode_jam(&self.1);

        match (chk_0, chk_1) {
            (Some(a), Some(b)) => {
                let chosen = if a.event_num > b.event_num { a } else { b };
                trace!("Loading checkpoint with event_num: {}", chosen.event_num);
                Some(Checkpoint::load(stack, chosen))
            }
            (Some(c), _) | (_, Some(c)) => Some(Checkpoint::load(stack, c)),
            _ => None,
        }
    }

    pub fn get_path_write(&self) -> Option<&PathBuf> {
        let chk_0 = Self::decode_jam(&self.0);
        let chk_1 = Self::decode_jam(&self.1);

        match (chk_0, chk_1) {
            (Some(a), Some(b)) => {
                if a == b {
                    trace!("Both jam checkpoints are identical, skipping save");
                    None
                }
                else if a.event_num < b.event_num { Some(&self.1) }
                else { Some(&self.0) }
            },
            (Some(_), None) => Some(&self.1),
            (None, Some(_)) => Some(&self.0),
            (None, None) => Some(&self.0)
        }
    }

    fn decode_jam(jam_path: &PathBuf) -> Option<JammedCheckpoint> {
        let jam: Vec<u8> = match std::fs::read(jam_path.as_path()) {
            Ok(jam) => jam,
            Err(e) => {
                warn!("Failed to read jam file: {}, {}", jam_path.display(), e);
                return None;
            }
        };

        let config = bincode::config::standard();
        let decoded: Result<(JammedCheckpoint, usize), NockAppError> =
            bincode::decode_from_slice(&jam, config).map_err(NockAppError::DecodeError);

        match decoded {
            Ok((checkpoint, _)) => {
                if checkpoint.validate() {
                    Some(checkpoint)
                } else {
                    warn!("Invalid checksum in jam file: {}", jam_path.display());
                    None
                }
            },
            Err(e) =>  {
                warn!("bincode: failed to decode jam: {:?}", e);
                None
            }
        }
    }

}
