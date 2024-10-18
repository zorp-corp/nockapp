//! # Crown
//!
//! The Crown library provides a set of modules and utilities for working with
//! the Sword runtime. It includes functionality for handling jammed nouns, kernels (as jammed nouns),
//! and various types and utilities that make sword easier to use.
//!
//! ## Modules
//!
//! - `kernel`: Sword runtime interface.
//! - `noun`: Extensions and utilities for working with Urbit nouns.
//! - `utils`: Errors, misc functions and extensions.
//!
#![feature(trait_alias)]
pub mod kernel;
pub mod noun;
pub mod utils;
pub mod nockapp;

pub use bytes::*;
pub use noun::{AtomExt, JammedNoun, NounExt};
pub use nockapp::NockApp;
pub use sword::noun::Noun;
pub use utils::bytes::{ToBytes, ToBytesExt};
pub use utils::error::{CrownError, Result};

use std::path::PathBuf;

/// Returns the directory where kernel data is stored.
///
/// # Arguments
///
/// * `dir` - A string slice that holds the kernel identifier.
///
/// # Example
///
/// ```
///
/// use std::path::PathBuf;
/// use crown::default_data_dir;
/// let dir = default_data_dir("crown");
/// assert_eq!(dir, PathBuf::from("./.data.crown"));
/// ```
pub fn default_data_dir(kernel_name: &str) -> PathBuf {
    PathBuf::from(format!("./.data.{}", kernel_name))
}

pub fn default_jam_paths(base_dir: &str) -> [PathBuf; 2] {
    create_jam_paths(base_dir, ["0", "1"])
}

fn create_jam_paths(base_dir: &str, names: [&str; 2]) -> [PathBuf; 2] {
    let base_path = crate::default_data_dir(base_dir);
    [
        base_path.join(format!("{}.jam", names[0])),
        base_path.join(format!("{}.jam", names[1])),
    ]
}

/// Default size for the Nock stack (1 GB)
pub const DEFAULT_NOCK_STACK_SIZE: usize = 1 << 27;
