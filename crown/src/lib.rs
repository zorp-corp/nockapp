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
mod drivers;
pub mod kernel;
pub mod nockapp;
pub mod noun;
pub mod utils;

pub use bytes::*;
pub use nockapp::NockApp;
pub use noun::{AtomExt, JammedNoun, NounExt};
pub use sword::noun::Noun;
pub use utils::bytes::{ToBytes, ToBytesExt};
pub use utils::error::{CrownError, Result};

pub use drivers::exit::exit as exit_driver;
pub use drivers::file::file as file_driver;
pub use drivers::http::http as http_driver;
pub use drivers::markdown::markdown as markdown_driver;
pub use drivers::npc::{npc_client as npc_client_driver, npc_listener as npc_listener_driver};
pub use drivers::one_punch::one_punch_man as one_punch_driver;

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
pub fn default_data_dir(dir_name: &str) -> PathBuf {
    PathBuf::from(format!("./.data.{}", dir_name))
}

/// Default size for the Nock stack (1 GB)
pub const DEFAULT_NOCK_STACK_SIZE: usize = 1 << 27;
