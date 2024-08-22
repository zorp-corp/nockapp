//! # Crown
//!
//! The Crown library provides a set of modules and utilities for working with
//! the Sword runtime. It includes functionality for handling jammed nouns, kernels (as jammed nouns),
//! pilots (drivers of the Sword runtime), and various types and utilities that extend the various Sword
//! components.
//!
//! ## Modules
//!
//! - `kernel`: Traits that Crown apps implement to create their own kernels.
//! - `newt`: Utilities for working with Newt responses from a socket.
//! - `noun`: Extensions and utilities for working with Urbit nouns.
//! - `pilot`: Driver of the Sword runtime.
//! - `types`: Types used throughout the library.
//! - `utils`: Errors, misc functions and extensions.
//!
#![feature(trait_alias)]
pub mod kernel;
pub mod noun;
pub mod utils;

pub use bytes::*;
pub use kernel::input::{BaseCommands, Convert, KernelInput};
pub use noun::{AtomExt, JammedNoun, NounExt};
pub use sword::noun::Noun;
pub use utils::bytes::{ToBytes, ToBytesExt};
pub use utils::error::{CrownError, Result};

use std::path::PathBuf;

/// Returns the default data directory for the given Urbit name.
///
/// # Arguments
///
/// * `urbit_name` - A string slice that holds the name of the Urbit.
///
/// # Example
///
/// ```
///
/// use std::path::PathBuf;
/// use crown::default_data_dir;
/// let dir = default_data_dir("nus");
/// assert_eq!(dir, PathBuf::from("./.data.nus"));
/// ```
pub fn default_data_dir(urbit_name: &str) -> PathBuf {
    PathBuf::from(format!("./.data.{}", urbit_name))
}

/// Returns the default socket path for the given Urbit name.
///
/// # Arguments
///
/// * `urbit_name` - A string slice that holds the name of the Urbit.
///
/// # Example
///
/// ```
/// use std::path::PathBuf;
/// use crown::default_socket_path;
/// let path = default_socket_path("nus");
/// assert_eq!(path, PathBuf::from("./.data.nus/nc.sock"));
/// ```
pub fn default_socket_path(urbit_name: &str) -> PathBuf {
    PathBuf::from(format!("./.data.{}/nc.sock", urbit_name))
}

/// Returns the default pier path for the given Urbit name.
///
/// # Arguments
///
/// * `urbit_name` - A string slice that holds the name of the Urbit.
///
/// # Example
///
/// ```
/// use std::path::PathBuf;
/// use crown::default_pier_path;
/// let path = default_pier_path("nus");
/// assert_eq!(path, PathBuf::from("./.data.nus/pier"));
/// ```
pub fn default_pier_path(urbit_name: &str) -> PathBuf {
    PathBuf::from(format!("./.data.{}/pier", urbit_name))
}

/// Default snapshot interval for the Sword binary
pub const SNAPSHOT_INTERVAL: std::time::Duration = std::time::Duration::from_secs(120);

/// Default path for the Sword binary
pub fn default_sword_path() -> PathBuf {
    PathBuf::from("./sword")
}

/// Default size for the Nock stack (1 GB)
pub const DEFAULT_NOCK_STACK_SIZE: usize = 1 << 27;
