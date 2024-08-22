//! # Crown Types
//!
//! Defines types used throughout the library.
use crate::CrownError;

use crate::Result;
use bincode::{Decode, Encode};
use std::sync::{Arc, RwLock};
use sword::mem::NockStack;
use sword::noun::Cell;
use tokio::net::UnixStream;

///
/// # Convert
///
/// Trait that all commands must implement. Used to convert a
/// command representation to a command.
///
/// # Example
///
/// use crown::types::Convert;
/// use sword::mem::NockStack;
/// use sword::noun::Cell;
/// use crown::CrownError;
///
/// #[derive(Debug)]
/// pub enum BaseCommands {
///    Snapshot,
///    Exit,
/// }
/// impl Convert for BaseCommands {
///     fn convert(c: Cell, _: &mut NockStack) -> Result<Self> {
///         match c.to_string() {
///             "snapshot" => Ok(BaseCommands::Snapshot),
///             "exit" => Ok(BaseCommands::Exit),
///             _ => Err(CrownError::NewtError),
///         }
///     }
/// }
pub trait Convert
where
    Self: Sized + std::fmt::Debug,
{
    fn convert(command: Cell, stack: &mut NockStack) -> Result<Self>;
}

/// # SocketReply
///
/// Reply to a socket.
#[derive(Encode, Decode, PartialEq, Debug)]
pub enum SocketReply {
    Ack(u32),
    Nack(u32),
}

///
/// # SocketEvent
///
/// Event from a socket.
#[derive(Encode, Decode, PartialEq, Debug, Clone)]
pub enum SocketEvent {
    Poke(JammedNoun, u32),
    Peek(JammedNoun, u32),
}

///
/// # SocketData
///
/// Track metadata for a socket..
pub struct SocketData {
    pub socket: Arc<RwLock<UnixStream>>,
    pub id: u32,
    pub is_lick: bool,
}

///
/// # Effect
///
/// An event is handled by the kernel, while a command is handled by the process.
#[derive(Encode, Decode, Debug)]
pub enum Effect<C>
where
    C: Convert,
{
    Event(JammedNoun),
    Command(C),
}

///
/// # KernelInput
///
/// Input to the kernel.
#[derive(Encode, Decode, Debug)]
pub enum KernelInput {
    Event(EventInput),
    Fact(FactInput),
    Socket(SocketInput),
}

#[derive(Encode, Decode, Debug)]
pub struct EventInput(pub JammedNoun);

#[derive(Encode, Decode, Debug)]
pub struct FactInput(pub JammedNoun);

#[derive(Encode, Decode, Clone, Debug)]
pub struct SocketInput(pub SocketEvent);

#[derive(Encode, Decode, Debug)]
pub enum BaseCommands {
    Snapshot,
    Exit,
}

use std::cmp::Ordering;

use crate::JammedNoun;

impl PartialOrd for SocketData {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl Ord for SocketData {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialEq for SocketData {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for SocketData {}

impl Convert for BaseCommands {
    fn convert(command: Cell, _: &mut NockStack) -> Result<Self> {
        let c = command.head().as_atom()?;
        let cmd = std::str::from_utf8(c.as_bytes())?.trim_end_matches('\0');
        match cmd {
            "snapshot" => Ok(BaseCommands::Snapshot),
            "exit" => Ok(BaseCommands::Exit),
            _ => Err(CrownError::NewtError),
        }
    }
}

impl From<SocketEvent> for SocketInput {
    fn from(noun: SocketEvent) -> Self {
        SocketInput(noun)
    }
}

impl From<JammedNoun> for FactInput {
    fn from(noun: JammedNoun) -> Self {
        FactInput(noun)
    }
}

impl From<JammedNoun> for EventInput {
    fn from(noun: JammedNoun) -> Self {
        EventInput(noun)
    }
}
