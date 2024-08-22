use thiserror::Error;

#[derive(Debug, Error)]
pub enum ExternalError {
    #[error("unknown error: {0}")]
    UnknownError(anyhow::Error),
    #[error("conversion error: {0}")]
    ConversionError(String),
    // Add other common error variants as needed
}

#[derive(Debug, Error)]
pub enum CrownError<T = ExternalError> {
    #[error("external")]
    External(T),
    #[error("mutex error")]
    MutexError,
    #[error("invalid kernel input")]
    InvalidKernelInput,
    #[error("unknown effect")]
    UnknownEffect,
    #[error("io error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Crown NounError: {0}")]
    Noun(#[from] NounError),
    #[error("{0}")]
    InterpreterError(#[from] SwordError),
    #[error("kernel error")]
    KernelError(Option<sword::noun::Noun>),
    #[error("{0}")]
    Utf8FromError(#[from] std::string::FromUtf8Error),
    #[error("{0}")]
    Utf8Error(#[from] std::str::Utf8Error),
    #[error("sword load error")]
    SwordLoadError,
    #[error("newt error")]
    NewtError,
    #[error("newt")]
    Newt(#[from] anyhow::Error),
    #[error("boot error")]
    BootError,
    #[error("work bail")]
    WorkBail,
    #[error("peek bail")]
    PeekBail,
    #[error("work swap")]
    WorkSwap,
    #[error("tank error")]
    TankError,
    #[error("play bail")]
    PlayBail,
    #[error("queue error")]
    QueueRecv(yaque::TryRecvError),
    #[error("save error: {0}")]
    SaveError(String),
    #[error("try from int error: {0}")]
    IntError(#[from] std::num::TryFromIntError),
    #[error("join error: {0}")]
    JoinError(#[from] tokio::task::JoinError),
    #[error("decode error")]
    DecodeError(#[from] bincode::error::DecodeError),
    #[error("encode error")]
    EncodeError(#[from] bincode::error::EncodeError),
    #[error("unknown error: {0}")]
    Unknown(String),
    #[error("conversion error: {0}")]
    ConversionError(#[from] ConversionError),
    #[error("unknown error")]
    UnknownError(anyhow::Error),
    #[error("queue")]
    QueueError(#[from] QueueErrorWrapper),
}

#[derive(Debug)]
pub struct QueueErrorWrapper(pub yaque::TrySendError<Vec<u8>>);

#[derive(Debug, Error)]
pub struct SwordError(pub sword::interpreter::Error);

impl std::fmt::Display for SwordError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Sword Error: {}", self)
    }
}
impl From<sword::interpreter::Error> for CrownError {
    fn from(e: sword::interpreter::Error) -> Self {
        CrownError::InterpreterError(SwordError(e))
    }
}

impl From<sword::jets::JetErr> for CrownError {
    fn from(e: sword::jets::JetErr) -> Self {
        CrownError::InterpreterError(SwordError(sword::interpreter::Error::from(e)))
    }
}

impl std::fmt::Display for QueueErrorWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "queue error: {}", self.0)
    }
}

impl std::error::Error for QueueErrorWrapper {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl From<yaque::TrySendError<Vec<u8>>> for CrownError {
    fn from(e: yaque::TrySendError<Vec<u8>>) -> Self {
        CrownError::QueueError(QueueErrorWrapper(e))
    }
}

#[derive(Debug, Error)]
#[error("conversion error: {0}")]
pub enum ConversionError {
    TooBig(String),
}

#[derive(Debug, Error)]
pub struct NounError(pub sword::noun::Error);

impl std::fmt::Display for NounError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Noun Error: {}", self)
    }
}

impl From<sword::noun::Error> for CrownError {
    fn from(e: sword::noun::Error) -> Self {
        CrownError::Noun(NounError(e))
    }
}

impl<T, E: Into<CrownError>> IntoCrownError<T> for core::result::Result<T, E> {
    fn crown(self) -> core::result::Result<T, CrownError> {
        match self {
            Ok(val) => Ok(val),
            Err(e) => Err(e.into()),
        }
    }
}

pub trait IntoCrownError<T> {
    fn crown(self) -> core::result::Result<T, CrownError>;
}

pub type Result<V, E = CrownError<ExternalError>> = std::result::Result<V, E>;
