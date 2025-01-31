use std::future::Future;
use std::pin::Pin;

use tokio::sync::mpsc;
use tokio::task::JoinHandle;

// enumerate errors that actors may terminate with
pub enum ActorError {
    JoinError(tokio::task::JoinError),
}

impl From<tokio::task::JoinError> for ActorError {
    fn from(err: tokio::task::JoinError) -> Self {
        ActorError::JoinError(err)
    }
}

// result of an actor (when joining)
type ActorResult = Result<(), ActorError>;

// errors when sending a message to an actor
pub enum ActorSendError<M> {
    MpscSendError(mpsc::error::SendError<M>),
}

impl<M> From<mpsc::error::SendError<M>> for ActorSendError<M> {
    fn from(err: mpsc::error::SendError<M>) -> Self {
        ActorSendError::MpscSendError(err)
    }
}

// result when sending a message to an actor
pub type ActorSendResult<M> = Result<(), ActorSendError<M>>;

pub enum ActorReceiveError {
    MailboxClosed,
}

pub type ActorReceiveResult<M> = Result<M, ActorReceiveError>;

/// returned when an actor is spawned
pub struct ActorHandle<M> {
    handle: JoinHandle<()>,
    dropbox: Dropbox<M>,
}

impl<M> ActorHandle<M>
where
    M: Sync + Send,
{
    pub fn is_finished(&self) -> bool {
        self.handle.is_finished()
    }

    pub async fn join(self) -> ActorResult {
        Ok(self.handle.await?)
    }

    pub async fn send(&mut self, message: M) -> ActorSendResult<M> {
        self.dropbox.send(message).await
    }
    pub fn dropbox(&self) -> Dropbox<M> {
        self.dropbox.clone()
    }
}

pub struct Mailbox<M> {
    mailbox: mpsc::Receiver<M>,
}

impl<M> Mailbox<M>
where
    M: Send + Sync,
{
    pub async fn receive(&mut self) -> Result<M, ActorReceiveError> {
        self.mailbox
            .recv()
            .await
            .map_or_else(|| Err(ActorReceiveError::MailboxClosed), |x| Ok(x))
    }
    pub fn has_messages(&self) -> bool {
        !self.mailbox.is_empty()
    }
}

pub struct Dropbox<M> {
    dropbox: mpsc::Sender<M>,
}

impl<M: Send> Dropbox<M> {
    async fn send(&mut self, message: M) -> ActorSendResult<M> {
        Ok(self.dropbox.send(message).await?)
    }
}

impl<M> Clone for Dropbox<M> {
    fn clone(&self) -> Self {
        Dropbox {
            dropbox: self.dropbox.clone(),
        }
    }
}

pub fn spawn<F, M>(task: F, size: usize) -> ActorHandle<M>
where
    F: FnOnce(Mailbox<M>) -> Pin<Box<dyn ActorFuture>>,
{
    let (outbox, inbox) = mpsc::channel(size);
    let mailbox = Mailbox { mailbox: inbox };
    let handle = tokio::spawn(task(mailbox));
    let dropbox = Dropbox { dropbox: outbox };
    ActorHandle { handle, dropbox }
}

pub trait ActorFuture: Future<Output = ()> + Send {}
