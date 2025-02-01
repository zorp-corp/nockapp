use std::any::Any;
use std::future::Future;
use std::pin::Pin;

use tokio::sync::mpsc;

// enumerate errors that actors may terminate with
pub enum ActorError {
    JoinError(tokio::task::JoinError),
    BlockingJoinError(Box<dyn Any + Send + 'static>),
}

impl From<tokio::task::JoinError> for ActorError {
    fn from(err: tokio::task::JoinError) -> Self {
        ActorError::JoinError(err)
    }
}

impl From<Box<dyn Any + Send + 'static>> for ActorError {
    fn from(err: Box<dyn Any + Send + 'static>) -> Self {
        ActorError::BlockingJoinError(err)
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

// Errors that may occur when an actor tries to receive in its mailbox
pub enum ActorReceiveError {
    MailboxClosed,
}

// Result type for receiving a message in a mailbox
pub type ActorReceiveResult<M> = Result<M, ActorReceiveError>;

/// returned when an actor is spawned
pub struct ActorHandle<M> {
    handle: tokio::task::JoinHandle<()>,
    dropbox: Dropbox<M>,
    cancel: tokio_util::sync::CancellationToken,
}

impl<M> ActorHandle<M>
where
    M: Sync + Send,
{
    /// Has this actor finished running?
    pub fn is_finished(&self) -> bool {
        self.handle.is_finished()
    }

    /// Wait on this actor to finish
    pub async fn join(self) -> ActorResult {
        Ok(self.handle.await?)
    }

    /// Send a message to this actor's mailbox
    pub async fn send(&self, message: M) -> ActorSendResult<M> {
        self.dropbox.send(message).await
    }

    pub fn blocking_send(&self, message: M) -> ActorSendResult<M> {
        self.dropbox.blocking_send(message)
    }

    /// Clone this actor's dropbox so that other actors can send it messages
    pub fn dropbox(&self) -> Dropbox<M> {
        self.dropbox.clone()
    }

    /// Cancel this actor
    pub fn cancel(&self) {
        self.cancel.cancel()
    }
}

/// Handle to an actor's mailbox by which it can receive messages
pub struct Mailbox<M> {
    mailbox: mpsc::Receiver<M>,
    dropbox: Dropbox<M>,
    cancel: tokio_util::sync::CancellationToken,
}

impl<M> Mailbox<M>
where
    M: Send,
{
    /// Receive a message
    pub async fn receive(&mut self) -> Result<M, ActorReceiveError> {
        self.mailbox
            .recv()
            .await
            .map_or_else(|| Err(ActorReceiveError::MailboxClosed), Ok)
    }

    /// Check if there are any messages
    pub fn has_messages(&self) -> bool {
        !self.mailbox.is_empty()
    }

    /// Get a dropbox to send messages to this mailbox.
    ///
    /// Useful to allow an actor to get a dropbox which it may pass to spawned
    /// threads to allow them to send messages back.
    pub fn dropbox(&self) -> Dropbox<M> {
        self.dropbox.clone()
    }

    // Future which resolves when the actor is cancelled
    pub fn cancelled(&self) -> tokio_util::sync::WaitForCancellationFuture {
        self.cancel.cancelled()
    }

    // Has this actor been cancelled?
    pub fn is_cancelled(&self) -> bool {
        self.cancel.is_cancelled()
    }
}

/// Handle to send messages to an actor
///
/// This is separated from ActorHandle because only one actor can await another actor's completion,
/// but many actors might send it messages.
pub struct Dropbox<M> {
    dropbox: mpsc::Sender<M>,
}

impl<M: Send> Dropbox<M> {
    /// Send a message to the actor linked to this dropbox
    pub async fn send(&self, message: M) -> ActorSendResult<M> {
        Ok(self.dropbox.send(message).await?)
    }

    pub fn blocking_send(&self, message: M) -> ActorSendResult<M> {
        Ok(self.dropbox.blocking_send(message)?)
    }
}

impl<M> Clone for Dropbox<M> {
    fn clone(&self) -> Self {
        Dropbox {
            dropbox: self.dropbox.clone(),
        }
    }
}

/// Spawn an actor from a closure which takes a mailbox and returns a future.
///
/// Takes a size for the actor's mailbox which is the number of unread
/// messages that may be passed before sending messages will block.
pub fn spawn<F, M>(task: F, size: usize) -> ActorHandle<M>
where
    F: FnOnce(Mailbox<M>) -> Pin<Box<dyn Future<Output = ()> + Send>>,
{
    let cancel = tokio_util::sync::CancellationToken::new();
    let (outbox, inbox) = mpsc::channel(size);
    let dropbox = Dropbox { dropbox: outbox };
    let mailbox = Mailbox {
        mailbox: inbox,
        dropbox: dropbox.clone(),
        cancel: cancel.clone(),
    };
    let handle = tokio::spawn(task(mailbox));
    ActorHandle {
        handle,
        dropbox,
        cancel,
    }
}

/// Actors for blocking IO/computation
pub mod blocking {
    use super::{mpsc, ActorResult, Dropbox};
    use std::thread;

    /// Actor handle for a blocking actor
    pub struct ActorHandle<M> {
        handle: std::thread::JoinHandle<()>,
        dropbox: Dropbox<M>,
        cancel: tokio_util::sync::CancellationToken,
    }

    impl<M> ActorHandle<M>
    where
        M: Send + 'static,
    {
        /// Check if an actor is finished
        pub fn is_finished(&self) -> bool {
            self.handle.is_finished()
        }

        /// Join an actor (blocking)
        ///
        /// Blocks until this actor is finished
        pub fn join(self) -> ActorResult {
            Ok(self.handle.join()?)
        }

        /// Join an actor
        ///
        /// Joining the actor, but as a future
        pub async fn join_async(self) -> ActorResult {
            tokio::task::spawn_blocking(move || self.join()).await?
        }

        /// Send a message to this actor
        pub async fn send(&mut self, message: M) -> super::ActorSendResult<M> {
            self.dropbox.send(message).await
        }

        pub fn blocking_send(&self, message: M) -> super::ActorSendResult<M> {
            self.dropbox.blocking_send(message)
        }

        /// Get the dropbox for this actor.
        pub fn dropbox(&self) -> Dropbox<M> {
            self.dropbox.clone()
        }

        /// Cancel this actor
        pub fn cancel(&self) {
            self.cancel.cancel()
        }
    }

    /// Spawn a blocking closure as an actor
    pub fn spawn<F, M>(task: F, size: usize) -> ActorHandle<M>
    where
        F: FnOnce(Mailbox<M>) -> () + Send + 'static,
        M: Send + 'static,
    {
        let (outbox, inbox) = mpsc::channel(size);
        let cancel = tokio_util::sync::CancellationToken::new();
        let dropbox = Dropbox { dropbox: outbox };
        let mailbox = Mailbox {
            mailbox: inbox,
            dropbox: dropbox.clone(),
            cancel: cancel.clone(),
        };
        let handle = thread::spawn(move || task(mailbox));
        ActorHandle {
            handle,
            dropbox,
            cancel,
        }
    }

    /// Mailbox for a blocking actor
    pub struct Mailbox<M> {
        mailbox: mpsc::Receiver<M>,
        dropbox: Dropbox<M>,
        cancel: tokio_util::sync::CancellationToken,
    }

    impl<M> Mailbox<M>
    where
        M: Send,
    {
        /// Receive a message synchronously
        pub fn receive(&mut self) -> super::ActorReceiveResult<M> {
            self.mailbox
                .blocking_recv()
                .map_or_else(|| Err(super::ActorReceiveError::MailboxClosed), Ok)
        }

        /// Check if there are messages to receive
        pub fn has_messages(&self) -> bool {
            !self.mailbox.is_empty()
        }

        /// The dropbox to send to this mailbox. Useful so the actor can allow spawned actors to send messages back
        pub fn dropbox(&self) -> Dropbox<M> {
            self.dropbox.clone()
        }

        /// Is this actor cancelled?
        pub fn is_cancelled(&self) -> bool {
            self.cancel.is_cancelled()
        }
    }
}
