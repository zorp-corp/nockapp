use std::future::Future;

use tokio::sync::mpsc;
use tokio::task::JoinHandle;

// enumerate errors that actors may terminate with
enum ActorError {}

// result of an actor (when joining)
type ActorResult = Result<(), ActorError>;

// result of polling an actor's status
enum ActorStatus {
    ActorError(ActorError),
    ActorRunning,
    ActorDone,
}

// errors when sending a message to an actor
enum ActorSendError<M> {
    _Oops(M),
}

// result when sending a message to an actor
type ActorSendResult<M> = Result<(), ActorSendError<M>>;

enum ActorReceiveError<M> {
    _Oops(M),
}

type ActorReceiveResult<M> = Result<M, ActorReceiveError<M>>;

/// returned when an actor is spawned
struct ActorHandle<M> {
    handle: JoinHandle<ActorResult>,
    mailbox: mpsc::Sender<M>,
}

impl<M> ActorHandle<M>
where
    M: Sync + Send,
{
    fn status(&self) -> ActorStatus {
        todo!()
    }
    fn send(&self, message: M) -> ActorSendResult<M> {
        todo!()
    }
    fn dropbox(&self) -> Dropbox<M> {
        todo!()
    }
}

struct Mailbox<M> {
    mailbox: mpsc::Receiver<M>,
}

struct Dropbox<M> {
    dropbox: mpsc::Sender<M>,
}

impl<M> Mailbox<M>
where
    M: Send + Sync,
{
    async fn receive(&mut self) -> Result<M, ActorReceiveError<M>> {
        todo!()
    }
    fn has_messages(&self) -> bool {
        todo!()
    }
}

fn spawn<F, M>(task: F) -> ActorHandle<M>
where
    F: FnOnce(Mailbox<M>) -> Box<dyn Future<Output = ()>>,
{
    todo!()
}
