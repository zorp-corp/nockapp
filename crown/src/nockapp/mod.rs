use crate::kernel::form::Kernel;
use crate::noun::slab::{CueError, NounSlab};
use crate::{CrownError, NounExt};
use futures::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use thiserror::Error;
use tokio::sync::{broadcast, mpsc, oneshot, AcquireError, Mutex, OwnedSemaphorePermit};
use tokio::task::JoinSet;
use tokio::time::Duration;
use tokio::{fs, select};
use tracing::{debug, error, info};

pub type IODriverFuture = Pin<Box<dyn Future<Output = Result<(), NockAppError>> + Send>>;
pub type IODriverFn = Box<dyn FnOnce(NockAppHandle) -> IODriverFuture>;
pub type TaskJoinSet = JoinSet<Result<(), NockAppError>>;
pub type ActionSender = mpsc::Sender<IOAction>;
pub type ActionReceiver = mpsc::Receiver<IOAction>;
pub type EffectSender = broadcast::Sender<NounSlab>;
pub type EffectReceiver = broadcast::Receiver<NounSlab>;

pub struct NockApp {
    // Nock kernel
    pub kernel: Kernel,
    // Current join handles for IO drivers (parallel to `drivers`)
    pub tasks: Arc<Mutex<tokio::task::JoinSet<Result<(), NockAppError>>>>,
    // Current join handles for save task
    pub save_tasks: Arc<Mutex<tokio::task::JoinSet<Result<u64, NockAppError>>>>,
    // Exit signal sender
    pub exit_send: mpsc::Sender<usize>,
    // Exit signal receiver
    pub exit_recv: mpsc::Receiver<usize>,
    // Save event num sender
    pub watch_send: tokio::sync::watch::Sender<u64>,
    // Save event num receiver
    pub watch_recv: tokio::sync::watch::Receiver<u64>,
    // IO action channel
    pub action_channel: mpsc::Receiver<IOAction>,
    // IO action channel sender
    pub action_channel_sender: mpsc::Sender<IOAction>,
    // Effect broadcast channel
    pub effect_broadcast: broadcast::Sender<NounSlab>,
    // Jam buffer toggle
    pub buff_toggle: Arc<AtomicBool>,
    // Save semaphore
    pub save_sem: Arc<tokio::sync::Semaphore>,
}

impl NockApp {
    pub fn new(kernel: Kernel) -> Self {
        let (action_channel_sender, action_channel) = mpsc::channel(100);
        let (effect_broadcast, _) = broadcast::channel(100);
        let tasks = Arc::new(Mutex::new(TaskJoinSet::new()));
        let (exit_send, exit_recv) = mpsc::channel(1);
        let buff_toggle = Arc::new(AtomicBool::new(false));
        let save_sem = Arc::new(tokio::sync::Semaphore::new(1));
        let (watch_send, watch_recv) = tokio::sync::watch::channel(kernel.serf.event_num);
        let save_tasks = Arc::new(Mutex::new(JoinSet::new()));

        Self {
            kernel,
            tasks,
            save_tasks,
            exit_send,
            exit_recv,
            watch_send,
            watch_recv,
            action_channel,
            action_channel_sender,
            effect_broadcast,
            buff_toggle,
            save_sem,
        }
    }

    pub fn get_handle(&self) -> NockAppHandle {
        NockAppHandle {
            io_sender: self.action_channel_sender.clone(),
            effect_sender: self.effect_broadcast.clone(),
            effect_receiver: Mutex::new(self.effect_broadcast.subscribe()),
            exit: self.exit_send.clone(),
        }
    }

    pub async fn add_io_driver(&mut self, driver: IODriverFn) {
        let io_sender = self.action_channel_sender.clone();
        let effect_sender = self.effect_broadcast.clone();
        let effect_receiver = Mutex::new(self.effect_broadcast.subscribe());
        let exit= self.exit_send.clone();
        let fut = driver(NockAppHandle {
            io_sender,
            effect_sender,
            effect_receiver,
            exit,
        });
        let _ = self.tasks.clone().lock_owned().await.spawn(fut);
    }

    pub async fn save(
        &mut self,
        save: Result<OwnedSemaphorePermit, AcquireError>,
    ) -> Result<(), NockAppError> {
        let checkpoint = self.kernel.serf.jam_checkpoint();
        let bytes = checkpoint.encode()?;
        let jam_paths = self.kernel.jam_paths.clone();
        let toggle = self.buff_toggle.clone();
        self.save_tasks.lock().await.spawn(async move {
            let file = if toggle.load(Ordering::SeqCst) {
                &jam_paths.1
            } else {
                &jam_paths.0
            };

            fs::write(&file, bytes).await?;
            debug!(
                "Write to {:?} successful, checksum: {}, event: {}",
                file, checkpoint.checksum, checkpoint.event_num
            );

            // Flip toggle after successful write
            toggle.store(!toggle.load(Ordering::SeqCst), Ordering::SeqCst);

            drop(save);
            Ok(checkpoint.event_num)
        });

        // Need to add this for ctrl-c to register
        tokio::time::sleep(Duration::from_secs(1)).await;
        Ok(())
    }

    pub async fn work(&mut self) -> Result<(), NockAppError> {
        let tasks_fut = async {
            let mut joinset = self.tasks.clone().lock_owned().await;
            joinset.join_next().await
        };
        let tasks_save= async {
            let mut joinset = self.save_tasks.clone().lock_owned().await;
            joinset.join_next().await
        };

        select!(
            res = tasks_fut => {
                match res {
                    Some(Ok(Err(e))) => {
                        Err(e)
                    },
                    Some(Err(e)) => {
                        Err(e)?
                    },
                    _ => {Ok(())},
                }
            },
            res = tasks_save => {
                match res {
                    Some(Ok(Err(e))) => {
                        Err(e)
                    },
                    Some(Err(e)) => {
                        Err(e)?
                    },
                    Some(Ok(Ok(event_num))) => {self.watch_send.send(event_num)?; Ok(())},
                    _ => {Ok(())},
                }
            },
            permit = self.save_sem.clone().acquire_owned() => {
                //  Check if we should write in the first place
                let curr_event = self.kernel.serf.event_num;

                if !self.kernel.jam_paths.can_write(curr_event) {
                    debug!("Skipping save, event number has not changed from: {}", curr_event);
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    return Ok(());
                }
                self.save(permit).await
            },
            exit = self.exit_recv.recv() => {
                // TODO: handle option
                let code = exit.unwrap();
                let exit_event_num = self.kernel.serf.event_num;
                info!("Exit request received, waiting for save for event_num {}", exit_event_num);
                // TODO: actually prevent new pokes from coming in

                let recv = self.watch_recv.clone();
                tokio::task::spawn(async move {
                    loop {
                        let new = *(recv.borrow());
                        if new == exit_event_num {
                            info!("Save event_num reached, exiting with code {}", code);
                            std::process::exit(code as i32);
                        }
                    }

                });
                Ok(())
            }
            action_res = self.action_channel.recv() => {
                if let Some(action) = action_res {
                    info!("action: {:?}", action);
                    match action {
                        IOAction::Poke { poke, ack_channel } => {
                            debug!("poke slab: {:?}", poke);
                            let poke_noun = poke.copy_to_stack(self.kernel.serf.stack());
                            debug!("poke_noun: {:?}", poke_noun);
                            let effects_res = self.kernel.poke(poke_noun);
                            debug!("effects_res: {:?}", effects_res);
                            match effects_res {
                                Ok(effects) => {
                                    let _ = ack_channel.send(PokeResult::Ack);
                                    for effect in effects.list_iter() {
                                        let mut effect_slab = NounSlab::new();
                                        effect_slab.copy_into(effect);
                                        let _ = self.effect_broadcast.send(effect_slab);
                                    }
                                },
                                Err(_) => {
                                    let _ = ack_channel.send(PokeResult::Nack);
                                },
                            }
                        },
                        IOAction::Peek { path, result_channel } => {
                            let path_noun = path.copy_to_stack(self.kernel.serf.stack());
                            info!("path_noun: {:?}", path_noun);
                            let peek_res = self.kernel.peek(path_noun);

                            match peek_res {
                                Ok(res_noun) => {
                                    let mut res_slab = NounSlab::new();
                                    res_slab.copy_into(res_noun);
                                    let _ = result_channel.send(Some(res_slab));
                                },
                                Err(_) => {
                                    let _ = result_channel.send(None);
                                }
                            }
                        },
                    }
                }
                Ok(())
            }
        )
    }
}

pub struct NockAppHandle {
    io_sender: ActionSender,
    pub effect_sender: EffectSender,
    effect_receiver: Mutex<EffectReceiver>,
    pub exit: mpsc::Sender<usize>,
}

impl NockAppHandle {
    pub async fn poke(&self, poke: NounSlab) -> Result<PokeResult, NockAppError> {
        let (ack_channel, ack_future) = oneshot::channel();
        self.io_sender
            .send(IOAction::Poke { poke, ack_channel })
            .await?;
        Ok(ack_future.await?)
    }

    pub async fn peek(&self, path: NounSlab) -> Result<Option<NounSlab>, NockAppError> {
        let (result_channel, result_future) = oneshot::channel();
        self.io_sender
            .send(IOAction::Peek {
                path,
                result_channel,
            })
            .await?;
        Ok(result_future.await?)
    }

    pub async fn next_effect(&self) -> Result<NounSlab, NockAppError> {
        let mut effect_receiver = self.effect_receiver.lock().await;
        Ok(effect_receiver.recv().await?)
    }

    pub fn dup(self) -> (Self, Self) {
        let io_sender = self.io_sender.clone();
        let effect_sender = self.effect_sender.clone();
        let effect_receiver = Mutex::new(effect_sender.subscribe());
        let exit = self.exit.clone();
        (
            self,
            NockAppHandle {
                io_sender,
                effect_sender,
                effect_receiver,
                exit,
            },
        )
    }

    pub fn clone_io_sender(&self) -> ActionSender {
        self.io_sender.clone()
    }
}

/// IO actions to be sent over channels
#[derive(Debug)]
pub enum IOAction {
    Poke {
        poke: NounSlab,
        ack_channel: oneshot::Sender<PokeResult>,
    },
    Peek {
        path: NounSlab,
        result_channel: oneshot::Sender<Option<NounSlab>>,
    },
}

/// Result of a poke: either Ack if it succeeded or Nack if it failed
#[derive(Debug)]
pub enum PokeResult {
    Ack,
    Nack,
}

pub enum Operation {
    Poke,
    Peek,
}

/// Error type for NockApps
#[derive(Debug, Error)]
pub enum NockAppError {
    #[error("Timeout")]
    Timeout,
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("MPSC send error (probably trying to send a poke): {0}")]
    MPSCSendError(#[from] tokio::sync::mpsc::error::SendError<IOAction>),
    #[error("Oneshot receive error (sender dropped): {0}")]
    OneShotRecvError(#[from] tokio::sync::oneshot::error::RecvError),
    #[error("Error cueing jam buffer: {0}")]
    CueError(#[from] CueError),
    #[error("Error receiving effect broadcast: {0}")]
    BroadcastRecvError(#[from] tokio::sync::broadcast::error::RecvError),
    #[error("Error joining task (probably the task panicked: {0}")]
    JoinError(#[from] tokio::task::JoinError),
    #[error("Error converting string: {0}")]
    FromUtf8Error(#[from] std::string::FromUtf8Error),
    #[error("Crown error: {0}")]
    CrownError(#[from] CrownError),
    #[error("Other error")]
    OtherError,
    #[error("Peek failed")]
    PeekFailed,
    #[error("Poke failed")]
    PokeFailed,
    #[error("Unexpected result")]
    UnexpectedResult,
    #[error("sword error: {0}")]
    SwordError(#[from] sword::noun::Error),
    #[error("Save error: {0}")]
    EncodeError(#[from] bincode::error::EncodeError),
    #[error("Decode error: {0}")]
    DecodeError(#[from] bincode::error::DecodeError),
    #[error("Send error: {0}")]
    SendError(#[from] tokio::sync::watch::error::SendError<u64>),
}

pub fn make_driver<F, Fut>(f: F) -> IODriverFn
where
    F: FnOnce(NockAppHandle) -> Fut + Send + 'static,
    Fut: Future<Output = Result<(), NockAppError>> + Send + 'static,
{
    Box::new(move |handle| Box::pin(f(handle)))
}

#[cfg(test)]
mod tests {
    use crate::kernel::checkpoint::JamPaths;
    use crate::noun::slab::slab_equality;

    use super::*;
    use std::fs;
    use std::path::Path;
    use sword::jets::cold::Nounable;
    use sword::jets::util::slot;
    use sword::noun::{Noun, D, T};
    use sword::serialization::{cue, jam};
    use sword::unifying_equality::unifying_equality;
    use sword_macros::tas;
    use tempfile::TempDir;
    use tracing_test::traced_test;

    fn setup_kernel(jam: &str) -> (Kernel, TempDir) {
        let temp_dir = TempDir::new().expect("Failed to create temp directory");
        let snap_dir = temp_dir.path().to_path_buf();
        let jam_paths = JamPaths::new(&snap_dir);
        let jam_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("assets")
            .join(jam);
        let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
        let kernel = Kernel::load(snap_dir, jam_paths, &jam_bytes, false);
        (kernel, temp_dir)
    }

    async fn save_nockapp(nockapp: &mut NockApp) {
        let permit = nockapp.save_sem.clone().acquire_owned().await;
        let _ = nockapp.save(permit).await.unwrap();
        let _ = nockapp
            .tasks
            .lock()
            .await
            .join_next()
            .await
            .unwrap()
            .unwrap();
    }

    // Test nockapp save
    #[tokio::test]
    #[traced_test]
    async fn test_nockapp_save() {
        let (kernel, _temp_dir) = setup_kernel("dumb.jam");
        let mut nockapp = NockApp::new(kernel);
        let mut arvo = nockapp.kernel.serf.arvo;
        let jam_paths = nockapp.kernel.jam_paths.clone();
        assert_eq!(nockapp.kernel.serf.event_num, 0);

        // Save
        save_nockapp(&mut nockapp).await;

        // Permit should be dropped
        assert_eq!(nockapp.save_sem.available_permits(), 1);

        // A valid checkpoint should exist in one of the jam files
        let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
        assert!(checkpoint.is_ok());
        let mut checkpoint = checkpoint.unwrap();

        // Checkpoint event number should be 0
        assert!(checkpoint.event_num == 0);

        // Checkpoint kernel should be equal to the saved kernel
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut checkpoint.arvo,
                &mut arvo
            ));
        }

        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint.cold.into_noun(nockapp.kernel.serf.stack());
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack());
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            ));
        };
    }

    // Test nockapp poke
    #[tokio::test]
    #[traced_test]
    async fn test_nockapp_poke_save() {
        let (kernel, _temp_dir) = setup_kernel("dumb.jam");
        let mut nockapp = NockApp::new(kernel);
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let mut arvo_before_poke = nockapp.kernel.serf.arvo;

        let poke = T(
            nockapp.kernel.serf.stack(),
            &[D(tas!(b"command")), D(tas!(b"born")), D(0)],
        );
        let _ = nockapp.kernel.poke(poke).unwrap();

        // Save
        save_nockapp(&mut nockapp).await;

        // A valid checkpoint should exist in one of the jam files
        let jam_paths = &nockapp.kernel.jam_paths;
        let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
        assert!(checkpoint.is_ok());
        let mut checkpoint = checkpoint.unwrap();

        // Checkpoint event number should be 1
        assert!(checkpoint.event_num == 1);
        let mut arvo_after_poke = nockapp.kernel.serf.arvo;

        unsafe {
            let stack = nockapp.kernel.serf.stack();
            // Checkpoint kernel should be equal to the saved kernel
            assert!(unifying_equality(
                stack, &mut checkpoint.arvo, &mut arvo_after_poke
            ));
            // Checkpoint kernel should be different from the kernel before the poke
            assert!(!unifying_equality(
                stack, &mut checkpoint.arvo, &mut arvo_before_poke
            ));
        }

        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint.cold.into_noun(nockapp.kernel.serf.stack());
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack());
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            ));
        };
    }

    #[tokio::test]
    async fn test_nockapp_save_multiple() {
        let (kernel, _temp_dir) = setup_kernel("test.jam");
        let mut nockapp = NockApp::new(kernel);
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let jam_paths = nockapp.kernel.jam_paths.clone();

        for i in 1..4 {
            // Poke to increment the state
            let poke = D(tas!(b"inc"));
            let _ = nockapp.kernel.poke(poke).unwrap();

            // Save
            save_nockapp(&mut nockapp).await;

            // Permit should be dropped
            assert_eq!(nockapp.save_sem.available_permits(), 1);

            // A valid checkpoint should exist in one of the jam files
            let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
            assert!(checkpoint.is_ok());
            let checkpoint = checkpoint.unwrap();

            // Checkpoint event number should be i
            assert!(checkpoint.event_num == i);

            // Checkpointed state should have been incremented
            let peek = T(nockapp.kernel.serf.stack(), &[D(tas!(b"state")), D(0)]);

            // res should be [~ ~ val]
            let res = nockapp.kernel.peek(peek).unwrap();
            let val = slot(res, 7).unwrap();
            unsafe {
                assert!(val.raw_equals(D(i)));
            }
        }
    }

    // Tests for fallback to previous checkpoint if checkpoint is corrupt
    #[tokio::test]
    #[traced_test]
    async fn test_nockapp_corrupt_check() {
        let (kernel, _temp_dir) = setup_kernel("test.jam");
        let mut nockapp = NockApp::new(kernel);
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let jam_paths = nockapp.kernel.jam_paths.clone();

        // Save a valid checkpoint
        save_nockapp(&mut nockapp).await;

        // Assert the checkpoint exists
        assert!(jam_paths.0.exists());

        // Permit should be dropped
        assert_eq!(nockapp.save_sem.available_permits(), 1);

        // Generate an invalid checkpoint by incrementing the event number
        let mut invalid = nockapp.kernel.serf.jam_checkpoint();
        invalid.event_num = invalid.event_num + 1;
        assert!(!invalid.validate());

        // The invalid checkpoint has a higher event number than the valid checkpoint
        let valid = jam_paths
            .load_checkpoint(nockapp.kernel.serf.stack())
            .unwrap();
        assert!(valid.event_num < invalid.event_num);

        // Save the corrupted checkpoint, because of the toggle buffer, we will write to jam file 1
        assert!(!jam_paths.1.exists());
        let jam_path = &jam_paths.1;
        let jam_bytes = invalid.encode().unwrap();
        tokio::fs::write(jam_path, jam_bytes).await.unwrap();

        // The loaded checkpoint will be the valid one
        let chk = jam_paths
            .load_checkpoint(nockapp.kernel.serf.stack())
            .unwrap();
        assert!(chk.event_num == valid.event_num);
    }

    #[test]
    fn test_jam_equality_stack() {
        let (mut kernel, _temp_dir) = setup_kernel("http.jam");
        let mut arvo = kernel.serf.arvo.clone();
        let stack = kernel.serf.stack();
        let j = jam(stack, arvo);
        let mut c = cue(stack, j).unwrap();
        // new nockstack
        unsafe { assert!(unifying_equality(stack, &mut arvo, &mut c)) }
    }

    #[test]
    fn test_jam_equality_slab() {
        let (kernel, _temp_dir) = setup_kernel("http.jam");
        let mut slab = NounSlab::new();
        let arvo = kernel.serf.arvo.clone();
        slab.copy_into(arvo);
        let bytes = slab.jam();
        let c = slab.cue_into(bytes).unwrap();
        unsafe { assert!(slab_equality(slab.root(), c)) }
    }

    #[test]
    fn test_jam_equality_slab_stack() {
        let (mut kernel, _temp_dir) = setup_kernel("http.jam");
        let mut arvo = kernel.serf.arvo.clone();
        let mut slab = NounSlab::new();
        slab.copy_into(arvo);
        // Use slab to jam
        let bytes = slab.jam();
        let stack = kernel.serf.stack();
        // Use the stack to cue
        let mut c = Noun::cue_bytes(stack, &bytes).unwrap();
        unsafe {
            // check for equality
            assert!(unifying_equality(stack, &mut arvo, &mut c))
        }
    }
}
