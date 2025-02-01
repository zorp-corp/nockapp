use super::driver::{IOAction, IODriverFn, NockAppHandle, PokeResult};
use super::NockAppError;
use crate::kernel::form::Kernel;
use crate::noun::slab::NounSlab;
use crate::NounExt;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use tokio::io::AsyncWriteExt;
use tokio::sync::{broadcast, mpsc, AcquireError, Mutex, OwnedSemaphorePermit};
use tokio::time::Duration;
use tokio::{fs, select};
use tokio_util::task::TaskTracker;
use tracing::{debug, error, info, trace};

type NockAppResult = Result<(), NockAppError>;

pub struct NockApp {
    /// Nock kernel
    pub(crate) kernel: Kernel,
    /// Current join handles for IO drivers (parallel to `drivers`)
    pub(crate) tasks: tokio_util::task::TaskTracker,
    /// Exit signal sender
    exit_send: mpsc::Sender<usize>,
    /// Exit signal receiver
    exit_recv: mpsc::Receiver<usize>,
    /// Exit status
    exit_status: AtomicBool,
    /// Save event num sender
    watch_send: Arc<Mutex<tokio::sync::watch::Sender<u64>>>,
    /// Save event num receiver
    watch_recv: tokio::sync::watch::Receiver<u64>,
    /// IO action channel
    action_channel: mpsc::Receiver<IOAction>,
    /// IO action channel sender
    action_channel_sender: mpsc::Sender<IOAction>,
    /// Effect broadcast channel
    effect_broadcast: broadcast::Sender<NounSlab>,
    /// Save semaphore
    pub(crate) save_sem: Arc<tokio::sync::Semaphore>,
    /// Save interval
    save_interval: Duration,
    /// Shutdown oneshot sender
    shutdown_send: Option<tokio::sync::oneshot::Sender<NockAppResult>>,
    /// Shutdown oneshot receiver
    shutdown_recv: tokio::sync::oneshot::Receiver<NockAppResult>,
    // cancel_token: tokio_util::sync::CancellationToken,
    pub npc_socket_path: Option<PathBuf>,
}

pub enum NockAppRun {
    Pending,
    Done,
}

impl NockApp {
    pub fn new(kernel: Kernel, save_interval: Duration) -> Self {
        let (action_channel_sender, action_channel) = mpsc::channel(100);
        let (effect_broadcast, _) = broadcast::channel(100);
        // let tasks = Arc::new(Mutex::new(TaskJoinSet::new()));
        // let tasks = TaskJoinSet::new();
        // let tasks = Arc::new(TaskJoinSet::new());
        let tasks = TaskTracker::new();
        let (exit_send, exit_recv) = mpsc::channel(1);
        let save_sem = Arc::new(tokio::sync::Semaphore::new(1));
        let (watch_send, watch_recv) = tokio::sync::watch::channel(kernel.serf.event_num);
        let watch_send = Arc::new(Mutex::new(watch_send.clone()));
        let exit_status = AtomicBool::new(false);
        let (shutdown_send, shutdown_recv) = tokio::sync::oneshot::channel();
        // let cancel_token = tokio_util::sync::CancellationToken::new();

        Self {
            kernel,
            tasks,
            exit_send,
            exit_recv,
            exit_status,
            watch_send,
            watch_recv,
            action_channel,
            action_channel_sender,
            effect_broadcast,
            save_sem,
            save_interval,
            shutdown_send: Some(shutdown_send),
            shutdown_recv,
            // cancel_token,
            npc_socket_path: None,
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

    /// Assume at-least-once processing and track the state necessary to know whether
    /// all critical IO actions have been performed correctly or not from the jammed state.
    pub async fn add_io_driver(&mut self, driver: IODriverFn) {
        let io_sender = self.action_channel_sender.clone();
        let effect_sender = self.effect_broadcast.clone();
        let effect_receiver = Mutex::new(self.effect_broadcast.subscribe());
        let exit = self.exit_send.clone();
        let fut = driver(NockAppHandle {
            io_sender,
            effect_sender,
            effect_receiver,
            exit,
        });
        // TODO: Stop using the task tracker for user code?
        let _ = self.tasks.spawn(fut);
    }

    /// Purely for testing purposes (injecting delays) for now.
    pub(crate) async fn save_f(
        &mut self,
        save: Result<OwnedSemaphorePermit, AcquireError>,
        f: impl std::future::Future<Output = ()> + Send + 'static,
    ) -> Result<tokio::task::JoinHandle<NockAppResult>, NockAppError> {
        let toggle = self.kernel.buffer_toggle.clone();
        let jam_paths = self.kernel.jam_paths.clone();
        let checkpoint = self.kernel.checkpoint();
        let bytes = checkpoint.encode()?;
        let send_lock = self.watch_send.clone();

        let join_handle = self.tasks.spawn(async move {
            f.await;
            let path = if toggle.load(Ordering::SeqCst) {
                jam_paths.1
            } else {
                jam_paths.0
            };
            let mut file = fs::File::create(&path)
                .await
                .map_err(|e| NockAppError::SaveError(e))?;

            file.write_all(&bytes)
                .await
                .map_err(|e| NockAppError::SaveError(e))?;
            file.sync_all()
                .await
                .map_err(|e| NockAppError::SaveError(e))?;

            trace!(
                "Write to {:?} successful, checksum: {}, event: {}",
                path.display(),
                checkpoint.checksum,
                checkpoint.event_num
            );

            // Flip toggle after successful write
            toggle.store(!toggle.load(Ordering::SeqCst), Ordering::SeqCst);
            let send = send_lock.lock().await;
            send.send(checkpoint.event_num)?;
            drop(save);
            Ok::<(), NockAppError>(())
        });
        // We don't want to close and re-open the tasktracker from multiple places
        // so we're just returning the join_handle to let the caller decide.
        Ok(join_handle)
    }

    /// Except in tests, save should only be called by the permit handler.
    pub(crate) async fn save(
        &mut self,
        save: Result<OwnedSemaphorePermit, AcquireError>,
    ) -> NockAppResult {
        let _join_handle = self.save_f(save, async {}).await?;
        Ok(())
    }

    /// Peek at a noun in the kernel, blocking operation
    #[tracing::instrument(skip(self, path))]
    pub fn peek_sync(&mut self, path: NounSlab) -> Result<NounSlab, NockAppError> {
        trace!("About to copy to stack");
        let path_noun = path.copy_to_stack(self.kernel.serf.stack());
        trace!("Peeking at noun: {:?}", path_noun);
        let peek_noun = self.kernel.peek(path_noun)?;
        trace!("Peeked noun: {:?}", peek_noun);
        let mut res_slab = NounSlab::new();
        res_slab.copy_into(peek_noun);
        trace!("Copied res_slab");
        Ok(res_slab)
    }

    /// Poke at a noun in the kernel, blocking operation
    #[tracing::instrument(skip(self, wire, poke))]
    pub fn poke_sync(
        &mut self,
        wire: NounSlab,
        poke: NounSlab,
    ) -> Result<Vec<NounSlab>, NockAppError> {
        let wire_noun = wire.copy_to_stack(self.kernel.serf.stack());
        let poke_noun = poke.copy_to_stack(self.kernel.serf.stack());
        let effects = self.kernel.poke(wire_noun, poke_noun)?;
        let mut effect_slabs = Vec::new();
        for effect in effects.list_iter() {
            let mut effect_slab = NounSlab::new();
            effect_slab.copy_into(effect);
            effect_slabs.push(effect_slab);
        }
        Ok(effect_slabs)
    }

    /// Runs until the nockapp is done (returns exit 0 or an error)
    pub async fn run(&mut self) -> NockAppResult {
        debug!("Starting nockapp run");
        // Reset NockApp for next run
        // self.reset();
        // debug!("Reset NockApp for next run");
        loop {
            let work_res = self.work().await;
            match work_res {
                Ok(nockapp_run) => match nockapp_run {
                    crate::nockapp::NockAppRun::Pending => continue,
                    crate::nockapp::NockAppRun::Done => return Ok(()),
                },
                Err(NockAppError::Exit(code)) => {
                    if code == 0 {
                        // zero is success, we're simply done.
                        info!("nockapp exited successfully with code: {}", code);
                        return Ok(());
                    } else {
                        error!("nockapp exited with error code: {}", code);
                        return Err(NockAppError::Exit(code));
                    }
                }
                Err(e) => {
                    error!("Got error running nockapp: {:?}", e);
                    return Err(e);
                }
            };
        }
    }

    fn cleanup_socket_(socket: &Option<PathBuf>) {
        // Clean up npc socket file if it exists
        if let Some(socket) = socket {
            if socket.exists() {
                if let Err(e) = std::fs::remove_file(socket) {
                    error!("Failed to remove npc socket file before exit: {}", e);
                }
            }
        }
    }

    fn cleanup_socket(&self) {
        // Clean up npc socket file if it exists
        Self::cleanup_socket_(&self.npc_socket_path);
    }

    async fn work(&mut self) -> Result<NockAppRun, NockAppError> {
        select!(
            shutdown = &mut self.shutdown_recv => {
                debug!("Shutdown channel received");
                self.cleanup_socket();
                return match shutdown {
                    Ok(Ok(())) => {
                        info!("Shutdown triggered, exiting");
                        Ok(NockAppRun::Done)
                    },
                    Ok(Err(e)) => {
                        error!("Shutdown triggered with error: {}", e);
                        Err(e)
                    },
                    // Err(_recv_error) => {},
                    Err(_recv_error) => {
                        error!("Shutdown channel closed prematurely");
                        Err(NockAppError::ChannelClosedError)
                    },
                };
            },
            permit = self.save_sem.clone().acquire_owned() => {
                //  Check if we should write in the first place
                let curr_event_num = self.kernel.serf.event_num;
                let saved_event_num = self.watch_recv.borrow();
                if curr_event_num <= *saved_event_num {
                    return Ok(NockAppRun::Pending)
                }
                drop(saved_event_num);

                // Get the current time so we can lower bound elapsed time of save
                // If we are currently exiting, do not enforce the lower bound.
                let now = std::time::Instant::now();
                let res = self.save(permit).await;
                let elapsed = now.elapsed();

                // TODO: choo::bin/choo tests::test_compile_test_app fails without this
                if elapsed < self.save_interval && !self.exit_status.load(Ordering::SeqCst) {
                    tokio::time::sleep(self.save_interval - elapsed).await;
                }
                return res.map(|_| NockAppRun::Pending);
            },
            exit = self.exit_recv.recv() => {
                debug!("Exit signal received");
                if let Some(code) = exit {
                    // `cargo nextest run`
                    // 2025-01-23T01:11:52.365215Z  INFO crown::nockapp::nockapp: Exit request received, waiting for save checkpoint with event_num 60
                    // 2025-01-23T01:11:52.403120Z ERROR crown::nockapp::nockapp: Action channel closed prematurely
                    // 2025-01-23T01:11:52.403132Z ERROR crown::nockapp::nockapp: Got error running nockapp: ChannelClosedError
                    // test tests::test_compile_test_app ... FAILED
                    // self.action_channel.close();
                    // TODO: See if exit_status is duplicative of what the cancel token is for.
                    self.exit_status.store(true, Ordering::SeqCst);
                    let exit_event_num = self.kernel.serf.event_num;
                    info!("Exit request received, waiting for save checkpoint with event_num {}", exit_event_num);

                    let mut recv = self.watch_recv.clone();
                    // let cancel_token = self.cancel_token.clone();
                    let shutdown_send = self.shutdown_send.take().unwrap();
                    // self.tasks.close();
                    // self.tasks.wait().await;
                    // recv from the watch channel until we reach the exit event_num, wrapped up in a future
                    // that will send the shutdown result when we're done.
                    let socket_path = self.npc_socket_path.clone();
                    // TODO: Break this out as a separate select! handler with no spawn
                    self.tasks.spawn(async move {
                        recv.wait_for(|&new| {
                            assert!(new <= exit_event_num);
                            new == exit_event_num
                        }).await.expect("Failed to wait for saves to catch up to exit_event_num");
                        Self::cleanup_socket_(&socket_path);
                        info!("Save event_num reached, finishing with code {}", code);
                        let shutdown_result = if code == 0 {
                            Ok(())
                        } else {
                            Err(NockAppError::Exit(code))
                        };
                        // Ensure we send the shutdown result before canceling so that
                        // we don't get a race condition where the yielded result is
                        // "canceled" instead of the actual result.
                        let _ = shutdown_send.send(shutdown_result);
                    });
                    Ok(NockAppRun::Pending)
                } else {
                    error!("Exit signal channel closed prematurely");
                    Err(NockAppError::ChannelClosedError)
                }
            }
            // FIXME: This shouldn't be hanging the event loop on the kernel poke/peek etc.
            action_res = self.action_channel.recv() => {
                debug!("Action channel received");
                if let Some(action) = action_res {
                    match action {
                        IOAction::Poke { wire, poke, ack_channel } => {
                            let wire_noun = wire.copy_to_stack(self.kernel.serf.stack());
                            let poke_noun = poke.copy_to_stack(self.kernel.serf.stack());
                            let effects_res = self.kernel.poke(wire_noun, poke_noun);
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
                    Ok(NockAppRun::Pending)
                } else {
                    error!("Action channel closed prematurely");
                    Err(NockAppError::ChannelClosedError)
                }
            }
        )
    }
}
