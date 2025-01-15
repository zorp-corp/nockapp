use super::driver::{IOAction, IODriverFn, NockAppHandle, PokeResult};
use super::NockAppError;
use crate::kernel::form::Kernel;
use crate::noun::slab::NounSlab;
use crate::NounExt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::path::PathBuf;

use tokio::io::AsyncWriteExt;
use tokio::sync::{broadcast, mpsc, AcquireError, Mutex, OwnedSemaphorePermit};
use tokio::time::Duration;
use tokio::{fs, select};
use tokio_util::task::TaskTracker;
use tracing::{error, info, trace};

type NockAppResult = Result<(), NockAppError>;

pub struct NockApp {
    /// Nock kernel
    pub kernel: Kernel,
    /// Current join handles for IO drivers (parallel to `drivers`)
    // pub tasks: Arc<tokio::task::JoinSet<NockAppResult>>,
    pub tasks: tokio_util::task::TaskTracker,
    /// Exit signal sender
    pub exit_send: mpsc::Sender<usize>,
    /// Exit signal receiver
    pub exit_recv: mpsc::Receiver<usize>,
    /// Exit status
    pub exit_status: AtomicBool,
    /// Save event num sender
    pub watch_send: Arc<Mutex<tokio::sync::watch::Sender<u64>>>,
    /// Save event num receiver
    pub watch_recv: tokio::sync::watch::Receiver<u64>,
    /// IO action channel
    pub action_channel: mpsc::Receiver<IOAction>,
    /// IO action channel sender
    pub action_channel_sender: mpsc::Sender<IOAction>,
    /// Effect broadcast channel
    pub effect_broadcast: broadcast::Sender<NounSlab>,
    /// Save semaphore
    pub save_sem: Arc<tokio::sync::Semaphore>,
    /// Save interval
    pub save_interval: Duration,
    /// Shutdown oneshot sender
    pub shutdown_send: Option<tokio::sync::oneshot::Sender<NockAppResult>>,
    /// Shutdown oneshot receiver
    pub shutdown_recv: tokio::sync::oneshot::Receiver<NockAppResult>,
    pub cancel_token: tokio_util::sync::CancellationToken,
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
        let cancel_token = tokio_util::sync::CancellationToken::new();
        // let ctrl_c = tokio::signal::ctrl_c();

        // tokio::task::spawn(async move {
        //     let _ = ctrl_c.await;
        //     info!("ctrl_c registered");
        //     // TODO: Should this trigger cleanup actions?
        //     // FIXME: Don't use std::process::exit
        //     std::process::exit(0);
        // });

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
            cancel_token,
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
        let _ = self.tasks.spawn(fut);
    }

    pub async fn save(
        &mut self,
        save: Result<OwnedSemaphorePermit, AcquireError>,
    ) -> NockAppResult {
        let toggle = self.kernel.buffer_toggle.clone();
        let jam_paths = self.kernel.jam_paths.clone();
        let checkpoint = self.kernel.checkpoint();
        let bytes = checkpoint.encode()?;
        let send_lock = self.watch_send.clone();
        self.tasks.spawn(async move {
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

        Ok(())
    }

    /// Runs until the nockapp is done (returns exit 0 or an error)
    pub async fn work_loop(mut self) -> NockAppResult {
        // Close the task tracker so wait can return when the task tracker is complete.
        self.tasks.close();
        loop {
            let work_res = self.work().await;
            match work_res {
                Ok(nockapp_run) => {
                    match nockapp_run {
                        crate::nockapp::NockAppRun::Pending => continue,
                        crate::nockapp::NockAppRun::Done => return Ok(()),
                    }
                }
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

    fn cleanup_socket(&self) {
        // Clean up npc socket file if it exists
        if let Some(socket) = &self.npc_socket_path {
            if socket.exists() {
                if let Err(e) = std::fs::remove_file(socket) {
                    error!("Failed to remove npc socket file before exit: {}", e);
                }
            }
        }
    }

    pub async fn work(&mut self) -> Result<NockAppRun, NockAppError> {
        select!(
            shutdown = &mut self.shutdown_recv => {
                self.cleanup_socket();
                match shutdown {
                    Ok(Ok(())) => {
                        info!("Shutdown triggered, exiting");
                        return Ok(NockAppRun::Done);
                    },
                    Ok(Err(e)) => {
                        error!("Shutdown triggered with error: {}", e);
                        return Err(e);
                    },
                    // Err(_recv_error) => {},
                    Err(_recv_error) => {
                        error!("Shutdown channel closed prematurely");
                        return Err(NockAppError::ChannelClosedError);
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

                if elapsed < self.save_interval && !self.exit_status.load(Ordering::SeqCst) {
                    tokio::time::sleep(self.save_interval - elapsed).await;
                }
                return res.map(|_| NockAppRun::Pending);
            },
            exit = self.exit_recv.recv() => {
                if let Some(code) = exit {
                    // TODO: Is this necessary?
                    // self.action_channel.close();
                    // TODO: See if exit_status is duplicative of what the cancel token is for.
                    self.exit_status.store(true, Ordering::SeqCst);
                    let exit_event_num = self.kernel.serf.event_num;
                    info!("Exit request received, waiting for save checkpoint with event_num {}", exit_event_num);

                    let mut recv = self.watch_recv.clone();
                    let socket_path = self.npc_socket_path.clone();
                    let cancel_token = self.cancel_token.clone();
                    let shutdown_send = self.shutdown_send.take().unwrap();
                    self.tasks.spawn(async move {
                        loop {
                            let _ = recv.changed().await;
                            let new = *(recv.borrow());
                            assert!(new <= exit_event_num);
                            if new == exit_event_num {
                                if let Some(ref path) = socket_path {
                                    if path.exists() {
                                        if let Err(e) = std::fs::remove_file(&path) {
                                            error!("Failed to remove socket file on exit: {}", e);
                                        }
                                    }
                                }
                                info!("Save event_num reached, finishing with code {}", code);
                                let shutdown_result = if code == 0 {
                                    Ok(())
                                } else {
                                    Err(NockAppError::Exit(code))
                                };
                                let _ = shutdown_send.send(shutdown_result);
                                cancel_token.cancel();
                                break
                            };
                        }
                    });
                    Ok(NockAppRun::Pending)
                } else {
                    error!("Exit signal channel closed prematurely");
                    Err(NockAppError::ChannelClosedError)
                }
            }
            action_res = self.action_channel.recv() => {
                if let Some(action) = action_res {
                    match action {
                        IOAction::Poke { poke, ack_channel } => {
                            let poke_noun = poke.copy_to_stack(self.kernel.serf.stack());
                            let effects_res = self.kernel.poke(poke_noun);
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
