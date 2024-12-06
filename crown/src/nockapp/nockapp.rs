use super::driver::{IOAction, IODriverFn, NockAppHandle, PokeResult, TaskJoinSet};
use super::NockAppError;
use crate::kernel::form::Kernel;
use crate::noun::slab::NounSlab;
use crate::NounExt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use tokio::io::AsyncWriteExt;
use tokio::sync::{broadcast, mpsc, AcquireError, Mutex, OwnedSemaphorePermit};
use tokio::time::Duration;
use tokio::{fs, select};
use tracing::{error, info, trace};

pub struct NockApp {
    // Nock kernel
    pub kernel: Kernel,
    // Current join handles for IO drivers (parallel to `drivers`)
    pub tasks: Arc<Mutex<tokio::task::JoinSet<Result<(), NockAppError>>>>,
    // Exit signal sender
    pub exit_send: mpsc::Sender<usize>,
    // Exit signal receiver
    pub exit_recv: mpsc::Receiver<usize>,
    // Exit status
    pub exit_status: AtomicBool,
    // Save event num sender
    pub watch_send: Arc<Mutex<tokio::sync::watch::Sender<u64>>>,
    // Save event num receiver
    pub watch_recv: tokio::sync::watch::Receiver<u64>,
    // IO action channel
    pub action_channel: mpsc::Receiver<IOAction>,
    // IO action channel sender
    pub action_channel_sender: mpsc::Sender<IOAction>,
    // Effect broadcast channel
    pub effect_broadcast: broadcast::Sender<NounSlab<'static>>,
    // Save semaphore
    pub save_sem: Arc<tokio::sync::Semaphore>,
    // Save interval
    pub save_interval: Duration,
    // Cancel token
    pub cancel_token: tokio_util::sync::CancellationToken,
}

impl NockApp {
    pub fn new(kernel: Kernel, save_interval: Duration) -> Self {
        let (action_channel_sender, action_channel) = mpsc::channel(100);
        let (effect_broadcast, _) = broadcast::channel(100);
        let tasks = Arc::new(Mutex::new(TaskJoinSet::new()));
        let (exit_send, exit_recv) = mpsc::channel(1);
        let save_sem = Arc::new(tokio::sync::Semaphore::new(1));
        let (watch_send, watch_recv) = tokio::sync::watch::channel(kernel.serf.event_num);
        let watch_send = Arc::new(Mutex::new(watch_send.clone()));
        let exit_status = AtomicBool::new(false);

        let ctrl_c = tokio::signal::ctrl_c();
        let cancel_token = tokio_util::sync::CancellationToken::new();

        tokio::task::spawn(async move {
            let _ = ctrl_c.await;
            info!("ctrl_c registered");
            std::process::exit(0);
        });

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
            cancel_token,
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
        let _ = self.tasks.clone().lock_owned().await.spawn(fut);
    }

    pub async fn save(
        &mut self,
        save: Result<OwnedSemaphorePermit, AcquireError>,
    ) -> Result<(), NockAppError> {
        let toggle = self.kernel.buffer_toggle.clone();
        let jam_paths = self.kernel.jam_paths.clone();
        let checkpoint = self.kernel.checkpoint();
        let bytes = checkpoint.encode()?;
        let send_lock = self.watch_send.clone();
        self.tasks.lock().await.spawn(async move {
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
            Ok(())
        });

        Ok(())
    }

    pub async fn work(&mut self) -> Result<(), NockAppError> {
        let tasks_fut = async {
            let mut joinset = self.tasks.clone().lock_owned().await;
            joinset.join_next().await
        };

        if self.cancel_token.is_cancelled() {
            info!("Cancel token received, exiting");
            std::process::exit(1);
        }

        select!(
            res = tasks_fut => {
                match res {
                    Some(Ok(Err(e))) => {
                        if let NockAppError::SaveError(_) = e {
                            error!("{}", e);
                            self.cancel_token.cancel();
                        }
                        Err(e)
                    },
                    Some(Err(e)) => {
                        Err(e)?
                    },
                    _ => {Ok(())},
                }
            },
            permit = self.save_sem.clone().acquire_owned() => {
                //  Check if we should write in the first place
                let curr_event_num = self.kernel.serf.event_num;
                let saved_event_num = self.watch_recv.borrow();
                if curr_event_num <= *saved_event_num {
                    trace!("Skipping save, event number has not changed from: {}", curr_event_num);
                    return Ok(())
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
                return res
            },
            exit = self.exit_recv.recv() => {
                if let Some(code) = exit {
                    self.action_channel.close();
                    self.exit_status.store(true, Ordering::SeqCst);
                    let exit_event_num = self.kernel.serf.event_num;
                    info!("Exit request received, waiting for save checkpoint with event_num {}", exit_event_num);

                    let mut recv = self.watch_recv.clone();
                    tokio::task::spawn(async move {
                        loop {
                            let _ = recv.changed().await;
                            let new = *(recv.borrow());
                            assert!(new <= exit_event_num);
                            if new == exit_event_num {
                                info!("Save event_num reached, exiting with code {}", code);
                                std::process::exit(code as i32);
                            }
                        }
                    });
                    Ok(())
                }
                else {
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
                }
                Ok(())
            }
        )
    }
}
