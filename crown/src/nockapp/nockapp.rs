use super::driver::{IOAction, IODriverFn, NockAppHandle, PokeResult, TaskJoinSet};
use super::NockAppError;
use crate::kernel::checkpoint::JamPaths;
use crate::kernel::form::{Kernel, KernelArgs};
use crate::noun::slab::NounSlab;
use crate::utils::NOCK_STACK_SIZE;
use crate::NounExt;
use std::sync::Arc;

use sword::mem::NockStack;
use tokio::io::AsyncWriteExt;
use tokio::sync::{broadcast, mpsc, oneshot, Mutex};
use tokio::task::JoinSet;
use tokio::time::{interval, Duration, Interval};
use tokio::{fs, select};
use tracing::{error, info, trace, warn};

pub struct NockApp {
    /// Current join handles for IO drivers (parallel to `drivers`)
    pub tasks: Arc<Mutex<tokio::task::JoinSet<Result<(), NockAppError>>>>,
    /// Exit signal sender
    pub exit_send: mpsc::Sender<usize>,
    /// Exit signal receiver
    pub exit_recv: mpsc::Receiver<usize>,
    /// IO action channel sender
    pub action_channel_sender: mpsc::Sender<IOAction>,
    /// Effect broadcast channel
    pub effect_broadcast: broadcast::Sender<NounSlab>,
    /// Save interval
    pub save_interval: Interval,
    /// Jam persistence buffer paths.
    pub jam_paths: JamPaths,
    ///
    pub save_mutex: Arc<Mutex<()>>,
    /// Cancel token
    pub cancel_token: tokio_util::sync::CancellationToken,
}

impl NockApp {
    pub async fn new(args: KernelArgs, save_duration: Duration) -> Self {
        let jam_paths = args.jam_paths.clone();
        let (action_channel_sender, action_channel) = mpsc::channel(100);
        let (effect_broadcast, _) = broadcast::channel(100);
        let tasks = Arc::new(Mutex::new(TaskJoinSet::new()));
        let (exit_send, exit_recv) = mpsc::channel(1);
        let mut save_interval = interval(save_duration);
        save_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
        let save_mutex = Arc::new(Mutex::new(()));

        let ctrl_c = tokio::signal::ctrl_c();
        let cancel_token = tokio_util::sync::CancellationToken::new();

        spawn_worker(
            tasks.clone(),
            args,
            action_channel,
            effect_broadcast.clone(),
        )
        .await;

        tokio::task::spawn(async move {
            let _ = ctrl_c.await;
            info!("ctrl_c registered");
            std::process::exit(0);
        });

        Self {
            tasks,
            exit_send,
            exit_recv,
            action_channel_sender,
            effect_broadcast,
            save_interval,
            jam_paths,
            save_mutex,
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

    pub async fn save(&mut self, exit_after: Option<i32>) -> Result<(), NockAppError> {
        let save_mutex = self.save_mutex.clone();
        let jam_paths = self.jam_paths.clone();
        let (checkpoint_sender, checkpoint_future) = oneshot::channel();
        let sender = self.action_channel_sender.clone();
        self.tasks.lock().await.spawn(async move {
            sender
                .send(IOAction::Checkpoint {
                    result_channel: checkpoint_sender,
                })
                .await
                .expect("Could not request checkpoint from kernel thread");
            let guard = save_mutex.lock().await;
            let checkpoint = checkpoint_future.await.expect("Failed to get checkpoint");
            let buff_index = checkpoint.buff_index;
            let bytes = checkpoint.encode()?;
            let path = if buff_index { jam_paths.1 } else { jam_paths.0 };
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
            if let Some(code) = exit_after {
                std::process::exit(code);
            }
            drop(guard);
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
            _instant = self.save_interval.tick() => {
                self.save(None).await;
              Ok(())
            },
            exit = self.exit_recv.recv() => {
                if let Some(code) = exit {
                    self.save(Some(code as i32)).await;
                } else {
                    warn!("Exit channel closed prematurely!");
                }
                Ok(())
            }
        )
    }
}

async fn spawn_worker(
    joinset: Arc<Mutex<JoinSet<Result<(), NockAppError>>>>,
    args: KernelArgs,
    mut io_receiver: mpsc::Receiver<IOAction>,
    effect_broadcast: broadcast::Sender<NounSlab>,
) {
    joinset.lock_owned().await.spawn_blocking(move || {
        let mut stack = NockStack::new(NOCK_STACK_SIZE, 0);
        let checkpoint = if args.jam_paths.checkpoint_exists() {
            info!("Checkpoint file(s) found, validating and loading from jam");
            args.jam_paths.load_checkpoint(&mut stack).ok()
        } else {
            info!("No checkpoint file found, starting from scratch");
            None
        };

        let mut buffer_toggle = checkpoint
            .as_ref()
            .map_or_else(|| false, |snapshot| !snapshot.buff_index);
        let mut kernel = Kernel::load(args, stack, checkpoint);
        while let Some(action) = io_receiver.blocking_recv() {
            match action {
                IOAction::Poke { poke, ack_channel } => {
                    let poke_noun = poke.copy_to_stack(kernel.serf.stack());
                    let effects_res = kernel.poke(poke_noun);
                    match effects_res {
                        Ok(effects) => {
                            ack_channel
                                .send(PokeResult::Ack)
                                .unwrap_or_else(|_err| warn!("Could not send poke ack."));
                            for effect in effects.list_iter() {
                                let mut effect_slab = NounSlab::new();
                                effect_slab.copy_into(effect);
                                let _sent =
                                    effect_broadcast.send(effect_slab).unwrap_or_else(|_err| {
                                        warn!("Could not send effect.");
                                        0
                                    });
                            }
                        }
                        Err(err) => {
                            trace!("Poke error: {err}");
                            ack_channel
                                .send(PokeResult::Nack)
                                .unwrap_or_else(|_err| warn!("Could not send poke nack."));
                        }
                    }
                }
                IOAction::Peek {
                    path,
                    result_channel,
                } => {
                    let path_noun = path.copy_to_stack(kernel.serf.stack());
                    let peek_res = kernel.peek(path_noun);
                    match peek_res {
                        Ok(res_noun) => {
                            let mut res_slab = NounSlab::new();
                            res_slab.copy_into(res_noun);
                            let _ = result_channel.send(Some(res_slab));
                        }
                        Err(err) => {
                            trace!("Peek error: {err}");
                            let _ = result_channel.send(None);
                        }
                    }
                }
                IOAction::Checkpoint { result_channel } => {
                    let checkpoint = kernel.checkpoint(&mut buffer_toggle);
                    result_channel.send(checkpoint);
                }
            }
        }
        Ok(())
    });
}
