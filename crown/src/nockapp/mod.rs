use crate::kernel::form::Kernel;
use crate::utils::bytes::BytesWrapper;
use crate::{default_jam_paths, Bytes, CrownError, Noun, NounExt};
use crate::noun::{FromAtom, slab::CueError, slab::NounSlab};
use bincode::{config, encode_to_vec, Decode, Encode};
use bytes::buf::BufMut;
use crc32fast::Hasher;
use futures::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::sync::atomic::Ordering;

use sword::noun::{IndirectAtom, D, T};
use sword_macros::tas;
use thiserror::Error;
use tokio::io::{split, AsyncReadExt, AsyncWriteExt, ReadHalf, WriteHalf};
use tokio::net::{UnixListener, UnixStream};
use tokio::select;
use tokio::fs;
use tokio::sync::{broadcast, mpsc, oneshot, Mutex};
use tokio::task::JoinSet;
use tokio::time::{sleep, Duration};
use tracing::{debug, error, trace};

pub type IODriverFuture = Pin<Box<dyn Future<Output = Result<(), NockAppError>> + Send>>;
pub type IODriverFn = Box<dyn FnOnce(NockAppHandle) -> IODriverFuture>;
pub type TaskJoinSet = JoinSet<Result<(), NockAppError>>;
pub type ActionSender = mpsc::Sender<IOAction>;
pub type ActionReceiver = mpsc::Receiver<IOAction>;
pub type EffectSender = broadcast::Sender<NounSlab>;
pub type EffectReceiver = broadcast::Receiver<NounSlab>;

#[derive(Encode, Decode, PartialEq, Debug, Clone)]
pub struct JammedState {
    pub event_num: u64,
    pub jam_len: usize,
    pub jam: BytesWrapper,
}

#[derive(Encode, Decode, PartialEq, Debug)]
pub struct SlabCheckpoint {
    pub checksum: u32,
    pub slab: JammedState,
}

impl JammedState {
    pub fn new(event_num: u64, jam: Bytes) -> Self {
        Self {
            event_num,
            jam_len: jam.len(),
            jam: BytesWrapper(jam),
        }
    }

    pub fn checkpoint(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        let checksum = self.calculate_checksum();
        let checkpoint = SlabCheckpoint {
            checksum,
            slab: self.clone(),
        };
        encode_to_vec(checkpoint, config::standard())
    }

    pub fn calculate_checksum(&self) -> u32 {
        let mut hasher = Hasher::new();
        hasher.update(&self.event_num.to_le_bytes());
        hasher.update(&self.jam_len.to_le_bytes());
        hasher.update(&self.jam.0);
        hasher.finalize()
    }
}

pub mod http_driver;

/// File IO Driver
///
/// ## Effects
/// `[%file %read path=@t]`
/// results in poke
/// `[%file %read ~]` on read error
/// or
/// `[%file %read ~ contents=@]` on read success
///
///  `[%file %write path=@t contents=@]`
///  results in file written to disk and poke
///  `[%file %write path=@t contents=@ success=?]`
pub fn file() -> IODriverFn {
    make_driver(|handle| async move {
        loop {
            let effect_res = handle.next_effect().await;
            let slab = match effect_res {
                Ok(slab) => slab,
                Err(e) => {
                    eprintln!("Error receiving effect: {:?}", e);
                    continue;
                }
            };

            let Ok(effect_cell) = unsafe { slab.root() }.as_cell() else {
                continue;
            };

            if !unsafe { effect_cell.head().raw_equals(D(tas!(b"file"))) } {
                continue;
            }

            let Ok(file_cell) = effect_cell.tail().as_cell() else {
                continue;
            };

            let (operation, path_atom) = match file_cell.head().as_direct() {
                Ok(tag) if tag.data() == tas!(b"read") => {
                    ("read", file_cell.tail().as_atom().ok())
                }
                Ok(tag) if tag.data() == tas!(b"write") => {
                    let Ok(write_cell) = file_cell.tail().as_cell() else {
                        continue;
                    };
                    ("write", write_cell.head().as_atom().ok())
                }
                _ => continue,
            };

            match (operation, path_atom) {
                ("read", Some(path_atom)) => {
                    let path = String::from_utf8(Vec::from(path_atom.as_bytes()))?;
                    match tokio::fs::read(&path).await {
                        Ok(contents) => {
                            let mut poke_slab = NounSlab::new();
                            let contents_atom = unsafe {
                                IndirectAtom::new_raw_bytes_ref(&mut poke_slab, &contents)
                                    .normalize_as_atom()
                            };
                            let contents_noun = Noun::from_atom(contents_atom);
                            let poke_noun = T(
                                &mut poke_slab,
                                &[D(tas!(b"file")), D(tas!(b"read")), D(0), contents_noun],
                            );
                            poke_slab.set_root(poke_noun);
                            handle.poke(poke_slab).await?;
                        }
                        Err(_) => {
                            let mut poke_slab = NounSlab::new();
                            let poke_noun =
                                T(&mut poke_slab, &[D(tas!(b"file")), D(tas!(b"read")), D(0)]);
                            poke_slab.set_root(poke_noun);
                            handle.poke(poke_slab).await?;
                        }
                    }
                }
                ("write", Some(path_atom)) => {
                    let Ok(write_cell) = file_cell.tail().as_cell() else {
                        continue;
                    };
                    let Ok(contents_atom) = write_cell.tail().as_atom() else {
                        continue;
                    };
                    let path = String::from_utf8(Vec::from(path_atom.as_bytes()))?;
                    let contents = contents_atom.as_bytes();
                    match tokio::fs::write(&path, contents).await {
                        Ok(_) => {
                            let mut poke_slab = NounSlab::new();
                            let poke_noun = T(
                                &mut poke_slab,
                                &[
                                    D(tas!(b"file")),
                                    D(tas!(b"write")),
                                    path_atom.as_noun(),
                                    contents_atom.as_noun(),
                                    D(1),
                                ],
                            );
                            poke_slab.set_root(poke_noun);
                            handle.poke(poke_slab).await?;
                        }
                        Err(_) => {
                            let mut poke_slab = NounSlab::new();
                            let poke_noun = T(
                                &mut poke_slab,
                                &[
                                    D(tas!(b"file")),
                                    D(tas!(b"write")),
                                    path_atom.as_noun(),
                                    contents_atom.as_noun(),
                                    D(0),
                                ],
                            );
                            poke_slab.set_root(poke_noun);
                            handle.poke(poke_slab).await?;
                        }
                    }
                }
                _ => continue,
            }
        }
    })
}

/// NPC Listener IO driver
pub fn npc_listener(listener: UnixListener) -> IODriverFn {
    make_driver(move |mut handle| async move {
        let mut client_join_set = TaskJoinSet::new();
        loop {
            select! {
                stream_res = listener.accept() => {
                    debug!("Accepted new connection");
                    match stream_res {
                        Ok((stream, _)) => {
                            let (my_handle, their_handle) = handle.dup();
                            handle = my_handle;
                            let _ = client_join_set.spawn(npc_client(stream)(their_handle));
                        },
                        Err(e) => {
                            error!("Error accepting connection: {:?}", e);
                        }
                    }
                },
                Some(result) = client_join_set.join_next() => {
                    match result {
                        Ok(Ok(())) => debug!("npc: client task completed successfully"),
                        Ok(Err(e)) => error!("npc: client task error: {:?}", e),
                        Err(e) => error!("npc: client task join error: {:?}", e),
                    }
                },
                // TODO: don't do this, revive robin hood
                _ = sleep(Duration::from_millis(100)) => {
                    // avoid tight-looping
                }
            }
        }
    })
}

/// NPC Client IO driver
pub fn npc_client(stream: UnixStream) -> IODriverFn {
    make_driver(move |handle| async move {
        let (stream_read, mut stream_write) = split(stream);
        let stream_read_arc = Arc::new(Mutex::new(stream_read));
        let mut read_message_join_set = JoinSet::new();
        read_message_join_set.spawn(read_message(stream_read_arc.clone()));

        'driver: loop {
            select! {
                message = read_message_join_set.join_next() => {
                    match message {
                        Some(Ok(Ok(Some(mut slab)))) => {
                            let Ok(message_cell) = unsafe { slab.root() }.as_cell() else {
                                continue;
                            };

                            let (pid, directive_cell) = match (message_cell.head().as_direct(), message_cell.tail().as_cell()) {
                                (Ok(direct), Ok(cell)) => (direct.data(), cell),
                                _ => continue,
                            };

                            let Ok(directive_tag) = directive_cell.head().as_direct() else {
                                continue;
                            };
                            let directive_tag = directive_tag.data();

                            match directive_tag {
                                tas!(b"poke") => {
                                    let poke = T(&mut slab, &[D(tas!(b"npc")), directive_cell.tail()]);
                                    slab.set_root(poke);

                                    let result = handle.poke(slab).await?;
                                    let (tag, noun) = match result {
                                        PokeResult::Ack => (tas!(b"pack"), D(0)),
                                        PokeResult::Nack => (tas!(b"nack"), D(0)),
                                    };

                                    let mut response_slab = NounSlab::new();
                                    let response_noun = T(&mut response_slab, &[D(pid), D(tag), noun]);
                                    response_slab.set_root(response_noun);
                                    if !write_message(&mut stream_write, response_slab).await? {
                                        break 'driver;
                                    }
                                },
                                tas!(b"peek") => {
                                    let path = directive_cell.tail();
                                    slab.set_root(path);
                                    if let Some(mut bind_slab) = handle.peek(slab).await? {
                                        let peek_res = unsafe { bind_slab.root() };
                                        let bind_noun = T(&mut bind_slab, &[D(pid), D(tas!(b"bind")), peek_res]);
                                        bind_slab.set_root(bind_noun);
                                        if !write_message(&mut stream_write, bind_slab).await? {
                                            break 'driver;
                                        }
                                    }
                                },
                                tas!(b"pack") | tas!(b"nack") | tas!(b"bind") => {
                                    let tag = match directive_tag {
                                        tas!(b"pack") => tas!(b"npc-pack"),
                                        tas!(b"nack") => tas!(b"npc-nack"),
                                        tas!(b"bind") => tas!(b"npc-bind"),
                                        _ => unreachable!(),
                                    };
                                    let poke = if tag == tas!(b"npc-bind") {
                                        T(&mut slab, &[D(tag), D(pid), directive_cell.tail()])
                                    } else {
                                        T(&mut slab, &[D(tag), D(pid)])
                                    };
                                    slab.set_root(poke);
                                    if tag == tas!(b"npc-nack") {
                                        handle.poke(slab).await?;
                                    } else {
                                        handle.poke(slab).await?;
                                    }
                                },
                                _ => {
                                    println!("unexpected message: {:?}", directive_tag);
                                },
                            }
                        },
                        Some(Ok(Ok(None))) => {
                            break 'driver;
                        },
                        Some(Err(e)) => {
                            eprintln!("{e:?}");
                        },
                        Some(Ok(Err(e))) => {
                            eprintln!("{e:?}");
                        },
                        None => {
                            read_message_join_set.spawn(read_message(stream_read_arc.clone()));
                        }
                    }
                },
                effect_res = handle.next_effect() => {
                    println!("effect_res: {:?}", effect_res);
                    let mut slab = effect_res?; // Closed error should error driver
                    let Ok(effect_cell) = unsafe { slab.root() }.as_cell() else {
                        continue;
                    };
                    // TODO: distinguish connections
                    if unsafe { effect_cell.head().raw_equals(D(tas!(b"npc"))) } {
                        slab.set_root(effect_cell.tail());
                        if !write_message(&mut stream_write, slab).await? {
                            break 'driver;
                        }
                    }
                }
            }
        }
        Ok(())
    })
}

async fn read_message(
    stream_arc: Arc<Mutex<ReadHalf<UnixStream>>>,
) -> Result<Option<NounSlab>, NockAppError> {
    let mut stream = stream_arc.lock_owned().await;
    let mut size_bytes = [0u8; 8];
    debug!("Attempting to read message size...");
    match stream.read_exact(&mut size_bytes).await {
        Ok(0) => {
            debug!("Connection closed");
            return Ok(None);
        }
        Ok(size) => {
            debug!("Read size: {:?}", size);
        },
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
            debug!("Connection closed unexpectedly");
            return Ok(None);
        }
        Err(e) => {
            debug!("Error reading size: {:?}", e);
            return Err(e.into());
        },
    }
    let size = usize::from_le_bytes(size_bytes);
    debug!("Message size: {} bytes", size);
    let mut buf = Vec::with_capacity(size).limit(size);
    while buf.remaining_mut() > 0 {
        debug!("Reading message content, {} bytes remaining", buf.remaining_mut());
        match stream.read_buf(&mut buf).await {
            Ok(0) => {
                debug!("Connection closed while reading message content");
                return Ok(None);
            }
            Ok(_) => {},
            Err(e) => return Err(e.into()),
        }
    }
    debug!("Successfully read entire message");
    let mut slab = NounSlab::new();
    let noun = slab.cue_into(Bytes::from(buf.into_inner()))?;
    slab.set_root(noun);
    Ok(Some(slab))
}

async fn write_message(
    stream: &mut WriteHalf<UnixStream>,
    msg_slab: NounSlab,
) -> Result<bool, NockAppError> {
    let msg_bytes = msg_slab.jam();
    let msg_len = msg_bytes.len();
    debug!("Attempting to write message of {} bytes", msg_len);
    let mut msg_len_bytes = &msg_len.to_le_bytes()[..];
    let mut msg_buf = &msg_bytes[..];
    while msg_len_bytes.len() > 0 {
        debug!("Writing message length, {} bytes remaining", msg_len_bytes.len());
        let bytes = stream.write_buf(&mut msg_len_bytes).await?;
        if bytes == 0 {
            debug!("Wrote 0 bytes for message length, returning false");
            return Ok(false);
        }
    }
    while msg_buf.len() > 0 {
        debug!("Writing message content, {} bytes remaining", msg_buf.len());
        let bytes = stream.write_buf(&mut msg_buf).await?;
        if bytes == 0 {
            debug!("Wrote 0 bytes for message content, returning false");
            return Ok(false);
        }
    }
    debug!("Successfully wrote entire message");
    Ok(true)
}

pub struct NockApp {
    // Nock kernel
    pub kernel: Kernel,
    // Current join handles for IO drivers (parallel to `drivers`)
    pub tasks: Arc<Mutex<tokio::task::JoinSet<Result<(), NockAppError>>>>,
    // IO action channel
    pub action_channel: mpsc::Receiver<IOAction>,
    // IO action channel sender
    pub action_channel_sender: mpsc::Sender<IOAction>,
    // Effect broadcast channel
    pub effect_broadcast: broadcast::Sender<NounSlab>,
    // Save semaphore
    pub save_sem: Arc<tokio::sync::Semaphore>,
    // Alternating jam dirs
    pub jam_paths: [PathBuf; 2],
    // Jam buffer toggle
    pub buff_toggle: Arc<AtomicBool>,
}

impl NockApp {
    pub fn new(kernel: Kernel) -> Self {
        let (action_channel_sender, action_channel) = mpsc::channel(100);
        let (effect_broadcast, _) = broadcast::channel(100);
        let tasks = Arc::new(Mutex::new(TaskJoinSet::new()));
        let save_sem = Arc::new(tokio::sync::Semaphore::new(1));
        let buff_toggle = Arc::new(AtomicBool::new(false));
        let jam_paths = default_jam_paths("crown");

        Self {
            kernel,
            tasks,
            action_channel,
            action_channel_sender,
            effect_broadcast,
            save_sem,
            jam_paths,
            buff_toggle
        }
    }
    pub async fn add_io_driver(&mut self, driver: IODriverFn) {
        let io_sender = self.action_channel_sender.clone();
        let effect_sender = self.effect_broadcast.clone();
        let effect_receiver = Mutex::new(self.effect_broadcast.subscribe());
        let fut = driver(NockAppHandle {
            io_sender,
            effect_sender,
            effect_receiver,
        });
        let _ = self.tasks.clone().lock_owned().await.spawn(fut);
    }

    pub async fn work(&mut self) -> Result<(), NockAppError> {
        let tasks_fut = async {
            let mut joinset = self.tasks.clone().lock_owned().await;
            joinset.join_next().await
        };

        select!(
            res = tasks_fut => {
                match res {
                    Some(Ok(Err(e))) => {
                        eprintln!("{e:?}");
                    },
                    Some(Err(e)) => {
                        eprintln!("{e:?}");
                    },
                    _ => {},
                }
            },
            save = self.save_sem.clone().acquire_owned() => {
                // TODO: update with noun serialized cold state once ready
                let cold_noun = D(0);
                let mut slab = NounSlab::new();
                let cell = T(&mut slab, &[self.kernel.serf.arvo, cold_noun]);
                slab.set_root(cell);

                let event_num = self.kernel.serf.event_num;
                let jam_dirs = self.jam_paths.clone();
                let toggle = self.buff_toggle.clone();

                let _ = tokio::spawn(async move {
                    let jam = slab.jam();
                    let jammed_state = JammedState::new(event_num, jam);
                    let file = if toggle.load(Ordering::Relaxed) {
                        &jam_dirs[1]
                    } else {
                        &jam_dirs[0]
                    };
                    trace!("Saving arvo checkpoint to {:?}", file);
                    let checkpoint = jammed_state.checkpoint()?;
                    fs::write(file, checkpoint).await?;
                    toggle.store(!toggle.load(Ordering::Relaxed), Ordering::Relaxed);
                    drop(save);

                    Ok::<(), NockAppError>(())
                });
            },
            action_res = self.action_channel.recv() => {
                if let Some(action) = action_res {
                    println!("action: {:?}", action);
                    match action {
                        IOAction::Poke { poke, ack_channel } => {
                            println!("poke slab: {:?}", poke);
                            let poke_noun = poke.copy_to_stack(self.kernel.serf.stack());
                            println!("poke_noun: {:?}", poke_noun);
                            let effects_res = self.kernel.poke(poke_noun);
                            println!("effects_res: {:?}", effects_res);
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
                            println!("path_noun: {:?}", path_noun);
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
            }
        );
        Ok(())
    }
}


pub struct NockAppHandle {
    io_sender: ActionSender,
    pub effect_sender: EffectSender,
    effect_receiver: Mutex<EffectReceiver>,
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
        (
            self,
            NockAppHandle {
                io_sender,
                effect_sender,
                effect_receiver,
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
}

pub fn make_driver<F, Fut>(f: F) -> IODriverFn
where
    F: FnOnce(NockAppHandle) -> Fut + Send + 'static,
    Fut: Future<Output = Result<(), NockAppError>> + Send + 'static,
{
    Box::new(move |handle| Box::pin(f(handle)))
}
