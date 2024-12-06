use crate::nockapp::driver::{make_driver, IODriverFn, PokeResult, TaskJoinSet};
use crate::nockapp::NockAppError;
use crate::noun::slab::NounSlab;
use crate::Bytes;
use bytes::buf::BufMut;
use std::sync::Arc;

use sword::noun::{D, T};
use sword_macros::tas;
use tokio::io::{split, AsyncReadExt, AsyncWriteExt, ReadHalf, WriteHalf};
use tokio::net::{UnixListener, UnixStream};
use tokio::select;
use tokio::sync::Mutex;
use tokio::task::JoinSet;
use tokio::time::{sleep, Duration};
use tracing::{debug, error};

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
                                    let peek_res = handle.peek(slab).await?;
                                    match peek_res {
                                        Some(mut bind_slab) => {
                                            let peek_res = unsafe { bind_slab.root() };
                                            let peek_res_owned = *peek_res;
                                            let bind_noun = T(&mut bind_slab, &[D(pid), D(tas!(b"bind")), peek_res_owned]);
                                            bind_slab.set_root(bind_noun);
                                            if !write_message(&mut stream_write, bind_slab).await? {
                                                break 'driver;
                                            }
                                        },
                                        None => {
                                            error!("npc: peek failed!");
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
                                    debug!("unexpected message: {:?}", directive_tag);
                                },
                            }
                        },
                        Some(Ok(Ok(None))) => {
                            break 'driver;
                        },
                        Some(Err(e)) => {
                            error!("{e:?}");
                        },
                        Some(Ok(Err(e))) => {
                            error!("{e:?}");
                        },
                        None => {
                            read_message_join_set.spawn(read_message(stream_read_arc.clone()));
                        }
                    }
                },
                effect_res = handle.next_effect() => {
                    debug!("effect_res: {:?}", effect_res);
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

async fn read_message<'a>(
    stream_arc: Arc<Mutex<ReadHalf<UnixStream>>>,
) -> Result<Option<NounSlab<'a>>, NockAppError> {
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
        }
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
            debug!("Connection closed unexpectedly");
            return Ok(None);
        }
        Err(e) => {
            debug!("Error reading size: {:?}", e);
            return Err(NockAppError::IoError(e));
        }
    }
    let size = usize::from_le_bytes(size_bytes);
    debug!("Message size: {} bytes", size);
    let mut buf = Vec::with_capacity(size).limit(size);
    while buf.remaining_mut() > 0 {
        debug!(
            "Reading message content, {} bytes remaining",
            buf.remaining_mut()
        );
        match stream.read_buf(&mut buf).await {
            Ok(0) => {
                debug!("Connection closed while reading message content");
                return Ok(None);
            }
            Ok(_) => {}
            Err(e) => return Err(NockAppError::IoError(e)),
        }
    }
    debug!("Successfully read entire message");
    let mut slab = NounSlab::new();
    let noun = slab.cue_into(Bytes::from(buf.into_inner()))?;
    slab.set_root(noun);
    Ok(Some(slab))
}

async fn write_message<'a>(
    stream: &mut WriteHalf<UnixStream>,
    msg_slab: NounSlab<'a>,
) -> Result<bool, NockAppError> {
    let msg_bytes = msg_slab.jam();
    let msg_len = msg_bytes.len();
    debug!("Attempting to write message of {} bytes", msg_len);
    let mut msg_len_bytes = &msg_len.to_le_bytes()[..];
    let mut msg_buf = &msg_bytes[..];
    while msg_len_bytes.len() > 0 {
        debug!(
            "Writing message length, {} bytes remaining",
            msg_len_bytes.len()
        );
        let bytes = stream
            .write_buf(&mut msg_len_bytes)
            .await
            .map_err(NockAppError::IoError)?;
        if bytes == 0 {
            debug!("Wrote 0 bytes for message length, returning false");
            return Ok(false);
        }
    }
    while msg_buf.len() > 0 {
        debug!("Writing message content, {} bytes remaining", msg_buf.len());
        let bytes = stream
            .write_buf(&mut msg_buf)
            .await
            .map_err(NockAppError::IoError)?;
        if bytes == 0 {
            debug!("Wrote 0 bytes for message content, returning false");
            return Ok(false);
        }
    }
    debug!("Successfully wrote entire message");
    Ok(true)
}
