use crate::nockapp::driver::{make_driver, IODriverFn, PokeResult, TaskJoinSet};
use crate::nockapp::wire::Wire;
use crate::nockapp::NockAppError;
use crate::noun::slab::NounSlab;
use crate::utils::make_tas;
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

pub enum NpcWire {
    Poke(u64),
    Pack(u64),
    Nack(u64),
    Bind(u64),
}

impl Wire for NpcWire {
    const VERSION: u64 = 1;
    const SOURCE: &'static str = "npc";

    fn to_noun_slab(&self) -> NounSlab {
        let mut slab = NounSlab::new();
        let source = make_tas(&mut slab, NpcWire::SOURCE).as_noun();
        let wire = match self {
            NpcWire::Poke(pid) => T(
                &mut slab,
                &[source, D(NpcWire::VERSION), D(tas!(b"poke")), D(*pid), D(0)],
            ),
            NpcWire::Pack(pid) => T(
                &mut slab,
                &[source, D(NpcWire::VERSION), D(tas!(b"pack")), D(*pid), D(0)],
            ),
            NpcWire::Nack(pid) => T(
                &mut slab,
                &[source, D(NpcWire::VERSION), D(tas!(b"nack")), D(*pid), D(0)],
            ),
            NpcWire::Bind(pid) => T(
                &mut slab,
                &[source, D(NpcWire::VERSION), D(tas!(b"bind")), D(*pid), D(0)],
            ),
        };
        slab.set_root(wire);
        slab
    }
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
                            debug!("npc_client: read message");
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
                                    debug!("npc_client: poke");
                                    let mut poke_slab = NounSlab::new();
                                    let poke = directive_cell.tail();
                                    poke_slab.copy_into(poke);
                                    let wire = NpcWire::Poke(pid).to_noun_slab();
                                    let result = handle.poke(wire, poke_slab).await?;
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
                                    debug!("npc_client: peek");
                                    let path = directive_cell.tail();
                                    slab.set_root(path);
                                    let peek_res = handle.peek(slab).await?;
                                    match peek_res {
                                        Some(mut bind_slab) => {
                                            let peek_res = unsafe { bind_slab.root() };
                                            let bind_noun = T(&mut bind_slab, &[D(pid), D(tas!(b"bind")), peek_res]);
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
                                    debug!("npc_client: pack, nack, or bind");
                                    let tag = match directive_tag {
                                        tas!(b"pack") => tas!(b"npc-pack"),
                                        tas!(b"nack") => tas!(b"npc-nack"),
                                        tas!(b"bind") => tas!(b"npc-bind"),
                                        _ => unreachable!(),
                                    };
                                    let wire = match directive_tag {
                                        tas!(b"pack") => NpcWire::Pack(pid),
                                        tas!(b"nack") => NpcWire::Nack(pid),
                                        tas!(b"bind") => NpcWire::Bind(pid),
                                        _ => unreachable!(),
                                    };
                                    let poke = if tag == tas!(b"npc-bind") {
                                        T(&mut slab, &[D(tag), D(pid), directive_cell.tail()])
                                    } else {
                                        T(&mut slab, &[D(tag), D(pid)])
                                    };
                                    slab.set_root(poke);

                                    if tag == tas!(b"npc-nack") {
                                        handle.poke(wire.to_noun_slab(), slab).await?;
                                    } else {
                                        handle.poke(wire.to_noun_slab(), slab).await?;
                                    }
                                },
                                _ => {
                                    debug!("npc_client: unexpected message: {:?}", directive_tag);
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

#[cfg(test)]
mod tests {
    use crate::nockapp::driver::{IOAction, NockAppHandle};

    use super::*;
    use std::io::{Read, Write};
    use std::os::unix::net::UnixStream as StdUnixStream;
    use std::time::Duration;
    use tempfile::tempdir;
    use tokio::net::UnixStream;
    use tokio::sync::{broadcast, mpsc};
    use tokio::time::timeout;
    use tracing_test::traced_test;

    async fn setup_socket_pair() -> (UnixStream, StdUnixStream) {
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("test.sock");
        let listener = UnixListener::bind(&socket_path).unwrap();

        let client = StdUnixStream::connect(&socket_path).unwrap();
        let (server, _) = listener.accept().await.unwrap();

        (server, client)
    }

    #[tokio::test]
    async fn test_npc_wire_variants() {
        let test_cases = vec![
            (NpcWire::Poke(123), tas!(b"poke")),
            (NpcWire::Pack(456), tas!(b"pack")),
            (NpcWire::Nack(789), tas!(b"nack")),
            (NpcWire::Bind(101), tas!(b"bind")),
        ];

        for (wire, expected_tag) in test_cases {
            let mut slab = wire.to_noun_slab();
            let root = unsafe { slab.root() };
            let cell = root.as_cell().unwrap();

            // Test source
            assert_eq!(
                cell.head().as_direct().unwrap().data(),
                make_tas(&mut slab, NpcWire::SOURCE)
                    .as_noun()
                    .as_direct()
                    .unwrap()
                    .data()
            );

            // Test version and tag
            let rest = cell.tail().as_cell().unwrap();
            assert_eq!(rest.head().as_direct().unwrap().data(), 1);

            let tag_cell = rest.tail().as_cell().unwrap();
            assert_eq!(tag_cell.head().as_direct().unwrap().data(), expected_tag);
        }
    }

    #[tokio::test]
    async fn test_npc_poke_wire_format() {
        let mut slab = NpcWire::Poke(123).to_noun_slab();
        let root = unsafe { slab.root() };
        let cell = root.as_cell().unwrap();

        // Test source
        assert_eq!(
            cell.head().as_direct().unwrap().data(),
            make_tas(&mut slab, NpcWire::SOURCE)
                .as_noun()
                .as_direct()
                .unwrap()
                .data()
        );

        // Test version
        let rest = cell.tail().as_cell().unwrap();
        assert_eq!(rest.head().as_direct().unwrap().data(), 1);

        // Test tag and pid
        let tag_cell = rest.tail().as_cell().unwrap();
        assert_eq!(tag_cell.head().as_direct().unwrap().data(), tas!(b"poke"));

        let pid_cell = tag_cell.tail().as_cell().unwrap();
        assert_eq!(pid_cell.head().as_direct().unwrap().data(), 123);
    }

    #[tokio::test]
    async fn test_write_message_format() {
        let (server, mut client) = setup_socket_pair().await;
        let (_, mut writer) = split(server);

        let mut test_slab = NounSlab::new();
        let test_noun = T(&mut test_slab, &[D(123), D(456)]);
        test_slab.set_root(test_noun);

        write_message(&mut writer, test_slab).await.unwrap();

        let mut size_buf = [0u8; 8];
        client.read_exact(&mut size_buf).unwrap();
        let size = usize::from_le_bytes(size_buf);

        let mut msg_buf = vec![0u8; size];
        client.read_exact(&mut msg_buf).unwrap();

        let mut received_slab = NounSlab::new();
        let received_noun = received_slab.cue_into(Bytes::from(msg_buf)).unwrap();
        received_slab.set_root(received_noun);

        let root = unsafe { received_slab.root() };
        let cell = root.as_cell().unwrap();

        assert_eq!(cell.head().as_direct().unwrap().data(), 123);
        assert_eq!(cell.tail().as_direct().unwrap().data(), 456);
    }

    #[tokio::test]
    async fn test_write_message_empty() {
        let (server, mut client) = setup_socket_pair().await;
        let (_, mut writer) = split(server);

        let mut test_slab = NounSlab::new();
        let test_noun = T(&mut test_slab, &[D(0), D(0)]);
        test_slab.set_root(test_noun);

        assert!(write_message(&mut writer, test_slab).await.unwrap());

        let mut size_buf = [0u8; 8];
        client.read_exact(&mut size_buf).unwrap();
        assert!(usize::from_le_bytes(size_buf) > 0);
    }

    #[tokio::test]
    async fn test_read_message_eof() {
        let (server, client) = setup_socket_pair().await;
        drop(client);

        let stream_arc = Arc::new(Mutex::new(split(server).0));
        let result = read_message(stream_arc).await;
        assert!(result.unwrap().is_none());
    }

    #[tokio::test]
    #[traced_test]
    async fn test_npc_driver() {
        // Setup
        let dir = tempdir().unwrap();
        let socket_path = dir.path().join("test.sock");
        let listener = UnixListener::bind(&socket_path).unwrap();

        // Create channels for driver communication
        let (tx_io, mut rx_io) = mpsc::channel(32);
        let (tx_effect, rx_effect) = broadcast::channel(32);
        let (tx_exit, _) = mpsc::channel(1);

        let handle = NockAppHandle {
            io_sender: tx_io,
            effect_sender: tx_effect.clone(),
            effect_receiver: Mutex::new(rx_effect),
            exit: tx_exit,
        };

        // Spawn the listener driver
        let _driver_task = tokio::spawn(npc_listener(listener)(handle));

        // Connect client
        let mut client = StdUnixStream::connect(&socket_path).unwrap();

        // Create test noun slab
        let mut test_slab = NounSlab::new();
        let msg_noun = T(&mut test_slab, &[D(tas!(b"poke")), D(123), D(456)]);
        let test_noun = T(&mut test_slab, &[D(1), msg_noun]);
        test_slab.set_root(test_noun);

        // Jam the noun to bytes
        let msg_bytes = test_slab.jam();
        let msg_len = msg_bytes.len();

        // Write length prefix and jammed noun
        client.write_all(&(msg_len as u64).to_le_bytes()).unwrap();
        client.write_all(&msg_bytes).unwrap();

        debug!("client: wrote {} bytes", msg_len);

        // Verify driver received poke
        if let Some(IOAction::Poke {
            wire: wire_slab,
            poke: noun_slab,
            ack_channel: _,
        }) = timeout(Duration::from_secs(1), rx_io.recv()).await.unwrap()
        {
            let wire = unsafe { wire_slab.root() };
            let wire_cell = wire.as_cell().unwrap();

            debug!("test_npc_driver: wire: {:?}", wire);
            debug!("test_npc_driver: poke data: {:?}", unsafe {
                noun_slab.root()
            });
            // Verify wire format
            assert_eq!(
                wire_cell.head().as_direct().unwrap().data(),
                make_tas(&mut wire_slab.clone(), NpcWire::SOURCE)
                    .as_noun()
                    .as_direct()
                    .unwrap()
                    .data()
            );

            let rest = wire_cell.tail().as_cell().unwrap();
            assert_eq!(rest.head().as_direct().unwrap().data(), 1);

            let tag_cell = rest.tail().as_cell().unwrap();
            assert_eq!(tag_cell.head().as_direct().unwrap().data(), tas!(b"poke"));

            // Verify noun content
            let noun = unsafe { noun_slab.root() };
            let noun_cell = noun.as_cell().unwrap();
            assert_eq!(noun_cell.head().as_direct().unwrap().data(), 123);
            assert_eq!(noun_cell.tail().as_direct().unwrap().data(), 456);

        // TODO: make this work
        /* ack_channel.send(PokeResult::Ack).unwrap();

        // Send effect through broadcast channel
        let mut ack_slab = NounSlab::new();
        let ack = T(&mut ack_slab.clone(), &[
            D(tas!(b"npc")),
            T(&mut ack_slab.clone(), &[D(123), D(tas!(b"pack")), D(0)])
        ]);
        ack_slab.set_root(ack);
        tx_effect.send(ack_slab).unwrap();

        // Verify client receives ack
        let mut size_buf = [0u8; 8];
        client.read_exact(&mut size_buf).unwrap();
        let size = usize::from_le_bytes(size_buf);

        let mut msg_buf = vec![0u8; size];
        client.read_exact(&mut msg_buf).unwrap();

        let mut received_slab = NounSlab::new();
        let received_noun = received_slab.cue_into(Bytes::from(msg_buf)).unwrap();
        received_slab.set_root(received_noun);

        let root = unsafe { received_slab.root() };
        let cell = root.as_cell().unwrap();
        assert_eq!(cell.head().as_direct().unwrap().data(), 123);
        let rest = cell.tail().as_cell().unwrap();
        assert_eq!(rest.head().as_direct().unwrap().data(), tas!(b"pack"));
        assert_eq!(rest.tail().as_direct().unwrap().data(), 0); */
        } else {
            panic!("Did not receive poke message");
        }

        // Cleanup
        drop(client);
    }
}
