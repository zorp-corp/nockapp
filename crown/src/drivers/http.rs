use crate::nockapp::driver::{make_driver, IODriverFn, PokeResult};
use crate::nockapp::NockAppError;
use crate::noun::slab::{NounRef, NounSlab};
use crate::{AtomExt, Bytes};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use axum::body::Body;
use axum::extract::State;
use axum::http::{HeaderMap, Method, StatusCode, Uri};
use axum::response::Response;
use axum::routing::any;
use sword::noun::{Atom, D, T};
use sword_macros::tas;
use tokio::select;
use tokio::sync::{oneshot, RwLock};
use tracing::debug;

type Responder = oneshot::Sender<Result<Response, StatusCode>>;
#[derive(Debug)]
struct RequestMessage {
    id: u64,
    uri: Uri,
    method: Method,
    headers: HeaderMap,
    body: Option<axum::body::Bytes>,
    resp: Responder,
}

struct ResponseBuilder {
    status_code: StatusCode,
    headers: Vec<(String, String)>,
    body: Option<axum::body::Bytes>,
}

static COUNTER: AtomicU64 = AtomicU64::new(0);
// wraps on overflow
fn get_id() -> u64 {
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// HTTP IO driver
pub fn http() -> IODriverFn {
    make_driver(move |handle| async move {
        let (tx, mut rx) = tokio::sync::mpsc::channel::<RequestMessage>(10);
        let app = any(sword_handler).with_state(tx);

        let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
        debug!("listening on {}", listener.local_addr().unwrap());
        tokio::spawn(async move {
            axum::serve(listener, app.into_make_service())
                .await
                .unwrap();
        });

        let channel_map = RwLock::new(HashMap::<u64, Responder>::new());

        loop {
            // Start receiving messages
            select! {
                msg = rx.recv() => {
                    let msg = msg.unwrap();
                    channel_map.write().await.insert(msg.id, msg.resp);
                    let mut slab = NounSlab::new();
                    let id =  Atom::from_value(&mut slab, msg.id).unwrap().as_noun();
                    let uri =
                        Atom::from_value(&mut slab, msg.uri.to_string()).unwrap().as_noun();

                    let method =
                        Atom::from_value(&mut slab, msg.method.to_string()).unwrap().as_noun();

                    let mut headers = D(0);
                    for (k, v) in msg.headers {
                        let key = k.unwrap().as_str().to_string();
                        let val = v.to_str().unwrap().to_string();
                        let k_atom = Atom::from_value(&mut slab, key).unwrap();
                        let v_atom = Atom::from_value(&mut slab, val).unwrap();
                        let header_cell = T(&mut slab, &[k_atom.as_noun(), v_atom.as_noun()]);
                        headers = T(&mut slab, &[header_cell, headers]);
                    }

                    let body: crate::Noun = {
                        if let Some(bod) = msg.body {
                            let ato = Atom::from_bytes(&mut slab, &bod).as_noun();
                            T(
                                &mut slab,
                                &[D(0), D(bod.len().try_into().unwrap()), ato],
                            )
                        } else {
                            D(0)
                        }
                    };

                    let poke = {
                        T(
                            &mut slab,
                            &[D(tas!(b"req")), id, uri, method, headers, body],
                        )
                    };
                    debug!("poking: {:?}", poke);
                    slab.set_root(poke);

                    let poke_result = handle.poke(slab).await?;
                    debug!("poke result: {:?}", poke_result);

                    if let PokeResult::Nack = poke_result {
                        return Err(NockAppError::PokeFailed);
                    }
                }
                effect = handle.next_effect() => {
                    let (resp, id) = {
                        debug!("effect: {:?}", effect);
                        let unwrapped = effect.unwrap();
                        let effect = unsafe { unwrapped.root() };
                        let res_list = effect.as_cell()?;
                        let res_tail = res_list.tail();
                        let res = res_tail.as_cell()?;
                        let id = res.head().as_atom()?.as_u64().unwrap();
                        let next_tail = res.tail();
                        let mut new_slab = NounSlab::new();
                        next_tail.as_cell()?.as_noun().copy_into(&mut new_slab);
                        let new_root_res = unsafe { new_slab.root() };
                        let res = new_root_res.as_cell()?;
                        let status_code = res
                            .head()
                            .as_atom()?
                            .direct()
                            .expect("not a valid status code!")
                            .data();
                        let res_tail = res.tail();
                        let res_tail_cell = res_tail.as_cell()?;
                        let mut header_vec: Vec<(String, String)> = Vec::new();
                        let mut current_slab = NounSlab::new();
                        res_tail_cell.head().copy_into(&mut current_slab);
                        loop {
                            let root = unsafe { current_slab.root() };
                            if root.is_atom() {
                                break;
                            } else {
                                let header_list_cell = root.as_cell()?;
                                let header_list_cell_head = header_list_cell.head();
                                let header = header_list_cell_head.as_cell()?;
                                let header_head = header.head();
                                let header_tail = header.tail();
                                let key_vec = header_head.as_atom()?;
                                let val_vec = header_tail.as_atom()?;

                                if let Ok(key) = key_vec.to_bytes_until_nul() {
                                    if let Ok(val) = val_vec.to_bytes_until_nul() {
                                        header_vec.push((
                                            String::from_utf8(key)?,
                                            String::from_utf8(val)?,
                                        ));
                                        let mut new_slab = NounSlab::new();
                                        header_list_cell.tail().copy_into(&mut new_slab);
                                        current_slab = new_slab;
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                        }

                        let res_tail = res.tail();
                        let res_tail_cell = res_tail.as_cell()?;
                        let maybe_body = res_tail_cell.tail();

                        let body: Option<Bytes> = {
                            if maybe_body.is_cell() {
                                let maybe_body_cell = maybe_body.as_cell()?;
                                let maybe_body_tail = maybe_body_cell.tail();
                                let body_octs = maybe_body_tail.as_cell()?;
                                let body_len = body_octs
                                    .head()
                                    .as_atom()?
                                    .direct()
                                    .expect("body len")
                                    .data();
                                let mut body_vec: Vec<u8> = b"0".repeat(body_len.try_into().unwrap());
                                let body_tail = body_octs.tail();
                                let body_atom = body_tail.as_atom()?;
                                body_vec.copy_from_slice(&body_atom.to_bytes_until_nul().unwrap());
                                Some(Bytes::from(body_vec))
                            } else {
                                None
                            }
                        };

                        let resp = if let Ok(status) = StatusCode::from_u16(status_code as u16) {
                            let res_builder = ResponseBuilder {
                                status_code: status,
                                headers: header_vec,
                                body: body,
                            };

                            let mut res = Response::builder().status(res_builder.status_code);

                            for (k, v) in res_builder.headers {
                                res = res.header(k, v);
                            }

                            let bod = res_builder.body.ok_or("invalid response").unwrap();
                            Ok(res.body(Body::from(bod)).unwrap())
                        } else {
                            debug!("statuscode internal server error");
                            Err(StatusCode::INTERNAL_SERVER_ERROR)
                        };
                        (resp, id)
                    };
                    let resp_tx = channel_map.write().await.remove(&id).unwrap();
                    let _ = resp_tx.send(resp);
                }
            }
        }
    })
}

async fn sword_handler(
    method: Method,
    headers: HeaderMap,
    uri: Uri,
    State(sender): State<tokio::sync::mpsc::Sender<RequestMessage>>,
    body: axum::body::Bytes,
) -> Result<Response, StatusCode> {
    let (resp_tx, resp_rx) = oneshot::channel::<Result<Response, StatusCode>>();
    let opt_body: Option<axum::body::Bytes> = {
        if body.len() == 0 {
            None
        } else {
            Some(body)
        }
    };
    let msg = RequestMessage {
        id: get_id(),
        uri,
        method,
        headers,
        body: opt_body,
        resp: resp_tx,
    };

    let _ = sender.send(msg).await;

    // Await the response
    if let Ok(result) = resp_rx.await {
        result
    } else {
        Err(StatusCode::INTERNAL_SERVER_ERROR)
    }
}
