use crate::mpsc::Receiver;

use std::ffi::CStr;
use std::sync::mpsc;
use std::sync::mpsc::SyncSender;
use sword_macros::tas;

use axum::body::Body;
use axum::extract::State;
use axum::http::{HeaderMap, Method, StatusCode, Uri};
use axum::response::Response;
use axum::routing::any;
use crown::Bytes;
use tokio::sync::oneshot;

use crown::AtomExt;
use sword::noun::{Atom, D, T};

use crown::kernel::boot;

use clap::{command, ColorChoice, Parser};
static KERNEL_JAM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/http.jam"));

use crown::kernel::boot::Cli as BootCli;

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

type Responder = oneshot::Sender<Result<Response, StatusCode>>;

struct RequestMessage {
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::sync_channel::<RequestMessage>(0);

    let app = any(sword_handler).with_state(tx);

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();

    tokio::spawn(async move { axum::serve(listener, app.into_make_service()).await });

    let _ = manage_kernel(rx).await;

    Ok(())
}

async fn sword_handler(
    method: Method,
    headers: HeaderMap,
    uri: Uri,
    State(sender): State<SyncSender<RequestMessage>>,
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
        uri: uri,
        method: method,
        headers: headers,
        body: opt_body,
        resp: resp_tx,
    };

    let send_res = sender.send(msg);

    // Await the response
    if let Ok(result) = resp_rx.await {
        result
    } else {
        Err(StatusCode::INTERNAL_SERVER_ERROR)
    }
}

async fn manage_kernel(rx: Receiver<RequestMessage>) -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut kernel = boot::setup_form(KERNEL_JAM, Some(cli.boot))?;

    loop {
        // Start receiving messages
        if let Ok(msg) = rx.recv() {
            let uri_bytes = msg.uri.to_string().as_bytes().to_vec();
            let uri = Atom::from_bytes(kernel.serf.stack(), &Bytes::from(uri_bytes)).as_noun();

            let method_bytes = msg.method.to_string().as_bytes().to_vec();
            let method =
                Atom::from_bytes(kernel.serf.stack(), &Bytes::from(method_bytes)).as_noun();

            let mut headers = D(0);
            for (k, v) in msg.headers {
                let key = k.unwrap().as_str().to_string();
                let val = v.to_str().unwrap().to_string();
                let k_atom = Atom::from_bytes(kernel.serf.stack(), &Bytes::from(key)).as_noun();
                let v_atom = Atom::from_bytes(kernel.serf.stack(), &Bytes::from(val)).as_noun();
                let header_cell = T(kernel.serf.stack(), &[k_atom, v_atom]);
                headers = T(kernel.serf.stack(), &[header_cell, headers]);
            }

            let body: crown::Noun = {
                if let Some(bod) = msg.body {
                    let ato = Atom::from_bytes(kernel.serf.stack(), &bod).as_noun();
                    T(
                        kernel.serf.stack(),
                        &[D(0), D(bod.len().try_into().unwrap()), ato],
                    )
                } else {
                    D(0)
                }
            };

            let poke = {
                T(
                    kernel.serf.stack(),
                    &[D(tas!(b"req")), uri, method, headers, body],
                )
            };

            let mut do_poke = || -> Result<ResponseBuilder, crown::CrownError> {
                let poke_result = kernel.poke(poke)?;
                let res_list = poke_result.as_cell()?;
                let res = res_list.head().as_cell()?.tail().as_cell()?;
                let status_code = res
                    .head()
                    .as_atom()?
                    .direct()
                    .expect("not a valid status code!")
                    .data();
                let mut header_list = res.tail().as_cell()?.head();
                let mut header_vec: Vec<(String, String)> = Vec::new();
                loop {
                    if header_list.is_atom() {
                        break;
                    } else {
                        let header = header_list.as_cell()?.head().as_cell()?;
                        let key_vec = header.head().as_atom()?;
                        let val_vec = header.tail().as_atom()?;

                        if let Ok(key) = CStr::from_bytes_until_nul(key_vec.as_bytes()) {
                            if let Ok(val) = CStr::from_bytes_until_nul(val_vec.as_bytes()) {
                                header_vec.push((
                                    String::from_utf8(key.to_bytes().to_vec())?,
                                    String::from_utf8(val.to_bytes().to_vec())?
                                ));
                                header_list = header_list.as_cell()?.tail();
                            } else { break; }
                        } else { break; }
                    }
                }

                let maybe_body = res.tail().as_cell()?.tail();

                let body: Option<Bytes> = {
                    if maybe_body.is_cell() {
                        let body_octs = maybe_body.as_cell()?.tail().as_cell()?;
                        let body_len = body_octs
                            .head()
                            .as_atom()?
                            .direct()
                            .expect("body len")
                            .data();
                        let mut body_vec: Vec<u8> = b"0".repeat(body_len.try_into().unwrap());
                        let body_atom = body_octs.tail().as_atom()?;
                        body_vec.copy_from_slice(body_atom.as_bytes());
                        Some(Bytes::from(body_vec))
                    } else {
                        None
                    }
                };

                if let Ok(status) = StatusCode::from_u16(status_code as u16) {
                    Ok(ResponseBuilder {
                        status_code: status,
                        headers: header_vec,
                        body: body,
                    })
                } else {
                    Err(crown::CrownError::Unknown(
                        "Invalid status code".to_string(),
                    ))
                }
            };

            if let Ok(res_builder) = do_poke() {
                let mut res = Response::builder().status(res_builder.status_code);

                for (k, v) in res_builder.headers {
                    res = res.header(k, v);
                }

                let bod = res_builder.body.ok_or("invalid response")?;
                let _ = msg.resp.send(Ok(res.body(Body::from(bod)).unwrap()));
            } else {
                println!("statuscode internal server error");
                let _ = msg.resp.send(Err(StatusCode::INTERNAL_SERVER_ERROR));
            }
        }
    }
}
