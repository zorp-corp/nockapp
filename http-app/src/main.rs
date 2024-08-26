use std::sync::mpsc;
use std::sync::mpsc::SyncSender;
use crate::mpsc::Receiver;

use tokio::sync::oneshot;
use axum::body::Body;
use axum::extract::State;
use axum::http::{HeaderMap, Method, StatusCode, Uri};
use axum::response::Response;
use axum::routing::any;

use sword::noun::D;
use sword::noun::T;
use crown::kernel::boot;
use crown::kernel::form::Kernel;
use crown::utils::make_tas;
use crown::Noun;
use tracing::info;

use clap::{arg, command, ColorChoice, Parser};
static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/kernel.jam"));

use crown::kernel::boot::Cli as BootCli;

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,

    #[command(subcommand)]
    command: Command,
}

#[derive(Parser, Debug)]
enum Command {
    #[command(about = "Serve a simple message over HTTP")]
    ServeMessage {
        #[arg(help = "The message")]
        n: String,
    },
}

type Responder = oneshot::Sender<Result<Response, StatusCode>>;

struct Message {
    data: String,
    resp: Responder
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::sync_channel::<Message>(1);

    let app = any(sword_handler).with_state(tx);

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080")
        .await
        .unwrap();

    tokio::spawn(async move {
        axum::serve(listener, app.into_make_service()).await
    });

    manage_kernel(rx).await;

    Ok(())
}

async fn manage_kernel(rx: Receiver<Message>) -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut kernel = boot::setup_form(KERNEL_JAM, Some(cli.boot))?;

    loop {
        println!("manager looping");
         // Start receiving messages
        if let Ok(msg) = rx.recv() {
            println!("{}", msg.data);
            let poke = {
                let eve_tas = make_tas(kernel.serf.stack(), "serve-message");
                let uri_tas = make_tas(kernel.serf.stack(), msg.data.as_str());
                create_poke(&mut kernel, &[eve_tas.as_noun(), uri_tas.as_noun(), D(0)])
            };

            let mut do_datom = || -> Result<u64, crown::CrownError> {
                info!("Sending poke: {:?}", poke);
                let poke_result = kernel.poke(poke)?;
                info!("Poke response: {:?}", poke_result);

                let d: u64 = poke_result
                    .as_cell()?
                    .head()
                    .as_atom()?
                    .direct()
                    .unwrap()
                    .data();
                Ok(d)
            };

            if let Ok(datom) = do_datom() {
                let vec_u8 = datom.to_le_bytes().to_vec();
                let body = Body::from(vec_u8);
                let res = Response::builder()
                    .status(StatusCode::OK)
                    .header("content-type", "text/html")
                    .body(body)
                    .unwrap();

                let _ = msg.resp.send(Ok(res));
            } else {
                println!("statuscode internal server error");
                let _ = msg.resp.send(Err(StatusCode::INTERNAL_SERVER_ERROR));
            }
        }
    }

    Ok(())
}

async fn sword_handler(
    _method: Method,
    _headers: HeaderMap,
    uri: Uri,
    State(sender): State<SyncSender<Message>>,
) -> Result<Response, StatusCode> {
    let (resp_tx, resp_rx) = oneshot::channel::<Result<Response, StatusCode>>();
    let msg = Message {
        data: uri.to_string(),
        resp: resp_tx,
    };

    let send_res = sender.send(msg);
    println!("{:?}", send_res);

    // Await the response
    if let Ok(result) = resp_rx.await {
        result
    } else {
        Err(StatusCode::INTERNAL_SERVER_ERROR)
    }
}

fn create_poke(kernel: &mut Kernel, args: &[Noun]) -> Noun {
    // error if args is less than 2 elements
    if args.len() < 2 {
        panic!("args must have at least 2 elements");
    }
    T(kernel.serf.stack(), args)
}
