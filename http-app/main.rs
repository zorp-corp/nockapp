
use crown::nockapp::http_driver::http;

use crown::kernel::boot;
use crown::nockapp::NockApp;

use clap::{command, ColorChoice, Parser};
static KERNEL_JAM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/http.jam"));

use crown::kernel::boot::Cli as BootCli;
use tokio::select;
use tracing::debug;

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}


struct Mock {
    app: NockApp
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    debug!("KERNEL_JAM len: {:?}", KERNEL_JAM.to_vec().len());
    let kernel = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;
    let mut mock = Mock{app: NockApp::new(kernel)};
    mock.app.add_io_driver(http()).await;

    loop {
        select! {
            work_res = mock.app.work() => {
                if let Err(e) = work_res {
                    debug!("work error: {:?}", e);
                    break
                }
            }
            _ = tokio::signal::ctrl_c() => {
                debug!("ctrl_c registered");
                break;
            }
        }
    }

    Ok(())
}