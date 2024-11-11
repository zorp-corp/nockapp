use clap::{command, ColorChoice, Parser};
use crown::kernel::boot;
use crown::kernel::boot::Cli as BootCli;
use tracing::debug;

static KERNEL_JAM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/http.jam"));
#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    debug!("KERNEL_JAM len: {:?}", KERNEL_JAM.to_vec().len());
    let mut http_app = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;
    http_app.add_io_driver(crown::http_driver()).await;

    loop {
        let work_res = http_app.work().await;
        if let Err(e) = work_res {
            debug!("work error: {:?}", e);
            break;
        }
    }

    Ok(())
}
