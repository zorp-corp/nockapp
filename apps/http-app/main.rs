use clap::{command, ColorChoice, Parser};
use crown::kernel::boot;
use crown::kernel::boot::Cli as BootCli;
use tracing::debug;

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/http.jam"));
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
    boot::init_default_tracing(&cli.boot.clone());
    let mut http_app = boot::setup(KERNEL_JAM, Some(cli.boot.clone()), &[], "choo")?;
    http_app.add_io_driver(crown::http_driver()).await;

    http_app.run().await.expect("Failed to run app");

    Ok(())
}
