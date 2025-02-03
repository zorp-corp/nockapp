use clap::{command, ColorChoice, Parser};
use crown::kernel::boot;
use crown::kernel::boot::Cli as BootCli;
use crown::noun::slab::NounSlab;
use sword::noun::D;
use sword_macros::tas;
use tracing::debug;

static KERNEL_JAM: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/bootstrap/test-ker.jam"
));
#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
    #[arg(long, help = "Exit after poke")]
    exit: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    debug!("KERNEL_JAM len: {:?}", KERNEL_JAM.to_vec().len());
    boot::init_default_tracing(&cli.boot.clone());
    let mut test_app = boot::setup(KERNEL_JAM, Some(cli.boot.clone()), &[], "test")?;
    let poke = if cli.exit {
        D(tas!(b"inc-exit"))
    } else {
        D(tas!(b"inc"))
    };
    let mut slab = NounSlab::new();
    slab.set_root(poke);
    test_app
        .add_io_driver(crown::one_punch_driver(
            slab,
            crown::nockapp::driver::Operation::Poke,
        ))
        .await;

    test_app.add_io_driver(crown::exit_driver()).await;

    test_app.run().await.expect("Failed to run app");

    Ok(())
}
