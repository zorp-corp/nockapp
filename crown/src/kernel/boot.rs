use crate::default_data_dir;
use crate::kernel::form::Kernel;
use clap::{arg, command, ColorChoice, Parser};
use sword::jets::hot::HotEntry;
use tracing::{debug, info};
use tracing_subscriber::prelude::*;
use tracing_subscriber::{fmt, EnvFilter};

#[derive(Parser, Debug, Clone)]
#[command(about = "boot a nockapp", author, version, color = ColorChoice::Auto)]
pub struct Cli {
    #[arg(
        long,
        help = "Start with a new data directory, removing any existing data",
        default_value = "false"
    )]
    pub new: bool,

    #[arg(long, help = "Make an Sword trace", default_value = "false")]
    pub trace: bool,

    #[arg(
        long,
        env = "RUST_LOG",
        default_value = "debug",
        help = "Set the log level"
    )]
    log_level: String,

    #[arg(long, help = "Control colored output", value_enum, default_value_t = ColorChoice::Auto)]
    color: ColorChoice,
}

pub fn setup(
    jam: &[u8],
    cli: Option<Cli>,
    hot_state: &[HotEntry],
) -> Result<Kernel, Box<dyn std::error::Error>> {
    let cli = cli.unwrap_or_else(|| Cli::parse());

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_ansi(cli.color == ColorChoice::Auto || cli.color == ColorChoice::Always),
        )
        .with(EnvFilter::new(&cli.log_level))
        .init();

    let snap_dir = default_data_dir("crown");

    if cli.new {
        if snap_dir.exists() {
            std::fs::remove_dir_all(&snap_dir)?;
            info!("Deleted existing snap directory: {:?}", snap_dir);
        }
    }
    info!("kernel: starting");
    debug!("kernel: snapshot directory: {:?}", snap_dir);
    let kernel = Kernel::load_with_hot_state(snap_dir, jam, hot_state, cli.trace);

    Ok(kernel)
}

pub fn setup_form(jam: &[u8], cli: Option<Cli>) -> Result<Kernel, Box<dyn std::error::Error>> {
    let cli = cli.unwrap_or_else(|| Cli::parse());

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_ansi(cli.color == ColorChoice::Auto || cli.color == ColorChoice::Always),
        )
        .with(EnvFilter::new(&cli.log_level))
        .init();

    let snap_dir = default_data_dir("crown");

    if cli.new {
        if snap_dir.exists() {
            std::fs::remove_dir_all(&snap_dir)?;
            info!("Deleted existing snap directory: {:?}", snap_dir);
        }
    }
    info!("kernel: starting");
    debug!("kernel: snapshot directory: {:?}", snap_dir);
    let kernel = Kernel::load_form(snap_dir, jam, cli.trace);

    Ok(kernel)
}
