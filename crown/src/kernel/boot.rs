use crate::default_data_dir;
use crate::kernel::checkpoint::JamPaths;
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
        default_value = "info",
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

    let pma_dir = default_data_dir("pma");
    let jams_dir = default_data_dir("jams");
    let jam_paths = JamPaths::new(&jams_dir);

    if pma_dir.exists() {
        std::fs::remove_dir_all(&pma_dir)?;
        info!("Deleted existing pma directory: {:?}", pma_dir);
    }

    if cli.new {
        if jams_dir.exists() {
            std::fs::remove_dir_all(&jams_dir)?;
        }
        info!("Deleted existing checkpoint directory: {:?}", jams_dir);
    }

    info!("kernel: starting");
    debug!("kernel: pma directory: {:?}", pma_dir);
    debug!(
        "kernel: jam buffer paths: {:?}, {:?}",
        jam_paths.0, jam_paths.1
    );
    let kernel = Kernel::load_with_hot_state(pma_dir, jam_paths, jam, hot_state, cli.trace);

    Ok(kernel)
}
