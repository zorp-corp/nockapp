use crate::kernel::checkpoint::JamPaths;
use crate::kernel::form::Kernel;
use crate::{default_data_dir, NockApp};
use clap::{arg, command, ColorChoice, Parser};
use sword::jets::hot::HotEntry;
use tracing::{debug, info};
use tracing_subscriber::prelude::*;
use tracing_subscriber::{fmt, EnvFilter};
use std::fs;

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
        default_value = "1000",
        help = "Set the save interval for checkpoints (in ms)"
    )]
    pub save_interval: u64,

    #[arg(
        long,
        env = "RUST_LOG",
        default_value = "info",
        help = "Set the log level"
    )]
    pub log_level: String,

    #[arg(long, help = "Control colored output", value_enum, default_value_t = ColorChoice::Auto)]
    pub color: ColorChoice,

    #[arg(
        long,
        help = "Path to a jam file containing existing kernel state"
    )]
    pub state_jam: Option<String>,
}

pub fn init_default_tracing(cli: &Cli) {
    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_ansi(cli.color == ColorChoice::Auto || cli.color == ColorChoice::Always),
        )
        .with(EnvFilter::new(&cli.log_level))
        .init();
}

pub fn setup(
    jam: &[u8],
    cli: Option<Cli>,
    hot_state: &[HotEntry],
    name: &str,
) -> Result<NockApp, Box<dyn std::error::Error>> {
    let cli = cli.unwrap_or_else(|| Cli::parse());

    let data_dir = default_data_dir(name);
    let pma_dir = data_dir.join("pma");
    let jams_dir = data_dir.join("jams");
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

    let kernel = if let Some(state_path) = cli.state_jam {
        let state_jam = fs::read(&state_path)?;
        debug!("kernel: loading state from jam file: {:?}", state_path);
        Kernel::load_with_kernel_state(pma_dir, jam_paths, jam, &state_jam, hot_state, cli.trace)?
    } else {
        Kernel::load_with_hot_state(pma_dir, jam_paths, jam, hot_state, cli.trace)
    };

    let save_interval = std::time::Duration::from_millis(cli.save_interval);

    Ok(NockApp::new(kernel, save_interval))
}
