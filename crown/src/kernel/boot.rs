use crate::kernel::checkpoint::JamPaths;
use crate::kernel::form::KernelArgs;
use crate::{default_data_dir, NockApp};
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
        default_value = "1000",
        help = "Set the save interval for checkpoints (in ms)"
    )]
    save_interval: u64,

    #[arg(
        long,
        env = "RUST_LOG",
        default_value = "info",
        help = "Set the log level"
    )]
    pub log_level: String,

    #[arg(long, help = "Control colored output", value_enum, default_value_t = ColorChoice::Auto)]
    pub color: ColorChoice,
}

pub async fn setup(
    jam: &'static [u8],
    cli: Option<Cli>,
    hot_state: &'static [HotEntry],
    name: &str,
) -> Result<NockApp, Box<dyn std::error::Error>> {
    let cli = cli.unwrap_or_else(|| Cli::parse());

    if !cfg!(feature = "skip-subscriber") {
        tracing_subscriber::registry()
            .with(
                fmt::layer()
                    .with_ansi(cli.color == ColorChoice::Auto || cli.color == ColorChoice::Always),
            )
            .with(EnvFilter::new(&cli.log_level))
            .init();
    }

    let data_dir = default_data_dir(name);
    let jams_dir = data_dir.join("jams");
    let jam_paths = JamPaths::new(&jams_dir);

    let args = KernelArgs {
        jam_paths: jam_paths.clone(),
        kernel: jam,
        hot_state: Some(hot_state),
        trace: cli.trace,
    };

    if cli.new {
        if jams_dir.exists() {
            std::fs::remove_dir_all(&jams_dir)?;
        }
        info!("Deleted existing checkpoint directory: {:?}", jams_dir);
    }

    info!("kernel: starting");
    debug!(
        "kernel: jam buffer paths: {:?}, {:?}",
        jam_paths.0, jam_paths.1
    );

    let save_interval = std::time::Duration::from_millis(cli.save_interval);

    Ok(NockApp::new(args, save_interval).await)
}
