use clap::{arg, command, ColorChoice, Parser};
use tokio::fs::{self, File};
use tokio::io::AsyncReadExt;
use tracing::info;
use walkdir::{DirEntry, WalkDir};

use crown::kernel::boot::{self, default_boot_cli, Cli as BootCli};
use crown::nockapp::driver::Operation;
use crown::noun::slab::NounSlab;
use crown::AtomExt;
use sword::noun::{Atom, D, T};
use sword_macros::tas;

pub type Error = Box<dyn std::error::Error>;

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo.jam"));

static HOON_TXT: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../hoon/hoon-138.hoon"
));

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
pub struct ChooCli {
    #[command(flatten)]
    pub boot: BootCli,

    #[arg(help = "Path to file to compile")]
    pub entry: std::path::PathBuf,

    #[arg(help = "Path to root of dependency directory", default_value = "hoon")]
    pub directory: std::path::PathBuf,

    #[arg(
        long,
        help = "Build raw, without file hash injection",
        default_value = "false"
    )]
    pub arbitrary: bool,
}

pub async fn initialize_nockapp(cli: ChooCli) -> Result<crown::nockapp::NockApp, Error> {
    initialize_(cli.entry, cli.directory, cli.arbitrary, cli.boot.clone()).await
}

pub async fn initialize_with_default_cli(
    entry: std::path::PathBuf,
    deps_dir: std::path::PathBuf,
    arbitrary: bool,
) -> Result<crown::nockapp::NockApp, Error> {
    let cli = default_boot_cli();
    initialize_(entry, deps_dir, arbitrary, cli).await
}

pub async fn initialize_(
    entry: std::path::PathBuf,
    deps_dir: std::path::PathBuf,
    arbitrary: bool,
    boot_cli: BootCli,
) -> Result<crown::nockapp::NockApp, Error> {
    let mut nockapp = boot::setup(KERNEL_JAM, Some(boot_cli.clone()), &[], "choo")?;
    boot::init_default_tracing(&boot_cli.clone());
    let mut slab = NounSlab::new();
    let hoon_cord = Atom::from_value(&mut slab, HOON_TXT).unwrap().as_noun();
    let bootstrap_poke = T(&mut slab, &[D(tas!(b"boot")), hoon_cord]);
    slab.set_root(bootstrap_poke);

    nockapp
        .add_io_driver(crown::one_punch_driver(slab, Operation::Poke))
        .await;

    let mut slab = NounSlab::new();
    let entry_contents = {
        let mut contents_vec: Vec<u8> = vec![];
        let mut file = File::open(&entry).await?;
        file.read_to_end(&mut contents_vec).await?;
        Atom::from_value(&mut slab, contents_vec).unwrap().as_noun()
    };

    let entry_string = canonicalize_and_string(&entry)?;
    let entry_path = Atom::from_value(&mut slab, entry_string.to_lowercase())
        .unwrap()
        .as_noun();

    let mut directory_noun = D(0);
    let directory = canonicalize_and_string(&deps_dir)?;

    let walker = WalkDir::new(&directory).follow_links(true).into_iter();
    for entry_result in walker.filter_entry(|e| is_valid_file_or_dir(e)) {
        let entry = entry_result?;
        let is_file = entry.metadata().unwrap().is_file();
        if is_file {
            let path_str = entry
                .path()
                .to_str()
                .unwrap()
                .strip_prefix(&directory)
                .unwrap();

            let path_cord = Atom::from_value(&mut slab, path_str.to_lowercase())
                .unwrap()
                .as_noun();

            let contents = {
                let mut contents_vec: Vec<u8> = vec![];
                let mut file = File::open(entry.path()).await?;
                file.read_to_end(&mut contents_vec).await?;
                Atom::from_value(&mut slab, contents_vec).unwrap().as_noun()
            };

            let entry_cell = T(&mut slab, &[path_cord, contents]);
            directory_noun = T(&mut slab, &[entry_cell, directory_noun]);
        }
    }
    let arbitrary_noun = if arbitrary { D(0) } else { D(1) };
    let poke = T(
        &mut slab,
        &[D(tas!(b"build")), entry_path, entry_contents, directory_noun, arbitrary_noun],
    );
    slab.set_root(poke);

    nockapp
        .add_io_driver(crown::one_punch_driver(slab, Operation::Poke))
        .await;
    nockapp.add_io_driver(crown::file_driver()).await;
    nockapp.add_io_driver(crown::exit_driver()).await;
    Ok(nockapp)
}

pub fn is_valid_file_or_dir(entry: &DirEntry) -> bool {
    let is_dir = entry.metadata().unwrap().is_dir();

    let is_hoon = entry
        .file_name()
        .to_str()
        .map(|s| s.ends_with(".hoon"))
        .unwrap_or(false);

    let is_jock = entry
        .file_name()
        .to_str()
        .map(|s| s.ends_with(".jock"))
        .unwrap_or(false);

    is_dir || is_hoon || is_jock
}

pub fn canonicalize_and_string(path: &std::path::Path) -> Result<String, Error> {
    let path = path.canonicalize()?;
    let path = path.to_str().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Path is not valid or file cannot be found",
        )
    })?;
    Ok(path.to_string())
}

/// Run the build and verify the output file, used to build files outside of cli.
pub async fn run_build(nockapp: &mut crown::nockapp::NockApp) -> Result<(), Error> {
    nockapp.run().await?;
    // TODO this doesn't work because choo exits when compilation is done.
    // Verify output file exists and is not empty
    let metadata = fs::metadata("out.jam").await?;
    info!("Output file size: {} bytes", metadata.len());
    assert!(metadata.len() > 0, "Output file is empty");
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_canonicalize_and_string() {
        let path = std::path::Path::new("Cargo.toml");
        let result = super::canonicalize_and_string(path);
        assert!(result.is_ok());
        // left: "/Users/callen/work/zorp/nockapp/apps/choo/Cargo.toml"
        // right: "Cargo.toml"
        let pwd = std::env::current_dir().unwrap();
        let cargo_toml = pwd.join("Cargo.toml").canonicalize().unwrap();
        assert_eq!(result.unwrap(), cargo_toml.to_str().unwrap());
    }
}
