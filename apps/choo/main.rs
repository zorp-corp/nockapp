use crown::kernel::boot;
use crown::nockapp::driver::Operation;
use crown::noun::slab::NounSlab;
use crown::AtomExt;
use futures::FutureExt;
use sword::mem::{AllocationError, NewStackError};
use sword::noun::{Atom, D, T};
use sword_macros::tas;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use walkdir::{DirEntry, WalkDir};

use clap::{arg, command, ColorChoice, Parser};
use crown::kernel::boot::Cli as BootCli;

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo.jam"));

static HOON_TXT: &[u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../hoon/hoon-138.hoon"
));

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct ChooCli {
    #[command(flatten)]
    boot: BootCli,

    #[arg(help = "Path to file to compile")]
    entry: std::path::PathBuf,

    #[arg(help = "Path to root of dependency directory", default_value = "hoon")]
    directory: std::path::PathBuf,

    #[arg(
        long,
        help = "Build raw, without file hash injection",
        default_value = "false"
    )]
    arbitrary: bool,
}

fn is_valid_file_or_dir(entry: &DirEntry) -> bool {
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

type Error = Box<dyn std::error::Error>;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let cli = ChooCli::parse();
    let result = std::panic::AssertUnwindSafe(async {
        let mut nockapp = initialize_nockapp(cli).await?;
        nockapp.run().await?;
        Ok::<(), Error>(())
    })
    .catch_unwind()
    .await;
    if result.is_err() {
        println!("Caught panic!");
        // now we downcast the error
        // and print it out
        let e = result.unwrap_err();
        if let Some(e) = e.downcast_ref::<AllocationError>() {
            println!("Allocation error occurred: {}", e);
        } else if let Some(e) = e.downcast_ref::<NewStackError>() {
            println!("NockStack creation error occurred: {}", e);
        } else {
            println!("Unknown panic: {e:?}");
        }
    } else {
        println!("no panic!");
    }

    Ok(())
}

fn canonicalize_and_string(path: &std::path::Path) -> Result<String, Error> {
    let path = path.canonicalize()?;
    let path = path.to_str().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Path is not valid or file cannot be found",
        )
    })?;
    Ok(path.to_string())
}

async fn initialize_nockapp(cli: ChooCli) -> Result<crown::nockapp::NockApp, Error> {
    let mut nockapp = boot::setup(KERNEL_JAM, Some(cli.boot.clone()), &[], "choo")?;
    boot::init_default_tracing(&cli.boot.clone());
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
        let mut file = File::open(&cli.entry).await?;
        file.read_to_end(&mut contents_vec).await?;
        Atom::from_value(&mut slab, contents_vec).unwrap().as_noun()
    };

    let entry_string = canonicalize_and_string(&cli.entry)?;
    let entry_path = Atom::from_value(&mut slab, entry_string.to_lowercase())
        .unwrap()
        .as_noun();

    let mut directory_noun = D(0);
    let directory = canonicalize_and_string(&cli.directory)?;

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
    let arbitrary_noun = if cli.arbitrary { D(0) } else { D(1) };
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

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::fs;
    use tracing::{debug, info};

    async fn test_nockapp(
        entry: std::path::PathBuf,
        deps_dir: std::path::PathBuf,
    ) -> Result<crown::nockapp::NockApp, Error> {
        let cli = ChooCli {
            boot: BootCli {
                save_interval: 1000,
                new: false,
                trace: false,
                log_level: "trace".to_string(),
                color: ColorChoice::Auto,
                state_jam: None,
            },
            entry,
            directory: deps_dir,
            arbitrary: false,
        };
        initialize_nockapp(cli).await
    }

    async fn test_build(nockapp: &mut crown::nockapp::NockApp) -> Result<(), Error> {
        nockapp.run().await?;
        // TODO this doesn't work because choo exits when compilation is done.
        // Verify output file exists and is not empty
        let metadata = fs::metadata("out.jam").await?;
        info!("Output file size: {} bytes", metadata.len());
        assert!(metadata.len() > 0, "Output file is empty");
        Ok(())
    }

    // TODO: Move this to an integration test.
    #[tokio::test]
    #[cfg_attr(miri, ignore)]
    async fn test_compile_test_app() -> Result<(), Box<dyn std::error::Error>> {
        // let mut test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // test_dir.pop();
        // test_dir.push("test-app");

        // use std::path to get pwd() and then canonicalize
        let pwd = std::env::current_dir().unwrap();
        let mut test_dir = pwd.clone();
        test_dir.pop();
        test_dir.push("test-app");

        let entry = test_dir.join("bootstrap/kernel.hoon");

        // TODO: Add -o flag to specify output file and then use the tmp-dir
        // TODO: instead of mutating the non-tmp filesystem in this test
        // Clean up any existing output file
        let _ = fs::remove_file("out.jam").await;

        let mut deps_dir = pwd.clone();
        deps_dir.pop();
        deps_dir.push("hoon-deps");
        info!("Test directory: {:?}", test_dir);
        info!("Dependencies directory: {:?}", deps_dir);
        info!("Entry file: {:?}", entry);

        let mut nockapp = test_nockapp(entry, deps_dir).await?;
        let result = test_build(&mut nockapp).await;
        assert!(result.is_ok());
        // Cleanup
        let _ = fs::remove_file("out.jam").await;
        debug!("Removed file");

        // Second run to test consecutive execution
        // FIXME: This currently panics because of the one-shot.
        // let result = test_build(&mut nockapp).await;
        // // Cleanup
        // let _ = fs::remove_file("out.jam").await;
        result
    }

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
