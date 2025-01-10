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
use tracing::error;
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
    entry: String,

    #[arg(help = "Path to root of dependency directory", default_value = "hoon")]
    directory: String,

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
        let nockapp = initialize_nockapp(cli).await?;
        work_loop(nockapp).await;
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

async fn initialize_nockapp(cli: ChooCli) -> Result<crown::nockapp::NockApp, Error> {
    // Remove trailing slash from directory if present
    let directory = cli.directory.trim_end_matches('/').to_string();

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

    let mut entry = cli.entry.clone();

    //  Insert a leading slash if it is not present
    //  Needed to make the entry path an actual hoon $path type
    if !entry.starts_with('/') {
        entry.insert(0, '/');
    }

    // hoon does not support uppercase paths
    let entry_path = Atom::from_value(&mut slab, entry.to_lowercase()).unwrap().as_noun();

    let mut directory_noun = D(0);

    let walker = WalkDir::new(&directory)
        .follow_links(true)
        .into_iter();
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

            let path_cord = Atom::from_value(&mut slab, path_str.to_lowercase()).unwrap().as_noun();

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

async fn work_loop(mut nockapp: crown::nockapp::NockApp) {
    loop {
        let work_res = nockapp.work().await;
        if let Err(e) = work_res {
            error!("work error: {:?}", e);
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use tokio::fs;
    use tracing::info;

    #[ignore]
    #[tokio::test]
    async fn test_compile_test_app() -> Result<(), Box<dyn std::error::Error>> {
        let mut test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        test_dir.pop();
        test_dir.push("test-app");

        // Clean up any existing output file
        let _ = fs::remove_file("out.jam").await;

        let mut deps_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        deps_dir.pop();
        deps_dir.push("hoon-deps");
        let entry = format!("{}/bootstrap/kernel.hoon", test_dir.display());

        let result = async {
            let cli = ChooCli {
                boot: BootCli {
                    save_interval: 1000,
                    new: false,
                    trace: false,
                    log_level: "trace".to_string(),
                    color: ColorChoice::Auto,
                    state_jam: None,
                },
                entry: entry.clone(),
                directory: deps_dir.display().to_string(),
                arbitrary: false,
            };

            let nockapp = initialize_nockapp(cli).await?;
            info!("Test directory: {}", test_dir.display());
            info!("Dependencies directory: {}", deps_dir.display());
            info!("Entry file: {}", entry.clone());
            work_loop(nockapp).await;

            // TODO this doesn't work because choo exits when compilation is done.
            // Verify output file exists and is not empty
            let metadata = fs::metadata("out.jam").await?;
            info!("Output file size: {} bytes", metadata.len());
            assert!(metadata.len() > 0, "Output file is empty");
            Ok(())
        }.await;

        // Cleanup
        let _ = fs::remove_file("out.jam").await;

        result
    }
}
