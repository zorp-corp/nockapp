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
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo5.jam"));

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
    let mut nockapp = boot::setup(KERNEL_JAM, Some(cli.boot), &[], "choo")?;

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

    let entry_path = Atom::from_value(&mut slab, entry).unwrap().as_noun();

    let mut directory_noun = D(0);

    let walker = WalkDir::new(cli.directory.clone())
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
                .strip_prefix(&cli.directory)
                .unwrap();
            let path_cord = Atom::from_value(&mut slab, path_str).unwrap().as_noun();

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
