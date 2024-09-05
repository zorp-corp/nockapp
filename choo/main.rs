use crown::kernel::boot;
use crown::{AtomExt, NounExt};
use sword::noun::{Atom, D, T};
use sword_macros::tas;
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use walkdir::{DirEntry, WalkDir};

use clap::{arg, command, ColorChoice, Parser};
use crown::kernel::boot::Cli as BootCli;

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo.jam"));

static HOON_TXT: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/hoon-139.hoon"));

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct ChooCli {
    #[command(flatten)]
    boot: BootCli,

    #[arg(help = "Path to file to compile")]
    entry: String,

    #[arg(help = "Path to root of dependency directory", default_value = "hoon")]
    directory: String,
}

fn is_hoon_or_dir(entry: &DirEntry) -> bool {
    let is_dir = entry.metadata().unwrap().is_dir();

    let is_hoon =
        entry.file_name().to_str()
            .map(|s| s.ends_with(".hoon"))
            .unwrap_or(false);

    is_dir || is_hoon
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = ChooCli::parse();

    let mut kernel = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;

    let hoon_cord =
        Atom::from_value(kernel.serf.stack(), HOON_TXT).unwrap().as_noun();

    let bootstrap_poke = T(kernel.serf.stack(), &[D(tas!(b"boot")), hoon_cord]);
    let _ = kernel.poke(bootstrap_poke)?;

    let entry_string = cli.entry.strip_prefix("hoon").unwrap();
    let entry_noun = Atom::from_value(kernel.serf.stack(), entry_string).unwrap().as_noun();

    let mut directory_noun = D(0);

    let walker = WalkDir::new(cli.directory).follow_links(true).into_iter();
    for entry_result in walker
        .filter_entry(|e| is_hoon_or_dir(e)) {
        let entry = entry_result?;
        let is_file = entry.metadata().unwrap().is_file();
        if is_file {
            let path_str = entry.path().to_str().unwrap().strip_prefix("hoon").unwrap();
            let path_cord = Atom::from_value(kernel.serf.stack(), path_str).unwrap().as_noun();

            let contents = {
                let mut contents_vec: Vec<u8> = vec![];
                let mut file = File::open(entry.path()).await?;
                file.read_to_end(&mut contents_vec).await?;
                Atom::from_value(kernel.serf.stack(), contents_vec).unwrap().as_noun()
            };

            let entry_cell = T(kernel.serf.stack(), &[path_cord, contents]);
            directory_noun = T(kernel.serf.stack(), &[entry_cell, directory_noun]);
            println!("{}", path_str);
        }
    }
    let poke = T(
        kernel.serf.stack(),
        &[D(tas!(b"build")), entry_noun, directory_noun],
    );

    let mut poke_result = kernel.poke(poke)?;
    //println!("result: {}", poke_result);

    loop {
        if let Ok(fec_it) = poke_result.as_cell() {
            poke_result = fec_it.tail();
            if let Ok(fec) = fec_it.head().as_cell() {
                if fec.head().eq_bytes(b"jam") {
                    let mut fil = File::create("out.jam").await?;
                    fil.write_all(fec.tail().jam_self(kernel.serf.stack()).as_ref())
                        .await?;
                } else {
                    //debug!("Unknown effect {:?}", fec_it.head());
                }
            }
        } else {
            break;
        }
    }
    Ok(())
}
