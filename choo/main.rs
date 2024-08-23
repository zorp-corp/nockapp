use bytes::Bytes;
use crown::kernel::boot;
use crown::kernel::form::Kernel;
use crown::{AtomExt, Noun, NounExt};
use sword::noun::{Atom, D, NO, T, YES};
use sword_macros::tas;
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use clap::{arg, command, ColorChoice, Parser};
use crown::kernel::boot::Cli as BootCli;

static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo.jam"));

#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct ChooCli {
    #[command(flatten)]
    boot: BootCli,

    #[arg(long, help = "Hoon expression to run")]
    hoon: Option<String>,

    #[arg(long, help = "Path to Hoon file to compile")]
    path: Option<String>,

    #[arg(long, help = "Optional flag to output raw nock", default_value = "false")]
    nock: bool,

    #[arg(short, long, help = "Execute a Hoon")]
    exec: bool,

    #[arg(long, help = "Print output", default_value = "false")]
    print: bool,

    #[arg(short, long, help = "Optional path to subject jam file to compile against")]
    subj: Option<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = ChooCli::parse();

    let mut kernel = boot::setup_form(KERNEL_JAM, Some(cli.boot))?;

    let gen_noun = {

        if let Some(hoon_option) = cli.hoon {
            Atom::from_bytes(kernel.serf.stack(), &Bytes::from(hoon_option)).as_noun()
        } else if let Some(path_option) = cli.path {
            file_to_atom(&mut kernel, path_option).await.unwrap().as_noun()
        } else {
            panic!("invalid option");
        }
    };

    let subj_knob = {
        if let Some(sub_path) = cli.subj {
            jamfile_to_noun(&mut kernel, sub_path).await.unwrap()
        } else {
            T(kernel.serf.stack(), &[D(tas!(b"noun")), D(1), D(0)])
        }
    };

    let nok_loobean = if cli.nock { YES } else { NO };
    let print_loobean = if cli.print { YES } else { NO };

    let poke = {
        if cli.exec {
            T(
                kernel.serf.stack(),
                &[D(tas!(b"execute")), subj_knob, nok_loobean, print_loobean, gen_noun],
            )
        } else {
            T(
                kernel.serf.stack(),
                &[D(tas!(b"compile")), subj_knob, nok_loobean, print_loobean, gen_noun],
            )
        }
    };

    let mut poke_result = kernel.poke(poke)?;

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

async fn jamfile_to_noun(kernel: &mut Kernel, path: String) -> Result<Noun, std::io::Error> {
    let mut sub_contents_vec: Vec<u8> = vec![];
    let mut sub_file = File::open(path).await?;
    sub_file.read_to_end(&mut sub_contents_vec).await?;
    Ok(Noun::cue_bytes_slice(kernel.serf.stack(), &sub_contents_vec[..]))
}

async fn file_to_atom(kernel: &mut Kernel, path: String) -> Result<Atom, std::io::Error> {
    let mut contents_vec: Vec<u8> = vec![];
    let mut file = File::open(path).await?;
    file.read_to_end(&mut contents_vec).await?;
    Ok(Atom::from_bytes(kernel.serf.stack(), &Bytes::from(contents_vec)))
}
