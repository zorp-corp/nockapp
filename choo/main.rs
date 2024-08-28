use bytes::Bytes;
use crown::kernel::boot;
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

    #[arg(long, help = "Compile vase?", default_value = "false")]
    knob: bool,

    #[arg(help = "Path to file to compile")]
    pax: String,

    #[arg(help = "Path to subject")]
    sub: Option<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = ChooCli::parse();

    let mut kernel = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;

    let pax_noun = D(0);

    let contents = {
        let mut contents_vec: Vec<u8> = vec![];
        let mut file = File::open(cli.pax).await?;
        file.read_to_end(&mut contents_vec).await?;
        Atom::from_bytes(kernel.serf.stack(), &Bytes::from(contents_vec)).as_noun()
    };

    let sub_knob = {
        if let Some(sub_path) = cli.sub {
            let mut sub_contents_vec: Vec<u8> = vec![];
            let mut sub_file = File::open(sub_path).await?;
            sub_file.read_to_end(&mut sub_contents_vec).await?;
            Noun::cue_bytes_slice(kernel.serf.stack(), &sub_contents_vec[..])
        } else {
            T(kernel.serf.stack(), &[D(tas!(b"noun")), D(1), D(0)])
        }
    };

    let nob_loobean = if cli.knob { YES } else { NO };

    let poke = T(
        kernel.serf.stack(),
        &[D(tas!(b"compile")), sub_knob, pax_noun, contents, nob_loobean],
    );

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
