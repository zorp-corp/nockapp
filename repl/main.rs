use std::io::Write;
use tracing::debug;

use crown::kernel::form::Kernel;
use bytes::Bytes;
use crown::kernel::boot;
use crown::{AtomExt, NounExt};
use sword::noun::{Atom, D, T};
use sword_macros::tas;
use tokio::fs::File;
use tokio::io::{AsyncWriteExt};


static KERNEL_JAM: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/choo.jam"));

static HOON_TXT: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/bootstrap/hoon-139.hoon"));

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut kernel = boot::setup(KERNEL_JAM, None, &[])?;

    let hoon_cord =
        Atom::from_bytes(kernel.serf.stack(), &Bytes::from(HOON_TXT)).as_noun();

    let bootstrap_poke = T(kernel.serf.stack(), &[D(tas!(b"boot")), hoon_cord]);
    let _ = kernel.poke(bootstrap_poke)?;

    loop {
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        match respond(line, &mut kernel).await {
            Ok(quit) => {
                if quit {
                    break;
                }
            }
            Err(err) => {
                write!(std::io::stdout(), "{err}").map_err(|e| e.to_string())?;
                std::io::stdout().flush().map_err(|e| e.to_string())?;
            }
        }
    }

    Ok(())
}

fn readline() -> Result<String, String> {
    write!(std::io::stdout(), "$ ").map_err(|e| e.to_string())?;
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| e.to_string())?;
    Ok(buffer)
}

async fn respond(line: &str, kernel: &mut Kernel) -> Result<bool, Box<dyn std::error::Error>> {

    let input_noun = Atom::from_bytes(kernel.serf.stack(), &Bytes::from(line.as_bytes().to_vec())).as_noun();

    let poke = T(
        kernel.serf.stack(),
        &[D(tas!(b"input")), input_noun],
    );

    let poke_result = kernel.poke(poke)?;

    if let Ok(fec_it) = poke_result.as_cell() {
        if let Ok(fec) = fec_it.head().as_cell() {
            if fec.head().eq_bytes(b"jam") {
                let mut fil = File::create("out.jam").await?;
                fil.write_all(fec.tail().jam_self(kernel.serf.stack()).as_ref())
                    .await?;

                write!(std::io::stdout(), "jammed to out.jam").map_err(|e| e.to_string())?;
                std::io::stdout().flush().map_err(|e| e.to_string())?;
                Ok(false)
            } else {
                debug!("Unknown effect {:?}", fec_it.head());
                Ok(true)
            }
        } else {
            Ok(true)
        }
    } else {
        // all done
        Ok(false)
    }
}

