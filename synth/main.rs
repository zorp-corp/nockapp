use assert_no_alloc::*;

use std::time::Duration;

use std::io::Write;
use crate::mpsc::Receiver;
use std::sync::mpsc;
use std::sync::mpsc::SyncSender;
use tokio::sync::oneshot;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{FromSample, SizedSample};

use crown::Noun;
use crown::kernel::boot;
use crown::Bytes;
use crown::NounExt;
use crown::AtomExt;
use sword::noun::{Atom, D, T};

use crown::kernel::form::Kernel;
use clap::{command, ColorChoice, Parser};

use fundsp::hacker::*;
use crown::kernel::boot::Cli as BootCli;

use tracing::debug;
use sword_macros::tas;


static KERNEL_JAM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/synth.jam"));


#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

#[derive(Debug)]
struct Riff {
    oscillator: Oscillator,
    frequency: u64
}

#[derive(Debug, PartialEq)]
enum Oscillator {
    Organ,
    Saw,
    Sine,
    Triangle,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::sync_channel::<Riff>(0);

    tokio::spawn(async move { synthesizer(rx).await });

    let _ = manage_kernel(tx).await;

    Ok(())
}

async fn manage_kernel(sender: SyncSender<Riff>) -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut kernel = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;

    loop {
        // get keyboard input
        //
        let line = readline()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        match respond(line, &mut kernel).await {
            Ok(maybe_msg) => {
                let msg = maybe_msg.unwrap();
                let _ = sender.send(msg);
            }
            Err(err) => {
                write!(std::io::stdout(), "{err}").map_err(|e| e.to_string())?;
                std::io::stdout().flush().map_err(|e| e.to_string())?;
            }
        }
    }
}

fn readline() -> Result<String, String> {
    write!(std::io::stdout(), "♫ ").map_err(|e| e.to_string())?;
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| e.to_string())?;
    Ok(buffer)
}

async fn respond(line: &str, kernel: &mut Kernel) -> Result<Option<Riff>, Box<dyn std::error::Error>> {
    let input_noun = Atom::from_bytes(kernel.serf.stack(), &Bytes::from(line.as_bytes().to_vec())).as_noun();

    let poke = T(
        kernel.serf.stack(),
        &[D(tas!(b"input")), input_noun],
    );

    let poke_result = kernel.poke(poke)?;

    if let Ok(effect_list) = poke_result.as_cell() {
        if let Ok(effect) = effect_list.head().as_cell() {
            if effect.head().eq_bytes(b"riff") {
                std::io::stdout().flush().map_err(|e| e.to_string())?;
                let maybe_riff = parse_riff(effect.tail());
                if let Some(riff) = maybe_riff {
                    Ok(Some(riff))
                } else {
                    Ok(None)
                }
            } else {
                debug!("Unknown effect {:?}", effect_list.head());
                Ok(None)
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn parse_riff(n: Noun) -> Option<Riff> {
    if let Ok(note_atom) = n.as_atom() {
        let msg = Riff {
            oscillator: Oscillator::Organ,
            frequency: note_atom.direct()?.data()
        };

        return Some(msg);
    }

    if let Ok(riff_cell) = n.as_cell() {
        let oscillator = {
            use Oscillator::*;
            if riff_cell.head().eq_bytes(b"sine") {
                Sine
            } else if riff_cell.head().eq_bytes(b"saw") {
                Saw
            } else if riff_cell.head().eq_bytes(b"triangle") {
                Triangle
            } else if riff_cell.head().eq_bytes(b"organ") {
                Organ
            } else {
                panic!("no known oscillator!");
            }
        };
        let frequency = riff_cell.tail().as_atom().ok()?.direct()?.data();

        Some(Riff {
            oscillator,
            frequency
        })
    } else {
        None
    }
}

async fn synthesizer(rx: Receiver<Riff>) {
    let host = cpal::default_host();

    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();


    loop {
        // Start receiving messages

        let config = config.clone();
        if let Ok(msg) = rx.recv() {
            match config.sample_format() {
                cpal::SampleFormat::F32 => run::<f32>(&device, &config.into(), msg).unwrap(),
                cpal::SampleFormat::I16 => run::<i16>(&device, &config.into(), msg).unwrap(),
                cpal::SampleFormat::U16 => run::<u16>(&device, &config.into(), msg).unwrap(),
                _ => panic!("Unsupported format"),
            }
        }
    }
}

fn run<T>(device: &cpal::Device, config: &cpal::StreamConfig, msg: Riff) -> Result<(), anyhow::Error> where T: SizedSample + FromSample<f32> {
    let sample_rate = config.sample_rate.0 as f64;
    let channels = config.channels as usize;

    let f = midi_hz(msg.frequency as f32);
    let mut c: An<Unit<U0, U1>> = {
        use Oscillator::*;
        match msg.oscillator {
            Organ => unit::<U0, U1>(Box::new(organ_hz(f))),
            Saw => unit::<U0, U1>(Box::new(saw_hz(f))),
            Sine => unit::<U0, U1>(Box::new(sine_hz(f))),
            Triangle => unit::<U0, U1>(Box::new(triangle_hz(f)))
        }
    };
    c.set_sample_rate(sample_rate);
    c.allocate();



    let mut next_value = move || assert_no_alloc(|| c.get_stereo());

    let err_fn = |err| eprintln!("an error occurred on stream: {}", err);

    let stream = device.build_output_stream(
        config,
        move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
            write_data(data, channels, &mut next_value)
        },
        err_fn,
        None,
    )?;
    stream.play()?;
    std::thread::sleep(Duration::from_millis(1000));

    Ok(())
}

fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> (f32, f32)) where T: SizedSample + FromSample<f32>,
{
    for frame in output.chunks_mut(channels) {
        let sample = next_sample();
        let left = T::from_sample(sample.0);
        let right: T = T::from_sample(sample.1);

        for (channel, sample) in frame.iter_mut().enumerate() {
            if channel & 1 == 0 {
                *sample = left;
            } else {
                *sample = right;
            }
        }
    }
}
