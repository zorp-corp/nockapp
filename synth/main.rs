use assert_no_alloc::*;

use crate::mpsc::Receiver;
use std::sync::mpsc;
use std::sync::mpsc::SyncSender;
use tokio::sync::oneshot;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{FromSample, SizedSample};

use crown::kernel::boot;

use clap::{command, ColorChoice, Parser};

use fundsp::hacker::*;
use crown::kernel::boot::Cli as BootCli;


static KERNEL_JAM: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/synth.jam"));


#[derive(Parser, Debug)]
#[command(about = "Tests various poke types for the kernel", author = "zorp", version, color = ColorChoice::Auto)]
struct TestCli {
    #[command(flatten)]
    boot: BootCli,
}

#[derive(Debug)]
struct Message {
    idea: String
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::sync_channel::<Message>(0);

    tokio::spawn(async move { synthesizer(tx).await });

    let _ = manage_kernel(rx).await;

    Ok(())
}

async fn manage_kernel(rx: Receiver<Message>) -> Result<(), Box<dyn std::error::Error>> {
    let cli = TestCli::parse();
    let mut _kernel = boot::setup(KERNEL_JAM, Some(cli.boot), &[])?;


    loop {
        // Start receiving messages
        if let Ok(msg) = rx.recv() {
            println!("message: {:?}", msg);
        }
    }
}

async fn synthesizer(_tx: SyncSender<Message>) {
    let host = cpal::default_host();

    let device = host
        .default_output_device()
        .expect("failed to find a default output device");
    let config = device.default_output_config().unwrap();

    match config.sample_format() {
        cpal::SampleFormat::F32 => run::<f32>(&device, &config.into()).unwrap(),
        cpal::SampleFormat::I16 => run::<i16>(&device, &config.into()).unwrap(),
        cpal::SampleFormat::U16 => run::<u16>(&device, &config.into()).unwrap(),
        _ => panic!("Unsupported format"),
    }
}

fn run<T>(device: &cpal::Device, config: &cpal::StreamConfig) -> Result<(), anyhow::Error> where T: SizedSample + FromSample<f32> {
    let sample_rate = config.sample_rate.0 as f64;
    let channels = config.channels as usize;

    let c = 0.2 * (organ_hz(midi_hz(57.0)) + organ_hz(midi_hz(61.0)) + organ_hz(midi_hz(64.0)));
    let c = c >> pan(0.0);
    let c = c >> (chorus(0, 0.0, 0.01, 0.2) | chorus(1, 0.0, 0.01, 0.2));

    let mut c = c
        >> (declick() | declick())
        >> (dcblock() | dcblock())
        >> limiter_stereo(1.0, 5.0);

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

    std::thread::sleep(std::time::Duration::from_millis(10000));

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
