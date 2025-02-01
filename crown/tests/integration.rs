use std::fs;
use std::path::Path;
use std::time::Duration;
use tempfile::TempDir;
use tracing::{debug, info};

use crown::kernel::checkpoint::JamPaths;
use crown::kernel::form::Kernel;
use crown::nockapp::wire::{SystemWire, Wire};
use crown::noun::slab::NounSlab;
use crown::observability::*;
use crown::NockApp;

use sword::noun::{Noun, Slots, D};
use sword_macros::tas;

fn setup_nockapp(jam: &str) -> (TempDir, NockApp) {
    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let snap_dir = temp_dir.path().to_path_buf();
    let jam_paths = JamPaths::new(&snap_dir);
    debug!("jam_paths: {:?}", jam_paths);
    let jam_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-jams")
        .join(jam);
    let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
    let kernel = Kernel::load(snap_dir, jam_paths, &jam_bytes, false);
    debug!("Finished kernel load");
    (temp_dir, NockApp::new(kernel, Duration::from_secs(1)))
}

#[tracing::instrument(skip(nockapp, loopin_yo))]
fn run_once(nockapp: &mut NockApp, loopin_yo: metrics::Counter, i: u64) {
    info!("before poke construction");
    let poke = D(tas!(b"inc")).into();
    info!("Poke constructed");
    let wire = SystemWire.to_noun_slab();
    info!("Wire constructed");
    let _ = nockapp.poke_sync(wire, poke).unwrap();
    info!("after poke_sync");
    let peek: NounSlab = [D(tas!(b"state")), D(0)].into();
    // res should be [~ ~ %0 val]
    let res = nockapp.peek_sync(peek);
    info!("after peek_sync");
    let res = res.unwrap();
    let root = unsafe { res.root() };
    let val: Noun = root.slot(15).unwrap();
    unsafe {
        assert!(val.raw_equals(D(i)), "Expected {} but got {}", i, val);
    }
    info!("after raw_equals");
    loopin_yo.increment(1);
    info!("after increment");
}

// This is just an experimental test to exercise the tracing and metrics
// To run this test:
// OTEL_SERVICE_NAME="nockapp_test" RUST_LOG="debug" OTEL_EXPORTER_JAEGER_ENDPOINT=http://localhost:4317 cargo nextest run test_looped_sync_peek_and_poke --nocapture --run-ignored all
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore]
async fn test_looped_sync_peek_and_poke() {
    let subscriber = init_tracing().unwrap();
    init_prometheus();
    eprintln!("Use docker compose up to start prometheus and jaeger");
    eprintln!("Prometheus dashboard: http://localhost:9090/");
    eprintln!("Jaeger dashboard: http://localhost:16686/");
    tracing::subscriber::with_default(subscriber, || {
        tracing::info!("Starting run_forever");
        let loopin_yo = metrics::counter!("loopin_yo");
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        for i in 1.. {
            info!("before run_once");
            run_once(&mut nockapp, loopin_yo.clone(), i);
            info!("after run_once");
        }
    });
}

#[tokio::test]
#[cfg_attr(miri, ignore)]
async fn test_sync_peek_and_poke() {
    let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
    for i in 1..4 {
        let poke = D(tas!(b"inc")).into();
        let wire = SystemWire.to_noun_slab();
        let _ = nockapp.poke_sync(wire, poke).unwrap();
        let peek: NounSlab = [D(tas!(b"state")), D(0)].into();
        // res should be [~ ~ %0 val]
        let res = nockapp.peek_sync(peek);
        let res = res.unwrap();
        let root = unsafe { res.root() };
        let val: Noun = root.slot(15).unwrap();
        unsafe {
            assert!(val.raw_equals(D(i)));
        }
    }
}
