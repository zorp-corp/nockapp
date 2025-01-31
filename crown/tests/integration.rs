use crown::kernel::checkpoint::JamPaths;
use crown::kernel::form::Kernel;
use crown::nockapp::wire::{SystemWire, Wire};
use crown::noun::slab::NounSlab;
use crown::NockApp;
use sword::noun::Slots;
use tracing::debug;

use std::fs;
use std::path::Path;
use std::time::Duration;
use sword::noun::{Noun, D};
use sword_macros::tas;
use tempfile::TempDir;

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
