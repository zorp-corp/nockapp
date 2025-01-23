#[cfg(test)]
mod tests {
    use crate::kernel::checkpoint::JamPaths;
    use crate::kernel::form::Kernel;
    use crate::nockapp::wire::{SystemWire, Wire};
    use crate::noun::slab::{slab_equality, NounSlab};
    use crate::{kernel, NockApp, NounExt};
    use bytes::Bytes;
    use sword::noun::Slots;
    use tracing::info;

    use std::fs;
    use std::path::Path;
    use std::time::Duration;
    use sword::jets::cold::Nounable;
    use sword::jets::util::slot;
    use sword::noun::{Noun, D, T};
    use sword::serialization::{cue, jam};
    use sword::unifying_equality::unifying_equality;
    use sword_macros::tas;
    use tempfile::TempDir;
    use tracing_test::traced_test;

    fn setup_nockapp(jam: &str) -> (TempDir, NockApp) {
        let temp_dir = TempDir::new().expect("Failed to create temp directory");
        let snap_dir = temp_dir.path().to_path_buf();
        let jam_paths = JamPaths::new(&snap_dir);
        eprintln!("jam_paths: {:?}", jam_paths);
        let jam_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test-jams")
            .join(jam);
        let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
        let kernel = Kernel::load(snap_dir, jam_paths, &jam_bytes, false);
        (temp_dir, NockApp::new(kernel, Duration::from_secs(1)))
    }

    async fn save_nockapp(nockapp: &mut NockApp) {
        nockapp.tasks.close();
        let permit = nockapp.save_sem.clone().acquire_owned().await;
        let _ = nockapp.save(permit).await;
        let _ = nockapp.tasks.wait().await;
        nockapp.tasks.reopen();
    }

    // Panics if checkpoint failed to load, only permissible because this is expressly for testing
    async fn spawn_save_t(nockapp: &mut NockApp, sleep_t: std::time::Duration) {
        let sleepy_time = tokio::time::sleep(sleep_t);
        let permit = nockapp.save_sem.clone().acquire_owned().await;
        let _join_handle = nockapp
            .save_f(permit, sleepy_time)
            .await
            .expect("Failed to spawn nockapp save task");
        // join_handle.await.expect("Failed to save nockapp").expect("Failed to save nockapp 2");
    }

    // Test nockapp save
    #[test]
    #[traced_test]
    #[cfg_attr(miri, ignore)]
    fn test_nockapp_save_race_condition() {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()
            .unwrap();
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        // first run
        runtime.block_on(spawn_save_t(&mut nockapp, Duration::from_millis(1000)));
        // second run
        nockapp.kernel.serf.event_num = 1;
        runtime.block_on(spawn_save_t(&mut nockapp, Duration::from_millis(5000)));
        // Simulate what the event handlers would be doing and wait for the task tracker to be done
        nockapp.tasks.close();
        runtime.block_on(nockapp.tasks.wait());
        nockapp.tasks.reopen();
        // Shutdown the runtime immediately
        runtime.shutdown_timeout(std::time::Duration::from_secs(0));
        let checkpoint = nockapp
            .kernel
            .jam_paths
            .load_checkpoint(nockapp.kernel.serf.stack())
            .expect("Failed to get checkpoint");
        info!("checkpoint: {:?}", checkpoint);
        assert_eq!(checkpoint.event_num, 1);
        assert_ne!(
            nockapp.kernel.jam_paths.0, nockapp.kernel.jam_paths.1,
            "After a new checkpoint the jam_paths should be different"
        );
    }

    // Test nockapp save
    #[tokio::test]
    #[traced_test]
    #[cfg_attr(miri, ignore)]
    async fn test_nockapp_save() {
        // console_subscriber::init();
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        let mut arvo = nockapp
            .kernel
            .serf
            .arvo
            .slot(crate::kernel::form::STATE_AXIS)
            .expect("Could not slot state from kernel");
        let jam_paths = nockapp.kernel.jam_paths.clone();
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        // Save
        save_nockapp(&mut nockapp).await;
        // Permit should be dropped
        assert_eq!(nockapp.save_sem.available_permits(), 1);
        // A valid checkpoint should exist in one of the jam files
        let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
        assert!(checkpoint.is_ok());
        let mut checkpoint = checkpoint.unwrap();

        // Checkpoint event number should be 0
        assert_eq!(checkpoint.event_num, 0);

        // Checkpoint kernel should be equal to the saved kernel
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut checkpoint.ker_state,
                &mut arvo
            ));
        }
        info!("8");
        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint.cold.into_noun(nockapp.kernel.serf.stack());
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack());
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            ));
        };
    }

    // Test nockapp poke
    #[tokio::test]
    #[traced_test]
    #[cfg_attr(miri, ignore)]
    async fn test_nockapp_poke_save() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let mut state_before_poke = nockapp
            .kernel
            .serf
            .arvo
            .slot(kernel::form::STATE_AXIS)
            .expect("Could not slot state from kernel");

        let poke = D(tas!(b"inc"));

        let wire = SystemWire.to_noun_slab();
        let wire_root = unsafe { wire.root() };
        let _ = nockapp.kernel.poke(wire_root, poke).unwrap();

        // Save
        save_nockapp(&mut nockapp).await;

        // A valid checkpoint should exist in one of the jam files
        let jam_paths = &nockapp.kernel.jam_paths;
        let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
        assert!(checkpoint.is_ok());
        let mut checkpoint = checkpoint.unwrap();

        // Checkpoint event number should be 1
        assert!(checkpoint.event_num == 1);
        let mut state_after_poke = nockapp
            .kernel
            .serf
            .arvo
            .slot(kernel::form::STATE_AXIS)
            .expect("Could not slot state from kernel");
        unsafe {
            let stack = nockapp.kernel.serf.stack();
            // Checkpoint kernel should be equal to the saved kernel
            assert!(unifying_equality(
                stack, &mut checkpoint.ker_state, &mut state_after_poke
            ));
            // Checkpoint kernel should be different from the kernel before the poke
            assert!(!unifying_equality(
                stack, &mut checkpoint.ker_state, &mut state_before_poke
            ));
        }
        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint.cold.into_noun(nockapp.kernel.serf.stack());
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack());
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            ));
        };
    }

    #[tokio::test]
    #[cfg_attr(miri, ignore)]
    async fn test_nockapp_save_multiple() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let jam_paths = nockapp.kernel.jam_paths.clone();

        for i in 1..4 {
            // Poke to increment the state
            let poke = D(tas!(b"inc"));
            let wire = SystemWire.to_noun_slab();
            let wire_root = unsafe { wire.root() };
            let _ = nockapp.kernel.poke(wire_root, poke).unwrap();

            // Save
            save_nockapp(&mut nockapp).await;

            // Permit should be dropped
            assert_eq!(nockapp.save_sem.available_permits(), 1);

            // A valid checkpoint should exist in one of the jam files
            let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
            assert!(checkpoint.is_ok());
            let checkpoint = checkpoint.unwrap();

            // Checkpoint event number should be i
            assert!(checkpoint.event_num == i);

            // Checkpointed state should have been incremented
            let peek = T(nockapp.kernel.serf.stack(), &[D(tas!(b"state")), D(0)]);

            // res should be [~ ~ val]
            let res = nockapp.kernel.peek(peek).unwrap();
            let val = slot(res, 7).unwrap();
            unsafe {
                assert!(val.raw_equals(D(i)));
            }
        }
    }

    // Tests for fallback to previous checkpoint if checkpoint is corrupt
    #[tokio::test]
    #[traced_test]
    #[cfg_attr(miri, ignore)]
    async fn test_nockapp_corrupt_check() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let jam_paths = nockapp.kernel.jam_paths.clone();

        // Save a valid checkpoint
        save_nockapp(&mut nockapp).await;

        // Assert the checkpoint exists
        assert!(jam_paths.0.exists());

        // Permit should be dropped
        assert_eq!(nockapp.save_sem.available_permits(), 1);

        // Generate an invalid checkpoint by incrementing the event number
        let mut invalid = nockapp.kernel.checkpoint();
        invalid.event_num = invalid.event_num + 1;
        assert!(!invalid.validate());

        // The invalid checkpoint has a higher event number than the valid checkpoint
        let valid = jam_paths
            .load_checkpoint(nockapp.kernel.serf.stack())
            .unwrap();
        assert!(valid.event_num < invalid.event_num);

        // Save the corrupted checkpoint, because of the toggle buffer, we will write to jam file 1
        assert!(!jam_paths.1.exists());
        let jam_path = &jam_paths.1;
        let jam_bytes = invalid.encode().unwrap();
        tokio::fs::write(jam_path, jam_bytes).await.unwrap();

        // The loaded checkpoint will be the valid one
        let chk = jam_paths
            .load_checkpoint(nockapp.kernel.serf.stack())
            .unwrap();
        assert!(chk.event_num == valid.event_num);
    }

    #[tokio::test]
    #[cfg_attr(miri, ignore)]
    async fn test_jam_equality_stack() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let mut kernel = nockapp.kernel;
        let mut arvo = kernel.serf.arvo.clone();
        let stack = kernel.serf.stack();
        let j = jam(stack, arvo);
        let mut c = cue(stack, j).unwrap();
        // new nockstack
        unsafe { assert!(unifying_equality(stack, &mut arvo, &mut c)) }
    }

    // This actually gets used to test with miri
    // but when it was successful it took too long.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_jam_equality_slab_no_driver() {
        let bytes = include_bytes!("../../test-jams/test-ker.jam");
        let mut slab1 = NounSlab::new();
        slab1.cue_into(Bytes::from(Vec::from(bytes))).unwrap();
        let jammed_bytes = slab1.jam();
        let mut slab2 = NounSlab::new();
        let c = slab2.cue_into(jammed_bytes).unwrap();
        unsafe { assert!(slab_equality(slab1.root(), c)) }
    }

    #[tokio::test]
    #[cfg_attr(miri, ignore)]
    async fn test_jam_equality_slab() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let kernel = nockapp.kernel;
        let mut slab = NounSlab::new();
        let arvo = kernel.serf.arvo.clone();
        slab.copy_into(arvo);
        let bytes = slab.jam();
        let c = slab.cue_into(bytes).unwrap();
        unsafe { assert!(slab_equality(slab.root(), c)) }
    }

    #[tokio::test]
    #[cfg_attr(miri, ignore)]
    async fn test_jam_equality_slab_stack() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let mut kernel = nockapp.kernel;
        let mut arvo = kernel.serf.arvo.clone();
        let mut slab = NounSlab::new();
        slab.copy_into(arvo);
        // Use slab to jam
        let bytes = slab.jam();
        let stack = kernel.serf.stack();
        // Use the stack to cue
        let mut c = Noun::cue_bytes(stack, &bytes).unwrap();
        unsafe {
            // check for equality
            assert!(unifying_equality(stack, &mut arvo, &mut c))
        }
    }
}
