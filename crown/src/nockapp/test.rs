#[cfg(test)]
mod tests {
    use crate::kernel::checkpoint::JamPaths;
    use crate::kernel::form::Kernel;
    use crate::noun::slab::{slab_equality, NounSlab};
    use crate::{NockApp, NounExt};

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
            .join("tests")
            .join(jam);
        let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
        let kernel = Kernel::load(jam_paths, &jam_bytes, false).expect("Failed to load kernel");
        (temp_dir, NockApp::new(kernel, Duration::from_secs(1)))
    }

    async fn save_nockapp(nockapp: &mut NockApp) {
        let permit = nockapp.save_sem.clone().acquire_owned().await;
        let _ = nockapp.save(permit).await;
        let _ = nockapp
            .tasks
            .lock()
            .await
            .join_next()
            .await
            .expect("Failed to join task");
    }

    // Test nockapp save
    #[tokio::test]
    #[traced_test]
    async fn test_nockapp_save() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        let mut arvo = nockapp.kernel.serf.arvo;
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
        assert!(checkpoint.event_num == 0);

        // Checkpoint kernel should be equal to the saved kernel
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut checkpoint.ker_state,
                &mut arvo
            )
            .expect("Unifying equality failed with allocation error"));
        }

        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint
            .cold
            .into_noun(nockapp.kernel.serf.stack())
            .unwrap();
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack())
            .unwrap();
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            )
            .expect("unifying equality failed with allocation error"));
        };
    }

    // Test nockapp poke
    #[tokio::test]
    #[traced_test]
    async fn test_nockapp_poke_save() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let mut arvo_before_poke = nockapp.kernel.serf.arvo;

        let poke = D(tas!(b"inc"));

        let _ = nockapp.kernel.poke(poke).unwrap();

        // Save
        save_nockapp(&mut nockapp).await;

        // A valid checkpoint should exist in one of the jam files
        let jam_paths = &nockapp.kernel.jam_paths;
        let checkpoint = jam_paths.load_checkpoint(nockapp.kernel.serf.stack());
        assert!(checkpoint.is_ok());
        let mut checkpoint = checkpoint.unwrap();

        // Checkpoint event number should be 1
        assert!(checkpoint.event_num == 1);
        let mut arvo_after_poke = nockapp.kernel.serf.arvo;

        unsafe {
            let stack = nockapp.kernel.serf.stack();
            // Checkpoint kernel should be equal to the saved kernel
            assert!(
                unifying_equality(stack, &mut checkpoint.ker_state, &mut arvo_after_poke)
                    .expect("unifying equality failed with allocation error")
            );
            // Checkpoint kernel should be different from the kernel before the poke
            assert!(
                !unifying_equality(stack, &mut checkpoint.ker_state, &mut arvo_before_poke)
                    .expect("unifying equality failed with allocation error")
            );
        }

        // Checkpoint cold state should be equal to the saved cold state
        let mut cold_chk_noun = checkpoint
            .cold
            .into_noun(nockapp.kernel.serf.stack())
            .unwrap();
        let mut cold_noun = nockapp
            .kernel
            .serf
            .context
            .cold
            .into_noun(nockapp.kernel.serf.stack())
            .unwrap();
        unsafe {
            assert!(unifying_equality(
                nockapp.kernel.serf.stack(),
                &mut cold_chk_noun,
                &mut cold_noun
            )
            .expect("unifying equality failed with allocation error"));
        };
    }

    #[tokio::test]
    async fn test_nockapp_save_multiple() {
        let (_temp, mut nockapp) = setup_nockapp("test-ker.jam");
        assert_eq!(nockapp.kernel.serf.event_num, 0);
        let jam_paths = nockapp.kernel.jam_paths.clone();

        for i in 1..4 {
            // Poke to increment the state
            let poke = D(tas!(b"inc"));
            let _ = nockapp.kernel.poke(poke).unwrap();

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
            let peek = T(nockapp.kernel.serf.stack(), &[D(tas!(b"state")), D(0)]).unwrap();

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
        let mut invalid = nockapp.kernel.checkpoint().unwrap();
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
    async fn test_jam_equality_stack() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let mut kernel = nockapp.kernel;
        let mut arvo = kernel.serf.arvo.clone();
        let stack = kernel.serf.stack();
        let j = jam(stack, arvo).unwrap();
        let mut c = cue(stack, j).unwrap();
        // new nockstack
        unsafe {
            assert!(unifying_equality(stack, &mut arvo, &mut c)
                .expect("unifying equality failed with allocation error"))
        }
    }

    #[tokio::test]
    async fn test_jam_equality_slab() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let kernel = nockapp.kernel;
        let mut slab = NounSlab::new();
        let arvo = kernel.serf.arvo.clone();
        slab.copy_into(arvo).unwrap();
        let bytes = slab.jam();
        let c = slab.cue_into(bytes).unwrap();
        unsafe { assert!(slab_equality(slab.root(), c)) }
    }

    #[tokio::test]
    async fn test_jam_equality_slab_stack() {
        let (_temp, nockapp) = setup_nockapp("test-ker.jam");
        let mut kernel = nockapp.kernel;
        let mut arvo = kernel.serf.arvo.clone();
        let mut slab = NounSlab::new();
        slab.copy_into(arvo).unwrap();
        // Use slab to jam
        let bytes = slab.jam();
        let stack = kernel.serf.stack();
        // Use the stack to cue
        let mut c = Noun::cue_bytes(stack, &bytes).unwrap();
        unsafe {
            // check for equality
            assert!(unifying_equality(stack, &mut arvo, &mut c)
                .expect("unifying equality failed with allocation error"))
        }
    }
}
