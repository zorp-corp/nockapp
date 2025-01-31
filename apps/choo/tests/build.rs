use choo;
use tracing::{debug, info};

#[tokio::test]
#[cfg_attr(miri, ignore)]
async fn test_compile_test_app() -> Result<(), Box<dyn std::error::Error>> {
    // use std::path to get pwd() and then canonicalize
    let pwd = std::env::current_dir().unwrap();
    let mut test_dir = pwd.clone();
    test_dir.pop();
    test_dir.push("test-app");

    let entry = test_dir.join("bootstrap/kernel.hoon");

    // TODO: Add -o flag to specify output file and then use the tmp-dir
    // TODO: instead of mutating the non-tmp filesystem in this test
    // Clean up any existing output file
    let _ = tokio::fs::remove_file("out.jam").await;

    let mut deps_dir = pwd.clone();
    deps_dir.pop();
    deps_dir.push("hoon-deps");
    info!("Test directory: {:?}", test_dir);
    info!("Dependencies directory: {:?}", deps_dir);
    info!("Entry file: {:?}", entry);

    let mut nockapp = choo::initialize_with_default_cli(entry, deps_dir, false, true).await?;
    let result = choo::run_build(&mut nockapp).await;
    assert!(result.is_ok());
    // Cleanup
    let _ = tokio::fs::remove_file("out.jam").await;
    debug!("Removed file");

    // Second run to test consecutive execution
    // FIXME: This currently panics because of the one-shot.
    // let result = test_build(&mut nockapp).await;
    // // Cleanup
    // let _ = fs::remove_file("out.jam").await;
    result
}
