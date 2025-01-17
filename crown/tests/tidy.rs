#[test]
#[cfg_attr(miri, ignore)]
fn test_formatting() {
    let output = std::process::Command::new("cargo")
        .arg("fmt")
        .arg("--all")
        .arg("--")
        .arg("--check")
        .output()
        .unwrap();
    assert!(output.status.success());
}
