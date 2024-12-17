# Choo: compile hoon

To self-bootstrap:

```bash
cargo build
cargo run --release bootstrap/kernel.hoon ../hoon-deps
```

This will save the built kernel as a jam file in the `.data.choo` directory.

## Usage

The following assumes you have the `choo` binary in your path, which can be built with `cargo build --release` and found in `target/release/choo`.

For `choo`, the first argument is the entrypoint to the program, while the second argument is the root directory for source files.

```bash
choo main.hoon hoon/
```

### Building Arbitrary Hoon

To build arbitrary Hoon files, use the `--arbitrary` flag:

```bash
# Create a directory for your Hoon files
mkdir hoon

# Create a simple Hoon file
echo '%trivial' > hoon/trivial.hoon

# Build the Hoon file (exclude --new if you want to use the build cache)
choo --new --arbitrary hoon/trivial.hoon
```

## Hoon

Choo supports the Hoon language as defined in `/sys/hoon`.  However, the build system does not replicate Urbit's `+ford`
functionality exactly, as that is closely tied to the Urbit Arvo operating system.  `choo` supports the following build
runes:

- `/+` load from `/lib`
- `/-` load from `/sur` (currently discouraged in NockApps)
- `/=` load from specified path (required `%hoon` mark)
- `/*` load from specified path via specified mark (presumptively `%hoon` or `%jock`)
- `/?` version pinning (ignored)
