# Choo: compile hoon

To self-bootstrap:

```bash
cargo run --release bootstrap/kernel.hoon ../hoon-deps
```

This will save the built kernel as `out.jam` in the current directory. This should be moved to the bootstrap directory so the `choo` binary can pick it up:

```bash
mv out.jam bootstrap/choo.jam
```

Once this is done, you should be able to run

``` bash
cargo build --release
```

and use the resulting binary in `target/release/choo` (in the `nockapp` directory) to build NockApp kernels or arbitrary hoon files as detailed in the following section.

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
