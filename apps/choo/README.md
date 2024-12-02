# Choo: compile hoon

To self-bootstrap:

```
$ cargo build
$ ../target/debug/choo --new --knob hoon/lib/hoon-139.hoon
$ mv out.jam hoon-139.jam
$ ../target/debug/choo --new hoon/lib/kernel.hoon hoon-139.jam
$ mv out.jam bootstrap/choo.jam
$ cargo build
```

You now have a choo executable whose nock was built entirely by choo from Hoon sources.

## Usage

For `choo`, the first argument is the entrypoint to the program, while the second argument is the root directory for source files.

```bash
choo main.hoon hoon/
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
