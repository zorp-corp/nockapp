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
