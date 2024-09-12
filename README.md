# NockApp

***DEVELOPER ALPHA***

<img align="right" src="https://zorp.io/img/nockapp.png" height="150px" alt="NockApp">

NockApps are pure-functional state machines with automatic persistence and modular IO.

The NockApp framework provides two modules, Crown and Sword:
1. Crown provides a minimal Rust interface to a Nock kernel.
2. [Sword](https://github.com/zorp-corp/sword) is a modern Nock runtime that achieves durable execution.

<br>

## Get Started

To test compiling a Nock kernel using the `choo` command-line Hoon compiler, run the following commands:

```
cd choo
cargo run --release hoon/lib/kernel.hoon
```

## Building NockApps

The `crown` library is the primary framework for building NockApps. It provides a simple interface to a `Kernel`: a Nock core which can make state transitions with effects (via the `poke()` method) and allow inspection of its state via the `peek()` method.

For compiling Hoon to Nock, we're also including a pre-release of `choo`: a NockApp for the Hoon compiler. `choo` can compile Hoon to Nock as a batch-mode command-line process, without the need to spin up an interactive Urbit ship. It is intended both for developer workflows and for CI. `choo` is also our first example NockApp. More are coming!

