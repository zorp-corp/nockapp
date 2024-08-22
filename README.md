![NockApp logo](https://zorp.io/img/nockapp.png)
# NockApp framework

***DEVELOPER ALPHA***

The NockApp framework is a set of tools for building NockApps: self-contained Nock state machines with Rust IO drivers.

NockApps use [Sword](https://github.com/zorp-corp/sword) for durable Nock execution.

The `crown` library is the primary framework for building NockApps. It provides a simple interface to a `Kernel`: a Nock core which can make state transitions with effects (via the `poke()` method) and allow inspection of its state via the `peek()` method.

For compiling Hoon to Nock, we're also including a pre-release of `choo`: a NockApp for the Hoon compiler. `choo` can compile Hoon to Nock as a batch-mode command-line process, without the need to spin up an interactive Urbit ship. It is intended both for developer workflows and for CI. `choo` is also our first example NockApp. More are coming!
