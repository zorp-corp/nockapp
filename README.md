![NockApp logo](https://private-user-images.githubusercontent.com/1377557/358028192-6ecd153e-8f28-4e8e-89b7-43e8c6b1fb94.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3MjQzNDc0NjEsIm5iZiI6MTcyNDM0NzE2MSwicGF0aCI6Ii8xMzc3NTU3LzM1ODAyODE5Mi02ZWNkMTUzZS04ZjI4LTRlOGUtODliNy00M2U4YzZiMWZiOTQucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDgyMiUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA4MjJUMTcxOTIxWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9NjkxMzYyNmVjZDMzYTc3NTZkOWYzYjU5MDhiMjk1Y2JiNWIyODI5NDA5NmQxMzNkMjI0YjgxMDUwNzlmNjM5ZSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QmYWN0b3JfaWQ9MCZrZXlfaWQ9MCZyZXBvX2lkPTAifQ.ZrhihJkN6D3gby8TSXZIonD3GrfaFSoDIy_0JuX7wyA)
# NockApp framework

***DEVELOPER ALPHA***

The NockApp framework is a set of tools for building NockApps: self-contained Nock state machines with Rust IO drivers.

NockApps use [Sword](https://github.com/zorp-corp/sword) for durable Nock execution.

The `crown` library is the primary framework for building NockApps. It provides a simple interface to a `Kernel`: a Nock core which can make state transitions with effects (via the `poke()` method) and allow inspection of its state via the `peek()` method.

For compiling Hoon to Nock, we're also including a pre-release of `choo`: a NockApp for the Hoon compiler. `choo` can compile Hoon to Nock as a batch-mode command-line process, without the need to spin up an interactive Urbit ship. It is intended both for developer workflows and for CI. `choo` is also our first example NockApp. More are coming!
