# Starstream ledger

The Starstream node runs a mock ledger consisting of
- initial UTXOs (WASM Components) that form the genesis block
- a node RPC to allow querying the state of the ledger

## Build

```bash
cargo install wit-deps-cli
cd wrpc-multiplexer && wit-deps update && cd ..
cd wit/node-rpc && wit-deps update && cd ../..
cd node-server && wit-deps update && cd ..
cd node-client/wit/rpc && wit-deps update && cd ../../..
cd node-client/wit/external && wit-deps update && cd ../../..
cargo build
```

## Start

Start server

```bash
cargo run -p starstream-node-server -- '[::1]:7762'
```

Start client

```bash
cargo run -p starstream-node-client -- --addr='[::1]:7762' --contract-hash='0x1170FAD15BECBB08C00B29067171110B34E8B4CEBC648BA662147BA0F2F1224F'
```

## Concept

The node RPC allows not just introspection of the chain (finding which smart contracts are deployed - aka which UTXOs exist on the chain), but also facilitates calling these UTXOs as each of them is a WASM Component with a corresponding WIT interface.

Notably, the node RPC defines the following functions:
- `call`: call a function of a smart contract using the wRPC extension of the [Component model binary encoding](https://github.com/WebAssembly/component-model/blob/ddc8f6502c71e123ac083039fbd2ffdf3c0959f6/design/mvp/Binary.md) for the arguments and the return value.

## Current Implementation Status

Testing WASM Component encodings
- [ ] test encoding a single uint argument
- [ ] test encoding a multiple uint arguments
- [ ] encode return value between server & client using WASM Component binary spec
- [ ] design & implement multiplexing so the host can call the underlying WASM component based on contract address
    - current we have a complex `encode.rs`, but this could maybe be cleaner if it was an in-memory transport for wRPC
- [ ] handle function overloading (?)
- [ ] test contracts that return streams
- [ ] test contracts that use complex types (structs, unions, resources, etc.)
- [ ] better error handling
- [ ] allow calling the same contract twice

Ledger itself (server)
- [ ] need the concept of accounts (P2PKH utxos)
- [ ] dynamic registry based on UTXO set
- [ ] built a reth-like plugin system for the ledger (tricky: how to sandbox)
- [ ] persist UTXO on disk. Probably needs to be a combination of (component bytes, host interface) to allow for upgrading the host
- [ ] compilation cache
    - [ ] look into better startup times with InstancePre
    - [ ] serialize components into cache
- [ ] look into wasmtime allocation_strategy
- [ ] cost depending on memory size of UTXO
- [ ] disable floating point in wasmtime (?)
- [ ] have a single global engine to ensure cache/allocation logic works properly
- [ ] enforce determinism
    - [ ] enable deterministic execution https://github.com/bytecodealliance/wasmtime/pull/11284/files#diff-908a422cef87524478865a23918af79040c970e02a3cc6862f6eb508710606f9R2770
    - [ ] review Component Model non-determinism: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Concurrency.md#nondeterminism

Ledger client
- [ ] Implement browser-compatible client

Registry:
- [ ] accept WIT files placed into it
    - [ ] flatten dependencies (and make sure they're all in the registry)
    - [ ] make sure the package name is the WIT hash (incl hash of all dependencies) - reuse wit-deps logic for this
    - [ ] only use most strict semver possible (to guarantee unique result)
    - [ ] careful about wit deduplication causing file content to change due to semver (i.e an `import` disappears because it is redundant as it's covered my another import)

External connections
- [ ] connect to the real Starstream
- [ ] connect to the playground
- [ ] parse CLI arguments with [WAVE](https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-wave)
- [ ] look into wassette
- [ ] consider moving to something runtime agnostic like https://github.com/DouglasDwyer/wasm_component_layer
- [ ] have the ledger have a Ghostty frontend that can compile to WASM
- [ ] implement MessageBroadcast as a communication style for wRPC
    - "The Preview 3 release of the component model comes with asynchronous streams. These can avoid copies between the host and guest components by providing the host with a buffer within the component’s memory space that it can write into directly, or pass to asynchronous I/O operations provided by the operating system, like io_uring. For avoiding copies between components themselves, there’s work on shared heaps which could be leveraged for views of a single buffer with exclusive ownership of this view being passed between components."
    - MessageBroadcast may be hard if each component also has its own UI that it's rendering to a canvas. Two items to look into:
        - wasi-gfx (wasi:surface and wasi-webgpu)
        - OffscreenCanvas
    - Possibly use DurableStreams instead (https://github.com/durable-streams/durable-streams)
- [ ] wallet WASM component
    - maybe as a reth-like plugin that decrypts every block as it comes in?
        - too tightly coupled for real wallets connecting over RPC

Refactoring
- [ ] double-check that `hash.rs` matches the result of other poseidon2+goldilock hashes out there
- [ ] docs

wRPC Multiplexer
- [ ] support passing host resources. This is a bit tricky, as we'd have to merge the host resources from the "client" with the host resources from the ledger

New RPC calls
- [ ] use fuel to avoid `call` DOSing RPC nodes
- [ ] get UTXO by ID
- [ ] get UTXO set (with filters)
- [ ] `eth_chainId`
- [ ] `eth_getBalance`
- [ ] `eth_getLogs`
- [ ] get the WIT for a contract hash (and filter it based on utxo state)
