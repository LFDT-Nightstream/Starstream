# Starstream node

The Starstream node runs a mock ledger consisting of
- initial UTXOs (WASM Components) that form the genesis block
- a node RPC to allow querying the state of the ledger

## Concept

The node RPC allows not just introspection of the chain (finding which smart contracts are deployed - aka which UTXOs exist on the chain), but also facilitates calling these UTXOs as each of them is a WASM Component with a corresponding WIT interface.

Notably, the node RPC defines the following functions:
- `call`: call a function of a smart contract using the [Component model binary encoding](https://github.com/WebAssembly/component-model/blob/ddc8f6502c71e123ac083039fbd2ffdf3c0959f6/design/mvp/Binary.md) for the arguments and the return value.

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

Ledger itself
- [ ] need the concept of accounts (P2PKH utxos)
- [ ] dynamic registry based on UTXO set
- [ ] built a reth-like plugin system for the ledger (tricky: how to sandbox)

Connections
- [ ] connect to the real Starstream
- [ ] connect to the playground
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

New RPC calls
- [ ] get UTXO by ID
- [ ] get UTXO set (with filters)
- [ ] `eth_chainId`
- [ ] `eth_getBalance`
- [ ] `eth_getLogs`
- [ ] get the WIT for a contract hash (and filter it based on utxo state)
