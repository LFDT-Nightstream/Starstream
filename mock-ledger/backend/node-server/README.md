# Starstream node

The Starstream node runs a mock ledger consisting of
- initial UTXOs (WASM Components) that form the genesis block
- a node RPC to allow querying the state of the ledger

## Concept

The node RPC allows not just introspection of the chain (finding which smart contracts are deployed - aka which UTXOs exist on the chain), but also facilitates calling these UTXOs as each of them is a WASM Component with a corresponding WIT interface.

Notably, the node RPC defines the following functions:
- `call`: call a function of a smart contract using the [Component model binary encoding](https://github.com/WebAssembly/component-model/blob/ddc8f6502c71e123ac083039fbd2ffdf3c0959f6/design/mvp/Binary.md) for the arguments and the return value.

## Current Implementation Status

- [ ] test encoding a single uint argument
- [ ] test encoding a multiple uint arguments
- [ ] encode return value between server & client using WASM Component binary spec
- [ ] handle function overloading
- [ ] test contracts that return streams
- [ ] test contracts that use complex types (structs, unions, resources, etc.)
- [ ] lookup by contract hash / utxo ID
- [ ] support member variable operations (instead of just calling functions)
- [ ] need the concept of accounts (P2PKH utxos)
- [ ] connect to the real Starstream
- [ ] connect to the playground
- [ ] dynamic registry based on UTXO set
- [ ] better error handling
- [ ] create example query from CLI (without needing to write Rust code)
- [ ] consider moving to something runtime agnostic like https://github.com/DouglasDwyer/wasm_component_layer

The following RPC calls needs to be added
- [ ] get UTXO by ID
- [ ] get UTXO set (with filters)
- [ ] `eth_chainId`
- [ ] `eth_getBalance`
- [ ] `eth_getLogs`
- [ ] get the WIT for a contract hash (and filter it based on utxo state)
