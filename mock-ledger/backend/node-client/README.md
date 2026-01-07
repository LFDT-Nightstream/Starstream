# Starstream client

Example showing how to connect to the Starstream ledger (from Rust). The connection can be established via any wRPC transport

It contains two examples for calling contracts:
1. `implicit_dynamic_call` for interacting with UTXOs in the ledger implicitly through Wasm components, where the interface for the component isn't known ahead of time
2. `implicit_static_call` for interacting with UTXOs in the ledger implicitly through Wasm components, where the interface for the component is statically known (ex: comes for wit_bindgen_wrpc)
3. `explicit_call` for interacting with UTXOs in the ledger via wRPC directly
