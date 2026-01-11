# wRPC Multiplexer

Goal: support a wRPC server that may have millions of components it has to server. Instead of serving all of them constantly which would require millions of handlers, there is instead a single handler for a `call` function. This `call` function can ephemerally load up a component as needed, server it, and then clear it from memory. The WIT for this `call` interface can be found [here](./wit/multiplexer.wit)

Implementation notes:
1. Although wRPC supports bidirectional streaming RPC, `call` does not. The full information is passed to it, and then it waits for the full response from the server. The conversion of constant data to streams internally is achieved through [adapters](./src/adapter.rs).
2. (as a corollary) it does not support connecting host resources to the guest
