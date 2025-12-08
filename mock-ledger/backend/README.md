# Starstream ledger

## Build

```bash
cargo install wit-deps-cli
cd node-server && wit-deps && cd ..
cd node-client && wit-deps && cd ..
cargo build
```

## Start

Start server

```bash
cargo run -p starstream-node-server -- '[::1]:7762'
```

Start client

```bash
cargo run -p starstream-node-client -- '[::1]:7762'
```
