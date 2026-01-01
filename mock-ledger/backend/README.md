# Starstream ledger

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
