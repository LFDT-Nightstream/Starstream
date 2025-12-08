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
cargo run -p starstream-node-client -- --addr='[::1]:7762' --contract-hash='0x1170FAD15BECBB08C00B29067171110B34E8B4CEBC648BA662147BA0F2F1224F'
```
