//! Compiled example Starstream contracts (from `examples/`) for the host
//! crates' tests.

pub const EXAMPLE_SCORE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/score.wasm"));
