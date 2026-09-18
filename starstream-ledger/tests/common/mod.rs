use core::net::{Ipv6Addr, SocketAddr};

use std::collections::BTreeSet;
use std::sync::LazyLock;

use anyhow::Context as _;
use ed25519_dalek::SigningKey;
use sha2::{Digest as _, Sha256};
use tokio::net::TcpListener;

#[path = "../../../starstream-runtime-next/tests/common/mod.rs"]
pub mod runtime;
pub use runtime::*;

pub static ADMIN: LazyLock<SigningKey> = LazyLock::new(|| SigningKey::from_bytes(&[0x42; 32]));

pub const NETWORK: &str = "starstream:test";

pub static SCORE_WASM: LazyLock<Box<[u8]>> = LazyLock::new(|| {
    compile_contract(include_str!("../../../examples/score.star"))
        .unwrap()
        .into_boxed_slice()
});

pub static SCORE_WASM_DIGEST: LazyLock<[u8; 32]> =
    LazyLock::new(|| Sha256::digest(&*SCORE_WASM).into());

pub static SCORE_EXAMPLE_METHODS: LazyLock<BTreeSet<(u64, u64, u64, u64)>> = LazyLock::new(|| {
    [
        "get_chips",
        "get_mult",
        "plus_chips",
        "plus_mult",
        "mult_mult",
        "finish",
    ]
    .map(method_hash)
    .into_iter()
    .collect()
});

pub async fn free_tcp_addr() -> anyhow::Result<SocketAddr> {
    let lis = TcpListener::bind((Ipv6Addr::LOCALHOST, 0))
        .await
        .context("failed to bind TCP listener")?;
    lis.local_addr()
        .context("failed to get TCP listener local address")
}
