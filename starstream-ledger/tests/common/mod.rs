use core::net::{Ipv6Addr, SocketAddr};

use std::collections::BTreeSet;
use std::path::Path;
use std::sync::LazyLock;

use anyhow::{Context as _, anyhow, ensure};
use ed25519_dalek::SigningKey;
use sha2::{Digest as _, Sha256};
use starstream_compiler::{ModuleGraph, TypecheckOptions, typecheck_modules};
use starstream_types::FileSystem;
use tokio::net::TcpListener;

#[path = "../../../starstream-runtime-next/tests/common/mod.rs"]
pub mod runtime;
pub use runtime::*;

pub static ADMIN: LazyLock<SigningKey> = LazyLock::new(|| SigningKey::from_bytes(&[0x42; 32]));

pub const NETWORK: &str = "starstream:test";

fn hash_methods<const N: usize>(names: [&str; N]) -> BTreeSet<(u64, u64, u64, u64)> {
    names.map(method_hash).into_iter().collect()
}

pub fn compile_contract_file(path: impl AsRef<Path>) -> anyhow::Result<Vec<u8>> {
    let mut fs = FileSystem::default();
    let (graph, module_id) = ModuleGraph::from_entry(&mut fs, path.as_ref())
        .map_err(|errors| anyhow!("failed to load module graph: {errors:?}"))?;
    let graph = typecheck_modules(&graph, TypecheckOptions::default())
        .map_err(|failure| anyhow!("failed to typecheck program: {:?}", failure.errors))?;
    let compile_result = starstream_to_wasm::compile_contract(&graph, module_id);
    ensure!(
        compile_result.errors.is_empty(),
        "failed to compile program: {:#?}",
        compile_result.errors
    );
    compile_result
        .to_component()
        .map_err(|err| anyhow!("failed to componentize program: {err:?}"))
}

pub static SCORE_WASM: LazyLock<Box<[u8]>> = LazyLock::new(|| {
    compile_contract(include_str!("../../../examples/score.star"))
        .unwrap()
        .into_boxed_slice()
});

pub static SCORE_WASM_DIGEST: LazyLock<[u8; 32]> =
    LazyLock::new(|| Sha256::digest(&*SCORE_WASM).into());

pub static SCORE_EXAMPLE_METHODS: LazyLock<BTreeSet<(u64, u64, u64, u64)>> = LazyLock::new(|| {
    hash_methods([
        "get_chips",
        "get_mult",
        "plus_chips",
        "plus_mult",
        "mult_mult",
        "finish",
    ])
});

pub static REQUIRE_HASH_PREIMAGE_WASM: LazyLock<Box<[u8]>> = LazyLock::new(|| {
    let mut sha256lib = std::process::Command::new(env!("CARGO"))
        .args([
            "build",
            "--release",
            "--target",
            "wasm32-unknown-unknown",
            "-p",
            "sha256lib",
        ])
        .spawn()
        .unwrap();
    let status = sha256lib.wait().unwrap();
    if !status.success() {
        panic!("failed to compile `sha256lib`")
    }
    compile_contract_file(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../examples/require_hash_preimage.star"
    ))
    .unwrap()
    .into_boxed_slice()
});

pub static REQUIRE_HASH_PREIMAGE_WASM_DIGEST: LazyLock<[u8; 32]> =
    LazyLock::new(|| Sha256::digest(&*REQUIRE_HASH_PREIMAGE_WASM).into());

pub static REQUIRE_HASH_PREIMAGE_CREATE_METHODS: LazyLock<BTreeSet<(u64, u64, u64, u64)>> =
    LazyLock::new(|| hash_methods(["consume"]));

pub async fn free_tcp_addr() -> anyhow::Result<SocketAddr> {
    let lis = TcpListener::bind((Ipv6Addr::LOCALHOST, 0))
        .await
        .context("failed to bind TCP listener")?;
    lis.local_addr()
        .context("failed to get TCP listener local address")
}
