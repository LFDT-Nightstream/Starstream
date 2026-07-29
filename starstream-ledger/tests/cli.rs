//! End-to-end test driving the compiled `starstream-ledger` server with
//! the compiled `starstream-ledger-cli` client: the full
//! `score_contract_flow` from `tests/test.rs`, reproduced from the CLI
//! alone.

#![cfg(feature = "cli")]

use core::net::{Ipv4Addr, SocketAddr};
use core::time::Duration;

use std::net::TcpListener;
use std::process::Output;

use ed25519_dalek::SigningKey;
use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::componentize;
use tokio::net::TcpStream;

const NETWORK: &str = "starstream:test";

const SCORE_WIT: &str = include_str!("wit/score.wit");

const SCORE_PROGRESS_WIT: &str = include_str!("wit/score-progress.wit");

/// Compile a Starstream contract source to a Wasm component, the publishable
/// representation.
fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = typecheck_program(&program, TypecheckOptions::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = starstream_to_wasm::compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    let wasm = result.wasm.expect("compiling produced no Wasm");
    componentize(wasm).expect("failed to componentize contract")
}

/// Run the CLI against the ledger at `addr` and assert it succeeded,
/// returning its stdout.
async fn cli(addr: SocketAddr, args: &[&str]) -> String {
    let Output {
        status,
        stdout,
        stderr,
    } = tokio::process::Command::new(env!("CARGO_BIN_EXE_starstream-ledger-cli"))
        .arg("--url")
        .arg(format!("http://{addr}"))
        .args(args)
        .output()
        .await
        .expect("failed to run the CLI");
    let stdout = String::from_utf8_lossy(&stdout).into_owned();
    let stderr = String::from_utf8_lossy(&stderr);
    assert!(
        status.success(),
        "CLI {args:?} failed: {status}\nstdout: {stdout}\nstderr: {stderr}"
    );
    stdout
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn score_contract_flow_via_cli() {
    let seed = Sha256::digest("starstream:test:cli:account");
    let key = SigningKey::from_bytes(&seed.into());
    let account = hex::encode(key.verifying_key().to_bytes());

    let wasm = compile_contract(include_str!("../../examples/score.star"));
    let digest = hex::encode(Sha256::digest(&wasm));
    let wasm_path = std::env::temp_dir().join(format!("starstream-score-{digest}.wasm"));
    std::fs::write(&wasm_path, &wasm).unwrap();

    // Grab a free port, then let the ledger rebind it (SO_REUSEADDR).
    let addr = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))
        .unwrap()
        .local_addr()
        .unwrap();
    let mut ledger = tokio::process::Command::new(env!("CARGO_BIN_EXE_starstream-ledger"))
        .args([
            "--network",
            NETWORK,
            "--addr",
            &addr.to_string(),
            "--account",
            &format!("{account}={}", wasm.len()),
        ])
        .kill_on_drop(true)
        .spawn()
        .unwrap();
    for _ in 0..100 {
        if let Some(status) = ledger.try_wait().unwrap() {
            panic!("ledger exited early: {status}");
        }
        if TcpStream::connect(addr).await.is_ok() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    let stdout = cli(
        addr,
        &[
            "publish",
            "--key",
            &hex::encode(seed),
            "--network",
            NETWORK,
            "--nonce",
            "1",
            wasm_path.to_str().unwrap(),
        ],
    )
    .await;
    assert_eq!(stdout.trim(), digest);

    // With no script name the contract's script ABI is served as WIT.
    let stdout = cli(addr, &["script", &digest]).await;
    assert_eq!(stdout, SCORE_WIT);

    // `ScoreProgress::new()` in the script resolves through the UTXO import,
    // mapped back to this same contract; the script returns no results. The
    // resulting UTXO is persisted as transaction 0.
    let stdout = cli(
        addr,
        &[
            "script",
            "--utxo",
            &format!("score-progress={digest}"),
            &digest,
            "example",
        ],
    )
    .await;
    assert_eq!(stdout, "");

    // With no method name the UTXO's ABI is served as WIT.
    let stdout = cli(addr, &["method", "0", "0"]).await;
    assert_eq!(stdout, SCORE_PROGRESS_WIT);

    for (method, args) in [
        ("plus-chips", &["7"][..]),
        ("plus-mult", &["42"]),
        ("mult-mult", &["200"]),
        ("finish", &[]),
    ] {
        let stdout = cli(addr, &[&["method", "0", "0", method], args].concat()).await;
        assert_eq!(stdout, "", "unexpected `{method}` results");
    }
}
