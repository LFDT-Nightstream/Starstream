//! End-to-end test driving the compiled `starstream-ledger` server with
//! the compiled `starstream-ledger-cli` client: the full
//! `score_contract_flow` from `tests/ledger.rs`, reproduced from the CLI
//! alone.

#![cfg(feature = "cli")]

use core::net::{Ipv4Addr, SocketAddr};
use core::time::Duration;

use std::net::TcpListener;
use std::process::Output;

use ed25519_dalek::SigningKey;
use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_ledger::encode_digest;
use starstream_runtime_next::componentize;
use tokio::net::TcpStream;

const NETWORK: &str = "starstream:test";

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
/// returning its stdout and stderr.
async fn cli(addr: SocketAddr, args: &[&str]) -> (String, String) {
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
    let stderr = String::from_utf8_lossy(&stderr).into_owned();
    assert!(
        status.success(),
        "CLI {args:?} failed: {status}\nstdout: {stdout}\nstderr: {stderr}"
    );
    (stdout, stderr)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn score_contract_flow_via_cli() {
    let seed = Sha256::digest("starstream:test:cli:account");
    let key = SigningKey::from_bytes(&seed.into());
    let account = hex::encode(key.verifying_key().to_bytes());

    let wasm = compile_contract(include_str!("../../examples/score.star"));
    let digest = encode_digest(&Sha256::digest(&wasm).into());
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
            "--admin-key",
            &account,
            "--admin-balance",
            &wasm.len().to_string(),
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

    let (stdout, _) = cli(
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
    let (stdout, _) = cli(addr, &["script", &digest]).await;
    assert_eq!(
        stdout,
        format!(
            "package starstream:contract;
interface {digest} {{
  example: func();
}}
"
        )
    );

    // `ScoreProgress::new()` in the script resolves through the UTXO import,
    // mapped back to this same contract; the script returns no results. The
    // resulting UTXO is persisted under its snapshot digest, which the CLI
    // reports on stderr.
    let (stdout, stderr) = cli(
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
    let (_, report) = stderr
        .split_once("UTXO persisted")
        .unwrap_or_else(|| panic!("no persisted UTXO reported on stderr: {stderr}"));
    // The canonical digest encoding of a sha2-256 multihash always starts
    // with `bciq`.
    let i = report
        .find("bciq")
        .unwrap_or_else(|| panic!("no UTXO digest reported on stderr: {stderr}"));
    let utxo: String = report[i..]
        .chars()
        .take_while(char::is_ascii_alphanumeric)
        .collect();
    assert_ne!(utxo, digest);

    // With no method name the UTXO's ABI is served as WIT.
    let (stdout, _) = cli(addr, &["method", &utxo]).await;
    assert_eq!(
        stdout,
        format!(
            "package starstream:utxo;
interface {utxo} {{
  plus-chips: func(chips2: u64);
  plus-mult: func(mult2: u64);
  mult-mult: func(mult-pct: u64);
  finish: func();
}}
"
        )
    );

    for (method, args) in [
        ("plus-chips", &["7"][..]),
        ("plus-mult", &["42"]),
        ("mult-mult", &["200"]),
        ("finish", &[]),
    ] {
        let (stdout, _) = cli(addr, &[&["method", &utxo, method], args].concat()).await;
        assert_eq!(stdout, "", "unexpected `{method}` results");
    }
}
