use std::ffi::OsStr;
use std::process::{Output, Stdio};
use std::sync::Arc;

use anyhow::{Context as _, ensure};
use ed25519_dalek::SigningKey;
use starstream_ledger::client::build_publish_envelope;
use starstream_ledger::server::Ledger;
use starstream_ledger::{Envelope, EnvelopeContext, Publish, Transaction, TransactionOutput};
use tempfile::NamedTempFile;
use tokio::fs;
use tokio::process::Command;

#[path = "../../starstream-ledger/tests/common/mod.rs"]
pub mod ledger;
pub use ledger::*;

async fn run_cli(args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> anyhow::Result<Vec<u8>> {
    let cmd = Command::new(env!("CARGO_BIN_EXE_starstream-ledger-cli"))
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .context("failed to spawn CLI process")?;
    let Output {
        status,
        stdout,
        stderr,
    } = cmd
        .wait_with_output()
        .await
        .context("failed to wait for CLI")?;
    ensure!(status.success());
    ensure!(stderr == b"");
    Ok(stdout)
}

#[tokio::test]
async fn cli() {
    let account = run_cli(["key", "generate"]).await.unwrap();
    let mut buf = [0u8; 32];
    hex::decode_to_slice(account.trim_ascii_end(), &mut buf)
        .expect("failed to parse generated key");
    let account = SigningKey::from_bytes(&buf);

    let account_file = NamedTempFile::new().unwrap();
    fs::write(&account_file, hex::encode(account.to_bytes()))
        .await
        .expect("failed to write account key file");

    let admin_file = NamedTempFile::new().unwrap();
    fs::write(&admin_file, hex::encode(ADMIN.to_bytes()))
        .await
        .expect("failed to write admin key file");

    let ledger = Ledger::new(
        wasmtime::Engine::default(),
        128,
        NETWORK,
        ADMIN.verifying_key(),
        [],
    );
    let ledger = Arc::new(ledger);
    let addr = free_tcp_addr().await.unwrap();
    let (ledger, shutdown) = ledger
        .handle_http(addr)
        .await
        .expect("failed to handle HTTP");
    let ledger = tokio::spawn(ledger);

    let wasm = NamedTempFile::new().unwrap();
    fs::write(&wasm, &*SCORE_WASM)
        .await
        .with_context(|| format!("failed to write Wasm to `{}`", wasm.path().display()))
        .unwrap();

    let digest = run_cli(["digest", &wasm.path().to_string_lossy()])
        .await
        .unwrap();
    let digest = str::from_utf8(&digest)
        .expect("contract digest is not valid UTF-8")
        .trim_end();

    let tx_file = NamedTempFile::new().unwrap();
    let stdout = run_cli([
        "--url",
        &format!("http://{addr}"),
        "contract",
        "script",
        "call",
        "--network",
        NETWORK,
        "--simulate",
        "--output-transaction",
        &tx_file.path().to_string_lossy(),
        "--import",
        &wasm.path().to_string_lossy(),
        digest,
        "example",
    ])
    .await
    .unwrap();
    assert_eq!(stdout, b"()\n");
    let tx = fs::read(&tx_file).await.unwrap();
    let Envelope {
        context,
        network,
        payload: Transaction {
            inputs, outputs, ..
        },
    } = minicbor::decode(&tx).expect("failed to decode transaction envelope");
    assert_eq!(context, EnvelopeContext::Transaction);
    assert_eq!(network.as_ref(), NETWORK);
    assert_eq!(inputs, []);
    let [
        TransactionOutput {
            contract, instance, ..
        },
    ] = outputs.as_slice()
    else {
        panic!("invalid outputs: {outputs:?}")
    };
    assert_eq!(contract.as_ref(), digest);
    assert_eq!(instance.as_ref(), "score-progress");

    let publish_envelope = build_publish_envelope(
        account.clone(),
        NETWORK,
        Publish {
            nonce: 1,
            wasm: SCORE_WASM.clone(),
        },
    )
    .unwrap();
    let publish_cost = publish_envelope.len();

    let stdout = run_cli(["--url", &format!("http://{addr}"), "block", "height"])
        .await
        .unwrap();
    assert_eq!(stdout, b"0\n");

    let stdout = run_cli([
        "--url",
        &format!("http://{addr}"),
        "account",
        "fund",
        "--key",
        &admin_file.path().to_string_lossy(),
        "--network",
        NETWORK,
        "--nonce",
        "1",
        &hex::encode(account.verifying_key().to_bytes()),
        &publish_cost.to_string(),
    ])
    .await
    .unwrap();
    assert_eq!(stdout, b"");

    let stdout = run_cli([
        "--url",
        &format!("http://{addr}"),
        "contract",
        "publish",
        "--key",
        &account_file.path().to_string_lossy(),
        "--network",
        NETWORK,
        "--nonce",
        "1",
        &wasm.path().to_string_lossy(),
    ])
    .await
    .unwrap();
    assert_eq!(stdout, b"");

    let stdout = run_cli(["--url", &format!("http://{addr}"), "block", "height"])
        .await
        .unwrap();
    assert_eq!(stdout, b"2\n");

    let tx_file = NamedTempFile::new().unwrap();
    let stdout = run_cli([
        "--url",
        &format!("http://{addr}"),
        "contract",
        "script",
        "call",
        "--network",
        NETWORK,
        "--simulate",
        "--output-transaction",
        &tx_file.path().to_string_lossy(),
        digest,
        "example",
    ])
    .await
    .unwrap();
    assert_eq!(stdout, b"()\n");
    let tx = fs::read(&tx_file).await.unwrap();
    let Envelope {
        context,
        network,
        payload: Transaction {
            inputs, outputs, ..
        },
    } = minicbor::decode(&tx).expect("failed to decode transaction envelope");
    assert_eq!(context, EnvelopeContext::Transaction);
    assert_eq!(network.as_ref(), NETWORK);
    assert_eq!(inputs, []);
    let [
        TransactionOutput {
            contract, instance, ..
        },
    ] = outputs.as_slice()
    else {
        panic!("invalid outputs: {outputs:?}")
    };
    assert_eq!(contract.as_ref(), digest);
    assert_eq!(instance.as_ref(), "score-progress");

    shutdown.notify_one();
    ledger.await.expect("ledger task panicked");
}
