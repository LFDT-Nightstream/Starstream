use core::net::Ipv6Addr;

use std::ffi::OsStr;
use std::process::{Output, Stdio};
use std::sync::{Arc, LazyLock};

use anyhow::{Context as _, anyhow, ensure};
use ed25519_dalek::SigningKey;
use starstream_compiler::typecheck::TypecheckSuccess;
use starstream_compiler::{TypecheckFailure, TypecheckOptions, parse_program, typecheck_program};
use starstream_ledger::client::build_publish_envelope;
use starstream_ledger::server::Ledger;
use starstream_to_wasm::CompileResult;
use tempfile::NamedTempFile;
use tokio::fs;
use tokio::net::TcpListener;
use tokio::process::Command;
use wit_component::ComponentEncoder;

fn compile_contract(source: &str) -> anyhow::Result<Vec<u8>> {
    let (program, errs) = parse_program(source).into_output_errors();
    ensure!(errs.is_empty(), "failed to parse program: {errs:?}");
    let program = program.context("parser did not produce a program")?;

    let TypecheckSuccess { program, .. } = typecheck_program(&program, TypecheckOptions::default())
        .map_err(|TypecheckFailure { errors, .. }| {
            anyhow!("failed to typecheck program: {:?}", errors)
        })?;

    let CompileResult { errors, wasm, .. } = starstream_to_wasm::compile(&program);
    ensure!(errors.is_empty(), "failed to compile program: {errors:?}");

    let wasm = wasm.context("compilation did not produce Wasm")?;
    ComponentEncoder::default()
        .validate(true)
        .module(&wasm)
        .context("failed to set core component module")?
        .encode()
        .context("failed to encode a component")
}

const NETWORK: &str = "starstream:test";

static SCORE_WASM: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")).unwrap());

static ADMIN: LazyLock<SigningKey> = LazyLock::new(|| SigningKey::from_bytes(&[0x42; 32]));

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
    assert!(status.success());
    assert_eq!(stderr, b"");
    Ok(stdout)
}

#[tokio::test]
async fn cli() -> anyhow::Result<()> {
    let account = run_cli(["key", "generate"]).await?;
    let mut buf = [0u8; 32];
    hex::decode_to_slice(&account, &mut buf).context("failed to parse generated key")?;
    let account = SigningKey::from_bytes(&buf);

    let account_file = NamedTempFile::new()?;
    fs::write(&account_file, hex::encode(account.to_bytes()))
        .await
        .context("failed to write account key file")?;

    let admin_file = NamedTempFile::new()?;
    fs::write(&admin_file, hex::encode(ADMIN.to_bytes()))
        .await
        .context("failed to write admin key file")?;

    let addr = {
        let lis = TcpListener::bind((Ipv6Addr::LOCALHOST, 0))
            .await
            .context("failed to bind TCP listener")?;
        lis.local_addr()
            .context("failed to get TCP listener local address")?
    };

    let ledger = Ledger::new(
        wasmtime::Engine::default(),
        128,
        NETWORK,
        ADMIN.verifying_key(),
    );
    let ledger = Arc::new(ledger);
    let (ledger, shutdown) = ledger
        .handle_http(addr)
        .await
        .context("failed to handle HTTP")?;
    let ledger = tokio::spawn(ledger);

    let wasm = NamedTempFile::new()?;
    fs::write(&wasm, &*SCORE_WASM)
        .await
        .with_context(|| format!("failed to write Wasm to `{}`", wasm.path().display()))?;

    let publish_envelope =
        build_publish_envelope(account.clone(), NETWORK, 1, SCORE_WASM.as_slice())?;
    let publish_cost = publish_envelope.len();

    let stdout = run_cli(["--url", &format!("http://{addr}"), "block", "height"]).await?;
    assert_eq!(stdout, b"0");

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
    .await?;
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
    .await?;
    assert_eq!(stdout, b"");

    let stdout = run_cli(["--url", &format!("http://{addr}"), "block", "height"]).await?;
    assert_eq!(stdout, b"2");

    let digest = run_cli(["contract", "digest", &wasm.path().to_string_lossy()]).await?;
    let digest = str::from_utf8(&digest).context("contract digest is not valid UTF-8")?;

    let stdout = run_cli([
        "--url",
        &format!("http://{addr}"),
        "contract",
        "script",
        "call",
        digest,
        "example",
    ])
    .await?;
    assert_eq!(stdout, b"()");

    shutdown.notify_one();
    ledger.await.context("ledger task panicked")
}
