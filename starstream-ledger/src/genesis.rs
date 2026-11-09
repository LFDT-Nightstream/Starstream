//! Execution of a ledger's genesis coordination scripts.

use std::collections::HashMap;

use anyhow::{Context as _, bail, ensure};
use bytes::Bytes;
use serde::Deserialize;
use tracing::warn;
use wasmtime::component::Val;
use wasmtime::{Engine, Store};
use wasmtime_wizer::Wizer;

use crate::client::runtime::{Client, Contract, Ctx, call_coordination_script, compile_contract};
use crate::{CardanoCtx, TransactionInput, TransactionOutput, encode_digest, parse_digest};

/// Coordination script call producing genesis outputs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GenesisScript {
    /// Digest of the contract exporting the coordination script.
    pub contract: Box<str>,
    /// Coordination script to call.
    pub script: Box<str>,
    /// WAVE-encoded coordination script arguments.
    #[serde(default)]
    pub args: Vec<Box<str>>,
}

/// Definition of a ledger's genesis.
#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Genesis {
    #[serde(default)]
    pub scripts: Vec<GenesisScript>,
}

struct GenesisClient<'a>(&'a HashMap<[u8; 32], Bytes>);

fn get_contract<'a>(
    contracts: &'a HashMap<[u8; 32], Bytes>,
    digest: &[u8; 32],
) -> anyhow::Result<&'a Bytes> {
    contracts
        .get(digest)
        .with_context(|| format!("contract `{}` not imported", encode_digest(digest)))
}

impl Client for GenesisClient<'_> {
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        get_contract(self.0, &digest).cloned()
    }

    async fn get_input_utxo(&self, _input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        bail!("genesis scripts cannot have UTXO inputs")
    }
}

/// Run the scripts of `genesis` using `engine`, resolving contracts in `imported`
/// by their digest, and return the outputs they create.
///
/// `engine` must be configured with [`crate::configure_wasmtime`].
pub async fn build_genesis(
    engine: &Engine,
    genesis: Genesis,
    imported: &HashMap<[u8; 32], Bytes>,
    cardano: CardanoCtx,
) -> anyhow::Result<Vec<TransactionOutput>> {
    let wizer = Wizer::new();
    let mut contracts: HashMap<_, Contract> = HashMap::with_capacity(imported.len());
    let mut outputs = Vec::default();
    for GenesisScript {
        contract,
        script,
        args,
    } in genesis.scripts
    {
        let digest = parse_digest(&contract)
            .with_context(|| format!("failed to parse `{contract}` as multibase multihash"))?;
        let Contract { contract, wasm } = if let Some(contract) = contracts.get(&digest) {
            contract.clone()
        } else {
            let wasm = get_contract(imported, &digest)?;
            let contract = compile_contract(
                &GenesisClient(imported),
                engine,
                &wizer,
                wasm,
                None,
                &mut contracts,
            )
            .await?;
            let contract = Contract {
                contract: Some(contract),
                wasm: wasm.clone(),
            };
            contracts.insert(digest, contract.clone());
            contract
        };
        let contract = contract.context("contract was not compiled")?;
        let script = contract.get_coordination_script(&script)?;
        let ty = script.ty();
        let mut params = Vec::with_capacity(ty.params().len());
        let mut args = args.into_iter();
        for (name, ty) in ty.params() {
            let v = args
                .next()
                .with_context(|| format!("missing value for parameter `{name}`"))?;
            let v = wasm_wave::from_str::<Val>(&ty, v.as_ref())
                .with_context(|| format!("failed to parse value for parameter `{name}`"))?;
            params.push(v.into());
        }
        ensure!(args.next().is_none(), "trailing arguments");
        let mut results = vec![Val::Bool(false); ty.results().len()];
        let ctx = Ctx {
            cardano,
            ..Ctx::default()
        };
        let (tx, execution) = call_coordination_script(
            &mut Store::new(engine, ctx),
            &GenesisClient(imported),
            &wizer,
            &contract,
            &wasm,
            &script,
            &mut contracts,
            params,
            &mut results,
            &mut Vec::default(),
        )
        .await?;
        if let Err(err) = execution {
            warn!(?err, "failed to check execution trace");
        }
        outputs.extend(tx.outputs);
    }
    Ok(outputs)
}
