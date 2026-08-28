use core::iter::zip;
use core::pin::Pin;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use clap::Args;
use miette::IntoDiagnostic as _;
use sha2::{Digest as _, Sha256};
use starstream_runtime_next::{Contract, ContractLookup, Host, Utxo, bindings};
use tokio::fs;
use tracing::{debug, info, instrument};
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut as _, Store, StoreContextMut, ensure};

/// Run a coordination script exported by a Wasm component
#[derive(Args, Debug)]
pub struct Run {
    /// Path to Wasm component
    wasm: PathBuf,

    /// Script to run
    script: String,

    /// Script arguments
    args: Vec<String>,

    /// Contracts to import
    #[arg(long = "import", value_name = "PATH")]
    imports: Vec<PathBuf>,
}

struct Ctx {
    contract: Contract<Self>,
    table: ResourceTable,
    outputs: Vec<Utxo<Arc<Mutex<UtxoCtx>>>>,
}

#[derive(Debug, Default)]
struct UtxoCtx {
    methods: Vec<(u64, u64, u64, u64)>,
}

#[derive(Default)]
struct Imports(HashMap<String, Contract<Ctx>>);

impl ContractLookup<Ctx> for &Imports {
    fn get_contract(&self, external_id: &str) -> wasmtime::Result<Contract<Ctx>> {
        let contract = self
            .0
            .get(external_id)
            .with_context(|| format!("contract `{external_id}` not found"))?;
        Ok(contract.clone())
    }
}

impl bindings::starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        Ok(0)
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        Ok(0)
    }
}

impl Host for Ctx {
    type UtxoContext = Arc<Mutex<UtxoCtx>>;
    type Token = ();

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    fn contract(store: StoreContextMut<Self>) -> Contract<Self> {
        store.data().contract.clone()
    }

    #[instrument(level = "trace", skip_all, ret)]
    async fn call_utxo_main(
        mut store: StoreContextMut<'_, Self>,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        let cx = Self::UtxoContext::default();
        let utxo = f(store.as_context_mut(), Arc::clone(&cx)).await?;
        let Ctx { outputs, .. } = store.data_mut();
        info!(i = outputs.len(), "constructed UTXO");
        store.data_mut().outputs.push(utxo.clone());
        Ok(utxo)
    }

    #[instrument(level = "debug", skip(store, utxo), ret)]
    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let Ctx { table, .. } = store.data();
        let utxo = table.get(&utxo)?;
        Ok(utxo.context().lock().unwrap().methods.contains(&hash))
    }

    #[instrument(level = "debug", skip_all, ret)]
    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let _utxo = table.delete(utxo)?;
        Ok(())
    }

    #[instrument(level = "debug", skip(store, cx), ret)]
    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        cx.lock().unwrap().methods.push(hash);
        Ok(())
    }

    #[instrument(level = "debug", skip_all, ret)]
    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        cx.lock().unwrap().methods.clear();
        Ok(())
    }

    #[instrument(level = "debug", skip_all, ret)]
    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let _cx = table.delete(cx)?;
        Ok(())
    }

    #[instrument(level = "debug", skip_all, ret)]
    fn drop_token(
        mut store: StoreContextMut<Self>,
        token: Resource<Self::Token>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        () = table.delete(token)?;
        Ok(())
    }

    #[instrument(level = "debug", skip(_store), ret)]
    fn emit_event(
        _store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        let params = Val::Tuple(params.to_vec())
            .to_wave()
            .context("failed to encode parameters")?;
        info!(abi = ?abi_name, event = ?name, params);
        Ok(())
    }
}

async fn exec(
    Run {
        wasm,
        script,
        args,
        imports,
    }: Run,
) -> wasmtime::Result<()> {
    let mut config = wasmtime::Config::new();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config)?;

    let mut lookup = Imports::default();
    for import in imports {
        let wasm = fs::read(&import)
            .await
            .with_context(|| format!("failed to read import contract `{}`", import.display()))?;
        let contract = Contract::new(&engine, &lookup, &wasm)
            .with_context(|| format!("failed to load import contract `{}`", import.display()))?;
        let digest = Sha256::digest(&wasm);
        let digest = format!("{digest:02x}");
        debug!(?digest, path = %import.display(), "imported contract");
        lookup.0.insert(digest, contract);
    }

    let wasm = fs::read(&wasm).await.context("failed to read contract")?;
    let contract = Contract::new(&engine, &lookup, wasm).context("failed to load contract")?;
    let mut store = Store::new(
        &engine,
        Ctx {
            contract: contract.clone(),
            table: ResourceTable::default(),
            outputs: Vec::default(),
        },
    );
    let script = contract.get_coordination_script(&script)?;
    let contract = contract.instantiate(&mut store).await?;
    let params_ty = script.ty().params();
    ensure!(
        params_ty.len() == args.len(),
        "expected {} arguments, got {}",
        params_ty.len(),
        args.len(),
    );
    let params = zip(params_ty, args)
        .map(|((name, ty), arg)| {
            Val::from_wave(&ty, &arg).with_context(|| format!("failed to parse argument `{name}`"))
        })
        .collect::<wasmtime::Result<Vec<_>>>()?;
    let mut results = vec![Val::Bool(false); script.ty().results().len()];
    contract
        .call_coordination_script(&mut store, &script, &params, &mut results)
        .await?;
    debug!(outputs = store.data().outputs.len(), "script returned");
    let results = Val::Tuple(results)
        .to_wave()
        .context("failed to encode results")?;
    println!("{results}");
    Ok(())
}

impl Run {
    pub fn exec(self) -> miette::Result<()> {
        let rt = tokio::runtime::Runtime::new().into_diagnostic()?;
        match rt.block_on(exec(self)) {
            Ok(()) => Ok(()),
            Err(err) => Err(miette::miette!("{err:?}")),
        }
    }
}
