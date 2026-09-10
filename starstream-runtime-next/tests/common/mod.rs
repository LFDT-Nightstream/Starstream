use std::pin::Pin;
use std::sync::{Arc, LazyLock, Mutex};

use sha2::{Digest as _, Sha256};
use starstream_compiler::typecheck::TypecheckSuccess;
use starstream_compiler::{TypecheckFailure, TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::{Contract, ContractLookup, Host, Utxo, bindings};
use starstream_to_wasm::CompileResult;
use tracing::instrument;
use wasmtime::component::{Component, Resource, ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut as _, StoreContextMut, bail, ensure, format_err};
use wit_component::ComponentEncoder;

pub static ENGINE: LazyLock<wasmtime::Engine> = LazyLock::new(|| {
    let mut config = wasmtime::Config::default();
    let config = config.wasm_component_model_implements(true);
    let config = config.wasm_component_model_nested_names(true);
    wasmtime::Engine::new(config).expect("failed to construct engine")
});

pub fn compile_contract(source: &str) -> wasmtime::Result<Component> {
    let (program, errs) = parse_program(source).into_output_errors();
    ensure!(errs.is_empty(), "failed to parse program: {errs:?}");
    let program = program.context("parser did not produce a program")?;

    let TypecheckSuccess { program, .. } = typecheck_program(&program, TypecheckOptions::default())
        .map_err(|TypecheckFailure { errors, .. }| {
            format_err!("failed to typecheck program: {:?}", errors)
        })?;

    let CompileResult { errors, wasm, .. } = starstream_to_wasm::compile(&program);
    ensure!(errors.is_empty(), "failed to compile program: {errors:?}");

    let wasm = wasm.context("compilation did not produce Wasm")?;
    let wasm = ComponentEncoder::default()
        .validate(true)
        .module(&wasm)
        .map_err(wasmtime::error::Error::from_anyhow)
        .context("failed to set core component module")?
        .encode()
        .map_err(wasmtime::error::Error::from_anyhow)
        .context("failed to encode a component")?;
    Component::from_binary(&ENGINE, &wasm).context("failed to compile component")
}

pub fn method_hash(name: &str) -> (u64, u64, u64, u64) {
    let digest = Sha256::digest(name.as_bytes());
    let mut chunks = digest
        .chunks_exact(8)
        .map(|chunk| u64::from_le_bytes(chunk.try_into().unwrap()));
    let hash = (
        chunks.next().unwrap(),
        chunks.next().unwrap(),
        chunks.next().unwrap(),
        chunks.next().unwrap(),
    );
    assert_eq!(chunks.next(), None);
    hash
}

pub struct NoopContractLookup;

impl<T> ContractLookup<T> for NoopContractLookup {
    fn get_contract(&self, contract_id: &str) -> wasmtime::Result<Contract<T>> {
        bail!("contract `{contract_id}` unknown")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Event {
    pub abi_name: Arc<str>,
    pub name: Arc<str>,
    pub params: Box<[Val]>,
}

pub struct Ctx {
    pub table: ResourceTable,
    pub events: Vec<Event>,
    pub outputs: Vec<Utxo<Arc<Mutex<UtxoCtx>>>>,
}

#[derive(Debug, Default)]
pub struct UtxoCtx {
    pub methods: Vec<(u64, u64, u64, u64)>,
    pub dropped: bool,
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
    type Token = (); // TODO: add token support

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

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
        store.data_mut().outputs.push(utxo.clone());
        Ok(utxo)
    }

    #[instrument(skip(store, utxo), ret)]
    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let Ctx { table, .. } = store.data();
        let utxo = table.get(&utxo)?;
        Ok(utxo.context().lock().unwrap().methods.contains(&hash))
    }

    #[instrument(skip(store, utxo), ret)]
    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let _utxo = table.delete(utxo)?;
        Ok(())
    }

    #[instrument(skip(store, cx), ret)]
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

    #[instrument(skip(store, cx), ret)]
    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        cx.lock().unwrap().methods.clear();
        Ok(())
    }

    #[instrument(skip(store, cx), ret)]
    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.delete(cx)?;
        cx.lock().unwrap().dropped = true;
        Ok(())
    }

    #[instrument(skip(store, token), ret)]
    fn drop_token(
        mut store: StoreContextMut<Self>,
        token: Resource<Self::Token>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        () = table.delete(token)?;
        Ok(())
    }

    #[instrument(skip(store), ret)]
    fn emit_event(
        mut store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        let Ctx { events, .. } = store.data_mut();
        events.push(Event {
            abi_name: Arc::clone(abi_name),
            name: Arc::clone(name),
            params: params.into(),
        });
        Ok(())
    }
}
