use std::pin::Pin;
use std::sync::{Arc, Mutex};

use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::{Contract, ContractLookup, Host, Utxo, bindings};
use starstream_to_wasm::compile;
use tracing::instrument;
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::{AsContextMut as _, StoreContextMut, bail};

/// Compile a single Starstream contract source to a core Wasm module in-process.
///
/// Mirrors the browser sandbox's compile path: parse the source, typecheck it,
/// then emit the contract. The result carries a `component-type` custom section,
/// so `Contract::new`'s `componentize` step wraps it into a component at run
/// time.
pub fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = typecheck_program(&program, TypecheckOptions::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    result.wasm.expect("compiling produced no Wasm")
}

/// The ABI method identity hash the guest reports via `implements-method`:
/// the SHA-256 of the method name, split into four little-endian `u64`s. Mirrors
/// the codegen in `starstream-to-wasm`.
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
    fn get_contract(&self, external_id: &str) -> wasmtime::Result<Contract<T>> {
        bail!("contract with external_id `{external_id}` unknown")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Event {
    pub abi_name: Arc<str>,
    pub name: Arc<str>,
    pub params: Box<[Val]>,
}

pub struct Ctx {
    pub contract: Contract<Self>,

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

    fn contract(store: StoreContextMut<Self>) -> Contract<Self> {
        store.data().contract.clone()
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
