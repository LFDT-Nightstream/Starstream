use std::collections::BTreeMap;
use std::pin::Pin;
use std::sync::{Arc, LazyLock, Mutex};

use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::{
    Contract, CoordinationScriptExport, Host, MethodExport, Utxo, UtxoMainExport,
    UtxoStorageExport, bindings,
};
use starstream_to_wasm::compile;
use tracing::{Instrument as _, info_span, instrument};
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut as _, Store, StoreContextMut, bail};

/// Compile a single Starstream contract source to a core Wasm module in-process.
///
/// Mirrors the browser sandbox's compile path: parse the source, typecheck it,
/// then emit the contract. The result carries a `component-type` custom section,
/// so `Contract::new`'s `componentize` step wraps it into a component at run
/// time.
fn compile_contract(source: &str) -> Vec<u8> {
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

static EXAMPLE_SCORE: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")));

#[derive(Debug, Eq, PartialEq)]
struct Event {
    abi_name: Arc<str>,
    name: Arc<str>,
    params: Box<[Val]>,
}

struct Ctx {
    contract: Contract<Self>,

    table: ResourceTable,
    events: Vec<Event>,

    outputs: Vec<(Utxo, Arc<Mutex<UtxoCtx>>)>,
}

#[derive(Debug, Default)]
pub struct UtxoCtx {
    methods: Vec<(u64, u64, u64, u64)>,
    dropped: bool,
}

impl bindings::starstream::std::builtin::Host for Ctx {}

impl bindings::starstream::std::builtin::HostUtxo for Ctx {
    fn drop(&mut self, _utxo: Resource<Utxo>) -> wasmtime::Result<()> {
        Ok(())
    }

    fn has_method(
        &mut self,
        _utxo: Resource<Utxo>,
        _hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        // TODO
        Ok(false)
    }
}

impl bindings::starstream::std::builtin::HostToken for Ctx {
    fn drop(
        &mut self,
        _token: Resource<bindings::starstream::std::builtin::Token>,
    ) -> wasmtime::Result<()> {
        Ok(())
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
        )
            -> Pin<Box<dyn Future<Output = wasmtime::Result<Utxo>> + Send + 'a>>
        + Send,
    ) -> wasmtime::Result<Utxo> {
        let cx = Self::UtxoContext::default();
        let utxo = f(store.as_context_mut(), Arc::clone(&cx)).await?;
        store.data_mut().outputs.push((utxo, cx));
        Ok(utxo)
    }

    #[instrument(skip(store, cx), ret)]
    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get_mut(&cx)?;
        cx.lock().unwrap().methods.push(hash);
        Ok(())
    }

    #[instrument(skip(store, cx), ret)]
    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get_mut(&cx)?;
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

    #[instrument(skip(store), ret)]
    fn emit_event(
        mut store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        store.data_mut().events.push(Event {
            abi_name: Arc::clone(abi_name),
            name: Arc::clone(name),
            params: params.into(),
        });
        Ok(())
    }
}

/// The ABI method identity hash the guest reports via `implements-method`:
/// the SHA-256 of the method name, split into four little-endian `u64`s. Mirrors
/// the codegen in `starstream-to-wasm`.
fn method_hash(name: &str) -> (u64, u64, u64, u64) {
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

/// The methods of the `Score` ABI, in declaration (and `yield`) order.
static METHODS: LazyLock<[(u64, u64, u64, u64); 4]> =
    LazyLock::new(|| ["plus_chips", "plus_mult", "mult_mult", "finish"].map(method_hash));

#[derive(Clone)]
struct ProgressUtxo {
    storage: UtxoStorageExport,
    new: UtxoMainExport,
    finish: MethodExport,
    mult_mult: MethodExport,
    plus_chips: MethodExport,
    plus_mult: MethodExport,
    example: CoordinationScriptExport,
}

fn assert_progress_utxo<T: Host>(contract: &Contract<T>) -> wasmtime::Result<ProgressUtxo> {
    let mut utxo_exports = contract.utxos();
    let utxo = match (utxo_exports.next(), utxo_exports.next()) {
        (Some(("score-progress", Ok(utxo))), None) => utxo,
        exports => bail!("unexpected UTXO exports: {exports:?}"),
    };
    let _named = contract
        .get_utxo("score-progress")
        .context("failed to get `score-progress` UTXO export by name")?;

    let new = {
        let mut exports = contract.utxo_mains(&utxo);
        match (exports.next(), exports.next()) {
            (Some(("[static]utxo.new", Ok(new))), None) => new,
            exports => bail!("unexpected UTXO main exports: {exports:?}"),
        }
    };

    let _named = contract
        .get_utxo_main(&utxo, "[static]utxo.new")
        .context("failed to get `[static]utxo.new` UTXO main export by name")?;

    let methods = contract.utxo_methods(&utxo);
    let methods: BTreeMap<_, _> = methods
        .map(|(name, export)| export.map(|export| (String::from(name), export)))
        .collect::<wasmtime::Result<_>>()
        .context("failed to iterate methods")?;
    assert_eq!(
        methods.keys().collect::<Vec<_>>(),
        [
            "[method]utxo.finish",
            "[method]utxo.mult-mult",
            "[method]utxo.plus-chips",
            "[method]utxo.plus-mult"
        ]
    );
    for name in methods.keys() {
        let _named = contract
            .get_utxo_method(&utxo, name)
            .with_context(|| format!("failed to get `{name}` UTXO method export by name"))?;
    }

    let example = {
        let mut exports = contract.coordination_scripts();
        match (exports.next(), exports.next()) {
            (Some(("example", Ok(example))), None) => example,
            exports => bail!("unexpected UTXO coordination script exports: {exports:?}"),
        }
    };

    let _named = contract
        .get_coordination_script("example")
        .context("failed to get `example` UTXO coordination script export by name")?;

    let storage = utxo.storage().context("failed to lookup storage export")?;
    Ok(ProgressUtxo {
        storage: storage.clone(),
        new,
        finish: methods["[method]utxo.finish"].clone(),
        mult_mult: methods["[method]utxo.mult-mult"].clone(),
        plus_chips: methods["[method]utxo.plus-chips"].clone(),
        plus_mult: methods["[method]utxo.plus-mult"].clone(),
        example,
    })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct ProgressStorage {
    chips: i64,
    mult: i64,
    r#yield: i32,
    yield1: i32,
    yield1_v1: i32,
}

impl<'a> FromIterator<&'a (String, Val)> for ProgressStorage {
    fn from_iter<T: IntoIterator<Item = &'a (String, Val)>>(fields: T) -> Self {
        let mut fields = fields.into_iter().map(|(k, v)| (k.as_str(), v));
        match (
            fields.next(),
            fields.next(),
            fields.next(),
            fields.next(),
            fields.next(),
            fields.next(),
        ) {
            (
                Some(("yield", Val::S32(r#yield))),
                Some(("chips", Val::S64(chips))),
                Some(("mult", Val::S64(mult))),
                Some(("yield1", Val::S32(yield1))),
                Some(("yield1-v1", Val::S32(yield1_v1))),
                None,
            ) => ProgressStorage {
                chips: *chips,
                mult: *mult,
                r#yield: *r#yield,
                yield1: *yield1,
                yield1_v1: *yield1_v1,
            },
            fields => panic!("unexpected progress UTXO storage fields: {fields:?}"),
        }
    }
}

async fn get_progress_storage<T: Send + 'static>(
    store: &mut Store<T>,
    utxo: &Utxo,
    storage: &UtxoStorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .get(store)
        .await
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[test_log::test(tokio::test)]
#[ignore] // Due to "dynamic UTXO imports unsupported"
async fn score_main_new() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract =
        Contract::new(&engine, EXAMPLE_SCORE.as_slice()).context("failed to create contract")?;
    let ty = assert_progress_utxo(&contract)?;

    let mut table = ResourceTable::default();
    let utxo_cx = Arc::new(Mutex::new(UtxoCtx::default()));
    let utxo_cx_res = table.push(Arc::clone(&utxo_cx))?;
    let mut store = wasmtime::Store::new(
        &engine,
        Ctx {
            contract: contract.clone(),
            table,
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let utxo_cx_res = utxo_cx_res.try_into_resource_any(&mut store)?;
    let instance = contract.instantiate(&mut store).await?;
    let utxo = instance
        .call_utxo_main(&mut store, &ty.new, [Val::Resource(utxo_cx_res)])
        .instrument(info_span!("new"))
        .await
        .context("failed to construct UTXO")?;

    let Ctx { events, .. } = store.data();
    assert_eq!(events.as_ref(), []);
    assert_eq!(utxo_cx.lock().unwrap().methods, *METHODS);

    let ProgressStorage {
        chips,
        mult,
        r#yield,
        yield1,
        yield1_v1,
    } = get_progress_storage(&mut store, &utxo, &ty.storage).await?;
    assert_eq!(chips, 0);
    assert_eq!(mult, 0);
    assert_eq!(r#yield, 1);
    assert_eq!(yield1, 1);
    assert_eq!(yield1_v1, 2);

    utxo.call(
        &mut store,
        &ty.plus_chips,
        [Val::Resource(utxo.resource()), Val::U64(3)],
        [],
    )
    .instrument(info_span!("plus-chips"))
    .await
    .context("failed to call `plus-chips`")?;

    utxo.call(
        &mut store,
        &ty.plus_mult,
        [Val::Resource(utxo.resource()), Val::U64(4)],
        [],
    )
    .instrument(info_span!("plus-mult"))
    .await
    .context("failed to call `plus-mult`")?;

    utxo.call(
        &mut store,
        &ty.mult_mult,
        [Val::Resource(utxo.resource()), Val::U64(200)],
        [],
    )
    .instrument(info_span!("mult-mult"))
    .await
    .context("failed to call `mult-mult`")?;

    let ProgressStorage {
        chips,
        mult,
        r#yield,
        yield1,
        yield1_v1,
    } = get_progress_storage(&mut store, &utxo, &ty.storage).await?;
    assert_eq!(chips, 3);
    assert_eq!(mult, 4 * 2);
    assert_eq!(r#yield, 1);
    assert_eq!(yield1, 1);
    assert_eq!(yield1_v1, 2);

    utxo.call(&mut store, &ty.finish, [Val::Resource(utxo.resource())], [])
        .instrument(info_span!("finish"))
        .await
        .context("failed to call `finish`")?;

    utxo.drop(&mut store).await.context("failed to drop UTXO")?;

    let Ctx {
        table,
        events,
        outputs,
        ..
    } = store.into_data();
    assert!(outputs.is_empty());
    assert!(table.is_empty());
    assert_eq!(
        events,
        [Event {
            abi_name: "score".into(),
            name: "finish".into(),
            params: [Val::U64(3 * 4 * 2)].into()
        }]
    );
    let UtxoCtx { methods, dropped } = Arc::into_inner(utxo_cx)
        .expect("UTXO context must have been dropped")
        .into_inner()
        .unwrap();
    assert_eq!(methods, []);
    assert!(dropped);
    Ok(())
}

#[test_log::test(tokio::test)]
#[ignore] // Due to "dynamic UTXO imports unsupported"
async fn score_script_example() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract =
        Contract::new(&engine, EXAMPLE_SCORE.as_slice()).context("failed to create contract")?;
    let ty = assert_progress_utxo(&contract)?;

    let mut store = wasmtime::Store::new(
        &engine,
        Ctx {
            contract: contract.clone(),
            table: ResourceTable::default(),
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let instance = contract
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;
    instance
        .call_coordination_script(&mut store, &ty.example, [], [])
        .instrument(info_span!("example"))
        .await
        .context("failed to call `example` coordination script")?;

    let Ctx {
        events, outputs, ..
    } = store.data();
    let mut outputs = outputs.iter();
    let (utxo, utxo_cx) = match (outputs.next(), outputs.next()) {
        (Some((utxo, utxo_cx)), None) => (*utxo, utxo_cx),
        _ => bail!("unexpected outputs created: {outputs:?}"),
    };
    {
        let utxo_cx = utxo_cx.lock().unwrap();
        assert_eq!(utxo_cx.methods, *METHODS);
        assert!(!utxo_cx.dropped);
    }
    assert!(events.is_empty());

    let ProgressStorage {
        chips,
        mult,
        r#yield,
        yield1,
        yield1_v1,
    } = get_progress_storage(&mut store, &utxo, &ty.storage).await?;
    assert_eq!(chips, 42);
    assert_eq!(mult, 4);
    assert_eq!(r#yield, 1);
    assert_eq!(yield1, 1);
    assert_eq!(yield1_v1, 2);

    utxo.drop(&mut store).await.context("failed to drop UTXO")?;

    Ok(())
}
