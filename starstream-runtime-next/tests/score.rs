use core::array;
use core::iter::zip;

use std::collections::BTreeMap;
use std::sync::{Arc, LazyLock};

use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::{
    ConstructorExport, Contract, CoordinationScriptExport, EventHandler, Host, MethodExport,
    ResourceView, Utxo, UtxoHandler, UtxoStorageExport, bindings,
};
use starstream_to_wasm::compile;
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{Store, bail};

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

struct Ctx {
    contract: Contract<Self>,
    ty: ProgressUtxo,

    table: ResourceTable,
    events: Vec<(String, String, Box<[Val]>)>,

    utxo_cx: Option<UtxoCtx>,
}

pub struct UtxoCtx {
    methods: Vec<(u64, u64, u64, u64)>,
}

impl ResourceView for Ctx {
    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }
}

impl bindings::starstream::std::builtin::Host for Ctx {}

impl bindings::starstream::std::builtin::HostUtxo for Ctx {
    fn drop(&mut self, _utxo: Resource<Utxo>) -> wasmtime::Result<()> {
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

impl EventHandler for Ctx {
    fn emit_event(&mut self, instance: &str, name: &str, params: &[Val]) {
        self.events
            .push((instance.into(), name.into(), params.into()));
    }
}

impl UtxoHandler for Ctx {
    type Context = UtxoCtx;

    async fn construct_utxo(
        mut store: wasmtime::StoreContextMut<'_, Self>,
        instance: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<Utxo>
    where
        Self: Sized,
    {
        match (instance.as_ref(), name.as_ref()) {
            ("score-progress", "[static]utxo.new") => {
                let Ctx { contract, ty, .. } = store.data();
                let contract = contract.clone();
                let ty = ty.clone();
                let instance = contract.instantiate(&mut store).await?;
                instance.create_utxo(store, &ty.new, params).await
            }
            _ => panic!("unexpected UTXO constructor call `{instance}#{name}`"),
        }
    }

    fn resume(
        mut store: wasmtime::StoreContextMut<'_, Self>,
        cx: Resource<UtxoCtx>,
    ) -> wasmtime::Result<()> {
        let _cx = store.data_mut().table.get_mut(&cx)?;
        // TODO: handle resume
        Ok(())
    }

    fn implements_method(
        mut store: wasmtime::StoreContextMut<'_, Self>,
        cx: Resource<UtxoCtx>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get_mut(&cx)?;
        cx.methods.push(hash);
        Ok(())
    }

    fn drop(
        mut store: wasmtime::StoreContextMut<'_, Self>,
        cx: Resource<UtxoCtx>,
    ) -> wasmtime::Result<()> {
        let data = store.data_mut();
        let cx = data.table.delete(cx)?;
        assert!(data.utxo_cx.replace(cx).is_none());
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
    new: ConstructorExport,
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
        let mut exports = contract.utxo_constructors(&utxo);
        match (exports.next(), exports.next()) {
            (Some(("[static]utxo.new", Ok(new))), None) => new,
            exports => bail!("unexpected UTXO constructor exports: {exports:?}"),
        }
    };

    let _named = contract
        .get_utxo_constructor(&utxo, "[static]utxo.new")
        .context("failed to get `[static]utxo.new` UTXO constructor export by name")?;

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
        ) {
            (
                Some(("yield", Val::S32(r#yield))),
                Some(("chips", Val::S64(chips))),
                Some(("mult", Val::S64(mult))),
                Some(("yield1", Val::S32(yield1))),
                None,
            ) => ProgressStorage {
                chips: *chips,
                mult: *mult,
                r#yield: *r#yield,
                yield1: *yield1,
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

#[ignore = "compiler does not support the new WIT shape yet"]
#[tokio::test]
async fn score() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract =
        Contract::new(&engine, EXAMPLE_SCORE.as_slice()).context("failed to create contract")?;
    let ty = assert_progress_utxo(&contract)?;

    let [utxo0, utxo1, utxo2, utxo3, utxo4] = array::from_fn(|_| async {
        let mut store = wasmtime::Store::new(
            &engine,
            Ctx {
                contract: contract.clone(),
                ty: ty.clone(),
                table: ResourceTable::default(),
                utxo_cx: None,
                events: Vec::default(),
            },
        );
        let instance = contract.instantiate(&mut store).await?;
        instance
            .create_utxo(&mut store, &ty.new, [])
            .await
            .map(|utxo| (store, utxo))
    });
    let (utxo0, utxo1, utxo2, utxo3, utxo4) =
        tokio::try_join!(utxo0, utxo1, utxo2, utxo3, utxo4).context("failed to construct UTXOs")?;

    for (i, (mut store, utxo)) in zip(0.., [utxo0, utxo1, utxo2, utxo3, utxo4]) {
        let Ctx {
            events, utxo_cx, ..
        } = store.data();
        assert_eq!(events.as_ref(), []);
        assert!(utxo_cx.is_none());

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut store, &utxo, &ty.storage).await?;
        assert_eq!(chips, 0);
        assert_eq!(mult, 0);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        utxo.call(
            &mut store,
            &ty.plus_chips,
            [Val::Resource(utxo.resource()), Val::U64(i)],
            [],
        )
        .await
        .context("failed to call `plus-chips`")?;

        utxo.call(
            &mut store,
            &ty.plus_mult,
            [Val::Resource(utxo.resource()), Val::U64(i)],
            [],
        )
        .await
        .context("failed to call `plus-mult`")?;

        utxo.call(
            &mut store,
            &ty.mult_mult,
            [Val::Resource(utxo.resource()), Val::U64(200)],
            [],
        )
        .await
        .context("failed to call `mult-mult`")?;

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut store, &utxo, &ty.storage).await?;
        assert_eq!(chips, i as i64);
        assert_eq!(mult, (i * 2) as i64);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        utxo.call(&mut store, &ty.finish, [Val::Resource(utxo.resource())], [])
            .await
            .context("failed to call `finish`")?;

        utxo.drop(&mut store).await.context("failed to drop UTXO")?;
        let Ctx {
            table,
            events,
            utxo_cx,
            ..
        } = store.into_data();
        assert!(table.is_empty());
        assert!(utxo_cx.is_none());
        // TODO: Assert methods on the context
        //assert_eq!(methods, *METHODS);
        assert_eq!(
            events,
            [(
                "starstream:events/score".into(),
                "finish".into(),
                [Val::U64(i * i * 2)].into()
            )]
        );
        assert!(utxo_cx.is_none());
    }

    let mut store = wasmtime::Store::new(
        &engine,
        Ctx {
            contract: contract.clone(),
            ty: ty.clone(),
            table: ResourceTable::default(),
            events: Vec::default(),
            utxo_cx: None,
        },
    );
    let instance = contract
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;
    instance
        .call_coordination_script(&mut store, &ty.example, [], [])
        .await
        .context("failed to call `example` coordination script")?;
    let data = store.data_mut();
    let utxo = {
        let mut resources = data.table.iter_mut();
        match (resources.next(), resources.next()) {
            (Some(utxo), None) => utxo,
            _ => bail!("unexpected resources in table"),
        }
    };
    let UtxoCtx { methods } = data.utxo_cx.take().context("UTXO context missing")?;
    assert_eq!(methods.as_slice(), *METHODS);
    assert!(data.events.is_empty());
    let utxo = utxo
        .downcast_mut::<Utxo>()
        .context("failed to downcast UTXO")
        .copied()?;
    utxo.drop(&mut store).await.context("failed to drop UTXO")?;
    Ok(())
}
