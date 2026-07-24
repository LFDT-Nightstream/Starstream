use core::array;
use core::iter::zip;

use std::collections::BTreeMap;
use std::sync::LazyLock;

use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_runtime_next::{
    ConstructorExport, Contract, EventHandler, Host, MethodExport, Utxo, UtxoHandler,
    UtxoStorageExport, bindings, new_wasmtime_config,
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

#[derive(Debug, Default)]
struct Ctx {
    table: ResourceTable,
    methods: Vec<(u64, u64, u64, u64)>,
    events: Vec<(String, String, Box<[Val]>)>,
}

impl bindings::starstream::std::builtin::Host for Ctx {
    fn implements_method(&mut self, hash: (u64, u64, u64, u64)) -> wasmtime::Result<()> {
        self.methods.push(hash);
        Ok(())
    }
}

impl bindings::starstream::std::builtin::HostUtxo for Ctx {
    fn drop(&mut self, _utxo: Resource<Utxo>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl bindings::starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> i64 {
        0
    }

    fn current_slot(&mut self) -> i64 {
        0
    }
}

impl EventHandler for Ctx {
    fn emit_event(&mut self, instance: &str, name: &str, params: &[Val]) {
        self.events
            .push((instance.into(), name.into(), params.into()));
    }
}

impl UtxoHandler for Ctx {
    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn construct_utxo(
        _store: wasmtime::StoreContextMut<'_, Self>,
        _instance: &str,
        _name: &str,
        _params: &[Val],
    ) -> wasmtime::Result<Utxo>
    where
        Self: Sized,
    {
        bail!("UTXO construction not supported yet")
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

struct ProgressUtxo {
    storage: UtxoStorageExport,
    new: ConstructorExport,
    finish: MethodExport,
    mult_mult: MethodExport,
    plus_chips: MethodExport,
    plus_mult: MethodExport,
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

    let storage = utxo.storage().context("failed to lookup storage export")?;
    Ok(ProgressUtxo {
        storage: storage.clone(),
        new,
        finish: methods["[method]utxo.finish"].clone(),
        mult_mult: methods["[method]utxo.mult-mult"].clone(),
        plus_chips: methods["[method]utxo.plus-chips"].clone(),
        plus_mult: methods["[method]utxo.plus-mult"].clone(),
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

fn get_progress_storage<T: 'static>(
    store: &mut Store<T>,
    utxo: &Utxo,
    storage: &UtxoStorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .get(store)
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[cfg(feature = "async")]
async fn get_progress_storage_async<T: Send + 'static>(
    store: &mut Store<T>,
    utxo: &Utxo,
    storage: &UtxoStorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .get_async(store)
        .await
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[test]
fn score_sync() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::new(&new_wasmtime_config())?;
    let contract =
        Contract::new(&engine, EXAMPLE_SCORE.as_slice()).context("failed to create contract")?;
    let ProgressUtxo {
        storage,
        new,
        finish,
        mult_mult,
        plus_chips,
        plus_mult,
    } = assert_progress_utxo(&contract)?;

    let utxos: [_; 5] = array::from_fn(|_| {
        let mut store = Store::new(&engine, Ctx::default());
        let utxo = contract
            .create_utxo(&mut store, &new, [])
            .expect("failed to construct UTXO");
        (store, utxo)
    });

    for (i, (mut store, utxo)) in zip(0.., utxos) {
        let Ctx {
            methods, events, ..
        } = store.data();
        assert_eq!(methods.as_ref(), *METHODS);
        assert_eq!(events.as_ref(), []);

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut store, &utxo, &storage)?;
        assert_eq!(chips, 0);
        assert_eq!(mult, 0);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call(
                &mut store,
                &plus_chips,
                [Val::Resource(utxo.resource()), Val::U64(i)],
            )
            .context("failed to call `plus-chips`")?;
        assert!(res.is_empty());

        let res = utxo
            .call(
                &mut store,
                &plus_mult,
                [Val::Resource(utxo.resource()), Val::U64(i)],
            )
            .context("failed to call `plus-mult`")?;
        assert!(res.is_empty());

        let res = utxo
            .call(
                &mut store,
                &mult_mult,
                [Val::Resource(utxo.resource()), Val::U64(200)],
            )
            .context("failed to call `mult-mult`")?;
        assert!(res.is_empty());

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut store, &utxo, &storage)?;
        assert_eq!(chips, i as i64);
        assert_eq!(mult, (i * 2) as i64);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call(&mut store, &finish, [Val::Resource(utxo.resource())])
            .context("failed to call `finish`")?;
        assert!(res.is_empty());

        utxo.drop(&mut store).context("failed to drop UTXO")?;
        let Ctx {
            methods, events, ..
        } = store.into_data();
        assert_eq!(methods, *METHODS);
        assert_eq!(
            events,
            [(
                "starstream:events/score".into(),
                "finish".into(),
                [Val::U64(i * i * 2)].into()
            )]
        );
    }
    Ok(())
}

#[cfg(feature = "async")]
#[tokio::test]
async fn score_async() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::new(&new_wasmtime_config())?;
    let contract =
        Contract::new(&engine, EXAMPLE_SCORE.as_slice()).context("failed to create contract")?;
    let ProgressUtxo {
        storage,
        new,
        finish,
        mult_mult,
        plus_chips,
        plus_mult,
    } = assert_progress_utxo(&contract)?;

    let [utxo0, utxo1, utxo2, utxo3, utxo4] = array::from_fn(|_| {
        let mut store = Store::new(&engine, Ctx::default());
        async {
            contract
                .create_utxo_async(&mut store, &new, [])
                .await
                .map(|utxo| (store, utxo))
        }
    });
    let (utxo0, utxo1, utxo2, utxo3, utxo4) =
        tokio::try_join!(utxo0, utxo1, utxo2, utxo3, utxo4).context("failed to construct UTXOs")?;

    for (i, (mut store, utxo)) in zip(0.., [utxo0, utxo1, utxo2, utxo3, utxo4]) {
        let Ctx {
            methods, events, ..
        } = store.data();
        assert_eq!(methods.as_ref(), *METHODS);
        assert_eq!(events.as_ref(), []);

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage_async(&mut store, &utxo, &storage).await?;
        assert_eq!(chips, 0);
        assert_eq!(mult, 0);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call_async(
                &mut store,
                &plus_chips,
                [Val::Resource(utxo.resource()), Val::U64(i)],
            )
            .await
            .context("failed to call `plus-chips`")?;
        assert!(res.is_empty());

        let res = utxo
            .call_async(
                &mut store,
                &plus_mult,
                [Val::Resource(utxo.resource()), Val::U64(i)],
            )
            .await
            .context("failed to call `plus-mult`")?;
        assert!(res.is_empty());

        let res = utxo
            .call_async(
                &mut store,
                &mult_mult,
                [Val::Resource(utxo.resource()), Val::U64(200)],
            )
            .await
            .context("failed to call `mult-mult`")?;
        assert!(res.is_empty());

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage_async(&mut store, &utxo, &storage).await?;
        assert_eq!(chips, i as i64);
        assert_eq!(mult, (i * 2) as i64);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call_async(&mut store, &finish, [Val::Resource(utxo.resource())])
            .await
            .context("failed to call `finish`")?;
        assert!(res.is_empty());

        utxo.drop_async(&mut store)
            .await
            .context("failed to drop UTXO")?;
        let Ctx {
            methods, events, ..
        } = store.into_data();
        assert_eq!(methods, *METHODS);
        assert_eq!(
            events,
            [(
                "starstream:events/score".into(),
                "finish".into(),
                [Val::U64(i * i * 2)].into()
            )]
        );
    }
    Ok(())
}
