pub mod common;

use std::collections::BTreeMap;
use std::sync::{Arc, LazyLock, Mutex};

use starstream_runtime_next::{
    Contract, CoordinationScriptExport, Host, MethodExport, StorageExport, Utxo, UtxoExport,
    UtxoMainExport,
};
use tracing::{Instrument as _, info_span, instrument};
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{Store, bail};

use crate::common::{Ctx, Event, NoopContractLookup, UtxoCtx, compile_contract, method_hash};

static CONTRACT: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")));

/// The methods of the `Score` ABI, in declaration (and `yield`) order.
static METHODS: LazyLock<[(u64, u64, u64, u64); 4]> =
    LazyLock::new(|| ["plus_chips", "plus_mult", "mult_mult", "finish"].map(method_hash));

#[derive(Clone)]
struct ProgressUtxo {
    utxo: UtxoExport,
    storage: StorageExport,
    new: UtxoMainExport,
    finish: MethodExport,
    mult_mult: MethodExport,
    plus_chips: MethodExport,
    plus_mult: MethodExport,
    example: CoordinationScriptExport,
}

fn assert_progress_utxo<T: Host>(contract: &Contract<T>) -> wasmtime::Result<ProgressUtxo> {
    if let Some(export) = contract.tokens().next() {
        bail!("unexpected token export: {export:?}")
    }

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
        utxo: utxo.clone(),
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

impl From<ProgressStorage> for Vec<(String, Val)> {
    fn from(
        ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
            yield1_v1,
        }: ProgressStorage,
    ) -> Self {
        vec![
            ("yield".into(), Val::S32(r#yield)),
            ("chips".into(), Val::S64(chips)),
            ("mult".into(), Val::S64(mult)),
            ("yield1".into(), Val::S32(yield1)),
            ("yield1-v1".into(), Val::S32(yield1_v1)),
        ]
    }
}

async fn get_progress_storage<T>(
    store: &mut Store<impl Send + 'static>,
    utxo: &Utxo<T>,
    storage: &StorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .call_get(store)
        .await
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

async fn assert_call_method(
    mut store: &mut Store<Ctx>,
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    utxo_export: &UtxoExport,
    storage_export: &StorageExport,
    method_export: &MethodExport,
    params: &[Val],
    expected_drop: bool,
    expected_storage: &ProgressStorage,
    expected_events: &[Event],
) -> wasmtime::Result<()> {
    let old_storage = get_progress_storage(store, utxo, storage_export).await?;
    utxo.call_method(
        &mut store,
        method_export,
        [[Val::Resource(utxo.resource())].as_slice(), params].concat(),
        [],
    )
    .await
    .context("failed to call method")?;
    let new_storage = get_progress_storage(store, utxo, storage_export).await?;
    assert_eq!(new_storage, *expected_storage);

    let mut table = ResourceTable::default();
    let utxo_cx = Arc::new(Mutex::new(UtxoCtx::default()));
    let utxo_cx_res = table.push(Arc::clone(&utxo_cx))?;
    let mut store = wasmtime::Store::new(
        store.engine(),
        Ctx {
            contract: contract.clone(),
            table,
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let utxo_cx_rep = utxo_cx_res.rep();
    let utxo_cx_res = utxo_cx_res.try_into_resource_any(&mut store)?;
    let instance = contract.instantiate(&mut store).await?;
    let utxo = instance
        .load_utxo(
            &mut store,
            utxo_export,
            storage_export,
            utxo_cx.clone(),
            [Val::Resource(utxo_cx_res), Val::Record(old_storage.into())],
        )
        .await?;
    utxo.call_method(
        &mut store,
        method_export,
        [[Val::Resource(utxo.resource())].as_slice(), params].concat(),
        [],
    )
    .await
    .context("failed to call method on loaded UTXO")?;
    let loaded_storage = get_progress_storage(&mut store, &utxo, storage_export).await?;
    assert_eq!(loaded_storage, new_storage);

    utxo.drop(&mut store)
        .await
        .context("failed to drop loaded UTXO")?;

    let Ctx {
        mut table,
        events,
        outputs,
        ..
    } = store.into_data();
    assert!(outputs.is_empty());
    assert_eq!(events, expected_events);
    let utxo_cx = if expected_drop {
        utxo_cx
    } else {
        drop(utxo_cx);
        table.delete(Resource::new_own(utxo_cx_rep))?
    };
    assert!(table.is_empty());
    let UtxoCtx { methods, dropped } = Arc::into_inner(utxo_cx)
        .expect("UTXO context must have been dropped")
        .into_inner()
        .unwrap();
    assert_eq!(methods, []);
    assert_eq!(dropped, expected_drop);
    Ok(())
}

#[instrument(skip(store, contract, utxo, ty), ret)]
async fn assert_call_plus_chips(
    store: &mut Store<Ctx>,
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    ty: &ProgressUtxo,
    v: u64,
    expected_storage: &ProgressStorage,
) -> wasmtime::Result<()> {
    assert_call_method(
        store,
        contract,
        utxo,
        &ty.utxo,
        &ty.storage,
        &ty.plus_chips,
        &[Val::U64(v)],
        false,
        expected_storage,
        &[],
    )
    .await
}

#[instrument(skip(store, contract, utxo, ty), ret)]
async fn assert_call_plus_mult(
    store: &mut Store<Ctx>,
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    ty: &ProgressUtxo,
    v: u64,
    expected_storage: &ProgressStorage,
) -> wasmtime::Result<()> {
    assert_call_method(
        store,
        contract,
        utxo,
        &ty.utxo,
        &ty.storage,
        &ty.plus_mult,
        &[Val::U64(v)],
        false,
        expected_storage,
        &[],
    )
    .await
}

#[instrument(skip(store, contract, utxo, ty), ret)]
async fn assert_call_mult_mult(
    store: &mut Store<Ctx>,
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    ty: &ProgressUtxo,
    v: u64,
    expected_storage: &ProgressStorage,
) -> wasmtime::Result<()> {
    assert_call_method(
        store,
        contract,
        utxo,
        &ty.utxo,
        &ty.storage,
        &ty.mult_mult,
        &[Val::U64(v)],
        false,
        expected_storage,
        &[],
    )
    .await
}

#[instrument(skip(store, contract, utxo, ty), ret)]
async fn assert_call_finish(
    store: &mut Store<Ctx>,
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    ty: &ProgressUtxo,
    expected_storage: &ProgressStorage,
) -> wasmtime::Result<()> {
    assert_call_method(
        store,
        contract,
        utxo,
        &ty.utxo,
        &ty.storage,
        &ty.finish,
        &[],
        true,
        expected_storage,
        &[Event {
            abi_name: "score".into(),
            name: "finish".into(),
            params: [Val::U64(
                (expected_storage.chips * expected_storage.mult) as _,
            )]
            .into(),
        }],
    )
    .await
}

#[test_log::test(tokio::test)]
async fn score_main_new() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract = Contract::new(&engine, NoopContractLookup, CONTRACT.as_slice())
        .context("failed to create contract")?;
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
        .call_utxo_main(
            &mut store,
            &ty.utxo,
            &ty.new,
            Arc::clone(&utxo_cx),
            [Val::Resource(utxo_cx_res)],
        )
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

    assert_call_plus_chips(
        &mut store,
        &contract,
        &utxo,
        &ty,
        1,
        &ProgressStorage {
            chips: 1,
            mult,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;
    assert_call_plus_chips(
        &mut store,
        &contract,
        &utxo,
        &ty,
        2,
        &ProgressStorage {
            chips: 3,
            mult,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;

    assert_call_plus_mult(
        &mut store,
        &contract,
        &utxo,
        &ty,
        3,
        &ProgressStorage {
            chips: 3,
            mult: 3,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;
    assert_call_plus_mult(
        &mut store,
        &contract,
        &utxo,
        &ty,
        1,
        &ProgressStorage {
            chips: 3,
            mult: 4,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;

    assert_call_mult_mult(
        &mut store,
        &contract,
        &utxo,
        &ty,
        100,
        &ProgressStorage {
            chips: 3,
            mult: 4,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;

    assert_call_mult_mult(
        &mut store,
        &contract,
        &utxo,
        &ty,
        200,
        &ProgressStorage {
            chips: 3,
            mult: 8,
            r#yield,
            yield1,
            yield1_v1,
        },
    )
    .await?;

    assert_call_finish(
        &mut store,
        &contract,
        &utxo,
        &ty,
        &ProgressStorage {
            chips: 3,
            mult: 8,
            r#yield,
            yield1: 0,
            yield1_v1: 0,
        },
    )
    .await?;

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
async fn score_script_example() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract = Contract::new(&engine, NoopContractLookup, CONTRACT.as_slice())
        .context("failed to create contract")?;
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
    let utxo = match (outputs.next(), outputs.next()) {
        (Some(utxo), None) => utxo.clone(),
        _ => bail!("unexpected outputs created: {outputs:?}"),
    };
    {
        let utxo_cx = utxo.context().lock().unwrap();
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
