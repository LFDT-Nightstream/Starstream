use core::array;
use core::iter::zip;

use std::collections::BTreeMap;

use starstream_runtime_next::{
    ConstructorExport, Contract, EventHandler, Host, MethodExport, Utxo, UtxoStorageExport,
    bindings,
};
use test_contracts::EXAMPLE_SCORE;
use wasmtime::bail;
use wasmtime::component::Val;
use wasmtime::error::Context as _;

#[derive(Clone, Debug, Default)]
struct Ctx {
    methods: Vec<(u64, u64, u64, u64)>,
    events: Vec<(String, String, Box<[Val]>)>,
}

impl bindings::starstream::std::builtin::Host for Ctx {
    fn implements_method(&mut self, hash: (u64, u64, u64, u64)) -> wasmtime::Result<()> {
        self.methods.push(hash);
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

const METHODS: [(u64, u64, u64, u64); 4] = [
    (
        9984741223472054651,
        3777191895585416912,
        11414430493733454782,
        17853027721887758589,
    ),
    (
        10031671596645923877,
        5859691412821126436,
        14636224297006305312,
        18400238348769808647,
    ),
    (
        10486967187806387020,
        13073113330981563051,
        14419176846782403470,
        15804774841702521566,
    ),
    (
        5384737671178596162,
        7822374597054249491,
        11449494298339117153,
        15762863519083182186,
    ),
];

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

    let mut constructor_exports = contract.utxo_constructors(&utxo);
    let new = match (constructor_exports.next(), constructor_exports.next()) {
        (Some(("[static]utxo.new", Ok(new))), None) => new,
        exports => bail!("unexpected UTXO constructor exports: {exports:?}"),
    };
    drop(constructor_exports);

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

fn get_progress_storage<T>(
    utxo: &mut Utxo<T>,
    storage: &UtxoStorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .get()
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[cfg(feature = "async")]
async fn get_progress_storage_async<T: Send>(
    utxo: &mut Utxo<T>,
    storage: &UtxoStorageExport,
) -> wasmtime::Result<ProgressStorage> {
    let storage = utxo
        .storage(storage)
        .get_async()
        .await
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[test]
fn score_sync() -> wasmtime::Result<()> {
    let contract = Contract::new(EXAMPLE_SCORE).context("failed to create contract")?;
    let ProgressUtxo {
        storage,
        new,
        finish,
        mult_mult,
        plus_chips,
        plus_mult,
    } = assert_progress_utxo(&contract)?;

    let utxos: [_; 5] = array::from_fn(|_| {
        contract
            .create_utxo(Ctx::default(), &new, [])
            .expect("failed to construct UTXO")
    });

    for (i, mut utxo) in zip(0.., utxos) {
        let Ctx { methods, events } = utxo.store().data();
        assert_eq!(methods.as_ref(), METHODS);
        assert_eq!(events.as_ref(), []);

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut utxo, &storage)?;
        assert_eq!(chips, 0);
        assert_eq!(mult, 0);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call(&plus_chips, [Val::Resource(utxo.resource()), Val::U64(i)])
            .context("failed to call `plus-chips`")?;
        assert!(res.is_empty());

        let res = utxo
            .call(&plus_mult, [Val::Resource(utxo.resource()), Val::U64(i)])
            .context("failed to call `plus-mult`")?;
        assert!(res.is_empty());

        let res = utxo
            .call(&mult_mult, [Val::Resource(utxo.resource()), Val::U64(200)])
            .context("failed to call `mult-mult`")?;
        assert!(res.is_empty());

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage(&mut utxo, &storage)?;
        assert_eq!(chips, i as i64);
        assert_eq!(mult, (i * 2) as i64);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call(&finish, [Val::Resource(utxo.resource())])
            .context("failed to call `finish`")?;
        assert!(res.is_empty());

        let Ctx { methods, events } = utxo.into_inner().context("failed to drop UTXO")?;
        assert_eq!(methods, METHODS);
        assert_eq!(
            events,
            [(
                "score".into(),
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
    let contract = Contract::new(EXAMPLE_SCORE).context("failed to create contract")?;
    let ProgressUtxo {
        storage,
        new,
        finish,
        mult_mult,
        plus_chips,
        plus_mult,
    } = assert_progress_utxo(&contract)?;

    let [utxo0, utxo1, utxo2, utxo3, utxo4] =
        array::from_fn(|_| contract.create_utxo_async(Ctx::default(), &new, []));
    let utxos =
        tokio::try_join!(utxo0, utxo1, utxo2, utxo3, utxo4).context("failed to construct UTXOs")?;

    for (i, mut utxo) in zip(0.., [utxos.0, utxos.1, utxos.2, utxos.3, utxos.4]) {
        let Ctx { methods, events } = utxo.store().data();
        assert_eq!(methods.as_ref(), METHODS);
        assert_eq!(events.as_ref(), []);

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage_async(&mut utxo, &storage).await?;
        assert_eq!(chips, 0);
        assert_eq!(mult, 0);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call_async(&plus_chips, [Val::Resource(utxo.resource()), Val::U64(i)])
            .await
            .context("failed to call `plus-chips`")?;
        assert!(res.is_empty());

        let res = utxo
            .call_async(&plus_mult, [Val::Resource(utxo.resource()), Val::U64(i)])
            .await
            .context("failed to call `plus-mult`")?;
        assert!(res.is_empty());

        let res = utxo
            .call_async(&mult_mult, [Val::Resource(utxo.resource()), Val::U64(200)])
            .await
            .context("failed to call `mult-mult`")?;
        assert!(res.is_empty());

        let ProgressStorage {
            chips,
            mult,
            r#yield,
            yield1,
        } = get_progress_storage_async(&mut utxo, &storage).await?;
        assert_eq!(chips, i as i64);
        assert_eq!(mult, (i * 2) as i64);
        assert_eq!(r#yield, 1);
        assert_eq!(yield1, 1);

        let res = utxo
            .call_async(&finish, [Val::Resource(utxo.resource())])
            .await
            .context("failed to call `finish`")?;
        assert!(res.is_empty());

        let Ctx { methods, events } = utxo
            .into_inner_async()
            .await
            .context("failed to drop UTXO")?;
        assert_eq!(methods, METHODS);
        assert_eq!(
            events,
            [(
                "score".into(),
                "finish".into(),
                [Val::U64(i * i * 2)].into()
            )]
        );
    }
    Ok(())
}
