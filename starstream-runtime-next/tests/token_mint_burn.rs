pub mod common;

use std::collections::BTreeMap;
use std::sync::LazyLock;

use starstream_runtime_next::{Contract, Host, StorageExport, Token, TokenFunctionExport};
use tracing::{Instrument as _, info_span};
use wasmtime::component::{ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{Store, bail};

use crate::common::{Ctx, NoopContractLookup, compile_contract};

static CONTRACT: LazyLock<Vec<u8>> = LazyLock::new(|| {
    compile_contract(include_str!(
        "../../starstream-to-wasm/tests/inputs/token_mint_burn.star"
    ))
});

struct MyToken {
    storage: StorageExport,
    burn: TokenFunctionExport,
    mint: TokenFunctionExport,
}

fn assert_my_token<T: Host>(contract: &Contract<T>) -> wasmtime::Result<MyToken> {
    if let Some(export) = contract.utxos().next() {
        bail!("unexpected UTXO export: {export:?}")
    }

    if let Some(export) = contract.coordination_scripts().next() {
        bail!("unexpected coordination script export: {export:?}")
    }

    let mut token_exports = contract.tokens();
    let token = match (token_exports.next(), token_exports.next()) {
        (Some(("my-token", Ok(token))), None) => token,
        exports => bail!("unexpected token exports: {exports:?}"),
    };
    let _named = contract
        .get_token("my-token")
        .context("failed to get `my-token` token export by name")?;

    let functions = contract.token_functions(&token);
    let functions: BTreeMap<_, _> = functions
        .map(|(name, export)| export.map(|export| (String::from(name), export)))
        .collect::<wasmtime::Result<_>>()
        .context("failed to iterate functions")?;
    assert_eq!(
        functions.keys().collect::<Vec<_>>(),
        ["[static]token.burn", "[static]token.mint"]
    );
    for name in functions.keys() {
        let _named = contract
            .get_token_function(&token, name)
            .with_context(|| format!("failed to get token `{name}` function export by name"))?;
    }

    Ok(MyToken {
        storage: token.storage().clone(),
        burn: functions["[static]token.burn"].clone(),
        mint: functions["[static]token.mint"].clone(),
    })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct MyTokenStorage {
    total: i64,
}

impl<'a> FromIterator<&'a (String, Val)> for MyTokenStorage {
    fn from_iter<T: IntoIterator<Item = &'a (String, Val)>>(fields: T) -> Self {
        let mut fields = fields.into_iter().map(|(k, v)| (k.as_str(), v));
        match (fields.next(), fields.next()) {
            (Some(("total", Val::S64(total))), None) => MyTokenStorage { total: *total },
            fields => panic!("unexpected MyToken token storage fields: {fields:?}"),
        }
    }
}

async fn get_my_token_storage(
    store: &mut Store<impl Send + 'static>,
    token: &Token,
    storage: &StorageExport,
) -> wasmtime::Result<MyTokenStorage> {
    let storage = token
        .storage(storage)
        .call_get(store)
        .await
        .context("failed to get storage")?;
    Ok(storage.iter().collect())
}

#[test_log::test(tokio::test)]
async fn token_mint_burn() -> wasmtime::Result<()> {
    let engine = wasmtime::Engine::default();
    let contract = Contract::new(&engine, NoopContractLookup, CONTRACT.as_slice())
        .context("failed to create contract")?;
    let ty = assert_my_token(&contract)?;

    let mut store = Store::new(
        &engine,
        Ctx {
            table: ResourceTable::default(),
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let instance = contract
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;

    let token = instance
        .load_token(
            &mut store,
            &ty.storage,
            [(String::from("total"), Val::S64(42))],
        )
        .instrument(info_span!("load"))
        .await
        .context("failed to load token")?;

    let MyTokenStorage { total } = get_my_token_storage(&mut store, &token, &ty.storage).await?;
    assert_eq!(total, 42);

    let mut results = [Val::Bool(false)];
    instance
        .call_token_function(&mut store, &ty.mint, [], &mut results)
        .instrument(info_span!("mint"))
        .await
        .context("failed to call `mint`")?;
    let [Val::Resource(minted)] = results else {
        bail!("`mint` did not return a resource");
    };
    assert!(minted.owned());

    let MyTokenStorage { total } = get_my_token_storage(&mut store, &token, &ty.storage).await?;
    assert_eq!(total, 43);

    instance
        .call_token_function(&mut store, &ty.burn, [Val::Resource(minted)], [])
        .instrument(info_span!("burn"))
        .await
        .context("failed to call `burn`")?;
    let MyTokenStorage { total } = get_my_token_storage(&mut store, &token, &ty.storage).await?;
    assert_eq!(total, 42);

    token
        .drop(&mut store)
        .await
        .context("failed to drop token")?;

    let Ctx {
        table,
        events,
        outputs,
    } = store.into_data();
    assert!(table.is_empty());
    assert!(events.is_empty());
    assert!(outputs.is_empty());
    Ok(())
}
