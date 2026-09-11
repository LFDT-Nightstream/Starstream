pub mod common;

use std::sync::LazyLock;

use starstream_runtime_next::{
    Contract, Host, StorageExport, Token, TokenBurnExport, TokenMintExport,
    get_coordination_script_instance_import, utxo_imports,
};
use tracing::{Instrument as _, info_span};
use wasmtime::component::{ResourceTable, Val};
use wasmtime::error::Context as _;
use wasmtime::{Store, bail};

use crate::common::{Ctx, ENGINE, NoopContractLookup, compile_contract};

struct MyToken {
    storage: StorageExport,
    burn: TokenBurnExport,
    mint: TokenMintExport,
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

    let mut mints = contract.token_mints(&token);
    let mint = match (mints.next(), mints.next()) {
        (Some(("[static]token.mint", Ok(mint))), None) => mint,
        exports => bail!("unexpected token mint exports: {exports:?}"),
    };
    let _named = contract
        .get_token_mint(&token, "[static]token.mint")
        .context("failed to get token `mint fn` export by name")?;

    let mut burns = contract.token_burns(&token);
    let burn = match (burns.next(), burns.next()) {
        (Some(("[static]token.burn", Ok(burn))), None) => burn,
        exports => bail!("unexpected token burn exports: {exports:?}"),
    };
    let _named = contract
        .get_token_burn(&token, "[static]token.burn")
        .context("failed to get token `burn fn` export by name")?;

    Ok(MyToken {
        storage: token.storage().clone(),
        burn,
        mint,
    })
}

static CONTRACT: LazyLock<Contract<Ctx>> = LazyLock::new(|| {
    let component = compile_contract(include_str!(
        "../../starstream-to-wasm/tests/inputs/token_mint_burn.star"
    ))
    .unwrap();
    let ty = component.component_type();
    assert!(get_coordination_script_instance_import(&ENGINE, &ty).is_none());
    assert!(utxo_imports(&ENGINE, &ty).next().is_none());
    Contract::new(&component, NoopContractLookup).expect("failed to create contract")
});

static MY_TOKEN: LazyLock<MyToken> = LazyLock::new(|| assert_my_token(&CONTRACT).unwrap());

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
async fn mint() -> wasmtime::Result<()> {
    let mut store = Store::new(
        &ENGINE,
        Ctx {
            table: ResourceTable::default(),
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let instance = CONTRACT
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;

    let token = instance
        .call_token_mint(&mut store, &MY_TOKEN.mint, [])
        .instrument(info_span!("mint"))
        .await
        .context("failed to call `mint`")?;
    let MyTokenStorage { total } =
        get_my_token_storage(&mut store, &token, &MY_TOKEN.storage).await?;
    assert_eq!(total, 1);

    token
        .call_burn(&mut store, &MY_TOKEN.burn, [])
        .instrument(info_span!("burn"))
        .await
        .context("failed to call `burn`")?;

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

#[test_log::test(tokio::test)]
async fn load() -> wasmtime::Result<()> {
    let mut store = Store::new(
        &ENGINE,
        Ctx {
            table: ResourceTable::default(),
            events: Vec::default(),
            outputs: Vec::default(),
        },
    );
    let instance = CONTRACT
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;

    let token = instance
        .load_token(
            &mut store,
            &MY_TOKEN.storage,
            [(String::from("total"), Val::S64(1))],
        )
        .instrument(info_span!("load"))
        .await
        .context("failed to load token")?;
    let MyTokenStorage { total } =
        get_my_token_storage(&mut store, &token, &MY_TOKEN.storage).await?;
    assert_eq!(total, 1);

    token
        .call_burn(&mut store, &MY_TOKEN.burn, [])
        .instrument(info_span!("burn"))
        .await
        .context("failed to call `burn`")?;

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
