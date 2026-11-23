pub mod common;

use std::sync::{Arc, LazyLock, Mutex};

use starstream_runtime::{Contract, Utxo};
use tracing::{Instrument as _, info_span};
use wasmtime::component::{Component, Val};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut as _, Store};

use crate::common::{Ctx, ENGINE, Event, NoopContractLookup, UtxoCtx, compile_contract};

static CONTRACT: LazyLock<Contract<Ctx>> = LazyLock::new(|| {
    let wasm = compile_contract(include_str!(
        "../../starstream-to-wasm/tests/inputs/token_attach.star"
    ))
    .unwrap();
    let component = Component::from_binary(&ENGINE, &wasm).unwrap();
    Contract::new(&component, None, NoopContractLookup).expect("failed to create contract")
});

fn event(name: &str, count: u64) -> Event {
    Event {
        abi_name: "my-token-events".into(),
        name: name.into(),
        params: [Val::U64(count)].into(),
    }
}

/// Construct a `Holder` UTXO and return a handle to it usable as a script argument.
async fn new_holder(
    store: &mut Store<Ctx>,
    instance: &starstream_runtime::ContractInstance,
) -> wasmtime::Result<Val> {
    let holder = CONTRACT.get_utxo("holder")?;
    let new = CONTRACT.get_utxo_main(&holder, "[static]utxo.new")?;

    let cx = Arc::new(Mutex::new(UtxoCtx::default()));
    let cx_res = store.data_mut().table.push(Arc::clone(&cx))?;
    let cx_res = cx_res.try_into_resource_any(store.as_context_mut())?;
    let utxo: Utxo<_> = instance
        .call_utxo_main(
            store.as_context_mut(),
            &holder,
            &new,
            cx,
            [Val::Resource(cx_res)],
        )
        .await
        .context("failed to construct `Holder`")?;
    let utxo = store.data_mut().table.push(utxo)?;
    let utxo = utxo.try_into_resource_any(store.as_context_mut())?;
    Ok(Val::Resource(utxo))
}

/// A script calling `.attach`/`.detach` on a `token`-declared value runs the
/// token's own `impl Token` methods.
#[test_log::test(tokio::test)]
async fn move_token() -> wasmtime::Result<()> {
    let mut store = Store::new(&ENGINE, Ctx::default());
    let instance = CONTRACT
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;

    let source = new_holder(&mut store, &instance).await?;
    let dest = new_holder(&mut store, &instance).await?;

    let move_token = CONTRACT.get_coordination_script("move-token")?;
    instance
        .call_coordination_script(&mut store, &move_token, [source, dest], [])
        .instrument(info_span!("move-token"))
        .await
        .context("failed to call `move-token` coordination script")?;

    let Ctx {
        starstream, events, ..
    } = store.into_data();
    assert_eq!(
        events,
        [
            event("attached", 1),
            event("detached", 0),
            event("attached", 1)
        ]
    );
    assert!(starstream.instance_stack.is_empty());
    assert!(starstream.effect_stack.is_empty());
    Ok(())
}

/// A script's `Token` parameter accepts a token minted outside the script.
#[test_log::test(tokio::test)]
async fn attach_minted_token() -> wasmtime::Result<()> {
    let mut store = Store::new(&ENGINE, Ctx::default());
    let instance = CONTRACT
        .instantiate(&mut store)
        .await
        .context("failed to instantiate contract")?;

    let my_token = CONTRACT.get_token("my-token")?;
    let my_mint = CONTRACT.get_token_mint(&my_token, "[static]token.my-mint")?;
    let token = instance
        .call_token_mint(&mut store, &my_token, &my_mint, [])
        .instrument(info_span!("my-mint"))
        .await
        .context("failed to call `my-mint`")?;
    let token = store.data_mut().table.push(token)?;
    let token = token.try_into_resource_any(store.as_context_mut())?;

    let dest = new_holder(&mut store, &instance).await?;

    let attach_any = CONTRACT.get_coordination_script("attach-any")?;
    instance
        .call_coordination_script(&mut store, &attach_any, [Val::Resource(token), dest], [])
        .instrument(info_span!("attach-any"))
        .await
        .context("failed to call `attach-any` coordination script")?;

    let Ctx {
        starstream, events, ..
    } = store.into_data();
    assert_eq!(events, [event("attached", 1)]);
    assert!(starstream.instance_stack.is_empty());
    assert!(starstream.effect_stack.is_empty());
    Ok(())
}
