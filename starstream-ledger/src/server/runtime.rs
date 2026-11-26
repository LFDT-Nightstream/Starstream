use core::pin::Pin;

use std::sync::Arc;

use starstream_runtime::bindings::starstream;
use starstream_runtime::{StarstreamCtx, Token, Utxo, UtxoExport};
use wasmtime::component::{Resource, ResourceTable, Val, types};
use wasmtime::{StoreContextMut, bail};

use crate::server::{Ctx, UtxoCtx};

impl starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        Ok(self.cardano.block_height)
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        Ok(self.cardano.current_slot)
    }
}

impl starstream::std::ledger::Host for Ctx {
    fn assert_transaction_signed_by(
        &mut self,
        _key: starstream::std::ledger::PublicKey,
    ) -> wasmtime::Result<()> {
        // TODO: real implementation.
        Ok(())
    }
}

impl starstream_runtime::Host for Ctx {
    type UtxoContext = Arc<UtxoCtx>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    fn starstream(&mut self) -> &mut StarstreamCtx {
        &mut self.starstream
    }

    async fn call_utxo_main<'a>(
        _store: StoreContextMut<'a, Self>,
        instance_name: &Arc<str>,
        _external_id: Option<&Arc<str>>,
        _export: UtxoExport,
        _f: impl for<'b> FnOnce(
            &'b mut StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'b>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        bail!("attempted to call `main fn` of UTXO `{instance_name}`")
    }

    fn has_method(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let utxo = store.data_mut().table.get(&utxo)?;
        Ok(utxo.context().methods.contains(&hash))
    }

    fn drop_utxo(
        _store: StoreContextMut<Self>,
        _utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        bail!("attempted to drop UTXO")
    }

    fn implements_method(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
        _hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        bail!("attempted to update method set")
    }

    fn resume(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        bail!("attempted to resume")
    }

    fn drop_utxo_context(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        bail!("attempted to drop UTXO context")
    }

    fn drop_token(_store: StoreContextMut<Self>, _token: Resource<Token>) -> wasmtime::Result<()> {
        bail!("attempted to drop token")
    }

    async fn mint_token<'a>(
        _store: StoreContextMut<'a, Self>,
        instance_name: &Arc<str>,
        _external_id: Option<&Arc<str>>,
        _f: impl for<'b> FnOnce(
            &'b mut StoreContextMut<'a, Self>,
        )
            -> Pin<Box<dyn Future<Output = wasmtime::Result<Token>> + Send + 'b>>
        + Send,
    ) -> wasmtime::Result<Token> {
        bail!("attempted to mint token `{instance_name}`")
    }

    async fn burn_token<'a>(
        _store: StoreContextMut<'a, Self>,
        _token: Resource<Token>,
        _f: impl for<'b> FnOnce(
            &'b mut StoreContextMut<'a, Self>,
            Resource<Token>,
        )
            -> Pin<Box<dyn Future<Output = wasmtime::Result<()>> + Send + 'b>>
        + Send,
    ) -> wasmtime::Result<()> {
        bail!("attempted to burn token")
    }

    async fn attach_token<'a>(
        _store: StoreContextMut<'a, Self>,
        _token: Resource<Token>,
        _utxo: Resource<Utxo<Self::UtxoContext>>,
        _f: impl for<'b> FnOnce(
            &'b mut StoreContextMut<'a, Self>,
            Resource<Token>,
            Resource<Utxo<Self::UtxoContext>>,
        )
            -> Pin<Box<dyn Future<Output = wasmtime::Result<()>> + Send + 'b>>
        + Send,
    ) -> wasmtime::Result<()> {
        bail!("attempted to attach token")
    }

    async fn detach_token<'a>(
        _store: StoreContextMut<'a, Self>,
        _token: Resource<Token>,
        _utxo: Resource<Utxo<Self::UtxoContext>>,
        _f: impl for<'b> FnOnce(
            &'b mut StoreContextMut<'a, Self>,
            Resource<Token>,
            Resource<Utxo<Self::UtxoContext>>,
        )
            -> Pin<Box<dyn Future<Output = wasmtime::Result<()>> + Send + 'b>>
        + Send,
    ) -> wasmtime::Result<()> {
        bail!("attempted to detach token")
    }

    fn emit_event(
        _store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        _ty: types::ComponentFunc,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        bail!("attempted to emit event `{abi_name}` `{name}` with `{params:?}`")
    }
}
