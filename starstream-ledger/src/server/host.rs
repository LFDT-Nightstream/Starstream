use core::pin::Pin;

use std::sync::Arc;

use starstream_runtime_next::bindings::starstream;
use starstream_runtime_next::{Token, Utxo, UtxoExport};
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::{StoreContextMut, bail};

use crate::server::{Ctx, UtxoCtx};

impl starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }
}

impl starstream_runtime_next::Host for Ctx {
    type UtxoContext = Arc<UtxoCtx>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        _store: StoreContextMut<'_, Self>,
        instance_name: Arc<str>,
        _external_id: Option<Arc<str>>,
        _export: UtxoExport,
        _f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        bail!("attempted to call `main fn` of UTXO `{instance_name}`")
    }

    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let Ctx { table, .. } = store.data();
        let utxo = table.get(&utxo)?;
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

    fn emit_event(
        _store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        bail!("attempted to emit event `{abi_name}` `{name}` with `{params:?}`")
    }
}
