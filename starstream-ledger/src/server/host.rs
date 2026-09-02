use core::pin::Pin;

use std::sync::Arc;

use starstream_runtime_next::Utxo;
use starstream_runtime_next::bindings::starstream;
use wasmtime::StoreContextMut;
use wasmtime::bail;
use wasmtime::component::{Resource, ResourceTable, Val};

use crate::server::Ctx;

impl starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }
}

impl starstream_runtime_next::Host for Ctx {
    type UtxoContext = ();
    type Token = ();

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        _store: StoreContextMut<'_, Self>,
        _f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        bail!("TODO")
    }

    fn has_method(
        _store: StoreContextMut<Self>,
        _utxo: Resource<Utxo<Self::UtxoContext>>,
        _hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        bail!("TODO")
    }

    fn drop_utxo(
        _store: StoreContextMut<Self>,
        _utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn implements_method(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
        __hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn resume(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn drop_utxo_context(
        _store: StoreContextMut<Self>,
        _cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn drop_token(
        _store: StoreContextMut<Self>,
        _token: Resource<Self::Token>,
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn emit_event(
        _store: StoreContextMut<Self>,
        _abi_name: &Arc<str>,
        _name: &Arc<str>,
        _params: &[Val],
    ) -> wasmtime::Result<()> {
        bail!("TODO")
    }
}
