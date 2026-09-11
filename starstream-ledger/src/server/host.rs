use core::pin::Pin;

use std::sync::Arc;

use starstream_runtime_next::bindings::starstream;
use starstream_runtime_next::{Token, Utxo};
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::{AsContextMut as _, StoreContextMut, bail, format_err};

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
    type UtxoContext = Arc<std::sync::Mutex<UtxoCtx>>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        mut store: StoreContextMut<'_, Self>,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        let utxo = f(store.as_context_mut(), Self::UtxoContext::default()).await?;
        store.as_context_mut().data_mut().outputs.push(utxo.clone());
        Ok(utxo)
    }

    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let Ctx { table, .. } = store.data();
        let utxo = table.get(&utxo)?;
        let cx = utxo.context().lock().map_err(|err| format_err!("{err}"))?;
        Ok(cx.methods.contains(&hash))
    }

    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        table.delete(utxo)?;
        Ok(())
    }

    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        let mut cx = cx.lock().map_err(|err| format_err!("{err}"))?;
        cx.methods.push(hash);
        Ok(())
    }

    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        let mut cx = cx.lock().map_err(|err| format_err!("{err}"))?;
        cx.methods.clear();
        Ok(())
    }

    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        table.delete(cx)?;
        Ok(())
    }

    fn drop_token(_store: StoreContextMut<Self>, _token: Resource<Token>) -> wasmtime::Result<()> {
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
