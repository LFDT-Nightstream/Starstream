use std::sync::Arc;

use starstream_runtime_next::bindings::starstream;
use starstream_runtime_next::{EventHandler, Utxo, UtxoHandler};
use tracing::info;
use wasmtime::StoreContextMut;
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::error::Context as _;

use crate::{Ctx, UtxoImport, UtxoOutput};

impl starstream::std::builtin::Host for Ctx {
    fn implements_method(&mut self, hash: (u64, u64, u64, u64)) -> wasmtime::Result<()> {
        self.implemented.insert(hash);
        Ok(())
    }
}

impl starstream::std::builtin::HostUtxo for Ctx {
    fn drop(&mut self, _utxo: Resource<Utxo>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> i64 {
        self.cardano.block_height
    }

    fn current_slot(&mut self) -> i64 {
        self.cardano.current_slot
    }
}

impl EventHandler for Ctx {
    fn emit_event(&mut self, instance: &str, name: &str, params: &[Val]) {
        info!(instance, name, ?params, "ABI event emitted");
    }
}

impl UtxoHandler for Ctx {
    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn construct_utxo(
        mut store: StoreContextMut<'_, Self>,
        instance: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<Utxo>
    where
        Self: Sized,
    {
        let Ctx { imports, .. } = store.data();
        let UtxoImport { contract, export } = imports
            .get(instance.as_ref())
            .with_context(|| format!("unresolved UTXO instance import {instance}"))?;

        let export = contract
            .get_utxo_constructor(export, name)
            .context("failed to get UTXO constructor")?;
        let contract = contract.clone();
        let utxo = contract.create_utxo(&mut store, &export, params).await?;
        let ctx = store.data_mut();
        ctx.outputs.push(UtxoOutput {
            utxo,
            instance: Arc::clone(instance),
            implemented: ctx.implemented.drain().collect(),
        });
        Ok(utxo)
    }
}
