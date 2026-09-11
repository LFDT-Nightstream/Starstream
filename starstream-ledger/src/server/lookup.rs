use std::collections::HashMap;
use std::sync::Arc;

use tracing::error;
use wasmtime::error::Context as _;

use crate::server::{Contract, Ctx};

pub struct ContractLookup<'a>(pub &'a HashMap<&'a str, Arc<Contract>>);

impl starstream_runtime_next::ContractLookup<Ctx> for ContractLookup<'_> {
    fn get_contract(
        &self,
        external_id: &str,
    ) -> wasmtime::Result<starstream_runtime_next::Contract<Ctx>> {
        let contract = self.0.get(external_id).with_context(|| {
            error!(external_id, "unresolved contract import");
            format!("contract identified by `external-id` `{external_id}` not found")
        })?;
        Ok(contract.contract.clone())
    }
}
