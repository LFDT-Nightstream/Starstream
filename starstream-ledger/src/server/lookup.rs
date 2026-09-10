use std::collections::HashMap;
use std::sync::Arc;

use tracing::error;
use wasmtime::error::Context as _;

use crate::server::{Contract, Ctx};

pub struct ContractLookup<'a>(pub &'a HashMap<&'a str, Arc<Contract>>);

impl starstream_runtime_next::ContractLookup<Ctx> for ContractLookup<'_> {
    fn get_contract(
        &self,
        contract_id: &str,
    ) -> wasmtime::Result<starstream_runtime_next::Contract<Ctx>> {
        let contract = self.0.get(contract_id).with_context(|| {
            error!(contract_id, "unresolved contract import");
            format!("imported contract `{contract_id}` not found")
        })?;
        Ok(contract.contract.clone())
    }
}
