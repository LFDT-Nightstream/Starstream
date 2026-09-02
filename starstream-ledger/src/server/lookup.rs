use std::collections::HashMap;
use std::sync::Arc;

use wasmtime::error::Context as _;

use crate::parse_digest;
use crate::server::{Contract, Ctx};

pub struct ContractLookup<'a>(pub &'a HashMap<[u8; 32], Arc<Contract>>);

impl starstream_runtime_next::ContractLookup<Ctx> for ContractLookup<'_> {
    fn get_contract(
        &self,
        external_id: &str,
    ) -> wasmtime::Result<starstream_runtime_next::Contract<Ctx>> {
        let digest = parse_digest(external_id).with_context(|| {
            format!("failed to parse `external-id` `{external_id}` as multibase multihash")
        })?;
        let contract = self.0.get(&digest).with_context(|| {
            format!("contract identified by `external-id` `{external_id}` not found")
        })?;
        Ok(contract.contract.clone())
    }
}
