pub mod common;

use std::sync::LazyLock;

use starstream_runtime_next::{
    Contract, ContractLookup, CoordinationScriptsImport, UtxoImport, coordination_script_imports,
    utxo_imports,
};
use wasm_encoder::{
    ComponentImportSection, ComponentTypeRef, ComponentTypeSection, InstanceType, TypeBounds,
};
use wasmtime::component::Component;
use wasmtime::error::Context as _;
use wasmtime::{bail, ensure};

use crate::common::{Ctx, ENGINE, NoopContractLookup, compile_contract};

static DEP: LazyLock<Component> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")).unwrap());

const DEP_A: &str = "bdepa";
const DEP_B: &str = "bdepb";

struct Lookup(Contract<Ctx>);

impl ContractLookup<Ctx> for Lookup {
    fn get_contract(&self, contract_id: &str) -> wasmtime::Result<Contract<Ctx>> {
        ensure!(
            matches!(contract_id, DEP_A | DEP_B),
            "unexpected contract id `{contract_id}`"
        );
        Ok(self.0.clone())
    }
}

fn build_importing_component() -> Vec<u8> {
    let mut component = wasm_encoder::Component::new();

    let mut types = ComponentTypeSection::new();
    let mut utxo = InstanceType::new();
    utxo.export("utxo", ComponentTypeRef::Type(TypeBounds::SubResource));
    types.instance(&utxo);
    let mut scripts = InstanceType::new();
    scripts
        .ty()
        .function()
        .params([] as [(&str, wasm_encoder::ComponentValType); 0])
        .result(None);
    scripts.export("example", ComponentTypeRef::Func(0));
    types.instance(&scripts);
    component.section(&types);

    let mut imports = ComponentImportSection::new();
    for dep in [DEP_A, DEP_B] {
        imports.import(
            format!("starstream:contract/{dep}/utxo/score-progress"),
            ComponentTypeRef::Instance(0),
        );
        imports.import(
            format!("starstream:contract/{dep}/scripts"),
            ComponentTypeRef::Instance(1),
        );
    }
    component.section(&imports);

    component.finish()
}

#[test_log::test]
fn contract_imports() -> wasmtime::Result<()> {
    let dep = Contract::new(&DEP, NoopContractLookup).context("failed to create dep contract")?;

    let wasm = build_importing_component();
    let component =
        Component::from_binary(&ENGINE, &wasm).context("failed to compile importing component")?;
    let ty = component.component_type();

    let mut utxos = utxo_imports(&ENGINE, &ty);
    match (utxos.next(), utxos.next(), utxos.next()) {
        (
            Some(UtxoImport {
                name: "score-progress",
                contract_id: DEP_A,
                ..
            }),
            Some(UtxoImport {
                name: "score-progress",
                contract_id: DEP_B,
                ..
            }),
            None,
        ) => {}
        imports => bail!("unexpected UTXO imports: {imports:?}"),
    }

    let mut scripts = coordination_script_imports(&ENGINE, &ty);
    match (scripts.next(), scripts.next(), scripts.next()) {
        (
            Some(CoordinationScriptsImport {
                contract_id: DEP_A, ..
            }),
            Some(CoordinationScriptsImport {
                contract_id: DEP_B, ..
            }),
            None,
        ) => {}
        imports => bail!("unexpected coordination script imports: {imports:?}"),
    }

    let _contract =
        Contract::new(&component, Lookup(dep)).context("failed to link importing component")?;
    Ok(())
}
