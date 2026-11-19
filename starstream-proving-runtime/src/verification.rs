use std::collections::{BTreeMap, BTreeSet};

use neo_wasm::nebula::{WasmLimits, WasmSize};
use neo_wasm::{CommChainState, WasmStepState};
use nightstream::nebula::Program;
use wasmtime::ensure;
use wasmtime::error::Context as _;

/// Verifier-selected component bytes and metadata, not values taken from a proof.
/// `wasm` must include the same deterministic instrumentation and input snapshot
/// as the executing component. `contract_digest` identifies the original contract.
pub struct VerificationProgram<'a> {
    pub wasm: &'a [u8],
    pub coordination_exports: &'a [&'a str],
    pub contract_digest: &'a [u8; 32],
}

pub(crate) struct ProgramVerificationInputs {
    pub program: Program,
    pub allowed_initial_states: Vec<WasmStepState>,
}

/// Independently reconstructed programs and canonical entry states.
pub struct PreparedVerification {
    pub(crate) programs: BTreeMap<WasmSize, Vec<ProgramVerificationInputs>>,
}

impl PreparedVerification {
    pub(crate) fn instance(
        &self,
        size: WasmSize,
        program: &Program,
        initial: &WasmStepState,
    ) -> wasmtime::Result<(&Program, &WasmStepState)> {
        let expected = self
            .programs
            .get(&size)
            .and_then(|programs| {
                programs
                    .iter()
                    .find(|expected| expected.program == *program)
            })
            .context("proof program is not among the verifier's prepared programs")?;
        let initial = expected
            .allowed_initial_states
            .iter()
            .find(|entry| *entry == initial)
            .context("proof does not start at a canonical exported entry state")?;
        Ok((&expected.program, initial))
    }
}

/// Rebuild commitments and entry states without consulting execution traces.
/// Sizes may come from proof metadata: they select fixed circuit profiles only.
/// The caller must authenticate the component bytes and metadata.
pub fn prepare_verification(
    programs: &[VerificationProgram<'_>],
    sizes: impl IntoIterator<Item = WasmSize>,
) -> wasmtime::Result<PreparedVerification> {
    ensure!(!programs.is_empty(), "no verification programs supplied");
    let sizes = sizes.into_iter().collect::<BTreeSet<_>>();
    ensure!(!sizes.is_empty(), "no verification sizes supplied");
    let mut prepared = BTreeMap::<_, Vec<ProgramVerificationInputs>>::new();
    let applications = sizes
        .iter()
        .map(|&size| Ok((size, size.application()?)))
        .collect::<wasmtime::Result<BTreeMap<_, _>>>()?;
    for source in programs {
        let templates = crate::build_component_templates(
            source.wasm,
            source.coordination_exports,
            source.contract_digest,
        )?;
        let module = crate::templates::first_core_module(source.wasm)?;
        let mut artifacts = neo_wasm::extract_wasm_program_artifacts(module)?;
        artifacts.host_event_bindings = templates.bindings;
        let mut preload = neo_wasm::preload_from_program_artifacts(&artifacts);
        neo_wasm::memory_semantics::preload_host_event_tables(
            &mut preload,
            &artifacts.host_event_bindings,
            &artifacts,
        );
        let mut allowed_initial_states = Vec::new();
        for fref in &templates.initial_exports {
            let pc = artifacts
                .tables
                .function_entries
                .iter()
                .find_map(|&(function, pc)| (function == u64::from(*fref)).then_some(pc))
                .context("export has no program entry")?;
            allowed_initial_states.push(neo_wasm::host_event_top_level_initial_state(
                &artifacts.tables,
                pc,
                &artifacts.host_event_bindings,
                *fref,
                CommChainState::default(),
            )?);
        }
        let limits = WasmLimits::of(&[&preload], &[])?;
        for (&size, app) in &applications {
            if size.limits().holds(&limits) {
                prepared
                    .entry(size)
                    .or_default()
                    .push(ProgramVerificationInputs {
                        program: app.program(&preload)?,
                        allowed_initial_states: allowed_initial_states.clone(),
                    });
            }
        }
    }
    for size in sizes {
        ensure!(
            prepared.contains_key(&size),
            "no supplied program fits {size:?}"
        );
    }
    Ok(PreparedVerification { programs: prepared })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preparation_binds_bytecode_and_canonical_entry_state() -> wasmtime::Result<()> {
        let module = |value| {
            wat::parse_str(format!(
                r#"(module
            (func (export "fixture#internal"))
            (func (export "fixture#[method]utxo.read") (param i32))
            (func (export "fixture#[static]utxo.new") (param i32) i32.const {value} drop))"#
            ))
        };
        let wasm = module(1)?;
        let changed = module(2)?;
        let prepare = |bytes: &[u8]| {
            prepare_verification(
                &[VerificationProgram {
                    wasm: bytes,
                    coordination_exports: &[],
                    contract_digest: &[0; 32],
                }],
                [WasmSize::Small],
            )
        };
        let expected = prepare(&wasm)?;
        let rebuilt = prepare(&wasm)?;
        let program = &rebuilt.programs[&WasmSize::Small][0];
        assert_eq!(program.allowed_initial_states.len(), 1);
        let initial = program.allowed_initial_states[0];
        assert_eq!(initial.host_callee_fref, 3);
        expected.instance(WasmSize::Small, &program.program, &initial)?;
        assert!(
            prepare(&changed)?
                .instance(WasmSize::Small, &program.program, &initial)
                .is_err()
        );
        let mut changed_initial = initial;
        changed_initial.comm_chain[0] = 1;
        assert!(
            expected
                .instance(WasmSize::Small, &program.program, &changed_initial)
                .is_err()
        );
        changed_initial = initial;
        changed_initial.pc += 1;
        assert!(
            expected
                .instance(WasmSize::Small, &program.program, &changed_initial)
                .is_err()
        );
        assert!(
            expected
                .instance(WasmSize::Large, &program.program, &initial)
                .is_err()
        );
        Ok(())
    }
}
