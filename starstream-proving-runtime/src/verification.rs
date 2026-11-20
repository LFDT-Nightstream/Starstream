use std::collections::{BTreeMap, BTreeSet};

use neo_wasm::nebula::{WasmLimits, WasmSize};
use neo_wasm::{CommChainState, WasmStepState};
use nightstream::{PrimeField64, nebula::Program};
use starstream_interleaving_prover::CoroutineId;
use wasmtime::ensure;
use wasmtime::error::Context as _;

/// Verifier-selected component bytes and metadata, not values taken from a proof.
/// `wasm` must include the same deterministic instrumentation and input snapshot
/// as the executing component. `contract_digest` identifies the original contract.
pub struct VerificationProgram<'a> {
    pub wasm: &'a [u8],
    pub coordination_exports: &'a [&'a str],
    pub contract_digest: &'a [u8; 32],
    /// Ledger input positions and their UTXO export names in this component.
    pub input_utxos: &'a [(usize, String)],
}

pub(crate) struct ProgramVerificationInputs {
    source_index: usize,
    pub program: Program,
    pub allowed_initial_states: Vec<WasmStepState>,
}

/// Independently reconstructed programs and canonical entry states.
pub struct PreparedVerification {
    programs: BTreeMap<WasmSize, Vec<ProgramVerificationInputs>>,
    input_entries: BTreeMap<u32, (usize, u32)>,
}

impl PreparedVerification {
    pub(crate) fn input_count(&self) -> usize {
        self.input_entries.len()
    }

    pub(crate) fn resolve(
        &self,
        coroutine: u32,
        size: WasmSize,
        digest: [u64; 4],
        entry_fref: u32,
    ) -> wasmtime::Result<(&Program, &WasmStepState)> {
        let input = self.input_entries.get(&coroutine);
        let program = self
            .programs
            .get(&size)
            .into_iter()
            .flatten()
            .find(|p| {
                input.is_none_or(|&(source, _)| source == p.source_index)
                    && p.program.d_init().map(|x| x.as_canonical_u64()) == digest
            })
            .context("unexpected program for proof coroutine")?;
        if let Some(&(_, setter)) = input {
            ensure!(
                entry_fref == setter,
                "input proof must start at its UTXO's set-storage export"
            );
        }
        let initial = program
            .allowed_initial_states
            .iter()
            .find(|s| s.host_callee_fref == entry_fref)
            .context("unknown proof entry")?;
        Ok((&program.program, initial))
    }

    pub(crate) fn instance(
        &self,
        coroutine: u32,
        size: WasmSize,
        program: &Program,
        initial: &WasmStepState,
    ) -> wasmtime::Result<(&Program, &WasmStepState)> {
        let expected = self.resolve(
            coroutine,
            size,
            program.d_init().map(|x| x.as_canonical_u64()),
            initial.host_callee_fref,
        )?;
        ensure!(
            initial == expected.1,
            "proof does not start at the canonical exported entry state"
        );
        Ok(expected)
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
    let mut input_entries = BTreeMap::new();
    for (source_index, source) in programs.iter().enumerate() {
        let templates = crate::build_component_templates(
            source.wasm,
            source.coordination_exports,
            source.contract_digest,
        )?;
        for (index, export) in source.input_utxos {
            let index = u32::try_from(*index)?;
            ensure!(
                index <= u32::MAX / 2,
                "input index exceeds coroutine ID range"
            );
            let coroutine = CoroutineId::Utxo(index).encoded();
            let setter = templates
                .export_fref(&format!("{export}#set-storage"))
                .context("input UTXO is missing its set-storage export")?;
            ensure!(
                input_entries
                    .insert(coroutine, (source_index, setter))
                    .is_none(),
                "duplicate input program binding"
            );
        }
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
                        source_index,
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
    ensure!(
        input_entries
            .keys()
            .copied()
            .eq((0..input_entries.len()).map(|i| CoroutineId::Utxo(i as u32).encoded())),
        "input program bindings must follow transaction input order"
    );
    Ok(PreparedVerification {
        programs: prepared,
        input_entries,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inputs_select_their_program_and_storage_export() -> wasmtime::Result<()> {
        let module = |value| {
            wat::parse_str(format!(
                r#"(module
            (func (export "first#set-storage") (param i32 i32) (result i32) i32.const {value})
            (func (export "second#set-storage") (param i32 i32) (result i32) i32.const 0))"#
            ))
        };
        let first = module(1)?;
        let second = module(2)?;
        let programs = [
            VerificationProgram {
                wasm: &first,
                coordination_exports: &[],
                contract_digest: &[0; 32],
                input_utxos: &[(0, "first".into()), (2, "second".into())],
            },
            VerificationProgram {
                wasm: &second,
                coordination_exports: &[],
                contract_digest: &[0; 32],
                input_utxos: &[(1, "first".into())],
            },
        ];
        let size = WasmSize::Small;
        let expected = prepare_verification(&programs, [size])?;
        let prepared = &expected.programs[&size];
        let input = |index, source: usize, entry: usize| {
            let program = &prepared[source];
            expected.instance(
                CoroutineId::Utxo(index).encoded(),
                size,
                &program.program,
                &program.allowed_initial_states[entry],
            )
        };
        input(0, 0, 0)?;
        input(1, 1, 0)?;
        input(2, 0, 1)?;
        assert!(input(0, 1, 0).is_err());
        assert!(input(0, 0, 1).is_err());
        Ok(())
    }

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
                    input_utxos: &[],
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
        expected.instance(0, WasmSize::Small, &program.program, &initial)?;
        assert!(
            prepare(&changed)?
                .instance(0, WasmSize::Small, &program.program, &initial)
                .is_err()
        );
        let mut changed_initial = initial;
        changed_initial.comm_chain[0] = 1;
        assert!(
            expected
                .instance(0, WasmSize::Small, &program.program, &changed_initial)
                .is_err()
        );
        changed_initial = initial;
        changed_initial.pc += 1;
        assert!(
            expected
                .instance(0, WasmSize::Small, &program.program, &changed_initial)
                .is_err()
        );
        assert!(
            expected
                .instance(0, WasmSize::Large, &program.program, &initial)
                .is_err()
        );
        Ok(())
    }
}
