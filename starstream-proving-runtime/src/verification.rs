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
    /// These bytes were prepared from the original contract without applying an input snapshot.
    pub is_original_image: bool,
    /// Ledger input positions and their UTXO export names in this component.
    pub input_utxos: &'a [(usize, String)],
    /// Transaction-local token indices and their storage interfaces in this component.
    pub input_tokens: &'a [(usize, String)],
    /// Transaction-local IDs of outputs created by this component, and their
    /// UTXO export names. IDs follow the inputs, in creation order.
    pub new_utxos: &'a [(u32, String)],
}

pub(crate) struct ProgramVerificationInputs {
    source_index: usize,
    is_original_image: bool,
    pub program: Program,
    pub allowed_initial_states: Vec<WasmStepState>,
}

/// Independently reconstructed programs and canonical entry states.
pub struct PreparedVerification {
    programs: BTreeMap<WasmSize, Vec<ProgramVerificationInputs>>,
    /// Source program and admissible entry exports per bound resource coroutine:
    /// `set-storage` for an input, any constructor for a new output.
    resource_entries: BTreeMap<u32, (usize, Vec<u32>)>,
    input_count: usize,
    input_token_count: usize,
}

impl PreparedVerification {
    pub(crate) fn input_count(&self) -> usize {
        self.input_count
    }

    pub(crate) fn input_token_count(&self) -> usize {
        self.input_token_count
    }

    pub(crate) fn resolve(
        &self,
        coroutine: u32,
        size: WasmSize,
        digest: [u64; 4],
        entry_fref: u32,
    ) -> wasmtime::Result<(&Program, &WasmStepState)> {
        let bound = self.resource_entries.get(&coroutine);
        match CoroutineId::from_packed(coroutine) {
            Some(CoroutineId::Token(_)) => {
                ensure!(bound.is_some(), "missing input token program binding")
            }
            Some(_) => {}
            None => wasmtime::bail!("invalid coroutine kind"),
        }
        let program = self
            .programs
            .get(&size)
            .into_iter()
            .flatten()
            .find(|p| {
                bound.map_or(p.is_original_image, |&(source, _)| source == p.source_index)
                    && p.program.d_init().map(|x| x.as_canonical_u64()) == digest
            })
            .context("unexpected program for proof coroutine")?;
        if let Some((_, entries)) = bound {
            ensure!(
                entries.contains(&entry_fref),
                "resource proof must start at its bound export"
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
///
/// Verification assumes the compiler's component layout: each public entry
/// resolves to the conventionally named export in the first core module.
/// For example, the constructor `counter.new` must resolve to `counter#[static]utxo.new`.
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
    let mut resource_entries = BTreeMap::new();
    let mut input_count = 0;
    let mut input_token_count = 0;
    let mut bind = |coroutine: CoroutineId, source_index, entries: Vec<u32>| {
        let packed = coroutine
            .checked_encoded()
            .context("resource index exceeds coroutine ID range")?;
        ensure!(
            resource_entries
                .insert(packed, (source_index, entries))
                .is_none(),
            "duplicate resource program binding"
        );
        Ok(())
    };
    for (source_index, source) in programs.iter().enumerate() {
        let templates = crate::build_component_templates(
            source.wasm,
            source.coordination_exports,
            source.contract_digest,
        )?;
        for (indices, token) in [(source.input_utxos, false), (source.input_tokens, true)] {
            for (index, export) in indices {
                let setter = templates
                    .export_fref(&format!("{export}#set-storage"))
                    .context("input resource is missing its set-storage export")?;
                ensure!(
                    templates
                        .storage_setters
                        .get(&setter)
                        .is_some_and(
                            |kind| matches!(kind, crate::StorageKind::Token { .. }) == token
                        ),
                    "storage setter has the wrong resource kind"
                );
                let index = u32::try_from(*index)?;
                let id = if token {
                    input_token_count += 1;
                    CoroutineId::Token(index)
                } else {
                    input_count += 1;
                    CoroutineId::Utxo(index)
                };
                bind(id, source_index, vec![setter])?;
            }
        }
        ensure!(
            source.new_utxos.is_empty() || source.is_original_image,
            "new output requires the original contract program"
        );
        for (utxo, export) in source.new_utxos {
            // The constructor's entry event commits to this contract and
            // export, and the interleaving proof matches it to the caller's
            // `NewUtxo`, so any of the export's constructors is admissible.
            let constructors = templates
                .export_frefs
                .range(format!("{export}#[static]utxo.")..)
                .take_while(|(name, _)| {
                    name.strip_prefix(export.as_str())
                        .is_some_and(|item| item.starts_with("#[static]utxo."))
                })
                .map(|(_, &fref)| fref)
                .collect::<Vec<_>>();
            ensure!(
                !constructors.is_empty(),
                "output UTXO export has no constructor"
            );
            bind(CoroutineId::Utxo(*utxo), source_index, constructors)?;
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
                        is_original_image: source.is_original_image,
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
    // Inputs take the first IDs, so with distinct keys this also places every
    // new output after them.
    ensure!(
        programs
            .iter()
            .flat_map(|p| p.input_utxos)
            .all(|(index, _)| *index < input_count),
        "input program bindings must follow transaction input order"
    );
    ensure!(
        programs
            .iter()
            .flat_map(|p| p.input_tokens)
            .all(|(index, _)| *index < input_token_count),
        "token bindings must follow transaction input order"
    );
    Ok(PreparedVerification {
        programs: prepared,
        resource_entries,
        input_count,
        input_token_count,
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
                is_original_image: false,
                input_tokens: &[],
                input_utxos: &[(0, "first".into()), (2, "second".into())],
                new_utxos: &[],
            },
            VerificationProgram {
                wasm: &second,
                coordination_exports: &[],
                contract_digest: &[0; 32],
                is_original_image: false,
                input_tokens: &[],
                input_utxos: &[(1, "first".into())],
                new_utxos: &[],
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
    fn new_outputs_select_their_program_and_constructor() -> wasmtime::Result<()> {
        let module = |value| {
            wat::parse_str(format!(
                r#"(module
            (memory 1)
            (data (i32.const 0) "{value}")
            (func (export "first#[static]utxo.new") (param i32))
            (func (export "first#[static]utxo.other") (param i32))
            (func (export "second#[static]utxo.new") (param i32))
            (func (export "first#set-storage") (param i32 i32) (result i32) i32.const 0))"#
            ))
        };
        let first = module(1)?;
        let second = module(2)?;
        let program = |wasm, is_original_image, input_utxos, new_utxos| VerificationProgram {
            wasm,
            coordination_exports: &[],
            contract_digest: &[0; 32],
            is_original_image,
            input_tokens: &[],
            input_utxos,
            new_utxos,
        };
        let inputs = [(0, "first".into())];
        let outputs = [(1, "first".into())];
        let size = WasmSize::Small;
        let expected = prepare_verification(
            &[
                program(&first, true, &inputs, &outputs),
                program(&second, false, &[], &[]),
            ],
            [size],
        )?;
        let prepared = &expected.programs[&size];
        let utxo = |index, source: usize, export: usize| {
            let program = &prepared[source];
            // Initial states follow export order: both `first` constructors,
            // the `second` constructor, then `first#set-storage`.
            expected.instance(
                CoroutineId::Utxo(index).encoded(),
                size,
                &program.program,
                &program.allowed_initial_states[export],
            )
        };
        utxo(1, 0, 0)?;
        utxo(1, 0, 1)?;
        // Another export's constructor, the storage loader, and a different initial memory image.
        assert!(utxo(1, 0, 2).is_err());
        assert!(utxo(1, 0, 3).is_err());
        assert!(utxo(1, 1, 0).is_err());
        // Fresh coroutines may use the original even when it also serves an input.
        utxo(2, 0, 2)?;
        assert!(utxo(2, 1, 2).is_err());
        let snapshot = &prepared[1];
        assert!(
            expected
                .resolve(
                    CoroutineId::Coord(1).encoded(),
                    size,
                    snapshot.program.d_init().map(|x| x.as_canonical_u64()),
                    snapshot.allowed_initial_states[0].host_callee_fref,
                )
                .is_err()
        );
        assert_eq!(expected.input_count(), 1);

        // An output cannot take an input's ID, and needs a constructor.
        let clash = [(0, "first".into())];
        assert!(prepare_verification(&[program(&first, true, &inputs, &clash)], [size]).is_err());
        let missing = [(1, "third".into())];
        assert!(prepare_verification(&[program(&first, true, &inputs, &missing)], [size]).is_err());
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
                    is_original_image: true,
                    input_tokens: &[],
                    input_utxos: &[],
                    new_utxos: &[],
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
