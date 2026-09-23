//! Prove each traced core instance with Neo-Wasm's Nebula pipeline and verify
//! the proof against the instance's final state and its absorbed transcript.
//!
//! These are full recursive folding proofs, so they are ignored by default.
//! The test target needs the `nebula-proofs` feature and a Nightstream
//! revision providing `WasmNebulaProfile::with_schedule` and the
//! reduced-memory host-event preprocessing entrypoint (see README.md):
//!
//! ```text
//! cargo test --release -p starstream-proving-runtime --features nebula-proofs \
//!   --test runtime_prove counter_full_transaction_proves_and_verifies \
//!   -- --ignored --exact --nocapture
//! ```
//!
//! Knobs (environment variables):
//! - `STARSTREAM_PROOF_STEPS`: application steps per segment (default 4).
//!   Proving time is proportional to the folds, preprocessing to the batch
//!   width, so fewer steps mean faster proofs but slower preprocessing.
//! - `STARSTREAM_PROOF_PARAMS=production`: shape-derived Appendix B.2
//!   parameters instead of the insecure λ = 20 demo parameters.

mod common;
#[path = "common/proof.rs"]
mod proof_config;

use proof_config::{entry_pc, params, profile};

use std::time::{Duration, Instant};

use neo_math::F;
use neo_wasm::comm_chain::{CommChainState, absorbed_event_blocks, fold_event_blocks};
use starstream_interleaving_prover::TraceCommitments;
use wasmtime::bail;

use crate::common::{
    MINIMAL_METHOD_CALL, SCORE, TracedExecution, coroutine_id, trace_coordination_script,
};

const PREPROCESSING_SEED: u64 = 0x5742_0002;

#[derive(Default)]
struct ProofTimings {
    preprocessing: Duration,
    proving: Duration,
    verification: Duration,
}

struct VerifiedExecution {
    roots: TraceCommitments,
    timings: ProofTimings,
}

/// Preprocess, prove and verify every core instance of `execution`.
fn prove_execution(
    name: &str,
    rom_bits: u32,
    execution: &TracedExecution,
) -> wasmtime::Result<VerifiedExecution> {
    #[cfg(target_vendor = "apple")]
    let mut prover = neo_wasm::WasmProver::metal()?;
    #[cfg(not(target_vendor = "apple"))]
    let mut prover = neo_wasm::WasmProver::auto();
    let mut verified = VerifiedExecution {
        roots: TraceCommitments::new(),
        timings: ProofTimings::default(),
    };
    eprintln!("{name}: prover backend {}", prover.backend().as_str());
    for (index, instance) in execution.instances.iter().enumerate() {
        let label = format!("{name} instance {index} (export {})", instance.entry_fref);
        let profile = profile(rom_bits, instance.trace.len());
        eprintln!(
            "{label}: {} captured rows, {} normalized rows, {} semantic steps; geometry rom 2^{} \
             ram 2^{}, {} steps of {} rows per segment",
            instance.captured.len(),
            instance.trace.len(),
            instance.steps.0.len(),
            profile.memory().r,
            profile.memory().mu,
            profile.memory().steps_per_segment(),
            profile.batch_size(),
        );

        let started = Instant::now();
        let prep = neo_wasm::nebula::preprocess_seeded_host_events_reduced_memory_test_only(
            params(rom_bits),
            profile,
            &execution.artifacts,
            entry_pc(&execution.templates.program_tables, instance.entry_fref)?,
            &execution.templates.bindings,
            instance.entry_fref,
            PREPROCESSING_SEED,
            CommChainState::default(),
        )
        .map_err(|error| wasmtime::format_err!("{label}: preprocessing failed: {error}"))?;
        let preprocessing = started.elapsed();
        verified.timings.preprocessing += preprocessing;
        let structure = prep.inner().relation().structure();
        eprintln!(
            "{label}: preprocessing took {:?} (relation {} rows x {} columns)",
            preprocessing, structure.n, structure.m
        );

        let started = Instant::now();
        let proof = prover
            .prove(&prep, &instance.trace)
            .map_err(|error| wasmtime::format_err!("{label}: proving failed: {error}"))?;
        let proving = started.elapsed();
        verified.timings.proving += proving;
        eprintln!(
            "{label}: proving took {:?} on {} ({} folded steps){}",
            proving,
            prover.backend().as_str(),
            proof.inner().state.step_count,
            prover
                .fallback_reason()
                .map(|reason| format!("; fell back: {reason}"))
                .unwrap_or_default()
        );

        let Some(final_state) = instance.trace.last().map(|row| row.state_after) else {
            bail!("{label}: empty normalized trace");
        };
        let started = Instant::now();
        prover
            .verify(&prep, &proof, final_state)
            .map_err(|error| wasmtime::format_err!("{label}: verification failed: {error}"))?;
        let verification = started.elapsed();
        verified.timings.verification += verification;
        eprintln!("{label}: verification took {verification:?}");

        assert!(
            verified
                .roots
                .insert(coroutine_id(index), final_state.comm_chain)
                .is_none(),
            "each coroutine must have exactly one verified Wasm proof"
        );

        // The proven final chain is the fold of exactly the blocks the decoder
        // consumed, so the semantic steps are bound to the proof.
        let absorbed = absorbed_event_blocks(&instance.trace)
            .into_iter()
            .map(|block| block.words.map(F::new))
            .collect::<Vec<_>>();
        let expected_chain =
            fold_event_blocks(CommChainState::default(), &absorbed).canonical_u64();
        if final_state.comm_chain != expected_chain {
            bail!("{label}: the decoded transcript does not fold to the proven chain");
        }
        let mut tampered = final_state;
        tampered.comm_chain[0] = tampered.comm_chain[0].wrapping_add(1);
        if prover.verify(&prep, &proof, tampered).is_ok() {
            bail!("{label}: verification accepted a tampered transcript claim");
        }
    }
    Ok(verified)
}

/// Print how many ROM/RAM preload cells each program table needs, to size the
/// Nebula geometry. Host-event tables are added on top by preprocessing.
#[tokio::test]
#[ignore = "diagnostic; run with -- --ignored --nocapture"]
async fn print_program_table_demand() -> wasmtime::Result<()> {
    for (name, source) in [("counter", MINIMAL_METHOD_CALL), ("score", SCORE)] {
        let execution = trace_coordination_script(source, "example").await?;
        // Preprocessing lays every table out as a dense region of
        // `prod(2^bits(max component + 1))` cells, so report both the sparse
        // entry count and the dense region size.
        let mut tables = std::collections::BTreeMap::<String, (usize, Vec<u64>)>::new();
        let mut preload = neo_wasm::preload_from_program_artifacts(&execution.artifacts);
        neo_wasm::preload_host_event_tables(&mut preload, &execution.templates.bindings);
        for (memory, address, _) in preload.entries() {
            let (count, maxima) = tables.entry(format!("{memory:?}")).or_default();
            *count += 1;
            maxima.resize(maxima.len().max(address.len()), 0);
            for (component, &value) in address.iter().enumerate() {
                maxima[component] = maxima[component].max(u64::from(value) + 1);
            }
        }
        let mut dense_total = 0_u64;
        let mut entry_total = 0;
        for (memory, (count, maxima)) in &tables {
            let dense: u64 = maxima
                .iter()
                .map(|&bound| bound.max(2).next_power_of_two())
                .product();
            dense_total += dense;
            entry_total += count;
            eprintln!("  {memory}: {count} entries, dense {dense} cells (bounds {maxima:?})");
        }
        eprintln!("{name}: {entry_total} program-table entries, {dense_total} dense cells");
        let tables = &execution.templates.program_tables;
        eprintln!(
            "  decode rows {}, pc rom {}, function entries {}, imports templates {}, export templates {}",
            tables.program_decode.len(),
            tables.pc_rom.len(),
            tables.function_entries.len(),
            execution.templates.bindings.imports.len(),
            execution.templates.bindings.exports.len(),
        );
    }
    Ok(())
}

#[tokio::test]
#[ignore = "full Nebula proofs; run with --release -- --ignored --nocapture"]
async fn counter_execution_proves_and_verifies() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
    prove_execution("counter", 14, &execution).map(|_| ())
}

#[tokio::test]
#[ignore = "full Nebula proofs; run with --release -- --ignored --nocapture"]
async fn score_execution_proves_and_verifies() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(SCORE, "example").await?;
    prove_execution("score", 14, &execution).map(|_| ())
}

#[cfg(target_vendor = "apple")]
#[tokio::test]
#[ignore = "all Wasm proofs plus interleaving proof on Metal; run with --release --ignored"]
async fn counter_full_transaction_proves_and_verifies() -> wasmtime::Result<()> {
    use common::{STORAGE_COUNTER, interleaving_params, storage_counter_statement};
    use starstream_interleaving_prover::proving::TransactionProofContext;

    let total_started = Instant::now();
    let execution = trace_coordination_script(STORAGE_COUNTER, "example").await?;
    let tracing = total_started.elapsed();
    let statement = storage_counter_statement();
    assert_eq!(execution.instances.len(), 2, "coordinator and one UTXO");
    assert_eq!(execution.transaction.statement, statement);
    starstream_interleaving_prover::verify_transaction_sat(
        &execution.transaction.trace,
        3,
        &statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("transaction circuit check failed: {error}"))?;
    eprintln!("full transaction: compile, execute and trace took {tracing:?}");

    // Retain only authenticated roots after each Wasm verification, so large
    // preprocessing contexts and witnesses can be released between instances.
    let wasm = prove_execution("storage counter", 14, &execution)?;
    assert_eq!(wasm.roots.keys().copied().collect::<Vec<_>>(), vec![1, 2]);
    assert_eq!(wasm.roots, execution.commitments);

    let started = Instant::now();
    let context = TransactionProofContext::new(3, interleaving_params(), [7; 32])
        .map_err(|error| wasmtime::format_err!("interleaving preprocessing failed: {error}"))?;
    let preprocessing = started.elapsed();
    eprintln!("interleaving: preprocessing took {preprocessing:?}");
    let mut adapter = neo_prover_metal::MetalNifsProver::new()?;
    let started = Instant::now();
    let proof = context
        .prove_with_nifs_adapter(
            &mut adapter,
            &execution.transaction.trace,
            &statement,
            &execution.commitments,
        )
        .map_err(|error| wasmtime::format_err!("interleaving proving failed: {error}"))?;
    let proving = started.elapsed();
    assert!(
        adapter.session().activity().dispatches > 0,
        "interleaving proof must use Metal"
    );
    eprintln!("interleaving: proving took {proving:?}");

    // Verification takes the roots authenticated by the Wasm proofs, rather
    // than trusting the trace's proposed roots used during proving.
    let started = Instant::now();
    context
        .verify_with_opening_backend(
            &proof,
            &statement,
            &wasm.roots,
            adapter.final_witness_opening_backend(),
        )
        .map_err(|error| wasmtime::format_err!("combined verification failed: {error}"))?;
    let verification = started.elapsed();
    eprintln!("interleaving: verification took {verification:?}");

    for &id in wasm.roots.keys() {
        let mut missing = wasm.roots.clone();
        missing.remove(&id);
        assert!(
            context.verify(&proof, &statement, &missing).is_err(),
            "missing proof for coroutine {id}"
        );
        let mut changed = wasm.roots.clone();
        changed.get_mut(&id).unwrap()[0] ^= 1;
        assert!(
            context.verify(&proof, &statement, &changed).is_err(),
            "changed root for coroutine {id}"
        );
    }
    let mut swapped = wasm.roots.clone();
    swapped.insert(1, wasm.roots[&2]);
    swapped.insert(2, wasm.roots[&1]);
    assert!(
        context.verify(&proof, &statement, &swapped).is_err(),
        "roots assigned to the wrong coroutines"
    );
    let mut wrong_storage = statement.clone();
    wrong_storage.outputs[0].storage.0[0] += 1;
    assert!(context.verify(&proof, &wrong_storage, &wasm.roots).is_err());

    eprintln!(
        "full transaction: preprocessing {:?}, proving {:?}, verification {:?}; \
         compile/execute/trace {:?}; total wall time {:?}",
        wasm.timings.preprocessing + preprocessing,
        wasm.timings.proving + proving,
        wasm.timings.verification + verification,
        tracing,
        total_started.elapsed(),
    );
    Ok(())
}
