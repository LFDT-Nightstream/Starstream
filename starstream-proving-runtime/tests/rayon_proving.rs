#![cfg(feature = "proof-tests")]

mod common;

use rayon_core::ThreadPoolBuilder;
use starstream_interleaving_prover::CoroutineId;
use starstream_proving_runtime::{Engine, TransactionProver, decode_captured_execution};

#[tokio::test]
#[ignore = "Nebula proofs; run with --release --features proof-tests -- --ignored"]
async fn warm_proof_on_the_only_global_rayon_worker_verifies() -> wasmtime::Result<()> {
    ThreadPoolBuilder::new().num_threads(1).build_global()?;
    let (_, registry) =
        common::trace_coordination_script_with_registry(common::MINIMAL_METHOD_CALL, "example")
            .await?;
    let execution = decode_captured_execution(&registry, [])?;
    let mut prover = TransactionProver::new(Engine::Optimized);
    let (cold, _) = ThreadPoolBuilder::new()
        .build()?
        .install(|| prover.prove(&registry, &execution))?;
    let expected = cold.interleaving.to_bytes()?;
    let sizes = cold.through_bytes()?.1.wasm;
    drop(cold);

    let (warm, timings) = rayon_core::scope(|_| {
        assert_eq!(rayon_core::current_thread_index(), Some(0));
        assert_eq!(rayon_core::current_num_threads(), 1);
        prover.prove(&registry, &execution)
    })?;
    assert!(timings.interleaving_circuit_reused);
    assert!(timings.wasm.iter().all(|phase| phase.circuit_reused));
    let (warm, warm_sizes) = warm.through_bytes()?;
    assert_eq!(warm_sizes.wasm, sizes);
    assert!(warm.interleaving.to_bytes()? == expected);
    let ids = [CoroutineId::Coord(1), CoroutineId::Utxo(0)].map(CoroutineId::encoded);
    assert!(
        warm.instances
            .iter()
            .map(|instance| instance.coroutine)
            .eq(ids)
    );
    prover.verify(&warm, &execution.transaction.statement)
}
