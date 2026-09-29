use neo_wasm::comm_chain::{CommChainState, absorbed_event_blocks};
use starstream_interleaving_spec::interleaver::{InterleavedTransaction, interleave_transaction};
use starstream_interleaving_spec::{ResourceHandle, Trace};

use crate::decode_tagged_blocks;

/// Semantic traces recovered from the registered components' instances.
#[derive(Debug)]
pub struct CapturedExecution {
    /// One trace per captured core Wasm instance, in Wasmtime instance order.
    pub traces: Vec<Trace>,
    /// The traces interleaved with the transaction lifecycle steps synthesized.
    pub transaction: InterleavedTransaction,
}

/// Normalize and decode all instances registered for a traced
/// coordination-script call.
pub fn decode_captured_execution(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    coordinator_handles: impl IntoIterator<Item = ResourceHandle>,
) -> wasmtime::Result<CapturedExecution> {
    let mut coordinator_handles = coordinator_handles.into_iter();
    let mut traces = Vec::new();
    for (instance, captured) in registry
        .instances()
        .map_err(|error| wasmtime::format_err!("trace capture failed: {error}"))?
    {
        let trace = neo_wasm::traces_from_wasmtime_steps_with_host_events(
            captured.steps(),
            captured.artifacts(),
            CommChainState::default(),
        )
        .map_err(|error| {
            wasmtime::format_err!("failed to normalize instance {instance}: {error}")
        })?;
        let blocks = absorbed_event_blocks(&trace);
        let blocks = blocks.iter().map(|block| block.words).collect::<Vec<_>>();
        let trace = decode_tagged_blocks(&blocks, &mut coordinator_handles).map_err(|error| {
            wasmtime::format_err!("failed to decode instance {instance}: {error}")
        })?;
        traces.push(trace);
    }
    let transaction = interleave_transaction(&traces)
        .map_err(|error| wasmtime::format_err!("failed to interleave traces: {error}"))?;
    Ok(CapturedExecution {
        traces,
        transaction,
    })
}

/// Decode a traced coordination-script call and check its execution relation.
///
/// TODO: This is host-side validation only. Transaction I/O and recursive proof
/// binding are intentionally outside this initial integration.
#[cfg(feature = "interleaving-check")]
pub fn check_captured_execution(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    coordinator_handles: impl IntoIterator<Item = ResourceHandle>,
) -> wasmtime::Result<CapturedExecution> {
    let execution = decode_captured_execution(registry, coordinator_handles)?;
    starstream_interleaving_prover::verify_sat(&execution.transaction.execution).map_err(
        |error| wasmtime::format_err!("interleaving relation rejected execution: {error}"),
    )?;
    Ok(execution)
}
