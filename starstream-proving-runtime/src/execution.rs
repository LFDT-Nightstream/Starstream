use neo_wasm::comm_chain::{CommChainState, absorbed_event_blocks};
use starstream_interleaving_spec::interleaver::{
    InterleavedTransaction, interleave_transaction_with_inputs,
};
use starstream_interleaving_spec::{MethodHash, ResourceHandle, Trace};

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
    entry_handles: impl IntoIterator<Item = ResourceHandle>,
) -> wasmtime::Result<CapturedExecution> {
    decode_captured_transaction(registry, entry_handles, &[], &[])
}

/// Decode input UTXOs and tokens using the ledger's method lists and token owner indices.
pub fn decode_captured_transaction(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    entry_handles: impl IntoIterator<Item = ResourceHandle>,
    input_methods: &[Vec<MethodHash>],
    input_token_owners: &[u32],
) -> wasmtime::Result<CapturedExecution> {
    let mut entry_handles = entry_handles.into_iter();
    let mut traces = Vec::new();
    for (instance, captured) in registry
        .instances()
        .map_err(|error| wasmtime::format_err!("trace capture failed: {error}"))?
    {
        let trace = decode_captured_instance(captured, &mut entry_handles).map_err(|error| {
            wasmtime::format_err!("failed to decode instance {instance}: {error:#}")
        })?;
        traces.push(trace);
    }
    let transaction =
        interleave_transaction_with_inputs(&traces, input_methods, input_token_owners)
            .map_err(|error| wasmtime::format_err!("failed to interleave traces: {error}"))?;
    Ok(CapturedExecution {
        traces,
        transaction,
    })
}

/// Recover the semantic steps of one captured guest, supplying its root-local input handle on load.
pub fn decode_captured_instance(
    captured: &neo_wasm::WasmtimeTraceState,
    entry_handles: &mut impl Iterator<Item = ResourceHandle>,
) -> wasmtime::Result<Trace> {
    let trace = neo_wasm::traces_from_wasmtime_steps_with_host_events(
        captured.steps(),
        captured.artifacts(),
        CommChainState::default(),
    )
    .map_err(|error| wasmtime::format_err!("failed to normalize Wasm trace: {error}"))?;
    let blocks = absorbed_event_blocks(&trace)
        .into_iter()
        .map(|block| block.words)
        .collect::<Vec<_>>();
    Ok(decode_tagged_blocks(&blocks, entry_handles)?)
}

/// Check the interleaved execution reconstructed from the captured calls.
pub fn check_captured_transaction(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    entry_handles: impl IntoIterator<Item = ResourceHandle>,
    input_methods: &[Vec<MethodHash>],
    input_token_owners: &[u32],
) -> wasmtime::Result<CapturedExecution> {
    let execution =
        decode_captured_transaction(registry, entry_handles, input_methods, input_token_owners)?;
    starstream_interleaving_prover::verify_sat(
        &execution.transaction.execution,
        &execution.transaction.statement.signers,
    )
    .map_err(|error| wasmtime::format_err!("interleaving relation rejected execution: {error}"))?;
    Ok(execution)
}
