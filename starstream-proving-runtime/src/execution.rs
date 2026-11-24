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
    entry_utxo_handles: impl IntoIterator<Item = ResourceHandle>,
) -> wasmtime::Result<CapturedExecution> {
    decode_captured_transaction(registry, entry_utxo_handles, &[])
}

/// Decode loaded inputs followed by constructed UTXOs, using the ledger's ABI lists.
pub fn decode_captured_transaction(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    entry_utxo_handles: impl IntoIterator<Item = ResourceHandle>,
    input_methods: &[Vec<MethodHash>],
) -> wasmtime::Result<CapturedExecution> {
    let mut entry_utxo_handles = entry_utxo_handles.into_iter();
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
        let trace = decode_tagged_blocks(&blocks, &mut entry_utxo_handles).map_err(|error| {
            wasmtime::format_err!("failed to decode instance {instance}: {error}")
        })?;
        traces.push(trace);
    }
    let transaction = interleave_transaction_with_inputs(&traces, input_methods)
        .map_err(|error| wasmtime::format_err!("failed to interleave traces: {error}"))?;
    Ok(CapturedExecution {
        traces,
        transaction,
    })
}

/// Check the interleaved execution reconstructed from the captured calls.
pub fn check_captured_transaction(
    registry: &neo_wasm::WasmtimeTraceRegistry,
    entry_utxo_handles: impl IntoIterator<Item = ResourceHandle>,
    input_methods: &[Vec<MethodHash>],
) -> wasmtime::Result<CapturedExecution> {
    let execution = decode_captured_transaction(registry, entry_utxo_handles, input_methods)?;
    starstream_interleaving_prover::verify_sat(
        &execution.transaction.execution,
        &execution.transaction.statement.signers,
    )
    .map_err(|error| wasmtime::format_err!("interleaving relation rejected execution: {error}"))?;
    Ok(execution)
}
