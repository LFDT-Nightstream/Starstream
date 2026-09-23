use std::collections::HashMap;

use serde::Serialize;
use serde_json::{Value, json};
use starstream_interleaving_spec::interleaver::InterleavedTransaction;
use starstream_interleaving_spec::{ResourceHandle, Step, events};

use crate::common::{TracedExecution, coroutine_id};

pub const COUNTER: &str = include_str!("programs/counter.star");
pub const COORDINATOR: &str = include_str!("programs/coordinator.star");
pub const SIMPLE_COORDINATOR: &str = include_str!("programs/simple-coordinator.star");
pub const ORACLE: &str = include_str!("programs/oracle.star");
pub const CONSUMER: &str = include_str!("programs/consumer.star");

pub fn sources(example: &str) -> Option<Vec<Value>> {
    let files = match example {
        "small" => vec![
            ("Coord.star", SIMPLE_COORDINATOR),
            ("Counter.star", COUNTER),
        ],
        "dynamic" => vec![
            ("Coord.star", COORDINATOR),
            ("Oracle.star", ORACLE),
            ("Consumer.star", CONSUMER),
        ],
        _ => return None,
    };
    Some(
        files
            .into_iter()
            .map(|(name, source)| json!({"name": name, "source": source.trim()}))
            .collect(),
    )
}

pub fn source(example: &str) -> Option<String> {
    let sources = sources(example)?;
    sources
        .iter()
        .skip(1)
        .chain(sources.first())
        .map(|file| file["source"].as_str().map(str::to_owned))
        .collect::<Option<Vec<_>>>()
        .map(|files| files.join("\n"))
}

pub fn root(words: [u64; 4]) -> [String; 4] {
    words.map(|word| format!("{word:016x}"))
}

pub fn label(index: usize) -> String {
    if index == 0 {
        "Coord".into()
    } else {
        format!("UTXO {}", index - 1)
    }
}

#[derive(Default, Serialize)]
pub struct ProofStatus {
    pub label: String,
    pub status: String,
    pub preprocessing_ms: Option<f64>,
    pub proving_ms: Option<f64>,
    pub verification_ms: Option<f64>,
    pub root: Option<[String; 4]>,
}

#[derive(Default, Serialize)]
pub struct State {
    pub run_id: u64,
    pub revision: u64,
    pub busy: bool,
    pub active: Option<usize>,
    pub phase: String,
    pub phase_started_ms: u64,
    pub error: Option<String>,
    pub proofs: Vec<ProofStatus>,
    pub trace_checks: Vec<Option<Value>>,
    pub transaction_check: Option<Value>,
    pub verification: Option<Value>,
    #[serde(skip)]
    pub execution: Option<Value>,
}

// JSON numbers cannot carry arbitrary field elements through a JS Number.
fn exact_json(value: &mut Value) {
    match value {
        Value::Number(n) if n.as_u64().is_some_and(|n| n > (1 << 53) - 1) => {
            *value = Value::String(n.to_string());
        }
        Value::Array(values) => values.iter_mut().for_each(exact_json),
        Value::Object(values) => values.values_mut().for_each(exact_json),
        _ => {}
    }
}

fn storage_value(value: &wasmtime::component::Val) -> String {
    use wasmtime::component::Val;
    match value {
        Val::S64(value) => value.to_string(),
        Val::U64(value) => value.to_string(),
        Val::S32(value) => value.to_string(),
        Val::U32(value) => value.to_string(),
        Val::Bool(value) => value.to_string(),
        _ => format!("{value:?}"),
    }
}

pub(crate) fn normalized_row(row: &neo_wasm::WasmVmStep, index: usize) -> Value {
    let slot = row.host_event_rom_slot;
    let cursor = usize::from(row.state_before.host_events.slot_cursor);
    json!({
        "index": index,
        "kind": format!("{:?}", row.row_kind),
        "opcode": row.info.name,
        "pc": row.state_before.pc.to_string(),
        "function": row.current_function_ref,
        "slot": slot.map(|slot| format!("{:?}", slot.kind)),
        "slot_cursor": slot.map(|_| cursor),
        "gather_value": slot.map(|_| row.state_after.event_absorb.evbuf[cursor].to_string()),
        "stack_read0": row.stack_read0.map(|read| json!({
            "addr_lo": read.addr_lo.to_string(), "value_lo": read.value_lo, "value_hi": read.value_hi,
        })),
        "stack_write0": row.stack_write0.map(|write| json!({
            "addr_lo": write.addr_lo.to_string(), "value_lo": write.value_lo, "value_hi": write.value_hi,
        })),
        "state_before": {
            "comm_chain": row.state_before.comm_chain.map(|word| word.to_string()),
            "evbuf": row.state_before.event_absorb.evbuf.map(|word| word.to_string()),
            "perm_state": row.state_before.event_absorb.perm_state.map(|word| word.to_string()),
        },
        "state_after": {
            "comm_chain": row.state_after.comm_chain.map(|word| word.to_string()),
            "evbuf": row.state_after.event_absorb.evbuf.map(|word| word.to_string()),
            "perm_state": row.state_after.event_absorb.perm_state.map(|word| word.to_string()),
        },
    })
}

pub(crate) fn event(step: &Step, index: usize) -> Value {
    let method = match step {
        Step::CallMethod { method, .. }
        | Step::EnterMethod { method, .. }
        | Step::RegisterMethod { method }
        | Step::ReadAbi { method } => ["read", "increment", "add"]
            .into_iter()
            .find(|name| starstream_proving_runtime::method_hash_from_name(name) == *method),
        _ => None,
    };
    let mut payload = serde_json::to_value(step).expect("step serialization");
    exact_json(&mut payload);
    json!({"index": index, "kind": payload["event"], "method_name": method, "payload": payload,
        "blocks": events::encode(step).iter().map(|block| block.map(|word| word.to_string())).collect::<Vec<_>>()})
}

fn interleaving_origins(transaction: &InterleavedTransaction) -> Vec<Option<usize>> {
    let mut origins = Vec::with_capacity(transaction.trace.0.len());
    let mut current = 0;
    let mut next_utxo = 1;
    let mut callers = Vec::new();
    let mut resources = HashMap::<(usize, ResourceHandle), usize>::new();
    for step in &transaction.execution.0 {
        origins.push(Some(current));
        match step {
            Step::NewUtxo { resource, .. } => {
                callers.push((current, Some((resource.0, next_utxo))));
                current = next_utxo;
                next_utxo += 1;
            }
            Step::CallMethod { resource, .. } => {
                callers.push((current, None));
                current = resources[&(current, *resource)];
            }
            Step::Return { .. } => {
                if let Some((caller, pending)) = callers.pop() {
                    if let Some((resource, utxo)) = pending {
                        resources.insert((caller, resource), utxo);
                    }
                    current = caller;
                }
            }
            _ => {}
        }
    }
    let mut finalizing = 1;
    for step in transaction.trace.0.iter().skip(origins.len()) {
        origins.push(match step {
            Step::GetStorage { .. } => Some(finalizing),
            _ => None,
        });
        if matches!(step, Step::GetStorage { .. } | Step::SkipConsumed) {
            finalizing += 1;
        }
    }
    origins
}

pub fn interleaving_rows(transaction: &InterleavedTransaction) -> Vec<Value> {
    transaction
        .trace
        .0
        .iter()
        .zip(interleaving_origins(transaction))
        .enumerate()
        .map(|(i, (step, origin))| {
            let mut row = event(step, i);
            row["source"] = json!(origin.map(label).unwrap_or_else(|| "Transaction".into()));
            row
        })
        .collect()
}

pub fn execution_view(
    execution: &TracedExecution,
    example: &str,
    elapsed_ms: f64,
) -> wasmtime::Result<Value> {
    let core = starstream_proving_runtime::first_core_module(&execution.component_wasm)?;
    let core_wat = wasmprinter::print_bytes(core)
        .map_err(|error| wasmtime::format_err!("failed to print core Wasm: {error:#}"))?;
    let component_wat = wasmprinter::print_bytes(&execution.component_wasm)
        .map_err(|error| wasmtime::format_err!("failed to print component: {error:#}"))?;
    let traces = execution.instances.iter().enumerate().map(|(index, instance)| {
        let captured = instance.captured.iter().map(|row| json!({
            "index": row.step, "opcode": row.opcode, "pc": row.pc,
            "function": row.current_function_ref,
            "stack": row.operand_stack, "locals": row.locals,
        })).collect::<Vec<_>>();
        json!({"id": index, "coroutine_id": coroutine_id(index), "label": label(index),
            "root": root(execution.commitments[&coroutine_id(index)]),
            "captured": captured,
            "normalized": instance.trace.iter().enumerate().map(|(i, row)| normalized_row(row, i)).collect::<Vec<_>>(),
            "events": instance.steps.0.iter().enumerate().map(|(i, step)| event(step, i)).collect::<Vec<_>>()})
    }).collect::<Vec<_>>();
    let outputs = execution.transaction.statement.outputs.iter().zip(&execution.storage).map(|(output, fields)| {
        json!({"utxo": output.utxo, "storage_root": root(output.storage.0),
            "methods": output.methods.iter().map(|method| method.to_hex()).collect::<Vec<_>>(),
            "fields": fields.iter().map(|(name, value)| json!({"name": name, "value": storage_value(value)})).collect::<Vec<_>>()})
    }).collect::<Vec<_>>();
    let digest = starstream_interleaving_prover::transaction_commitment(
        &execution.transaction.statement,
        &execution.commitments,
    )
    .expect("checked transaction");
    Ok(
        json!({"example": example, "source": source(example), "elapsed_ms": elapsed_ms, "traces": traces, "outputs": outputs,
        "commitment": root(digest), "sources": sources(example),
        "generated": [
            {"name": "Core Wasm", "source": core_wat, "kind": "wat"},
            {"name": "Component", "source": component_wat, "kind": "wat"}
        ],
        "interleaving": interleaving_rows(&execution.transaction) }),
    )
}
