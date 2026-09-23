//! Small demo-only wire format for editing normalized rows without changing Neo-Wasm's IR API.

use serde_json::Value;
use wasmtime::{bail, ensure};

use crate::common::TracedExecution;
use crate::model::normalized_row;

fn words<const N: usize>(value: &Value, name: &str) -> wasmtime::Result<[u64; N]> {
    let entries = value
        .as_array()
        .ok_or_else(|| wasmtime::format_err!("{name} must be an array"))?;
    ensure!(entries.len() == N, "{name} needs {N} words");
    entries
        .iter()
        .enumerate()
        .map(|(index, entry)| {
            entry
                .as_str()
                .ok_or_else(|| wasmtime::format_err!("{name}[{index}] must be a decimal string"))?
                .parse::<u64>()
                .map_err(Into::into)
        })
        .collect::<wasmtime::Result<Vec<_>>>()?
        .try_into()
        .map_err(|_| wasmtime::format_err!("{name} needs {N} words"))
}

fn same_fields(edited: &Value, before: &Value, name: &str) -> wasmtime::Result<()> {
    let edited = edited
        .as_object()
        .ok_or_else(|| wasmtime::format_err!("{name} must be an object"))?;
    let before = before.as_object().unwrap();
    ensure!(edited.keys().eq(before.keys()), "{name} fields changed");
    Ok(())
}

pub fn apply(
    execution: &mut TracedExecution,
    target: usize,
    index: usize,
    raw: &str,
) -> wasmtime::Result<Value> {
    let row = execution
        .instances
        .get_mut(target)
        .and_then(|instance| instance.trace.get_mut(index))
        .ok_or_else(|| wasmtime::format_err!("Unknown normalized row"))?;
    let edited: Value = serde_json::from_str(raw)?;
    let before = normalized_row(row, index);
    let mut candidate = row.clone();
    let mut unchanged = edited.clone();
    for key in ["gather_value", "stack_read0", "state_before", "state_after"] {
        unchanged[key] = before[key].clone();
    }
    ensure!(
        unchanged == before,
        "Only the displayed witness values can be edited"
    );
    for key in ["state_before", "state_after"] {
        same_fields(&edited[key], &before[key], key)?;
    }

    let before_state = &edited["state_before"];
    let after_state = &edited["state_after"];
    let before_evbuf = words::<8>(&before_state["evbuf"], "state_before.evbuf")?;
    let mut after_evbuf = words::<8>(&after_state["evbuf"], "state_after.evbuf")?;
    let before_chain = words::<4>(&before_state["comm_chain"], "state_before.comm_chain")?;
    let after_chain = words::<4>(&after_state["comm_chain"], "state_after.comm_chain")?;
    let before_perm = words::<12>(&before_state["perm_state"], "state_before.perm_state")?;
    let after_perm = words::<12>(&after_state["perm_state"], "state_after.perm_state")?;
    for word in before_evbuf
        .into_iter()
        .chain(after_evbuf)
        .chain(before_chain)
        .chain(after_chain)
        .chain(before_perm)
        .chain(after_perm)
    {
        ensure!(
            word < starstream_interleaving_spec::FIELD_MODULUS,
            "Witness field word is noncanonical"
        );
    }

    if let Some(cursor) = before["slot_cursor"].as_u64() {
        let cursor = usize::try_from(cursor)?;
        ensure!(cursor < 8, "Invalid gather cursor");
        let edited_gather = edited["gather_value"]
            .as_str()
            .ok_or_else(|| wasmtime::format_err!("gather_value must be a decimal string"))?
            .parse::<u64>()?;
        ensure!(
            edited_gather < starstream_interleaving_spec::FIELD_MODULUS,
            "Gather word is noncanonical"
        );
        let old_gather = row.state_after.event_absorb.evbuf[cursor];
        if edited_gather != old_gather {
            ensure!(
                after_evbuf[cursor] == old_gather || after_evbuf[cursor] == edited_gather,
                "gather_value and state_after.evbuf disagree"
            );
            after_evbuf[cursor] = edited_gather;
        }
    } else {
        ensure!(
            edited["gather_value"] == before["gather_value"],
            "This row has no gather value"
        );
    }

    match (
        candidate.stack_read0.as_mut(),
        edited["stack_read0"].as_object(),
    ) {
        (Some(read), Some(value)) => {
            same_fields(
                &edited["stack_read0"],
                &before["stack_read0"],
                "stack_read0",
            )?;
            ensure!(
                value["addr_lo"] == before["stack_read0"]["addr_lo"],
                "Stack address editing is unsupported"
            );
            read.value_lo = u32::try_from(
                value["value_lo"]
                    .as_u64()
                    .ok_or_else(|| wasmtime::format_err!("stack_read0.value_lo must be a u32"))?,
            )?;
            read.value_hi = if value["value_hi"].is_null() {
                None
            } else {
                Some(u32::try_from(value["value_hi"].as_u64().ok_or_else(
                    || wasmtime::format_err!("stack_read0.value_hi must be a u32"),
                )?)?)
            };
        }
        (None, None) => {}
        _ => bail!("Cannot add or remove a stack read"),
    }
    candidate.state_before.event_absorb.evbuf = before_evbuf;
    candidate.state_after.event_absorb.evbuf = after_evbuf;
    candidate.state_before.comm_chain = before_chain;
    candidate.state_after.comm_chain = after_chain;
    candidate.state_before.event_absorb.perm_state = before_perm;
    candidate.state_after.event_absorb.perm_state = after_perm;
    let view = normalized_row(&candidate, index);
    *row = candidate;
    Ok(view)
}
