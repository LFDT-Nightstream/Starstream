use crate::RuntimeState;
use starstream_interleaving_spec::{
    InterleavingInstance, ProcessId, REF_PUSH_WIDTH, REF_WRITE_WIDTH, Ref, Value, WitEffectOutput,
    WitLedgerEffect,
};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{env, fs, time};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DecodeMode {
    None,
    ResponseOnly,
    RequestAndResponse,
}

pub fn emit_trace_mermaid(instance: &InterleavingInstance, state: &RuntimeState) {
    if env::var("STARSTREAM_RUNTIME_TRACE_MERMAID")
        .ok()
        .filter(|v| v != "0")
        .is_none()
    {
        return;
    }

    if state.interleaving.is_empty() {
        return;
    }

    let labels = build_process_labels(&instance.is_utxo);
    let mut out = String::new();
    out.push_str("sequenceDiagram\n");
    for label in &labels {
        out.push_str(&format!("    participant {label}\n"));
    }

    let mut ref_store: HashMap<Ref, Vec<Value>> = HashMap::new();
    let mut ref_state: HashMap<ProcessId, (Ref, usize, usize)> = HashMap::new();
    let mut handler_targets: HashMap<ProcessId, ProcessId> = HashMap::new();

    for (idx, (pid, effect)) in state.interleaving.iter().enumerate() {
        if let Some(line) = format_edge_line(
            idx,
            &labels,
            &instance.is_utxo,
            *pid,
            effect,
            &ref_store,
            &handler_targets,
            &state.interleaving,
        ) {
            out.push_str("    ");
            out.push_str(&line);
            out.push('\n');
        }

        apply_ref_mutations(*pid, effect, &mut ref_store, &mut ref_state);
        update_handler_targets(*pid, effect, &mut handler_targets);
    }

    let ts = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let mmd_path = env::temp_dir().join(format!("starstream_trace_{ts}.mmd"));
    if let Err(err) = fs::write(&mmd_path, out) {
        eprintln!("mermaid: failed to write {}: {err}", mmd_path.display());
        return;
    }

    let svg_path = PathBuf::from(mmd_path.with_extension("svg"));
    if mmdc_available() {
        let puppeteer_config_path = env::temp_dir().join(format!("starstream_mmdc_{ts}.json"));
        let puppeteer_config = r#"{"args":["--no-sandbox","--disable-setuid-sandbox"]}"#;
        let _ = fs::write(&puppeteer_config_path, puppeteer_config);

        match Command::new("mmdc")
            .arg("-p")
            .arg(&puppeteer_config_path)
            .arg("-i")
            .arg(&mmd_path)
            .arg("-o")
            .arg(&svg_path)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
        {
            Ok(status) if status.success() => {
                println!("mermaid svg: {}", svg_path.display());
                return;
            }
            Ok(status) => {
                eprintln!(
                    "mermaid: mmdc failed (exit={})",
                    status.code().unwrap_or(-1)
                );
            }
            Err(err) => {
                eprintln!("mermaid: failed to run mmdc: {err}");
            }
        }
    }

    println!("mermaid mmd: {}", mmd_path.display());
}

fn mmdc_available() -> bool {
    Command::new("mmdc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn build_process_labels(is_utxo: &[bool]) -> Vec<String> {
    let mut labels = Vec::with_capacity(is_utxo.len());
    let mut utxo_idx = 0usize;
    let mut coord_idx = 0usize;
    for (_pid, is_u) in is_utxo.iter().enumerate() {
        if *is_u {
            labels.push(format!("utxo{utxo_idx}"));
            utxo_idx += 1;
        } else {
            labels.push(format!("coord{coord_idx}"));
            coord_idx += 1;
        }
    }
    labels
}

fn format_edge_line(
    idx: usize,
    labels: &[String],
    is_utxo: &[bool],
    pid: ProcessId,
    effect: &WitLedgerEffect,
    ref_store: &HashMap<Ref, Vec<Value>>,
    handler_targets: &HashMap<ProcessId, ProcessId>,
    interleaving: &[(ProcessId, WitLedgerEffect)],
) -> Option<String> {
    let from = labels.get(pid.0)?;
    match effect {
        WitLedgerEffect::Resume { target, val, .. } => {
            let decode_mode = if handler_targets.get(&pid) == Some(target) {
                DecodeMode::RequestAndResponse
            } else if handler_targets.get(target) == Some(&pid) {
                DecodeMode::ResponseOnly
            } else if is_utxo.get(target.0).copied().unwrap_or(false) {
                DecodeMode::ResponseOnly
            } else {
                DecodeMode::None
            };
            let label = format!(
                "resume<br/>{}",
                format_ref_with_value(ref_store, *val, decode_mode)
            );
            let to = labels.get(target.0)?;
            Some(format!("{from} ->> {to}: {label}"))
        }
        WitLedgerEffect::Yield { val, .. } => {
            let next_pid = interleaving.get(idx + 1).map(|(p, _)| *p)?;
            let label = format!(
                "yield<br/>{}",
                format_ref_with_value(ref_store, *val, DecodeMode::None)
            );
            let to = labels.get(next_pid.0)?;
            Some(format!("{from} -->> {to}: {label}"))
        }
        WitLedgerEffect::NewUtxo { val, id, .. } => {
            let WitEffectOutput::Resolved(pid) = id else {
                return None;
            };
            let created = labels.get(pid.0)?;
            let label = format!("new_utxo<br/>{}", format_ref_with_values(ref_store, *val));
            Some(format!("{from} ->> {created}: {label}"))
        }
        WitLedgerEffect::NewCoord { val, id, .. } => {
            let WitEffectOutput::Resolved(pid) = id else {
                return None;
            };
            let created = labels.get(pid.0)?;
            let label = format!("new_coord<br/>{}", format_ref_with_values(ref_store, *val));
            Some(format!("{from} ->> {created}: {label}"))
        }
        _ => None,
    }
}

fn format_ref_with_value(
    ref_store: &HashMap<Ref, Vec<Value>>,
    reff: Ref,
    decode_mode: DecodeMode,
) -> String {
    let mut out = format!("ref={}", reff.0);
    if decode_mode == DecodeMode::None {
        return out;
    }
    if let Some(values) = ref_store.get(&reff) {
        if let Some(extra) = decode_cell_protocol(values, decode_mode) {
            out.push(' ');
            out.push('[');
            out.push_str(&extra);
            out.push(']');
        }
    }
    out
}

fn format_ref_with_values(ref_store: &HashMap<Ref, Vec<Value>>, reff: Ref) -> String {
    let mut out = format!("ref={}", reff.0);
    if let Some(values) = ref_store.get(&reff) {
        let v0 = values.get(0).map(|v| v.0).unwrap_or(0);
        let v1 = values.get(1).map(|v| v.0).unwrap_or(0);
        let v2 = values.get(2).map(|v| v.0).unwrap_or(0);
        let v3 = values.get(3).map(|v| v.0).unwrap_or(0);
        out.push(' ');
        out.push('[');
        out.push_str(&format!("vals={v0},{v1},{v2},{v3}"));
        out.push(']');
    }
    out
}

fn decode_cell_protocol(values: &[Value], decode_mode: DecodeMode) -> Option<String> {
    let disc = values.get(0)?.0;
    let v1 = values.get(1).map(|v| v.0).unwrap_or(0);
    let v2 = values.get(2).map(|v| v.0).unwrap_or(0);
    let is_request = matches!(disc, 1 | 2 | 3 | 4);
    let is_response = matches!(disc, 10 | 11 | 12 | 13);
    if is_request && decode_mode == DecodeMode::ResponseOnly {
        return None;
    }
    if decode_mode == DecodeMode::RequestAndResponse
        || (decode_mode == DecodeMode::ResponseOnly && is_response)
    {
        let label = match disc {
            1 => "disc=new_cell".to_string(),
            2 => format!("disc=write cell={v1} value={v2}"),
            3 => format!("disc=read cell={v1}"),
            4 => "disc=end".to_string(),
            10 => "disc=ack".to_string(),
            11 => format!("disc=new_cell_resp cell={v1}"),
            12 => format!("disc=read_resp value={v1}"),
            13 => "disc=end_ack".to_string(),
            _ => return None,
        };
        return Some(label);
    }
    None
}

fn apply_ref_mutations(
    pid: ProcessId,
    effect: &WitLedgerEffect,
    ref_store: &mut HashMap<Ref, Vec<Value>>,
    ref_state: &mut HashMap<ProcessId, (Ref, usize, usize)>,
) {
    match effect {
        WitLedgerEffect::NewRef { size, ret } => {
            if let WitEffectOutput::Resolved(ref_id) = ret {
                let size_words = *size;
                let size_elems = size_words * REF_PUSH_WIDTH;
                ref_store.insert(*ref_id, vec![Value(0); size_elems]);
                ref_state.insert(pid, (*ref_id, 0, size_words));
            }
        }
        WitLedgerEffect::RefPush { vals } => {
            if let Some((ref_id, offset, _size_words)) = ref_state.get_mut(&pid) {
                if let Some(store) = ref_store.get_mut(ref_id) {
                    let elem_offset = *offset;
                    for (i, val) in vals.iter().enumerate() {
                        if let Some(pos) = store.get_mut(elem_offset + i) {
                            *pos = *val;
                        }
                    }
                    *offset = elem_offset + REF_PUSH_WIDTH;
                }
            }
        }
        WitLedgerEffect::RefWrite { reff, offset, vals } => {
            if let Some(store) = ref_store.get_mut(reff) {
                let elem_offset = offset * REF_WRITE_WIDTH;
                for (i, val) in vals.iter().enumerate() {
                    if let Some(slot) = store.get_mut(elem_offset + i) {
                        *slot = *val;
                    }
                }
            }
        }
        _ => {}
    }
}

fn update_handler_targets(
    pid: ProcessId,
    effect: &WitLedgerEffect,
    handler_targets: &mut HashMap<ProcessId, ProcessId>,
) {
    if let WitLedgerEffect::GetHandlerFor { handler_id, .. } = effect {
        if let WitEffectOutput::Resolved(handler_id) = handler_id {
            handler_targets.insert(pid, *handler_id);
        }
    }
}
