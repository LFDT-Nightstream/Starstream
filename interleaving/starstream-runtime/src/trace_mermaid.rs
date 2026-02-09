use crate::RuntimeState;
use starstream_interleaving_spec::{
    InterfaceId, InterleavingInstance, ProcessId, REF_PUSH_WIDTH, REF_WRITE_WIDTH, Ref, Value,
    WitEffectOutput, WitLedgerEffect,
};
use std::collections::HashMap;
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex, OnceLock};
use std::{env, fs, time};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DecodeMode {
    None,
    ResponseOnly,
    RequestAndResponse,
}

type MermaidDecoder = Arc<dyn Fn(&[Value]) -> Option<String> + Send + Sync + 'static>;

static MERMAID_DECODERS: OnceLock<Mutex<HashMap<InterfaceId, MermaidDecoder>>> = OnceLock::new();

pub fn register_mermaid_decoder(
    interface_id: InterfaceId,
    decoder: impl Fn(&[Value]) -> Option<String> + Send + Sync + 'static,
) {
    let map = MERMAID_DECODERS.get_or_init(|| Mutex::new(HashMap::new()));
    let mut map = map.lock().expect("mermaid decoder lock poisoned");
    map.insert(interface_id, Arc::new(decoder));
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
    let mut handler_interfaces: HashMap<ProcessId, InterfaceId> = HashMap::new();

    for (idx, (pid, effect)) in state.interleaving.iter().enumerate() {
        let ctx = EdgeContext {
            labels: &labels,
            is_utxo: &instance.is_utxo,
            ref_store: &ref_store,
            handler_targets: &handler_targets,
            handler_interfaces: &handler_interfaces,
            interleaving: &state.interleaving,
        };

        if let Some(line) = format_edge_line(idx, &ctx, *pid, effect) {
            out.push_str("    ");
            out.push_str(&line);
            out.push('\n');
        }

        apply_ref_mutations(*pid, effect, &mut ref_store, &mut ref_state);
        update_handler_targets(*pid, effect, &mut handler_targets, &mut handler_interfaces);
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

    let svg_path = mmd_path.with_extension("svg");
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
    for is_u in is_utxo.iter() {
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

struct EdgeContext<'a> {
    labels: &'a [String],
    is_utxo: &'a [bool],
    ref_store: &'a HashMap<Ref, Vec<Value>>,
    handler_targets: &'a HashMap<ProcessId, ProcessId>,
    handler_interfaces: &'a HashMap<ProcessId, InterfaceId>,
    interleaving: &'a [(ProcessId, WitLedgerEffect)],
}

fn format_edge_line(
    idx: usize,
    ctx: &EdgeContext<'_>,
    pid: ProcessId,
    effect: &WitLedgerEffect,
) -> Option<String> {
    let from = ctx.labels.get(pid.0)?;
    match effect {
        WitLedgerEffect::Resume { target, val, .. } => {
            let interface_id = ctx
                .handler_interfaces
                .get(target)
                .or_else(|| ctx.handler_interfaces.get(&pid));
            let decode_mode = if ctx.handler_targets.get(&pid) == Some(target) {
                DecodeMode::RequestAndResponse
            } else if ctx.handler_targets.get(target) == Some(&pid)
                || ctx.is_utxo.get(target.0).copied().unwrap_or(false)
            {
                DecodeMode::ResponseOnly
            } else {
                DecodeMode::None
            };
            let label = format!(
                "resume<br/>{}",
                format_ref_with_value(ctx.ref_store, *val, interface_id, decode_mode)
            );
            let to = ctx.labels.get(target.0)?;
            Some(format!("{from} ->> {to}: {label}"))
        }
        WitLedgerEffect::Yield { val, .. } => {
            let next_pid = ctx.interleaving.get(idx + 1).map(|(p, _)| *p)?;
            let label = format!(
                "yield<br/>{}",
                format_ref_with_value(ctx.ref_store, *val, None, DecodeMode::None)
            );
            let to = ctx.labels.get(next_pid.0)?;
            Some(format!("{from} -->> {to}: {label}"))
        }
        WitLedgerEffect::NewUtxo { val, id, .. } => {
            let WitEffectOutput::Resolved(pid) = id else {
                return None;
            };
            let created = ctx.labels.get(pid.0)?;
            let label = format!(
                "new_utxo<br/>{}",
                format_ref_with_values(ctx.ref_store, *val)
            );
            Some(format!("{from} ->> {created}: {label}"))
        }
        WitLedgerEffect::NewCoord { val, id, .. } => {
            let WitEffectOutput::Resolved(pid) = id else {
                return None;
            };
            let created = ctx.labels.get(pid.0)?;
            let label = format!(
                "new_coord<br/>{}",
                format_ref_with_values(ctx.ref_store, *val)
            );
            Some(format!("{from} ->> {created}: {label}"))
        }
        _ => None,
    }
}

fn format_ref_with_value(
    ref_store: &HashMap<Ref, Vec<Value>>,
    reff: Ref,
    interface_id: Option<&InterfaceId>,
    decode_mode: DecodeMode,
) -> String {
    let mut out = format!("ref={}", reff.0);
    if let Some(values) = ref_store.get(&reff) {
        let extra = interface_id
            .and_then(|id| decode_with_registry(id, values, decode_mode))
            .unwrap_or_else(|| format_raw_values(values));
        out.push(' ');
        out.push('[');
        out.push_str(&extra);
        out.push(']');
    }
    out
}

fn format_ref_with_values(ref_store: &HashMap<Ref, Vec<Value>>, reff: Ref) -> String {
    let mut out = format!("ref={}", reff.0);
    if let Some(values) = ref_store.get(&reff) {
        out.push(' ');
        out.push('[');
        out.push_str(&format_raw_values(values));
        out.push(']');
    }
    out
}

fn format_raw_values(values: &[Value]) -> String {
    let v0 = values.first().map(|v| v.0).unwrap_or(0);
    let v1 = values.get(1).map(|v| v.0).unwrap_or(0);
    let v2 = values.get(2).map(|v| v.0).unwrap_or(0);
    let v3 = values.get(3).map(|v| v.0).unwrap_or(0);
    format!("vals={v0},{v1},{v2},{v3}")
}

fn decode_with_registry(
    interface_id: &InterfaceId,
    values: &[Value],
    decode_mode: DecodeMode,
) -> Option<String> {
    if decode_mode == DecodeMode::None {
        return None;
    }
    let map = MERMAID_DECODERS.get_or_init(|| Mutex::new(HashMap::new()));
    let map = map.lock().ok()?;
    let decoder = map.get(interface_id)?.clone();
    decoder(values)
}

fn apply_ref_mutations(
    pid: ProcessId,
    effect: &WitLedgerEffect,
    ref_store: &mut HashMap<Ref, Vec<Value>>,
    ref_state: &mut HashMap<ProcessId, (Ref, usize, usize)>,
) {
    match effect {
        WitLedgerEffect::NewRef {
            size,
            ret: WitEffectOutput::Resolved(ref_id),
        } => {
            let size_words = *size;
            let size_elems = size_words * REF_PUSH_WIDTH;
            ref_store.insert(*ref_id, vec![Value(0); size_elems]);
            ref_state.insert(pid, (*ref_id, 0, size_words));
        }
        WitLedgerEffect::RefPush { vals } => {
            if let Some((ref_id, offset, _size_words)) = ref_state.get_mut(&pid)
                && let Some(store) = ref_store.get_mut(ref_id)
            {
                let elem_offset = *offset;
                for (i, val) in vals.iter().enumerate() {
                    if let Some(pos) = store.get_mut(elem_offset + i) {
                        *pos = *val;
                    }
                }
                *offset = elem_offset + REF_PUSH_WIDTH;
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
    handler_interfaces: &mut HashMap<ProcessId, InterfaceId>,
) {
    if let WitLedgerEffect::GetHandlerFor {
        handler_id,
        interface_id,
    } = effect
        && let WitEffectOutput::Resolved(handler_id) = handler_id
    {
        handler_targets.insert(pid, *handler_id);
        handler_interfaces.insert(*handler_id, *interface_id);
    }
}
