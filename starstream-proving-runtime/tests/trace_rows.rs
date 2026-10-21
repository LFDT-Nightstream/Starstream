//! Neo-Wasm trace-row census: how many rows each compiled Starstream contract
//! costs to prove, by row kind and by function.
//!
//! Neo-Wasm proves an execution by single-stepping it. Every executed Wasm
//! operator, structural ones included, is one trace row of the same width
//! whatever its opcode, and normalization adds aux rows: one per call
//! parameter, one per declared local, and the host-event gather and Poseidon
//! permutation rows. The dynamic row count is therefore the number a Wasm
//! optimizer is judged by, though host-event rows only shrink when the program
//! makes fewer or smaller host calls. This only traces and normalizes; it
//! neither folds nor proves.
//!
//! Run with:
//!
//! ```sh
//! cargo test -p starstream-proving-runtime --test trace_rows -- --ignored --nocapture
//! ```

mod common;

use std::collections::BTreeMap;

use neo_wasm::comm_chain::COMM_CHAIN_PERM_ROWS;
use neo_wasm::{WasmAuxOpcode, WasmOpcode, WasmRowKind, WasmVmStep};
use wasmparser::{CompositeInnerType, FuncType, Operator, Parser, Payload, TypeRef, ValType};

use crate::common::{
    DISTINCT_UTXO_TYPES, MINIMAL_METHOD_CALL, MULTI_UTXO, SCORE, STORAGE_COUNTER, TracedExecution,
    trace_coordination_script,
};

/// The measured contracts as `(name, source, coordination script)`.
///
/// Of `examples/*.star` only `score.star` runs here: the other files are
/// empty, import a linked library, or have scripts that take arguments.
const CONTRACTS: &[(&str, &str, &str)] = &[
    ("minimal_method_call", MINIMAL_METHOD_CALL, "example"),
    ("storage_counter", STORAGE_COUNTER, "example"),
    ("distinct_utxo_types", DISTINCT_UTXO_TYPES, "example"),
    ("multi_utxo", MULTI_UTXO, "example"),
    ("examples/score.star", SCORE, "example"),
];

/// Rows per folding step in `prove_wasm_instances`; the last batch is padded.
const BATCH_SIZE: usize = 16;

/// Functions listed per contract, by total rows.
const TOP_FUNCTIONS: usize = 12;

/// Opcodes listed per contract, by executed rows.
const TOP_OPCODES: usize = 10;

/// Prefix of the overflow-checked arithmetic intrinsics' names.
const CHECKED_INTRINSIC_PREFIX: &str = "__starstream_i64_";

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RowKind {
    Control,
    Variable,
    Const,
    Alu,
    Memory,
    Call,
    CallParamInit,
    LocalZero,
    TailEnter,
    EventGather,
    EventPerm,
    TurnBoundary,
    HostSuspend,
    HostResume,
    Padding,
}

impl RowKind {
    const ALL: [Self; 15] = [
        Self::Control,
        Self::Variable,
        Self::Const,
        Self::Alu,
        Self::Memory,
        Self::Call,
        Self::CallParamInit,
        Self::LocalZero,
        Self::TailEnter,
        Self::EventGather,
        Self::EventPerm,
        Self::TurnBoundary,
        Self::HostSuspend,
        Self::HostResume,
        Self::Padding,
    ];

    fn of(row: &WasmVmStep) -> Self {
        // Aux rows carry a `nop` opcode, so the row kind decides first.
        let WasmRowKind::Aux(aux) = row.row_kind else {
            return Self::of_opcode(row.opcode);
        };
        match aux {
            WasmAuxOpcode::CallParamInit => Self::CallParamInit,
            WasmAuxOpcode::LocalZero => Self::LocalZero,
            WasmAuxOpcode::TailEnter => Self::TailEnter,
            WasmAuxOpcode::HostEventGather => Self::EventGather,
            WasmAuxOpcode::HostEventPerm => Self::EventPerm,
            WasmAuxOpcode::TurnBoundary => Self::TurnBoundary,
            WasmAuxOpcode::HostSuspend => Self::HostSuspend,
            WasmAuxOpcode::HostResume => Self::HostResume,
            WasmAuxOpcode::Padding => Self::Padding,
        }
    }

    fn of_opcode(opcode: WasmOpcode) -> Self {
        use WasmOpcode::*;
        match opcode {
            Block | Loop | If | Else | End | Br | BrIf | BrTable | Nop | Return | Unreachable
            | Trap | Unsupported => Self::Control,
            LocalGet | LocalSet | LocalTee | GlobalGet | GlobalSet | Drop => Self::Variable,
            I32Const | I64Const | RefFunc => Self::Const,
            Call | CallIndirect | ReturnCall | ReturnCallIndirect => Self::Call,
            MemorySize | MemoryGrow | TableSize | TableGet | TableSet => Self::Memory,
            _ if opcode.uses_linear_memory() => Self::Memory,
            _ => Self::Alu,
        }
    }

    fn group(self) -> RowGroup {
        match self {
            Self::Control
            | Self::Variable
            | Self::Const
            | Self::Alu
            | Self::Memory
            | Self::Call => RowGroup::Wasm,
            Self::CallParamInit | Self::LocalZero | Self::TailEnter => RowGroup::FrameAux,
            Self::EventGather
            | Self::EventPerm
            | Self::TurnBoundary
            | Self::HostSuspend
            | Self::HostResume
            | Self::Padding => RowGroup::EventAux,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Control => "control (block/br/end/...)",
            Self::Variable => "local/global/drop",
            Self::Const => "const",
            Self::Alu => "alu/compare/convert",
            Self::Memory => "memory/table",
            Self::Call => "call",
            Self::CallParamInit => "aux call-param-init",
            Self::LocalZero => "aux local-zero",
            Self::TailEnter => "aux tail-enter",
            Self::EventGather => "aux event-gather",
            Self::EventPerm => "aux event-perm",
            Self::TurnBoundary => "aux turn-boundary",
            Self::HostSuspend => "aux host-suspend",
            Self::HostResume => "aux host-resume",
            Self::Padding => "aux padding (in trace)",
        }
    }
}

/// What a row is overhead of: an executed Wasm operator, setting up a call
/// frame, or committing host events (in-trace padding, never seen in
/// practice, is grouped with the events).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum RowGroup {
    Wasm,
    FrameAux,
    EventAux,
}

impl RowGroup {
    const ALL: [Self; 3] = [Self::Wasm, Self::FrameAux, Self::EventAux];

    fn label(self) -> &'static str {
        match self {
            Self::Wasm => "= wasm operator rows",
            Self::FrameAux => "= call frame aux rows",
            Self::EventAux => "= host event aux rows",
        }
    }
}

/// Rows attributed to one function across a contract's instances. Every row
/// counts once: call rows toward their target, all others toward the frame
/// they run in.
#[derive(Default)]
struct FunctionRows {
    /// Operators run in its frames, excluding the calls they make.
    wasm: usize,
    /// Call-param-init, local-zero and tail-enter rows of its frames.
    frame_aux: usize,
    /// Host-event rows run in its frames.
    event_aux: usize,
    /// Executed call rows targeting it, one per call.
    calls: usize,
}

impl FunctionRows {
    fn add(&mut self, group: RowGroup) {
        match group {
            RowGroup::Wasm => self.wasm += 1,
            RowGroup::FrameAux => self.frame_aux += 1,
            RowGroup::EventAux => self.event_aux += 1,
        }
    }

    fn total(&self) -> usize {
        self.wasm + self.frame_aux + self.event_aux + self.calls
    }
}

/// Function names keyed by Neo-Wasm function ref, which is the Wasm function
/// index plus one. Imports and exports keep their names; unexported functions
/// are `func[index]` unless they are a recognized checked intrinsic.
fn function_names(component: &[u8]) -> BTreeMap<u32, String> {
    let module = Parser::new(0)
        .parse_all(component)
        .find_map(|payload| match payload.expect("valid component") {
            Payload::ModuleSection {
                unchecked_range, ..
            } => Some(&component[unchecked_range]),
            _ => None,
        })
        .expect("component embeds a core module");

    let mut types = Vec::<FuncType>::new();
    let mut names = BTreeMap::new();
    let mut imported = 0;
    let mut defined_types = Vec::new();
    let mut defined = 0;
    for payload in Parser::new(0).parse_all(module) {
        match payload.expect("valid core module") {
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    for subtype in rec_group.expect("valid type").into_types() {
                        let CompositeInnerType::Func(function) = subtype.composite_type.inner
                        else {
                            panic!("non-function core type");
                        };
                        types.push(function);
                    }
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let import = import.expect("valid import");
                    if let TypeRef::Func(_) = import.ty {
                        imported += 1;
                        names.insert(imported, format!("{}#{}", import.module, import.name));
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                defined_types = reader
                    .into_iter()
                    .collect::<Result<_, _>>()
                    .expect("valid function section");
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("valid export");
                    if export.kind == wasmparser::ExternalKind::Func {
                        names.insert(export.index + 1, export.name.to_owned());
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                let index = imported + defined;
                let ty = &types[defined_types[defined as usize] as usize];
                if let Some(name) = checked_intrinsic_name(ty, &body) {
                    names.insert(index + 1, name.to_owned());
                }
                names
                    .entry(index + 1)
                    .or_insert_with(|| format!("func[{index}]"));
                defined += 1;
            }
            _ => {}
        }
    }
    names
}

/// Recognize the unexported overflow-checked intrinsics that
/// `starstream-to-wasm/src/intrinsics.rs` emits by signature and prologue.
fn checked_intrinsic_name(ty: &FuncType, body: &wasmparser::FunctionBody) -> Option<&'static str> {
    if ty.params() != [ValType::I64, ValType::I64] || ty.results() != [ValType::I64] {
        return None;
    }
    let prologue = body
        .get_operators_reader()
        .ok()?
        .into_iter()
        .take(5)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    use Operator::{I64Add, I64Mul, I64Sub, LocalGet, LocalTee};
    match prologue.as_slice() {
        [
            LocalGet { local_index: 0 },
            LocalGet { local_index: 1 },
            op,
            LocalTee { local_index: 2 },
            _,
        ] => match op {
            I64Add => Some("__starstream_i64_add_checked"),
            I64Sub => Some("__starstream_i64_sub_checked"),
            _ => None,
        },
        [
            LocalGet { local_index: 0 },
            LocalTee { local_index: 3 },
            LocalGet { local_index: 1 },
            LocalTee { local_index: 4 },
            I64Mul,
        ] => Some("__starstream_i64_mul_checked"),
        _ => None,
    }
}

/// Instance 0 is the coordination script; UTXOs follow in allocation order.
fn instance_label(index: usize) -> String {
    match index.checked_sub(1) {
        None => "coord".to_owned(),
        Some(utxo) => format!("utxo{utxo}"),
    }
}

fn padded(rows: usize) -> usize {
    rows.div_ceil(BATCH_SIZE) * BATCH_SIZE
}

fn percent(part: usize, whole: usize) -> f64 {
    100.0 * part as f64 / whole as f64
}

/// One contract's line in the closing summary.
struct ContractSummary {
    name: &'static str,
    instances: usize,
    rows: usize,
    padded: usize,
    by_group: BTreeMap<RowGroup, usize>,
    checked_intrinsic_calls: usize,
    checked_intrinsic_rows: usize,
}

fn report_contract(
    name: &'static str,
    script: &str,
    execution: &TracedExecution,
) -> ContractSummary {
    let traces = execution
        .instances
        .iter()
        .map(|instance| &instance.trace)
        .collect::<Vec<_>>();
    let names = function_names(&execution.wasm);
    let total_rows = traces.iter().map(|trace| trace.len()).sum::<usize>();
    let total_padded = traces
        .iter()
        .map(|trace| padded(trace.len()))
        .sum::<usize>();

    let mut by_kind = vec![BTreeMap::<RowKind, usize>::new(); traces.len()];
    let mut by_function = BTreeMap::<u32, FunctionRows>::new();
    let mut by_opcode = BTreeMap::<&'static str, usize>::new();
    for (counts, trace) in by_kind.iter_mut().zip(&traces) {
        for row in trace.iter() {
            let kind = RowKind::of(row);
            *counts.entry(kind).or_default() += 1;
            if row.row_kind.is_program() {
                *by_opcode.entry(row.opcode.name()).or_default() += 1;
            }
            match (kind, row.function_ref) {
                (RowKind::Call, Some(target)) => by_function.entry(target).or_default().calls += 1,
                _ => by_function
                    .entry(row.current_function_ref)
                    .or_default()
                    .add(kind.group()),
            }
        }
    }
    let count = |instance: usize, kind: RowKind| by_kind[instance].get(&kind).copied().unwrap_or(0);

    println!();
    println!(
        "== {name} (script `{script}`, {} instances) ==",
        traces.len()
    );

    // Rows by kind, per instance.
    let print_line = |label: &str, per_instance: &[usize]| {
        let total = per_instance.iter().sum::<usize>();
        print!("{label:<28}");
        for count in per_instance {
            print!("{count:>9}");
        }
        println!("{total:>9}{:>8.1}", percent(total, total_rows));
    };
    print!("{:<28}", "row kind");
    for index in 0..traces.len() {
        print!("{:>9}", instance_label(index));
    }
    println!("{:>9}{:>8}", "total", "%");
    let mut by_group = BTreeMap::new();
    for group in RowGroup::ALL {
        let mut subtotal = vec![0; traces.len()];
        for kind in RowKind::ALL
            .into_iter()
            .filter(|kind| kind.group() == group)
        {
            let per_instance = (0..traces.len())
                .map(|instance| count(instance, kind))
                .collect::<Vec<_>>();
            print_line(kind.label(), &per_instance);
            for (subtotal, count) in subtotal.iter_mut().zip(per_instance) {
                *subtotal += count;
            }
        }
        print_line(group.label(), &subtotal);
        by_group.insert(group, subtotal.iter().sum::<usize>());
    }
    let rows = traces.iter().map(|trace| trace.len()).collect::<Vec<_>>();
    print_line("rows", &rows);
    let padded_label = format!("padded to batch of {BATCH_SIZE}");
    print!("{padded_label:<28}");
    for trace in &traces {
        print!("{:>9}", padded(trace.len()));
    }
    println!("{total_padded:>9}");
    let blocks = (0..traces.len())
        .map(|instance| {
            let perm = count(instance, RowKind::EventPerm);
            format!(
                "{} {}",
                instance_label(instance),
                perm / COMM_CHAIN_PERM_ROWS
            )
        })
        .collect::<Vec<_>>();
    println!(
        "absorbed event blocks ({COMM_CHAIN_PERM_ROWS} perm rows each): {}",
        blocks.join(", ")
    );

    // Rows by function, all instances together.
    let mut functions = by_function
        .iter()
        .map(|(fref, rows)| {
            let name = names
                .get(fref)
                .cloned()
                .unwrap_or_else(|| format!("fref {fref}"));
            (name, rows)
        })
        .collect::<Vec<_>>();
    functions
        .sort_by(|(a_name, a), (b_name, b)| b.total().cmp(&a.total()).then(a_name.cmp(b_name)));
    let shown = &functions[..functions.len().min(TOP_FUNCTIONS)];
    let width = shown
        .iter()
        .map(|(name, _)| name.len())
        .chain([8])
        .max()
        .unwrap_or_default()
        + 2;
    println!();
    println!(
        "{:<width$}{:>7}{:>7}{:>7}{:>7}{:>7}{:>7}{:>9}{:>7}",
        "function", "wasm", "frame", "event", "calls", "total", "%", "per call", "% acc"
    );
    let mut accumulated = 0;
    for (name, rows) in shown {
        let per_call = if rows.calls == 0 {
            "-".to_owned()
        } else {
            format!("{:.1}", rows.total() as f64 / rows.calls as f64)
        };
        accumulated += rows.total();
        println!(
            "{name:<width$}{:>7}{:>7}{:>7}{:>7}{:>7}{:>7.1}{per_call:>9}{:>7.1}",
            rows.wasm,
            rows.frame_aux,
            rows.event_aux,
            rows.calls,
            rows.total(),
            percent(rows.total(), total_rows),
            percent(accumulated, total_rows)
        );
    }
    if functions.len() > shown.len() {
        println!(
            "({} more functions, {} rows)",
            functions.len() - shown.len(),
            total_rows - accumulated
        );
    }

    let (checked_intrinsic_calls, checked_intrinsic_rows) = functions
        .iter()
        .filter(|(name, _)| name.starts_with(CHECKED_INTRINSIC_PREFIX))
        .fold((0, 0), |(calls, total), (_, rows)| {
            (calls + rows.calls, total + rows.total())
        });
    println!(
        "{CHECKED_INTRINSIC_PREFIX}*_checked: {checked_intrinsic_calls} calls, \
         {checked_intrinsic_rows} rows ({:.1}% of {total_rows})",
        percent(checked_intrinsic_rows, total_rows)
    );

    // Executed opcodes, program rows only.
    let mut opcodes = by_opcode.into_iter().collect::<Vec<_>>();
    opcodes.sort_by_key(|&(opcode, count)| (std::cmp::Reverse(count), opcode));
    let top = opcodes
        .iter()
        .take(TOP_OPCODES)
        .map(|(opcode, count)| format!("{opcode} {count}"))
        .collect::<Vec<_>>();
    println!("top opcodes: {}", top.join(", "));

    ContractSummary {
        name,
        instances: traces.len(),
        rows: total_rows,
        padded: total_padded,
        by_group,
        checked_intrinsic_calls,
        checked_intrinsic_rows,
    }
}

#[tokio::test]
#[ignore = "prints a row census; run with -- --ignored --nocapture"]
async fn trace_row_census() -> wasmtime::Result<()> {
    let mut summaries = Vec::new();
    for &(name, source, script) in CONTRACTS {
        let execution = trace_coordination_script(source, script).await?;
        summaries.push(report_contract(name, script, &execution));
    }

    println!();
    println!("== summary ==");
    println!(
        "{:<22}{:>6}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}{:>9}{:>8}",
        "contract",
        "inst",
        "rows",
        "padded",
        "wasm",
        "frame",
        "event",
        "checked",
        "checked",
        "checked"
    );
    println!(
        "{:<22}{:>6}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}{:>9}{:>8}",
        "", "", "", "", "", "", "", "calls", "rows", "%"
    );
    let line = |name: &str, summary: &ContractSummary| {
        println!(
            "{name:<22}{:>6}{:>8}{:>8}{:>8}{:>8}{:>8}{:>9}{:>9}{:>8.1}",
            summary.instances,
            summary.rows,
            summary.padded,
            summary.by_group[&RowGroup::Wasm],
            summary.by_group[&RowGroup::FrameAux],
            summary.by_group[&RowGroup::EventAux],
            summary.checked_intrinsic_calls,
            summary.checked_intrinsic_rows,
            percent(summary.checked_intrinsic_rows, summary.rows)
        );
    };
    let mut total = ContractSummary {
        name: "total",
        instances: 0,
        rows: 0,
        padded: 0,
        by_group: RowGroup::ALL.into_iter().map(|group| (group, 0)).collect(),
        checked_intrinsic_calls: 0,
        checked_intrinsic_rows: 0,
    };
    for summary in &summaries {
        line(summary.name, summary);
        total.instances += summary.instances;
        total.rows += summary.rows;
        total.padded += summary.padded;
        for (group, rows) in &summary.by_group {
            *total.by_group.get_mut(group).expect("every group") += rows;
        }
        total.checked_intrinsic_calls += summary.checked_intrinsic_calls;
        total.checked_intrinsic_rows += summary.checked_intrinsic_rows;
    }
    line(total.name, &total);
    Ok(())
}
