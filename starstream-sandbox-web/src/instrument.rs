//! Per-opcode execution-trace instrumentation for core Wasm modules.
//!
//! > **Note**: this is a temporary, AI-generated solution for showing an
//! > execution trace in the sandbox Run panel. The plan of record is to
//! > produce the trace from the zkVM execution path instead (a Wasm
//! > interpreter implementing Nightstream's `neo-vm-trace` `VmCpu`), at which
//! > point this module should be deleted.
//!
//! [`instrument`] rewrites a core module so that every instruction of every
//! defined function is preceded by
//!
//! ```wat
//! i32.const <trace-point-id>
//! call $trace ;; (import "$root" "trace" (func (param i32)))
//! ```
//!
//! The injected pair is stack-neutral, so the rewrite is valid before any
//! instruction, including `else` and `end`. The host implements the `trace`
//! import and receives one call per executed opcode; the returned
//! [`Instrumented::points`] side table maps each trace-point id back to the
//! function index, byte offset, mnemonic, and immediate operands in the
//! *original* module, so a trace can be correlated with the WAT/core Wasm
//! already shown in the UI.
//!
//! Notes and limitations:
//!
//! - [`TracePoint`] locations refer to the original (uninstrumented) module:
//!   `func` is the original function index and `offset` is the byte offset of
//!   the opcode in the original binary — the same offsets `wasm-objdump` and
//!   `wasmprinter` report.
//! - Adding the import shifts every *defined* function index up by one. Call
//!   instructions, `ref.func`, exports, element segments, the start section,
//!   and the name section are all remapped automatically; the trace function
//!   type is appended after all existing types so type indices are unchanged.
//! - Const expressions (global initializers, data/element offsets) are not
//!   instrumented: calls are not permitted there.
//! - The import is named `"$root" "trace"`: wit-component's (legacy) name
//!   mangling for a world-level function import `trace`. Declaring that
//!   import in the module's embedded WIT world (see `instrument_for_trace` in
//!   `lib.rs`) makes `ComponentEncoder` map it to a component-level import
//!   that the sandbox satisfies with a host function.

use core::convert::Infallible;

use wasm_encoder::reencode::{self, Reencode};
use wasm_encoder::{
    CodeSection, EntityType, ImportSection, Instruction, Module, SectionId, TypeSection, ValType,
};
use wasmparser::{FunctionBody, Operator, Parser, Payload, TypeRef};

/// Module name of the injected host import: wit-component's legacy name
/// mangling expects world-level function imports under `"$root"`.
pub const TRACE_MODULE: &str = "$root";

/// Field name of the injected host import; also the name of the function
/// import declared in the WIT world.
pub const TRACE_FUNC: &str = "trace";

/// Location of a single trace point in the *original* (uninstrumented) module.
#[derive(Clone, Debug, serde::Serialize)]
pub struct TracePoint {
    /// Function index in the original module's index space.
    pub func: u32,
    /// Byte offset of the opcode within the original module binary.
    pub offset: usize,
    /// WAT mnemonic of the opcode (e.g. `local.get`, `i64.add`).
    pub opcode: &'static str,
    /// The space-separated, `Debug`-formatted immediate operands of the
    /// opcode (e.g. `41` for `i64.const`); empty for opcodes without any.
    ///
    /// These are the static immediates, not the runtime values popped off the
    /// operand stack.
    pub operands: String,
}

/// Result of [`instrument`].
pub struct Instrumented {
    /// The instrumented module.
    pub wasm: Vec<u8>,
    /// Side table mapping each trace-point id (the `i32` passed to the trace
    /// import, reinterpreted as `u32`) to the opcode it precedes.
    pub points: Vec<TracePoint>,
}

/// Rewrites the core module `wasm` to report every executed opcode through an
/// injected `(import "$root" "trace" (func (param i32)))`.
pub fn instrument(wasm: &[u8]) -> Result<Instrumented, reencode::Error> {
    // Pre-scan: the injected import goes after all existing function imports
    // (so only defined function indices shift, by exactly one) and its type
    // goes after all existing types (so type indices do not shift at all).
    let mut num_types = 0;
    let mut num_imported_funcs = 0;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload? {
            Payload::TypeSection(section) => {
                for group in section {
                    num_types +=
                        u32::try_from(group?.types().len()).expect("type count exceeds u32::MAX");
                }
            }
            Payload::ImportSection(section) => {
                for import in section {
                    if let TypeRef::Func(_) = import?.ty {
                        num_imported_funcs += 1;
                    }
                }
            }
            // Both sections precede the function section; stop early.
            Payload::FunctionSection(_) => break,
            _ => {}
        }
    }

    let mut instrumenter = Instrumenter {
        num_imported_funcs,
        trace_type_index: num_types,
        next_func_index: num_imported_funcs,
        types_done: false,
        imports_done: false,
        points: Vec::new(),
    };
    let mut module = Module::new();
    instrumenter.parse_core_module(&mut module, Parser::new(0), wasm)?;
    Ok(Instrumented {
        wasm: module.finish(),
        points: instrumenter.points,
    })
}

struct Instrumenter {
    /// Number of imported functions in the original module; also the index of
    /// the trace import in the instrumented module.
    num_imported_funcs: u32,
    /// Type index of the trace function type in the instrumented module.
    trace_type_index: u32,
    /// Original index of the defined function whose body is instrumented next.
    next_func_index: u32,
    /// Whether the (possibly injected) type section has been emitted.
    types_done: bool,
    /// Whether the (possibly injected) import section has been emitted.
    imports_done: bool,
    /// Side table under construction.
    points: Vec<TracePoint>,
}

impl Instrumenter {
    fn append_trace_type(&mut self, types: &mut TypeSection) {
        types.ty().function([ValType::I32], []);
        self.types_done = true;
    }

    fn append_trace_import(&mut self, imports: &mut ImportSection) {
        imports.import(
            TRACE_MODULE,
            TRACE_FUNC,
            EntityType::Function(self.trace_type_index),
        );
        self.imports_done = true;
    }
}

/// Position of a section in the canonical core-module section order, used to
/// decide where to inject the type/import sections when the original module
/// lacks them. (`SectionId` discriminants are *not* in canonical order: the
/// tag and data-count sections sort differently than their ids.)
fn section_rank(id: SectionId) -> u8 {
    match id {
        // The intersperse hook is never called for custom sections.
        SectionId::Custom => 0,
        SectionId::Type => 1,
        SectionId::Import => 2,
        SectionId::Function => 3,
        SectionId::Table => 4,
        SectionId::Memory => 5,
        SectionId::Tag => 6,
        SectionId::Global => 7,
        SectionId::Export => 8,
        SectionId::Start => 9,
        SectionId::Element => 10,
        SectionId::DataCount => 11,
        SectionId::Code => 12,
        SectionId::Data => 13,
    }
}

impl Reencode for Instrumenter {
    type Error = Infallible;

    fn function_index(&mut self, func: u32) -> Result<u32, reencode::Error> {
        // The trace import takes index `num_imported_funcs`, shifting every
        // defined function up by one. Calls, `ref.func`, exports, element
        // segments, the start section, and the name section all go through
        // this hook.
        Ok(if func >= self.num_imported_funcs {
            func + 1
        } else {
            func
        })
    }

    fn parse_type_section(
        &mut self,
        types: &mut TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<(), reencode::Error> {
        reencode::utils::parse_type_section(self, types, section)?;
        self.append_trace_type(types);
        Ok(())
    }

    fn parse_import_section(
        &mut self,
        imports: &mut ImportSection,
        section: wasmparser::ImportSectionReader<'_>,
    ) -> Result<(), reencode::Error> {
        reencode::utils::parse_import_section(self, imports, section)?;
        self.append_trace_import(imports);
        Ok(())
    }

    fn intersperse_section_hook(
        &mut self,
        module: &mut Module,
        _after: Option<SectionId>,
        before: Option<SectionId>,
    ) -> Result<(), reencode::Error> {
        // If the original module lacks a type or import section, inject one
        // no later than just before the first section that must follow it.
        let rank = before.map_or(u8::MAX, section_rank);
        if !self.types_done && rank > section_rank(SectionId::Type) {
            let mut types = TypeSection::new();
            self.append_trace_type(&mut types);
            module.section(&types);
        }
        if !self.imports_done && rank > section_rank(SectionId::Import) {
            let mut imports = ImportSection::new();
            self.append_trace_import(&mut imports);
            module.section(&imports);
        }
        Ok(())
    }

    fn parse_function_body(
        &mut self,
        code: &mut CodeSection,
        func: FunctionBody<'_>,
    ) -> Result<(), reencode::Error> {
        let func_index = self.next_func_index;
        self.next_func_index += 1;

        let mut f = self.new_function_with_parsed_locals(&func)?;
        let mut reader = func.get_operators_reader()?;
        while !reader.eof() {
            let offset = reader.original_position();
            let op = reader.read()?;

            let id = u32::try_from(self.points.len()).expect("trace point id exceeds u32::MAX");
            let (opcode, operands) = describe_operator(&op);
            self.points.push(TracePoint {
                func: func_index,
                offset,
                opcode,
                operands,
            });
            f.instruction(&Instruction::I32Const(id as i32));
            // Call the trace import by its index in the *instrumented* module;
            // `function_index` must not be applied, it remaps original indices.
            f.instruction(&Instruction::Call(self.num_imported_funcs));

            f.instruction(&self.instruction(op)?);
        }
        code.function(&f);
        Ok(())
    }
}

macro_rules! define_describe_operator {
    ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($ann:tt)*))*) => {
        /// The WAT mnemonic and the immediate operands of an operator, e.g.
        /// `("i64.const", "41")`.
        fn describe_operator(op: &Operator<'_>) -> (&'static str, String) {
            use core::fmt::Write as _;
            #[allow(unreachable_patterns)]
            match op {
                $( Operator::$op $({ $($arg),* })? => {
                    // Computed once per opcode; the static gives it the
                    // `'static` lifetime.
                    static OPCODE: std::sync::LazyLock<String> =
                        std::sync::LazyLock::new(|| wat_name(stringify!($visit)));

                    #[allow(unused_mut)]
                    let mut operands = String::new();
                    $($(
                        if !operands.is_empty() {
                            operands.push(' ');
                        }
                        write!(operands, "{:?}", $arg).expect("infallible write to String");
                    )*)?
                    (OPCODE.as_str(), operands)
                } )*
                _ => ("<unknown>", String::new()),
            }
        }
    };
}
wasmparser::for_each_operator!(define_describe_operator);

/// The WAT spelling of an opcode, derived from wasmparser's snake_case
/// visitor name: the leading namespace segment is separated by `.` like in
/// the text format (`local.get`, `i32.load8_s`, `i32.atomic.rmw8.add_u`),
/// while opcodes without a namespace (`br_if`, `call_indirect`) keep their
/// underscores.
fn wat_name(visit: &'static str) -> String {
    /// The namespaces the text format separates with a `.`.
    const NAMESPACES: &[&str] = &[
        "i32", "i64", "f32", "f64", "v128", "i8x16", "i16x8", "i32x4", "i64x2", "f32x4", "f64x2",
        "local", "global", "table", "memory", "elem", "data", "ref", "struct", "array", "any",
        "extern", "i31",
    ];

    let name = visit.strip_prefix("visit_").unwrap_or(visit);
    // `select` with explicit result type(s) is still spelled `select`.
    if name == "typed_select" || name == "typed_select_multi" {
        return "select".into();
    }
    let Some((head, rest)) = name.split_once('_') else {
        return name.into();
    };
    if !NAMESPACES.contains(&head) {
        return name.into();
    }
    // The atomic (and atomic read-modify-write) segments are separated by
    // `.` too: `memory.atomic.notify`, `i32.atomic.rmw8.add_u`.
    let rest = rest.replacen("atomic_", "atomic.", 1);
    let rest = if let Some((rmw, op)) = rest
        .strip_prefix("atomic.")
        .and_then(|op| op.split_once('_'))
        .filter(|(rmw, _)| matches!(*rmw, "rmw" | "rmw8" | "rmw16" | "rmw32"))
    {
        format!("atomic.{rmw}.{op}")
    } else {
        rest
    };
    format!("{head}.{rest}")
}
