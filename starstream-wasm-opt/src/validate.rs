//! Checks that a core module stays inside what the Starstream proving pipeline
//! can handle.
//!
//! The supported subset is the intersection of:
//!
//! - what Neo-Wasm (Nightstream's `crates/neo-wasm`) can trace: integer
//!   operators only, one 32-bit memory, and host imports that return at most
//!   one value and are never reached through a tail call;
//! - what the ledger can snapshot as UTXO state
//!   (`starstream-ledger/src/runtime.rs`): no imported memories or globals, and
//!   globals initialized by a single integer constant.
//!
//! Neo-Wasm skips operators it cannot decode instead of rejecting them, which
//! only surfaces later as an unrelated constraint failure, so they are
//! rejected here up front.

use thiserror::Error;
use wasmparser::{
    BinaryReaderError, FuncType, Operator, Parser, Payload, TypeRef, Validator, WasmFeatures,
};

/// The Wasm proposals the subset uses. wasmparser rejects everything else
/// (floats, SIMD, bulk memory, multiple memories, memory64, threads,
/// exceptions, GC types, ...), which also limits value types to `i32`, `i64`
/// and `funcref`.
const FEATURES: WasmFeatures = WasmFeatures::MUTABLE_GLOBAL
    .union(WasmFeatures::SIGN_EXTENSION)
    .union(WasmFeatures::REFERENCE_TYPES)
    .union(WasmFeatures::MULTI_VALUE)
    .union(WasmFeatures::TAIL_CALL);

/// Neo-Wasm's limit on a function type's parameters, and on its results.
const MAX_FUNCTION_ARITY: usize = 255;

/// Why a module falls outside the supported subset.
#[derive(Debug, Error)]
pub enum ValidationError {
    /// Invalid Wasm, or Wasm using an unsupported proposal.
    #[error(transparent)]
    Invalid(#[from] BinaryReaderError),
    /// An operator Neo-Wasm cannot trace.
    #[error(
        "function {function} uses `{operator}` at offset {offset:#x}, which Neo-Wasm cannot trace"
    )]
    UnsupportedOperator {
        function: u32,
        operator: String,
        offset: usize,
    },
    /// A module construct the prover or the ledger cannot handle.
    #[error("{message} at offset {offset:#x}")]
    Unsupported { message: String, offset: usize },
}

fn unsupported(offset: usize, message: impl Into<String>) -> ValidationError {
    ValidationError::Unsupported {
        message: message.into(),
        offset,
    }
}

/// Check that `wasm`, a core module, stays inside the supported subset.
pub fn validate(wasm: &[u8]) -> Result<(), ValidationError> {
    Validator::new_with_features(FEATURES).validate_all(wasm)?;

    let mut types = Vec::<FuncType>::new();
    let mut imported_functions = 0;
    let mut defined_functions = 0;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload? {
            Payload::TypeSection(reader) => {
                let offset = reader.range().start;
                for ty in reader.into_iter_err_on_gc_types() {
                    let ty = ty?;
                    if ty.params().len() > MAX_FUNCTION_ARITY
                        || ty.results().len() > MAX_FUNCTION_ARITY
                    {
                        return Err(unsupported(
                            offset,
                            format!(
                                "function type {} has more than {MAX_FUNCTION_ARITY} parameters or results",
                                types.len()
                            ),
                        ));
                    }
                    types.push(ty);
                }
            }
            Payload::ImportSection(reader) => {
                for import in reader.into_imports_with_offsets() {
                    let (offset, import) = import?;
                    let name = format!("{}#{}", import.module, import.name);
                    match import.ty {
                        TypeRef::Func(ty) => {
                            if types[ty as usize].results().len() > 1 {
                                return Err(unsupported(
                                    offset,
                                    format!("import `{name}` returns more than one value"),
                                ));
                            }
                            imported_functions += 1;
                        }
                        TypeRef::Table(_) => {
                            return Err(unsupported(offset, format!("import `{name}` is a table")));
                        }
                        TypeRef::Memory(_) => {
                            return Err(unsupported(
                                offset,
                                format!("import `{name}` is a memory"),
                            ));
                        }
                        TypeRef::Global(_) => {
                            return Err(unsupported(
                                offset,
                                format!("import `{name}` is a global"),
                            ));
                        }
                        // Tags need the exceptions proposal, already rejected.
                        _ => {}
                    }
                }
            }
            Payload::GlobalSection(reader) => {
                for global in reader.into_iter_with_offsets() {
                    let (offset, global) = global?;
                    let mut init = global.init_expr.get_operators_reader();
                    let first = init.read()?;
                    if !matches!(first, Operator::I32Const { .. } | Operator::I64Const { .. })
                        || !init.is_end_then_eof()
                    {
                        return Err(unsupported(
                            offset,
                            "global is not initialized by a single integer constant",
                        ));
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                let function = imported_functions + defined_functions;
                defined_functions += 1;
                for operator in body.get_operators_reader()?.into_iter_with_offsets() {
                    let (operator, offset) = operator?;
                    match operator {
                        Operator::ReturnCall { function_index }
                            if function_index < imported_functions =>
                        {
                            return Err(unsupported(
                                offset,
                                format!(
                                    "function {function} tail-calls host import {function_index}"
                                ),
                            ));
                        }
                        operator if !is_traceable(&operator) => {
                            return Err(ValidationError::UnsupportedOperator {
                                function,
                                operator: operator_name(&operator),
                                offset,
                            });
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

/// The operator's variant name, without its immediates.
fn operator_name(operator: &Operator) -> String {
    let debug = format!("{operator:?}");
    debug
        .split_once(' ')
        .map_or(debug.as_str(), |(name, _)| name)
        .to_owned()
}

/// Whether Neo-Wasm can trace `operator`. Mirrors `decode_opcode` in
/// Neo-Wasm's `adapters/wasmtime/decode.rs`, plus `end`, which it decodes
/// separately.
fn is_traceable(operator: &Operator) -> bool {
    use Operator::*;
    matches!(
        operator,
        // Control.
        Unreachable
            | Nop
            | Block { .. }
            | Loop { .. }
            | If { .. }
            | Else
            | End
            | Br { .. }
            | BrIf { .. }
            | BrTable { .. }
            | Return
            | Call { .. }
            | CallIndirect { .. }
            | ReturnCall { .. }
            | ReturnCallIndirect { .. }
            | Drop
            | Select
            | TypedSelect { .. }
            // Variables.
            | LocalGet { .. }
            | LocalSet { .. }
            | LocalTee { .. }
            | GlobalGet { .. }
            | GlobalSet { .. }
            // Memory and tables.
            | I32Load { .. }
            | I64Load { .. }
            | I32Load8S { .. }
            | I32Load8U { .. }
            | I32Load16S { .. }
            | I32Load16U { .. }
            | I64Load8S { .. }
            | I64Load8U { .. }
            | I64Load16S { .. }
            | I64Load16U { .. }
            | I64Load32S { .. }
            | I64Load32U { .. }
            | I32Store { .. }
            | I64Store { .. }
            | I32Store8 { .. }
            | I32Store16 { .. }
            | I64Store8 { .. }
            | I64Store16 { .. }
            | I64Store32 { .. }
            | MemorySize { .. }
            | MemoryGrow { .. }
            | TableSize { .. }
            | TableGet { .. }
            | TableSet { .. }
            // Constants.
            | I32Const { .. }
            | I64Const { .. }
            | RefFunc { .. }
            // i32 arithmetic and comparisons.
            | I32Eqz
            | I32Eq
            | I32Ne
            | I32LtS
            | I32LtU
            | I32GtS
            | I32GtU
            | I32LeS
            | I32LeU
            | I32GeS
            | I32GeU
            | I32Clz
            | I32Ctz
            | I32Popcnt
            | I32Add
            | I32Sub
            | I32Mul
            | I32DivS
            | I32DivU
            | I32RemS
            | I32RemU
            | I32And
            | I32Or
            | I32Xor
            | I32Shl
            | I32ShrS
            | I32ShrU
            | I32Rotl
            | I32Rotr
            // i64 arithmetic and comparisons.
            | I64Eqz
            | I64Eq
            | I64Ne
            | I64LtS
            | I64LtU
            | I64GtS
            | I64GtU
            | I64LeS
            | I64LeU
            | I64GeS
            | I64GeU
            | I64Clz
            | I64Ctz
            | I64Popcnt
            | I64Add
            | I64Sub
            | I64Mul
            | I64DivS
            | I64DivU
            | I64RemS
            | I64RemU
            | I64And
            | I64Or
            | I64Xor
            | I64Shl
            | I64ShrS
            | I64ShrU
            | I64Rotl
            | I64Rotr
            // Conversions.
            | I32WrapI64
            | I64ExtendI32S
            | I64ExtendI32U
            | I32Extend8S
            | I32Extend16S
            | I64Extend8S
            | I64Extend16S
            | I64Extend32S
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(wat: &str) -> Result<(), ValidationError> {
        validate(&wat::parse_str(wat).expect("valid wat"))
    }

    #[test]
    fn accepts_the_supported_subset() {
        check(
            r#"
            (module
              (import "host" "get" (func $get (result i64)))
              (memory 1)
              (global $counter (mut i64) (i64.const 0))
              (func $pair (result i32 i64)
                i32.const 1
                i64.const 2)
              (func $add (param i64 i64) (result i64)
                local.get 0
                local.get 1
                i64.add)
              (func (export "run") (param i64) (result i64)
                i32.const 0
                i32.const 255
                i32.store8
                call $get
                global.set $counter
                block
                  local.get 0
                  i64.eqz
                  br_if 0
                  i32.const 7
                  i32.extend8_s
                  drop
                end
                call $pair
                drop
                drop
                local.get 0
                global.get $counter
                return_call $add))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn rejects_what_the_pipeline_cannot_handle() {
        let cases = [
            ("(module (func (param f32)))", "floating-point"),
            (
                "(module (memory 1) (func (memory.fill (i32.const 0) (i32.const 0) (i32.const 0))))",
                "bulk memory",
            ),
            (
                "(module (table 1 funcref) (func (drop (table.grow (table.get (i32.const 0)) (i32.const 1)))))",
                "`TableGrow`",
            ),
            (
                "(module (func (drop (ref.is_null (ref.null func)))))",
                "`RefNull`",
            ),
            (
                r#"(module (import "host" "f" (func $f)) (func (return_call $f)))"#,
                "tail-calls host import 0",
            ),
            (
                r#"(module (import "host" "f" (func (result i32 i32))))"#,
                "returns more than one value",
            ),
            (
                r#"(module (import "host" "t" (table 1 funcref)))"#,
                "is a table",
            ),
            (r#"(module (import "host" "m" (memory 1)))"#, "is a memory"),
            (
                r#"(module (import "host" "g" (global i32)))"#,
                "is a global",
            ),
            (
                "(module (func $f) (global funcref (ref.func $f)))",
                "single integer constant",
            ),
        ];
        for (wat, expected) in cases {
            let error = check(wat).expect_err(wat).to_string();
            assert!(error.contains(expected), "{wat}: {error}");
        }
    }
}
