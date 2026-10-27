//! Wasm-to-Wasm optimization of the core module `starstream-to-wasm` emits,
//! targeting the Neo-Wasm prover.
//!
//! Neo-Wasm proves an execution by single-stepping it: every executed
//! operator, structural ones included, becomes one trace row of the same width
//! whatever its opcode, and every call adds a row per declared local of the
//! callee to zero it. Passes here therefore aim to execute fewer operators and
//! declare fewer locals.
//!
//! A published contract's id is the hash of its bytes, so the output must be
//! deterministic. Only function bodies are rewritten: every other section, and
//! every body no pass changes, is copied byte for byte.

use std::ops::Range;

use wasm_encoder::reencode::Error;
use wasm_encoder::{CodeSection, Module, RawSection};
use wasmparser::{Parser, Payload};

mod locals;
#[cfg(test)]
mod tests;

/// Optimize `wasm`, a core module.
pub fn optimize(wasm: &[u8]) -> Result<Vec<u8>, Error> {
    let mut module = Module::new();
    // Parameter count by type index, then type index by defined function.
    let mut type_params = Vec::new();
    let mut function_types = Vec::new();
    let mut defined = 0;
    let mut code: Option<CodeSection> = None;

    for payload in Parser::new(0).parse_all(wasm) {
        let payload = payload?;
        if !matches!(payload, Payload::CodeSectionEntry(_))
            && let Some(code) = code.take()
        {
            module.section(&code);
        }

        match &payload {
            Payload::TypeSection(reader) => {
                for ty in reader.clone().into_iter_err_on_gc_types() {
                    type_params.push(ty?.params().len() as u32);
                }
            }
            Payload::FunctionSection(reader) => {
                function_types = reader.clone().into_iter().collect::<Result<_, _>>()?;
            }
            Payload::CodeSectionStart { .. } => {
                code = Some(CodeSection::new());
                continue;
            }
            Payload::CodeSectionEntry(body) => {
                let params = type_params[function_types[defined] as usize];
                defined += 1;
                let code = code
                    .as_mut()
                    .expect("code entries follow the code section start");
                match locals::clean_up(body, params)? {
                    Some(function) => code.function(&function),
                    None => code.raw(bytes(wasm, body.range())),
                };
            }
            _ => {}
        }

        if let Some((id, range)) = payload.as_section() {
            module.section(&RawSection {
                id,
                data: bytes(wasm, range),
            });
        }
    }

    Ok(module.finish())
}

fn bytes(wasm: &[u8], range: Range<u64>) -> &[u8] {
    &wasm[range.start as usize..range.end as usize]
}
