use wasm_encoder::{
    CodeSection, ExportSection, FunctionSection, ImportSection, Module, TypeSection,
};

use crate::ast::*;

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile(program: &StarstreamProgram) -> Result<Vec<u8>, String> {
    // Prepare sections.
    let types = TypeSection::new();
    let imports = ImportSection::new();
    let functions = FunctionSection::new();
    let exports = ExportSection::new();
    let code = CodeSection::new();

    // TODO(#6) WASM code generation
    let _ = program;

    // Write sections to module.
    // Mandatory WASM order per https://webassembly.github.io/spec/core/binary/modules.html#binary-module:
    // type, import, func, table, mem, global, export, start, elem, datacount, code, data.
    let mut module = Module::new();
    assert!(functions.len() == code.len());
    if !types.is_empty() {
        module.section(&types);
    }
    if !imports.is_empty() {
        module.section(&imports);
    }
    if !functions.is_empty() {
        module.section(&functions);
    }
    if !exports.is_empty() {
        module.section(&exports);
    }
    if !code.is_empty() {
        module.section(&code);
    }

    dbg!(&module);
    Ok(module.finish())
}
