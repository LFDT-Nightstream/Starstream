use starstream_types::Program;
use wasm_encoder::*;

/// Compile a Starstream program to a Wasm module.
pub fn compile<'a>(program: &'a Program) -> Vec<u8> {
    let compiler = Compiler::default();
    compiler.finish()
}

#[derive(Default)]
struct Compiler {
    // Wasm binary output.
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memory: MemorySection,
    globals: GlobalSection,
    exports: ExportSection,
    code: CodeSection,
    data: DataSection,
}

impl Compiler {
    fn finish(mut self) -> Vec<u8> {
        Default::default()
    }

    fn to_module(&self) -> Module {
        assert_eq!(self.functions.len(), self.code.len());

        // Write sections to module.
        // Mandatory WASM order per https://webassembly.github.io/spec/core/binary/modules.html#binary-module:
        // type, import, func, table, mem, tag, global, export, start, elem, datacount, code, data.
        let mut module = Module::new();
        if !self.types.is_empty() {
            module.section(&self.types);
        }
        if !self.imports.is_empty() {
            module.section(&self.imports);
        }
        if !self.functions.is_empty() {
            module.section(&self.functions);
        }
        if !self.memory.is_empty() {
            module.section(&self.memory);
        }
        if !self.globals.is_empty() {
            module.section(&self.globals);
        }
        if !self.exports.is_empty() {
            module.section(&self.exports);
        }
        if !self.code.is_empty() {
            module.section(&self.code);
        }
        if !self.data.is_empty() {
            module.section(&self.data);
        }
        module
    }
}
