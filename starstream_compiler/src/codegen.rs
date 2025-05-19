use std::collections::HashMap;

use ariadne::Report;
use wasm_encoder::{
    CodeSection, ExportSection, FuncType, Function, FunctionSection, ImportSection,
    InstructionSink, Module, TypeSection,
};

use crate::ast::*;

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile(program: &StarstreamProgram) -> (Option<Vec<u8>>, Vec<Report>) {
    let mut compiler = Compiler::default();
    compiler.visit_program(program);
    compiler.finish()
}

#[derive(Default)]
struct Compiler {
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,

    func_types: HashMap<FuncType, u32>,

    errors: Vec<Report<'static>>,
}

impl Compiler {
    fn finish(self) -> (Option<Vec<u8>>, Vec<Report<'static>>) {
        // TODO: return None if the errors were fatal.
        let module = self.to_module();
        (Some(module.finish()), self.errors)
    }

    fn to_module(&self) -> Module {
        assert_eq!(self.functions.len(), self.code.len());
        // Write sections to module.
        // Mandatory WASM order per https://webassembly.github.io/spec/core/binary/modules.html#binary-module:
        // type, import, func, table, mem, global, export, start, elem, datacount, code, data.
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
        if !self.exports.is_empty() {
            module.section(&self.exports);
        }
        if !self.code.is_empty() {
            module.section(&self.code);
        }
        module
    }

    fn add_func_type(&mut self, ty: FuncType) -> u32 {
        match self.func_types.get(&ty) {
            Some(&index) => index,
            None => {
                let index = self.types.len();
                self.types.ty().func_type(&ty);
                self.func_types.insert(ty, index);
                index
            }
        }
    }

    fn add_function(&mut self, ty: FuncType, code: &Function) -> u32 {
        let type_index = self.add_func_type(ty);
        let func_index = self.functions.len();
        self.functions.function(type_index);
        self.code.function(code);
        func_index
    }

    fn visit_program(&mut self, program: &StarstreamProgram) {
        for item in &program.items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &ProgramItem) {
        match item {
            ProgramItem::Script(script) => self.visit_script(script),
            _ => unimplemented!(),
        }
    }

    fn visit_script(&mut self, script: &Script) {
        for fndef in &script.definitions {
            let ty = FuncType::new([], []);
            let mut function = Function::new([]);
            self.visit_block(function.instructions(), &fndef.body);
            function.instructions().end();
            let index = self.add_function(ty, &function);
            self.exports
                .export(&fndef.name.0, wasm_encoder::ExportKind::Func, index);
        }
    }

    fn visit_block(&mut self, mut sink: InstructionSink, block: &Block) {
        // TODO
    }
}
