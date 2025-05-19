use std::{collections::HashMap, ops::Range};

use ariadne::{Report, ReportBuilder, ReportKind};
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportSection, FuncType, Function,
    FunctionSection, ImportSection, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::ast::*;

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile(program: &StarstreamProgram) -> (Option<Vec<u8>>, Vec<Report>) {
    let mut compiler = Compiler::new();
    compiler.visit_program(program);
    compiler.finish()
}

/// Typed intermediate value.
#[derive(Debug)]
enum Intermediate {
    /// Nothing! Absolutely nothing!
    Void,
    /// An error intermediate. Suppress further typechecking errors.
    Error,
    /// `()` An imported or local function by ID.
    StaticFunction(u32),
    /// `(i32 i32)` A string reference.
    Str,
}

#[derive(Default)]
struct Compiler {
    errors: Vec<Report<'static>>,

    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memory: MemorySection,
    exports: ExportSection,
    code: CodeSection,
    data: DataSection,

    func_types: HashMap<FuncType, u32>,
    global_scope_functions: HashMap<String, u32>,
    bump_ptr: u32,
}

impl Compiler {
    fn new() -> Compiler {
        let mut this = Compiler::default();

        // Function indices in calls, exports, etc. are based on the combined
        // imports + declared functions list. The easiest way to handle this is
        // to know the whole list of imported functions before compiling. Do
        // that here for now.
        let eprint_ty = this.add_func_type(FuncType::new([ValType::I32, ValType::I32], []));
        let import_id: u32 = this.imports.len();
        this.imports
            .import("env", "eprint", EntityType::Function(eprint_ty));
        this.global_scope_functions
            .insert("print".to_owned(), import_id);

        // Always export memory 0. It's created in finish().
        this.exports
            .export("memory", wasm_encoder::ExportKind::Memory, 0);

        this
    }

    fn finish(mut self) -> (Option<Vec<u8>>, Vec<Report<'static>>) {
        let page_size = 64 * 1024;
        self.memory.memory(MemoryType {
            minimum: u64::try_from((self.bump_ptr + page_size - 1) / page_size).unwrap(),
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

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
        if !self.memory.is_empty() {
            module.section(&self.memory);
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

    // ------------------------------------------------------------------------
    // Diagnostics

    fn todo(&mut self, why: String) {
        Report::build(ReportKind::Custom("Todo", ariadne::Color::Red), 0..0)
            .with_message(why)
            .push(self);
    }

    // ------------------------------------------------------------------------
    // Memory management

    fn alloc_constant(&mut self, bytes: &[u8]) -> u32 {
        if self.bump_ptr == 0 {
            // Leave 1K of zeroes at the bottom.
            self.bump_ptr = 1024;
        }

        let ptr = self.bump_ptr;
        self.data.active(
            0,
            &ConstExpr::i32_const(ptr.cast_signed()),
            bytes.iter().copied(),
        );
        self.bump_ptr += u32::try_from(bytes.len()).unwrap();
        ptr
    }

    // ------------------------------------------------------------------------
    // Table management

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

    // ------------------------------------------------------------------------
    // Visitors

    fn visit_program(&mut self, program: &StarstreamProgram) {
        for item in &program.items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &ProgramItem) {
        match item {
            ProgramItem::Script(script) => self.visit_script(script),
            _ => self.todo(format!("ProgramItem::{:?}", item)),
        }
    }

    fn visit_script(&mut self, script: &Script) {
        for fndef in &script.definitions {
            let ty = FuncType::new([], []);
            let mut function = Function::new([]);
            self.visit_block(&mut function, &fndef.body);
            function.instructions().end();
            let index = self.add_function(ty, &function);
            self.exports.export(
                &fndef.name.0,
                wasm_encoder::ExportKind::Func,
                self.imports.len() + index,
            );
        }
    }

    fn visit_block(&mut self, func: &mut Function, mut block: &Block) {
        loop {
            match block {
                Block::Chain { head, tail } => {
                    match &**head {
                        ExprOrStatement::Statement(statement) => {
                            self.visit_statement(func, statement)
                        }
                        ExprOrStatement::Expr(expr) => {
                            let im = self.visit_expr(func, expr);
                            self.drop_intermediate(func, im);
                        }
                    }
                    block = &tail;
                }
                Block::Close { semicolon: _ } => {
                    break;
                }
            }
        }
    }

    fn drop_intermediate(&mut self, func: &mut Function, im: Intermediate) {
        match im {
            Intermediate::Void | Intermediate::Error | Intermediate::StaticFunction(_) => {}
            Intermediate::Str => {
                func.instructions().drop().drop();
            }
        }
    }

    fn visit_statement(&mut self, func: &mut Function, statement: &Statement) {
        match statement {
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    let im = self.visit_expr(func, expr);
                    // TODO: allow actually returning things
                    self.drop_intermediate(func, im);
                }
                func.instructions().return_();
            }
            _ => self.todo(format!("Statement::{:?}", statement)),
        }
    }

    fn visit_expr(&mut self, func: &mut Function, expr: &Expr) -> Intermediate {
        match expr {
            Expr::PrimaryExpr(primary, args, methods) => {
                let mut im = self.visit_primary_expr(func, primary);
                if let Some(args) = args {
                    im = self.visit_call(func, im, &args.xs);
                }
                for (name, args) in methods {
                    im = self.visit_field(func, im, &name.0);
                    if let Some(args) = args {
                        im = self.visit_call(func, im, &args.xs);
                    }
                }
                im
            }
            _ => {
                self.todo(format!("Expr::{:?}", expr));
                Intermediate::Error
            }
        }
    }

    fn visit_primary_expr(&mut self, func: &mut Function, primary: &PrimaryExpr) -> Intermediate {
        match primary {
            PrimaryExpr::Ident(idents) => {
                if idents.len() == 1 && idents[0].0 == "print" {
                    Intermediate::StaticFunction(self.global_scope_functions["print"])
                } else {
                    self.todo(format!("PrimaryExpr::{:?}", primary));
                    Intermediate::Error
                }
            }
            PrimaryExpr::StringLiteral(string) => {
                let ptr = self.alloc_constant(string.as_bytes());
                let len = string.len();
                func.instructions()
                    .i32_const(ptr.cast_signed())
                    .i32_const(u32::try_from(len).unwrap().cast_signed());
                Intermediate::Str
            }
            _ => {
                self.todo(format!("PrimaryExpr::{:?}", primary));
                Intermediate::Error
            }
        }
    }

    fn visit_call(&mut self, func: &mut Function, im: Intermediate, args: &[Expr]) -> Intermediate {
        match im {
            Intermediate::Error => Intermediate::Error,
            Intermediate::StaticFunction(id) => {
                // TODO: typechecking
                for arg in args {
                    self.visit_expr(func, arg);
                }
                func.instructions().call(id);
                // TODO: intermediates corresponding to function's result types
                Intermediate::Void
            }
            _ => {
                Report::build(ReportKind::Error, 0..0)
                    .with_message(format_args!("attempting to call non-function {:?}", im))
                    .push(self);
                self.drop_intermediate(func, im);
                Intermediate::Error
            }
        }
    }

    fn visit_field(&mut self, func: &mut Function, im: Intermediate, name: &str) -> Intermediate {
        if let Intermediate::Error = im {
            return Intermediate::Error;
        }

        _ = func;
        self.todo(format!("Field {:?}.{:?}", im, name));
        Intermediate::Error
    }
}

trait ReportExt {
    fn push(self, c: &mut Compiler);
}

impl ReportExt for Report<'static> {
    fn push(self, c: &mut Compiler) {
        c.errors.push(self);
    }
}

impl ReportExt for ReportBuilder<'static, Range<usize>> {
    fn push(self, c: &mut Compiler) {
        c.errors.push(self.finish());
    }
}
