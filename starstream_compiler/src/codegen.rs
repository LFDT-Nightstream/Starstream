#![allow(dead_code)]
use std::{cmp::Ordering, collections::HashMap, ops::Range};

use ariadne::{Report, ReportBuilder, ReportKind};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, Encode, EntityType, ExportSection, FuncType,
    FunctionSection, ImportSection, InstructionSink, MemorySection, MemoryType, Module, RefType,
    TypeSection, ValType,
};

use crate::ast::*;

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile(program: &StarstreamProgram) -> (Option<Vec<u8>>, Vec<Report>) {
    let mut compiler = Compiler::new();
    compiler.visit_program(program);
    compiler.finish()
}

/// A static type in the Starstream type system.
#[derive(Debug, Clone)]
enum StaticType {
    Void,
    Bool,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    StrRef,
    // TODO: add info on the Starstream type of the ref.
    ExternRef,
    Function(Box<StarFunctionType>),
}

impl StaticType {
    fn stack_intermediate(&self) -> Intermediate {
        match self {
            StaticType::Void => Intermediate::Void,
            StaticType::Bool => Intermediate::StackBool,
            StaticType::I32 => Intermediate::StackI32,
            StaticType::U32 => Intermediate::StackU32,
            StaticType::I64 => Intermediate::StackI64,
            StaticType::U64 => Intermediate::StackU64,
            StaticType::F32 => Intermediate::StackF32,
            StaticType::F64 => Intermediate::StackF64,
            StaticType::StrRef => Intermediate::StackStrRef,
            StaticType::ExternRef => Intermediate::StackExternRef,
            StaticType::Function(_) => todo!(),
        }
    }

    fn lower(&self) -> &'static [ValType] {
        match self {
            StaticType::Void => &[],
            StaticType::Bool => &[ValType::I32],
            StaticType::I32 => &[ValType::I32],
            StaticType::U32 => &[ValType::I32],
            StaticType::I64 => &[ValType::I64],
            StaticType::U64 => &[ValType::I64],
            StaticType::F32 => &[ValType::F32],
            StaticType::F64 => &[ValType::F64],
            StaticType::StrRef => &[ValType::I32, ValType::I32],
            StaticType::ExternRef => &[ValType::EXTERNREF],
            _ => todo!(),
        }
    }
}

/// Typed intermediate value.
///
/// A product of static type, stack slot size, and constness.
#[derive(Debug, Clone)]
#[must_use]
enum Intermediate {
    /// Nothing! Absolutely nothing!
    Void,
    /// An error intermediate. Suppress further typechecking errors.
    Error,
    /// `()` The null constant.
    ConstNull,
    /// `()` An imported or local function by ID.
    ConstFunction(u32),
    /// `(i32)` 0 is false, 1 is true, other values are disallowed.
    StackBool,
    /// `(i32)`
    StackI32,
    /// `(i32)` But use unsigned math where relevant.
    StackU32,
    /// `(i64)`
    StackI64,
    /// `(i64)` But use unsigned math where relevant.
    StackU64,
    /// `(f32)`
    StackF32,
    /// `(f64)`
    StackF64,
    StackExternRef,
    /// `(i32 i32)` A string reference, pointer and length.
    StackStrRef,
}

impl Intermediate {
    fn stack_size(&self) -> usize {
        match self {
            // 0
            Intermediate::Void
            | Intermediate::Error
            | Intermediate::ConstFunction(_)
            | Intermediate::ConstNull => 0,
            // 1
            Intermediate::StackBool
            | Intermediate::StackI32
            | Intermediate::StackU32
            | Intermediate::StackI64
            | Intermediate::StackU64
            | Intermediate::StackF32
            | Intermediate::StackF64
            | Intermediate::StackExternRef => 1,
            // 2
            Intermediate::StackStrRef => 2,
        }
    }
}

impl From<ValType> for Intermediate {
    fn from(value: ValType) -> Self {
        match value {
            ValType::I32 => Intermediate::StackI32,
            ValType::I64 => Intermediate::StackI64,
            ValType::F32 => Intermediate::StackF32,
            ValType::F64 => Intermediate::StackF64,
            ValType::V128 => todo!(),
            ValType::Ref(RefType::EXTERNREF) => Intermediate::StackExternRef,
            ValType::Ref(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct StarFunctionType {
    result: StaticType,
    params: Vec<StaticType>,
}

impl StarFunctionType {
    fn lower(&self) -> FuncType {
        FuncType::new(
            self.params.iter().flat_map(|p| p.lower()).copied(),
            self.result.lower().iter().copied(),
        )
    }
}

#[derive(Default)]
struct Compiler {
    // Diagnostic output.
    errors: Vec<Report<'static>>,
    // Wasm binary output.
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memory: MemorySection,
    exports: ExportSection,
    code: CodeSection,
    data: DataSection,

    // Compiler state.
    bump_ptr: u32,
    raw_func_type_cache: HashMap<FuncType, u32>,
    function_types: Vec<StarFunctionType>,

    global_scope_functions: HashMap<String, u32>,
}

impl Compiler {
    fn new() -> Compiler {
        let mut this = Compiler::default();

        // Function indices in calls, exports, etc. are based on the combined
        // imports + declared functions list. The easiest way to handle this is
        // to know the whole list of imported functions before compiling. Do
        // that here for now.
        let print = this.import_function(
            "env",
            "eprint",
            StarFunctionType {
                result: StaticType::Void,
                params: vec![StaticType::StrRef],
            },
        );
        this.global_scope_functions
            .insert("print".to_owned(), print);
        let print_f64 = this.import_function(
            "starstream_debug",
            "f64",
            StarFunctionType {
                result: StaticType::F64,
                params: vec![StaticType::F64],
            },
        );
        this.global_scope_functions
            .insert("print_f64".to_owned(), print_f64);

        //

        // Always export memory 0. It's created in finish().
        this.exports
            .export("memory", wasm_encoder::ExportKind::Memory, 0);

        this
    }

    fn finish(mut self) -> (Option<Vec<u8>>, Vec<Report<'static>>) {
        let page_size = 64 * 1024;
        self.memory.memory(MemoryType {
            minimum: u64::from(self.bump_ptr.div_ceil(page_size)),
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

    fn add_raw_func_type(&mut self, ty: FuncType) -> u32 {
        match self.raw_func_type_cache.get(&ty) {
            Some(&index) => index,
            None => {
                let index = self.types.len();
                self.types.ty().func_type(&ty);
                self.raw_func_type_cache.insert(ty, index);
                index
            }
        }
    }

    fn add_function(&mut self, ty: StarFunctionType, code: &Function) -> u32 {
        let type_index = self.add_raw_func_type(ty.lower());
        let func_index = u32::try_from(self.function_types.len()).unwrap();
        self.function_types.push(ty);
        self.functions.function(type_index);
        let mut sink = Vec::new();
        code.encode(&mut sink);
        self.code.raw(&sink);
        func_index
    }

    fn import_function(&mut self, module: &str, field: &str, ty: StarFunctionType) -> u32 {
        let type_index = self.add_raw_func_type(ty.lower());
        let func_index = u32::try_from(self.function_types.len()).unwrap();
        self.function_types.push(ty);
        self.imports
            .import(module, field, EntityType::Function(type_index));
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
            let ty = StarFunctionType {
                result: StaticType::Void,
                params: vec![],
            };
            let lower_ty = ty.lower();
            let mut function = Function::new(lower_ty.params());
            let return_value = self.visit_block(&mut function, &fndef.body);
            // TODO: handle non-void return values
            self.drop_intermediate(&mut function, return_value);
            function.instructions().end();
            let index = self.add_function(ty, &function);
            self.exports
                .export(&fndef.name.0, wasm_encoder::ExportKind::Func, index);
        }
    }

    fn visit_block(&mut self, func: &mut Function, mut block: &Block) -> Intermediate {
        let mut last = Intermediate::Void;
        loop {
            match block {
                Block::Chain { head, tail } => {
                    match &**head {
                        ExprOrStatement::Statement(statement) => {
                            self.visit_statement(func, statement);
                        }
                        ExprOrStatement::Expr(expr) => {
                            self.drop_intermediate(func, last);
                            last = self.visit_expr(func, expr);
                        }
                    }
                    block = tail;
                }
                Block::Close { semicolon: true } => {
                    self.drop_intermediate(func, last);
                    return Intermediate::Void;
                }
                Block::Close { semicolon: false } => {
                    return last;
                }
            }
        }
    }

    fn drop_intermediate(&mut self, func: &mut Function, im: Intermediate) {
        for _ in 0..im.stack_size() {
            func.instructions().drop();
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
            Expr::Equals(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs);
                let rhs = self.visit_expr(func, rhs);
                match (lhs, rhs) {
                    (Intermediate::Error, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_eq();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Equals({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::Add(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs);
                let rhs = self.visit_expr(func, rhs);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_add();
                        Intermediate::StackF64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Expr::Add({:?}, {:?})", lhs, rhs));
                        Intermediate::Error
                    }
                }
            }
            Expr::And(lhs, rhs) => match self.visit_expr(func, lhs) {
                // Short-circuiting.
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().if_(BlockType::Result(ValType::I32));
                    match self.visit_expr(func, rhs) {
                        Intermediate::Error => return Intermediate::Error,
                        Intermediate::StackBool => {}
                        rhs => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "type mismatch: `&&` requires bools, but right side was {rhs:?}"
                                ))
                                .push(self);
                            return Intermediate::Error;
                        }
                    }
                    func.instructions().else_().i32_const(0).end();
                    Intermediate::StackBool
                }
                lhs => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "type mismatch: `&&` requires bools, but left side was {lhs:?}"
                        ))
                        .push(self);
                    Intermediate::Error
                }
            },
            Expr::Or(lhs, rhs) => match self.visit_expr(func, lhs) {
                // Short-circuiting.
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions()
                        .if_(BlockType::Result(ValType::I32))
                        .i32_const(1)
                        .else_();
                    match self.visit_expr(func, rhs) {
                        Intermediate::Error => return Intermediate::Error,
                        Intermediate::StackBool => {}
                        rhs => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "type mismatch: `&&` requires bools, but right side was {rhs:?}"
                                ))
                                .push(self);
                            return Intermediate::Error;
                        }
                    }
                    func.instructions().end();
                    Intermediate::StackBool
                }
                lhs => {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!(
                            "type mismatch: `&&` requires bools, but left side was {lhs:?}"
                        ))
                        .push(self);
                    Intermediate::Error
                }
            },
            Expr::BlockExpr(BlockExpr::Block(block)) => self.visit_block(func, block),
            Expr::BlockExpr(BlockExpr::IfThenElse(cond, if_, else_)) => {
                match self.visit_expr(func, cond) {
                    Intermediate::Error => Intermediate::Error,
                    Intermediate::StackBool => {
                        // TODO: handle non-Void if blocks.
                        func.instructions().if_(BlockType::Empty);
                        let im = self.visit_block(func, if_);
                        self.drop_intermediate(func, im);
                        if let Some(else_) = else_ {
                            func.instructions().else_();
                            let im = self.visit_block(func, else_);
                            self.drop_intermediate(func, im);
                        }
                        func.instructions().end();
                        Intermediate::Void
                    }
                    other => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message(format_args!(
                                "type mismatch: `if` requires bool, got {other:?}"
                            ))
                            .push(self);
                        Intermediate::Error
                    }
                }
            }
            _ => {
                self.todo(format!("Expr::{:?}", expr));
                Intermediate::Error
            }
        }
    }

    fn visit_primary_expr(&mut self, func: &mut Function, primary: &PrimaryExpr) -> Intermediate {
        match primary {
            PrimaryExpr::Null => Intermediate::ConstNull,
            PrimaryExpr::Number(number) => {
                func.instructions().f64_const(*number);
                Intermediate::StackF64
            }
            PrimaryExpr::Bool(true) => {
                func.instructions().i32_const(1);
                Intermediate::StackBool
            }
            PrimaryExpr::Bool(false) => {
                func.instructions().i32_const(0);
                Intermediate::StackBool
            }
            PrimaryExpr::Ident(idents) => {
                if idents.len() == 1 && idents[0].0 == "print" {
                    Intermediate::ConstFunction(self.global_scope_functions["print"])
                } else if idents.len() == 1 && idents[0].0 == "print_f64" {
                    Intermediate::ConstFunction(self.global_scope_functions["print_f64"])
                } else {
                    self.todo(format!("PrimaryExpr::{:?}", primary));
                    Intermediate::Error
                }
            }
            PrimaryExpr::ParExpr(expr) => self.visit_expr(func, expr),
            PrimaryExpr::StringLiteral(string) => {
                let ptr = self.alloc_constant(string.as_bytes());
                let len = string.len();
                func.instructions()
                    .i32_const(ptr.cast_signed())
                    .i32_const(u32::try_from(len).unwrap().cast_signed());
                Intermediate::StackStrRef
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
            Intermediate::ConstFunction(id) => {
                let func_type = self.function_types[id as usize].clone();
                for (param, arg) in func_type.params.iter().zip(args) {
                    let arg = self.visit_expr(func, arg);
                    match (param, arg) {
                        (StaticType::Void, Intermediate::Void) => {}
                        (StaticType::F64, Intermediate::StackF64) => {}
                        (StaticType::StrRef, Intermediate::StackStrRef) => {}
                        (param, arg) => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "parameter type mismatch: expected {param:?}, got {arg:?}"
                                ))
                                .push(self);
                        }
                    }
                }
                match func_type.params.len().cmp(&args.len()) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message("not enough arguments to function call")
                            .push(self);
                    }
                    Ordering::Greater => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message("too many arguments to function call")
                            .push(self);
                    }
                }
                func.instructions().call(id);
                func_type.result.stack_intermediate()
            }
            _ => {
                Report::build(ReportKind::Error, 0..0)
                    .with_message(format_args!("attempting to call non-function {im:?}"))
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

/// A replacement for [wasm_encoder::Function] that allows adding locals gradually.
#[derive(Default)]
pub struct Function {
    num_locals: u32,
    locals: Vec<(u32, ValType)>,
    bytes: Vec<u8>,
}

impl Function {
    fn new(params: &[ValType]) -> Function {
        let mut this = Function::default();
        for param in params {
            this.add_local(*param);
        }
        this
    }

    fn add_local(&mut self, ty: ValType) -> u32 {
        let id = self.num_locals;
        self.num_locals += 1;
        if let Some((last_count, last_type)) = self.locals.last_mut() {
            if ty == *last_type {
                *last_count += 1;
                return id;
            }
        }
        self.locals.push((1, ty));
        id
    }

    fn instructions(&mut self) -> InstructionSink {
        InstructionSink::new(&mut self.bytes)
    }
}

impl wasm_encoder::Encode for Function {
    fn encode(&self, sink: &mut Vec<u8>) {
        self.locals.len().encode(sink);
        for (count, ty) in &self.locals {
            count.encode(sink);
            ty.encode(sink);
        }
        sink.extend_from_slice(&self.bytes);
    }
}
