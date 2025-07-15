#![allow(dead_code)]
use std::{cmp::Ordering, collections::HashMap, ops::Range, rc::Rc};

use ariadne::{Report, ReportBuilder, ReportKind};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, Encode, EntityType, ExportSection, FuncType,
    FunctionSection, ImportSection, InstructionSink, MemorySection, MemoryType, Module, RefType,
    TypeSection, ValType,
};

use crate::{
    ast::*,
    symbols::{SymbolId, Symbols},
    typechecking::{ComparableType, PrimitiveType},
};

/// Compile a Starstream AST to a binary WebAssembly module.
pub fn compile<'a>(
    program: &'a StarstreamProgram,
    symbols: Symbols,
) -> (Option<Vec<u8>>, Vec<Report<'a>>) {
    let mut compiler = Compiler::new(symbols);
    compiler.visit_program(program);
    compiler.finish()
}

/// A static type in the Starstream type system.
#[derive(Debug, Clone)]
enum StaticType {
    Void,

    // Built-in types: primitive types
    // https://component-model.bytecodealliance.org/design/wit.html#primitive-types
    Bool,
    // S8,
    // S16,
    I32,
    I64,
    // U8,
    // U16,
    U32,
    U64,
    F32,
    F64,
    // Char,
    StrRef,

    Reference(Box<StaticType>),

    // Built-in types: lists, options, results, tuples
    // https://component-model.bytecodealliance.org/design/wit.html#lists
    // List(Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#options
    // Option(Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#results
    // Result(Box<StaticType>, Box<StaticType>),
    // https://component-model.bytecodealliance.org/design/wit.html#tuples
    // Tuple(Vec<StaticType>),

    // User-defined types
    // Record(Record),
    // Variant(Variant),
    // Enum(Enum),
    Resource(Rc<ResourceType>),

    //
    Function(Rc<StarFunctionType>),
}

impl StaticType {
    fn stack_intermediate(&self) -> Intermediate {
        match self {
            StaticType::Void => Intermediate::Void,
            StaticType::Bool => Intermediate::StackBool,
            StaticType::I32 => Intermediate::StackI32,
            StaticType::I64 => Intermediate::StackI64,
            StaticType::U32 => Intermediate::StackU32,
            StaticType::U64 => Intermediate::StackU64,
            StaticType::F32 => Intermediate::StackF32,
            StaticType::F64 => Intermediate::StackF64,
            StaticType::StrRef => Intermediate::StackStrRef,
            StaticType::Resource(_) => Intermediate::StackExternRef,

            StaticType::Reference(_) => Intermediate::StackI64,
            _ => todo!(),
        }
    }

    fn lower(&self) -> &'static [ValType] {
        self.stack_intermediate().stack_types()
    }

    fn from_canonical_type(ty: &ComparableType) -> Self {
        match ty {
            ComparableType::Primitive(PrimitiveType::Unit) => StaticType::Void,
            ComparableType::Primitive(PrimitiveType::U32) => StaticType::U32,
            ComparableType::Primitive(PrimitiveType::I32) => StaticType::I32,
            ComparableType::Primitive(PrimitiveType::U64) => StaticType::U64,
            ComparableType::Primitive(PrimitiveType::I64) => StaticType::I64,
            ComparableType::Primitive(PrimitiveType::F32) => StaticType::F32,
            ComparableType::Primitive(PrimitiveType::F64) => StaticType::F64,
            ComparableType::Primitive(PrimitiveType::Bool) => StaticType::Bool,
            ComparableType::Intermediate => StaticType::I64,
            ComparableType::FnType(_, _) => todo!(),
            ComparableType::Utxo(_symbol_id) => StaticType::I64,
            ComparableType::Var(_type_var) => {
                unreachable!("unbound type variable at the codegen phase")
            }
            ComparableType::Ref(ty) => {
                StaticType::Reference(Box::new(StaticType::from_canonical_type(ty)))
            }
            ComparableType::Void => StaticType::Void,
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
    fn stack_types(&self) -> &'static [ValType] {
        match self {
            Intermediate::Void => &[],
            Intermediate::StackBool => &[ValType::I32],
            Intermediate::StackI32 => &[ValType::I32],
            Intermediate::StackI64 => &[ValType::I64],
            Intermediate::StackU32 => &[ValType::I32],
            Intermediate::StackU64 => &[ValType::I64],
            Intermediate::StackF32 => &[ValType::F32],
            Intermediate::StackF64 => &[ValType::F64],
            Intermediate::StackStrRef => &[ValType::I32, ValType::I32],
            Intermediate::StackExternRef => &[ValType::EXTERNREF],
            _ => todo!(),
        }
    }

    fn stack_size(&self) -> usize {
        self.stack_types().len()
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
    params: Vec<StaticType>,
    results: Vec<StaticType>,
}

impl StarFunctionType {
    fn lower(&self) -> FuncType {
        FuncType::new(
            self.params.iter().flat_map(|p| p.lower()).copied(),
            self.results.iter().flat_map(|p| p.lower()).copied(),
        )
    }
}

// https://component-model.bytecodealliance.org/design/wit.html#resources
#[derive(Debug, Clone)]
struct ResourceType {
    // WIT splits this out... maybe we'll just say a method literally named "constructor" is the constructor.
    constructor: Option<StarFunctionType>,
    methods: HashMap<String, (MethodType, StarFunctionType)>,
}

#[derive(Debug, Clone, Copy)]
enum MethodType {
    Static,
    BorrowSelf,
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

    symbols_table: Symbols,

    current_utxo: Vec<String>,
}

impl Compiler {
    fn new(mut symbols_table: Symbols) -> Compiler {
        let mut this = Compiler::default();

        // Function indices in calls, exports, etc. are based on the combined
        // imports + declared functions list. The easiest way to handle this is
        // to know the whole list of imported functions before compiling. Do
        // that here for now.
        let print = this.import_function(
            "env",
            "eprint",
            StarFunctionType {
                params: vec![StaticType::StrRef],
                results: vec![],
            },
        );
        this.global_scope_functions
            .insert("print".to_owned(), print);
        let print_f64 = this.import_function(
            "starstream_debug",
            "f64",
            StarFunctionType {
                params: vec![StaticType::F64],
                results: vec![StaticType::F64],
            },
        );
        this.global_scope_functions
            .insert("print_f64".to_owned(), print_f64);

        //

        // Always export memory 0. It's created in finish().
        this.exports
            .export("memory", wasm_encoder::ExportKind::Memory, 0);

        for f_info in symbols_table.functions.values_mut() {
            if f_info.source == "resume" && f_info.info.mangled_name.is_some() {
                let index = this.import_function(
                    "starstream_utxo:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: f_info
                            .info
                            .inputs_canonical_ty
                            .iter()
                            .map(StaticType::from_canonical_type)
                            .collect(),
                        results: vec![],
                    },
                );

                f_info.info.index.replace(index);
            }

            if f_info.source == "new" && f_info.info.mangled_name.is_some() {
                let index = this.import_function(
                    "starstream_utxo:this",
                    f_info.info.mangled_name.as_ref().unwrap(),
                    StarFunctionType {
                        params: f_info
                            .info
                            .inputs_canonical_ty
                            .iter()
                            .map(StaticType::from_canonical_type)
                            .collect(),
                        results: f_info
                            .info
                            .output_canonical_ty
                            .as_ref()
                            .map(|ty| vec![StaticType::from_canonical_type(ty)])
                            .unwrap_or(vec![]),
                    },
                );

                f_info.info.index.replace(index);
            }
        }

        let starstream_yield = this.import_function(
            "starstream_utxo_env",
            "starstream_yield",
            StarFunctionType {
                params: vec![
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                    StaticType::I32,
                ],
                results: vec![],
            },
        );
        this.global_scope_functions
            .insert("starstream_yield".to_owned(), starstream_yield);

        add_builtin_assert(&mut this);
        add_builtin_is_tx_signed_by(&mut this);

        Compiler {
            symbols_table,
            ..this
        }
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
            ProgramItem::Utxo(utxo) => self.visit_utxo(utxo),
            _ => self.todo(format!("ProgramItem::{:?}", item)),
        }
    }

    fn visit_utxo(&mut self, utxo: &Utxo) {
        self.current_utxo.push(utxo.name.raw.clone());

        for item in &utxo.items {
            match item {
                UtxoItem::Main(main) => {
                    let (ty, mut function) = self.build_func(&main.ident.uid.unwrap(), true);

                    let return_value = self.visit_block(&mut function, &main.block);
                    self.drop_intermediate(&mut function, return_value);
                    function.instructions().end();

                    let index = self.add_function(ty, &function);

                    self.exports.export(
                        self.symbols_table.functions[&main.ident.uid.unwrap()]
                            .info
                            .mangled_name
                            .as_ref()
                            .unwrap(),
                        wasm_encoder::ExportKind::Func,
                        index,
                    );
                }
                UtxoItem::Impl(_) => self.todo("utxo methods".to_string()),
                UtxoItem::Storage(_storage) => self.todo("utxo storage".to_string()),
                UtxoItem::Yield(_type_arg) => self.todo("yielding data".to_string()),
                UtxoItem::Resume(_type_arg) => self.todo("resuming utxo with data".to_string()),
            }
        }

        self.current_utxo.pop();
    }

    fn visit_script(&mut self, script: &Script) {
        for fndef in &script.definitions {
            let (ty, mut function) = self.build_func(&fndef.ident.uid.unwrap(), false);

            let return_value = self.visit_block(&mut function, &fndef.body);
            // TODO: handle non-void return values
            self.drop_intermediate(&mut function, return_value);
            function.instructions().end();
            let index = self.add_function(ty, &function);
            self.exports
                .export(&fndef.ident.raw, wasm_encoder::ExportKind::Func, index);
        }
    }

    fn build_func(&self, fn_id: &SymbolId, is_main: bool) -> (StarFunctionType, Function) {
        let f_info = self.symbols_table.functions.get(fn_id).unwrap();

        // TODO: duplicated code
        let ty = StarFunctionType {
            params: f_info
                .info
                .inputs_canonical_ty
                .iter()
                .map(StaticType::from_canonical_type)
                .collect(),
            results: if is_main {
                vec![]
            } else {
                f_info
                    .info
                    .output_canonical_ty
                    .as_ref()
                    .map(|ty| vec![StaticType::from_canonical_type(ty)])
                    .unwrap_or_default()
            },
        };
        let lower_ty = ty.lower();
        let mut function = Function::new(lower_ty.params());

        for local in &f_info.info.locals {
            let var_info = self.symbols_table.vars.get(local).unwrap();

            match var_info.info.ty.as_ref().unwrap() {
                ComparableType::Primitive(PrimitiveType::U32) => {
                    let val_type = ValType::I32;
                    function.add_local(val_type);
                }
                ComparableType::Primitive(PrimitiveType::Bool) => {
                    let val_type = ValType::I32;
                    function.add_local(val_type);
                }
                ComparableType::Utxo(_) => {
                    let val_type = ValType::I64;
                    function.add_local(val_type);
                }
                ComparableType::Ref(_) => {
                    let val_type = ValType::I64;
                    function.add_local(val_type);
                }
                _ => todo!(),
            }
        }
        (ty, function)
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
            Statement::BindVar {
                var,
                mutable: _,
                ty: _,
                value,
            } => {
                let im = self.visit_expr(func, value);

                if matches!(im, Intermediate::Error) {
                    Report::build(ReportKind::Error, 0..0)
                        .with_message(format_args!("can't assign expression to variable"))
                        .push(self);

                    return;
                }

                let var_info = self.symbols_table.vars.get(&var.uid.unwrap()).unwrap();

                func.instructions().local_set(var_info.info.index as u32);
            }
            _ => self.todo(format!("Statement::{:?}", statement)),
        }
    }

    fn visit_expr(&mut self, func: &mut Function, expr: &Spanned<Expr>) -> Intermediate {
        match &expr.node {
            Expr::PrimaryExpr(secondary) => self.visit_field_access_expr(func, secondary),
            Expr::Equals(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs);
                let rhs = self.visit_expr(func, rhs);
                match (lhs, rhs) {
                    (Intermediate::Error, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32) => {
                        func.instructions().i32_eq();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_eq();
                        Intermediate::StackBool
                    }
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
            Expr::NotEquals(lhs, rhs) => {
                let lhs = self.visit_expr(func, lhs);
                let rhs = self.visit_expr(func, rhs);
                match (lhs, rhs) {
                    (Intermediate::Error, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI32, Intermediate::StackI32) => {
                        func.instructions().i32_ne();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_ne();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackF64, Intermediate::StackF64) => {
                        func.instructions().f64_ne();
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

    fn visit_field_access_expr(
        &mut self,
        func: &mut Function,
        expr: &FieldAccessExpression,
    ) -> Intermediate {
        match expr {
            FieldAccessExpression::PrimaryExpr(primary) => self.visit_primary_expr(func, primary),
            FieldAccessExpression::FieldAccess { base, field } => {
                let receiver = self.visit_field_access_expr(func, base);

                let expr: &IdentifierExpr = field;
                if let Intermediate::Error = receiver {
                    return Intermediate::Error;
                }

                if let Some(args) = &expr.args {
                    let f_info = self
                        .symbols_table
                        .functions
                        .get(&expr.name.uid.unwrap())
                        .unwrap();

                    let f_index = f_info.info.index.unwrap();
                    let func_im = Intermediate::ConstFunction(f_index);

                    let xs = &args.xs;

                    self.visit_call(func, func_im, xs, Some(receiver))
                } else {
                    _ = func;
                    self.todo(format!("Field {:?}.{:?}", receiver, expr));
                    Intermediate::Error
                }
            }
        }
    }

    fn visit_primary_expr(&mut self, func: &mut Function, primary: &PrimaryExpr) -> Intermediate {
        match primary {
            PrimaryExpr::Number(number) => {
                func.instructions().i32_const(*number as i32);
                Intermediate::StackI32
            }
            PrimaryExpr::Bool(true) => {
                func.instructions().i32_const(1);
                Intermediate::StackBool
            }
            PrimaryExpr::Bool(false) => {
                func.instructions().i32_const(0);
                Intermediate::StackBool
            }
            PrimaryExpr::Ident(ident)
            | PrimaryExpr::Namespace {
                namespaces: _,
                ident,
            } => {
                if ident.args.is_none() {
                    let var_info = self
                        .symbols_table
                        .vars
                        .get(&ident.name.uid.unwrap())
                        .unwrap();

                    let ty = var_info.info.ty.as_ref().unwrap();

                    func.instructions().local_get(var_info.info.index as u32);

                    return StaticType::from_canonical_type(ty).stack_intermediate();
                }

                let im = if ident.name.raw == "print" {
                    Intermediate::ConstFunction(self.global_scope_functions["print"])
                } else if ident.name.raw == "print_f64" {
                    Intermediate::ConstFunction(self.global_scope_functions["print_f64"])
                } else if ident.name.raw == "assert" {
                    Intermediate::ConstFunction(self.global_scope_functions["assert"])
                } else if ident.name.raw == "IsTxSignedBy" {
                    Intermediate::ConstFunction(self.global_scope_functions["IsTxSignedBy"])
                } else {
                    Intermediate::ConstFunction(
                        self.symbols_table
                            .functions
                            .get(&ident.name.uid.unwrap())
                            .unwrap()
                            .info
                            .index
                            .unwrap(),
                    )
                };

                if let Some(args) = &ident.args {
                    self.visit_call(func, im, &args.xs, None)
                } else {
                    im
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
            PrimaryExpr::Yield(_expr) => {
                let f_id = self.global_scope_functions["starstream_yield"];

                // TODO: yielding outside utxos
                let utxo_name = self.current_utxo.pop().unwrap();
                let ptr = self.alloc_constant(utxo_name.as_bytes());
                let len = utxo_name.len();
                func.instructions()
                    .i32_const(ptr.cast_signed())
                    .i32_const(u32::try_from(len).unwrap().cast_signed());

                // TODO: yield data

                // data
                func.instructions().i32_const(0);
                // data_len
                func.instructions().i32_const(0);
                // resume_arg
                func.instructions().i32_const(0);
                // resume_arg_len
                func.instructions().i32_const(0);

                func.instructions().call(f_id);

                Intermediate::Void
            }
            PrimaryExpr::Tuple(elems) if elems.is_empty() => Intermediate::Void,
            _ => {
                self.todo(format!("PrimaryExpr::{:?}", primary));
                Intermediate::Error
            }
        }
    }

    fn visit_call(
        &mut self,
        func: &mut Function,
        im: Intermediate,
        args: &[Spanned<Expr>],
        method_self: Option<Intermediate>,
    ) -> Intermediate {
        match im {
            Intermediate::Error => Intermediate::Error,
            Intermediate::ConstFunction(id) => {
                let func_type = self.function_types[id as usize].clone();
                for (param, arg) in func_type
                    .params
                    .iter()
                    .skip(if method_self.is_some() { 1 } else { 0 })
                    .zip(args)
                {
                    let arg = self.visit_expr(func, arg);
                    match (param, arg) {
                        (StaticType::Void, Intermediate::Void) => {}
                        (StaticType::F64, Intermediate::StackF64) => {}
                        (StaticType::I32, Intermediate::StackI32) => {}
                        (StaticType::I32, Intermediate::StackU32) => {}
                        (StaticType::U32, Intermediate::StackI32) => {}
                        (StaticType::U32, Intermediate::StackU32) => {}
                        (StaticType::I64, Intermediate::StackI64) => {}
                        (StaticType::U64, Intermediate::StackI64) => {}
                        (StaticType::U64, Intermediate::StackU64) => {}
                        (StaticType::StrRef, Intermediate::StackStrRef) => {}
                        (StaticType::Bool, Intermediate::StackBool) => {}
                        (StaticType::Reference(_s), Intermediate::Void) => {
                            // null pointer
                            func.instructions().i64_const(0);
                            // references to other types will need to be handled
                            // by allocating memory
                        }
                        (param, arg) => {
                            Report::build(ReportKind::Error, 0..0)
                                .with_message(format_args!(
                                    "parameter type mismatch: expected {param:?}, got {arg:?}"
                                ))
                                .push(self);
                        }
                    }
                }

                match func_type
                    .params
                    .len()
                    .cmp(&(args.len() + method_self.map(|_| 1).unwrap_or(0)))
                {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message("not enough arguments to function call")
                            .push(self);
                    }
                    Ordering::Greater => {
                        Report::build(ReportKind::Error, 0..0)
                            .with_message("not enough arguments to function call")
                            .push(self);
                    }
                }
                func.instructions().call(id);
                match func_type.results.first() {
                    // TODO: handle functions with multiple results
                    Some(r) => r.stack_intermediate(),
                    None => Intermediate::Void,
                }
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
}

fn add_builtin_assert(this: &mut Compiler) {
    let mut function = Function::new(&[ValType::I32]);

    function
        .instructions()
        .local_get(0)
        .if_(BlockType::Empty)
        .else_()
        .unreachable()
        .end()
        .end();

    let assert_fn = this.add_function(
        StarFunctionType {
            params: vec![StaticType::Bool],
            results: vec![],
        },
        &function,
    );

    this.global_scope_functions
        .insert("assert".to_owned(), assert_fn);
}

fn add_builtin_is_tx_signed_by(this: &mut Compiler) {
    let mut function = Function::new(&[ValType::I32]);

    function.instructions().i32_const(1).end();

    let assert_fn = this.add_function(
        StarFunctionType {
            params: vec![StaticType::I32],
            results: vec![StaticType::Bool],
        },
        &function,
    );

    this.global_scope_functions
        .insert("IsTxSignedBy".to_owned(), assert_fn);
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

// -----------------------------------------------------------------------------
// Tests

#[cfg(test)]
mod tests {
    use crate::{compile, do_scope_analysis, do_type_inference, parse};
    use wasmparser::{Parser, Payload};

    /// Collect all export names from a WASM module.
    fn export_names(bytes: &[u8]) -> Vec<String> {
        let mut names = Vec::new();
        for payload in Parser::new(0).parse_all(bytes) {
            if let Ok(Payload::ExportSection(reader)) = payload {
                for export in reader {
                    let export = export.unwrap();
                    names.push(export.name.to_string());
                }
            }
        }
        names
    }

    #[test]
    fn compile_hello_world() {
        let src = include_str!("../../grammar/examples/hello_world.star");
        let (program, parse_errors) = parse(src);
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
        let program = program.expect("parse failed");

        let (program, mut symbols) = do_scope_analysis(program).unwrap();

        let (program, _warnings) = do_type_inference(program, &mut symbols)
            .map_err(|errors| {
                for e in errors {
                    e.print(ariadne::Source::from(src)).unwrap();
                }
            })
            .unwrap();

        let (wasm, compile_errors) = compile(&program, symbols);
        assert!(
            compile_errors.is_empty(),
            "compile errors: {compile_errors:?}"
        );
        let wasm = wasm.expect("compilation failed");

        let exports = export_names(&wasm);
        assert!(exports.iter().any(|e| e == "main"), "exports: {exports:?}");
    }

    #[test]
    fn compile_pay_to_public_key_hash() {
        let src = include_str!("../../grammar/examples/pay_to_public_key_hash.star");
        let (program, parse_errors) = parse(src);
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
        let program = program.expect("parse failed");

        let (program, mut symbols) = do_scope_analysis(program).unwrap();

        let (program, _warnings) = do_type_inference(program, &mut symbols)
            .map_err(|errors| {
                for e in errors {
                    e.print(ariadne::Source::from(src)).unwrap();
                }
            })
            .unwrap();

        let (wasm, compile_errors) = compile(&program, symbols);
        assert!(
            compile_errors.is_empty(),
            "compile errors: {compile_errors:?}"
        );
        let wasm = wasm.expect("compilation failed");

        let exports = export_names(&wasm);
        assert!(exports.iter().any(|e| e == "main"), "exports: {exports:?}");
    }
}
