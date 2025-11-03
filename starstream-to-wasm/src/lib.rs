use std::collections::HashMap;

use miette::Report;
use starstream_types::*;
use wasm_encoder::*;

/*
    The entry point [compile] is responsible for the overall AST-to-WASM-module
    compilation that is exposed to the outside world. Most of the rest of this
    crate should stay private.

    One Starstream program is always converted to one Wasm module.

    The compiler walks the AST and generates section entries for types,
    imports, functions, and so on. Each function (for example `main`) is
    compiled to Wasm bytecode and added to the code section.

    Optimizations like constant-evaluation are left on the table for now.
    The easiest way to get them is probably to import [wasm-opt].

    [wasm-opt]: https://docs.rs/wasm-opt/latest/wasm_opt/
*/

/// Compile a Starstream program to a Wasm module.
pub fn compile(program: &Program) -> (Option<Vec<u8>>, Vec<Report>) {
    // TODO: accept input filename/context to put in reports.
    let mut compiler = Compiler::default();
    if let Err(e) = compiler.visit_program(program) {
        compiler.reports.push(e);
        return (None, compiler.reports);
    }
    compiler.finish()
}

/// Holds the in-progress Wasm sections and other module-wide information
/// needed to build them.
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

    // Diagnostics.
    fatal: bool,
    reports: Vec<Report>,

    // Function building.
    raw_func_type_cache: HashMap<FuncType, u32>,
}

impl Compiler {
    /// After [Compiler::visit_program], this function collates all the
    /// in-progress sections into an actual Wasm binary module.
    fn finish(self) -> (Option<Vec<u8>>, Vec<Report>) {
        // TODO: any other final activity on the sections here, such as
        // committing constants to the memory/data section.

        // Verify
        assert_eq!(self.functions.len(), self.code.len());

        // No reports generated beyond this point, so bail if fatal is set.
        if self.fatal {
            return (None, self.reports);
        }

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
        (Some(module.finish()), self.reports)
    }

    // ------------------------------------------------------------------------
    // Table management

    /// Add a function signature to the `types` section if needed, and return
    /// the index of the new or existing entry.
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

    /// Add a new function to both the `functions` and `code` section, and
    /// return its index.
    fn add_function(&mut self, ty: FuncType, code: Function) -> u32 {
        // TODO: enforce that all *imported* function IDs are known before this
        // is called, as Wasm requires all imports to precede all of the
        // module's own functions.

        let type_index = self.add_raw_func_type(ty);
        let func_index = self.functions.len();
        self.functions.function(type_index);

        let mut vec = Vec::new();
        code.encode(&mut vec);
        self.code.raw(&vec);

        func_index
    }

    // ------------------------------------------------------------------------
    // Visitors

    /// Root visitor called by [compile] to start walking the AST for a program,
    /// building the Wasm sections on the way.
    fn visit_program(&mut self, program: &Program) -> miette::Result<()> {
        let mut func = Function::default();
        self.visit_block(&mut func, &(), &program.statements);
        func.instructions().end();

        let idx = self.add_function(FuncType::new([], []), func);
        self.exports.export("main", ExportKind::Func, idx);
        Ok(())
    }

    /// Start a new identifier scope and generate bytecode for the statements
    /// of the block in sequence. Only creates a Wasm `block` when specifically
    /// needed for control flow reasons.
    fn visit_block(&mut self, func: &mut Function, parent: &dyn Locals, block: &[Statement]) {
        let mut locals = HashMap::new();
        for statement in block {
            match statement {
                Statement::Expression(expr) => {
                    let im = self.visit_expr(func, &(parent, &locals), expr.span, &expr.node);
                    self.discard(func, im);
                }
                Statement::VariableDeclaration { name, value } => {
                    let value = self.visit_expr(func, &(parent, &locals), value.span, &value.node);
                    match value {
                        Intermediate::Error => {}
                        Intermediate::StackI64 => {
                            let local = func.add_local(ValType::I64);
                            func.instructions().local_set(local);
                            locals.insert(name.name.clone(), local);
                        }
                        value => self.todo(format!("VariableDeclaration({value:?})")),
                    }
                }
                Statement::Assignment { target, value } => {
                    let local = (parent, &locals).get(&target.name);
                    let value = self.visit_expr(func, &(parent, &locals), value.span, &value.node);
                    match value {
                        Intermediate::Error => {}
                        Intermediate::StackI64 => {
                            func.instructions().local_set(local);
                        }
                        value => self.todo(format!("VariableDeclaration({value:?})")),
                    }
                }
                // Recursive
                Statement::Block(block) => {
                    self.visit_block(func, &(parent, &locals), &block.statements);
                }
                Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let im =
                        self.visit_expr(func, &(parent, &locals), condition.span, &condition.node);
                    assert!(matches!(im, Intermediate::StackBool));
                    func.instructions().if_(BlockType::Empty);
                    self.visit_block(func, &(parent, &locals), &then_branch.statements);
                    if let Some(else_branch) = else_branch {
                        func.instructions().else_();
                        self.visit_block(func, &(parent, &locals), &else_branch.statements);
                    }
                    func.instructions().end();
                }
                Statement::While { condition, body } => {
                    func.instructions().block(BlockType::Empty); // br(1) is break
                    func.instructions().loop_(BlockType::Empty); // br(0) is continue

                    let im =
                        self.visit_expr(func, &(parent, &locals), condition.span, &condition.node);
                    assert!(matches!(im, Intermediate::StackBool));
                    // if condition == 0, break
                    func.instructions().i32_eqz();
                    func.instructions().br_if(1);
                    // contents
                    self.visit_block(func, &(parent, &locals), &body.statements);
                    // continue
                    func.instructions().br(0).end().end();
                }
            }
        }
    }

    /// Insert bytecode to discard the given [Intermediate], such as during
    /// statement expressions.
    fn discard(&mut self, func: &mut Function, im: Intermediate) {
        match im {
            // 0 stack slots
            Intermediate::Error | Intermediate::Void => {}
            // 1 stack slot
            Intermediate::StackBool | Intermediate::StackI64 => {
                func.instructions().drop();
            }
        }
    }

    /// Compile a single [Expr] into the current function, returning an
    /// [Intermediate] representing that expression's output on the stack.
    fn visit_expr(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        _: Span,
        expr: &Expr,
    ) -> Intermediate {
        match expr {
            // Identifiers
            Expr::Identifier(Identifier { name, .. }) => {
                let local = locals.get(name);
                func.instructions().local_get(local);
                Intermediate::StackI64
            }
            // Literals
            Expr::Literal(Literal::Integer(i)) => {
                func.instructions().i64_const(*i);
                Intermediate::StackI64
            }
            Expr::Literal(Literal::Boolean(b)) => {
                func.instructions().i32_const(*b as i32);
                Intermediate::StackBool
            }
            // Arithmetic operators
            Expr::Binary {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_add();
                        Intermediate::StackI64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Add({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Subtract,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_sub();
                        Intermediate::StackI64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Subtract({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_mul();
                        Intermediate::StackI64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Multiply({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Divide,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_div_s();
                        Intermediate::StackI64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Divide({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Remainder,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_rem_s();
                        Intermediate::StackI64
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Remainder({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Unary {
                op: UnaryOp::Negate,
                expr,
            } => {
                // `-x` compiles to `0 - x`.
                func.instructions().i64_const(0);
                match self.visit_expr(func, locals, expr.span, &expr.node) {
                    Intermediate::Error => Intermediate::Error,
                    Intermediate::StackI64 => {
                        func.instructions().i64_sub();
                        Intermediate::StackI64
                    }
                    lhs => {
                        self.todo(format!("Negate({lhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Unary {
                op: UnaryOp::Not,
                expr,
            } => match self.visit_expr(func, locals, expr.span, &expr.node) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().i32_eqz();
                    Intermediate::StackBool
                }
                lhs => {
                    self.todo(format!("Not({lhs:?})"));
                    Intermediate::Error
                }
            },
            // Comparison operators
            Expr::Binary {
                op: BinaryOp::Equal,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_eq();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_eq();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Equal({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::NotEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_ne();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_ne();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("NotEqual({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Less,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_lt_s();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_lt_u();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Less({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::Greater,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_gt_s();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_gt_u();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Greater({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::LessEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_le_s();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_le_u();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("LessEqual({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            Expr::Binary {
                op: BinaryOp::GreaterEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs, rhs) {
                    (Intermediate::Error, _) | (_, Intermediate::Error) => Intermediate::Error,
                    (Intermediate::StackI64, Intermediate::StackI64) => {
                        func.instructions().i64_ge_s();
                        Intermediate::StackBool
                    }
                    (Intermediate::StackBool, Intermediate::StackBool) => {
                        func.instructions().i32_ge_u();
                        Intermediate::StackBool
                    }
                    (lhs, rhs) => {
                        self.todo(format!("Greater({lhs:?}, {rhs:?})"));
                        Intermediate::Error
                    }
                }
            }
            // Short-circuiting operators
            Expr::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => match self.visit_expr(func, locals, left.span, &left.node) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().if_(BlockType::Result(ValType::I32));
                    match self.visit_expr(func, locals, right.span, &right.node) {
                        Intermediate::Error => Intermediate::Error,
                        Intermediate::StackBool => {
                            func.instructions().else_().i32_const(0).end();
                            Intermediate::StackBool
                        }
                        right => {
                            self.todo(format!("And({left:?}, {right:?})"));
                            Intermediate::Error
                        }
                    }
                }
                left => {
                    self.todo(format!("And({left:?}, {right:?})"));
                    Intermediate::Error
                }
            },
            Expr::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => match self.visit_expr(func, locals, left.span, &left.node) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions()
                        .if_(BlockType::Result(ValType::I32))
                        .i32_const(1)
                        .else_();
                    match self.visit_expr(func, locals, right.span, &right.node) {
                        Intermediate::Error => Intermediate::Error,
                        Intermediate::StackBool => {
                            func.instructions().end();
                            Intermediate::StackBool
                        }
                        right => {
                            self.todo(format!("Or({left:?}, {right:?})"));
                            Intermediate::Error
                        }
                    }
                }
                left => {
                    self.todo(format!("Or({left:?}, {right:?})"));
                    Intermediate::Error
                }
            },
            // Nesting
            Expr::Grouping(expr) => self.visit_expr(func, locals, expr.span, &expr.node),
        }
    }

    fn todo(&mut self, why: String) {
        self.reports.push(Report::msg(format!("TODO: {why}")));
    }
}

// Probably inefficient, but fun. Fix later?
trait Locals {
    fn get(&self, name: &str) -> u32;
}

impl Locals for () {
    fn get(&self, name: &str) -> u32 {
        panic!("unknown local: {name:?}")
    }
}

impl Locals for (&dyn Locals, &HashMap<String, u32>) {
    fn get(&self, name: &str) -> u32 {
        match self.1.get(name) {
            Some(v) => *v,
            None => self.0.get(name),
        }
    }
}

/// Typed intermediate value.
///
/// Represents the static type and stack slots that evaluating an expression
/// produced, so that that expression's consumers can correctly consume or
/// discard those stack slots.
#[derive(Debug, Clone)]
#[must_use]
enum Intermediate {
    /// An error intermediate. Suppress further typechecking errors.
    Error,
    /// Nothing! Absolutely nothing!
    #[allow(dead_code)]
    Void,
    /// `(i32)` 0 is false, 1 is true, other values are disallowed.
    StackBool,
    /// `(i64)`
    StackI64,
}

/// A replacement for [wasm_encoder::Function] that allows adding locals gradually.
///
/// Bytecode can be encoded to the return value of [Function::instructions].
#[derive(Default)]
struct Function {
    num_locals: u32,
    locals: Vec<(u32, ValType)>,
    bytes: Vec<u8>,
}

impl Function {
    #[allow(dead_code)]
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
        if let Some((last_count, last_type)) = self.locals.last_mut()
            && ty == *last_type
        {
            *last_count += 1;
            return id;
        }
        self.locals.push((1, ty));
        id
    }

    fn instructions(&mut self) -> InstructionSink<'_> {
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
