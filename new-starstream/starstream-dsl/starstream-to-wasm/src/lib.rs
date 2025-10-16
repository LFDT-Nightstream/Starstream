use starstream_types::*;
use wasm_encoder::*;

/// Compile a Starstream program to a Wasm module.
pub fn compile(program: &Program) -> Vec<u8> {
    let mut compiler = Compiler::default();
    compiler.visit_program(program);
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
        self.to_module().finish()
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

    fn visit_program(&mut self, program: &Program) {
        let mut main = Function::default();
        self.visit_block(&mut main, &program.statements);
    }

    fn visit_block(&mut self, func: &mut Function, block: &[Statement]) {
        for statement in block {
            self.visit_statement(func, statement);
        }
    }

    fn visit_statement(&mut self, func: &mut Function, statement: &Statement) {
        todo!()
    }

    fn visit_expr(&mut self, func: &mut Function, _: Span, expr: &Expr) -> Intermediate {
        match expr {
            // Identifiers
            Expr::Identifier(Identifier { name, .. }) => {
                self.todo(format!("Identifier({name:?})"));
                Intermediate::Error
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                match self.visit_expr(func, expr.span, &expr.node) {
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
            } => match self.visit_expr(func, expr.span, &expr.node) {
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
                let lhs = self.visit_expr(func, left.span, &left.node);
                let rhs = self.visit_expr(func, right.span, &right.node);
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
            } => match self.visit_expr(func, left.span, &left.node) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions().if_(BlockType::Result(ValType::I32));
                    match self.visit_expr(func, right.span, &right.node) {
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
            } => match self.visit_expr(func, left.span, &left.node) {
                Intermediate::Error => Intermediate::Error,
                Intermediate::StackBool => {
                    func.instructions()
                        .if_(BlockType::Result(ValType::I32))
                        .i32_const(1)
                        .else_();
                    match self.visit_expr(func, right.span, &right.node) {
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
            Expr::Grouping(expr) => self.visit_expr(func, expr.span, &expr.node),
            _ => todo!(),
        }
    }

    fn todo(&mut self, why: String) {
        // TODO: better diagnostics
        panic!("{}", why);
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
    /// `(i32)` 0 is false, 1 is true, other values are disallowed.
    StackBool,
    /// `(i64)`
    StackI64,
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
