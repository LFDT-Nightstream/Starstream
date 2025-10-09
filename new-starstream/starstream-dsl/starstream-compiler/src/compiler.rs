//! Compiler for the Starstream DSL
//!
//! This module compiles AST nodes into stack machine opcodes.

use std::collections::HashMap;

use starstream_types::*;

/// Symbol table for tracking variable declarations
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    variables: HashMap<String, VariableInfo>,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub declared: bool,
    pub initialized: bool,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn declare_variable(&mut self, name: String) {
        self.variables.insert(
            name,
            VariableInfo {
                declared: true,
                initialized: false,
            },
        );
    }

    pub fn initialize_variable(&mut self, name: &str) {
        if let Some(info) = self.variables.get_mut(name) {
            info.initialized = true;
        }
    }

    pub fn is_declared(&self, name: &str) -> bool {
        self.variables.get(name).map_or(false, |info| info.declared)
    }

    pub fn is_initialized(&self, name: &str) -> bool {
        self.variables
            .get(name)
            .map_or(false, |info| info.initialized)
    }
}

fn simplify_field_access_expression(expr: &FieldAccessExpression) -> String {
    match expr {
        FieldAccessExpression::PrimaryExpr(PrimaryExpr::Ident(ident)) => ident.name.raw.clone(),
        _ => todo!(),
    }
}

fn iter_block<'a>(block: &'a Block) -> impl Iterator<Item = &'a ExprOrStatement> {
    match block {
        Block::Chain { head, tail } => Box::new(std::iter::once(&**head).chain(iter_block(tail)))
            as Box<dyn Iterator<Item = &'a ExprOrStatement>>,
        Block::Close { semicolon } => {
            Box::new(std::iter::empty())
        }
    }
}

fn iter_body(body: &LoopBody) -> impl Iterator<Item = ExprOrStatement> + use<'_> {
    match body {
        LoopBody::Statement(statement) => Box::new(std::iter::once(ExprOrStatement::Statement(
            (**statement).clone(),
        ))) as Box<dyn Iterator<Item = ExprOrStatement>>,
        // LoopBody::Block(block) => todo!(), // block.iter().flat_map(|stmt| iter_body(stmt)),
        // LoopBody::Expr(expr) => Box::new(std::iter::once(expr)),
        LoopBody::Block(block) => {
            Box::new(iter_block(block).cloned())
        }
        _ => todo!(),
    }
}

/// Compiler that translates AST to stack machine opcodes
pub struct Compiler {
    symbol_table: SymbolTable,
    opcodes: OpcodeSequence,
    label_counter: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            opcodes: OpcodeSequence::new(),
            label_counter: 0,
        }
    }

    /// Compile a complete program
    pub fn compile_program(&mut self, program: &StarstreamProgram) -> Result<OpcodeSequence> {
        self.symbol_table = SymbolTable::new();
        self.opcodes = OpcodeSequence::new();
        self.label_counter = 0;

        for item in &program.items {
            match item {
                // Just munge all the scripts together for now...
                ProgramItem::Script(script) => {
                    for fn_def in &script.definitions {
                        for statement in iter_block(&fn_def.body) {
                            self.compile_expr_or_statement(statement)?;
                        }
                    }
                }
                _ => todo!(),
            }
        }

        self.opcodes.add_opcode(Opcode::Halt);
        Ok(self.opcodes.clone())
    }

    fn compile_expr_or_statement(&mut self, expr_or_statement: &ExprOrStatement) -> Result<()> {
        match expr_or_statement {
            ExprOrStatement::Expr(expr) => {
                self.compile_expression(&expr.node)?;
                // Pop the result since it's not assigned to anything
                self.opcodes.add_opcode(Opcode::Pop);
                Ok(())
            }
            ExprOrStatement::Statement(statement) => self.compile_statement(statement),
        }
    }

    /// Compile a single statement
    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::BindVar {
                var: name, value, ..
            } => {
                self.symbol_table.declare_variable(name.raw.clone());
                self.compile_expression(&value.node)?;
                self.opcodes.add_opcode(Opcode::Store(name.raw.clone()));
                self.symbol_table.initialize_variable(&name.raw);
            }
            Statement::Assign {
                var: name,
                expr: value,
            } => {
                let name = simplify_field_access_expression(name);
                if !self.symbol_table.is_declared(&name) {
                    return Err(StarstreamError::UndefinedVariable(name.clone()));
                }
                self.compile_expression(&value.node)?;
                self.opcodes.add_opcode(Opcode::Store(name.clone()));
                self.symbol_table.initialize_variable(&name);
            }
            /* TODO: this is an expression
            Statement::If { condition, then_branch, else_branch } => {
                self.compile_expression(condition)?;
                let _else_label = self.generate_label();
                let _end_label = self.generate_label();

                // Jump to else if condition is false
                self.opcodes.add_opcode(Opcode::JumpIfNot(0)); // Will be patched
                let jump_if_not_addr = self.opcodes.len() - 1;

                // Compile then branch
                for stmt in then_branch {
                    self.compile_statement(stmt)?;
                }

                // Jump to end
                self.opcodes.add_opcode(Opcode::Jump(0)); // Will be patched
                let jump_to_end_addr = self.opcodes.len() - 1;

                // Patch the jump-if-not address
                self.opcodes.opcodes[jump_if_not_addr] = Opcode::JumpIfNot(self.opcodes.len());

                // Compile else branch if present
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.compile_statement(stmt)?;
                    }
                }

                // Patch the jump-to-end address
                self.opcodes.opcodes[jump_to_end_addr] = Opcode::Jump(self.opcodes.len());
            }
            */
            Statement::While(condition, body) => {
                let loop_start = self.opcodes.len();
                let _loop_end_label = self.generate_label();

                // Compile condition
                self.compile_expression(&condition.node)?;

                // Jump to end if condition is false
                self.opcodes.add_opcode(Opcode::JumpIfNot(0)); // Will be patched
                let jump_if_not_addr = self.opcodes.len() - 1;

                // Compile body
                for stmt in iter_body(body) {
                    self.compile_expr_or_statement(&stmt)?;
                }

                // Jump back to start
                self.opcodes.add_opcode(Opcode::Jump(loop_start));

                // Patch the jump-if-not address
                self.opcodes.opcodes[jump_if_not_addr] = Opcode::JumpIfNot(self.opcodes.len());
            }
            _ => todo!(),
        }
        Ok(())
    }

    /// Compile an expression
    fn compile_expression(&mut self, expression: &Expr) -> Result<()> {
        match expression {
            Expr::PrimaryExpr(FieldAccessExpression::PrimaryExpr(PrimaryExpr::Number {
                literal: value,
                ..
            })) => {
                self.opcodes.add_opcode(Opcode::Push(*value as i64));
            }
            Expr::PrimaryExpr(FieldAccessExpression::PrimaryExpr(PrimaryExpr::Bool(value))) => {
                self.opcodes
                    .add_opcode(Opcode::Push(if *value { 1 } else { 0 }));
            }
            Expr::PrimaryExpr(FieldAccessExpression::PrimaryExpr(PrimaryExpr::Ident(ident))) => {
                let name = &ident.name.raw;
                if !self.symbol_table.is_declared(name) {
                    return Err(StarstreamError::UndefinedVariable(name.clone()));
                }
                self.opcodes.add_opcode(Opcode::Load(name.clone()));
            }
            Expr::Add(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Add);
            }
            Expr::Sub(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Subtract);
            }
            Expr::Mod(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Modulo);
            }
            Expr::Mul(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Multiply);
            }
            Expr::Div(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Divide);
            }
            Expr::Equals(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::Equal);
            }
            Expr::NotEquals(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::NotEqual);
            }
            Expr::LessThan(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::LessThan);
            }
            Expr::LessEq(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::LessEqual);
            }
            Expr::GreaterThan(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::GreaterThan);
            }
            Expr::GreaterEq(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::GreaterEqual);
            }
            // TODO: rest of the binary operators
            Expr::Neg(expr) => {
                self.compile_expression(&expr.node)?;
                self.opcodes.add_opcode(Opcode::Negate);
            }
            Expr::Not(expr) => {
                self.compile_expression(&expr.node)?;
                self.opcodes.add_opcode(Opcode::Not);
            }
            /* TODO: opcodes for these ones
            Expr::BitNot(expr) => {
                self.compile_expression(&expr.node)?;
                self.opcodes.add_opcode(Opcode::BitNot);
            }
            Expr::BitAnd(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::BitAnd);
            }
            Expr::BitOr(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::BitOr);
            }
            Expr::BitXor(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::BitXor);
            }
            Expr::LShift(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::LShift);
            }
            Expr::RShift(left, right) => {
                self.compile_expression(&left.node)?;
                self.compile_expression(&right.node)?;
                self.opcodes.add_opcode(Opcode::RShift);
            }
            */
            // TODO: And and Or boolean short-circuiting operators
            _ => todo!(),
        }
        Ok(())
    }

    /// Generate a unique label
    fn generate_label(&mut self) -> String {
        let label = format!("label_{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}
