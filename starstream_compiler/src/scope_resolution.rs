use crate::ast::{
    Abi, AbiElem, Block, BlockExpr, EffectDecl, Expr, ExprOrStatement, FnDef, Identifier, LoopBody,
    PrimaryExpr, ProgramItem, Script, StarstreamProgram, Statement, Token, TokenItem, Type,
    TypeDef, Utxo, UtxoItem,
};
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::span::SimpleSpan;
use std::collections::HashMap;

/// This traverses the AST, and assigns an unique numeric ID to each identifier
/// on declaration. The ids are stored inside of the Identifier node of the AST.
///
/// Also references are resolved when found, according to the scoping rules.
/// These can then be used to index into the Symbols table to get information about
/// the declaration of that particular identifier.
///
/// This pass does _not_ do resolution of field accesses or method calls, since that
/// usually requires information about the types. Although it may be possible to
/// resolve functions in builtin types.
pub fn do_scope_analysis(
    mut program: StarstreamProgram,
) -> Result<(StarstreamProgram, Symbols), Vec<Report<'static>>> {
    let mut resolver = Visitor::new();
    resolver.visit_program(&mut program);
    let (symbols, errors) = resolver.finish();

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok((program, symbols))
    }
}

pub struct Symbols {
    pub map: HashMap<SymbolId, SymbolInformation>,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub index: u64,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct SymbolInformation {
    pub source: String,
    pub span: Option<SimpleSpan>,
    pub variable: Option<VarInfo>,
}

#[derive(Debug)]
pub struct Scope {
    declarations: HashMap<String, SymbolId>,
    is_function_scope: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct SymbolId {
    id: u64,
}

struct Visitor {
    stack: Vec<Scope>,
    // used to keep count of variables declared in the innermost function scope it's
    // kept outside the scope stack to avoid having to do parent traversal,
    // since not all scopes are function scopes.
    locals: Vec<u64>,
    // used to generate unique ids for new identifiers
    symbol_counter: u64,
    errors: Vec<Report<'static>>,
    symbols: Symbols,
}

enum DeclarationKind {
    Variable { mutable: bool },
}

impl Visitor {
    fn new() -> Self {
        Visitor {
            stack: vec![],
            locals: vec![],
            symbol_counter: 0,
            errors: vec![],
            symbols: Symbols {
                map: Default::default(),
            },
        }
    }

    fn push_scope(&mut self, is_function_scope: bool) {
        self.stack.push(Scope {
            declarations: HashMap::new(),
            is_function_scope,
        });

        if is_function_scope {
            self.locals.push(0);
        }
    }

    fn pop_scope(&mut self) {
        let scope = self.stack.pop();

        if let Some(scope) = scope {
            if scope.is_function_scope {
                self.locals.pop();
            }
        }
    }

    fn finish(self) -> (Symbols, Vec<Report<'static>>) {
        (self.symbols, self.errors)
    }

    fn builtins() -> Vec<&'static str> {
        // TODO: mostly just to get the examples working
        // these probably would have to be some sort of import?
        vec![
            "CoordinationCode",
            "ThisCode",
            "assert",
            "IsTxSignedBy",
            "None",
            "context",
            "String",
            "u32",
            "u64",
            "i32",
            "i64",
            "PublicKey",
            "Caller",
            "PayToPublicKeyHash",
            "List",
        ]
    }

    fn add_builtins(&mut self) {
        for builtin in Self::builtins() {
            self.push_declaration(&mut Identifier::new(builtin, None), None);
        }
    }

    fn visit_program(&mut self, program: &mut StarstreamProgram) {
        self.push_scope(false);

        self.add_builtins();

        for item in &mut program.items {
            match item {
                ProgramItem::TypeDef(type_def) => self.visit_type_def(type_def),
                ProgramItem::Token(token) => {
                    self.push_declaration(&mut token.name, None);
                }
                ProgramItem::Script(_script) => (),
                ProgramItem::Utxo(utxo) => {
                    self.push_declaration(&mut utxo.name, None);
                }
                ProgramItem::Constant { name, value: _ } => {
                    self.push_declaration(name, None);
                }
            }
        }

        for item in &mut program.items {
            match item {
                ProgramItem::Script(script) => {
                    self.visit_script(script);
                }
                ProgramItem::Utxo(utxo) => {
                    self.visit_utxo(utxo);
                }
                ProgramItem::Token(token) => {
                    self.visit_token(token);
                }
                _ => (),
            }
        }

        self.pop_scope();
    }

    pub fn visit_script(&mut self, script: &mut Script) {
        for definition in &mut script.definitions {
            self.visit_fn_def(definition, true);
        }
    }

    pub fn visit_utxo(&mut self, utxo: &mut Utxo) {
        self.push_declaration(&mut utxo.name, None);

        // we need to put these into scope before doing anything else
        for item in &mut utxo.items {
            if let UtxoItem::Abi(abi) = item {
                self.visit_abi(abi);
            }
        }

        for item in &mut utxo.items {
            match item {
                UtxoItem::Abi(_) => (),
                UtxoItem::Main(main) => {
                    self.visit_block(&mut main.block, true);
                }
                UtxoItem::Impl(utxo_impl) => {
                    for definition in &mut utxo_impl.definitions {
                        self.visit_fn_def(definition, false);
                    }
                }
                UtxoItem::Storage(_) => {}
            }
        }
    }

    pub fn visit_token(&mut self, token: &mut Token) {
        self.push_declaration(&mut token.name, None);

        for item in &mut token.items {
            match item {
                TokenItem::Abi(abi) => self.visit_abi(abi),
                TokenItem::Bind(bind) => {
                    self.push_scope(true);
                    self.push_declaration(
                        &mut Identifier::new("self", None),
                        Some(DeclarationKind::Variable { mutable: true }),
                    );
                    self.visit_block(&mut bind.0, false);
                    self.pop_scope();
                }
                TokenItem::Unbind(unbind) => {
                    self.push_scope(true);
                    self.push_declaration(
                        &mut Identifier::new("self", None),
                        Some(DeclarationKind::Variable { mutable: true }),
                    );
                    self.visit_block(&mut unbind.0, false);
                    self.pop_scope();
                }
                TokenItem::Mint(mint) => {
                    self.push_scope(true);
                    self.push_declaration(
                        &mut Identifier::new("self", None),
                        Some(DeclarationKind::Variable { mutable: true }),
                    );
                    self.visit_block(&mut mint.0, false);
                    self.pop_scope();
                }
            }
        }
    }

    pub fn visit_type_def(&mut self, type_def: &mut TypeDef) {
        self.push_declaration(&mut type_def.name, None);
    }

    fn visit_fn_def(&mut self, definition: &mut FnDef, is_declaration: bool) {
        if is_declaration {
            self.push_declaration(&mut definition.ident, None);
        } else {
            self.resolve_name(&mut definition.ident);
        }

        self.push_scope(true);

        for node in &mut definition.inputs {
            self.push_declaration(
                &mut node.name,
                Some(DeclarationKind::Variable { mutable: false }),
            );
        }

        self.visit_block(&mut definition.body, false);

        self.pop_scope();
    }

    fn push_declaration(
        &mut self,
        ident: &mut Identifier,
        kind: Option<DeclarationKind>,
    ) -> SymbolId {
        let scope = self.stack.last_mut().unwrap();

        let symbol = SymbolId {
            id: self.symbol_counter,
        };

        ident.uid.replace(symbol);

        self.symbol_counter += 1;

        scope.declarations.insert(ident.raw.clone(), symbol);

        self.symbols.map.insert(
            symbol,
            SymbolInformation {
                source: ident.raw.clone(),
                span: ident.span,
                variable: if let Some(DeclarationKind::Variable { mutable }) = kind {
                    // TODO: handle error
                    let fn_scope = self.locals.last_mut().unwrap();
                    let index = *fn_scope;
                    *fn_scope += 1;
                    Some(VarInfo { index, mutable })
                } else {
                    None
                },
            },
        );

        symbol
    }

    fn resolve_name(&mut self, identifier: &mut Identifier) {
        let resolution = self
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.declarations.get(&identifier.raw).cloned());

        let Some(resolved_name) = resolution else {
            self.push_error("not found in this scope", identifier.span.unwrap());
            return;
        };

        identifier.uid.replace(resolved_name);
    }

    fn visit_block(&mut self, block: &mut Block, new_scope: bool) {
        // Blocks as syntax elements can be both part of expressions or just
        // function definitions. We could create an inner scope for the function
        // definition, but it's probably better to not increase depth
        if new_scope {
            self.push_scope(false);
        }

        let mut curr = block;

        loop {
            match curr {
                Block::Chain { head, tail } => {
                    match &mut **head {
                        ExprOrStatement::Expr(expr) => {
                            self.visit_expr(expr);
                        }
                        ExprOrStatement::Statement(statement) => {
                            self.visit_statement(statement);
                        }
                    }

                    curr = tail;
                }
                Block::Close { semicolon: _ } => {
                    if new_scope {
                        self.pop_scope();
                    }

                    break;
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::PrimaryExpr(primary_expr, arguments, items) => {
                self.visit_primary_expr(primary_expr);

                if let Some(arguments) = arguments {
                    for expr in &mut arguments.xs {
                        self.visit_expr(expr);
                    }
                }

                for (_field_or_method, maybe_arguments) in items {
                    // NOTE: resolving _field_or_method requires resolving the type
                    // first
                    if let Some(arguments) = maybe_arguments {
                        for expr in &mut arguments.xs {
                            self.visit_expr(expr);
                        }
                    }
                }
            }
            Expr::BlockExpr(block_expr) => match block_expr {
                BlockExpr::IfThenElse(cond, _if, _else) => {
                    self.visit_expr(cond);
                    self.visit_block(&mut *_if, true);
                    if let Some(_else) = _else {
                        self.visit_block(&mut *_else, true);
                    }
                }
                BlockExpr::Block(block) => {
                    self.visit_block(block, true);
                }
            },
            Expr::Equals(lhs, rhs)
            | Expr::NotEquals(lhs, rhs)
            | Expr::LessThan(lhs, rhs)
            | Expr::GreaterThan(lhs, rhs)
            | Expr::LessEq(lhs, rhs)
            | Expr::GreaterEq(lhs, rhs)
            | Expr::Add(lhs, rhs)
            | Expr::Sub(lhs, rhs)
            | Expr::Mul(lhs, rhs)
            | Expr::Div(lhs, rhs)
            | Expr::Mod(lhs, rhs)
            | Expr::BitAnd(lhs, rhs)
            | Expr::BitOr(lhs, rhs)
            | Expr::BitXor(lhs, rhs)
            | Expr::LShift(lhs, rhs)
            | Expr::And(lhs, rhs)
            | Expr::Or(lhs, rhs)
            | Expr::RShift(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Expr::Neg(expr) | Expr::BitNot(expr) | Expr::Not(expr) => {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_statement(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::BindVar {
                var,
                mutable,
                value,
            } => {
                self.push_declaration(var, Some(DeclarationKind::Variable { mutable: *mutable }));
                self.visit_expr(value);
            }
            Statement::Return(expr) | Statement::Resume(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                }
            }
            Statement::Assign { var, expr } => {
                self.resolve_name(var);

                self.visit_expr(expr);
            }
            Statement::With(block, items) => {
                self.push_scope(false);

                for (decl, body) in items {
                    // TODO: depending on whether we compile effect handlers as
                    // functions or not we may need to change this
                    // also to handle captures probably
                    self.push_scope(true);

                    for node in &mut decl.args {
                        self.push_declaration(
                            &mut node.name,
                            Some(DeclarationKind::Variable { mutable: false }),
                        );
                    }

                    self.visit_block(body, false);

                    self.pop_scope();
                }

                self.visit_block(block, false);

                self.pop_scope();
            }
            Statement::While(expr, loop_body) => {
                self.visit_expr(expr);
                self.visit_loop_body(loop_body);
            }
            Statement::Loop(loop_body) => {
                self.visit_loop_body(loop_body);
            }
        }
    }

    fn visit_loop_body(&mut self, loop_body: &mut LoopBody) {
        match loop_body {
            LoopBody::Statement(stmt) => self.visit_statement(stmt),
            LoopBody::Block(block) => self.visit_block(block, true),
            LoopBody::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_primary_expr(&mut self, expr: &mut PrimaryExpr) {
        match expr {
            PrimaryExpr::Number(_) => (),
            PrimaryExpr::Bool(_) => (),
            PrimaryExpr::Ident(name) => {
                // TODO: figure out namespaces
                self.resolve_name(&mut name[0]);
            }
            PrimaryExpr::ParExpr(expr) => self.visit_expr(expr),
            PrimaryExpr::Yield(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                }
            }
            PrimaryExpr::Raise(expr) => self.visit_expr(expr),
            PrimaryExpr::Object(_, items) => {
                for (_ident, item) in items {
                    self.visit_expr(item);
                }
            }
            PrimaryExpr::StringLiteral(_) => (),
        }
    }

    fn visit_abi(&mut self, abi: &mut Abi) {
        for item in &mut abi.values {
            match item {
                AbiElem::FnDecl(decl) => {
                    self.push_declaration(&mut decl.0.name, None);

                    for ty in &mut decl.0.input_types {
                        self.visit_type(ty);
                    }

                    if let Some(output_ty) = &mut decl.0.output_type {
                        self.visit_type(output_ty);
                    }
                }
                AbiElem::EffectDecl(decl) => match decl {
                    EffectDecl::EffectSig(decl)
                    | EffectDecl::EventSig(decl)
                    | EffectDecl::ErrorSig(decl) => {
                        self.push_declaration(&mut decl.name, None);
                    }
                },
            }
        }
    }

    fn push_error(&mut self, message: &'static str, span: SimpleSpan) {
        self.errors.push(
            Report::build(ReportKind::Error, span.into_range())
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                // TODO: define error codes
                .with_code(1)
                .with_label(
                    Label::new(span.into_range())
                        .with_message(message)
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    fn visit_type(&mut self, ty: &mut Type) {
        match ty {
            Type::Bool => (),
            Type::String => (),
            Type::U32 => (),
            Type::I32 => (),
            Type::U64 => (),
            Type::I64 => (),
            Type::Intermediate { abi, storage } => {
                self.visit_type(abi);
                self.visit_type(storage);
            }
            Type::TypeApplication(identifier, params) => {
                self.resolve_name(identifier);

                if let Some(params) = params {
                    for ty in params {
                        self.visit_type(ty);
                    }
                }
            }
            Type::Object(typed_bindings) => {
                for (_name, ty) in &mut typed_bindings.values {
                    // NOTE: we can't resolve field accesses without resolving
                    // the type first.
                    self.visit_type(ty);
                }
            }
            Type::Variant { variants } => {
                for (variant, _) in variants {
                    self.push_declaration(variant, None);
                }
            }
            Type::FnType(typed_bindings, output_ty) => {
                for (_, ty) in &mut typed_bindings.values {
                    self.visit_type(ty);
                }

                if let Some(output_ty) = output_ty {
                    self.visit_type(output_ty);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scope_resolution::Visitor;

    use super::do_scope_analysis;
    use ariadne::Source;
    use chumsky::Parser as _;

    #[test]
    fn resolve_usdc_example() {
        let input = include_str!("../../grammar/examples/permissioned_usdc.star");
        let program = crate::starstream_program().parse(input).unwrap();

        // dbg!(&program);

        let ast = do_scope_analysis(program);

        if let Err(errors) = ast {
            for e in errors {
                e.print(Source::from(input)).unwrap();
            }

            panic!();
        }
    }

    #[test]
    fn resolve_oracle_example() {
        let input = include_str!("../../grammar/examples/oracle.star");
        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        if let Err(errors) = ast {
            for e in errors {
                e.print(Source::from(input)).unwrap();
            }

            panic!();
        }
    }

    #[test]
    fn resolve_abi_undeclared_fails() {
        let input = "
            utxo Utxo {
                abi {
                    fn foo(): u32;
                }

                impl Utxo {
                    fn bar(self) {}
                }
            }
        ";

        let ast = do_scope_analysis(crate::starstream_program().parse(input).unwrap());

        assert!(ast.is_err());

        let input = "
            utxo Utxo {
                abi {
                    fn foo(): u32;
                }

                impl Utxo {
                    fn foo(self): u32 {}
                }
            }
        ";

        let ast = do_scope_analysis(crate::starstream_program().parse(input).unwrap());

        assert!(ast.is_ok());
    }

    #[test]
    fn unbound_variable_fails() {
        let input = "
            script {
              fn foo() {
                let x = y + 1;
              }
            }
        ";

        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        assert!(ast.is_err());

        let input = "
            script {
              fn foo(y: u32) {
                let x = y + 1;
              }
            }
        ";

        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        assert!(ast.is_ok());
    }

    #[test]
    fn shadowing() {
        let input = "
            script {
              fn foo() {
                let mut x = 5;
                let y = 42;
                let x = x + y;

                x + x;
              }
            }
        ";

        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        match ast {
            Err(_errors) => {
                unreachable!();
            }
            Ok((_ast, table)) => {
                let vars = table
                    .map
                    .values()
                    .filter(|info| info.source == "x")
                    .collect::<Vec<_>>();

                assert_eq!(vars.len(), 2);

                let first = vars
                    .iter()
                    .map(|info| info.variable.clone().unwrap())
                    .find(|info| info.index == 0)
                    .unwrap();

                let second = vars
                    .iter()
                    .map(|info| info.variable.clone().unwrap())
                    .find(|info| info.index == 2)
                    .unwrap();

                assert!(first.mutable);
                assert!(!second.mutable);

                // 3 variables + the function name
                assert_eq!(table.map.len(), 4 + Visitor::builtins().len());
            }
        }
    }
}
