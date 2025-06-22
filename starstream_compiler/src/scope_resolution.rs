use crate::ast::{
    Abi, AbiElem, Block, BlockExpr, EffectDecl, Expr, ExprOrStatement, FnDef, Identifier, LoopBody,
    PrimaryExpr, ProgramItem, Script, StarstreamProgram, Statement, Token, TokenItem, Type,
    TypeDef, Utxo, UtxoItem,
};
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::span::SimpleSpan;
use std::collections::{HashMap, HashSet};

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
    pub vars: HashMap<SymbolId, SymbolInformation<VarInfo>>,
    pub types: HashMap<SymbolId, SymbolInformation<TypeInfo>>,
    pub functions: HashMap<SymbolId, SymbolInformation<FuncInfo>>,
    pub constants: HashMap<SymbolId, SymbolInformation<ConstInfo>>,
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub index: u64,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub declarations: HashSet<SymbolId>,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {}

#[derive(Debug, Clone)]
pub struct ConstInfo {}

#[derive(Debug)]
pub struct SymbolInformation<T> {
    pub source: String,
    pub span: Option<SimpleSpan>,
    pub info: T,
}

#[derive(Debug)]
pub struct Scope {
    var_declarations: HashMap<String, SymbolId>,
    function_declarations: HashMap<String, SymbolId>,
    type_declarations: HashMap<String, SymbolId>,
    is_function_scope: bool,
    is_type_scope: Option<SymbolId>,
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

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable,
    Function,
    Type,
    Constant,
}

impl Visitor {
    fn new() -> Self {
        Visitor {
            stack: vec![],
            locals: vec![],
            symbol_counter: 0,
            errors: vec![],
            symbols: Symbols {
                vars: Default::default(),
                types: Default::default(),
                functions: Default::default(),
                constants: Default::default(),
            },
        }
    }

    fn push_type_scope(&mut self, type_id: SymbolId) {
        self.stack.push(Scope {
            var_declarations: HashMap::new(),
            function_declarations: HashMap::new(),
            type_declarations: HashMap::new(),
            is_function_scope: false,
            is_type_scope: Some(type_id),
        });
    }

    fn push_function_scope(&mut self) {
        self.stack.push(Scope {
            var_declarations: HashMap::new(),
            function_declarations: HashMap::new(),
            type_declarations: HashMap::new(),
            is_function_scope: true,
            is_type_scope: None,
        });

        self.locals.push(0);
    }

    fn push_scope(&mut self) {
        self.stack.push(Scope {
            var_declarations: HashMap::new(),
            function_declarations: HashMap::new(),
            type_declarations: HashMap::new(),
            is_function_scope: false,
            is_type_scope: None,
        });
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

    fn builtins() -> Vec<(&'static str, SymbolKind, Option<Vec<&'static str>>)> {
        // TODO: mostly just to get the examples working
        // these probably would have to be some sort of import?
        vec![
            ("CoordinationCode", SymbolKind::Function, None),
            ("ThisCode", SymbolKind::Function, None),
            ("assert", SymbolKind::Function, None),
            ("IsTxSignedBy", SymbolKind::Function, None),
            ("None", SymbolKind::Function, None),
            ("context", SymbolKind::Constant, None),
            ("String", SymbolKind::Type, None),
            ("u32", SymbolKind::Type, None),
            ("u64", SymbolKind::Type, None),
            ("i32", SymbolKind::Type, None),
            ("i64", SymbolKind::Type, None),
            ("PublicKey", SymbolKind::Type, None),
            ("Caller", SymbolKind::Function, None),
            ("PayToPublicKeyHash", SymbolKind::Type, Some(vec![("new")])),
            ("List", SymbolKind::Type, Some(vec!["new"])),
            ("print", SymbolKind::Function, None),
        ]
    }

    fn add_builtins(&mut self) {
        for (builtin, kind, fns) in Self::builtins() {
            match kind {
                SymbolKind::Variable => {
                    self.push_var_declaration(&mut Identifier::new(builtin, None), false);
                }
                SymbolKind::Type => {
                    let type_id = self.push_type_declaration(&mut Identifier::new(builtin, None));

                    self.push_type_scope(type_id);

                    if let Some(fns) = fns {
                        for f in fns {
                            self.push_function_declaration(&mut Identifier::new(f, None));
                        }
                    }

                    self.pop_scope();
                }
                SymbolKind::Function => {
                    self.push_function_declaration(&mut Identifier::new(builtin, None));
                }
                SymbolKind::Constant => {
                    self.push_constant_declaration(&mut Identifier::new(builtin, None));
                }
            }
        }
    }

    fn visit_program(&mut self, program: &mut StarstreamProgram) {
        self.push_scope();

        self.add_builtins();

        for item in &mut program.items {
            match item {
                ProgramItem::TypeDef(type_def) => self.visit_type_def(type_def),
                ProgramItem::Token(token) => {
                    self.push_type_declaration(&mut token.name);
                }
                ProgramItem::Script(_script) => (),
                ProgramItem::Utxo(utxo) => {
                    self.push_type_declaration(&mut utxo.name);
                }
                ProgramItem::Constant { name, value: _ } => {
                    self.push_constant_declaration(name);
                }
            }
        }

        let mut items = program.items.iter_mut().collect::<Vec<_>>();

        items.sort_by_key(|item| match item {
            ProgramItem::Token(_token) => 0,
            ProgramItem::Utxo(_utxo) => 1,
            ProgramItem::TypeDef(_type_def) => 2,
            ProgramItem::Constant { name: _, value: _ } => 3,
            ProgramItem::Script(_script) => 4,
        });

        for item in items {
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
            self.push_function_declaration(&mut definition.ident);
        }

        for definition in &mut script.definitions {
            self.visit_fn_def(definition);
        }
    }

    pub fn visit_utxo(&mut self, utxo: &mut Utxo) {
        let uid = self.push_type_declaration(&mut utxo.name);

        // we need to put these into scope before doing anything else
        self.push_type_scope(uid);

        for item in &mut utxo.items {
            if let UtxoItem::Abi(abi) = item {
                self.visit_abi(abi);
            }
        }

        for item in &mut utxo.items {
            match item {
                UtxoItem::Abi(_) => (),
                UtxoItem::Main(main) => {
                    // TODO: may actually want to get the "main" span
                    self.push_function_declaration(&mut Identifier::new("new", None));
                    self.visit_block(&mut main.block, true);
                }
                UtxoItem::Impl(utxo_impl) => {
                    for definition in &mut utxo_impl.definitions {
                        self.visit_fn_def(definition);
                    }
                }
                UtxoItem::Storage(_) => {}
            }
        }

        self.pop_scope();
    }

    pub fn visit_token(&mut self, token: &mut Token) {
        let uid = self.push_type_declaration(&mut token.name);

        self.push_type_scope(uid);

        self.push_function_declaration(&mut Identifier::new("type", None));

        for item in &mut token.items {
            match item {
                TokenItem::Abi(abi) => self.visit_abi(abi),
                TokenItem::Bind(bind) => {
                    self.push_function_scope();
                    self.push_var_declaration(&mut Identifier::new("self", None), true);
                    self.visit_block(&mut bind.0, false);
                    self.pop_scope();
                }
                TokenItem::Unbind(unbind) => {
                    self.push_function_scope();
                    self.push_var_declaration(&mut Identifier::new("self", None), true);
                    self.visit_block(&mut unbind.0, false);
                    self.pop_scope();
                }
                TokenItem::Mint(mint) => {
                    self.push_function_declaration(&mut Identifier::new("mint", None));
                    self.push_function_scope();
                    self.push_var_declaration(&mut Identifier::new("self", None), true);
                    self.visit_block(&mut mint.0, false);
                    self.pop_scope();
                }
            }
        }

        self.pop_scope();
    }

    pub fn visit_type_def(&mut self, type_def: &mut TypeDef) {
        self.push_type_declaration(&mut type_def.name);
    }

    fn visit_fn_def(&mut self, definition: &mut FnDef) {
        self.resolve_name(&mut definition.ident, SymbolKind::Function);

        self.push_function_scope();

        for node in &mut definition.inputs {
            self.push_var_declaration(&mut node.name, false);
        }

        self.visit_block(&mut definition.body, false);

        self.pop_scope();
    }

    fn new_symbol(&mut self, ident: &mut Identifier) -> SymbolId {
        let id = self.symbol_counter;
        self.symbol_counter += 1;

        let symbol = SymbolId { id };
        ident.uid.replace(symbol);
        symbol
    }

    fn push_var_declaration(&mut self, ident: &mut Identifier, mutable: bool) -> SymbolId {
        let symbol = self.new_symbol(ident);

        let scope = self.stack.last_mut().unwrap();
        scope.var_declarations.insert(ident.raw.clone(), symbol);

        // TODO: handle error
        let fn_scope = self.locals.last_mut().unwrap();
        let index = *fn_scope;
        *fn_scope += 1;
        let var_info = VarInfo { index, mutable };

        self.symbols.vars.insert(
            symbol,
            SymbolInformation {
                source: ident.raw.clone(),
                span: ident.span,
                info: var_info,
            },
        );

        symbol
    }

    fn push_constant_declaration(&mut self, ident: &mut Identifier) -> SymbolId {
        let symbol = self.new_symbol(ident);

        let scope = self.stack.last_mut().unwrap();
        scope.var_declarations.insert(ident.raw.clone(), symbol);

        self.symbols.constants.insert(
            symbol,
            SymbolInformation {
                source: ident.raw.clone(),
                span: ident.span,
                info: ConstInfo {},
            },
        );

        symbol
    }

    fn push_function_declaration(&mut self, ident: &mut Identifier) -> SymbolId {
        let symbol = self.new_symbol(ident);

        self.symbols.functions.insert(
            symbol,
            SymbolInformation {
                source: ident.raw.clone(),
                span: ident.span,
                info: FuncInfo {},
            },
        );

        let scope = self.stack.last_mut().unwrap();

        if let Some(prev) = scope
            .function_declarations
            .insert(ident.raw.clone(), symbol)
        {
            let prev = self.symbols.functions.get(&prev).unwrap().span.unwrap();

            self.push_redeclaration_error(ident.span.unwrap(), prev);
        }

        let type_scope = self
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.is_type_scope);

        if let Some(type_scope) = type_scope {
            let type_information = self.symbols.types.get_mut(&type_scope).unwrap();

            let inserted = type_information.info.declarations.insert(symbol);

            if !inserted {
                // fine to unwrap since otherwise inserted would be true
                let prev = type_information.info.declarations.get(&symbol).unwrap();

                // TODO: cleanup the panics (compiler error)
                let prev = self.symbols.functions.get(prev).unwrap();

                self.push_redeclaration_error(ident.span.unwrap(), prev.span.unwrap());
            }
        }

        symbol
    }

    fn push_type_declaration(&mut self, ident: &mut Identifier) -> SymbolId {
        let symbol = self.new_symbol(ident);

        let scope = self.stack.last_mut().unwrap();
        scope.type_declarations.insert(ident.raw.clone(), symbol);

        self.symbols.types.insert(
            symbol,
            SymbolInformation {
                source: ident.raw.clone(),
                span: ident.span,
                info: TypeInfo {
                    declarations: HashSet::new(),
                },
            },
        );

        symbol
    }

    fn resolve_name(
        &mut self,
        identifier: &mut Identifier,
        symbol_kind: SymbolKind,
    ) -> Option<SymbolId> {
        let resolution = self.stack.iter().rev().find_map(|scope| match symbol_kind {
            SymbolKind::Variable | SymbolKind::Constant => {
                scope.var_declarations.get(&identifier.raw).cloned()
            }
            SymbolKind::Function => scope
                .function_declarations
                .get(&identifier.raw)
                .cloned()
                .or_else(|| scope.type_declarations.get(&identifier.raw).cloned()),
            SymbolKind::Type => scope.type_declarations.get(&identifier.raw).cloned(),
        });

        let Some(resolved_name) = resolution else {
            self.push_not_found_error(identifier.span.unwrap());
            return None;
        };

        identifier.uid.replace(resolved_name);

        Some(resolved_name)
    }

    fn visit_block(&mut self, block: &mut Block, new_scope: bool) {
        // Blocks as syntax elements can be both part of expressions or just
        // function definitions. We could create an inner scope for the function
        // definition, but it's probably better to not increase depth
        if new_scope {
            self.push_scope();
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
                self.visit_primary_expr(primary_expr, arguments.is_some());

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
                self.push_var_declaration(var, *mutable);
                self.visit_expr(value);
            }
            Statement::Return(expr) | Statement::Resume(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                }
            }
            Statement::Assign { var, expr } => {
                self.resolve_name(var, SymbolKind::Variable);

                self.visit_expr(expr);
            }
            Statement::With(block, items) => {
                self.push_scope();

                for (decl, body) in items {
                    // TODO: depending on whether we compile effect handlers as
                    // functions or not we may need to change this
                    // also to handle captures probably
                    self.push_function_scope();

                    for node in &mut decl.args {
                        self.push_var_declaration(&mut node.name, false);
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

    fn visit_primary_expr(&mut self, expr: &mut PrimaryExpr, is_function_call: bool) {
        match expr {
            PrimaryExpr::Number(_) => (),
            PrimaryExpr::Bool(_) => (),
            PrimaryExpr::Ident(name) => {
                if name.len() > 1 {
                    let ident_index = name.len() - 1;
                    let (tys, ident) = name.split_at_mut(ident_index);

                    let mut namespace = None;

                    for ty in tys {
                        if let Some(type_id) = self.resolve_name(ty, SymbolKind::Type) {
                            self.symbols.types.get(&type_id);

                            namespace.replace(type_id);
                        }
                    }

                    let Some(namespace) = namespace else {
                        return;
                    };

                    let f = self
                        .symbols
                        .types
                        .get(&namespace)
                        .unwrap()
                        .info
                        .declarations
                        .iter()
                        .find(|uid| {
                            self.symbols
                                .functions
                                .get(uid)
                                .map(|finfo| finfo.source == ident[0].raw)
                                .unwrap_or(false)
                        });

                    if let Some(f) = f {
                        ident[0].uid.replace(*f);
                    } else {
                        self.push_not_found_error(ident[0].span.unwrap());
                    }
                } else {
                    self.resolve_name(
                        &mut name[0],
                        if is_function_call {
                            SymbolKind::Function
                        } else {
                            SymbolKind::Variable
                        },
                    );
                }
            }
            PrimaryExpr::ParExpr(expr) => self.visit_expr(expr),
            PrimaryExpr::Yield(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                }
            }
            PrimaryExpr::Raise(expr) => {
                // dbg!(&self.stack);
                self.visit_expr(expr)
            }
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
                    self.push_function_declaration(&mut decl.0.name);

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
                        self.push_type_declaration(&mut decl.name);
                    }
                },
            }
        }
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
                self.resolve_name(identifier, SymbolKind::Type);

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
                    self.push_function_declaration(variant);
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

    fn push_not_found_error(&mut self, span: SimpleSpan) {
        self.errors.push(
            Report::build(ReportKind::Error, span.into_range())
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                // TODO: define error codes across the compiler
                .with_code(1)
                .with_label(
                    Label::new(span.into_range())
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                )
                .finish(),
        );
    }

    fn push_redeclaration_error(&mut self, prev: SimpleSpan, new: SimpleSpan) {
        self.errors.push(
            Report::build(ReportKind::Error, new.into_range())
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                // TODO: define error codes across the compiler
                .with_code(2)
                .with_label(
                    Label::new(new.into_range())
                        .with_message("function already declared")
                        .with_color(Color::Red),
                )
                .with_label(
                    Label::new(prev.into_range())
                        .with_message("here")
                        .with_color(Color::BrightRed),
                )
                .finish(),
        );
    }
}

#[cfg(test)]
mod tests {
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
                    .vars
                    .values()
                    .filter(|info| info.source == "x")
                    .collect::<Vec<_>>();

                assert_eq!(vars.len(), 2);

                let first = vars.iter().find(|info| info.info.index == 0).unwrap();

                let second = vars.iter().find(|info| info.info.index == 2).unwrap();

                assert!(first.info.mutable);
                assert!(!second.info.mutable);

                assert_eq!(table.vars.len(), 3);
            }
        }
    }

    #[test]
    fn script_function_order() {
        let input = "
            script {
              fn foo() {
                  bar();
              }

              fn bar() {
              }
            }
        ";

        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        match ast {
            Err(_errors) => {
                unreachable!();
            }
            Ok((_ast, _table)) => {}
        }
    }

    #[test]
    fn script_function_same_name_fails() {
        let input = "
            script {
              fn foo() {
              }

              fn foo() {
              }
            }
        ";

        let program = crate::starstream_program().parse(input).unwrap();

        let ast = do_scope_analysis(program);

        match ast {
            Err(_errors) => {
                // for e in _errors {
                //     e.eprint(Source::from(input)).unwrap();
                // }
            }
            Ok((_ast, _table)) => {
                unreachable!();
            }
        }
    }
}
