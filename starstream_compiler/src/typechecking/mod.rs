mod effects;
mod error;
mod linear;
mod types;

use crate::{
    ast::{
        Block, BlockExpr, Expr, ExprOrStatement, FieldAccessExpression, FnDef, IdentifierExpr,
        LoopBody, PrimaryExpr, ProgramItem, Script, Spanned, StarstreamProgram, Statement, Token,
        TokenItem, Utxo, UtxoItem,
    },
    scope_resolution::{SymbolId, Symbols},
};
use ariadne::Report;
use chumsky::span::SimpleSpan;
pub use effects::EffectSet;
use ena::unify::{EqUnifyValue, InPlaceUnificationTable};
use error::{
    error_effect_type_mismatch, error_field_not_found, error_invalid_return_type_for_utxo_main,
    error_linear_variable_affine, error_missing_effect_handler, error_non_signed,
    error_type_mismatch, error_unused_variable, error_variable_used_more_than_once,
};
use linear::{ManyWitness, Multiplicity, ResourceTracker};
use std::collections::{HashMap, HashSet};
pub use types::ComparableType;
use types::{PrimitiveType, TypeVar};

/// Performs type checking and type inference for locals.
///
/// Additionally this populates the `ty` field in the `vars` field of the
/// symbols table, in order for the type of every variable to be available for
/// following passes.
pub fn do_type_inference(
    mut ast: StarstreamProgram,
    symbols: &mut Symbols,
) -> Result<StarstreamProgram, Vec<Report<'static>>> {
    let tc = TypeInference::new(symbols);
    tc.visit_program(&mut ast).map(|_| ast)
}

pub struct TypeInference<'a> {
    symbols: &'a mut Symbols,
    errors: Vec<Report<'static>>,
    warnings: Vec<Report<'static>>,

    unification_table: InPlaceUnificationTable<TypeVar>,

    current_coroutine: Vec<SymbolId>,
    current_function: Vec<SymbolId>,
    current_handler: Vec<SymbolId>,

    multiplicity_tracker: ResourceTracker<SymbolId, SimpleSpan>,

    // checks to do after unification
    utxo_main_block_constraints: Vec<(SimpleSpan, ComparableType)>,
    num_signed_constraints: Vec<(SimpleSpan, ComparableType)>,
    is_numeric: HashSet<TypeVar>,
}

impl<'a> TypeInference<'a> {
    pub fn new(symbols: &'a mut Symbols) -> Self {
        Self {
            symbols,
            errors: vec![],
            warnings: vec![],
            unification_table: InPlaceUnificationTable::new(),
            num_signed_constraints: vec![],
            is_numeric: HashSet::new(),
            utxo_main_block_constraints: vec![],
            multiplicity_tracker: ResourceTracker::new(),

            current_function: vec![],
            current_handler: vec![],
            current_coroutine: vec![],
        }
    }

    pub fn visit_program(
        mut self,
        program: &mut StarstreamProgram,
    ) -> Result<(), Vec<Report<'static>>> {
        for item in &mut program.items {
            match item {
                ProgramItem::Script(script) => self.visit_script(script),
                ProgramItem::Utxo(utxo) => self.visit_utxo(utxo),
                ProgramItem::Token(token) => self.visit_token(token),
                ProgramItem::TypeDef(_type_def) => (),
                // TODO: add these
                ProgramItem::Constant { name, value: _ } => {
                    self.symbols
                        .vars
                        .get_mut(&name.uid.unwrap())
                        .unwrap()
                        .info
                        .ty
                        // TODO: add proper type annotations plus parsing for other types
                        .replace(ComparableType::u32());
                }
                ProgramItem::Abi(_abi) => (),
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        self.apply_substitutions();

        self.check_multiplicity_constraints();

        self.check_utxo_main_block_ty();

        self.check_signed_types();

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(())
        }
    }

    fn new_ty_var(&mut self) -> ComparableType {
        ComparableType::Var(self.unification_table.new_key(None))
    }

    fn apply_substitutions(&mut self) {
        for var in self.symbols.vars.values_mut() {
            let ty = var.info.ty.clone().unwrap();

            let ty = Self::substitute(&mut self.unification_table, ty, &self.is_numeric);

            var.info.ty.replace(ty);
        }
    }

    fn substitute(
        unification_table: &mut InPlaceUnificationTable<TypeVar>,
        ty: ComparableType,
        is_numeric: &HashSet<TypeVar>,
    ) -> ComparableType {
        match ty {
            ComparableType::Primitive(_) => ty,
            ComparableType::Intermediate => ty,
            ComparableType::Void => ty,
            ComparableType::Product(args) | ComparableType::Sum(args) => {
                let mut res = vec![];
                for (name, arg) in args {
                    res.push((name, Self::substitute(unification_table, arg, is_numeric)));
                }

                ComparableType::Product(res)
            }
            ComparableType::FnType(inputs, output) => {
                let mut new_inputs = vec![];

                for input in inputs {
                    new_inputs.push(Self::substitute(unification_table, input, is_numeric));
                }

                let output = Self::substitute(unification_table, *output, is_numeric);

                ComparableType::FnType(new_inputs, output.boxed())
            }
            ComparableType::Utxo(_) => ty,
            ComparableType::Var(type_var) => {
                let root = unification_table.find(type_var);

                match unification_table.probe_value(root) {
                    Some(ty) => Self::substitute(unification_table, ty, is_numeric),
                    None => {
                        if is_numeric.contains(&type_var) {
                            unification_table
                                .unify_var_value(type_var, Some(ComparableType::u32()))
                                .unwrap();

                            Self::substitute(unification_table, ty, is_numeric)
                        } else {
                            // this shouldn't really happen right now, but eventually
                            // it'll need to be handled with generics
                            todo!("unbound variable");
                        }
                    }
                }
            }
            ComparableType::Ref(ty) => ComparableType::Ref(
                Self::substitute(unification_table, (*ty).clone(), is_numeric).boxed(),
            ),
        }
    }

    fn unify_ty_ty(
        &mut self,
        span: SimpleSpan,
        unnorm_left: &ComparableType,
        unnorm_right: &ComparableType,
    ) {
        let left = self.follow_unified_variables(unnorm_left.clone());
        let right = self.follow_unified_variables(unnorm_right.clone());

        match (left, right) {
            (ComparableType::Var(a), ComparableType::Var(b))
                if self.is_numeric.contains(&a) == self.is_numeric.contains(&b) =>
            {
                self.unification_table.unify_var_var(a, b).unwrap()
            }
            (ty, ComparableType::Var(type_var)) | (ComparableType::Var(type_var), ty)
                if self.is_numeric.contains(&type_var) == ty.is_numeric() =>
            {
                ty.occurs_check(&type_var);

                self.unification_table
                    .unify_var_value(type_var, Some(ty))
                    .unwrap();
            }
            (ComparableType::Intermediate, ComparableType::Intermediate) => {}
            (ComparableType::Primitive(lhs), ComparableType::Primitive(rhs)) => {
                self.unify_ty_ty_primitive(span, lhs, rhs)
            }

            (ComparableType::Product(lhs), ComparableType::Product(rhs)) => {
                for ((_, lhs), (_, rhs)) in lhs.iter().zip(rhs.iter()) {
                    self.unify_ty_ty(span, lhs, rhs);
                }
            }
            (ComparableType::Sum(lhs), ComparableType::Sum(rhs)) => {
                for ((_, lhs), (_, rhs)) in lhs.iter().zip(rhs.iter()) {
                    self.unify_ty_ty(span, lhs, rhs);
                }
            }
            (
                ComparableType::FnType(inputs_lhs, output_lhs),
                ComparableType::FnType(inputs_rhs, output_rhs),
            ) => {
                for (lhs, rhs) in inputs_lhs.iter().zip(inputs_rhs.iter()) {
                    self.unify_ty_ty(span, lhs, rhs);
                }

                self.unify_ty_ty(span, &output_lhs, &output_rhs);
            }
            (ComparableType::Utxo(lhs), ComparableType::Utxo(rhs)) if lhs == rhs => {}
            (lhs, rhs) => {
                self.push_error_type_mismatch(span, &lhs, &rhs);
            }
        }
    }

    fn unify_ty_ty_primitive(&mut self, span: SimpleSpan, lhs: PrimitiveType, rhs: PrimitiveType) {
        if lhs != rhs {
            self.push_error_type_mismatch(
                span,
                &ComparableType::Primitive(lhs),
                &ComparableType::Primitive(rhs),
            );
        }
    }

    fn follow_unified_variables(&mut self, ty: ComparableType) -> ComparableType {
        match ty {
            ComparableType::Primitive(_) => ty,
            ComparableType::Utxo(_) => ty,
            ComparableType::Intermediate => ty,
            ComparableType::Void => ty,
            ComparableType::Product(canonical_types) | ComparableType::Sum(canonical_types) => {
                let mut new = vec![];
                for (name, ty) in canonical_types.into_iter() {
                    new.push((name, self.follow_unified_variables(ty)));
                }

                ComparableType::Product(new)
            }
            ComparableType::FnType(inputs_pre, output) => {
                let mut inputs = vec![];
                for ty in inputs_pre.into_iter() {
                    inputs.push(self.follow_unified_variables(ty));
                }

                ComparableType::FnType(
                    inputs,
                    self.follow_unified_variables(*output.clone()).boxed(),
                )
            }
            ComparableType::Var(type_var) => match self.unification_table.probe_value(type_var) {
                Some(ty) => self.follow_unified_variables(ty),
                None => ComparableType::Var(self.unification_table.find(type_var)),
            },
            ComparableType::Ref(normalized_type) => self.follow_unified_variables(*normalized_type),
        }
    }

    fn check_signed_types(&mut self) {
        let mut num_signed_constraints = vec![];
        std::mem::swap(
            &mut num_signed_constraints,
            &mut self.num_signed_constraints,
        );

        for (span, ty) in num_signed_constraints {
            let ty = Self::substitute(&mut self.unification_table, ty, &self.is_numeric);

            match ty {
                ComparableType::Primitive(PrimitiveType::I32) => (),
                ComparableType::Primitive(PrimitiveType::I64) => (),
                _ => self.push_error_non_signed(span, &ty),
            }
        }
    }

    fn check_utxo_main_block_ty(&mut self) {
        let mut utxo_main_block_constraints = vec![];
        std::mem::swap(
            &mut utxo_main_block_constraints,
            &mut self.utxo_main_block_constraints,
        );
        for (span, block_ty) in utxo_main_block_constraints {
            let ty = Self::substitute(&mut self.unification_table, block_ty, &self.is_numeric);

            match ty {
                ComparableType::Primitive(PrimitiveType::Unit) => (),
                ComparableType::Void => (),
                _ => self
                    .errors
                    .push(error_invalid_return_type_for_utxo_main(span)),
            }
        }
    }

    fn check_multiplicity_constraints(&mut self) {
        let multiplicities = self.multiplicity_tracker.finish();

        for (id, var) in &self.symbols.vars {
            let ty = var.info.ty.clone().unwrap();

            let mult = multiplicities
                .get(id)
                .cloned()
                .unwrap_or(Multiplicity::Unused);

            if mult == Multiplicity::Unused {
                self.warnings
                    .push(error_unused_variable(var, ty.is_linear()));
            }

            if ty.is_linear() || ty.is_affine() {
                match mult {
                    Multiplicity::Many {
                        witness: ManyWitness::UsedInLoop { span },
                    } => {
                        self.errors
                            .push(error_variable_used_more_than_once(var, span, span));
                    }
                    Multiplicity::Many {
                        witness: ManyWitness::UsedTwice { first, then },
                    } => {
                        self.errors
                            .push(error_variable_used_more_than_once(var, first, then));
                    }
                    _ => (),
                }
            }

            if ty.is_linear() {
                match mult {
                    Multiplicity::Linear { witness: _ } => (),
                    Multiplicity::Affine { witness: span } => {
                        // TODO: narrow it more
                        self.errors.push(error_linear_variable_affine(var, span));
                    }
                    _ => (),
                }
            }

            if ty.is_affine() {
                match mult {
                    Multiplicity::Unused => (),
                    Multiplicity::Affine { witness: _ } => (),
                    Multiplicity::Linear { witness: _ } => (),
                    _ => (),
                }
            }
        }
    }

    fn visit_utxo(&mut self, utxo: &mut Utxo) {
        let uid = utxo.name.uid.unwrap();
        let interfaces = self
            .symbols
            .types
            .get(&uid)
            .unwrap()
            .info
            .interfaces
            .clone();

        for item in &mut utxo.items {
            match item {
                UtxoItem::Main(main) => {
                    if let Some(args) = &main.type_sig {
                        for (ident, ty) in &args.values {
                            let ty = ty.canonical_form(self.symbols);

                            let var_info = self.symbols.vars.get_mut(&ident.uid.unwrap()).unwrap();

                            var_info.info.ty.replace(ty);
                        }
                    }

                    self.current_coroutine.push(uid);

                    let (span, block_ty, effects) = self.infer_block(&mut main.block);

                    if !effects.is_subset(&interfaces) {
                        self.errors.push(error_effect_type_mismatch(
                            SimpleSpan::from(0..0),
                            &interfaces,
                            &effects,
                        ));
                    }

                    self.current_coroutine.pop();

                    self.utxo_main_block_constraints.push((span, block_ty));
                }
                UtxoItem::Impl(utxo_impl) => {
                    for item in &mut utxo_impl.definitions {
                        self.visit_fn_def(item);
                    }
                }
                UtxoItem::Storage(_storage) => (),
                UtxoItem::Yield(_) => (),
                UtxoItem::Resume(_) => (),
            }
        }
    }

    fn visit_script(&mut self, script: &mut Script) {
        for fn_def in &mut script.definitions {
            self.visit_fn_def(fn_def);
        }
    }

    fn visit_token(&mut self, token: &mut Token) {
        for item in &mut token.items {
            match item {
                TokenItem::Bind(bind) => {
                    // TODO: check (need annotation syntax before)
                    let _effects = self.check_block(&mut bind.0, ComparableType::token_storage());
                }
                TokenItem::Unbind(unbind) => {
                    // TODO: check (need annotation syntax before)
                    let _effects = self.check_block(&mut unbind.0, ComparableType::token_storage());
                }
                TokenItem::Mint(mint) => {
                    let effects = self.check_block(&mut mint.0, ComparableType::token_storage());

                    if !effects.is_empty() {
                        self.errors.push(error_effect_type_mismatch(
                            SimpleSpan::from(0..0),
                            &EffectSet::empty(),
                            &effects,
                        ));
                    }
                }
            }
        }
    }

    fn visit_fn_def(&mut self, fn_def: &mut FnDef) {
        let symbol = fn_def.ident.uid.unwrap();

        self.current_function.push(symbol);

        let inputs = &self.symbols.functions.get(&symbol).unwrap().info.inputs_ty;

        for (arg_ty, arg_before) in inputs.iter().zip(fn_def.inputs.iter()) {
            let ty = arg_ty.canonical_form(self.symbols);

            let var_info = self
                .symbols
                .vars
                .get_mut(&arg_before.name.uid.unwrap())
                .unwrap();

            var_info.info.ty.replace(ty);
        }

        let output = fn_def
            .output
            .as_ref()
            .map(|ty| ty.canonical_form(self.symbols))
            .unwrap_or(ComparableType::unit());

        let actual_effects = self.check_block(&mut fn_def.body, output);

        let fn_info = self.symbols.functions.get_mut(&symbol).unwrap();

        if !actual_effects.is_subset(&fn_info.info.effects) {
            let span = fn_info.span.unwrap();
            self.errors.push(error_effect_type_mismatch(
                span,
                &fn_info.info.effects,
                &actual_effects,
            ));
        }

        self.current_function.pop();
    }

    fn visit_statement(&mut self, statement: &mut Statement) -> EffectSet {
        match statement {
            Statement::BindVar {
                var,
                mutable: _,
                value,
                ty: declared_ty,
            } => {
                let ty = if let Some(expected) = declared_ty {
                    expected.canonical_form(self.symbols)
                } else {
                    self.new_ty_var()
                };

                let symbol_id = var.clone().uid.unwrap();

                self.multiplicity_tracker.declare_variable(symbol_id);

                self.symbols
                    .vars
                    .get_mut(&symbol_id)
                    .unwrap()
                    .info
                    .ty
                    .replace(ty.clone());

                self.check_expr(value, ty)
            }
            Statement::Return(expr) => {
                let current_function = self.current_function.last().unwrap();

                let current_function = self.symbols.functions.get(current_function).unwrap();

                let expected = current_function
                    .info
                    .output_ty
                    .as_ref()
                    .map(|ty| ty.canonical_form(self.symbols))
                    .unwrap_or(ComparableType::unit());

                if let Some(expr) = expr {
                    return self.check_expr(expr, expected);
                } else if !matches!(expected, ComparableType::Primitive(PrimitiveType::Unit)) {
                    // TODO: get span of the return?
                    self.push_error_type_mismatch(
                        SimpleSpan::from(0..0),
                        &expected,
                        &ComparableType::unit(),
                    );
                }

                EffectSet::empty()
            }
            Statement::Resume(expr) => {
                let current_handler = self.current_handler.last().copied();

                let (ty, effects) = if let Some(expr) = expr {
                    self.infer_expr(expr)
                } else {
                    (ComparableType::unit(), EffectSet::empty())
                };

                if let Some(handler) = current_handler {
                    let expected = self
                        .symbols
                        .functions
                        .get(&handler)
                        .unwrap()
                        .info
                        .output_ty
                        .clone()
                        .map(|ty| ty.canonical_form(self.symbols))
                        .unwrap_or(ComparableType::unit());

                    self.unify_ty_ty(
                        expr.as_ref()
                            .map(|expr| expr.span)
                            .unwrap_or(SimpleSpan::from(0..0)),
                        &expected,
                        &ty,
                    );
                }

                effects
            }
            Statement::Assign { var, expr } => {
                let symbol_id = var.clone().uid.unwrap();

                let var_ty = self
                    .symbols
                    .vars
                    .get_mut(&symbol_id)
                    .unwrap()
                    .info
                    .ty
                    .clone()
                    .unwrap();

                self.check_expr(expr, var_ty)
            }
            Statement::With(block, items) => {
                let mut effects =
                    self.check_block(block, ComparableType::Primitive(PrimitiveType::Unit));

                let mut interfaces: HashMap<SymbolId, HashSet<SymbolId>> = HashMap::new();

                for (handler, block) in items {
                    let symbol_id = handler.interface.uid.unwrap();

                    interfaces
                        .entry(symbol_id)
                        .or_default()
                        .insert(handler.ident.uid.unwrap());

                    effects.remove(symbol_id);

                    self.current_handler.push(handler.ident.uid.unwrap());

                    let (_, _, handler_effects) = self.infer_block(block);

                    self.current_handler.pop();

                    effects = effects.combine(handler_effects);
                }

                for (interface, handlers) in interfaces {
                    let interface_info = self.symbols.interfaces.get(&interface).unwrap();

                    for handler in interface_info.info.effects.difference(&handlers) {
                        let effect_info = self.symbols.functions.get(handler).unwrap();

                        let span = effect_info.span.unwrap();

                        self.errors.push(error_missing_effect_handler(
                            span,
                            effect_info,
                            interface_info,
                        ));
                    }
                }

                effects
            }
            Statement::While(expr, loop_body) => {
                let cond_effects = self.check_expr(expr, ComparableType::boolean());

                let loop_body_effects = match loop_body {
                    LoopBody::Statement(statement) => self.visit_statement(statement),
                    LoopBody::Block(block) => {
                        self.multiplicity_tracker.push_loop_scope();
                        let effects = self.infer_block(block).2;
                        self.multiplicity_tracker.pop_loop();

                        effects
                    }
                    LoopBody::Expr(spanned) => self.infer_expr(spanned).1,
                };

                cond_effects.combine(loop_body_effects)
            }
            Statement::Loop(loop_body) => match loop_body {
                LoopBody::Statement(statement) => self.visit_statement(statement),
                LoopBody::Block(block) => self.infer_block(block).2,
                LoopBody::Expr(spanned) => self.infer_expr(spanned).1,
            },
        }
    }

    fn infer_block(&mut self, block: &mut Block) -> (SimpleSpan, ComparableType, EffectSet) {
        let mut curr = block;
        let mut ty = ComparableType::unit();
        // TODO: get span of the block
        let mut span = SimpleSpan::from(0..0);

        let mut effects = EffectSet::empty();

        loop {
            match curr {
                Block::Chain { head, tail } => {
                    match &mut **head {
                        ExprOrStatement::Expr(expr) => {
                            let (last_ty, new_effects) = self.infer_expr(expr);

                            effects = effects.combine(new_effects);

                            ty = last_ty;

                            span = expr.span;
                        }
                        ExprOrStatement::Statement(statement) => {
                            let new_effects = self.visit_statement(statement);

                            effects = effects.combine(new_effects);
                        }
                    }

                    curr = tail;
                }
                Block::Close { semicolon } => {
                    if *semicolon {
                        ty = ComparableType::unit();

                        // TODO: get span of the block
                        span = SimpleSpan::from(0..0);
                    }

                    break;
                }
            }
        }

        (span, ty, effects)
    }

    fn check_block(&mut self, block: &mut Block, expected: ComparableType) -> EffectSet {
        let (span, found, effects) = self.infer_block(block);

        self.unify_ty_ty(span, &found, &expected);

        effects
    }

    fn infer_expr(&mut self, expr: &mut Spanned<Expr>) -> (ComparableType, EffectSet) {
        match &mut expr.node {
            Expr::PrimaryExpr(field_access_expression) => {
                self.infer_field_access_expression(field_access_expression)
            }
            Expr::BlockExpr(block_expr) => match block_expr {
                BlockExpr::IfThenElse(cond, _if, _else) => {
                    let effects_cond = self.check_expr(cond, ComparableType::boolean());

                    self.multiplicity_tracker.push_branch();
                    let (_span, if_ty, effects_if_body) = self.infer_block(_if);

                    self.multiplicity_tracker.push_branch();

                    let effects_else_body = if let Some(_else) = _else {
                        self.check_block(_else, if_ty.clone())
                    } else {
                        EffectSet::empty()
                    };

                    self.multiplicity_tracker.pop_branches(2);

                    (
                        if_ty,
                        effects_cond
                            .combine(effects_if_body)
                            .combine(effects_else_body),
                    )
                }
                BlockExpr::Block(block) => {
                    let inferred = self.infer_block(block);
                    (inferred.1, inferred.2)
                }
            },
            Expr::Equals(lhs, rhs)
            | Expr::NotEquals(lhs, rhs)
            | Expr::LessThan(lhs, rhs)
            | Expr::GreaterThan(lhs, rhs)
            | Expr::LessEq(lhs, rhs)
            | Expr::GreaterEq(lhs, rhs) => {
                let (e1, effects_lhs) = self.infer_expr(lhs);
                let effects_rhs = self.check_expr(rhs, e1.clone());
                (ComparableType::boolean(), effects_lhs.combine(effects_rhs))
            }
            Expr::Add(lhs, rhs)
            | Expr::Sub(lhs, rhs)
            | Expr::Mul(lhs, rhs)
            | Expr::Div(lhs, rhs) => {
                let (e1, effects1) = self.infer_expr(lhs);
                let effects2 = self.check_expr(rhs, e1.clone());
                (e1, effects1.combine(effects2))
            }
            Expr::BitOr(lhs, rhs)
            | Expr::BitXor(lhs, rhs)
            | Expr::LShift(lhs, rhs)
            | Expr::RShift(lhs, rhs)
            | Expr::BitAnd(lhs, rhs)
            | Expr::Mod(lhs, rhs) => {
                let (lhs_ty, effects1) = self.infer_expr(lhs);
                let effects2 = self.check_expr(rhs, lhs_ty.clone());
                (lhs_ty, effects1.combine(effects2))
            }
            Expr::Neg(expr) => {
                let (inner, effects) = self.infer_expr(expr);

                self.num_signed_constraints.push((expr.span, inner.clone()));

                (inner, effects)
            }
            Expr::BitNot(expr) => self.infer_expr(expr),
            Expr::Not(expr) => {
                let effects = self.check_expr(expr, ComparableType::boolean());
                (ComparableType::boolean(), effects)
            }
            Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) => {
                let effects1 = self.check_expr(lhs, ComparableType::boolean());
                let effects2 = self.check_expr(rhs, ComparableType::boolean());

                (ComparableType::boolean(), effects1.combine(effects2))
            }
        }
    }

    fn infer_primary_expression(
        &mut self,
        primary_expr: &mut PrimaryExpr,
    ) -> (ComparableType, EffectSet) {
        match primary_expr {
            PrimaryExpr::Number(_) => {
                let new_ty_var = self.new_ty_var();

                self.is_numeric.insert(match &new_ty_var {
                    ComparableType::Var(type_var) => *type_var,
                    _ => unreachable!(),
                });

                (new_ty_var, EffectSet::empty())
            }
            PrimaryExpr::Bool(_) => (
                ComparableType::Primitive(PrimitiveType::Bool),
                EffectSet::empty(),
            ),
            PrimaryExpr::StringLiteral(_) => (
                ComparableType::Primitive(PrimitiveType::String),
                EffectSet::empty(),
            ),
            PrimaryExpr::Namespace {
                namespaces: _,
                ident,
            } => self.infer_identifier_expression(ident),
            PrimaryExpr::Ident(identifier) => self.infer_identifier_expression(identifier),
            PrimaryExpr::ParExpr(expr) => self.infer_expr(expr),
            PrimaryExpr::Yield(expr) => {
                let current_coroutine = self.current_coroutine.last().unwrap();

                let type_info = &self.symbols.types.get(current_coroutine).unwrap().info;

                let yields = type_info
                    .yield_ty
                    .as_ref()
                    .map(|ty| ty.canonical_form(self.symbols))
                    .unwrap_or(ComparableType::unit());

                let resume = type_info
                    .resume_ty
                    .as_ref()
                    .map(|ty| ty.canonical_form(self.symbols))
                    .unwrap_or(ComparableType::unit());

                if let Some(expr) = expr {
                    let effects = self.check_expr(expr, yields.clone());

                    (resume, effects)
                } else {
                    (resume, EffectSet::empty())
                }
            }
            PrimaryExpr::Raise { ident } => {
                let (ty, mut effects) = self.infer_identifier_expression(ident);

                // TODO: singleton interfaces are currently not registered, so
                // this should always cause a type error.
                effects.add(ident.name.uid.unwrap());

                (ty, effects)
            }
            PrimaryExpr::RaiseNamespaced { namespaces, ident } => {
                let (ty, mut effects) = self.infer_identifier_expression(ident);

                effects.add(namespaces[0].uid.unwrap());

                (ty, effects)
            }
            PrimaryExpr::Object(_, _items) => todo!(),
        }
    }

    fn infer_identifier_expression(
        &mut self,
        identifier: &mut IdentifierExpr,
    ) -> (ComparableType, EffectSet) {
        if let Some(args) = &mut identifier.args {
            let effects = EffectSet::empty();

            // application
            let fn_info = &self
                .symbols
                .functions
                .get(&identifier.name.uid.unwrap())
                .unwrap()
                .info;

            let mut effects = effects.combine(fn_info.effects.clone());

            let inputs: Vec<_> = fn_info
                .inputs_ty
                .iter()
                .map(|ty| ty.canonical_form(self.symbols))
                .collect();

            let output = fn_info
                .output_ty
                .as_ref()
                .map(|ty| ty.canonical_form(self.symbols))
                .unwrap_or(ComparableType::unit());

            for (arg, expected) in args.xs.iter_mut().zip(inputs.iter()) {
                effects = effects.combine(self.check_expr(arg, expected.clone()));
            }

            (output, effects)
        } else {
            let key = identifier.name.uid.unwrap();
            if self.symbols.vars.contains_key(&key) {
                self.multiplicity_tracker
                    .consume(key, identifier.name.span.unwrap());
            }

            (
                self.symbols
                    .vars
                    .get(&identifier.name.uid.unwrap())
                    .unwrap()
                    .info
                    .ty
                    .clone()
                    .unwrap()
                    .clone(),
                EffectSet::empty(),
            )
        }
    }

    fn infer_field_access_expression(
        &mut self,
        expr: &mut FieldAccessExpression,
    ) -> (ComparableType, EffectSet) {
        match expr {
            FieldAccessExpression::PrimaryExpr(primary_expr) => {
                self.infer_primary_expression(primary_expr)
            }
            FieldAccessExpression::FieldAccess { base, field } => {
                let (ty, effects) = self.infer_field_access_expression(base);

                let ty = Self::substitute(&mut self.unification_table, ty, &self.is_numeric);

                let ty = match ty.deref_1() {
                    ComparableType::Product(items) => items
                        .iter()
                        .find_map(|(name, ty)| (name == &field.name.raw).then_some(ty.clone()))
                        .unwrap_or(ComparableType::Void),
                    ComparableType::Utxo(utxo) => {
                        if field.args.is_some() {
                            self.resolve_utxo_method_name(field, utxo);

                            let (ty, effects) = self.infer_identifier_expression(field);

                            return (
                                Self::substitute(&mut self.unification_table, ty, &self.is_numeric),
                                effects,
                            );
                        }

                        let Some(storage) =
                            self.symbols.types.get(&utxo).unwrap().info.storage.as_ref()
                        else {
                            self.errors.push(error_field_not_found(
                                field.name.span.unwrap(),
                                &field.name.raw,
                            ));

                            return (ComparableType::Void, EffectSet::empty());
                        };

                        if let Some(ty) = storage.bindings.values.iter().find_map(|(name, ty)| {
                            (name.raw == field.name.raw).then_some(ty.canonical_form(self.symbols))
                        }) {
                            ty
                        } else {
                            self.errors.push(error_field_not_found(
                                field.name.span.unwrap(),
                                &field.name.raw,
                            ));

                            ComparableType::Void
                        }
                    }
                    _ => ComparableType::Void,
                };

                (ty, effects)
            }
        }
    }

    fn resolve_utxo_method_name(&mut self, field: &mut IdentifierExpr, utxo: SymbolId) {
        // methods are not resolved during name resolution, since it can't be
        // done without first knowing the type of the variable
        //
        // in this case, we try to find a declaration in the utxo/token that
        // matches the name of the method we are typechecking
        if field.name.uid.is_none() {
            let method_declaration = self
                .symbols
                .types
                .get(&utxo)
                .unwrap()
                .info
                .declarations
                .iter()
                .find_map(|uid| {
                    self.symbols
                        .functions
                        .get(uid)
                        .filter(|f| f.source == field.name.raw)
                        .map(|_| uid)
                });

            if let Some(method_declaration) = method_declaration {
                field.name.uid.replace(*method_declaration);
            } else {
                self.errors.push(error_field_not_found(
                    field.name.span.unwrap(),
                    &field.name.raw,
                ));
            }
        }
    }

    fn check_expr(&mut self, expr: &mut Spanned<Expr>, expected: ComparableType) -> EffectSet {
        match (&mut expr.node, expected) {
            (Expr::PrimaryExpr(field_access_expression), expected) => {
                let (ty, effects) = self.infer_field_access_expression(field_access_expression);

                self.unify_ty_ty(expr.span, &ty, &expected);

                effects
            }
            (Expr::Equals(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool))
            | (Expr::NotEquals(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool))
            | (Expr::LessThan(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool))
            | (Expr::LessEq(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool))
            | (Expr::GreaterThan(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool))
            | (Expr::GreaterEq(lhs, rhs), ComparableType::Primitive(PrimitiveType::Bool)) => {
                let (lhs_ty, effects_lhs) = self.infer_expr(lhs);
                let effects_rhs = self.check_expr(rhs, lhs_ty);

                effects_lhs.combine(effects_rhs)
            }
            (Expr::Add(lhs, rhs), expected)
            | (Expr::Sub(lhs, rhs), expected)
            | (Expr::Mul(lhs, rhs), expected)
            | (Expr::Div(lhs, rhs), expected)
            | (Expr::Mod(lhs, rhs), expected) => {
                let effects_lhs = self.check_expr(lhs, expected.clone());
                let effects_rhs = self.check_expr(rhs, expected);

                effects_lhs.combine(effects_rhs)
            }
            (_, expected_ty) => {
                let (actual_ty, effects) = self.infer_expr(expr);

                self.unify_ty_ty(expr.span, &expected_ty, &actual_ty);

                effects
            }
        }
    }

    fn push_error_type_mismatch(
        &mut self,
        span: SimpleSpan,
        expected: &ComparableType,
        found: &ComparableType,
    ) {
        self.errors.push(error_type_mismatch(span, expected, found));
    }

    fn push_error_non_signed(&mut self, span: SimpleSpan, found: &ComparableType) {
        self.errors.push(error_non_signed(span, found));
    }
}

impl EqUnifyValue for ComparableType {}

impl ena::unify::UnifyKey for TypeVar {
    type Value = Option<ComparableType>;

    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> TypeVar {
        TypeVar(u)
    }
    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[cfg(test)]
mod tests {
    use ariadne::Source;
    use chumsky::Parser as _;

    use crate::{do_scope_analysis, scope_resolution::Symbols, typechecking::ComparableType};

    use super::TypeInference;

    fn typecheck_str(input: &str) -> Result<Symbols, Vec<ariadne::Report<'_>>> {
        let program = crate::starstream_program().parse(input).unwrap();

        let (mut ast, mut symbols) = do_scope_analysis(program).unwrap();

        let tc = TypeInference::new(&mut symbols);

        tc.visit_program(&mut ast).map(|_| symbols)
    }

    fn typecheck_str_expect_success(input: &str) {
        let res = typecheck_str(input);

        match res {
            Ok(_) => (),
            Err(_errors) => {
                for e in _errors {
                    e.eprint(Source::from(input)).unwrap();
                }

                panic!();
            }
        }
    }

    fn typecheck_str_expect_error(input: &str) {
        let res = typecheck_str(input);

        match res {
            Ok(_) => panic!("expected error"),
            Err(_errors) => {
                // for e in _errors {
                //     e.eprint(Source::from(input)).unwrap();
                // }
            }
        }
    }

    #[test]
    fn typecheck_script_fn_body() {
        let input = "script {
            fn foo(x: u32): u32 {
                let a = 1;
                let b = 3;
                let c = 4;
                a + b * (c + x)
            }
        }";

        let symbols = typecheck_str(input);

        let symbols = match symbols {
            Ok(symbols) => symbols,
            Err(errors) => {
                for e in errors {
                    e.eprint(Source::from(input)).unwrap();
                }
                panic!();
            }
        };

        let a = symbols
            .vars
            .values()
            .find(|symbol| symbol.source == "a")
            .unwrap();

        assert_eq!(a.info.ty.clone().unwrap(), ComparableType::u32());

        let b = symbols
            .vars
            .values()
            .find(|symbol| symbol.source == "b")
            .unwrap();

        assert_eq!(b.info.ty.clone().unwrap(), ComparableType::u32());

        let c = symbols
            .vars
            .values()
            .find(|symbol| symbol.source == "c")
            .unwrap();

        assert_eq!(c.info.ty.clone().unwrap(), ComparableType::u32());

        let foo = symbols
            .functions
            .values()
            .find(|symbol| symbol.source == "foo")
            .unwrap();

        assert_eq!(
            ComparableType::from_fn_info(&foo.info, &symbols),
            ComparableType::FnType(vec![ComparableType::u32()], ComparableType::u32().boxed())
        );
    }

    #[test]
    fn typecheck_assign_fail() {
        let input = r#"script {
            fn foo() {
                let a = 1;
                let b = 3;
                a = "whatever";
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_assign_succeeds() {
        let input = r#"script {
            fn foo() {
                let a = 1;
                let b = 3;
                a = a + 5;
            }
        }"#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_fn_call_succeeds() {
        let input = r#"script {
            fn bar(): bool {
                foo()
            }

            fn foo(): bool {
                true
            }
        }"#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_if_else() {
        let input = r#"script {
            fn foo(cond: bool): u32 {
                if (cond) {
                    1
                }
                else {
                    2
                }
            }
        }"#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_if_else_fails_different_branches() {
        let input = r#"script {
            fn foo(cond: bool): u32 {
                if (cond) {
                    1
                }
                else {
                    true
                }
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_if_else_fails_non_bool_condition() {
        let input = r#"script {
            fn foo(): u32 {
                if (0) {
                    1
                }
                else {
                    2
                }
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_binops() {
        let input = r#"script {
            fn foo(): bool {
                1 < 3 && 3 <= 5 || 3 == 4
                && 8 > 5 || 6 >= 5 && 3 != 4
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"script {
            fn foo(): bool {
                1 == true 
            }
        }"#;

        typecheck_str_expect_error(input);

        let input = r#"script {
            fn foo(): u32 {
                1 == 1 
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_not() {
        let input = r#"script {
            fn foo(): bool {
                !true
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"script {
            fn foo(): bool {
                !1
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_fn_return() {
        let input = r#"script {
            fn foo(cond: bool): u32 {
                if (cond) {
                    return 1;
                }

                4
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"script {
            fn foo(cond: bool): u32 {
                if (cond) {
                    return "test";
                }

                4
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_field_access() {
        let input = r#"
        typedef T = { a: u32 }

        script {
            fn foo(x: T): u32 {
                x.a
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"
        typedef T = { a: u32 }

        script {
            fn foo(x: T): u32 {
                x.b
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_intermediate_linear() {
        let input = r#"
        script {
            fn foo(x: Intermediate<any, any>) {
                consume(x);
                consume(x);
            }

            fn consume(x: Intermediate<any, any>) {}
        }"#;

        typecheck_str_expect_error(input);

        let input = r#"
        script {
            fn foo(x: Intermediate<any, any>, cond: bool) {
                if(cond) {
                    consume(x);
                }
            }

            fn consume(x: Intermediate<any, any>) {}
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_utxo_main() {
        let input = r#"
            utxo Utxo {
                main {
                    3
                }
            }
        "#;

        typecheck_str_expect_error(input);

        let input = r#"
            utxo Utxo { main { } }
        "#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_infinite_loops() {
        let input = r#"
            utxo Utxo {
                main {
                    loop {

                    }
                }
            }
        "#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_effects() {
        let input = r#"
            abi Error {
                effect E();
            }

            script {
                fn foo() / { Error } {
                    raise Error::E();
                }

                fn bar() / { } {
                    foo();
                }
            }
        "#;

        typecheck_str_expect_error(input);

        let input = r#"
            abi Error {
                effect E();
            }

            abi Other {
                effect E();
            }

            script {
                fn foo() / { Error } {
                    raise Error::E();
                }

                fn bar() / { Error, Other } {
                    foo();
                }
            }
        "#;

        typecheck_str_expect_success(input);

        let input = r#"
            abi Error {
                effect E();
            }

            abi Other {
                effect E();
            }

            script {
                fn foo() / { Error } {
                    raise Error::E();
                }

                fn bar() / { Other } {
                    try {
                        foo()
                    }
                    with Error::E() {
                        print("here");
                    }
                }
            }
        "#;

        typecheck_str_expect_success(input);

        let input = r#"
            abi Error {
                effect E1();
                effect T2();
            }

            script {
                fn foo() / { Error } {
                    raise Error::E1();
                }

                fn bar() / {} {
                    try {
                        foo();
                    }
                    with Error::E1() {
                        print("handling E");
                    }
                }
            }
        "#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_resume() {
        let input = r#"
            abi Error {
                effect E(): u32;
            }

            script {
                fn foo() / {} {
                    try {
                        let a: u32 = raise Error::E();
                    }
                    with Error::E() {
                        resume 42;
                    }
                }
            }
        "#;

        typecheck_str_expect_success(input);

        let input = r#"
            abi Error {
                effect E(): u32;
            }

            script {
                fn foo() / {} {
                    try {
                        raise Error::E();
                    }
                    with Error::E() {
                        resume "forty-two";
                    }
                }
            }
        "#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_nums() {
        let input = r#"script {
            fn foo() {
                let a: u32 = 1;
                let b: u64 = 3;
                let c: u32 = 3;
                let d: i32 = 4;
            }
        }"#;

        typecheck_str_expect_success(input);
    }

    #[test]
    fn typecheck_storage() {
        let input = r#"
        abi Abi {
            fn foo();
        }

        utxo U {
            storage {
                x: string;
            }

            impl Abi {
                fn foo(self) {
                    print(self.x);
                }
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"
        abi Abi {
            fn foo();
        }

        utxo U {
            storage {
                x: bool;
            }

            impl Abi {
                fn foo(self) {
                    print(self.x);
                }
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_utxo_yield() {
        let input = r#"
        utxo U {
            Yield u32
            Resume string

            main {
                let r: string = yield 3;
            }
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"
        utxo U {
            Yield u32
            Resume string

            main {
                let r: bool = yield 3;
            }
        }"#;

        typecheck_str_expect_error(input);

        let input = r#"
        utxo U {
            Yield u32
            Resume string

            main {
                let r: string = yield "hello world";
            }
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_utxo_raise() {
        let input = r#"
        abi Abi {
            effect E();
        }

        utxo U {
            main {
                raise Abi::E();
            }

            impl Abi {}
        }"#;

        typecheck_str_expect_success(input);

        let input = r#"
        abi Abi {
            effect E();
        }

        abi UnimplementedAbi {
            effect E();
        }

        utxo U {
            main {
                raise UnimplementedAbi::E();
            }

            impl Abi {}
        }"#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_utxo_resume() {
        let input = r#"
        utxo U {
            Resume u32

            main {
                yield;
            }
        }

        script {
            fn main(utxo: U, x: u32) {
                let utxo = utxo.resume(x);
                utxo.resume(x);
            }
        }

        "#;

        typecheck_str_expect_success(input);

        let input = r#"
        utxo U {
            Resume u32

            main {
                yield;
            }
        }

        script {
            fn main(utxo: U, x: u32) {
                utxo.resume(x);
                utxo.resume(x);
            }
        }
        "#;

        typecheck_str_expect_error(input);
    }

    #[test]
    fn typecheck_utxo_methods() {
        let utxo = r#"
        abi Foo {
            fn foo(): u32;
        }

        utxo U {
            storage {
                x: u32;
            }

            main {
                yield;
            }

            impl Foo {
                fn foo(self): u32 {
                    self.x
                }
            }
        }
        "#;

        let script = r#"script {
            fn main(utxo: U) {
                let x: u32 = utxo.foo();
            }
        }
        "#;

        let input = format!("{utxo}\n{script}");

        typecheck_str_expect_success(&input);

        let script = r#"script {
            fn main(utxo: U) {
                let x: string = utxo.foo();
            }
        }
        "#;

        let input = format!("{utxo}\n{script}");

        typecheck_str_expect_error(&input);
    }
}
