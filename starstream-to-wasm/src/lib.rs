use std::{borrow::Cow, collections::HashMap};

use miette::{Diagnostic, LabeledSpan};
use starstream_types::*;
use thiserror::Error;
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
pub fn compile(program: &TypedProgram) -> (Option<Vec<u8>>, Vec<CompileError>) {
    let mut compiler = Compiler::default();
    compiler.visit_program(program);
    compiler.finish()
}

/// A Wasm compiler error.
#[derive(Debug, Error)]
#[error("{message}")]
pub struct CompileError {
    message: String,
    span: Span,
}

impl Diagnostic for CompileError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(
            LabeledSpan::new_primary_with_span(Some(self.message.clone()), self.span.into_range()),
        )))
    }
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

    // Component binary output.
    /*
        (@custom "component-type"
            (component ;; the component embedded in the file exports a single Package type
                (type
                    (component ;; that Package exports a single World type
                        (type
                            (component ;; that World exports our actual functions
                                (type (func ...))
                                (export "function-name" (func (type 0)))
                            )
                        )
                        (export "namespace-name:package-name/world-name@0.1.0" (component (type 0)))
                    )
                )
                (export "anything here, it's ignored" (type 0))
            )
        )
    */
    world_type: ComponentType,
    wit_types: HashMap<Type, ComponentValType>,

    // Diagnostics.
    fatal: bool,
    errors: Vec<CompileError>,

    // Function building.
    raw_func_type_cache: HashMap<FuncType, u32>,
}

impl Compiler {
    /// After [Compiler::visit_program], this function collates all the
    /// in-progress sections into an actual Wasm binary module.
    fn finish(self) -> (Option<Vec<u8>>, Vec<CompileError>) {
        // TODO: any other final activity on the sections here, such as
        // committing constants to the memory/data section.

        // Verify
        assert_eq!(self.functions.len(), self.code.len());

        // No reports generated beyond this point, so bail if fatal is set.
        if self.fatal {
            return (None, self.errors);
        }

        // The package type must always have 0 imports and 1 export which is the world.
        // Export must be named namespace:package/world, but @version is optional.
        let mut package_type = ComponentType::new();
        package_type.ty().component(&self.world_type);
        package_type.export(
            "my-namespace:my-package/my-world",
            ComponentTypeRef::Component(0),
        );

        let mut component_types = ComponentTypeSection::new();
        let mut component_exports: ComponentExportSection = ComponentExportSection::new();
        // Embedded component must have 1 export which is the package type.
        // Export name doesn't matter.
        component_types.component(&package_type);
        component_exports.export("x", ComponentExportKind::Type, 0, None);

        // Write component sections to embedded component.
        let mut component = Component::new();
        component.section(&CustomSection {
            name: Cow::Borrowed("wit-component-encoding"),
            data: Cow::Borrowed(b"\x04\x00"),
        });
        component.section(&component_types);
        component.section(&component_exports);
        let component = component.finish();

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
        module.section(&CustomSection {
            name: Cow::Borrowed("component-type"),
            data: Cow::Owned(component),
        });

        (Some(module.finish()), self.errors)
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

    fn add_component_type(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        let idx = self.world_type.type_count();
        (idx, self.world_type.ty())
    }

    fn star_to_component_type(&mut self, ty: &Type) -> ComponentValType {
        if let Some(&cvt) = self.wit_types.get(ty) {
            cvt
        } else {
            let cvt = match ty {
                Type::Var(_) => todo!(),
                Type::Int => ComponentValType::Primitive(PrimitiveValType::S64),
                Type::Bool => ComponentValType::Primitive(PrimitiveValType::Bool),
                Type::Unit => {
                    let (idx, ty) = self.add_component_type();
                    ty.defined_type()
                        .tuple(std::iter::empty::<ComponentValType>());
                    ComponentValType::Type(idx)
                }
                Type::Function(_, _) => todo!(),
                Type::Tuple(items) => {
                    let children: Vec<_> = items
                        .iter()
                        .map(|ty| self.star_to_component_type(ty))
                        .collect();
                    let (idx, ty) = self.add_component_type();
                    ty.defined_type().tuple(children);
                    ComponentValType::Type(idx)
                }
                Type::Record(record) => {
                    let fields: Vec<_> = record
                        .fields
                        .iter()
                        .map(|f| (f.name.as_str(), self.star_to_component_type(&f.ty)))
                        .collect();
                    let (idx, ty) = self.add_component_type();
                    ty.defined_type().record(fields);
                    ComponentValType::Type(idx)
                }
                Type::Enum(_enum_variant_types) => todo!(),
            };
            self.wit_types.insert(ty.clone(), cvt);
            cvt
        }
    }

    fn add_component_func_type(&mut self, function: &TypedFunctionDef) -> u32 {
        let mut params = Vec::with_capacity(function.params.len());
        for p in &function.params {
            params.push((p.name.as_str(), self.star_to_component_type(&p.ty)));
        }

        let result = match &function.return_type {
            // tuple<> is not a valid return type, so return none
            Type::Unit => None,
            other => Some(self.star_to_component_type(other)),
        };

        let (idx, ty) = self.add_component_type();
        ty.function().params(params).result(result);
        idx
    }

    // ------------------------------------------------------------------------
    // Visitors

    /// Root visitor called by [compile] to start walking the AST for a program,
    /// building the Wasm sections on the way.
    fn visit_program(&mut self, program: &TypedProgram) {
        for definition in &program.definitions {
            match definition {
                TypedDefinition::Function(func) => self.visit_function(func),
                TypedDefinition::Struct(struct_) => self.visit_struct(struct_),
                TypedDefinition::Enum(_) => self.todo("enums are not supported in Wasm yet".into()),
            }
        }
    }

    fn visit_function(&mut self, function: &TypedFunctionDef) {
        let mut func = Function::default();

        let mut locals = HashMap::<String, u32>::new();
        let mut params = Vec::with_capacity(16);
        for p in &function.params {
            locals.insert(p.name.name.clone(), u32::try_from(params.len()).unwrap());
            if !star_to_core_types(&mut params, &p.ty) {
                self.push_error(
                    p.name
                        .span
                        .or(function.name.span)
                        .unwrap_or(Span::from(0..0)),
                    format!("unknown lowering for parameter type {:?}", p.ty),
                );
            }
        }
        func.num_locals = params.len() as u32;

        let mut results = Vec::with_capacity(1);
        if !star_to_core_types(&mut results, &function.return_type) {
            self.push_error(
                function.name.span.unwrap_or(Span::from(0..0)),
                format!(
                    "unknown lowering for return type {:?}",
                    function.return_type
                ),
            );
        }

        let _ = self.visit_block(
            &mut func,
            &(&() as &dyn Locals, &locals),
            &function.body,
            &function.return_type,
        );
        func.instructions().end();

        let idx = self.add_function(FuncType::new(params, results), func);

        match function.export {
            Some(FunctionExport::Script) => {
                self.exports.export(
                    &to_kebab_case(function.name.as_str()),
                    ExportKind::Func,
                    idx,
                );
                self.world_export_fn(function);
            }
            None => {}
        }
    }

    fn world_export_fn(&mut self, function: &TypedFunctionDef) {
        let idx = self.add_component_func_type(function);
        self.world_type.export(
            &to_kebab_case(function.name.as_str()),
            ComponentTypeRef::Func(idx),
        );
    }

    fn visit_struct(&mut self, struct_: &TypedStructDef) {
        // Export to WIT.
        let ty = self.star_to_component_type(&struct_.ty);
        let ComponentValType::Type(idx) = ty else {
            unreachable!()
        };
        // "Exporting" a type consists of importing it with an equality constraint.
        let new_idx = self.world_type.type_count();
        self.world_type.import(
            &to_kebab_case(struct_.name.as_str()),
            ComponentTypeRef::Type(TypeBounds::Eq(idx)),
        );
        // Future uses must also refer to the imported version.
        self.wit_types
            .insert(struct_.ty.clone(), ComponentValType::Type(new_idx));
    }

    /// Start a new identifier scope and generate bytecode for the statements
    /// of the block in sequence. Only creates a Wasm `block` when specifically
    /// needed for control flow reasons.
    fn visit_block<'i>(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &'i TypedBlock,
        return_: &Type,
    ) -> ImResult<'i> {
        let mut locals = HashMap::new();
        for statement in &block.statements {
            match statement {
                TypedStatement::Expression(expr) => {
                    let im = self.visit_expr(func, &(parent, &locals), expr.span, &expr.node);
                    self.discard_r(func, im);
                }
                TypedStatement::VariableDeclaration {
                    mutable: _,
                    name,
                    value,
                } => {
                    let value = self.visit_expr(func, &(parent, &locals), value.span, &value.node);
                    match value {
                        Err(()) => {}
                        Ok(Intermediate::Stack(Type::Int)) => {
                            let local = func.add_local(ValType::I64);
                            func.instructions().local_set(local);
                            locals.insert(name.name.clone(), local);
                        }
                        Ok(value) => self.todo(format!("VariableDeclaration({value:?})")),
                    }
                }
                TypedStatement::Assignment { target, value } => {
                    if let Some(local) = (parent, &locals).get(&target.name) {
                        let value =
                            self.visit_expr(func, &(parent, &locals), value.span, &value.node);
                        match value {
                            Err(()) => {}
                            Ok(Intermediate::Stack(&Type::Int)) => {
                                func.instructions().local_set(local);
                            }
                            Ok(value) => self.todo(format!("VariableDeclaration({value:?})")),
                        }
                    } else {
                        self.push_error(
                            target.span.unwrap_or(value.span),
                            format!("unknown name {:?}", target.name),
                        );
                    }
                }
                // Recursive
                TypedStatement::Block(block) => {
                    let im = self.visit_block(func, &(parent, &locals), block, return_);
                    self.discard_r(func, im);
                }
                TypedStatement::If {
                    branches,
                    else_branch,
                } => {
                    // Emit basic double-block and trust the optimizer.
                    func.instructions().block(BlockType::Empty);
                    for (condition, block) in branches {
                        // Inner block for each condition.
                        func.instructions().block(BlockType::Empty);
                        let im = self.visit_expr(
                            func,
                            &(parent, &locals),
                            condition.span,
                            &condition.node,
                        );
                        if let Ok(im) = im {
                            assert!(matches!(im, Intermediate::Stack(&Type::Bool)));
                        }
                        func.instructions().i32_eqz(); // If condition is false,
                        func.instructions().br_if(0); // then try the next condition.
                        let im = self.visit_block(func, &(parent, &locals), block, return_);
                        self.discard_r(func, im);
                        func.instructions().br(1); // Go past end.
                        func.instructions().end();
                    }
                    // Final `else` branch is just inline.
                    if let Some(else_branch) = else_branch {
                        let im = self.visit_block(func, &(parent, &locals), else_branch, return_);
                        self.discard_r(func, im);
                    }
                    // End.
                    func.instructions().end();
                }
                TypedStatement::While { condition, body } => {
                    func.instructions().block(BlockType::Empty); // br(1) is break
                    func.instructions().loop_(BlockType::Empty); // br(0) is continue

                    let im =
                        self.visit_expr(func, &(parent, &locals), condition.span, &condition.node);
                    if let Ok(im) = im {
                        assert!(matches!(im, Intermediate::Stack(&Type::Bool)));
                    }
                    // if condition == 0, break
                    func.instructions().i32_eqz();
                    func.instructions().br_if(1);
                    // contents
                    let im = self.visit_block(func, &(parent, &locals), body, return_);
                    self.discard_r(func, im);
                    // continue
                    func.instructions().br(0).end().end();
                }
                TypedStatement::Return(Some(expr)) => {
                    assert_eq!(&expr.node.ty, return_); // Should be enforced by typechecker.
                    let _ = self.visit_expr(func, &(parent, &locals), expr.span, &expr.node);
                    func.instructions().return_();
                }
                TypedStatement::Return(None) => {
                    assert_eq!(return_, &Type::Unit); // Should be enforced by typechecker.
                    func.instructions().return_();
                }
            }
        }

        if let Some(expr) = &block.tail_expression {
            self.visit_expr(func, &(parent, &locals), expr.span, &expr.node)
        } else {
            Ok(Intermediate::Stack(&Type::Unit))
        }
    }

    /// Insert bytecode to discard the given [Intermediate], such as during
    /// statement expressions.
    fn discard(&mut self, func: &mut Function, im: Intermediate) {
        for _ in 0..self.count_stack_slots(&im) {
            func.instructions().drop();
        }
    }

    fn discard_r(&mut self, func: &mut Function, im: ImResult) {
        if let Ok(im) = im {
            self.discard(func, im);
        }
    }

    fn count_stack_slots(&mut self, im: &Intermediate) -> u32 {
        match im {
            Intermediate::Stack(ty) => self.count_type_stack_slots(ty),
        }
    }

    fn count_type_stack_slots(&mut self, ty: &Type) -> u32 {
        match ty {
            Type::Var(_) => todo!(),
            Type::Int => 1,
            Type::Bool => 1,
            Type::Unit => 0,
            Type::Function(_, _) => todo!(),
            Type::Tuple(items) => items.iter().map(|t| self.count_type_stack_slots(t)).sum(),
            Type::Record(record) => record
                .fields
                .iter()
                .map(|f| self.count_type_stack_slots(&f.ty))
                .sum(),
            Type::Enum(_variants) => todo!(),
        }
    }

    fn push_error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(CompileError {
            message: message.into(),
            span,
        });
        self.fatal = true;
    }

    /// Compile a single [Expr] into the current function, returning an
    /// [Intermediate] representing that expression's output on the stack.
    fn visit_expr<'i>(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        expr: &'i TypedExpr,
    ) -> ImResult<'i> {
        match &expr.kind {
            // Identifiers
            TypedExprKind::Identifier(ident) => {
                if let Some(local) = locals.get(&ident.name) {
                    for i in 0..self.count_type_stack_slots(&expr.ty) {
                        func.instructions().local_get(local + i);
                    }
                    Ok(Intermediate::Stack(&expr.ty))
                } else {
                    Err(self.push_error(
                        ident.span.unwrap_or(span),
                        format!("unknown name {:?}", &ident.name),
                    ))
                }
            }
            // Literals
            TypedExprKind::Literal(Literal::Integer(i)) => {
                func.instructions().i64_const(*i);
                Ok(Intermediate::Stack(&Type::Int))
            }
            TypedExprKind::Literal(Literal::Boolean(b)) => {
                func.instructions().i32_const(*b as i32);
                Ok(Intermediate::Stack(&Type::Bool))
            }
            TypedExprKind::Literal(Literal::Unit) => Ok(Intermediate::Stack(&Type::Unit)),
            // Arithmetic operators
            TypedExprKind::Binary {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_add();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Add({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Subtract,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_sub();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Subtract({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_mul();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Multiply({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Divide,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_div_s();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Divide({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Remainder,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_rem_s();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Remainder({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Negate,
                expr,
            } => {
                // `-x` compiles to `0 - x`.
                func.instructions().i64_const(0);
                match self.visit_expr(func, locals, expr.span, &expr.node)? {
                    Intermediate::Stack(&Type::Int) => {
                        func.instructions().i64_sub();
                        Ok(Intermediate::Stack(&Type::Int))
                    }
                    lhs => Err(self.todo(format!("Negate({lhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Not,
                expr,
            } => match self.visit_expr(func, locals, expr.span, &expr.node)? {
                Intermediate::Stack(&Type::Bool) => {
                    func.instructions().i32_eqz();
                    Ok(Intermediate::Stack(&Type::Bool))
                }
                lhs => Err(self.todo(format!("Not({lhs:?})"))),
            },
            // Comparison operators
            TypedExprKind::Binary {
                op: BinaryOp::Equal,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_eq();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_eq();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Equal({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::NotEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_ne();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_ne();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("NotEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Less,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_lt_s();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_lt_u();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Less({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Greater,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_gt_s();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_gt_u();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::LessEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_le_s();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_le_u();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("LessEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::GreaterEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr(func, locals, left.span, &left.node);
                let rhs = self.visit_expr(func, locals, right.span, &right.node);
                match (lhs?, rhs?) {
                    (Intermediate::Stack(&Type::Int), Intermediate::Stack(&Type::Int)) => {
                        func.instructions().i64_ge_s();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (Intermediate::Stack(&Type::Bool), Intermediate::Stack(&Type::Bool)) => {
                        func.instructions().i32_ge_u();
                        Ok(Intermediate::Stack(&Type::Bool))
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            // Short-circuiting operators
            TypedExprKind::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => match self.visit_expr(func, locals, left.span, &left.node)? {
                Intermediate::Stack(&Type::Bool) => {
                    func.instructions().if_(BlockType::Result(ValType::I32));
                    match self.visit_expr(func, locals, right.span, &right.node)? {
                        Intermediate::Stack(&Type::Bool) => {
                            func.instructions().else_().i32_const(0).end();
                            Ok(Intermediate::Stack(&Type::Bool))
                        }
                        right => Err(self.todo(format!("And({left:?}, {right:?})"))),
                    }
                }
                left => Err(self.todo(format!("And({left:?}, {right:?})"))),
            },
            TypedExprKind::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => match self.visit_expr(func, locals, left.span, &left.node)? {
                Intermediate::Stack(&Type::Bool) => {
                    func.instructions()
                        .if_(BlockType::Result(ValType::I32))
                        .i32_const(1)
                        .else_();
                    match self.visit_expr(func, locals, right.span, &right.node)? {
                        Intermediate::Stack(&Type::Bool) => {
                            func.instructions().end();
                            Ok(Intermediate::Stack(&Type::Bool))
                        }
                        right => Err(self.todo(format!("Or({left:?}, {right:?})"))),
                    }
                }
                left => Err(self.todo(format!("Or({left:?}, {right:?})"))),
            },
            // Field access
            TypedExprKind::FieldAccess { target, field } => {
                // Right now intermediates point to the stack only, which is
                // awkward when we want to access only one field of a local.
                // This implementation is inefficient but at least it's correct.
                let lhs = self.visit_expr(func, locals, target.span, &target.node);
                match lhs? {
                    Intermediate::Stack(Type::Record(record)) => {
                        let mut offset = 0;
                        let mut total = 0;
                        let mut ty = None;
                        for f in &record.fields {
                            if f.name.as_str() == field.as_str() {
                                ty = Some(&f.ty);
                            }
                            let slots = self.count_type_stack_slots(&f.ty);
                            total += slots;
                            if ty.is_none() {
                                offset += slots;
                            }
                        }
                        if let Some(ty) = ty {
                            let mut new_locals = Vec::new();
                            star_to_core_types(&mut new_locals, ty);

                            // Drop everything after what we are selecting.
                            for _ in offset + (new_locals.len() as u32)..total {
                                func.instructions().drop();
                            }

                            // If offset != 0, use locals to drop stuff before the offset.
                            if offset != 0
                                && let Some((&first, rest)) = new_locals.split_first()
                            {
                                // Allocate local slots.
                                let first_local = func.add_local(first);
                                for &each in rest {
                                    func.add_local(each);
                                }

                                // Store.
                                for i in (0..new_locals.len()).rev() {
                                    func.instructions().local_set(first_local + (i as u32));
                                }

                                // Drop.
                                for _ in 0..offset {
                                    func.instructions().drop();
                                }

                                // Get.
                                for i in 0..new_locals.len() {
                                    func.instructions().local_get(first_local + (i as u32));
                                }
                            }

                            Ok(Intermediate::Stack(ty))
                        } else {
                            Err(self.push_error(
                                field.span.unwrap_or(target.span),
                                format!(
                                    "no field {:?} on type {:?}",
                                    field.as_str(),
                                    target.node.ty
                                ),
                            ))
                        }
                    }
                    other => Err(self.push_error(
                        field.span.unwrap_or(target.span),
                        format!("field access is only valid on structs, not {:?}", other),
                    )),
                }
            }
            // Nesting
            TypedExprKind::Grouping(expr) => self.visit_expr(func, locals, expr.span, &expr.node),
            // Todo
            TypedExprKind::StructLiteral { .. }
            | TypedExprKind::EnumConstructor { .. }
            | TypedExprKind::Match { .. } => Err(self.todo(format!("{:?}", expr.kind))),
        }
    }

    fn todo(&mut self, why: String) {
        // TODO: better span
        self.push_error(Span::from(0..0), format!("TODO: {why}"));
    }
}

// Probably inefficient, but fun. Fix later?
trait Locals {
    fn get(&self, name: &str) -> Option<u32>;
}

impl Locals for () {
    fn get(&self, _: &str) -> Option<u32> {
        None
    }
}

impl Locals for (&dyn Locals, &HashMap<String, u32>) {
    fn get(&self, name: &str) -> Option<u32> {
        match self.1.get(name) {
            Some(v) => Some(*v),
            None => self.0.get(name),
        }
    }
}

fn star_to_core_types(dest: &mut Vec<ValType>, ty: &Type) -> bool {
    let mut ok = true;
    match ty {
        Type::Var(_) => ok = false,
        Type::Function(_, _) => ok = false,
        Type::Int => dest.push(ValType::I64),
        Type::Bool => dest.push(ValType::I32),
        Type::Unit => {}
        Type::Tuple(items) => {
            for each in items {
                ok = star_to_core_types(dest, each) && ok;
            }
        }
        Type::Record(record) => {
            for f in &record.fields {
                ok = star_to_core_types(dest, &f.ty) && ok;
            }
        }
        Type::Enum(_enum_variant_types) => ok = false,
    }
    ok
}

/// Typed intermediate value.
///
/// Represents the stack slots that evaluating an expression
/// produced, so that that expression's consumers can correctly consume or
/// discard those stack slots.
///
/// The Starstream-spec static type of an expression is determined earlier, by
/// the type checker, and stored separately.
#[derive(Debug, Clone)]
#[must_use]
enum Intermediate<'i> {
    /// An instance on the stack. Stack slots determined by ABI lowering.
    Stack(&'i Type),
    // /// An instance in locals. Local slots determined by ABI lowering.
    // Local { local: u32, ty: &'i Type },
}

type ImResult<'i> = std::result::Result<Intermediate<'i>, ()>;

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

fn to_kebab_case(name: &str) -> String {
    // ^A -> a
    // AB -> ab
    // AbC -> ab-c
    // Ab2 -> ab2
    let mut out = String::with_capacity(name.len());
    let mut prev: Option<char> = None;
    for ch in name.chars() {
        if ch.is_ascii_uppercase() {
            if let Some(p) = prev
                && p.is_ascii_lowercase()
            {
                out.push('-');
            }
            out.push(ch.to_ascii_lowercase());
        } else if ch.is_ascii_alphanumeric() {
            if ch.is_numeric() {
                // WIT doesn't permit segments to start with numbers.
                out.truncate(out.trim_end_matches('-').len());
                if out.is_empty() {
                    out.push('x');
                }
            }
            out.push(ch);
        } else if ch == '_' {
            if let Some(p) = prev
                && p != '_'
            {
                out.push('-');
            }
        }
        prev = Some(ch);
    }
    out.truncate(out.trim_end_matches('-').len());
    out
}
