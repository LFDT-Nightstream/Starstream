use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
};

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

/*
    Note on order of operations:

    - In binary operators, visit both the LHS and the RHS before `?`ing them.
    - When asserting or matching on a subexpression's type, do so after visiting it.

    This makes it possible to analyze the partial output of the compiler during
    debugging.
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

/// Token type to represent an error that's already been logged.
struct ErrorToken;

type Result<T> = std::result::Result<T, ErrorToken>;

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
    global_vars: HashMap<String, u32>,
    global_record_type: Vec<TypedStructField>,
}

impl Compiler {
    /// After [Compiler::visit_program], this function collates all the
    /// in-progress sections into an actual Wasm binary module.
    fn finish(mut self) -> (Option<Vec<u8>>, Vec<CompileError>) {
        // TODO: any other final activity on the sections here, such as
        // committing constants to the memory/data section.

        // Generate suspend/resume functions.
        self.generate_storage_exports();

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

    fn push_error(&mut self, span: Span, message: impl Into<String>) -> ErrorToken {
        self.errors.push(CompileError {
            message: message.into(),
            span,
        });
        self.fatal = true;
        ErrorToken
    }

    fn todo(&mut self, why: String) -> ErrorToken {
        // TODO: better span
        self.push_error(Span::from(0..0), format!("TODO: {why}"))
    }

    fn generate_storage_exports(&mut self) {
        let fields = std::mem::take(&mut self.global_record_type);
        if !fields.is_empty() {
            let storage_struct = Type::Record(RecordType {
                name: "Storage".into(),
                fields: fields
                    .iter()
                    .map(|f| RecordFieldType {
                        name: f.name.name.clone(),
                        ty: f.ty.clone(),
                    })
                    .collect(),
            });
            self.visit_struct(&TypedStructDef {
                name: Identifier::anon("Storage"),
                fields: fields.clone(),
                ty: storage_struct.clone(),
            });
            self.visit_function(&TypedFunctionDef {
                export: Some(FunctionExport::Script),
                name: Identifier::anon("get_storage"),
                params: Vec::new(),
                return_type: storage_struct.clone(),
                body: TypedBlock::from(Spanned::none(TypedExpr {
                    ty: storage_struct.clone(),
                    kind: TypedExprKind::StructLiteral {
                        name: Identifier::anon("Storage"),
                        fields: fields
                            .iter()
                            .map(|f| TypedStructLiteralField {
                                name: f.name.clone(),
                                value: Spanned::none(TypedExpr {
                                    ty: f.ty.clone(),
                                    kind: TypedExprKind::Identifier(f.name.clone()),
                                }),
                            })
                            .collect(),
                    },
                })),
            });
            self.visit_function(&TypedFunctionDef {
                export: Some(FunctionExport::Script),
                name: Identifier::anon("set_storage"),
                params: vec![TypedFunctionParam {
                    name: Identifier::anon("storage"),
                    ty: storage_struct.clone(),
                }],
                return_type: Type::Unit,
                body: TypedBlock::from(
                    fields
                        .iter()
                        .map(|f| TypedStatement::Assignment {
                            target: f.name.clone(),
                            value: Spanned::none(TypedExpr {
                                ty: f.ty.clone(),
                                kind: TypedExprKind::FieldAccess {
                                    target: Box::new(Spanned::none(TypedExpr {
                                        ty: storage_struct.clone(),
                                        kind: TypedExprKind::Identifier(Identifier::anon(
                                            "storage",
                                        )),
                                    })),
                                    field: f.name.clone(),
                                },
                            }),
                        })
                        .collect::<Vec<_>>(),
                ),
            });
        }
    }

    // ------------------------------------------------------------------------
    // Core table management

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

    fn add_globals(&mut self, types: impl IntoIterator<Item = ValType>) -> u32 {
        let idx = self.globals.len();
        for ty in types {
            self.globals.global(
                GlobalType {
                    val_type: ty,
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::i64_const(0),
            );
        }
        idx
    }

    // ------------------------------------------------------------------------
    // Component table management

    fn add_component_type(&mut self) -> (u32, ComponentTypeEncoder<'_>) {
        let idx = self.world_type.type_count();
        (idx, self.world_type.ty())
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

    fn export_component_fn(&mut self, function: &TypedFunctionDef) {
        let idx = self.add_component_func_type(function);
        self.world_type.export(
            &to_kebab_case(function.name.as_str()),
            ComponentTypeRef::Func(idx),
        );
    }

    // ------------------------------------------------------------------------
    // Type conversion

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

    fn star_to_core_types(&self, dest: &mut Vec<ValType>, ty: &Type) -> bool {
        let mut ok = true;
        match ty {
            Type::Unit => {}
            Type::Bool => dest.push(ValType::I32),
            Type::Int => dest.push(ValType::I64),
            Type::Tuple(items) => {
                for each in items {
                    ok = self.star_to_core_types(dest, each) && ok;
                }
            }
            Type::Record(record) => {
                for f in &record.fields {
                    ok = self.star_to_core_types(dest, &f.ty) && ok;
                }
            }
            Type::Enum(_enum_variant_types) => ok = false,
            Type::Function(_, _) => ok = false,
            Type::Var(_) => ok = false,
        }
        ok
    }

    fn star_count_core_types(&mut self, ty: &Type) -> u32 {
        match ty {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Int => 1,
            Type::Tuple(items) => items.iter().map(|t| self.star_count_core_types(t)).sum(),
            Type::Record(record) => record
                .fields
                .iter()
                .map(|f| self.star_count_core_types(&f.ty))
                .sum(),
            Type::Enum(_variants) => todo!(),
            Type::Function(_, _) => todo!(),
            Type::Var(_) => todo!(),
        }
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
                TypedDefinition::Utxo(utxo) => self.visit_utxo(utxo),
                TypedDefinition::Enum(_) => {
                    self.todo("enums are not supported in Wasm yet".into());
                }
            }
        }
    }

    fn visit_function(&mut self, function: &TypedFunctionDef) {
        let mut func = Function::default();

        let mut locals = HashMap::<String, u32>::new();
        let mut params = Vec::with_capacity(16);
        for p in &function.params {
            locals.insert(p.name.name.clone(), u32::try_from(params.len()).unwrap());
            if !self.star_to_core_types(&mut params, &p.ty) {
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
        if !self.star_to_core_types(&mut results, &function.return_type) {
            self.push_error(
                function.name.span.unwrap_or(Span::from(0..0)),
                format!(
                    "unknown lowering for return type {:?}",
                    function.return_type
                ),
            );
        }

        let _ = self.visit_block_stack(&mut func, &(&() as &dyn Locals, &locals), &function.body);
        func.instructions().end();

        let idx = self.add_function(FuncType::new(params, results), func);

        match function.export {
            Some(FunctionExport::Script) => {
                self.exports.export(
                    &to_kebab_case(function.name.as_str()),
                    ExportKind::Func,
                    idx,
                );
                self.export_component_fn(function);
            }
            None => {}
        }
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

    fn visit_utxo(&mut self, utxo: &TypedUtxoDef) {
        // TODO: use the utxo name to declare the type, etc.
        for part in &utxo.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    for var in vars {
                        let mut types = Vec::new();
                        self.star_to_core_types(&mut types, &var.ty);
                        let idx = self.add_globals(types.iter().copied());
                        // TODO: treat these identifiers as scoped only to this UTXO, rather than true globals
                        self.global_vars.insert(var.name.name.clone(), idx);
                        self.global_record_type.push(TypedStructField {
                            name: var.name.clone(),
                            ty: var.ty.clone(),
                        });
                    }
                }
            }
        }
    }

    /// Start a new identifier scope and generate bytecode for the statements
    /// of the block in sequence. Only creates a Wasm `block` when specifically
    /// needed for control flow reasons.
    fn visit_block_common(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<HashMap<String, u32>> {
        let mut locals = HashMap::new();
        for statement in &block.statements {
            match statement {
                TypedStatement::Expression(expr) => {
                    self.visit_expr_drop(func, &(parent, &locals), expr.span, &expr.node)?;
                }
                TypedStatement::VariableDeclaration {
                    mutable: _,
                    name,
                    value,
                } => {
                    // Allocate local space.
                    let mut local_types = Vec::new();
                    self.star_to_core_types(&mut local_types, &value.node.ty);
                    let local = func.add_locals(local_types.iter().copied());
                    locals.insert(name.name.clone(), local);

                    if self
                        .visit_expr_stack(func, &(parent, &locals), value.span, &value.node)
                        .is_ok()
                    {
                        // Pop from stack to set locals in reverse order.
                        for i in (0..local_types.len()).rev() {
                            func.instructions().local_set(local + (i as u32));
                        }
                    }
                }
                TypedStatement::Assignment { target, value } => {
                    if let Some(local) = (parent, &locals).get(&target.name) {
                        if self
                            .visit_expr_stack(func, &(parent, &locals), value.span, &value.node)
                            .is_ok()
                        {
                            let mut local_types = Vec::new();
                            self.star_to_core_types(&mut local_types, &value.node.ty);

                            // Pop from stack to set locals in reverse order.
                            for i in (0..local_types.len()).rev() {
                                func.instructions().local_set(local + (i as u32));
                            }
                        }
                    } else if let Some(global) = self.global_vars.get(&target.name).copied() {
                        if self
                            .visit_expr_stack(func, &(parent, &locals), value.span, &value.node)
                            .is_ok()
                        {
                            let mut types = Vec::new();
                            self.star_to_core_types(&mut types, &value.node.ty);

                            // Pop from stack to set locals in reverse order.
                            for i in (0..types.len()).rev() {
                                func.instructions().global_set(global + (i as u32));
                            }
                        }
                    } else {
                        self.push_error(
                            target.span.unwrap_or(value.span),
                            format!("unknown name {:?}", target.name),
                        );
                    }
                }
                // Recursive
                TypedStatement::While { condition, body } => {
                    func.instructions().block(BlockType::Empty); // br(1) is break
                    func.instructions().loop_(BlockType::Empty); // br(0) is continue

                    let _ = self.visit_expr_stack(
                        func,
                        &(parent, &locals),
                        condition.span,
                        &condition.node,
                    );
                    assert_eq!(condition.node.ty, Type::Bool);
                    // if condition == 0, break
                    func.instructions().i32_eqz();
                    func.instructions().br_if(1);
                    // contents
                    self.visit_block_drop(func, &(parent, &locals), body)?;
                    // continue
                    func.instructions().br(0).end().end();
                }
                TypedStatement::Return(Some(expr)) => {
                    let _ = self.visit_expr_stack(func, &(parent, &locals), expr.span, &expr.node);
                    func.instructions().return_();
                }
                TypedStatement::Return(None) => {
                    func.instructions().return_();
                }
            }
        }
        Ok(locals)
    }

    /// Visit a block, dropping its result.
    fn visit_block_drop(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<()> {
        let locals = self.visit_block_common(func, parent, block)?;
        if let Some(expr) = &block.tail_expression {
            self.visit_expr_drop(func, &(parent, &locals), expr.span, &expr.node)
        } else {
            Ok(())
        }
    }

    /// Visit a block, storing its result to the stack.
    fn visit_block_stack(
        &mut self,
        func: &mut Function,
        parent: &dyn Locals,
        block: &TypedBlock,
    ) -> Result<()> {
        let locals = self.visit_block_common(func, parent, block)?;
        if let Some(expr) = &block.tail_expression {
            self.visit_expr_stack(func, &(parent, &locals), expr.span, &expr.node)
        } else {
            Ok(())
        }
    }

    /// Visit an expression, dropping its result.
    fn visit_expr_drop(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        _span: Span,
        expr: &TypedExpr,
    ) -> Result<()> {
        // TODO: Warn on expressions that have no effect.
        match &expr.kind {
            TypedExprKind::Literal(_) => {}
            TypedExprKind::Identifier(_) => {}
            TypedExprKind::Unary { op: _, expr } => {
                self.visit_expr_drop(func, locals, expr.span, &expr.node)?;
            }
            // _ = lhs && rhs --> if (lhs) { _ = rhs; }
            TypedExprKind::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().if_(BlockType::Empty);
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
            }
            // _ = lhs || rhs --> if (!lhs) { _ = rhs; }
            TypedExprKind::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().i32_eqz();
                func.instructions().if_(BlockType::Empty);
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
            }
            // Other binary operators have no control flow or side effects.
            TypedExprKind::Binary { op: _, left, right } => {
                self.visit_expr_drop(func, locals, left.span, &left.node)?;
                self.visit_expr_drop(func, locals, right.span, &right.node)?;
            }
            TypedExprKind::Grouping(spanned) => {
                self.visit_expr_drop(func, locals, spanned.span, &spanned.node)?;
            }
            TypedExprKind::StructLiteral { name: _, fields } => {
                for field in fields {
                    self.visit_expr_drop(func, locals, field.value.span, &field.value.node)?;
                }
            }
            TypedExprKind::FieldAccess { target, field: _ } => {
                self.visit_expr_drop(func, locals, target.span, &target.node)?;
            }
            TypedExprKind::EnumConstructor {
                enum_name: _,
                variant: _,
                payload,
            } => match payload {
                TypedEnumConstructorPayload::Unit => {}
                TypedEnumConstructorPayload::Tuple(fields) => {
                    for field in fields {
                        self.visit_expr_drop(func, locals, field.span, &field.node)?;
                    }
                }
                TypedEnumConstructorPayload::Struct(fields) => {
                    for field in fields {
                        self.visit_expr_drop(func, locals, field.value.span, &field.value.node)?;
                    }
                }
            },
            TypedExprKind::Block(block) => self.visit_block_drop(func, locals, block)?,
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                // Emit basic double-block and trust the optimizer.
                func.instructions().block(BlockType::Empty);
                for (condition, block) in branches {
                    // Inner block for each condition.
                    func.instructions().block(BlockType::Empty);
                    let _ = self.visit_expr_stack(func, locals, condition.span, &condition.node);
                    assert_eq!(condition.node.ty, Type::Bool);
                    func.instructions().i32_eqz(); // If condition is false,
                    func.instructions().br_if(0); // then try the next condition.
                    self.visit_block_drop(func, locals, block)?;
                    func.instructions().br(1); // Go past end.
                    func.instructions().end();
                }
                // Final `else` branch is just inline.
                if let Some(else_branch) = else_branch {
                    self.visit_block_drop(func, locals, else_branch)?;
                }
                // End.
                func.instructions().end();
            }
            // Todo
            TypedExprKind::Match { .. } | TypedExprKind::Call { .. } => {
                return Err(self.todo(format!("{:?}", expr.kind)));
            }
        }
        Ok(())
    }

    /// Compile a single [Expr] into the current function, returning an
    /// [Intermediate] representing that expression's output on the stack.
    fn visit_expr_stack(
        &mut self,
        func: &mut Function,
        locals: &dyn Locals,
        span: Span,
        expr: &TypedExpr,
    ) -> Result<()> {
        match &expr.kind {
            // Identifiers
            TypedExprKind::Identifier(ident) => {
                if let Some(local) = locals.get(&ident.name) {
                    for i in 0..self.star_count_core_types(&expr.ty) {
                        func.instructions().local_get(local + i);
                    }
                    Ok(())
                } else if let Some(global) = self.global_vars.get(&ident.name).copied() {
                    for i in 0..self.star_count_core_types(&expr.ty) {
                        func.instructions().global_get(global + i);
                    }
                    Ok(())
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
                assert_eq!(expr.ty, Type::Int);
                Ok(())
            }
            TypedExprKind::Literal(Literal::Boolean(b)) => {
                func.instructions().i32_const(*b as i32);
                assert_eq!(expr.ty, Type::Bool);
                Ok(())
            }
            TypedExprKind::Literal(Literal::Unit) => {
                assert_eq!(expr.ty, Type::Unit);
                Ok(())
            }
            // Arithmetic operators
            TypedExprKind::Binary {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_add();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Add({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Subtract,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_sub();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Subtract({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Multiply,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_mul();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Multiply({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Divide,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_div_s();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Divide({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Remainder,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_rem_s();
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Remainder({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Negate,
                expr: inner,
            } => {
                // `-x` compiles to `0 - x`.
                func.instructions().i64_const(0);
                self.visit_expr_stack(func, locals, inner.span, &inner.node)?;
                match &inner.node.ty {
                    Type::Int => {
                        func.instructions().i64_sub();
                        assert_eq!(expr.ty, Type::Int);
                        Ok(())
                    }
                    lhs => Err(self.todo(format!("Negate({lhs:?})"))),
                }
            }
            TypedExprKind::Unary {
                op: UnaryOp::Not,
                expr: inner,
            } => {
                self.visit_expr_stack(func, locals, inner.span, &inner.node)?;
                match &inner.node.ty {
                    Type::Bool => {
                        func.instructions().i32_eqz();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    lhs => Err(self.todo(format!("Not({lhs:?})"))),
                }
            }
            // Comparison operators
            TypedExprKind::Binary {
                op: BinaryOp::Equal,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_eq();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_eq();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Equal({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::NotEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_ne();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_ne();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("NotEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Less,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_lt_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_lt_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Less({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::Greater,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_gt_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_gt_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::LessEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_le_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_le_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("LessEqual({lhs:?}, {rhs:?})"))),
                }
            }
            TypedExprKind::Binary {
                op: BinaryOp::GreaterEqual,
                left,
                right,
            } => {
                let lhs = self.visit_expr_stack(func, locals, left.span, &left.node);
                let rhs = self.visit_expr_stack(func, locals, right.span, &right.node);
                lhs?;
                rhs?;
                match (&left.node.ty, &right.node.ty) {
                    (Type::Int, Type::Int) => {
                        func.instructions().i64_ge_s();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (Type::Bool, Type::Bool) => {
                        func.instructions().i32_ge_u();
                        assert_eq!(expr.ty, Type::Bool);
                        Ok(())
                    }
                    (lhs, rhs) => Err(self.todo(format!("Greater({lhs:?}, {rhs:?})"))),
                }
            }
            // Short-circuiting operators
            TypedExprKind::Binary {
                op: BinaryOp::And,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions().if_(BlockType::Result(ValType::I32));
                self.visit_expr_stack(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().else_().i32_const(0).end();
                Ok(())
            }
            TypedExprKind::Binary {
                op: BinaryOp::Or,
                left,
                right,
            } => {
                self.visit_expr_stack(func, locals, left.span, &left.node)?;
                assert_eq!(left.node.ty, Type::Bool);
                func.instructions()
                    .if_(BlockType::Result(ValType::I32))
                    .i32_const(1)
                    .else_();
                self.visit_expr_stack(func, locals, right.span, &right.node)?;
                assert_eq!(right.node.ty, Type::Bool);
                func.instructions().end();
                Ok(())
            }
            // Field access
            TypedExprKind::FieldAccess { target, field } => {
                // Right now intermediates point to the stack only, which is
                // awkward when we want to access only one field of a local.
                // This implementation is inefficient but at least it's correct.
                self.visit_expr_stack(func, locals, target.span, &target.node)?;
                match &target.node.ty {
                    Type::Record(record) => {
                        let mut offset = 0;
                        let mut total = 0;
                        let mut ty = None;
                        for f in &record.fields {
                            if f.name.as_str() == field.as_str() {
                                ty = Some(&f.ty);
                            }
                            let slots = self.star_count_core_types(&f.ty);
                            total += slots;
                            if ty.is_none() {
                                offset += slots;
                            }
                        }
                        if let Some(ty) = ty {
                            let mut new_locals = Vec::new();
                            self.star_to_core_types(&mut new_locals, ty);

                            // Drop everything after what we are selecting.
                            for _ in offset + (new_locals.len() as u32)..total {
                                func.instructions().drop();
                            }

                            // If offset != 0, use locals to drop stuff before the offset.
                            if offset != 0 && !new_locals.is_empty() {
                                // Allocate local slots.
                                let first_local = func.add_locals(new_locals.iter().copied());

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

                            Ok(())
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
            TypedExprKind::Grouping(expr) => {
                self.visit_expr_stack(func, locals, expr.span, &expr.node)
            }
            TypedExprKind::Block(block) => self.visit_block_stack(func, locals, block),
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                // Create locals to store expression result.
                let mut new_locals = Vec::new();
                assert!(self.star_to_core_types(&mut new_locals, &expr.ty));
                let first_local = func.add_locals(new_locals.iter().copied());

                // Emit basic double-block and trust the optimizer.
                func.instructions().block(BlockType::Empty);
                for (condition, block) in branches {
                    // Inner block for each condition.
                    func.instructions().block(BlockType::Empty);
                    self.visit_expr_stack(func, locals, condition.span, &condition.node)?;
                    assert_eq!(condition.node.ty, Type::Bool);
                    func.instructions().i32_eqz(); // If condition is false,
                    func.instructions().br_if(0); // then try the next condition.
                    self.visit_block_stack(func, locals, block)?;
                    for i in (0..new_locals.len()).rev() {
                        func.instructions().local_set(first_local + (i as u32));
                    }
                    func.instructions().br(1); // Go past end.
                    func.instructions().end();
                }
                // Final `else` branch is just inline.
                if let Some(else_branch) = else_branch {
                    self.visit_block_stack(func, locals, else_branch)?;
                    for i in (0..new_locals.len()).rev() {
                        func.instructions().local_set(first_local + (i as u32));
                    }
                }
                // End.
                func.instructions().end();

                // Read locals back onto stack.
                for i in 0..new_locals.len() {
                    func.instructions().local_get(first_local + (i as u32));
                }
                Ok(())
            }
            // Todo
            TypedExprKind::StructLiteral { name: _, fields } => {
                let Type::Record(record) = &expr.ty else {
                    panic!("StructLiteral type must be a Record");
                };
                let fields = fields
                    .iter()
                    .map(|f| (f.name.as_str(), &f.value))
                    .collect::<BTreeMap<_, _>>();
                for field in &record.fields {
                    let expr = fields
                        .get(field.name.as_str())
                        .expect("StructLiteral missing field");
                    self.visit_expr_stack(func, locals, expr.span, &expr.node)?;
                }
                Ok(())
            }
            TypedExprKind::EnumConstructor { .. }
            | TypedExprKind::Match { .. }
            | TypedExprKind::Call { .. } => Err(self.todo(format!("{:?}", expr.kind))),
        }
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

    fn add_locals(&mut self, types: impl IntoIterator<Item = ValType>) -> u32 {
        let id = self.num_locals;
        for ty in types {
            self.num_locals += 1;
            if let Some((last_count, last_type)) = self.locals.last_mut()
                && ty == *last_type
            {
                *last_count += 1;
                return id;
            }
            self.locals.push((1, ty));
        }
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
        } else if ch == '_'
            && let Some(p) = prev
            && p != '_'
        {
            out.push('-');
        }
        prev = Some(ch);
    }
    out.truncate(out.trim_end_matches('-').len());
    out
}
