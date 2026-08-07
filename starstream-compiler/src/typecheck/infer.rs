#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use starstream_types::{
    AbiDef, AbiPart, AbiType, Arguments, DUMMY_SPAN, EffectDef, EventDef, FunctionExport,
    FunctionKind, FunctionType, GenericTypeDef, IfCondition, NameId, Scheme, ScopedName, Span,
    Spanned, StaticFunction, TokenDef, TokenGlobal, TokenPart, TokenType, Type, TypeParam,
    TypeVarId, TypedAbiMethodDecl, TypedImportItem, TypedTokenDef, TypedTokenGlobal,
    TypedTokenPart, TypedUtxoDef, TypedUtxoGlobal, TypedUtxoPart, UtxoDef, UtxoGlobal, UtxoPart,
    UtxoType,
    ast::{
        BinaryOp, Block, Definition, EnumDef, EnumVariantPayload, Expr, FunctionDef, Identifier,
        ImportItems, ImportSource, IntegerLiteral, Literal, Pattern, Program, Statement, StructDef,
        TypeAnnotation, UnaryOp,
    },
    typed_ast::{
        TypedAbiDef, TypedBlock, TypedDefinition, TypedEnumDef, TypedExpr, TypedExprKind,
        TypedFunctionDef, TypedFunctionParam, TypedIfCondition, TypedImportDef, TypedMatchArm,
        TypedPattern, TypedProgram, TypedStatement, TypedStructDef, TypedStructFieldInitializer,
        TypedStructPatternField,
    },
    types::{EnumType, EnumVariantKind, EnumVariantType, RecordFieldType, RecordType},
};

use super::{
    builtins::BuiltinRegistry,
    env::*,
    errors::{ConditionContext, EnumPayloadKind, TypeError, TypeErrorKind},
    tree::InferenceTree,
    warnings::{TypeWarning, TypeWarningKind},
};
use crate::{ModuleId, formatter, pointer_map::PointerMap};

/// Optional settings that control type-checker behavior.
#[derive(Clone, Debug, Default)]
pub struct TypecheckOptions {
    pub capture_traces: bool,
}

/// Successful type-checking result holding the typed AST and any inference traces.
#[derive(Clone, Debug)]
pub struct TypecheckSuccess {
    pub program: TypedProgram,
    pub traces: Vec<InferenceTree>,
    pub generic_types: HashMap<String, GenericTypeDef>,
    pub warnings: Vec<TypeWarning>,
}

impl TypecheckSuccess {
    pub fn display_traces(&self) -> impl Display {
        DisplayTraces(&self.traces)
    }
}

struct DisplayTraces<'a>(&'a [InferenceTree]);

impl<'a> Display for DisplayTraces<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, tree) in self.0.iter().enumerate() {
            if index > 0 {
                f.write_str("\n")?;
            }
            tree.fmt(f)?;
        }
        Ok(())
    }
}

/// Failed type-checking result preserving both errors and any warnings collected
/// prior to the failure.
#[derive(Debug)]
pub struct TypecheckFailure {
    pub errors: Vec<TypeError>,
    pub warnings: Vec<TypeWarning>,
}

/// Run Hindley–Milner style inference over the parsed program and return the
/// typed AST along with optional tracing information.
pub fn typecheck_program(
    program: &Program,
    options: TypecheckOptions,
) -> Result<TypecheckSuccess, TypecheckFailure> {
    let mut inferencer = Inferencer::new(options.capture_traces);
    let mut env = TypeEnv::new();

    // Pass 1: register imports
    env.root
        .import_all_from(&inferencer.builtins.prelude())
        .unwrap();
    if let Err(error) =
        inferencer.register_imports(&mut env, &program.definitions, &Default::default())
    {
        return Err(TypecheckFailure {
            errors: vec![error],
            warnings: inferencer.warnings,
        });
    }

    // Pass 2: process definitions
    let (mut program, traces) = match inferencer.process_definitions(&mut env, &program.definitions)
    {
        Ok(x) => x,
        Err(errors) => {
            return Err(TypecheckFailure {
                errors,
                warnings: inferencer.warnings,
            });
        }
    };

    // Default any unresolved integer type variables to i64.
    inferencer.default_int_vars();

    // Check range validity of integer literals against their resolved types.
    if let Err(errors) = inferencer.check_int_literal_ranges() {
        return Err(TypecheckFailure {
            errors,
            warnings: inferencer.warnings,
        });
    }

    inferencer.apply_substitutions_program(&mut program);

    let traces = if options.capture_traces {
        traces
    } else {
        Vec::new()
    };

    let generic_types = Inferencer::build_generic_type_defs(&inferencer.builtins.prelude());
    let warnings = inferencer.warnings;

    Ok(TypecheckSuccess {
        program,
        traces,
        generic_types,
        warnings,
    })
}

/// One typechecked module within a `TypedModuleGraph`.
#[derive(Clone, Debug)]
pub struct TypedModule {
    pub id: ModuleId,
    pub abs_path: std::path::PathBuf,
    pub source: std::sync::Arc<str>,
    pub program: TypedProgram,
    pub edges: Vec<ModuleId>,
}

/// Typechecked counterpart to `ModuleGraph`. Modules are listed in `id`
/// order (matching the source graph), with `topo_order` giving the iteration
/// order callers should use for downstream passes.
#[derive(Clone, Debug)]
pub struct TypedModuleGraph {
    pub modules: Vec<TypedModule>,
    pub topo_order: Vec<ModuleId>,
    /// Module ids that declare `contract;` — i.e. codegen entries.
    pub contract_entries: Vec<ModuleId>,
    pub generic_types: HashMap<String, GenericTypeDef>,
    /// Warnings collected from every module (e.g. unnecessary disclose,
    /// path-import-in-single-file). Carried even on success so callers can
    /// decide whether to render them.
    pub warnings: Vec<(ModuleId, TypeWarning)>,
}

impl TypedModuleGraph {
    pub fn module(&self, id: ModuleId) -> &TypedModule {
        &self.modules[id.index()]
    }
}

/// Failure returned by `typecheck_modules`. Each error/warning carries the
/// module id it originated in so callers can render diagnostics against the
/// right source.
#[derive(Debug)]
pub struct TypecheckModulesFailure {
    pub errors: Vec<(ModuleId, TypeError)>,
    pub warnings: Vec<(ModuleId, TypeWarning)>,
}

/// Run inference across an entire module graph in topological order.
///
/// Path imports are resolved here: their names get inserted into the importing
/// module's environment.
pub fn typecheck_modules(
    graph: &crate::ModuleGraph,
    options: TypecheckOptions,
) -> Result<TypedModuleGraph, TypecheckModulesFailure> {
    let mut inferencer = Inferencer::new(options.capture_traces);

    // `module_exports[id]` is populated as we finish typechecking each module.
    let mut module_exports: HashMap<ModuleId, Namespace> = HashMap::new();
    let mut typed_modules: HashMap<ModuleId, TypedProgram> = HashMap::new();

    let mut all_errors: Vec<(ModuleId, TypeError)> = Vec::new();
    let mut warnings: Vec<(ModuleId, TypeWarning)> = Vec::new();

    for &module_id in graph.topo_order() {
        let module = graph.module(module_id);
        let mut env = TypeEnv::new();

        // Pass 1: register imports
        env.root
            .import_all_from(&inferencer.builtins.prelude())
            .unwrap();
        let resolved_imports = resolve_path_imports(graph, module_id, &module_exports);
        if let Err(error) =
            inferencer.register_imports(&mut env, &module.program.definitions, &resolved_imports)
        {
            all_errors.push((module_id, error));
            continue;
        }

        // Pass 2: process definitions
        let (program, _) =
            match inferencer.process_definitions(&mut env, &module.program.definitions) {
                Ok(x) => x,
                Err(errors) => {
                    // Capture a (possibly partial) export table so other modules can
                    // continue — they may still produce useful diagnostics. But we
                    // flag the run as failed.
                    all_errors.extend(errors.into_iter().map(|e| (module_id, e)));
                    module_exports.insert(module_id, Namespace::default());
                    continue;
                }
            };

        // Capture this module's exports for downstream modules.
        // TODO: exclude private items.
        let exports = env.root;
        module_exports.insert(module_id, exports);

        typed_modules.insert(module_id, program);

        // Drain warnings emitted during this module's pass.
        for warning in inferencer.warnings.drain(..) {
            warnings.push((module_id, warning));
        }

        // Stop once any module has failed catastrophically.
        if !all_errors.is_empty() {
            break;
        }
    }

    if !all_errors.is_empty() {
        return Err(TypecheckModulesFailure {
            errors: all_errors,
            warnings,
        });
    }

    inferencer.default_int_vars();
    if let Err(errors) = inferencer.check_int_literal_ranges() {
        // `check_int_literal_ranges` doesn't know which module each literal
        // came from. Best effort: attribute to the first contract entry, or
        // (if no contract entries exist) the first module.
        let fallback = graph
            .contract_entries()
            .first()
            .copied()
            .unwrap_or_else(|| {
                graph
                    .modules()
                    .first()
                    .map(|m| m.id)
                    .expect("graph must have at least one module")
            });
        return Err(TypecheckModulesFailure {
            errors: errors.into_iter().map(|e| (fallback, e)).collect(),
            warnings,
        });
    }

    // Apply substitutions per module.
    for typed_program in typed_modules.values_mut() {
        inferencer.apply_substitutions_program(typed_program);
    }

    let generic_types = Inferencer::build_generic_type_defs(&inferencer.builtins.prelude());

    let mut modules: Vec<TypedModule> = Vec::with_capacity(graph.modules().len());
    for source_module in graph.modules() {
        let typed_program = typed_modules.remove(&source_module.id).unwrap_or_default();
        modules.push(TypedModule {
            id: source_module.id,
            abs_path: source_module.abs_path.clone(),
            source: source_module.source.clone(),
            program: typed_program,
            edges: graph
                .edges_of(source_module.id)
                .iter()
                .map(|e| e.target)
                .collect(),
        });
    }

    Ok(TypedModuleGraph {
        modules,
        topo_order: graph.topo_order().to_vec(),
        contract_entries: graph.contract_entries().to_vec(),
        generic_types,
        warnings,
    })
}

/// Resolution work for the `Definition::Import { from: Path(...) }` items in a
/// single module: produces the typed import nodes (indexed by their position
/// in the module's `definitions`) and inserts the imported names into the
/// importer's `env` / `namespaces` so subsequent inference can resolve them.
fn resolve_path_imports<'g>(
    graph: &crate::ModuleGraph,
    importer: ModuleId,
    module_exports: &'g HashMap<ModuleId, Namespace>,
) -> HashMap<usize, &'g Namespace> {
    let mut result: HashMap<usize, &'g Namespace> = HashMap::new();

    for edge in graph.edges_of(importer) {
        let target_id = edge.target;
        let Some(target_exports) = module_exports.get(&target_id) else {
            // Topological order should ensure deps are processed first.
            unreachable!();
        };
        result.insert(edge.def_index, target_exports);
    }

    result
}

/// Internal stateful helper that owns the substitution map and generates fresh
/// type variables while walking the AST.
struct Inferencer {
    // Options
    capture_traces: bool,

    // Outputs
    warnings: Vec<TypeWarning>,

    // Type var tracking
    next_type_var: TypeVarId,
    subst: HashMap<TypeVarId, Type>,
    /// Type variables constrained to integer types (from polymorphic integer literals).
    int_vars: HashSet<TypeVarId>,
    /// Tracks the literal value associated with each integer type variable for range checking.
    int_literal_values: HashMap<TypeVarId, (IntegerLiteral, Span)>,

    /// Builtin package registry for `import`s, and the prelude.
    builtins: BuiltinRegistry,

    /// Name ID assignment.
    next_name_id: NameId,
    function_names: PointerMap<NameId>,
    typed_definitions: PointerMap<TypedDefinition>,

    /// Stack of linearity trackers for `if x is Abi` blocks (supports nesting).
    abi_call_trackers: Vec<AbiCallTracker>,
}

/// Tracks linearity of method calls on a narrowed ABI variable.
struct AbiCallTracker {
    var_name: String,
    abi_name: String,
    first_call_span: Option<Span>,
}

struct FunctionCtx {
    expected_return: Type,
    return_span: Span,
    saw_return: bool,
    /// Declaration spans for function parameters that are private (non-`pub`).
    private_param_decl_spans: Vec<Span>,
    is_coroutine: bool,
}

impl Inferencer {
    /// Construct a fresh inferencer with an empty substitution environment.
    fn new(capture_traces: bool) -> Self {
        let (builtins, next_type_var, next_name_id) = BuiltinRegistry::new();
        Inferencer {
            capture_traces,
            next_type_var,
            subst: Default::default(),
            int_vars: Default::default(),
            int_literal_values: Default::default(),
            builtins,
            next_name_id,
            function_names: Default::default(),
            typed_definitions: Default::default(),
            warnings: Default::default(),
            abi_call_trackers: Default::default(),
        }
    }

    fn build_generic_type_defs(ns: &Namespace) -> HashMap<String, GenericTypeDef> {
        ns.types
            .iter()
            .filter(|(_, entry)| !entry.type_params.is_empty() || entry.doc.is_some())
            .map(|(name, entry)| {
                let mut ty = entry.ty.clone();
                // Set type_args to the type param vars so display renders them
                if let Type::Enum(ref mut e) = ty {
                    Arc::make_mut(e).type_args =
                        entry.type_params.iter().map(|p| Type::Var(p.id)).collect();
                }
                (
                    name.clone(),
                    GenericTypeDef {
                        ty,
                        type_params: entry.type_params.clone(),
                        doc: entry.doc.clone(),
                        variant_docs: entry.variant_docs.clone(),
                    },
                )
            })
            .collect()
    }

    // ------------------------------------------------------------------------
    // First pass: register imports

    fn register_imports(
        &mut self,
        env: &mut TypeEnv,
        definitions: &[Spanned<Definition>],
        resolved: &HashMap<usize, &Namespace>,
    ) -> Result<(), TypeError> {
        for (i, def) in definitions.iter().enumerate() {
            if let Definition::Import(import) = &def.node {
                // Resolve the namespace we're importing from.
                let source = &import.from.to_string();
                let namespace = match (resolved.get(&i), &import.from) {
                    (Some(&ns), _) => {
                        // Preresolved import. Common case for path imports which come
                        // from the module graph.
                        ns
                    }
                    (
                        None,
                        ImportSource::Wit {
                            namespace,
                            package,
                            interface,
                        },
                    ) => {
                        // WIT import.
                        self.builtins
                            .get_as_namespace(namespace, package, interface.as_ref())?
                    }
                    (None, ImportSource::Path(path)) => {
                        // Non-preresolved path import, usually leftover from
                        // the single-file so emit a warning explaining why
                        // the names aren't available.
                        self.warnings.push(TypeWarning::new(
                            TypeWarningKind::PathImportIgnoredInSingleFile {
                                path: path.value.clone(),
                            },
                            path.span,
                        ));
                        continue;
                    }
                };

                // Actually import the items.
                let mut typed_items: Vec<TypedImportItem>;
                match &import.items {
                    ImportItems::Named(items) => {
                        typed_items = Vec::with_capacity(items.len());
                        for item in items {
                            env.root.import_name_from(
                                &item.local,
                                namespace,
                                item.imported.as_str(),
                                source,
                            )?;

                            if let Some(ci) = namespace.constants.get(item.imported.as_str()) {
                                typed_items.push(TypedImportItem {
                                    imported: item.imported.clone(),
                                    local: item.local.clone(),
                                    ty: ci.ty.clone(),
                                });
                            }
                        }
                    }
                    ImportItems::Namespace(name) => {
                        env.root.import_as_namespace(name, namespace)?;

                        typed_items = namespace
                            .constants
                            .iter()
                            .map(|(k, ci)| TypedImportItem {
                                imported: Identifier::new(k.clone(), name.span),
                                local: Identifier::new(k.clone(), name.span),
                                ty: ci.ty.clone(),
                            })
                            .collect();
                    }
                }

                // Memorize the [TypedImportItem]s for later.
                self.typed_definitions.insert(
                    &def.node,
                    TypedDefinition::Import(TypedImportDef {
                        items: typed_items,
                        from: import.from.clone(),
                    }),
                );
            }
        }
        Ok(())
    }

    // ------------------------------------------------------------------------
    // Second pass: register definition names, mainly type names

    fn process_definitions(
        &mut self,
        env: &mut TypeEnv,
        definitions: &[Spanned<Definition>],
    ) -> Result<(TypedProgram, Vec<InferenceTree>), Vec<TypeError>> {
        let mut typed_definitions = Vec::with_capacity(definitions.len());
        let mut traces = Vec::with_capacity(definitions.len());
        let mut errors = Vec::new();

        // TODO: Allow types to refer to later types by splitting this into three passes:
        // 1. Preregister everything by name as a type var.
        // 2. Register struct, enum, and ABI definitions (they only need each other's names).
        // 3. Register function, utxo, and token definitions (they need to know ABI innards).

        // Register definitions.
        for definition in definitions {
            match &definition.node {
                Definition::Contract => {}
                Definition::Import(_) => {}
                Definition::Struct(def) => match self.register_struct(env, def) {
                    Ok(def2) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Struct(def2));
                    }
                    Err(e) => errors.push(e),
                },
                Definition::Enum(def) => match self.register_enum(env, def) {
                    Ok(def2) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Enum(def2));
                    }
                    Err(e) => errors.push(e),
                },
                Definition::Abi(def) => match self.register_abi(env, def) {
                    Ok(def2) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Abi(def2));
                    }
                    Err(e) => errors.push(e),
                },
                Definition::Utxo(def) => errors.extend(self.register_utxo(env, def).err()),
                Definition::Token(def) => errors.extend(self.register_token(env, def).err()),
                Definition::Function(def) => errors.extend(self.register_function(env, def).err()),
            }
        }

        // Typecheck function bodies.
        for definition in definitions {
            match &definition.node {
                Definition::Contract => {}
                Definition::Import(_) => {}
                Definition::Struct(_) => {}
                Definition::Enum(_) => {}
                Definition::Abi(_) => {}
                Definition::Utxo(def) => match self.infer_utxo(env, def) {
                    Ok((def2, trace)) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Utxo(def2));
                        traces.push(trace);
                    }
                    Err(e) => errors.push(e),
                },
                Definition::Token(def) => match self.infer_token(env, def) {
                    Ok((def2, trace)) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Token(def2));
                        traces.push(trace);
                    }
                    Err(e) => errors.push(e),
                },
                Definition::Function(def) => match self.infer_function(env, def) {
                    Ok((def2, _ty, trace)) => {
                        self.typed_definitions
                            .insert(&definition.node, TypedDefinition::Function(def2));
                        traces.push(trace);
                    }
                    Err(e) => errors.push(e),
                },
            }
        }

        // Collect typed AST.
        for definition in definitions {
            typed_definitions.extend(self.typed_definitions.remove(&definition.node));
        }

        if errors.is_empty() {
            Ok((
                TypedProgram {
                    definitions: typed_definitions,
                },
                traces,
            ))
        } else {
            Err(errors)
        }
    }

    fn register_struct(
        &mut self,
        env: &mut TypeEnv,
        def: &StructDef,
    ) -> Result<TypedStructDef, TypeError> {
        let mut seen = HashMap::new();
        let mut fields = Vec::with_capacity(def.fields.len());
        for field in &def.fields {
            if let Some(previous_span) = seen.get(&field.name.name) {
                return Err(TypeError::new(
                    TypeErrorKind::DuplicateStructField {
                        struct_name: def.name.name.clone(),
                        field_name: field.name.name.clone(),
                    },
                    field.name.span(),
                )
                .with_primary_message("duplicate")
                .with_secondary(*previous_span, "first defined here"));
            }
            seen.insert(field.name.name.clone(), field.name.span());
            let ty = self.type_from_annotation(env, &field.ty)?;
            fields.push(RecordFieldType {
                name: field.name.clone(),
                ty,
            });
        }
        let record_ty = Arc::new(RecordType {
            name: def.name.clone(),
            fields,
        });
        let ty = Type::from(record_ty.clone());
        env.root.insert_type(
            &def.name,
            TypeEntry {
                ty: ty.clone(),
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        )?;
        env.root.insert_struct_constructor(
            &def.name,
            StructConstructor {
                span: def.name.span,
                ty,
                type_params: vec![],
                enum_variant: 0,
            },
        )?;
        Ok(TypedStructDef { ty: record_ty })
    }

    fn register_enum(
        &mut self,
        env: &mut TypeEnv,
        def: &EnumDef,
    ) -> Result<TypedEnumDef, TypeError> {
        let mut seen = HashMap::new();
        let mut variants = Vec::with_capacity(def.variants.len());
        for variant in &def.variants {
            if let Some(previous_span) = seen.get(&variant.name.name) {
                return Err(TypeError::new(
                    TypeErrorKind::DuplicateEnumVariant {
                        enum_name: def.name.name.clone(),
                        variant_name: variant.name.name.clone(),
                    },
                    variant.name.span(),
                )
                .with_primary_message("duplicate")
                .with_secondary(*previous_span, "first defined here"));
            }
            seen.insert(variant.name.name.clone(), variant.name.span());
            let kind = match &variant.payload {
                EnumVariantPayload::Unit => EnumVariantKind::Unit,
                EnumVariantPayload::Tuple(items) => {
                    let mut payload = Vec::with_capacity(items.len());
                    for ty in items {
                        payload.push(self.type_from_annotation(env, ty)?);
                    }
                    EnumVariantKind::Tuple(payload)
                }
                EnumVariantPayload::Struct(fields) => {
                    let mut seen_fields = HashMap::new();
                    let mut payload = Vec::with_capacity(fields.len());
                    for field in fields {
                        if let Some(previous_span) = seen_fields.get(&field.name.name) {
                            return Err(TypeError::new(
                                TypeErrorKind::DuplicateStructField {
                                    struct_name: format!(
                                        "{}::{}",
                                        def.name.name, variant.name.name
                                    ),
                                    field_name: field.name.name.clone(),
                                },
                                field.name.span(),
                            )
                            .with_primary_message("duplicate")
                            .with_secondary(*previous_span, "first defined here"));
                        }
                        seen_fields.insert(field.name.name.clone(), field.name.span());
                        let ty = self.type_from_annotation(env, &field.ty)?;
                        payload.push(RecordFieldType {
                            name: field.name.clone(),
                            ty,
                        });
                    }
                    EnumVariantKind::Struct(payload)
                }
            };

            variants.push(EnumVariantType {
                name: variant.name.clone(),
                kind,
            });
        }

        let enum_type = Arc::new(EnumType {
            name: def.name.clone(),
            variants,
            type_args: vec![],
        });
        let ty = Type::from(enum_type.clone());
        env.root.insert_type(
            &def.name,
            TypeEntry {
                ty: ty.clone(),
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        )?;

        let namespace = env.root.add_child(def.name.to_string());
        for (i, (variant, base)) in enum_type.variants.iter().zip(&def.variants).enumerate() {
            match &variant.kind {
                EnumVariantKind::Unit => {
                    // Unit variants are constants
                    namespace.insert_constant(
                        &base.name,
                        ConstantInfo {
                            span: base.span,
                            ty: ty.clone(),
                            type_params: vec![],
                            variant: i,
                        },
                    )?;
                }
                EnumVariantKind::Tuple(params) => {
                    // Tuple variants are functions
                    namespace.insert_constant(
                        &base.name,
                        ConstantInfo::new(
                            base.span,
                            Type::from(FunctionType {
                                kind: FunctionKind::Normal,
                                name_span: DUMMY_SPAN,
                                params: params.clone(),
                                param_spans: vec![],
                                result: ty.clone(),
                                callee: Some(StaticFunction::Constructor { variant: i }),
                            }),
                        ),
                    )?;
                }
                EnumVariantKind::Struct(_fields) => {
                    // Struct variants are constructors
                    namespace.insert_struct_constructor(
                        &base.name,
                        StructConstructor {
                            span: base.span,
                            ty: ty.clone(),
                            type_params: vec![],
                            enum_variant: i,
                        },
                    )?;
                }
            }
        }

        Ok(TypedEnumDef { ty: enum_type })
    }

    fn register_abi(&mut self, env: &mut TypeEnv, def: &AbiDef) -> Result<TypedAbiDef, TypeError> {
        if def.name.name == "Token" {
            return Err(TypeError::new(
                TypeErrorKind::ReservedAbiName {
                    name: def.name.name.clone(),
                },
                def.name.span(),
            ));
        }

        let mut functions = Vec::new();
        let mut methods = Vec::new();
        for part in &def.parts {
            match part {
                AbiPart::Event(event) => functions.push(self.register_event(env, event)?),
                AbiPart::Effect(effect) => functions.push(self.register_effect(env, effect)?),
                AbiPart::FnDecl(method) => {
                    let return_type = match &method.return_type {
                        Some(ann) => self.type_from_annotation(env, ann)?,
                        None => Type::Unit,
                    };
                    let id = self.next_name_id.fresh();
                    self.function_names.insert(method, id);
                    let ty = Arc::new(FunctionType {
                        kind: FunctionKind::Normal,
                        name_span: method.name.span,
                        // TODO: recapture `pub` keyword here
                        params: method
                            .params
                            .iter()
                            .map(|p| self.type_from_annotation(env, &p.ty))
                            .collect::<Result<Vec<_>, _>>()?,
                        param_spans: method.params.iter().map(|p| p.name.span).collect(),
                        result: return_type,
                        callee: Some(StaticFunction::Named(id)),
                    });
                    let m = TypedAbiMethodDecl {
                        name: method.name.clone(),
                        id,
                        ty,
                    };
                    functions.push(m.clone());
                    methods.push(m);
                }
            }
        }
        let abi_ty = Arc::new(AbiType {
            name: def.name.clone(),
            methods,
        });
        env.root
            .insert_type(&def.name, TypeEntry::from(Type::Abi(abi_ty.clone())))?;
        Ok(TypedAbiDef {
            ty: abi_ty,
            functions,
        })
    }

    fn register_event(
        &mut self,
        env: &mut TypeEnv,
        event: &EventDef,
    ) -> Result<TypedAbiMethodDecl, TypeError> {
        let mut param_types = Vec::with_capacity(event.params.len());
        let mut param_spans = Vec::with_capacity(event.params.len());
        for param in &event.params {
            let ty = self.type_from_annotation(env, &param.ty)?;
            param_types.push(ty);
            param_spans.push(param.ty.name_span());
        }
        let id = self.next_name_id.fresh();
        self.function_names.insert(event, id);
        let func_ty = Arc::new(FunctionType {
            kind: FunctionKind::Emit,
            name_span: event.name.span,
            params: param_types,
            param_spans,
            result: Type::Unit,
            callee: Some(StaticFunction::Named(id)),
        });
        env.root.insert_constant(
            &event.name,
            ConstantInfo::new(event.name.span, Type::from(func_ty.clone())),
        )?;
        Ok(TypedAbiMethodDecl {
            name: event.name.clone(),
            id,
            ty: func_ty,
        })
    }

    fn register_effect(
        &mut self,
        env: &mut TypeEnv,
        effect: &EffectDef,
    ) -> Result<TypedAbiMethodDecl, TypeError> {
        let mut param_types = Vec::with_capacity(effect.params.len());
        let mut param_spans = Vec::with_capacity(effect.params.len());
        for param in &effect.params {
            let ty = self.type_from_annotation(env, &param.ty)?;
            param_types.push(ty);
            param_spans.push(param.ty.name_span());
        }
        let return_type = match &effect.return_type {
            Some(ann) => self.type_from_annotation(env, ann)?,
            None => Type::Unit,
        };
        let id = self.next_name_id.fresh();
        self.function_names.insert(effect, id);
        let func_ty = Arc::new(FunctionType {
            kind: FunctionKind::Raise,
            name_span: effect.name.span,
            params: param_types,
            param_spans,
            result: return_type,
            callee: Some(StaticFunction::Named(id)),
        });
        env.root.insert_constant(
            &effect.name,
            ConstantInfo::new(effect.name.span, Type::from(func_ty.clone())),
        )?;
        Ok(TypedAbiMethodDecl {
            name: effect.name.clone(),
            id,
            ty: func_ty,
        })
    }

    fn register_utxo(&mut self, env: &mut TypeEnv, def: &UtxoDef) -> Result<(), TypeError> {
        let mut yields = Vec::new();
        let mut possible_abis = Vec::new();
        for part in &def.parts {
            match part {
                UtxoPart::Function(def) => {
                    Self::collect_yields(&mut yields, &def.body);
                }
                UtxoPart::AbiImpl { abi, parts } => {
                    possible_abis.push(env.root.get_abi(&abi)?.clone());
                    for part in parts {
                        Self::collect_yields(&mut yields, &part.body);
                    }
                }
                _ => {}
            }
        }

        let mut always_abis = Vec::new();
        if let Some((first, rest)) = yields.split_first() {
            for abi in first.iter() {
                always_abis.push(env.root.get_abi(&abi)?.clone());
            }
            for &expr in rest {
                let mut abis = Vec::new();
                for abi in expr.iter() {
                    abis.push(env.root.get_abi(&abi)?.clone());
                }
                always_abis.retain(|a| abis.contains(a));
            }
        }

        let ty = Type::Utxo(Arc::new(UtxoType {
            name: def.name.to_string(),
            id: self.next_name_id.fresh(),
            possible_abis,
            always_abis,
        }));
        env.root.insert_type(
            &def.name,
            TypeEntry {
                ty: ty.clone(),
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        )?;

        let mut ns = Namespace::default();

        for part in &def.parts {
            match part {
                UtxoPart::Function(function_def) => {
                    if let Some(FunctionExport::UtxoMain) = function_def.export {
                        let mut func_ty = self.function_def_to_type(env, function_def)?;
                        func_ty.result = ty.clone();
                        ns.insert_constant(
                            &function_def.name,
                            ConstantInfo::new(function_def.name.span, Type::from(func_ty)),
                        )?;
                    }
                }
                UtxoPart::Storage(_) => {}
                UtxoPart::AbiImpl { .. } => {}
            }
        }

        // TODO: extraneous clone
        env.root
            .add_child(def.name.to_string())
            .import_all_from(&ns)?;

        Ok(())
    }

    fn collect_yields<'a>(dest: &mut Vec<&'a Vec<Identifier>>, block: &'a Block) {
        for stmt in &block.statements {
            match &stmt.node {
                Statement::VariableDeclaration { value, .. } => {
                    Self::collect_yields_expr(dest, &value.node);
                }
                Statement::Assignment { value, .. } => {
                    Self::collect_yields_expr(dest, &value.node);
                }
                Statement::While { condition, body } => {
                    Self::collect_yields_expr(dest, &condition.node);
                    Self::collect_yields(dest, body);
                }
                Statement::Expression(spanned) => {
                    Self::collect_yields_expr(dest, &spanned.node);
                }
                Statement::Return(spanned) => {
                    if let Some(expr) = spanned {
                        Self::collect_yields_expr(dest, &expr.node);
                    }
                }
                Statement::Resume => {}
                Statement::TryWith { subject, effects } => {
                    Self::collect_yields(dest, subject);
                    for (_, _, body) in effects {
                        Self::collect_yields(dest, body);
                    }
                }
            }
        }
        if let Some(tail) = &block.tail_expression {
            Self::collect_yields_expr(dest, &tail.node);
        }
    }

    fn collect_yields_expr<'a>(dest: &mut Vec<&'a Vec<Identifier>>, expr: &'a Expr) {
        match expr {
            Expr::Yield { abis } => {
                dest.push(abis);
            }
            Expr::Grouping(spanned) => Self::collect_yields_expr(dest, &spanned.node),
            Expr::ScopedName(_) => {}
            Expr::Literal(_) => {}
            Expr::StructConstructor { fields, .. } => {
                for each in fields {
                    Self::collect_yields_expr(dest, &each.value.node);
                }
            }
            Expr::Emit { callee, args }
            | Expr::Raise { callee, args }
            | Expr::Runtime { callee, args }
            | Expr::Call { callee, args } => {
                Self::collect_yields_expr(dest, &callee.node);
                for each in args {
                    Self::collect_yields_expr(dest, &each.node);
                }
            }
            Expr::Block(block) => Self::collect_yields(dest, block),
            Expr::If {
                branches,
                else_branch,
            } => {
                for (condition, block) in branches {
                    if let IfCondition::Bool(expr) = condition {
                        Self::collect_yields_expr(dest, &expr.node);
                    }
                    Self::collect_yields(dest, block);
                }
                if let Some(block) = else_branch {
                    Self::collect_yields(dest, block);
                }
            }
            Expr::Match { scrutinee, arms } => {
                Self::collect_yields_expr(dest, &scrutinee.node);
                for arm in arms {
                    Self::collect_yields(dest, &arm.body);
                }
            }
            Expr::FieldAccess { target, .. } => {
                Self::collect_yields_expr(dest, &target.node);
            }
            Expr::Disclose { expr } | Expr::Unary { expr, .. } => {
                Self::collect_yields_expr(dest, &expr.node);
            }
            Expr::Binary { left, right, .. } => {
                Self::collect_yields_expr(dest, &left.node);
                Self::collect_yields_expr(dest, &right.node);
            }
        }
    }

    fn register_token(&mut self, env: &mut TypeEnv, def: &TokenDef) -> Result<(), TypeError> {
        let ty = Type::Token(Arc::new(TokenType {
            name: def.name.to_string(),
            id: self.next_name_id.fresh(),
        }));
        env.root.insert_type(
            &def.name,
            TypeEntry {
                ty,
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        )?;
        Ok(())
    }

    fn register_function(&mut self, env: &mut TypeEnv, def: &FunctionDef) -> Result<(), TypeError> {
        let ty = Type::Function(Arc::new(self.function_def_to_type(env, def)?));
        env.root
            .insert_constant(&def.name, ConstantInfo::new(def.name.span, ty))?;
        Ok(())
    }

    // ------------------------------------------------------------------------

    fn infer_utxo(
        &mut self,
        env: &mut TypeEnv,
        def: &UtxoDef,
    ) -> Result<(TypedUtxoDef, InferenceTree), TypeError> {
        env.push_scope();

        let mut parts = Vec::with_capacity(def.parts.len());
        let mut traces = Vec::with_capacity(def.parts.len());

        for part in &def.parts {
            parts.push(match part {
                UtxoPart::Storage(vars) => TypedUtxoPart::Storage(
                    vars.iter()
                        .map(|var| self.infer_utxo_global(env, var))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
                UtxoPart::Function(function) => {
                    if function.export == Some(starstream_types::FunctionExport::UtxoMain)
                        && function.return_type.is_some()
                    {
                        return Err(TypeError::new(
                            TypeErrorKind::ReturnTypeNotAllowed,
                            function.name.span(),
                        ));
                    }
                    let (func, _, trace) = self.infer_function(env, function)?;
                    traces.push(trace);
                    TypedUtxoPart::Function(func.into())
                }
                UtxoPart::AbiImpl { abi, parts } => {
                    let span = abi.span();

                    let parts = parts
                        .iter()
                        .map(|function| {
                            let (func, _, trace) = self.infer_function(env, function)?;
                            traces.push(trace);
                            Ok(func)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    // Assert that the signature sets match
                    let abi_info = env.root.get_abi(&abi)?;
                    self.check_abi_impl(abi, abi_info, &parts)?;
                    TypedUtxoPart::AbiImpl {
                        abi: abi_info.clone(),
                        span,
                        parts,
                    }
                }
            });
        }

        env.pop_scope();

        let Some(ty) = env.root.types.get(def.name.as_str()) else {
            unreachable!()
        };
        let Type::Utxo(utxo_ty) = &ty.ty else {
            unreachable!()
        };
        Ok((
            TypedUtxoDef {
                name: def.name.clone(),
                parts,
                ty: utxo_ty.clone(),
            },
            self.make_trace("T-Utxo", None, Some(def.name.to_string()), None, || traces),
        ))
    }

    fn infer_token(
        &mut self,
        env: &mut TypeEnv,
        def: &TokenDef,
    ) -> Result<(TypedTokenDef, InferenceTree), TypeError> {
        // Structural pre-pass: validate the shape (at least one `mint fn`,
        // exactly one `impl Token`) before inferring any bodies, so these
        // token-specific diagnostics take precedence over body-level errors
        // such as the duplicate-function clash from two `impl Token` blocks.
        let mint_count = def
            .parts
            .iter()
            .filter(|part| {
                matches!(
                    part,
                    TokenPart::Function(f)
                        if f.export == Some(starstream_types::FunctionExport::TokenMint)
                )
            })
            .count();
        let token_impl_count = def
            .parts
            .iter()
            .filter(
                |part| matches!(part, TokenPart::AbiImpl { abi, .. } if abi.as_str() == "Token"),
            )
            .count();

        if mint_count == 0 {
            return Err(TypeError::new(
                TypeErrorKind::TokenMissingMintFn {
                    name: def.name.to_string(),
                },
                def.name.span(),
            ));
        }
        match token_impl_count {
            0 => {
                return Err(TypeError::new(
                    TypeErrorKind::TokenMissingImpl {
                        name: def.name.to_string(),
                    },
                    def.name.span(),
                ));
            }
            1 => {}
            _ => {
                return Err(TypeError::new(
                    TypeErrorKind::TokenDuplicateImpl {
                        name: def.name.to_string(),
                    },
                    def.name.span(),
                ));
            }
        }

        env.push_scope();

        let mut parts = Vec::with_capacity(def.parts.len());
        let mut traces = Vec::with_capacity(def.parts.len());

        for part in &def.parts {
            parts.push(match part {
                TokenPart::Storage(vars) => TypedTokenPart::Storage(
                    vars.iter()
                        .map(|var| self.infer_token_global(env, var))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
                TokenPart::Function(function) => {
                    // `mint fn`s implicitly return unit — the caller-facing
                    // result (a handle to the minted token) is supplied by the
                    // runtime, so an explicit return type is rejected.
                    if function.export == Some(starstream_types::FunctionExport::TokenMint)
                        && function.return_type.is_some()
                    {
                        return Err(TypeError::new(
                            TypeErrorKind::ReturnTypeNotAllowed,
                            function.name.span(),
                        ));
                    }
                    let (func, _, trace) = self.infer_function(env, function)?;
                    traces.push(trace);
                    TypedTokenPart::Function(func.into())
                }
                TokenPart::AbiImpl { abi, parts } => {
                    let span = abi.span();

                    let parts = parts
                        .iter()
                        .map(|function| {
                            let (func, _, trace) = self.infer_function(env, function)?;
                            traces.push(trace);
                            Ok(func)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    // Assert that the signature sets match. `Token` resolves to
                    // the built-in ABI; other names must be user-declared ABIs.
                    let abi_info = env.root.get_abi(&abi)?;
                    self.check_abi_impl(abi, abi_info, &parts)?;

                    let abi = Type::Abi(abi_info.clone());
                    TypedTokenPart::AbiImpl { abi, span, parts }
                }
            });
        }

        env.pop_scope();

        let Some(ty) = env.root.types.get(def.name.as_str()) else {
            unreachable!()
        };
        assert!(matches!(ty.ty, Type::Token(_)));
        Ok((
            TypedTokenDef {
                name: def.name.clone(),
                parts,
                ty: ty.ty.clone(),
            },
            self.make_trace("T-Token", None, Some(def.name.to_string()), None, || traces),
        ))
    }

    fn infer_token_global(
        &mut self,
        env: &mut TypeEnv,
        var: &TokenGlobal,
    ) -> Result<TypedTokenGlobal, TypeError> {
        let ty = self.type_from_annotation(env, &var.ty)?;
        env.insert(
            var.name.name.clone(),
            Binding {
                decl_span: var.name.span(),
                mutable: true,
                scheme: Scheme::monomorphic(ty.clone()),
                class: BindingClass::Storage,
                visibility: BindingVisibility::Public,
            },
        );
        Ok(TypedTokenGlobal {
            indexed: var.indexed,
            name: var.name.clone(),
            ty,
        })
    }

    fn check_abi_impl(
        &self,
        abi_name: &Identifier,
        abi: &AbiType,
        methods: &[TypedFunctionDef],
    ) -> Result<(), TypeError> {
        // TODO: Reusing existing error codes, may want them to be more specific.
        let mut abi_methods = abi
            .methods
            .iter()
            .map(|method| (method.name.as_str(), method))
            .collect::<HashMap<_, _>>();

        for impl_method in methods {
            if let Some(abi_method) = abi_methods.remove(impl_method.name.as_str()) {
                // Method found, make sure the parameter counts match
                if abi_method.ty.params.len() != impl_method.params.len() {
                    return Err(TypeError::new(
                        TypeErrorKind::ArityMismatch {
                            expected: abi_method.ty.params.len(),
                            found: impl_method.params.len(),
                        },
                        impl_method.name.span(),
                    )
                    .with_primary_message(format!(
                        "implemented with {} parameter{} here",
                        impl_method.params.len(),
                        if impl_method.params.len() == 1 {
                            ""
                        } else {
                            "s"
                        },
                    ))
                    .with_secondary(
                        abi_method.name.span,
                        format!(
                            "declared with {} parameter{} here",
                            abi_method.ty.params.len(),
                            if abi_method.ty.params.len() == 1 {
                                ""
                            } else {
                                "s"
                            },
                        ),
                    ));
                }
                // And that the parameter types match
                for (i, ((abi_param, &abi_param_span), impl_param)) in abi_method
                    .ty
                    .params
                    .iter()
                    .zip(abi_method.ty.param_spans.iter())
                    .zip(impl_method.params.iter())
                    .enumerate()
                {
                    if *abi_param != impl_param.ty {
                        return Err(TypeError::new(
                            TypeErrorKind::ArgumentTypeMismatch {
                                expected: abi_param.clone(),
                                found: impl_param.ty.clone(),
                                position: i,
                                param_span: Some(abi_param_span),
                            },
                            impl_param.name.span(),
                        ));
                    }
                }
                // And return type must match
                if abi_method.ty.result != impl_method.return_type {
                    return Err(TypeError::new(
                        TypeErrorKind::ReturnMismatch {
                            expected: abi_method.ty.result.clone(),
                            found: impl_method.return_type.clone(),
                        },
                        abi_method.name.span,
                    ));
                }
            } else {
                // Method not in ABI
                return Err(TypeError::new(
                    TypeErrorKind::AbiMethodNotFound {
                        abi_name: abi_name.to_string(),
                        method_name: impl_method.name.to_string(),
                    },
                    impl_method.name.span(),
                ));
            }
        }

        if let Some((abi_method_name, _)) = abi_methods.into_iter().next() {
            // ABI has methods not in impl block
            return Err(TypeError::new(
                TypeErrorKind::AbiMethodNotFound {
                    abi_name: abi_name.to_string(),
                    method_name: abi_method_name.to_owned(),
                },
                abi_name.span(),
            ));
        }

        Ok(())
    }

    fn infer_utxo_global(
        &mut self,
        env: &mut TypeEnv,
        var: &UtxoGlobal,
    ) -> Result<TypedUtxoGlobal, TypeError> {
        let ty = self.type_from_annotation(env, &var.ty)?;
        env.insert(
            var.name.name.clone(),
            Binding {
                decl_span: var.name.span(),
                mutable: true,
                scheme: Scheme::monomorphic(ty.clone()),
                class: BindingClass::Storage,
                visibility: BindingVisibility::Public,
            },
        );
        Ok(TypedUtxoGlobal {
            name: var.name.clone(),
            ty,
        })
    }

    fn lookup_struct_info<'a>(
        &self,
        env: &'a TypeEnv,
        name: &[Identifier],
    ) -> Result<&'a StructConstructor, TypeError> {
        let (last, path) = name.split_last().unwrap();
        let ns = env.root.get_child(path)?;
        ns.struct_constructors.get(last.as_str()).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownStruct {
                    name: last.to_string(),
                },
                last.span(),
            )
        })
    }

    fn refresh_type_params(
        next_type_var: &mut TypeVarId,
        ty: &Type,
        type_params: &[TypeParam],
    ) -> Type {
        if type_params.is_empty() {
            return ty.clone();
        }
        let fresh_args: Vec<Type> = type_params
            .iter()
            .map(|_| Type::Var(next_type_var.fresh()))
            .collect();
        Self::apply_type_args(ty, type_params, &fresh_args)
    }

    /// Substitute type parameters with concrete args in a template.
    fn apply_type_args(template_ty: &Type, type_params: &[TypeParam], type_args: &[Type]) -> Type {
        let mapping: HashMap<TypeVarId, Type> = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(param, arg)| (param.id, arg.clone()))
            .collect();

        let mut ty = substitute_type(template_ty, &mapping, &Default::default());
        if let Type::Enum(ref mut enum_type) = ty {
            Arc::make_mut(enum_type).type_args = type_args.to_vec();
        }

        ty
    }

    fn bind_pattern_identifier(
        &mut self,
        env: &mut TypeEnv,
        ident: &Identifier,
        ty: Type,
    ) -> Result<(), TypeError> {
        if env.get_in_current_scope(&ident.name).is_some() {
            return Err(TypeError::new(
                TypeErrorKind::Redeclaration {
                    name: ident.name.clone(),
                },
                ident.span(),
            ));
        }

        env.insert(
            ident.name.clone(),
            Binding {
                decl_span: ident.span(),
                mutable: false,
                scheme: Scheme::monomorphic(ty),
                class: BindingClass::Local,
                visibility: BindingVisibility::Private,
            },
        );
        Ok(())
    }

    fn infer_pattern(
        &mut self,
        env: &mut TypeEnv,
        pattern: &Pattern,
        expected_ty: &Type,
        value_span: Span,
    ) -> Result<(TypedPattern, Vec<InferenceTree>), TypeError> {
        match pattern {
            Pattern::Name(name) => {
                let (last, rest) = name.split_last().unwrap();
                let ns = env.root.get_child(rest)?;
                if let Some(constant) = ns.constants.get(last.as_str()).cloned() {
                    // Identifier matching a constant is a test against that constant.
                    let (.., unify_trace) = self.unify(
                        expected_ty.clone(),
                        constant.ty.clone(),
                        value_span,
                        last.span,
                        TypeErrorKind::PatternEnumMismatch {
                            enum_name: rest.last().unwrap_or(last).to_string(),
                            found: expected_ty.clone(),
                        },
                    )?;
                    Ok((
                        TypedPattern::Constant {
                            name: name.clone(),
                            variant: constant.variant,
                        },
                        vec![unify_trace],
                    ))
                } else if rest.is_empty() {
                    // Unscoped identifier not matching any constant is a binding.
                    self.bind_pattern_identifier(env, last, expected_ty.clone())?;
                    Ok((TypedPattern::Binding(last.clone()), Vec::new()))
                } else {
                    // Scoped identifier not matching any constant is not valid.
                    Err(TypeError::new(
                        TypeErrorKind::UnknownName {
                            name: last.to_string(),
                        },
                        last.span,
                    ))
                }
            }
            Pattern::Wildcard { .. } => {
                // Wildcard matches anything but doesn't introduce a binding
                Ok((TypedPattern::Wildcard, Vec::new()))
            }
            Pattern::Literal { value, span } => {
                // Literal patterns must match the expected type
                let literal_ty = match value {
                    Literal::Integer(literal) => {
                        // Digits overflowing `i128` cannot fit any integer type.
                        if literal.value().is_none() {
                            return Err(TypeError::new(
                                TypeErrorKind::LiteralOutOfRange {
                                    literal: literal.to_string(),
                                    ty: Type::int(),
                                },
                                *span,
                            ));
                        }
                        Type::int()
                    }
                    Literal::Boolean(_) => Type::Bool,
                    Literal::Unit => Type::Unit,
                };
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    literal_ty.clone(),
                    value_span,
                    *span,
                    TypeErrorKind::GeneralMismatch {
                        expected: self.apply_for_display(expected_ty),
                        found: literal_ty.clone(),
                    },
                )?;
                Ok((TypedPattern::Literal(value.clone()), vec![unify_trace]))
            }
            Pattern::Struct { name, fields } => {
                let info = self.lookup_struct_info(env, name)?.clone();
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    info.ty.clone(),
                    value_span,
                    name.last().unwrap().span(),
                    TypeErrorKind::GeneralMismatch {
                        expected: info.ty.clone(),
                        found: expected_ty.clone(),
                    },
                )?;
                let mut traces = vec![unify_trace];

                let mut expected_fields = info
                    .fields()
                    .iter()
                    .map(|field| (field.name.to_string(), field.clone()))
                    .collect::<HashMap<_, _>>();

                let mut typed_fields = Vec::with_capacity(fields.len());
                let mut seen = HashMap::new();
                for field in fields {
                    if let Some(previous_span) = seen.get(&field.name.name) {
                        return Err(TypeError::new(
                            TypeErrorKind::DuplicateStructLiteralField {
                                field_name: field.name.name.clone(),
                            },
                            field.name.span(),
                        )
                        .with_primary_message("duplicate")
                        .with_secondary(*previous_span, "first used here"));
                    }
                    seen.insert(field.name.name.clone(), field.name.span());

                    let expected_field =
                        expected_fields.remove(field.name.as_str()).ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::UnknownStructField {
                                    struct_name: name.last().unwrap().to_string(),
                                    field_name: field.name.to_string(),
                                },
                                field.name.span(),
                            )
                        })?;

                    let (typed_pattern, mut pattern_traces) =
                        self.infer_pattern(env, &field.pattern, &expected_field.ty, value_span)?;
                    traces.append(&mut pattern_traces);
                    typed_fields.push(TypedStructPatternField {
                        name: field.name.clone(),
                        pattern: Box::new(typed_pattern),
                    });
                }

                if let Some((field_name, _)) = expected_fields.into_iter().next() {
                    return Err(TypeError::new(
                        TypeErrorKind::MissingStructField {
                            struct_name: name.last().unwrap().to_string(),
                            field_name,
                        },
                        name.last().unwrap().span(),
                    ));
                }

                Ok((
                    TypedPattern::Struct {
                        name: name.clone(),
                        fields: typed_fields,
                    },
                    traces,
                ))
            }
            Pattern::Tuple { name, fields } => {
                let (last, rest) = name.split_last().unwrap();
                let enum_name = rest.last().map(|n| n.as_str()).unwrap_or("<anonymous>>");
                let ns = env.root.get_child(rest)?;
                let Some(callee) = ns.constants.get(last.as_str()) else {
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownName {
                            name: last.to_string(),
                        },
                        last.span,
                    ));
                };
                let Type::Function(func) = &callee.ty else {
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownName {
                            name: last.to_string(),
                        },
                        last.span,
                    ));
                };
                let Some(StaticFunction::Constructor { .. }) = &func.callee else {
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownEnumVariant {
                            enum_name: enum_name.to_owned(),
                            variant_name: last.to_string(),
                        },
                        last.span,
                    ));
                };
                if fields.len() != func.params.len() {
                    return Err(TypeError::new(
                        TypeErrorKind::EnumPayloadMismatch {
                            enum_name: enum_name.to_owned(),
                            variant_name: last.to_string(),
                            expected: EnumPayloadKind::tuple(func.params.len()),
                            found: EnumPayloadKind::tuple(fields.len()),
                        },
                        last.span,
                    ));
                }
                let func = func.clone();
                let (_, unify_trace) = self.unify(
                    expected_ty.clone(),
                    func.result.clone(),
                    value_span,
                    last.span,
                    TypeErrorKind::PatternEnumMismatch {
                        enum_name: enum_name.to_owned(),
                        found: self.apply_for_display(expected_ty),
                    },
                )?;
                let mut traces = vec![unify_trace];
                let mut typed = Vec::with_capacity(fields.len());
                for (pattern, ty) in fields.iter().zip(&func.params) {
                    let (typed_pattern, mut pattern_traces) =
                        self.infer_pattern(env, pattern, ty, value_span)?;
                    traces.append(&mut pattern_traces);
                    typed.push(typed_pattern);
                }
                Ok((
                    TypedPattern::Tuple {
                        name: name.clone(),
                        fields: typed,
                    },
                    traces,
                ))
            }
        }
    }

    fn function_def_to_type(
        &mut self,
        env: &TypeEnv,
        function: &FunctionDef,
    ) -> Result<FunctionType, TypeError> {
        // Visit param & return types.
        let param_types = function
            .params
            .iter()
            .map(|param| self.type_from_annotation(env, &param.ty))
            .collect::<Result<Vec<_>, _>>()?;
        let expected_return = match &function.return_type {
            Some(annotation) => self.type_from_annotation(env, annotation)?,
            None => Type::unit(),
        };
        let param_spans = function
            .params
            .iter()
            .map(|param| param.ty.name_span())
            .collect::<Vec<_>>();
        let id = *self
            .function_names
            .entry(function)
            .or_insert_with(|| self.next_name_id.fresh());
        Ok(FunctionType {
            params: param_types.clone(),
            param_spans,
            result: expected_return.clone(),
            kind: FunctionKind::Normal,
            name_span: function.name.span,
            callee: Some(StaticFunction::Named(id)),
        })
    }

    fn infer_function(
        &mut self,
        env: &mut TypeEnv,
        function: &FunctionDef,
    ) -> Result<(TypedFunctionDef, Arc<FunctionType>, InferenceTree), TypeError> {
        let func_ty = Arc::new(self.function_def_to_type(env, function)?);
        let return_span = function.return_span();

        // Insert function into environment. Happens before code so that recursion is allowed.
        env.insert(
            function.name.to_string(),
            Binding {
                decl_span: function.name.span,
                mutable: false,
                scheme: Scheme::monomorphic(Type::Function(func_ty.clone())),
                class: BindingClass::Local,
                visibility: BindingVisibility::Private,
            },
        );

        env.push_scope();
        let mut typed_params = Vec::with_capacity(function.params.len());
        let mut private_param_decl_spans = Vec::new();
        for (param, ty) in function.params.iter().zip(&func_ty.params) {
            let decl_span = param.name.span_or(function.name.span());
            if !param.public {
                private_param_decl_spans.push(decl_span);
            }
            env.insert(
                param.name.name.clone(),
                Binding {
                    decl_span,
                    mutable: false,
                    scheme: Scheme::monomorphic(ty.clone()),
                    class: BindingClass::Local,
                    visibility: if param.public {
                        BindingVisibility::Public
                    } else {
                        BindingVisibility::Private
                    },
                },
            );
            typed_params.push(TypedFunctionParam {
                public: param.public,
                name: param.name.clone(),
                ty: ty.clone(),
            });
        }

        let mut ctx = FunctionCtx {
            expected_return: func_ty.result.clone(),
            return_span,
            saw_return: false,
            private_param_decl_spans,
            is_coroutine: function.export == Some(starstream_types::FunctionExport::UtxoMain),
        };

        let (typed_body, body_traces) = self.infer_block(env, &function.body, &mut ctx, true)?;

        env.pop_scope();

        if func_ty.result != Type::unit() && !ctx.saw_return && typed_body.tail_expression.is_none()
        {
            return Err(TypeError::new(
                TypeErrorKind::MissingReturn {
                    expected: func_ty.result.clone(),
                },
                function.name.span_or(return_span),
            )
            .with_help("add a `return` or tail expression to satisfy the signature"));
        }

        let subject = self.maybe_string(|| function.name.name.clone());
        let result = self.maybe_string(|| self.format_type(&ctx.expected_return));
        let trace = self.make_trace("T-Fn", None, subject, result, || body_traces);

        Ok((
            TypedFunctionDef {
                export: function.export.clone(),
                name: function.name.clone(),
                id: *self.function_names.get(function).unwrap(),
                params: typed_params,
                return_type: ctx.expected_return,
                body: typed_body,
            },
            func_ty,
            trace,
        ))
    }

    /// Type-check a single statement, yielding its typed form and an inference trace.
    fn infer_statement(
        &mut self,
        env: &mut TypeEnv,
        statement: &Statement,
        ctx: &mut FunctionCtx,
    ) -> Result<(TypedStatement, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let stmt_repr = self.maybe_string(|| self.format_statement_src(statement));
        match statement {
            Statement::VariableDeclaration {
                public,
                mutable,
                name,
                ty,
                value,
            } => {
                if let Some(previous_decl) = env.get_in_current_scope(&name.name) {
                    return Err(TypeError::new(
                        TypeErrorKind::Redeclaration {
                            name: name.name.clone(),
                        },
                        name.span_or(value.span),
                    )
                    .with_secondary(previous_decl.decl_span, "previously defined here"));
                }

                let (typed_value, value_trace) = self.infer_expr(env, value, ctx)?;
                let mut children = vec![value_trace];
                let mut value_type = self.apply(&typed_value.node.ty);

                if let Some(ty) = ty {
                    // Binding has a type annotation, so unify it with the initial value.
                    let expected_type = self.type_from_annotation(env, ty)?;
                    let (new_value_type, unify_trace) = self.unify(
                        expected_type.clone(),
                        value_type.clone(),
                        name.span_or(value.span),
                        value.span,
                        TypeErrorKind::AssignmentMismatch {
                            name: name.name.clone(),
                            expected: self.apply_for_display(&expected_type),
                            found: self.apply_for_display(&value_type),
                        },
                    )?;
                    value_type = new_value_type;
                    children.push(unify_trace);
                }

                let value_visibility = self.source_expr_visibility(env, value);
                if *public && value_visibility != BindingVisibility::Public {
                    let help = if let Some(param_name) =
                        self.private_parameter_rhs_name(env, value, ctx)
                    {
                        format!(
                            "`{param_name}` is a private parameter; declare it as `pub {param_name}: ...` in the function signature if callers should provide a public value here.",
                        )
                    } else {
                        format!(
                            "`{}` is public; wrap private RHS with `disclose(...)`, e.g., `let pub {} = disclose(expr);`",
                            name.name, name.name
                        )
                    };
                    return Err(TypeError::new(
                        TypeErrorKind::ExplicitDisclosureRequiredForPublicBinding {
                            variable_name: name.name.clone(),
                        },
                        value.span,
                    )
                    .with_help(help));
                }

                let scheme = self.generalize(env, &value_type);
                env.insert(
                    name.name.clone(),
                    Binding {
                        decl_span: name.span_or(value.span),
                        mutable: *mutable,
                        scheme,
                        class: BindingClass::Local,
                        visibility: if *public {
                            BindingVisibility::Public
                        } else {
                            BindingVisibility::Private
                        },
                    },
                );

                let value_type_repr = self.maybe_string(|| self.format_type(&value_type));
                let tree = self.make_trace(
                    "T-Let",
                    env_context.clone(),
                    stmt_repr.clone(),
                    value_type_repr,
                    || children,
                );

                Ok((
                    TypedStatement::VariableDeclaration {
                        public: *public,
                        mutable: *mutable,
                        name: name.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Assignment { target, value } => {
                let binding = env.get(&target.name).cloned().ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownName {
                            name: target.name.clone(),
                        },
                        target.span_or(value.span),
                    )
                })?;

                if !binding.mutable {
                    return Err(TypeError::new(
                        TypeErrorKind::AssignmentToImmutable {
                            name: target.name.clone(),
                        },
                        target.span_or(value.span),
                    )
                    .with_primary_message("assigned here")
                    .with_secondary(binding.decl_span, "declared without `mut` here")
                    .with_help("consider changing `let` to `let mut`"));
                }

                let expected_type = self.instantiate(&binding.scheme);
                let (typed_value, value_trace) = self.infer_expr(env, value, ctx)?;
                let value_visibility = self.source_expr_visibility(env, value);
                if binding.visibility == BindingVisibility::Public
                    && value_visibility != BindingVisibility::Public
                {
                    let binding_kind = if binding.class == BindingClass::Storage {
                        "storage binding"
                    } else {
                        "public binding"
                    };
                    let help = if let Some(param_name) =
                        self.private_parameter_rhs_name(env, value, ctx)
                    {
                        format!(
                            "`{param_name}` is a private parameter; declare it as `pub {param_name}: ...` in the function signature if callers should provide a public value here.",
                        )
                    } else {
                        format!(
                            "`{}` is a {binding_kind}; wrap private RHS with `disclose(...)`, e.g., `{} = disclose(expr);`",
                            target.name, target.name
                        )
                    };
                    return Err(TypeError::new(
                        TypeErrorKind::ExplicitDisclosureRequiredForPublicBinding {
                            variable_name: target.name.clone(),
                        },
                        value.span,
                    )
                    .with_help(help));
                }
                let actual_type = typed_value.node.ty.clone();

                let (_, unify_trace) = self.unify(
                    actual_type.clone(),
                    expected_type.clone(),
                    value.span,
                    target.span_or(value.span),
                    TypeErrorKind::AssignmentMismatch {
                        name: target.name.clone(),
                        expected: self.apply_for_display(&expected_type),
                        found: self.apply_for_display(&actual_type),
                    },
                )?;

                let expected_repr = self.maybe_string(|| self.format_type(&expected_type));
                let tree = self.make_trace(
                    "T-Assign",
                    env_context.clone(),
                    stmt_repr.clone(),
                    expected_repr,
                    || vec![value_trace, unify_trace],
                );

                Ok((
                    TypedStatement::Assignment {
                        target: target.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Return(value) => {
                let result_repr = self.maybe_string(|| self.format_type(&ctx.expected_return));
                match value {
                    Some(expr) => {
                        let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
                        let actual_type = typed_expr.node.ty.clone();
                        let (_, unify_trace) = self.unify(
                            actual_type.clone(),
                            ctx.expected_return.clone(),
                            expr.span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply_for_display(&ctx.expected_return),
                                found: self.apply_for_display(&actual_type),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-Return",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![expr_trace, unify_trace],
                        );
                        Ok((TypedStatement::Return(Some(typed_expr)), tree))
                    }
                    None => {
                        let unit = Type::unit();
                        let (_, unify_trace) = self.unify(
                            unit.clone(),
                            ctx.expected_return.clone(),
                            ctx.return_span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply_for_display(&ctx.expected_return),
                                found: self.apply_for_display(&unit),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-ReturnUnit",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![unify_trace],
                        );
                        Ok((TypedStatement::Return(None), tree))
                    }
                }
            }
            Statement::While { condition, body } => {
                let (typed_condition, cond_trace) = self.infer_expr(env, condition, ctx)?;
                let bool_check = self.require_bool(
                    &typed_condition.node.ty,
                    condition.span,
                    ConditionContext::While,
                )?;

                let (typed_body, body_traces) = self.infer_block(env, body, ctx, false)?;

                let mut children = vec![cond_trace, bool_check];
                children.extend(body_traces);

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-While",
                    env_context.clone(),
                    stmt_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    TypedStatement::While {
                        condition: typed_condition,
                        body: typed_body,
                    },
                    tree,
                ))
            }
            Statement::Expression(expr) => {
                let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace("T-Expr", env_context, stmt_repr, unit_result, || {
                    vec![expr_trace]
                });
                Ok((TypedStatement::Expression(typed_expr), tree))
            }
            Statement::Resume => {
                // Do like `return ();`
                let result_repr = self.maybe_string(|| self.format_type(&ctx.expected_return));
                let (_, unify_trace) = self.unify(
                    Type::Unit,
                    ctx.expected_return.clone(),
                    ctx.return_span,
                    ctx.return_span,
                    TypeErrorKind::ReturnMismatch {
                        expected: self.apply_for_display(&ctx.expected_return),
                        found: Type::Unit,
                    },
                )?;
                ctx.saw_return = true;
                let tree = self.make_trace("T-Resume", env_context, stmt_repr, result_repr, || {
                    vec![unify_trace]
                });
                Ok((TypedStatement::Resume, tree))
            }
            Statement::TryWith { subject, effects } => {
                let (subject, mut children) = self.infer_block(env, subject, ctx, false)?;
                // Require block to return unit for now - we could make try-with an expression later?
                let (_, unit_trace) = self.unify(
                    Type::Unit,
                    subject.ty().clone(),
                    ctx.return_span,
                    subject.tail_span(),
                    TypeErrorKind::GeneralMismatch {
                        expected: Type::Unit,
                        found: self.apply_for_display(subject.ty()),
                    },
                )?;
                children.push(unit_trace);

                let mut typed_effects: Vec<(ScopedName, Vec<TypedPattern>, TypedBlock)> =
                    Vec::with_capacity(effects.len());
                for (name, patterns, block) in effects {
                    env.push_scope();
                    let (ty, _) = self.lookup_name(env, name)?;
                    let Type::Function(func) = ty else { panic!() };
                    // TODO: enforce that it's an effect?

                    if func.params.len() != patterns.len() {
                        return Err(TypeError::new(
                            TypeErrorKind::ArityMismatch {
                                expected: func.params.len(),
                                found: patterns.len(),
                            },
                            name.first().unwrap().span,
                        ));
                    }

                    let mut typed_patterns = Vec::with_capacity(patterns.len());
                    for (ty, pat) in func.params.iter().zip(patterns) {
                        let (tp, trace) = self.infer_pattern(env, pat, ty, DUMMY_SPAN)?;
                        typed_patterns.push(tp);
                        children.extend(trace);
                    }

                    let (block, trace) = self.infer_block(env, block, ctx, false)?;
                    children.extend(trace);
                    let (_, unit_trace) = self.unify(
                        Type::Unit,
                        subject.ty().clone(),
                        ctx.return_span,
                        subject.tail_span(),
                        TypeErrorKind::GeneralMismatch {
                            expected: Type::Unit,
                            found: self.apply_for_display(subject.ty()),
                        },
                    )?;
                    children.push(unit_trace);

                    typed_effects.push((name.clone(), typed_patterns, block));
                    env.pop_scope();
                }
                let tree = self.make_trace("T-TryWith", env_context, stmt_repr, None, || children);
                Ok((
                    TypedStatement::TryWith {
                        subject,
                        effects: typed_effects,
                    },
                    tree,
                ))
            }
        }
    }

    /// Type-check a block, returning typed statements plus per-statement traces.
    fn infer_block(
        &mut self,
        env: &mut TypeEnv,
        block: &Block,
        ctx: &mut FunctionCtx,
        treat_tail_as_return: bool,
    ) -> Result<(TypedBlock, Vec<InferenceTree>), TypeError> {
        env.push_scope();
        let mut typed_statements = Vec::with_capacity(block.statements.len());
        let mut traces = Vec::with_capacity(block.statements.len() + 1);
        for statement in &block.statements {
            let (typed, trace) = self.infer_statement(env, &statement.node, ctx)?;
            typed_statements.push(typed);
            traces.push(trace);
        }

        let mut tail_expression = None;
        if let Some(expr) = &block.tail_expression {
            let (typed_expr, expr_trace) = self.infer_expr(env, expr, ctx)?;
            let mut children = vec![expr_trace];
            if treat_tail_as_return {
                ctx.saw_return = true;
                let actual = typed_expr.node.ty.clone();
                let (_, unify_trace) = self.unify(
                    actual.clone(),
                    ctx.expected_return.clone(),
                    expr.span,
                    ctx.return_span,
                    TypeErrorKind::ReturnMismatch {
                        expected: self.apply_for_display(&ctx.expected_return),
                        found: self.apply_for_display(&actual),
                    },
                )?;
                children.push(unify_trace);
            }

            let label = if treat_tail_as_return {
                "T-ReturnTail"
            } else {
                "T-Tail"
            };
            let subject = self.maybe_string(|| self.format_expr_src(expr));
            let result = self.maybe_string(|| self.format_type(&typed_expr.node.ty));
            let tail_trace = self.make_trace(label, None, subject, result, || children);
            traces.push(tail_trace);
            tail_expression = Some(typed_expr);
        }

        env.pop_scope();
        Ok((TypedBlock::new(typed_statements, tail_expression), traces))
    }

    fn lookup_name(
        &mut self,
        env: &TypeEnv,
        name: &ScopedName,
    ) -> Result<(Type, Option<usize>), TypeError> {
        let (last, path) = name.split_last().unwrap();
        if path.is_empty()
            && let Some(local) = env.get(last.as_str())
        {
            return Ok((self.instantiate(&local.scheme), None));
        }

        let ns = env.root.get_child(path)?;
        if let Some(constant) = ns.constants.get(last.as_str()) {
            Ok((
                Self::refresh_type_params(
                    &mut self.next_type_var,
                    &constant.ty,
                    &constant.type_params,
                ),
                Some(constant.variant),
            ))
        } else {
            Err(TypeError::new(
                TypeErrorKind::UnknownName {
                    name: last.to_string(),
                },
                last.span,
            ))
        }
    }

    /// Type-check an expression, returning the typed node and corresponding trace tree.
    fn infer_expr(
        &mut self,
        env: &mut TypeEnv,
        expr: &Spanned<Expr>,
        ctx: &mut FunctionCtx,
    ) -> Result<(Spanned<TypedExpr>, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let subject_repr = self.maybe_string(|| self.format_expr_src(expr));
        match &expr.node {
            Expr::Literal(lit) => {
                let (ty, kind, rule) = match lit {
                    Literal::Integer(value) => {
                        let ty = self.fresh_int_var();
                        if let Type::Var(id) = &ty {
                            self.int_literal_values
                                .insert(*id, (value.clone(), expr.span));
                        }
                        (
                            ty,
                            TypedExprKind::Literal(Literal::Integer(value.clone())),
                            "T-Int",
                        )
                    }
                    Literal::Boolean(value) => (
                        Type::bool(),
                        TypedExprKind::Literal(Literal::Boolean(*value)),
                        "T-Bool",
                    ),
                    Literal::Unit => (
                        Type::unit(),
                        TypedExprKind::Literal(Literal::Unit),
                        "T-Unit",
                    ),
                };
                let typed = Spanned::new(TypedExpr::new(ty.clone(), kind), expr.span);
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::ScopedName(name) => {
                let (ty, constant) = self.lookup_name(env, name)?;
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let typed = Spanned::new(
                    TypedExpr::new(
                        ty,
                        TypedExprKind::ScopedName {
                            name: name.clone(),
                            constant,
                        },
                    ),
                    expr.span,
                );
                let tree = self.make_trace(
                    "T-Var",
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::Unary { op, expr: inner } => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner, ctx)?;
                let check = match op {
                    UnaryOp::Negate => {
                        let inner_ty = self.apply(&typed_inner.node.ty);
                        match &inner_ty {
                            Type::Int(w) if w.is_signed() => {
                                // Concrete signed int type — OK
                                let subject =
                                    self.maybe_string(|| self.format_type(&inner_ty).to_string());
                                let result = self.maybe_string(|| "ok (signed int)".to_string());
                                self.make_trace("Check-Negate", None, subject, result, Vec::new)
                            }
                            Type::Int(w) => {
                                // Unsigned int — error
                                return Err(TypeError::new(
                                    TypeErrorKind::UnaryMismatch {
                                        op: *op,
                                        expected: Type::int(),
                                        found: Type::Int(*w),
                                    },
                                    inner.span,
                                )
                                .with_primary_message(format!(
                                    "cannot negate unsigned type `{}`",
                                    w.display_name()
                                )));
                            }
                            Type::Var(id) if self.int_vars.contains(id) => {
                                // Int-constrained var (polymorphic literal) — allow for now,
                                // signedness will be checked when the type resolves
                                let subject =
                                    self.maybe_string(|| self.format_type(&inner_ty).to_string());
                                let result = self.maybe_string(|| "ok (int var)".to_string());
                                self.make_trace("Check-Negate", None, subject, result, Vec::new)
                            }
                            _ => {
                                return Err(TypeError::new(
                                    TypeErrorKind::UnaryMismatch {
                                        op: *op,
                                        expected: Type::int(),
                                        found: inner_ty,
                                    },
                                    inner.span,
                                ));
                            }
                        }
                    }
                    UnaryOp::Not => self.require_is(
                        &typed_inner.node.ty,
                        Type::bool(),
                        inner.span,
                        inner.span,
                        TypeErrorKind::UnaryMismatch {
                            op: *op,
                            expected: Type::bool(),
                            found: self.apply_for_display(&typed_inner.node.ty),
                        },
                    )?,
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Unary {
                            op: *op,
                            expr: Box::new(typed_inner.clone()),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    UnaryOp::Negate => "T-Unary-Neg",
                    UnaryOp::Not => "T-Unary-Not",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || vec![inner_trace, check],
                );

                Ok((typed, tree))
            }
            Expr::Binary { op, left, right } => {
                let (typed_left, left_trace) = self.infer_expr(env, left, ctx)?;
                let (typed_right, right_trace) = self.infer_expr(env, right, ctx)?;
                let left_ty = self.apply(&typed_left.node.ty);
                let right_ty = self.apply(&typed_right.node.ty);
                let left_label_span = self.label_span_for_expr(left);
                let right_label_span = self.label_span_for_expr(right);

                let mut children = vec![left_trace, right_trace];
                let expr_type = match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Remainder => {
                        let both_int = self.is_int_like(&left_ty) && self.is_int_like(&right_ty);
                        if !both_int {
                            let left_display = self.apply_for_display(&typed_left.node.ty);
                            let right_display = self.apply_for_display(&typed_right.node.ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_display.clone(),
                                    right: right_display.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!(
                                "has type `{}`",
                                left_display.compact_display()
                            ))
                            .with_secondary(
                                right_label_span,
                                format!("has type `{}`", right_display.compact_display()),
                            ));
                        }

                        // Unify left and right to ensure same int width
                        let (unified_ty, unify_trace) = self.unify(
                            left_ty.clone(),
                            right_ty.clone(),
                            left_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply_for_display(&typed_left.node.ty),
                                right: self.apply_for_display(&typed_right.node.ty),
                            },
                        )?;
                        children.push(unify_trace);
                        unified_ty
                    }
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual => {
                        let comparison = self.require_numeric_or_bool_pair(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(comparison);
                        Type::bool()
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        let check = self.require_same_primitive(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(check);
                        Type::bool()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        let both_bool =
                            matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool);
                        if !both_bool {
                            let left_display = self.apply_for_display(&typed_left.node.ty);
                            let right_display = self.apply_for_display(&typed_right.node.ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_display.clone(),
                                    right: right_display.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!(
                                "has type `{}`",
                                left_display.compact_display()
                            ))
                            .with_secondary(
                                right_label_span,
                                format!("has type `{}`", right_display.compact_display()),
                            ));
                        }

                        children.push(self.require_is(
                            &typed_left.node.ty,
                            Type::bool(),
                            left_label_span,
                            left_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply_for_display(&typed_left.node.ty),
                                right: self.apply_for_display(&typed_right.node.ty),
                            },
                        )?);
                        children.push(self.require_is(
                            &typed_right.node.ty,
                            Type::bool(),
                            right_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply_for_display(&typed_left.node.ty),
                                right: self.apply_for_display(&typed_right.node.ty),
                            },
                        )?);
                        Type::bool()
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        expr_type.clone(),
                        TypedExprKind::Binary {
                            op: *op,
                            left: Box::new(typed_left),
                            right: Box::new(typed_right),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    BinaryOp::Add => "T-Bin-Add",
                    BinaryOp::Subtract => "T-Bin-Sub",
                    BinaryOp::Multiply => "T-Bin-Mul",
                    BinaryOp::Divide => "T-Bin-Div",
                    BinaryOp::Remainder => "T-Bin-Rem",
                    BinaryOp::Less => "T-Bin-Lt",
                    BinaryOp::LessEqual => "T-Bin-Le",
                    BinaryOp::Greater => "T-Bin-Gt",
                    BinaryOp::GreaterEqual => "T-Bin-Ge",
                    BinaryOp::Equal => "T-Bin-Eq",
                    BinaryOp::NotEqual => "T-Bin-Neq",
                    BinaryOp::And => "T-Bin-And",
                    BinaryOp::Or => "T-Bin-Or",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || children,
                );

                Ok((typed, tree))
            }
            Expr::Grouping(inner) => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner, ctx)?;
                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Grouping(Box::new(typed_inner.clone())),
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree =
                    self.make_trace("T-Group", env_context, subject_repr, result_repr, || {
                        vec![inner_trace]
                    });
                Ok((typed, tree))
            }
            Expr::StructConstructor { name, fields } => {
                let info = self.lookup_struct_info(env, name)?.clone();
                let mut expected = info
                    .fields()
                    .iter()
                    .map(|field| (field.name.to_string(), (field.ty.clone(), field.name.span)))
                    .collect::<HashMap<_, _>>();
                let mut typed_fields = Vec::with_capacity(fields.len());
                let mut children = Vec::new();
                let mut seen = HashMap::new();

                for field in fields {
                    if let Some(previous_span) = seen.get(&field.name.name) {
                        return Err(TypeError::new(
                            TypeErrorKind::DuplicateStructLiteralField {
                                field_name: field.name.name.clone(),
                            },
                            field.name.span(),
                        )
                        .with_primary_message("duplicate")
                        .with_secondary(*previous_span, "first used here"));
                    }
                    seen.insert(field.name.name.clone(), field.name.span());

                    let (expected_ty, _) = expected.remove(&field.name.name).ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownStructField {
                                struct_name: name.last().unwrap().to_string(),
                                field_name: field.name.name.clone(),
                            },
                            field.name.span(),
                        )
                    })?;

                    let (typed_value, value_trace) = self.infer_expr(env, &field.value, ctx)?;
                    let actual_ty = typed_value.node.ty.clone();
                    let (_, unify_trace) = self.unify(
                        actual_ty.clone(),
                        expected_ty.clone(),
                        field.value.span,
                        field.name.span_or(field.value.span),
                        TypeErrorKind::GeneralMismatch {
                            expected: expected_ty,
                            found: self.apply_for_display(&actual_ty),
                        },
                    )?;
                    children.push(value_trace);
                    children.push(unify_trace);

                    typed_fields.push(TypedStructFieldInitializer {
                        name: field.name.clone(),
                        value: typed_value,
                    });
                }

                if let Some((field_name, _)) = expected.into_iter().next() {
                    return Err(TypeError::new(
                        TypeErrorKind::MissingStructField {
                            struct_name: name.last().unwrap().to_string(),
                            field_name,
                        },
                        name.last().unwrap().span(),
                    ));
                }

                let typed = Spanned::new(
                    TypedExpr::new(
                        Self::refresh_type_params(
                            &mut self.next_type_var,
                            &info.ty,
                            &info.type_params,
                        ),
                        TypedExprKind::StructConstructor {
                            name: name.clone(),
                            enum_variant: info.enum_variant,
                            fields: typed_fields,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    "T-StructLit",
                    env_context,
                    subject_repr,
                    result_repr,
                    || children,
                );
                Ok((typed, tree))
            }
            Expr::FieldAccess { target, field } => {
                let (typed_target, target_trace) = self.infer_expr(env, target, ctx)?;
                let target_ty = self.apply_for_display(&typed_target.node.ty);
                let field_ty = match target_ty.clone() {
                    Type::Record(record) => record
                        .fields
                        .iter()
                        .find(|entry| entry.name.as_str() == field.name.as_str())
                        .map(|entry| entry.ty.clone())
                        .ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::FieldAccessUnknownField {
                                    field_name: field.name.clone(),
                                    ty: target_ty.clone(),
                                },
                                field.span(),
                            )
                        })?,
                    Type::Abi(abi) => {
                        let method = abi
                            .methods
                            .iter()
                            .find(|m| m.name.as_str() == field.name)
                            .ok_or_else(|| {
                                TypeError::new(
                                    TypeErrorKind::AbiMethodNotFound {
                                        abi_name: abi.name.to_string(),
                                        method_name: field.name.clone(),
                                    },
                                    field.span(),
                                )
                            })?;
                        Type::Function(method.ty.clone())
                    }
                    _ => {
                        return Err(TypeError::new(
                            TypeErrorKind::FieldAccessNotStruct {
                                found: target_ty.clone(),
                            },
                            target.span,
                        ));
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        field_ty.clone(),
                        TypedExprKind::FieldAccess {
                            target: Box::new(typed_target.clone()),
                            field: field.clone(),
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&field_ty));
                let tree = self.make_trace(
                    "T-FieldAccess",
                    env_context,
                    subject_repr,
                    result_repr,
                    || vec![target_trace],
                );
                Ok((typed, tree))
            }
            Expr::Block(block) => {
                let (typed_block, block_traces) = self.infer_block(env, block, ctx, false)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-Block",
                    env_context.clone(),
                    subject_repr.clone(),
                    unit_result,
                    || block_traces,
                );
                let ty = typed_block
                    .tail_expression
                    .as_ref()
                    .map_or(Type::Unit, |tail| tail.node.ty.clone());
                Ok((
                    Spanned::new(
                        TypedExpr::new(ty, TypedExprKind::Block(Box::new(typed_block))),
                        expr.span,
                    ),
                    tree,
                ))
            }
            Expr::If {
                branches,
                else_branch,
            } => {
                let mut children = Vec::new();
                let mut typed_branches = Vec::with_capacity(branches.len());
                let mut result_ty: Option<Type> = None;

                for (condition, then_branch) in branches {
                    let typed_condition = match condition {
                        IfCondition::Bool(condition) => {
                            let (typed_condition, cond_trace) =
                                self.infer_expr(env, condition, ctx)?;
                            children.push(cond_trace);
                            let bool_check = self.require_bool(
                                &typed_condition.node.ty,
                                condition.span,
                                ConditionContext::If,
                            )?;
                            children.push(bool_check);
                            TypedIfCondition::Bool(typed_condition)
                        }
                        IfCondition::Is { name, abi_name } => {
                            let binding = env.get(&name.name).ok_or_else(|| {
                                TypeError::new(
                                    TypeErrorKind::UnknownName {
                                        name: name.name.clone(),
                                    },
                                    name.span(),
                                )
                            })?;
                            let var_ty = self.apply(&binding.scheme.ty);

                            match &var_ty {
                                Type::UtxoAny | Type::Utxo(_) => {}
                                _ => {
                                    return Err(TypeError::new(
                                        TypeErrorKind::IsCheckRequiresUtxo {
                                            name: name.name.clone(),
                                            found: self.apply_for_display(&var_ty),
                                        },
                                        name.span(),
                                    ));
                                }
                            }

                            let abi = env.root.get_abi(&abi_name)?.clone();
                            let var_name_str = name.name.clone();
                            let abi_name_str = abi_name.name.clone();

                            // Push a scope with the narrowed type and set up
                            // linearity tracking before inferring the block.
                            env.push_scope();
                            env.insert(
                                var_name_str.clone(),
                                Binding {
                                    decl_span: name.span(),
                                    mutable: false,
                                    scheme: Scheme::monomorphic(Type::Abi(abi.clone())),
                                    class: BindingClass::Local,
                                    visibility: BindingVisibility::Private,
                                },
                            );
                            self.abi_call_trackers.push(AbiCallTracker {
                                var_name: var_name_str,
                                abi_name: abi_name_str,
                                first_call_span: None,
                            });

                            let (typed_then, then_traces) =
                                self.infer_block(env, then_branch, ctx, false)?;
                            children.extend(then_traces);

                            self.abi_call_trackers.pop();
                            env.pop_scope();

                            let then_ty = typed_then
                                .tail_expression
                                .as_ref()
                                .map_or(Type::Unit, |tail| tail.node.ty.clone());

                            result_ty = if let Some(current) = result_ty {
                                let (merged, unify_trace) = self.unify(
                                    current.clone(),
                                    then_ty.clone(),
                                    typed_then
                                        .tail_expression
                                        .as_ref()
                                        .map(|expr| expr.span)
                                        .unwrap_or(expr.span),
                                    expr.span,
                                    TypeErrorKind::GeneralMismatch {
                                        expected: current,
                                        found: self.apply_for_display(&then_ty),
                                    },
                                )?;
                                children.push(unify_trace);
                                Some(merged)
                            } else {
                                Some(then_ty)
                            };

                            typed_branches.push((
                                TypedIfCondition::Is {
                                    name: name.clone(),
                                    abi_name: abi_name.clone(),
                                    original_type: var_ty,
                                },
                                typed_then,
                            ));
                            continue;
                        }
                    };

                    let (typed_then, then_traces) =
                        self.infer_block(env, then_branch, ctx, false)?;
                    children.extend(then_traces);

                    let then_ty = typed_then
                        .tail_expression
                        .as_ref()
                        .map_or(Type::Unit, |tail| tail.node.ty.clone());

                    result_ty = if let Some(current) = result_ty {
                        let (merged, unify_trace) = self.unify(
                            current.clone(),
                            then_ty.clone(),
                            typed_then
                                .tail_expression
                                .as_ref()
                                .map(|expr| expr.span)
                                .unwrap_or(expr.span),
                            expr.span,
                            TypeErrorKind::GeneralMismatch {
                                expected: current,
                                found: self.apply_for_display(&then_ty),
                            },
                        )?;
                        children.push(unify_trace);
                        Some(merged)
                    } else {
                        Some(then_ty)
                    };

                    typed_branches.push((typed_condition, typed_then));
                }

                let typed_else_block = if let Some(block) = else_branch {
                    let (typed_block, else_traces) = self.infer_block(env, block, ctx, false)?;
                    children.extend(else_traces);
                    Some(typed_block)
                } else {
                    None
                };

                let (else_ty, else_span) = typed_else_block
                    .as_ref()
                    .and_then(|b| {
                        b.tail_expression
                            .as_ref()
                            .map(|tail| (tail.node.ty.clone(), tail.span))
                    })
                    .unwrap_or((Type::Unit, expr.span));
                let result_ty = if let Some(current) = result_ty {
                    let (merged, unify_trace) = self.unify(
                        current.clone(),
                        else_ty.clone(),
                        else_span,
                        expr.span,
                        TypeErrorKind::GeneralMismatch {
                            expected: current,
                            found: self.apply_for_display(&else_ty),
                        },
                    )?;
                    children.push(unify_trace);
                    merged
                } else {
                    else_ty
                };

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-If",
                    env_context.clone(),
                    subject_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    Spanned::new(
                        TypedExpr {
                            ty: result_ty,
                            kind: TypedExprKind::If {
                                branches: typed_branches,
                                else_branch: typed_else_block.map(Box::new),
                            },
                        },
                        expr.span,
                    ),
                    tree,
                ))
            }
            Expr::Match { scrutinee, arms } => {
                let (typed_scrutinee, scrutinee_trace) = self.infer_expr(env, scrutinee, ctx)?;
                let mut children = vec![scrutinee_trace];
                let mut typed_arms = Vec::with_capacity(arms.len());
                let mut result_ty: Option<(Type, Span)> = None;

                for arm in arms {
                    env.push_scope();
                    let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                        env,
                        &arm.pattern,
                        &typed_scrutinee.node.ty,
                        scrutinee.span,
                    )?;
                    children.append(&mut pattern_traces);

                    let (typed_block, mut block_traces) =
                        self.infer_block(env, &arm.body, ctx, false)?;
                    children.append(&mut block_traces);
                    env.pop_scope();

                    let arm_ty = typed_block
                        .tail_expression
                        .as_ref()
                        .map(|expr| expr.node.ty.clone())
                        .unwrap_or_else(Type::unit);

                    let arm_span = arm
                        .body
                        .tail_expression
                        .as_ref()
                        .map(|e| e.span)
                        .unwrap_or(expr.span);

                    result_ty = if let Some((first_ty, first_span)) = result_ty {
                        let (merged, unify_trace) = self.unify_match_arms(
                            arm_ty.clone(),
                            first_ty.clone(),
                            arm_span,
                            first_span,
                        )?;
                        children.push(unify_trace);
                        Some((merged, first_span))
                    } else {
                        Some((arm_ty, arm_span))
                    };

                    typed_arms.push(TypedMatchArm {
                        pattern: typed_pattern,
                        body: typed_block,
                    });
                }

                // Check exhaustiveness and redundancy
                let scrutinee_ty = self.apply(&typed_scrutinee.node.ty);
                if let Err(exhaustiveness_errors) =
                    super::exhaustiveness::check_match(&scrutinee_ty, &typed_arms, expr.span)
                {
                    // Return the first exhaustiveness error
                    return Err(exhaustiveness_errors.into_iter().next().unwrap());
                }

                let expr_type = result_ty.map(|(ty, _)| ty).unwrap_or_else(Type::unit);
                let typed = Spanned::new(
                    TypedExpr::new(
                        expr_type.clone(),
                        TypedExprKind::Match {
                            scrutinee: Box::new(typed_scrutinee.clone()),
                            arms: typed_arms,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&expr_type));
                let tree =
                    self.make_trace("T-Match", env_context, subject_repr, result_repr, || {
                        children
                    });
                Ok((typed, tree))
            }
            Expr::Yield { abis } => {
                // Assert that we are inside a `main fn`.
                if !ctx.is_coroutine {
                    return Err(TypeError::new(TypeErrorKind::YieldOutsideMainFn, expr.span));
                }
                // TODO: assert that this utxo impls each abi named
                let abis = abis
                    .iter()
                    .map(|abi| Ok(env.root.get_abi(&abi)?.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((
                    Spanned::new(
                        TypedExpr::new(Type::Unit, TypedExprKind::Yield { abis }),
                        expr.span,
                    ),
                    Default::default(),
                ))
            }
            Expr::Disclose { expr: inner_expr } => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner_expr, ctx)?;
                let result_ty = typed_inner.node.ty.clone();

                if self.source_expr_visibility(env, inner_expr) == BindingVisibility::Public {
                    self.warnings.push(
                        TypeWarning::new(TypeWarningKind::UnnecessaryDisclose, expr.span).with_help(
                            "`disclose(...)` is redundant because the wrapped value is already public.",
                        ),
                    );
                }

                let typed = Spanned::new(
                    TypedExpr::new(
                        result_ty.clone(),
                        TypedExprKind::Disclose {
                            expr: Box::new(typed_inner),
                        },
                    ),
                    expr.span,
                );

                let result_repr = self.maybe_string(|| self.format_type(&result_ty));
                let tree =
                    self.make_trace("T-Disclose", env_context, subject_repr, result_repr, || {
                        vec![inner_trace]
                    });
                Ok((typed, tree))
            }
            Expr::Call { callee, args } => {
                self.infer_call(env, ctx, expr, callee, args, FunctionKind::Normal)
            }
            Expr::Emit { callee, args } => {
                self.infer_call(env, ctx, expr, callee, args, FunctionKind::Emit)
            }
            Expr::Raise { callee, args } => {
                self.infer_call(env, ctx, expr, callee, args, FunctionKind::Raise)
            }
            Expr::Runtime { callee, args } => {
                self.infer_call(env, ctx, expr, callee, args, FunctionKind::Runtime)
            }
        }
    }

    fn infer_call(
        &mut self,
        env: &mut TypeEnv,
        ctx: &mut FunctionCtx,
        expr: &Spanned<Expr>,
        callee: &Spanned<Expr>,
        args: &Arguments,
        used_kind: FunctionKind,
    ) -> Result<(Spanned<TypedExpr>, InferenceTree), TypeError> {
        let (typed_callee, callee_trace) = self.infer_expr(env, callee, ctx)?;
        let callee_ty = self.apply_for_display(&typed_callee.node.ty);
        let callee_name = callee.node.name().unwrap_or("<anonymous>");

        let Type::Function(func) = callee_ty else {
            return Err(TypeError::new(
                TypeErrorKind::NotAFunction { found: callee_ty },
                callee.span,
            ));
        };

        // Check kind: `event` requires `emit`; `effect` requires `raise`; runtime fns require `runtime`
        if used_kind != func.kind {
            if used_kind == FunctionKind::Normal {
                // Called without keyword, a keyword is required
                return Err(TypeError::new(
                    TypeErrorKind::EmitRaiseRuntimeNeeded {
                        function_name: callee_name.to_owned(),
                        needed_keyword: func.kind,
                    },
                    callee.span,
                ));
            } else if func.kind == FunctionKind::Normal {
                // Called with a keyword, but the function is normal
                return Err(TypeError::new(
                    TypeErrorKind::EmitRaiseRuntimeUnneeded {
                        function_name: callee_name.to_owned(),
                        unneeded_keyword: used_kind,
                    },
                    callee.span,
                ));
            } else {
                // Total mismatch
                return Err(TypeError::new(
                    TypeErrorKind::EmitRaiseRuntimeMismatch {
                        function_name: callee_name.to_owned(),
                        needed_keyword: func.kind,
                        wrong_keyword: used_kind,
                    },
                    callee.span,
                ));
            }
        }

        // Check linearity: if the callee is a field access on an AbiNarrow target,
        // enforce the one-method-call-per-block constraint.
        if let TypedExprKind::FieldAccess { target, .. } = &typed_callee.node.kind
            && let Type::Abi(_) = &target.node.ty
            && let TypedExprKind::ScopedName { name, .. } = &target.node.kind
            && name.len() == 1
        {
            for tracker in self.abi_call_trackers.iter_mut().rev() {
                if tracker.var_name == name[0].as_str() {
                    if let Some(first_span) = tracker.first_call_span {
                        return Err(TypeError::new(
                            TypeErrorKind::LinearMethodCallViolation {
                                var_name: tracker.var_name.clone(),
                                abi_name: tracker.abi_name.clone(),
                            },
                            expr.span,
                        )
                        .with_secondary(first_span, "first method call here"));
                    }
                    tracker.first_call_span = Some(expr.span);
                    break;
                }
            }
        }

        if args.len() != func.params.len() {
            return Err(TypeError::new(
                TypeErrorKind::ArityMismatch {
                    expected: func.params.len(),
                    found: args.len(),
                },
                expr.span,
            ));
        }

        let mut children = vec![callee_trace];
        let mut typed_args = Vec::with_capacity(args.len());

        for (index, (arg, expected_ty)) in args.iter().zip(func.params.iter()).enumerate() {
            let (typed_arg, arg_trace) = self.infer_expr(env, arg, ctx)?;
            let actual_ty = typed_arg.node.ty.clone();

            let param_span = func.param_spans.get(index).copied();

            let (_, unify_trace) = self.unify(
                actual_ty.clone(),
                expected_ty.clone(),
                arg.span,
                arg.span,
                TypeErrorKind::ArgumentTypeMismatch {
                    expected: expected_ty.clone(),
                    found: self.apply_for_display(&actual_ty),
                    position: index + 1,
                    param_span,
                },
            )?;

            children.push(arg_trace);
            children.push(unify_trace);
            typed_args.push(typed_arg);
        }

        let typed = Spanned::new(
            TypedExpr::new(
                func.result.clone(),
                TypedExprKind::Call {
                    callee: Box::new(typed_callee),
                    args: typed_args,
                },
            ),
            callee.span,
        );

        let result_repr = self.maybe_string(|| self.format_type(&func.result));

        let env_context = self.maybe_string(|| self.format_env(env));
        let subject_repr = self.maybe_string(|| self.format_expr_src(expr));
        let tree = self.make_trace("T-Call", env_context, subject_repr, result_repr, || {
            children
        });

        Ok((typed, tree))
    }

    fn private_parameter_rhs_name(
        &self,
        env: &TypeEnv,
        expr: &Spanned<Expr>,
        ctx: &FunctionCtx,
    ) -> Option<String> {
        let Expr::ScopedName(name) = &expr.node else {
            return None;
        };
        let [solo] = &name[..] else { return None };
        let binding = env.get(solo.as_str())?;
        if ctx.private_param_decl_spans.contains(&binding.decl_span) {
            Some(solo.to_string())
        } else {
            None
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn source_expr_visibility(&self, env: &TypeEnv, expr: &Spanned<Expr>) -> BindingVisibility {
        match &expr.node {
            Expr::Literal(_) => BindingVisibility::Public,
            Expr::ScopedName(name) => {
                if name.len() == 1 {
                    env.get(name[0].as_str())
                        .map(|binding| binding.visibility)
                        .unwrap_or(BindingVisibility::Private)
                } else {
                    BindingVisibility::Private
                }
            }
            Expr::Unary { expr, .. } | Expr::Grouping(expr) => {
                self.source_expr_visibility(env, expr)
            }
            Expr::Binary { left, right, .. } => {
                if self.source_expr_visibility(env, left) == BindingVisibility::Public
                    && self.source_expr_visibility(env, right) == BindingVisibility::Public
                {
                    BindingVisibility::Public
                } else {
                    BindingVisibility::Private
                }
            }
            Expr::StructConstructor { fields, .. } => {
                if fields.iter().all(|field| {
                    self.source_expr_visibility(env, &field.value) == BindingVisibility::Public
                }) {
                    BindingVisibility::Public
                } else {
                    BindingVisibility::Private
                }
            }
            Expr::FieldAccess { target, .. } => self.source_expr_visibility(env, target),
            Expr::Disclose { .. } => BindingVisibility::Public,
            Expr::Call { .. } => BindingVisibility::Private,
            Expr::Block(_)
            | Expr::If { .. }
            | Expr::Match { .. }
            | Expr::Emit { .. }
            | Expr::Raise { .. }
            | Expr::Runtime { .. }
            | Expr::Yield { .. } => BindingVisibility::Private,
        }
    }

    fn type_from_annotation(
        &mut self,
        env: &TypeEnv,
        annotation: &TypeAnnotation,
    ) -> Result<Type, TypeError> {
        // Resolve each generic arg (done first for lifetime reasons).
        let type_args: Vec<Type> = annotation
            .generics
            .iter()
            .map(|g| self.type_from_annotation(env, g))
            .collect::<Result<_, _>>()?;

        // Look up type information.
        let (last, path) = annotation.name.split_last().unwrap();
        let ns = env.root.get_child(path)?;
        let Some(entry) = ns.types.get(last.as_str()) else {
            return Err(TypeError::new(
                TypeErrorKind::UnknownTypeAnnotation {
                    name: last.to_string(),
                },
                annotation.name_span(),
            ));
        };

        // Check generic arity match.
        if entry.type_params.len() != annotation.generics.len() {
            return Err(TypeError::new(
                TypeErrorKind::WrongGenericArity {
                    type_name: last.to_string(),
                    expected: entry.type_params.len(),
                    found: annotation.generics.len(),
                },
                annotation.name_span(),
            ));
        }

        // Apply generics if needed.
        if entry.type_params.is_empty() {
            Ok(entry.ty.clone())
        } else {
            Ok(Self::apply_type_args(
                &entry.ty,
                &entry.type_params,
                &type_args,
            ))
        }
    }

    /// Ensure a type is boolean, emitting a trace entry describing the check.
    fn require_bool(
        &mut self,
        ty: &Type,
        span: Span,
        context: ConditionContext,
    ) -> Result<InferenceTree, TypeError> {
        let applied = self.apply_for_display(ty);
        if matches!(&applied, Type::Bool) {
            let subject = self.maybe_string(|| self.format_type(&applied));
            Ok(self.make_trace(
                "Check-Bool",
                None,
                subject,
                Some("ok".to_string()),
                Vec::new,
            ))
        } else {
            Err(TypeError::new(
                TypeErrorKind::ConditionNotBool {
                    context,
                    found: applied,
                },
                span,
            ))
        }
    }

    /// Unify two types, capturing the resulting unification step in the trace.
    fn require_is(
        &mut self,
        actual: &Type,
        expected: Type,
        left_span: Span,
        right_span: Span,
        kind: TypeErrorKind,
    ) -> Result<InferenceTree, TypeError> {
        let (_, tree) = self.unify(actual.clone(), expected, left_span, right_span, kind)?;
        Ok(tree)
    }

    /// Ensure comparison operands are either both integers or both booleans.
    fn require_numeric_or_bool_pair(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        let both_int = self.is_int_like(&left_ty) && self.is_int_like(&right_ty);

        if both_int {
            // Unify to ensure same int width
            let (unified_ty, unify_trace) = self.unify(
                left_ty.clone(),
                right_ty.clone(),
                left_span,
                right_span,
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: self.apply_for_display(&left.node.ty),
                    right: self.apply_for_display(&right.node.ty),
                },
            )?;
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&unified_ty)
                )
            });
            let result = self.maybe_string(|| "ok (int)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, || vec![unify_trace]))
        } else if matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok (bool)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, Vec::new))
        } else {
            let left_display = self.apply_for_display(&left.node.ty);
            let right_display = self.apply_for_display(&right.node.ty);
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_display.clone(),
                    right: right_display.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{}`", left_display.compact_display()))
            .with_secondary(
                right_span,
                format!("has type `{}`", right_display.compact_display()),
            ))
        }
    }

    /// Ensure equality operands share the same primitive type.
    fn require_same_primitive(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        let both_int = self.is_int_like(&left_ty) && self.is_int_like(&right_ty);

        if both_int {
            // Unify to ensure same int width
            let (unified_ty, unify_trace) = self.unify(
                left_ty.clone(),
                right_ty.clone(),
                left_span,
                right_span,
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: self.apply_for_display(&left.node.ty),
                    right: self.apply_for_display(&right.node.ty),
                },
            )?;
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&unified_ty)
                )
            });
            let result = self.maybe_string(|| "ok".to_string());
            Ok(self.make_trace("Check-Eq", None, subject, result, || vec![unify_trace]))
        } else if matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok".to_string());
            Ok(self.make_trace("Check-Eq", None, subject, result, Vec::new))
        } else {
            let left_display = self.apply_for_display(&left.node.ty);
            let right_display = self.apply_for_display(&right.node.ty);
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_display.clone(),
                    right: right_display.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{}`", left_display.compact_display()))
            .with_secondary(
                right_span,
                format!("has type `{}`", right_display.compact_display()),
            ))
        }
    }

    /// Normalize a type for use in user-facing error messages.
    ///
    /// Like [`apply`], but also defaults unresolved int-constrained type
    /// variables to `i64` so that error messages show a concrete type name
    /// instead of an internal type variable like `t3`.
    fn apply_for_display(&self, ty: &Type) -> Type {
        substitute_type(ty, &self.subst, &self.int_vars)
    }

    /// Fully normalize a type by applying the current substitution set.
    fn apply(&self, ty: &Type) -> Type {
        substitute_type(ty, &self.subst, &Default::default())
    }

    /// Rewrite every definition in the program with normalized types.
    fn apply_substitutions_program(&self, program: &mut TypedProgram) {
        for definition in &mut program.definitions {
            self.apply_definition(definition);
        }
    }

    fn apply_definition(&self, definition: &mut TypedDefinition) {
        match definition {
            TypedDefinition::Function(function) => self.apply_function(function),
            TypedDefinition::Utxo(utxo) => self.apply_utxo(utxo),
            TypedDefinition::Token(token) => self.apply_token(token),
            TypedDefinition::Import(_)
            | TypedDefinition::Struct(_)
            | TypedDefinition::Enum(_)
            | TypedDefinition::Abi(_)
            | TypedDefinition::Contract => {}
        }
    }

    fn apply_token(&self, token: &mut TypedTokenDef) {
        token.ty = self.apply(&token.ty);
        for part in &mut token.parts {
            match part {
                TypedTokenPart::Storage(vars) => {
                    for var in vars {
                        var.ty = self.apply(&var.ty);
                    }
                }
                TypedTokenPart::Function(func) => {
                    self.apply_function(func);
                }
                TypedTokenPart::AbiImpl {
                    abi,
                    span: _,
                    parts,
                } => {
                    *abi = self.apply(abi);
                    for part in parts {
                        self.apply_function(part);
                    }
                }
            }
        }
    }

    fn apply_utxo(&self, utxo: &mut TypedUtxoDef) {
        for part in &mut utxo.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    for var in vars {
                        var.ty = self.apply(&var.ty);
                    }
                }
                TypedUtxoPart::Function(func) => {
                    self.apply_function(func);
                }
                TypedUtxoPart::AbiImpl {
                    abi,
                    span: _,
                    parts,
                } => {
                    let Type::Abi(new_abi) = self.apply(&Type::Abi(abi.clone())) else {
                        unreachable!()
                    };
                    *abi = new_abi;
                    for part in parts {
                        self.apply_function(part);
                    }
                }
            }
        }
    }

    fn apply_function(&self, function: &mut TypedFunctionDef) {
        function.return_type = self.apply(&function.return_type);
        for param in &mut function.params {
            param.ty = self.apply(&param.ty);
        }
        self.apply_block(&mut function.body);
    }

    /// Visit a single statement and normalize any embedded type annotations.
    fn apply_statement(&self, statement: &mut TypedStatement) {
        match statement {
            TypedStatement::VariableDeclaration { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::Assignment { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::While { condition, body } => {
                self.apply_expr(condition);
                self.apply_block(body);
            }
            TypedStatement::Expression(expr) => self.apply_expr(expr),
            TypedStatement::Return(Some(expr)) => self.apply_expr(expr),
            TypedStatement::Return(None) => {}
            TypedStatement::Resume => {}
            TypedStatement::TryWith { subject, effects } => {
                self.apply_block(subject);
                for (_, _, block) in effects {
                    self.apply_block(block);
                }
            }
        }
    }

    /// Visit each statement inside a block and normalize its annotations.
    fn apply_block(&self, block: &mut TypedBlock) {
        for statement in &mut block.statements {
            self.apply_statement(statement);
        }
        if let Some(expr) = &mut block.tail_expression {
            self.apply_expr(expr);
        }
    }

    /// Normalize the type attached to an expression and recursively visit its children.
    fn apply_expr(&self, expr: &mut Spanned<TypedExpr>) {
        expr.node.ty = self.apply(&expr.node.ty);
        match &mut expr.node.kind {
            TypedExprKind::Literal(_) | TypedExprKind::ScopedName { .. } => {}
            TypedExprKind::Unary { expr: inner, .. } => self.apply_expr(inner),
            TypedExprKind::Binary { left, right, .. } => {
                self.apply_expr(left);
                self.apply_expr(right);
            }
            TypedExprKind::Grouping(inner) => self.apply_expr(inner),
            TypedExprKind::StructConstructor { fields, .. } => {
                for field in fields {
                    self.apply_expr(&mut field.value);
                }
            }
            TypedExprKind::FieldAccess { target, .. } => self.apply_expr(target),
            TypedExprKind::Block(block) => self.apply_block(block),
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                for (condition, then_branch) in branches {
                    match condition {
                        TypedIfCondition::Bool(expr) => self.apply_expr(expr),
                        TypedIfCondition::Is { original_type, .. } => {
                            *original_type = self.apply(original_type);
                        }
                    }
                    self.apply_block(then_branch);
                }
                if let Some(block) = else_branch {
                    self.apply_block(block);
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.apply_expr(scrutinee);
                for arm in arms {
                    self.apply_block(&mut arm.body);
                }
            }
            TypedExprKind::Yield { abis: _ } => {}
            TypedExprKind::Call { callee, args } => {
                self.apply_expr(callee);

                for arg in args {
                    self.apply_expr(arg);
                }
            }
            TypedExprKind::Disclose { expr } => self.apply_expr(expr),
            TypedExprKind::Emit { callee, args } => {
                self.apply_expr(callee);
                for arg in args {
                    self.apply_expr(arg);
                }
            }
            TypedExprKind::Raise { callee, args } => {
                self.apply_expr(callee);
                for arg in args {
                    self.apply_expr(arg);
                }
            }
            TypedExprKind::Runtime { callee, args } => {
                self.apply_expr(callee);
                for arg in args {
                    self.apply_expr(arg);
                }
            }
        }
    }

    /// Quantify over all type variables that are free in `ty` but not in the environment.
    fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
        let applied = self.apply(ty);
        let mut ty_free = free_type_vars_type(&applied);
        let env_free = env.free_type_vars();
        // Don't quantify int-constrained vars — they should stay monomorphic
        // so that all uses share the same int type variable.
        ty_free.retain(|var| !env_free.contains(var) && !self.int_vars.contains(var));
        let mut vars: Vec<_> = ty_free.into_iter().collect();
        vars.sort();
        Scheme { vars, ty: applied }
    }

    /// Replace every quantified variable in `scheme` with a fresh type variable.
    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut mapping = HashMap::new();
        for var in &scheme.vars {
            mapping.insert(*var, self.fresh_var());
        }
        substitute_type(&scheme.ty, &mapping, &Default::default())
    }

    /// Allocate a new inference variable unique to this inferencer.
    fn fresh_var_id(&mut self) -> TypeVarId {
        self.next_type_var.fresh()
    }

    fn fresh_var(&mut self) -> Type {
        Type::Var(self.fresh_var_id())
    }

    /// Create a fresh type variable constrained to integer types.
    /// Used for polymorphic integer literals.
    fn fresh_int_var(&mut self) -> Type {
        let id = self.fresh_var_id();
        self.int_vars.insert(id);
        Type::Var(id)
    }

    /// Default any unresolved integer type variables to `i64`.
    fn default_int_vars(&mut self) {
        for &id in &self.int_vars {
            let resolved = self.apply(&Type::Var(id));
            if matches!(resolved, Type::Var(_)) {
                self.subst.insert(id, Type::int());
            }
        }
    }

    /// Check that all integer literals fit within the range of their resolved type.
    fn check_int_literal_ranges(&self) -> Result<(), Vec<TypeError>> {
        let mut errors = Vec::new();
        for (&id, (literal, span)) in &self.int_literal_values {
            let resolved = self.apply(&Type::Var(id));
            if let Type::Int(w) = resolved
                // `value()` is `None` when the digits overflow `i128`, which no
                // supported integer type can hold either.
                && !literal.value().is_some_and(|value| w.fits(value))
            {
                errors.push(TypeError::new(
                    TypeErrorKind::LiteralOutOfRange {
                        literal: literal.to_string(),
                        ty: Type::Int(w),
                    },
                    *span,
                ));
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Render the current environment snapshot into a deterministic string.
    /// Resolves int-constrained type vars so traces show concrete types.
    fn format_env(&self, env: &TypeEnv) -> String {
        let snapshot = env.snapshot();
        if snapshot.is_empty() {
            "{}".to_string()
        } else {
            let entries = snapshot
                .iter()
                .map(|(name, scheme)| {
                    let display_ty = self.apply_for_display(&scheme.ty);
                    format!("{name}: {}", display_ty.compact_display())
                })
                .collect::<Vec<_>>();
            format!("{{{}}}", entries.join(", "))
        }
    }

    /// Pretty-print a statement for inclusion in the inference trace.
    fn format_statement_src(&self, statement: &Statement) -> String {
        formatter::statement(statement)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{statement:?}"))
    }

    /// Pretty-print an expression for inclusion in the inference trace.
    fn format_expr_src(&self, expr: &Spanned<Expr>) -> String {
        formatter::expression(&expr.node)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{:?}", expr.node))
    }

    /// Compute a string lazily, only when capture_traces is enabled.
    fn maybe_string<F>(&self, f: F) -> Option<String>
    where
        F: FnOnce() -> String,
    {
        if self.capture_traces { Some(f()) } else { None }
    }

    /// Assemble an inference tree node when tracing is active; otherwise return the default node.
    fn make_trace<C>(
        &self,
        rule: &str,
        context: Option<String>,
        subject: Option<String>,
        result: Option<String>,
        children: C,
    ) -> InferenceTree
    where
        C: FnOnce() -> Vec<InferenceTree>,
    {
        if !self.capture_traces {
            return InferenceTree::default();
        }

        let context = context.unwrap_or_default();
        let subject = subject.unwrap_or_default();
        let result = result.unwrap_or_default();
        let mut tree = InferenceTree::new(rule, context, subject, result);
        let kids = children();
        if !kids.is_empty() {
            tree = tree.with_children(kids);
        }
        tree
    }

    /// Produce a span sized to the formatted expression, avoiding trailing whitespace.
    fn label_span_for_expr(&self, expr: &Spanned<Expr>) -> Span {
        let formatted = self.format_expr_src(expr);
        let max_len = expr.span.end.saturating_sub(expr.span.start);
        let new_end = expr.span.start + formatted.len().min(max_len);
        Span {
            start: expr.span.start,
            end: new_end,
            context: expr.span.context,
        }
    }

    /// Short helper to format a type compactly for traces and error messages.
    /// Resolves int-constrained type vars to their default (`i64`) so that
    /// traces and errors never expose internal variable names like `t3`.
    fn format_type(&self, ty: &Type) -> String {
        self.apply_for_display(ty).compact_display().to_string()
    }

    /// Format the difference between the stored substitution map and a prior snapshot.
    fn format_subst_diff(&self, before: &HashMap<TypeVarId, Type>) -> String {
        let mut entries = Vec::new();
        for (key, value) in &self.subst {
            if before.get(key) != Some(value) {
                entries.push(format!("{}/{}", self.format_type(value), key));
            }
        }
        entries.sort();
        format!("{{{}}}", entries.join(", "))
    }

    /// Unify match arm types with custom error labels.
    ///
    /// The `current_arm` is the arm being checked, and `first_arm` is the first arm
    /// that established the expected type. On error, the primary label is on the
    /// current arm, and the secondary label explains that the first arm set the expectation.
    fn unify_match_arms(
        &mut self,
        current_arm_ty: Type,
        first_arm_ty: Type,
        current_arm_span: Span,
        first_arm_span: Span,
    ) -> Result<(Type, InferenceTree), TypeError> {
        let current_ty = self.apply(&current_arm_ty);
        let first_ty = self.apply(&first_arm_ty);

        match self.unify_inner(current_ty.clone(), first_ty.clone()) {
            Ok((result_ty, children, rule)) => {
                let subject = self.maybe_string(|| {
                    format!(
                        "{} ~ {}",
                        self.format_type(&current_ty),
                        self.format_type(&first_ty)
                    )
                });
                let result_repr = self.maybe_string(|| self.format_type(&result_ty));
                let tree = self.make_trace(rule, None, subject, result_repr, || children);
                Ok((result_ty, tree))
            }
            Err(_) => {
                let current_repr = self.format_type(&current_ty);
                let first_repr = self.format_type(&first_ty);
                let err = TypeError::new(
                    TypeErrorKind::GeneralMismatch {
                        expected: first_ty,
                        found: current_ty,
                    },
                    current_arm_span,
                )
                .with_primary_message(format!("has type `{current_repr}`"))
                .with_secondary(
                    first_arm_span,
                    format!("expected `{first_repr}` due to this"),
                );
                Err(err)
            }
        }
    }

    /// Inner unification logic that returns the result without creating error labels.
    fn unify_inner(
        &mut self,
        left: Type,
        right: Type,
    ) -> Result<(Type, Vec<InferenceTree>, &'static str), ()> {
        match (left, right) {
            (Type::Unit, Type::Unit) => Ok((Type::Unit, Vec::new(), "Unify-Const")),
            (Type::Bool, Type::Bool) => Ok((Type::Bool, Vec::new(), "Unify-Const")),
            (Type::Int(w1), Type::Int(w2)) if w1 == w2 => {
                Ok((Type::Int(w1), Vec::new(), "Unify-Const"))
            }
            (Type::Function(left), Type::Function(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                Ok((Type::Function(left), vec![], "Unify-Const"))
            }
            (Type::Function(left), Type::Function(right))
                if left.params.len() == right.params.len() && left.kind == right.kind =>
            {
                let mut children = Vec::new();
                for (l, r) in left.params.iter().zip(right.params.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                let (_, ret_child, _) =
                    self.unify_inner(left.result.clone(), right.result.clone())?;
                children.extend(ret_child);
                Ok((
                    Type::from(FunctionType {
                        params: left.params.clone(),
                        param_spans: left.param_spans.clone(),
                        result: left.result.clone(),
                        kind: left.kind,
                        name_span: left.name_span,
                        callee: if left.callee == right.callee {
                            left.callee.clone()
                        } else {
                            None
                        },
                    }),
                    children,
                    "Unify-Arrow",
                ))
            }
            (Type::Tuple(left), Type::Tuple(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                Ok((Type::Tuple(left), vec![], "Unify-Const"))
            }
            (Type::Tuple(ls), Type::Tuple(rs)) if ls.len() == rs.len() => {
                let mut children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                Ok((Type::Tuple(ls), children, "Unify-Tuple"))
            }
            // Records unify structurally: names are aliases, but fields must
            // line up in declaration order, matching `unify` below.
            (Type::Record(left), Type::Record(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                Ok((Type::Record(left), vec![], "Unify-Const"))
            }
            (Type::Record(ls), Type::Record(rs)) if ls.fields.len() == rs.fields.len() => {
                let mut children = Vec::new();
                for (lf, rf) in ls.fields.iter().zip(rs.fields.iter()) {
                    if lf.name.as_str() != rf.name.as_str() {
                        return Err(());
                    }
                    let (_, child, _) = self.unify_inner(lf.ty.clone(), rf.ty.clone())?;
                    children.extend(child);
                }
                Ok((Type::Record(ls), children, "Unify-Record"))
            }
            // Enums likewise unify by shape, not name, with variants compared
            // in declaration order.
            (Type::Enum(left), Type::Enum(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                Ok((Type::Enum(left), vec![], "Unify-Const"))
            }
            (Type::Enum(ls), Type::Enum(rs)) if ls.variants.len() == rs.variants.len() => {
                let mut children = Vec::new();
                for (lv, rv) in ls.variants.iter().zip(rs.variants.iter()) {
                    if lv.name != rv.name {
                        return Err(());
                    }
                    match (&lv.kind, &rv.kind) {
                        (EnumVariantKind::Unit, EnumVariantKind::Unit) => {}
                        (EnumVariantKind::Tuple(lt), EnumVariantKind::Tuple(rt))
                            if lt.len() == rt.len() =>
                        {
                            for (l, r) in lt.iter().zip(rt.iter()) {
                                let (_, c, _) = self.unify_inner(l.clone(), r.clone())?;
                                children.extend(c);
                            }
                        }
                        (EnumVariantKind::Struct(lf), EnumVariantKind::Struct(rf))
                            if lf.len() == rf.len() =>
                        {
                            for (l, r) in lf.iter().zip(rf.iter()) {
                                if l.name.as_str() != r.name.as_str() {
                                    return Err(());
                                }
                                let (_, c, _) = self.unify_inner(l.ty.clone(), r.ty.clone())?;
                                children.extend(c);
                            }
                        }
                        _ => return Err(()),
                    }
                }
                Ok((Type::Enum(ls), children, "Unify-Enum"))
            }
            // Utxo and Token types are nominal and only unify with themselves.
            (Type::Utxo(left), Type::Utxo(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                Ok((Type::Utxo(left), Vec::new(), "Unify-Const"))
            }
            (Type::Token(left), Type::Token(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                Ok((Type::Token(left), Vec::new(), "Unify-Const"))
            }
            // TODO: structural unification for Abi types
            (Type::Abi(left), Type::Abi(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                Ok((Type::Abi(left), Vec::new(), "Unify-Const"))
            }
            (Type::Var(id), ty) => {
                if ty == Type::Var(id) {
                    return Ok((ty, Vec::new(), "Unify-Var"));
                }
                if occurs_in(id, &ty, &self.subst) {
                    return Err(());
                }
                // If this var is int-constrained, verify the target is an int type
                // or propagate the constraint to another var.
                if self.int_vars.contains(&id) {
                    match &ty {
                        Type::Int(_) => {} // OK
                        Type::Var(other_id) => {
                            // Propagate int constraint to the other var
                            self.int_vars.insert(*other_id);
                            // Also propagate literal value tracking if present
                            if let Some(val) = self.int_literal_values.get(&id).cloned() {
                                self.int_literal_values.entry(*other_id).or_insert(val);
                            }
                        }
                        _ => return Err(()),
                    }
                }
                self.subst.insert(id, ty.clone());
                Ok((ty, Vec::new(), "Unify-Var"))
            }
            (ty, Type::Var(id)) => {
                if ty == Type::Var(id) {
                    return Ok((ty, Vec::new(), "Unify-Var"));
                }
                if occurs_in(id, &ty, &self.subst) {
                    return Err(());
                }
                // If this var is int-constrained, verify the target is an int type
                // or propagate the constraint to another var.
                if self.int_vars.contains(&id) {
                    match &ty {
                        Type::Int(_) => {} // OK
                        Type::Var(other_id) => {
                            self.int_vars.insert(*other_id);
                            if let Some(val) = self.int_literal_values.get(&id).cloned() {
                                self.int_literal_values.entry(*other_id).or_insert(val);
                            }
                        }
                        _ => return Err(()),
                    }
                }
                self.subst.insert(id, ty.clone());
                Ok((ty, Vec::new(), "Unify-Var"))
            }
            _ => Err(()),
        }
    }

    /// Unify two types, updating the substitution set and returning a trace node.
    fn unify(
        &mut self,
        left: Type,
        right: Type,
        left_span: Span,
        right_span: Span,
        error_kind: TypeErrorKind,
    ) -> Result<(Type, InferenceTree), TypeError> {
        let left = self.apply(&left);
        let right = self.apply(&right);
        let subject = self
            .maybe_string(|| format!("{} ~ {}", self.format_type(&left), self.format_type(&right)));
        let before = if self.capture_traces {
            Some(self.subst.clone())
        } else {
            None
        };

        let (result_ty, children, rule) = match (left, right) {
            (Type::Unit, Type::Unit) => (Type::Unit, Vec::new(), "Unify-Const"),
            (Type::Bool, Type::Bool) => (Type::Bool, Vec::new(), "Unify-Const"),
            (Type::Int(w1), Type::Int(w2)) if w1 == w2 => {
                (Type::Int(w1), Vec::new(), "Unify-Const")
            }
            (Type::Function(left), Type::Function(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                (Type::Function(left), Vec::new(), "Unify-Const")
            }
            (Type::Function(left), Type::Function(right)) => {
                if left.params.len() != right.params.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "function arity mismatch"));
                }
                if left.kind != right.kind {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "function kind mismatch"));
                }

                let mut arrow_children = Vec::new();
                for (l, r) in left.params.iter().zip(right.params.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    arrow_children.push(child);
                }

                let (_, ret_child) = self.unify(
                    left.result.clone(),
                    right.result.clone(),
                    left_span,
                    right_span,
                    TypeErrorKind::GeneralMismatch {
                        expected: left.result.clone(),
                        found: right.result.clone(),
                    },
                )?;
                arrow_children.push(ret_child);

                (
                    Type::from(FunctionType {
                        params: left.params.clone(),
                        param_spans: left.param_spans.clone(),
                        result: left.result.clone(),
                        kind: left.kind,
                        name_span: left.name_span,
                        callee: if left.callee == right.callee {
                            left.callee.clone()
                        } else {
                            None
                        },
                    }),
                    arrow_children,
                    "Unify-Arrow",
                )
            }
            (Type::Tuple(left), Type::Tuple(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                (Type::Tuple(left), Vec::new(), "Unify-Const")
            }
            (Type::Tuple(ls), Type::Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "type mismatch"));
                }

                let mut tuple_children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    tuple_children.push(child);
                }
                (Type::Tuple(ls), tuple_children, "Unify-Tuple")
            }
            (Type::Record(left), Type::Record(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                (Type::Record(left), Vec::new(), "Unify-Const")
            }
            (Type::Record(ls), Type::Record(rs)) => {
                if ls.fields.len() != rs.fields.len()
                    || ls
                        .fields
                        .iter()
                        .zip(rs.fields.iter())
                        .any(|(l, r)| l.name.as_str() != r.name.as_str())
                {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "struct field mismatch"));
                }

                let mut record_children = Vec::new();
                for (left_field, right_field) in ls.fields.iter().zip(rs.fields.iter()) {
                    let (_, trace) = self.unify(
                        left_field.ty.clone(),
                        right_field.ty.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: left_field.ty.clone(),
                            found: right_field.ty.clone(),
                        },
                    )?;
                    record_children.push(trace);
                }
                (Type::Record(ls), record_children, "Unify-Record")
            }
            (Type::Enum(left), Type::Enum(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                (Type::Enum(left), Vec::new(), "Unify-Const")
            }
            (Type::Enum(ls), Type::Enum(rs)) => {
                if ls.variants.len() != rs.variants.len()
                    || ls
                        .variants
                        .iter()
                        .zip(rs.variants.iter())
                        .any(|(l, r)| l.name != r.name)
                {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "enum variant mismatch"));
                }

                let mut enum_children = Vec::new();
                for (left_variant, right_variant) in ls.variants.iter().zip(rs.variants.iter()) {
                    match (&left_variant.kind, &right_variant.kind) {
                        (EnumVariantKind::Unit, EnumVariantKind::Unit) => {}
                        (
                            EnumVariantKind::Tuple(left_payload),
                            EnumVariantKind::Tuple(right_payload),
                        ) => {
                            if left_payload.len() != right_payload.len() {
                                return Err(TypeError::new(error_kind.clone(), left_span)
                                    .with_secondary(right_span, "enum payload mismatch"));
                            }
                            for (left_ty, right_ty) in left_payload.iter().zip(right_payload.iter())
                            {
                                let (_, trace) = self.unify(
                                    left_ty.clone(),
                                    right_ty.clone(),
                                    left_span,
                                    right_span,
                                    TypeErrorKind::GeneralMismatch {
                                        expected: left_ty.clone(),
                                        found: right_ty.clone(),
                                    },
                                )?;
                                enum_children.push(trace);
                            }
                        }
                        (
                            EnumVariantKind::Struct(left_fields),
                            EnumVariantKind::Struct(right_fields),
                        ) => {
                            if left_fields.len() != right_fields.len()
                                || left_fields
                                    .iter()
                                    .zip(right_fields.iter())
                                    .any(|(l, r)| l.name.as_str() != r.name.as_str())
                            {
                                return Err(TypeError::new(error_kind.clone(), left_span)
                                    .with_secondary(right_span, "enum payload mismatch"));
                            }

                            for (left_field, right_field) in
                                left_fields.iter().zip(right_fields.iter())
                            {
                                let (_, trace) = self.unify(
                                    left_field.ty.clone(),
                                    right_field.ty.clone(),
                                    left_span,
                                    right_span,
                                    TypeErrorKind::GeneralMismatch {
                                        expected: left_field.ty.clone(),
                                        found: right_field.ty.clone(),
                                    },
                                )?;
                                enum_children.push(trace);
                            }
                        }
                        _ => {
                            return Err(TypeError::new(error_kind.clone(), left_span)
                                .with_secondary(right_span, "enum payload mismatch"));
                        }
                    }
                }
                (Type::Enum(ls), enum_children, "Unify-Enum")
            }
            (Type::Utxo(left), Type::Utxo(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                (Type::Utxo(left), Vec::new(), "Unify-Const")
            }
            (Type::Token(left), Type::Token(right))
                if Arc::as_ptr(&left) == Arc::as_ptr(&right) =>
            {
                (Type::Token(left), Vec::new(), "Unify-Const")
            }
            (Type::Abi(left), Type::Abi(right)) if Arc::as_ptr(&left) == Arc::as_ptr(&right) => {
                (Type::Abi(left), Vec::new(), "Unify-Const")
            }
            (Type::Var(id), ty) => {
                self.bind(id, ty.clone(), left_span, right_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            (ty, Type::Var(id)) => {
                self.bind(id, ty.clone(), right_span, left_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            (left, right) => {
                let mut err = TypeError::new(error_kind, left_span);
                if left_span != right_span {
                    let left_repr = self.format_type(&left);
                    let right_repr = self.format_type(&right);
                    err = err
                        .with_primary_message(format!("has type `{left_repr}`"))
                        .with_secondary(right_span, format!("has type `{right_repr}`"));
                }
                return Err(err);
            }
        };

        let result_repr = if let Some(before) = before.as_ref() {
            self.maybe_string(|| self.format_subst_diff(before))
        } else {
            None
        };
        let tree = self.make_trace(rule, None, subject, result_repr, || children);
        Ok((result_ty, tree))
    }

    /// Returns `true` if `ty` is either a concrete `Type::Int(_)` or an int-constrained type variable.
    fn is_int_like(&self, ty: &Type) -> bool {
        match ty {
            Type::Int(_) => true,
            Type::Var(id) => self.int_vars.contains(id),
            _ => false,
        }
    }

    fn bind(
        &mut self,
        var: TypeVarId,
        ty: Type,
        var_span: Span,
        other_span: Span,
        kind: TypeErrorKind,
    ) -> Result<(), TypeError> {
        if ty == Type::Var(var) {
            return Ok(());
        }

        if occurs_in(var, &ty, &self.subst) {
            return Err(TypeError::new(kind, var_span)
                .with_secondary(other_span, "would create an infinite type"));
        }

        // If this var is int-constrained, verify the target is an int type
        // or propagate the constraint to another var.
        if self.int_vars.contains(&var) {
            match &ty {
                Type::Int(_) => {} // OK — concrete int type
                Type::Var(other_id) => {
                    // Propagate int constraint to the other var
                    self.int_vars.insert(*other_id);
                    // Also propagate literal value tracking if present
                    if let Some(val) = self.int_literal_values.get(&var).cloned() {
                        self.int_literal_values.entry(*other_id).or_insert(val);
                    }
                }
                _ => {
                    return Err(TypeError::new(kind, var_span)
                        .with_secondary(other_span, "expected an integer type"));
                }
            }
        }

        self.subst.insert(var, ty);
        Ok(())
    }
}

/// Recursively replace any variables mentioned in `mapping` within `ty`.
fn substitute_type(
    ty: &Type,
    mapping: &HashMap<TypeVarId, Type>,
    int_vars: &HashSet<TypeVarId>,
) -> Type {
    match ty {
        Type::Var(id) => match mapping.get(id) {
            Some(ty) => substitute_type(ty, mapping, int_vars),
            None if int_vars.contains(id) => Type::int(),
            None => Type::Var(*id),
        },
        Type::Function(func) => Type::from(FunctionType {
            params: func
                .params
                .iter()
                .map(|ty| substitute_type(ty, mapping, int_vars))
                .collect(),
            param_spans: func.param_spans.clone(),
            result: substitute_type(&func.result, mapping, int_vars),
            kind: func.kind,
            name_span: func.name_span,
            callee: func.callee.clone(),
        }),
        Type::Tuple(items) => Type::Tuple(Arc::new(
            items
                .iter()
                .map(|ty| substitute_type(ty, mapping, int_vars))
                .collect(),
        )),
        Type::Record(record) => Type::from(RecordType {
            name: record.name.clone(),
            fields: record
                .fields
                .iter()
                .map(|field| RecordFieldType {
                    name: field.name.clone(),
                    ty: substitute_type(&field.ty, mapping, int_vars),
                })
                .collect(),
        }),
        Type::Enum(enum_type) => Type::from(EnumType {
            name: enum_type.name.clone(),
            variants: enum_type
                .variants
                .iter()
                .map(|variant| EnumVariantType {
                    name: variant.name.clone(),
                    kind: match &variant.kind {
                        EnumVariantKind::Unit => EnumVariantKind::Unit,
                        EnumVariantKind::Tuple(payload) => EnumVariantKind::Tuple(
                            payload
                                .iter()
                                .map(|ty| substitute_type(ty, mapping, int_vars))
                                .collect(),
                        ),
                        EnumVariantKind::Struct(fields) => EnumVariantKind::Struct(
                            fields
                                .iter()
                                .map(|field| {
                                    RecordFieldType::new(
                                        field.name.clone(),
                                        substitute_type(&field.ty, mapping, int_vars),
                                    )
                                })
                                .collect(),
                        ),
                    },
                })
                .collect(),
            type_args: enum_type
                .type_args
                .iter()
                .map(|ty| substitute_type(ty, mapping, int_vars))
                .collect(),
        }),
        Type::Int(w) => Type::Int(*w),
        Type::Bool => Type::Bool,
        Type::Unit => Type::Unit,
        Type::UtxoAny => Type::UtxoAny,
        Type::Utxo(id) => Type::Utxo(id.clone()),
        Type::TokenAny => Type::TokenAny,
        Type::Token(id) => Type::Token(id.clone()),
        Type::Abi(name) => Type::Abi(name.clone()),
    }
}

/// Return `true` if `var` appears anywhere inside `ty`, expanding substitutions as needed.
fn occurs_in(var: TypeVarId, ty: &Type, mapping: &HashMap<TypeVarId, Type>) -> bool {
    match ty {
        Type::Var(id) => {
            if id == &var {
                true
            } else {
                mapping
                    .get(id)
                    .map(|ty| occurs_in(var, ty, mapping))
                    .unwrap_or(false)
            }
        }
        Type::Function(func) => {
            func.params.iter().any(|t| occurs_in(var, t, mapping))
                || occurs_in(var, &func.result, mapping)
        }
        Type::Tuple(items) => items.iter().any(|t| occurs_in(var, t, mapping)),
        Type::Record(record) => record
            .fields
            .iter()
            .any(|field| occurs_in(var, &field.ty, mapping)),
        Type::Enum(enum_type) => enum_type
            .variants
            .iter()
            .any(|variant| match &variant.kind {
                EnumVariantKind::Unit => false,
                EnumVariantKind::Tuple(payload) => {
                    payload.iter().any(|ty| occurs_in(var, ty, mapping))
                }
                EnumVariantKind::Struct(fields) => fields
                    .iter()
                    .any(|field| occurs_in(var, &field.ty, mapping)),
            }),
        Type::Int(_)
        | Type::Bool
        | Type::Unit
        | Type::UtxoAny
        | Type::Utxo(_)
        | Type::TokenAny
        | Type::Token(_)
        | Type::Abi(_) => false,
    }
}

/// Collect all free type variables present in `ty`.
pub(crate) fn free_type_vars_type(ty: &Type) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    collect_free_type_vars(ty, &mut set);
    set
}

/// Helper for `free_type_vars_type` that walks the type tree.
fn collect_free_type_vars(ty: &Type, set: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) => {
            set.insert(*id);
        }
        Type::Function(func) => {
            for ty in &func.params {
                collect_free_type_vars(ty, set);
            }
            collect_free_type_vars(&func.result, set);
        }
        Type::Tuple(items) => {
            for ty in items.iter() {
                collect_free_type_vars(ty, set);
            }
        }
        Type::Record(record) => {
            for field in &record.fields {
                collect_free_type_vars(&field.ty, set);
            }
        }
        Type::Enum(enum_type) => {
            for variant in &enum_type.variants {
                match &variant.kind {
                    EnumVariantKind::Unit => {}
                    EnumVariantKind::Tuple(payload) => {
                        for ty in payload {
                            collect_free_type_vars(ty, set);
                        }
                    }
                    EnumVariantKind::Struct(fields) => {
                        for field in fields {
                            collect_free_type_vars(&field.ty, set);
                        }
                    }
                }
            }
        }
        Type::Int(_)
        | Type::Bool
        | Type::Unit
        | Type::UtxoAny
        | Type::Utxo(_)
        | Type::TokenAny
        | Type::Token(_)
        | Type::Abi(_) => {}
    }
}
