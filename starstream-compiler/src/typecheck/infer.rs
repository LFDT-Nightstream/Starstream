#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use starstream_types::{
    Abi, AbiDef, AbiPart, Arguments, DUMMY_SPAN, EventDef, FunctionKind, GenericTypeDef,
    IfCondition, IntWidth, Scheme, Span, Spanned, StaticFunction, Type, TypeParam, TypeVarId,
    TypedTokenDef, TypedUtxoDef, TypedUtxoGlobal, TypedUtxoPart, UtxoDef, UtxoGlobal, UtxoPart,
    ast::{
        BinaryOp, Block, Definition, EnumDef, EnumPatternPayload, EnumVariantPayload, Expr,
        FunctionDef, Identifier, ImportDef, ImportItems, ImportSource, Literal, Pattern, Program,
        Statement, StructDef, TypeAnnotation, UnaryOp,
    },
    typed_ast::{
        TypedAbiDef, TypedAbiMethodDecl, TypedAbiPart, TypedBlock, TypedDefinition, TypedEnumDef,
        TypedEnumPatternPayload, TypedEnumVariant, TypedEnumVariantPayload, TypedEventDef,
        TypedExpr, TypedExprKind, TypedFunctionDef, TypedFunctionParam, TypedIfCondition,
        TypedImportDef, TypedImportItems, TypedImportNamedItem, TypedImportSource, TypedMatchArm,
        TypedPattern, TypedProgram, TypedStatement, TypedStructDef, TypedStructField,
        TypedStructFieldInitializer, TypedStructPatternField,
    },
    types::{
        EnumType, EnumVariantKind as TypeEnumVariantKind, EnumVariantType as TypeEnumVariant,
        RecordFieldType as TypeRecordField, RecordType,
    },
};

use super::{
    builtins::BuiltinRegistry,
    env::{Binding, BindingClass, BindingVisibility, TypeEnv},
    errors::{ConditionContext, EnumPayloadKind, TypeError, TypeErrorKind},
    tree::InferenceTree,
    warnings::{TypeWarning, TypeWarningKind},
};
use crate::formatter;

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

struct TypeRegistry {
    entries: HashMap<String, TypeEntry>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, entry: TypeEntry) -> &TypeEntry {
        self.entries.entry(name).insert_entry(entry).into_mut()
    }

    fn get(&self, name: &str) -> Option<&TypeEntry> {
        self.entries.get(name)
    }

    fn enum_info(&self, name: &str) -> Option<EnumInfo> {
        self.entries.get(name).and_then(|entry| match &entry.kind {
            TypeEntryKind::Enum { variants } => Some(EnumInfo {
                ty: entry.ty.clone(),
                variants: variants.clone(),
            }),
            _ => None,
        })
    }
}

#[derive(Clone)]
struct TypeEntry {
    ty: Type,
    kind: TypeEntryKind,
    span: Span,
    type_params: Vec<TypeParam>,
    doc: Option<String>,
    variant_docs: HashMap<String, String>,
}

// TODO: Fold this into Type
#[derive(Clone)]
enum TypeEntryKind {
    Struct,
    Enum { variants: Vec<EnumVariantInfo> },
    Handle,
}

#[derive(Clone)]
struct StructFieldInfo {
    name: Identifier,
    ty: Type,
    span: Span,
}

#[derive(Clone)]
struct EnumInfo {
    ty: Type,
    variants: Vec<EnumVariantInfo>,
}

#[derive(Clone)]
struct EnumVariantInfo {
    name: Identifier,
    kind: EnumVariantInfoKind,
}

#[derive(Clone)]
enum EnumVariantInfoKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructFieldInfo>),
}

/// Run Hindley–Milner style inference over the parsed program and return the
/// typed AST along with optional tracing information.
pub fn typecheck_program(
    program: &Program,
    options: TypecheckOptions,
) -> Result<TypecheckSuccess, TypecheckFailure> {
    let mut inferencer = Inferencer::new(options.capture_traces);

    if let Err(error) = inferencer.register_type_definitions(&program.definitions) {
        return Err(TypecheckFailure {
            errors: vec![error],
            warnings: inferencer.warnings,
        });
    }

    let mut typed_definitions = Vec::with_capacity(program.definitions.len());
    let mut definition_traces = Vec::with_capacity(program.definitions.len());
    let mut errors = Vec::new();

    let mut env = TypeEnv::new();
    for definition in &program.definitions {
        match inferencer.infer_definition(&mut env, &definition.node) {
            Ok((typed_definition, trace)) => {
                typed_definitions.push(typed_definition);
                definition_traces.push(trace);
            }
            Err(error) => {
                errors.push(error);
            }
        }
    }

    if !errors.is_empty() {
        return Err(TypecheckFailure {
            errors,
            warnings: inferencer.warnings,
        });
    }

    // Default any unresolved integer type variables to i64.
    inferencer.default_int_vars();

    // Check range validity of integer literals against their resolved types.
    if let Err(errors) = inferencer.check_int_literal_ranges() {
        return Err(TypecheckFailure {
            errors,
            warnings: inferencer.warnings,
        });
    }

    let mut typed_program = TypedProgram {
        has_yields: inferencer.has_yields,
        definitions: typed_definitions,
    };
    inferencer.apply_substitutions_program(&mut typed_program);

    let traces = if options.capture_traces {
        definition_traces
    } else {
        Vec::new()
    };

    let generic_types = inferencer.build_generic_type_defs();
    let warnings = inferencer.warnings;

    Ok(TypecheckSuccess {
        program: typed_program,
        traces,
        generic_types,
        warnings,
    })
}

/// One typechecked module within a `TypedModuleGraph`.
#[derive(Clone, Debug)]
pub struct TypedModule {
    pub id: crate::ModuleId,
    pub abs_path: std::path::PathBuf,
    pub source: std::sync::Arc<str>,
    pub program: TypedProgram,
}

/// Typechecked counterpart to `ModuleGraph`. Modules are listed in `id`
/// order (matching the source graph), with `topo_order` giving the iteration
/// order callers should use for downstream passes.
#[derive(Clone, Debug)]
pub struct TypedModuleGraph {
    pub modules: Vec<TypedModule>,
    pub topo_order: Vec<crate::ModuleId>,
    /// Module ids that declare `contract;` — i.e. codegen entries.
    pub contract_entries: Vec<crate::ModuleId>,
    pub generic_types: HashMap<String, GenericTypeDef>,
    /// Warnings collected from every module (e.g. unnecessary disclose,
    /// path-import-in-single-file). Carried even on success so callers can
    /// decide whether to render them.
    pub warnings: Vec<(crate::ModuleId, TypeWarning)>,
}

impl TypedModuleGraph {
    pub fn module(&self, id: crate::ModuleId) -> &TypedModule {
        &self.modules[id.index()]
    }
}

/// Failure returned by `typecheck_modules`. Each error/warning carries the
/// module id it originated in so callers can render diagnostics against the
/// right source.
#[derive(Debug)]
pub struct TypecheckModulesFailure {
    pub errors: Vec<(crate::ModuleId, TypeError)>,
    pub warnings: Vec<(crate::ModuleId, TypeWarning)>,
}

/// Per-module captured exports. Used when later modules import names from this
/// one.
#[derive(Default)]
struct ModuleExports {
    /// `local name -> (signature, decl span, effect)`.
    functions: HashMap<String, ExportedFunction>,
    /// Names of struct/enum/abi/utxo definitions owned by this module.
    /// They live in the shared `TypeRegistry`/`AbiRegistry`, so importing
    /// modules find them by name without further work — we just track them
    /// here so we can validate that an `import { Foo } from "..."` actually
    /// references something this module declares.
    types: HashSet<String>,
}

#[derive(Clone)]
struct ExportedFunction {
    param_types: Vec<Type>,
    param_spans: Vec<Span>,
    return_type: Type,
    kind: FunctionKind,
    name_span: Span,
}

impl ExportedFunction {
    fn to_function_type(&self) -> Type {
        Type::Function {
            params: self.param_types.clone(),
            param_spans: self.param_spans.clone(),
            result: Box::new(self.return_type.clone()),
            kind: self.kind,
            name_span: self.name_span,
            callee: None,
        }
    }

    fn to_function_info(&self) -> FunctionInfo {
        FunctionInfo {
            param_types: self.param_types.clone(),
            param_spans: self.param_spans.clone(),
            return_type: self.return_type.clone(),
            kind: self.kind,
            name_span: self.name_span,
            callee: None,
        }
    }
}

/// Run inference across an entire module graph in topological order.
///
/// Path imports are resolved here: their names get inserted into the importing
/// module's `TypeEnv` (for functions) or the shared `NamespaceRegistry` (for
/// `import x from "./x.star"`). Types/structs/enums declared in any module
/// share a single registry — strict per-module visibility is enforced for
/// **functions**, while types are globally visible (acceptable for v1; see
/// `docs/language-spec.md`).
pub fn typecheck_modules(
    graph: &crate::ModuleGraph,
    options: TypecheckOptions,
) -> Result<TypedModuleGraph, TypecheckModulesFailure> {
    let mut inferencer = Inferencer::new(options.capture_traces);

    // `module_exports[id]` is populated as we finish typechecking each module.
    let mut module_exports: HashMap<u32, ModuleExports> = HashMap::new();
    let mut typed_modules: HashMap<u32, TypedProgram> = HashMap::new();

    let mut all_errors: Vec<(crate::ModuleId, TypeError)> = Vec::new();
    let mut all_warnings: Vec<(crate::ModuleId, TypeWarning)> = Vec::new();

    let mut bailed = false;

    for &module_id in graph.topo_order() {
        if bailed {
            break;
        }

        let module = graph.module(module_id);
        let mut env = TypeEnv::new();

        // Resolve path imports for this module first. Any error here is fatal
        // for this module — we still continue to subsequent modules so the
        // user gets as many diagnostics as possible.
        let import_resolution = match resolve_path_imports(
            &mut inferencer,
            &mut env,
            &module.program,
            graph,
            module_id,
            &module_exports,
        ) {
            Ok(resolution) => resolution,
            Err(errors) => {
                all_errors.extend(errors.into_iter().map(|e| (module_id, e)));
                continue;
            }
        };

        // Run the existing inference pipeline on this module's definitions.
        if let Err(error) = inferencer.register_type_definitions(&module.program.definitions) {
            all_errors.push((module_id, error));
            continue;
        }

        let mut typed_definitions = Vec::with_capacity(module.program.definitions.len());
        let mut module_failed = false;
        for (idx, definition) in module.program.definitions.iter().enumerate() {
            // Path imports are short-circuited: build the typed AST from the
            // pre-resolved info instead of going through `register_import`.
            if let Definition::Import(import) = &definition.node
                && let ImportSource::Path(_) = &import.from
            {
                if let Some(typed) = import_resolution.typed_imports.get(&idx) {
                    typed_definitions.push(TypedDefinition::Import(typed.clone()));
                }
                continue;
            }

            match inferencer.infer_definition(&mut env, &definition.node) {
                Ok((typed_def, _trace)) => typed_definitions.push(typed_def),
                Err(error) => {
                    all_errors.push((module_id, error));
                    module_failed = true;
                }
            }
        }

        if module_failed {
            // Capture a (possibly partial) export table so other modules can
            // continue — they may still produce useful diagnostics. But we
            // flag the run as failed.
            module_exports.insert(module_id.0, ModuleExports::default());
            continue;
        }

        // Capture this module's exports for downstream modules.
        let exports = collect_exports(&typed_definitions);
        module_exports.insert(module_id.0, exports);

        let typed_program = TypedProgram {
            has_yields: inferencer.has_yields,
            definitions: typed_definitions,
        };
        typed_modules.insert(module_id.0, typed_program);

        // Drain warnings emitted during this module's pass.
        while let Some(warning) = inferencer.warnings.pop() {
            all_warnings.push((module_id, warning));
        }
        // Re-reverse: pop reverses order; emit in original order.
        let module_warnings_count = all_warnings
            .iter()
            .rev()
            .take_while(|(id, _)| *id == module_id)
            .count();
        let split = all_warnings.len() - module_warnings_count;
        all_warnings[split..].reverse();

        // Stop once any module has failed catastrophically.
        if !all_errors.is_empty() {
            bailed = true;
        }
    }

    if !all_errors.is_empty() {
        return Err(TypecheckModulesFailure {
            errors: all_errors,
            warnings: all_warnings,
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
            warnings: all_warnings,
        });
    }

    // Apply substitutions per module.
    for typed_program in typed_modules.values_mut() {
        inferencer.apply_substitutions_program(typed_program);
    }

    let generic_types = inferencer.build_generic_type_defs();

    let mut modules: Vec<TypedModule> = Vec::with_capacity(graph.modules().len());
    for source_module in graph.modules() {
        let typed_program = typed_modules
            .remove(&source_module.id.0)
            .unwrap_or_default();
        modules.push(TypedModule {
            id: source_module.id,
            abs_path: source_module.abs_path.clone(),
            source: source_module.source.clone(),
            program: typed_program,
        });
    }

    Ok(TypedModuleGraph {
        modules,
        topo_order: graph.topo_order().to_vec(),
        contract_entries: graph.contract_entries().to_vec(),
        generic_types,
        warnings: all_warnings,
    })
}

/// Resolution work for the `Definition::Import { from: Path(...) }` items in a
/// single module: produces the typed import nodes (indexed by their position
/// in the module's `definitions`) and inserts the imported names into the
/// importer's `env` / `namespaces` so subsequent inference can resolve them.
struct PathImportResolution {
    typed_imports: HashMap<usize, TypedImportDef>,
}

fn resolve_path_imports(
    inferencer: &mut Inferencer,
    env: &mut TypeEnv,
    program: &Program,
    graph: &crate::ModuleGraph,
    importer: crate::ModuleId,
    module_exports: &HashMap<u32, ModuleExports>,
) -> Result<PathImportResolution, Vec<TypeError>> {
    let mut typed_imports: HashMap<usize, TypedImportDef> = HashMap::new();
    let mut errors: Vec<TypeError> = Vec::new();

    let edges = graph.edges_of(importer);
    let edge_by_def: HashMap<usize, &crate::PathImport> =
        edges.iter().map(|e| (e.def_index, e)).collect();

    for (def_index, definition) in program.definitions.iter().enumerate() {
        let import = match &definition.node {
            Definition::Import(import) => import,
            _ => continue,
        };
        let path_value = match &import.from {
            ImportSource::Path(path) => path.value.clone(),
            _ => continue,
        };

        let edge = match edge_by_def.get(&def_index) {
            Some(edge) => *edge,
            None => continue, // Should not happen; module graph builds edges from these defs.
        };

        let target_id = edge.target;
        let target_module = graph.module(target_id);
        let target_exports = match module_exports.get(&target_id.0) {
            Some(exports) => exports,
            None => {
                // Topological order should ensure deps are processed first.
                // If we get here, there's likely a bug; skip gracefully.
                continue;
            }
        };

        let canonical = target_module.abs_path.clone();

        match &import.items {
            ImportItems::Named(items) => {
                let mut typed_items = Vec::with_capacity(items.len());
                for item in items {
                    let imported = item.imported.name.as_str();
                    if let Some(func) = target_exports.functions.get(imported) {
                        let ty = func.to_function_type();
                        env.insert(
                            item.local.name.clone(),
                            Binding {
                                decl_span: item.local.span(),
                                mutable: false,
                                scheme: Scheme::monomorphic(ty.clone()),
                                class: BindingClass::Local,
                                visibility: BindingVisibility::Private,
                            },
                        );
                        typed_items.push(TypedImportNamedItem {
                            imported: item.imported.clone(),
                            local: item.local.clone(),
                            ty,
                        });
                    } else if target_exports.types.contains(imported) {
                        // Types live in the shared `TypeRegistry` keyed by their
                        // original name. If the local name differs, register an
                        // alias so look-ups under the local name succeed.
                        if item.local.name != item.imported.name
                            && let Some(entry) = inferencer.types.entries.get(imported).cloned()
                        {
                            inferencer.types.insert(item.local.name.clone(), entry);
                        }
                        typed_items.push(TypedImportNamedItem {
                            imported: item.imported.clone(),
                            local: item.local.clone(),
                            ty: Type::Unit,
                        });
                    } else {
                        errors.push(TypeError::new(
                            TypeErrorKind::UnknownPathExport {
                                path: path_value.clone(),
                                name: imported.to_string(),
                            },
                            item.imported.span(),
                        ));
                    }
                }
                typed_imports.insert(
                    def_index,
                    TypedImportDef {
                        items: TypedImportItems::Named(typed_items),
                        from: TypedImportSource::Path {
                            value: path_value,
                            canonical: Some(canonical),
                        },
                    },
                );
            }
            ImportItems::Namespace(alias) => {
                // TODO: currently only covers functions, should probably import the other namespace wholesale.
                let mut namespace = Namespace::default();
                let mut typed_items = Vec::with_capacity(target_exports.functions.len());
                for (name, func) in &target_exports.functions {
                    namespace
                        .functions
                        .insert(name.clone(), func.to_function_info());
                    typed_items.push(TypedImportNamedItem {
                        imported: Identifier::anon(name),
                        local: Identifier::anon(name),
                        ty: func.to_function_type(),
                    });
                }
                inferencer
                    .root
                    .namespaces
                    .insert(alias.name.clone(), namespace);
                typed_imports.insert(
                    def_index,
                    TypedImportDef {
                        items: TypedImportItems::Namespace {
                            alias: alias.clone(),
                            functions: typed_items,
                        },
                        from: TypedImportSource::Path {
                            value: path_value,
                            canonical: Some(canonical),
                        },
                    },
                );
            }
        }
    }

    if errors.is_empty() {
        Ok(PathImportResolution { typed_imports })
    } else {
        Err(errors)
    }
}

fn collect_exports(typed_definitions: &[TypedDefinition]) -> ModuleExports {
    let mut exports = ModuleExports::default();
    for def in typed_definitions {
        match def {
            TypedDefinition::Function(func) => {
                let param_types = func.params.iter().map(|p| p.ty.clone()).collect();
                let param_spans = func.params.iter().map(|p| p.name.span()).collect();
                exports.functions.insert(
                    func.name.name.clone(),
                    ExportedFunction {
                        param_types,
                        param_spans,
                        return_type: func.return_type.clone(),
                        kind: FunctionKind::Normal,
                        name_span: func.name.span(),
                    },
                );
            }
            TypedDefinition::Struct(s) => {
                exports.types.insert(s.name.name.clone());
            }
            TypedDefinition::Enum(e) => {
                exports.types.insert(e.name.name.clone());
            }
            TypedDefinition::Abi(a) => {
                exports.types.insert(a.name.name.clone());
            }
            TypedDefinition::Utxo(u) => {
                exports.types.insert(u.name.name.clone());
            }
            // `token` is parse-only for now: no exported type yet.
            TypedDefinition::Token(_) => {}
            TypedDefinition::Import(_) => {}
            TypedDefinition::Contract => {}
        }
    }
    exports
}

/// Internal stateful helper that owns the substitution map and generates fresh
/// type variables while walking the AST.
struct Inferencer {
    // Options
    capture_traces: bool,

    // Outputs
    warnings: Vec<TypeWarning>,
    has_yields: bool,

    // Type var tracking
    next_type_var: u32,
    subst: HashMap<TypeVarId, Type>,
    /// Type variables constrained to integer types (from polymorphic integer literals).
    int_vars: HashSet<TypeVarId>,
    /// Tracks the literal value associated with each integer type variable for range checking.
    int_literal_values: HashMap<TypeVarId, (i128, Span)>,

    /// Root namespace, and registries for things that can't be namespaced (yet).
    root: Namespace,
    types: TypeRegistry,
    abis: AbiRegistry,

    /// Registry of builtins that are available to be `import`ed.
    builtins: BuiltinRegistry,

    /// Stack of linearity trackers for `if x is Abi` blocks (supports nesting).
    abi_call_trackers: Vec<AbiCallTracker>,
}

/// Namespace
#[derive(Clone, Default)]
struct Namespace {
    namespaces: HashMap<String, Namespace>,
    functions: HashMap<String, FunctionInfo>,
    struct_constructors: HashMap<String, StructConstructor>,
}

#[derive(Clone)]
struct FunctionInfo {
    /// Keyword needed to call this function, if any.
    kind: FunctionKind,
    name_span: Span,
    param_types: Vec<Type>,
    param_spans: Vec<Span>,
    return_type: Type,
    callee: Option<StaticFunction>,
}

#[derive(Clone)]
struct StructConstructor {
    ty: Type,
    enum_variant: usize,
}

impl StructConstructor {
    fn record_ty(&self) -> &RecordType {
        let Type::Record(r) = &self.ty else {
            unreachable!()
        };
        r
    }
}

struct AbiRegistry {
    entries: HashMap<String, Arc<Abi>>,
}

impl AbiRegistry {
    fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, info: Abi) {
        self.entries.insert(name, Arc::new(info));
    }

    fn get(&self, name: &str) -> Option<&Arc<Abi>> {
        self.entries.get(name)
    }
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
        let mut inferencer = Self {
            capture_traces,
            next_type_var: 0,
            subst: HashMap::new(),
            int_vars: HashSet::new(),
            int_literal_values: HashMap::new(),
            root: Namespace::default(),
            types: TypeRegistry::new(),
            abis: AbiRegistry::new(),
            builtins: BuiltinRegistry::new(),
            warnings: Vec::new(),
            abi_call_trackers: Vec::new(),
            has_yields: false,
        };
        inferencer.register_prelude_types();
        inferencer
    }

    /// Register builtin prelude types (`Option<T>`, `Result<T, E>`).
    fn register_prelude_types(&mut self) {
        // Option<T>
        let t = self.fresh_var_id();
        self.register_prelude_enum(
            "Option",
            vec![
                ("Some", EnumVariantInfoKind::Tuple(vec![Type::Var(t)])),
                ("None", EnumVariantInfoKind::Unit),
            ],
            vec![TypeParam {
                id: t,
                name: "T".into(),
            }],
            "A value that may or may not be present.",
            &[("Some", "Contains a value."), ("None", "No value present.")],
        );

        // Result<T, E>
        let t2 = self.fresh_var_id();
        let e = self.fresh_var_id();
        self.register_prelude_enum(
            "Result",
            vec![
                ("Ok", EnumVariantInfoKind::Tuple(vec![Type::Var(t2)])),
                ("Err", EnumVariantInfoKind::Tuple(vec![Type::Var(e)])),
            ],
            vec![
                TypeParam {
                    id: t2,
                    name: "T".into(),
                },
                TypeParam {
                    id: e,
                    name: "E".into(),
                },
            ],
            "A value representing either success or failure.",
            &[
                ("Ok", "Contains a success value."),
                ("Err", "Contains an error value."),
            ],
        );
    }

    /// Helper to register a prelude enum type, building both the `Type::Enum`
    /// and the internal `EnumVariantInfo` from a single variant description.
    fn register_prelude_enum(
        &mut self,
        name: &str,
        variants: Vec<(&str, EnumVariantInfoKind)>,
        type_params: Vec<TypeParam>,
        doc: &str,
        variant_docs: &[(&str, &str)],
    ) {
        let type_variants: Vec<TypeEnumVariant> = variants
            .iter()
            .map(|(vname, kind)| match kind {
                EnumVariantInfoKind::Unit => TypeEnumVariant::unit(*vname),
                EnumVariantInfoKind::Tuple(types) => TypeEnumVariant::tuple(*vname, types.clone()),
                EnumVariantInfoKind::Struct(fields) => TypeEnumVariant::struct_variant(
                    *vname,
                    fields
                        .iter()
                        .map(|f| TypeRecordField::new(f.name.clone(), f.ty.clone()))
                        .collect(),
                ),
            })
            .collect();

        let info_variants: Vec<EnumVariantInfo> = variants
            .into_iter()
            .map(|(vname, kind)| EnumVariantInfo {
                name: Identifier::anon(vname),
                kind,
            })
            .collect();

        self.types.insert(
            name.to_string(),
            TypeEntry {
                ty: Type::enum_type(name, type_variants),
                kind: TypeEntryKind::Enum {
                    variants: info_variants,
                },
                span: DUMMY_SPAN,
                type_params,
                doc: Some(doc.into()),
                variant_docs: variant_docs
                    .iter()
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .collect(),
            },
        );
    }

    fn build_generic_type_defs(&self) -> HashMap<String, GenericTypeDef> {
        self.types
            .entries
            .iter()
            .filter(|(_, entry)| !entry.type_params.is_empty() || entry.doc.is_some())
            .map(|(name, entry)| {
                let mut ty = entry.ty.clone();
                // Set type_args to the type param vars so display renders them
                if let Type::Enum(ref mut e) = ty {
                    e.type_args = entry.type_params.iter().map(|p| Type::Var(p.id)).collect();
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

    fn register_type_definitions(
        &mut self,
        definitions: &[Spanned<Definition>],
    ) -> Result<(), TypeError> {
        for definition in definitions {
            match &definition.node {
                Definition::Contract => {}
                Definition::Import(_) => {}
                Definition::Struct(def) => self.register_struct(def)?,
                Definition::Enum(def) => self.register_enum(def)?,
                Definition::Function(_) => {}
                Definition::Utxo(def) => self.register_utxo(def)?,
                // `token` definitions are parse-only for now: no type is
                // registered yet (the global `Token` type is a follow-up).
                Definition::Token(_) => {}
                Definition::Abi(def) => self.register_abi(def)?,
            }
        }
        for definition in definitions {
            if let Definition::Function(def) = &definition.node {
                self.register_function(def)?;
            }
        }
        Ok(())
    }

    fn register_function(&mut self, _def: &FunctionDef) -> Result<(), TypeError> {
        // TODO: hoist function type discovery back here, out of `infer_function`,
        // so that code can call functions declared later in the file.
        Ok(())
    }

    fn register_import(&mut self, env: &mut TypeEnv, import: &ImportDef) -> Result<(), TypeError> {
        match &import.from {
            ImportSource::Wit {
                namespace,
                package,
                interface,
            } => {
                self.register_wit_import(env, &import.items, namespace, package, interface.as_ref())
            }
            ImportSource::Path(path) => {
                // Path imports are resolved by `typecheck_modules`. In the
                // single-file flow (playground, LSP per-file checks,
                // `starstream check`) we emit a warning so users understand
                // why references to the imported names won't resolve.
                self.warnings.push(TypeWarning::new(
                    TypeWarningKind::PathImportIgnoredInSingleFile {
                        path: path.value.clone(),
                    },
                    path.span,
                ));
                Ok(())
            }
        }
    }

    fn register_wit_import(
        &mut self,
        env: &mut TypeEnv,
        items: &ImportItems,
        namespace_id: &Identifier,
        package_id: &Identifier,
        interface_id: Option<&Identifier>,
    ) -> Result<(), TypeError> {
        let namespace = &namespace_id.name;
        let package = &package_id.name;

        // Check if the package exists in our builtin registry
        if !self.builtins.has_package(namespace, package) {
            return Err(TypeError::new(
                TypeErrorKind::UnknownImportPackage {
                    namespace: namespace.clone(),
                    package: package.clone(),
                },
                namespace_id.span(),
            ));
        }

        match items {
            ImportItems::Named(items) => {
                // Named import: `import { blockHeight } from starstream:std/cardano;`
                let interface = interface_id.ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownImportInterface {
                            namespace: namespace.clone(),
                            package: package.clone(),
                            interface: "".to_string(),
                        },
                        package_id.span(),
                    )
                    .with_help("named imports require an interface, e.g., `starstream:std/cardano`")
                })?;

                let interface_funcs = self
                    .builtins
                    .get_interface(namespace, package, &interface.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownImportInterface {
                                namespace: namespace.clone(),
                                package: package.clone(),
                                interface: interface.name.clone(),
                            },
                            interface.span(),
                        )
                    })?;

                for item in items {
                    let builtin = interface_funcs.get(&item.imported.name).ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownImportFunction {
                                path: format!("{namespace}:{package}/{}", interface.name),
                                name: item.imported.name.clone(),
                            },
                            item.imported.span(),
                        )
                    })?;

                    // Add function to the type environment so it can be looked up
                    env.insert(
                        item.local.name.clone(),
                        Binding {
                            decl_span: item.local.span(),
                            mutable: false,
                            scheme: Scheme::monomorphic(builtin.to_function_type()),
                            class: BindingClass::Local,
                            visibility: BindingVisibility::Private,
                        },
                    );
                }
            }
            ImportItems::Namespace(alias) => {
                // Namespace import: `import cardano from starstream:std/cardano;`
                let interface = interface_id.ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownImportInterface {
                            namespace: namespace.clone(),
                            package: package.clone(),
                            interface: "".to_string(),
                        },
                        package_id.span(),
                    )
                    .with_help("namespace imports require an interface, e.g., `import cardano from starstream:std/cardano;`")
                })?;

                let interface_funcs = self
                    .builtins
                    .get_interface(namespace, package, &interface.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownImportInterface {
                                namespace: namespace.clone(),
                                package: package.clone(),
                                interface: interface.name.clone(),
                            },
                            interface.span(),
                        )
                    })?;

                // Build a map of all functions in this interface
                let mut namespace = Namespace::default();
                for (name, builtin) in interface_funcs {
                    namespace.functions.insert(
                        name.clone(),
                        FunctionInfo {
                            param_types: builtin.params.clone(),
                            param_spans: vec![],
                            return_type: builtin.return_type.clone(),
                            kind: builtin.kind,
                            name_span: alias.span(),
                            callee: Some(StaticFunction::Named(name.clone())),
                        },
                    );
                }

                self.root.namespaces.insert(alias.name.clone(), namespace);
            }
        }

        Ok(())
    }

    fn register_abi(&mut self, def: &AbiDef) -> Result<(), TypeError> {
        let mut methods = Vec::new();
        for part in &def.parts {
            match part {
                AbiPart::Event(event) => self.register_event(event)?,
                AbiPart::Effect(_effect) => {}
                AbiPart::FnDecl(method) => {
                    let mut params = Vec::with_capacity(method.params.len());
                    for param in &method.params {
                        let ty = self.type_from_annotation(&param.ty)?;
                        params.push(TypedFunctionParam {
                            public: param.public,
                            name: param.name.clone(),
                            ty,
                        });
                    }
                    let return_type = match &method.return_type {
                        Some(ann) => self.type_from_annotation(ann)?,
                        None => Type::Unit,
                    };
                    methods.push(TypedAbiMethodDecl {
                        name: method.name.clone(),
                        params,
                        return_type,
                        span: method.name.span(),
                    });
                }
            }
        }
        self.abis.insert(
            def.name.name.clone(),
            Abi {
                name: def.name.clone(),
                methods,
            },
        );
        Ok(())
    }

    fn register_event(&mut self, event: &EventDef) -> Result<(), TypeError> {
        let name = event.name.name.clone();
        let name_span = event.name.span();

        let mut param_types = Vec::with_capacity(event.params.len());
        let mut param_spans = Vec::with_capacity(event.params.len());
        for param in &event.params {
            let ty = self.type_from_annotation(&param.ty)?;
            param_types.push(ty);
            param_spans.push(param.ty.name.span());
        }
        self.root.functions.insert(
            name,
            FunctionInfo {
                kind: FunctionKind::Emit,
                name_span,
                param_types,
                param_spans,
                return_type: Type::Unit,
                callee: None,
            },
        );
        Ok(())
    }

    fn register_struct(&mut self, def: &StructDef) -> Result<(), TypeError> {
        let name = def.name.name.clone();
        if let Some(existing) = self.types.get(&name) {
            return Err(TypeError::new(
                TypeErrorKind::TypeAlreadyDefined { name },
                def.name.span(),
            )
            .with_secondary(existing.span, "previously defined here"));
        }

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
            let ty = self.type_from_annotation(&field.ty)?;
            fields.push(StructFieldInfo {
                name: field.name.clone(),
                ty,
                span: field.name.span(),
            });
        }

        let type_fields = fields
            .iter()
            .map(|field| TypeRecordField::new(field.name.clone(), field.ty.clone()))
            .collect();
        let ty = Type::record(def.name.name.clone(), type_fields);
        self.types.insert(
            def.name.name.clone(),
            TypeEntry {
                ty: ty.clone(),
                kind: TypeEntryKind::Struct,
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        );
        self.root.struct_constructors.insert(
            def.name.name.clone(),
            StructConstructor {
                ty,
                enum_variant: 0,
            },
        );
        Ok(())
    }

    fn register_enum(&mut self, def: &EnumDef) -> Result<(), TypeError> {
        let name = def.name.name.clone();
        if let Some(existing) = self.types.get(&name) {
            return Err(TypeError::new(
                TypeErrorKind::TypeAlreadyDefined { name },
                def.name.span(),
            )
            .with_secondary(existing.span, "previously defined here"));
        }

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
                EnumVariantPayload::Unit => EnumVariantInfoKind::Unit,
                EnumVariantPayload::Tuple(items) => {
                    let mut payload = Vec::with_capacity(items.len());
                    for ty in items {
                        payload.push(self.type_from_annotation(ty)?);
                    }
                    EnumVariantInfoKind::Tuple(payload)
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
                        let ty = self.type_from_annotation(&field.ty)?;
                        payload.push(StructFieldInfo {
                            name: field.name.clone(),
                            ty,
                            span: field.name.span(),
                        });
                    }
                    EnumVariantInfoKind::Struct(payload)
                }
            };

            variants.push(EnumVariantInfo {
                name: variant.name.clone(),
                kind,
            });
        }

        let type_variants = variants
            .iter()
            .map(|variant| match &variant.kind {
                EnumVariantInfoKind::Unit => TypeEnumVariant::unit(variant.name.name.clone()),
                EnumVariantInfoKind::Tuple(payload) => {
                    TypeEnumVariant::tuple(variant.name.name.clone(), payload.clone())
                }
                EnumVariantInfoKind::Struct(fields) => TypeEnumVariant::struct_variant(
                    variant.name.name.clone(),
                    fields
                        .iter()
                        .map(|field| TypeRecordField::new(field.name.clone(), field.ty.clone()))
                        .collect(),
                ),
            })
            .collect();
        let ty = Type::enum_type(def.name.name.clone(), type_variants);
        let ty = &self
            .types
            .insert(
                def.name.name.clone(),
                TypeEntry {
                    ty,
                    kind: TypeEntryKind::Enum { variants },
                    span: def.name.span(),
                    type_params: vec![],
                    doc: None,
                    variant_docs: HashMap::new(),
                },
            )
            .ty;
        let Type::Enum(enum_type) = ty else {
            unreachable!()
        };

        let namespace = self
            .root
            .namespaces
            .entry(def.name.name.clone())
            .or_default();
        for (i, variant) in enum_type.variants.iter().enumerate() {
            match &variant.kind {
                TypeEnumVariantKind::Unit => {
                    // TODO
                }
                TypeEnumVariantKind::Tuple(items) => {
                    namespace.functions.insert(
                        variant.name.to_string(),
                        FunctionInfo {
                            kind: FunctionKind::Normal,
                            // TODO: non-dummy spans here
                            name_span: DUMMY_SPAN,
                            param_types: items.clone(),
                            param_spans: vec![DUMMY_SPAN; items.len()],
                            return_type: ty.clone(),
                            callee: Some(StaticFunction::Constructor { variant: i }),
                        },
                    );
                }
                TypeEnumVariantKind::Struct(_fields) => {
                    namespace.struct_constructors.insert(
                        variant.name.to_string(),
                        StructConstructor {
                            ty: ty.clone(),
                            enum_variant: i,
                        },
                    );
                }
            }
        }

        Ok(())
    }

    fn register_utxo(&mut self, def: &UtxoDef) -> Result<(), TypeError> {
        let ty = Type::UtxoNamed(def.name.to_string());
        self.types.insert(
            def.name.to_string(),
            TypeEntry {
                ty,
                kind: TypeEntryKind::Handle,
                span: def.name.span(),
                type_params: vec![],
                doc: None,
                variant_docs: HashMap::new(),
            },
        );
        Ok(())
    }

    fn build_typed_struct(&self, def: &StructDef) -> Result<TypedStructDef, TypeError> {
        let info = self.lookup_struct_info(std::slice::from_ref(&def.name))?;

        let fields = info
            .record_ty()
            .fields
            .iter()
            .map(|field| TypedStructField {
                name: field.name.clone(),
                ty: field.ty.clone(),
            })
            .collect();

        Ok(TypedStructDef {
            name: def.name.clone(),
            fields,
            ty: info.ty.clone(),
        })
    }

    fn build_typed_enum(&self, def: &EnumDef) -> Result<TypedEnumDef, TypeError> {
        let info = self.types.enum_info(&def.name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownNamespace {
                    name: def.name.name.clone(),
                },
                def.name.span(),
            )
        })?;

        let variants = info
            .variants
            .iter()
            .map(|variant| TypedEnumVariant {
                name: variant.name.clone(),
                payload: match &variant.kind {
                    EnumVariantInfoKind::Unit => TypedEnumVariantPayload::Unit,
                    EnumVariantInfoKind::Tuple(payload) => {
                        TypedEnumVariantPayload::Tuple(payload.clone())
                    }
                    EnumVariantInfoKind::Struct(fields) => TypedEnumVariantPayload::Struct(
                        fields
                            .iter()
                            .map(|field| TypedStructField {
                                name: field.name.clone(),
                                ty: field.ty.clone(),
                            })
                            .collect(),
                    ),
                },
            })
            .collect();

        Ok(TypedEnumDef {
            name: def.name.clone(),
            variants,
            ty: info.ty,
        })
    }

    fn build_typed_import(&self, def: &ImportDef) -> TypedImportDef {
        use starstream_types::ast::ImportItems;

        match &def.from {
            ImportSource::Wit {
                namespace,
                package,
                interface,
            } => {
                let namespace_name = &namespace.name;
                let package_name = &package.name;
                let interface_name = interface.as_ref().map(|i| i.name.as_str());

                let items = match &def.items {
                    ImportItems::Named(named) => TypedImportItems::Named(
                        named
                            .iter()
                            .map(|item| {
                                // Look up the function type from the builtins registry
                                let ty = interface_name
                                    .and_then(|iface| {
                                        self.builtins
                                            .get_interface(namespace_name, package_name, iface)
                                            .and_then(|funcs| funcs.get(&item.imported.name))
                                            .map(|f| f.to_function_type())
                                    })
                                    .unwrap_or(Type::Unit);

                                TypedImportNamedItem {
                                    imported: item.imported.clone(),
                                    local: item.local.clone(),
                                    ty,
                                }
                            })
                            .collect(),
                    ),
                    ImportItems::Namespace(alias) => {
                        // Build the list of functions available in this namespace
                        let functions = interface_name
                            .and_then(|iface| {
                                self.builtins
                                    .get_interface(namespace_name, package_name, iface)
                            })
                            .map(|funcs| {
                                funcs
                                    .iter()
                                    .map(|(name, builtin)| TypedImportNamedItem {
                                        imported: Identifier::anon(name),
                                        local: Identifier::anon(name),
                                        ty: builtin.to_function_type(),
                                    })
                                    .collect()
                            })
                            .unwrap_or_default();

                        TypedImportItems::Namespace {
                            alias: alias.clone(),
                            functions,
                        }
                    }
                };

                let from = TypedImportSource::Wit {
                    namespace: namespace.clone(),
                    package: package.clone(),
                    interface: interface.clone(),
                };

                TypedImportDef { items, from }
            }
            ImportSource::Path(path) => {
                // Path-import typed shape is filled in by `typecheck_modules`; here we
                // produce a placeholder so the typed AST is well-formed when callers
                // invoke `typecheck_program` directly on a single file.
                let items = match &def.items {
                    ImportItems::Named(named) => TypedImportItems::Named(
                        named
                            .iter()
                            .map(|item| TypedImportNamedItem {
                                imported: item.imported.clone(),
                                local: item.local.clone(),
                                ty: Type::Unit,
                            })
                            .collect(),
                    ),
                    ImportItems::Namespace(alias) => TypedImportItems::Namespace {
                        alias: alias.clone(),
                        functions: Vec::new(),
                    },
                };

                let from = TypedImportSource::Path {
                    value: path.value.clone(),
                    canonical: None,
                };

                TypedImportDef { items, from }
            }
        }
    }

    fn build_typed_abi(&self, def: &AbiDef) -> Result<TypedAbiDef, TypeError> {
        let mut typed_parts = Vec::with_capacity(def.parts.len());

        for part in &def.parts {
            match part {
                AbiPart::Event(event) => {
                    let event_info =
                        self.root.functions.get(&event.name.name).ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::UnknownEvent {
                                    name: event.name.name.clone(),
                                },
                                event.name.span(),
                            )
                        })?;

                    let params = event_info
                        .param_types
                        .iter()
                        .zip(&event.params)
                        .map(|(ty, param)| TypedFunctionParam {
                            public: param.public,
                            name: param.name.clone(),
                            ty: ty.clone(),
                        })
                        .collect();

                    typed_parts.push(TypedAbiPart::Event(TypedEventDef {
                        name: event.name.clone(),
                        params,
                    }));
                }
                AbiPart::Effect(_effect) => {}
                AbiPart::FnDecl(method) => {
                    let abi_info = self.abis.get(&def.name.name).expect("abi registered");
                    let method_info = abi_info
                        .methods
                        .iter()
                        .find(|m| m.name.as_str() == method.name.as_str())
                        .expect("method registered");

                    typed_parts.push(TypedAbiPart::FnDecl(method_info.clone()));
                }
            }
        }

        Ok(TypedAbiDef {
            name: def.name.clone(),
            parts: typed_parts,
        })
    }

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
                    let (func, trace) = self.infer_function(env, function)?;
                    traces.push(trace);
                    TypedUtxoPart::Function(func.into())
                }
                UtxoPart::AbiImpl { abi, parts } => {
                    let span = abi.span();

                    let parts = parts
                        .iter()
                        .map(|function| {
                            let (func, trace) = self.infer_function(env, function)?;
                            traces.push(trace);
                            Ok(func)
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    // Assert that the signature sets match
                    let Some(abi_info) = self.abis.get(abi.as_str()) else {
                        return Err(TypeError::new(
                            TypeErrorKind::UnknownAbi {
                                name: abi.to_string(),
                            },
                            span,
                        ));
                    };
                    self.check_abi_impl(abi, abi_info, &parts)?;

                    let abi = Type::AbiNarrow(abi_info.clone());
                    TypedUtxoPart::AbiImpl { abi, span, parts }
                }
            });
        }

        env.pop_scope();

        Ok((
            TypedUtxoDef {
                name: def.name.clone(),
                parts,
                ty: Type::UtxoNamed(def.name.to_string()),
            },
            self.make_trace("T-Utxo", None, Some(def.name.to_string()), None, || traces),
        ))
    }

    fn check_abi_impl(
        &self,
        abi_name: &Identifier,
        abi: &Abi,
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
                // Method found, make sure parameters match
                for (i, (abi_param, impl_param)) in abi_method
                    .params
                    .iter()
                    .zip(impl_method.params.iter())
                    .enumerate()
                {
                    if abi_param.ty != impl_param.ty {
                        return Err(TypeError::new(
                            TypeErrorKind::ArgumentTypeMismatch {
                                expected: abi_param.ty.clone(),
                                found: impl_param.ty.clone(),
                                position: i,
                                param_span: Some(abi_method.params[i].name.span),
                            },
                            impl_param.name.span(),
                        ));
                    }
                }
                // And return type must match
                if abi_method.return_type != impl_method.return_type {
                    return Err(TypeError::new(
                        TypeErrorKind::ReturnMismatch {
                            expected: abi_method.return_type.clone(),
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
        let ty = self.type_from_annotation(&var.ty)?;
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

    fn lookup_struct_info(&self, name: &[Identifier]) -> Result<&StructConstructor, TypeError> {
        let (last, path) = name.split_last().unwrap();
        let mut ns = &self.root;
        for each in path {
            match ns.namespaces.get(each.as_str()) {
                Some(next) => ns = next,
                None => {
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownStruct {
                            name: each.to_string(),
                        },
                        each.span,
                    ));
                }
            }
        }
        ns.struct_constructors.get(last.as_str()).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownStruct {
                    name: last.to_string(),
                },
                last.span(),
            )
        })
    }

    fn lookup_enum_info(&mut self, name: &Identifier) -> Result<EnumInfo, TypeError> {
        self.instantiate_enum_info(&name.name).ok_or_else(|| {
            TypeError::new(
                TypeErrorKind::UnknownNamespace {
                    name: name.name.clone(),
                },
                name.span(),
            )
        })
    }

    /// Instantiate a (possibly generic) enum type entry, creating fresh type
    /// variables for any type parameters.
    fn instantiate_enum_info(&mut self, name: &str) -> Option<EnumInfo> {
        let entry = self.types.get(name)?;
        let TypeEntryKind::Enum { variants } = &entry.kind else {
            return None;
        };

        if entry.type_params.is_empty() {
            return Some(EnumInfo {
                ty: entry.ty.clone(),
                variants: variants.clone(),
            });
        }

        // Extract data before borrowing self mutably for fresh_var()
        let type_params = entry.type_params.clone();
        let template_ty = entry.ty.clone();
        let template_variants = variants.clone();

        let fresh_args: Vec<Type> = type_params.iter().map(|_| self.fresh_var()).collect();
        Self::apply_enum_substitution(&type_params, &fresh_args, &template_ty, &template_variants)
    }

    /// Instantiate a generic enum with explicit type arguments.
    fn instantiate_enum_with_args(&mut self, name: &str, type_args: &[Type]) -> Option<EnumInfo> {
        let entry = self.types.get(name)?;
        let TypeEntryKind::Enum { variants } = &entry.kind else {
            return None;
        };

        let type_params = entry.type_params.clone();
        let template_ty = entry.ty.clone();
        let template_variants = variants.clone();

        Self::apply_enum_substitution(&type_params, type_args, &template_ty, &template_variants)
    }

    /// Substitute type parameters with concrete args in an enum template,
    /// returning the instantiated `EnumInfo`.
    fn apply_enum_substitution(
        type_params: &[TypeParam],
        type_args: &[Type],
        template_ty: &Type,
        template_variants: &[EnumVariantInfo],
    ) -> Option<EnumInfo> {
        let mapping: HashMap<TypeVarId, Type> = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(param, arg)| (param.id, arg.clone()))
            .collect();

        let mut ty = substitute_type(template_ty, &mapping);
        if let Type::Enum(ref mut enum_type) = ty {
            enum_type.type_args = type_args.to_vec();
        }
        let variants = Self::substitute_variants(template_variants, &mapping);

        Some(EnumInfo { ty, variants })
    }

    /// Apply a type variable substitution to enum variant info.
    fn substitute_variants(
        variants: &[EnumVariantInfo],
        mapping: &HashMap<TypeVarId, Type>,
    ) -> Vec<EnumVariantInfo> {
        variants
            .iter()
            .map(|v| EnumVariantInfo {
                name: v.name.clone(),
                kind: match &v.kind {
                    EnumVariantInfoKind::Unit => EnumVariantInfoKind::Unit,
                    EnumVariantInfoKind::Tuple(types) => EnumVariantInfoKind::Tuple(
                        types.iter().map(|t| substitute_type(t, mapping)).collect(),
                    ),
                    EnumVariantInfoKind::Struct(fields) => EnumVariantInfoKind::Struct(
                        fields
                            .iter()
                            .map(|f| StructFieldInfo {
                                name: f.name.clone(),
                                ty: substitute_type(&f.ty, mapping),
                                span: f.span,
                            })
                            .collect(),
                    ),
                },
            })
            .collect()
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
        expected_ty: Type,
        value_span: Span,
    ) -> Result<(TypedPattern, Vec<InferenceTree>), TypeError> {
        match pattern {
            Pattern::Binding(ident) => {
                self.bind_pattern_identifier(env, ident, expected_ty.clone())?;
                Ok((TypedPattern::Binding(ident.clone()), Vec::new()))
            }
            Pattern::Wildcard { .. } => {
                // Wildcard matches anything but doesn't introduce a binding
                Ok((TypedPattern::Wildcard, Vec::new()))
            }
            Pattern::Literal { value, span } => {
                // Literal patterns must match the expected type
                let literal_ty = match value {
                    Literal::Integer(_) => Type::int(),
                    Literal::Boolean(_) => Type::Bool,
                    Literal::Unit => Type::Unit,
                };
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    literal_ty.clone(),
                    value_span,
                    *span,
                    TypeErrorKind::GeneralMismatch {
                        expected: self.apply_for_display(&expected_ty),
                        found: literal_ty.clone(),
                    },
                )?;
                Ok((TypedPattern::Literal(value.clone()), vec![unify_trace]))
            }
            Pattern::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                let info = self.lookup_enum_info(enum_name)?;
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    info.ty.clone(),
                    value_span,
                    enum_name.span(),
                    TypeErrorKind::PatternEnumMismatch {
                        enum_name: enum_name.name.clone(),
                        found: self.apply_for_display(&expected_ty),
                    },
                )?;
                let mut traces = vec![unify_trace];

                let variant_info = info
                    .variants
                    .iter()
                    .find(|v| v.name.name == variant.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownEnumVariant {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                            },
                            variant.span(),
                        )
                    })?;

                let typed_payload = match (payload, &variant_info.kind) {
                    (EnumPatternPayload::Unit, EnumVariantInfoKind::Unit) => {
                        TypedEnumPatternPayload::Unit
                    }
                    (EnumPatternPayload::Tuple(patterns), EnumVariantInfoKind::Tuple(expected)) => {
                        if patterns.len() != expected.len() {
                            return Err(TypeError::new(
                                TypeErrorKind::EnumPayloadMismatch {
                                    enum_name: enum_name.name.clone(),
                                    variant_name: variant.name.clone(),
                                    expected: EnumPayloadKind::tuple(expected.len()),
                                    found: EnumPayloadKind::tuple(patterns.len()),
                                },
                                variant.span(),
                            ));
                        }

                        let mut typed = Vec::with_capacity(patterns.len());
                        for (pattern, ty) in patterns.iter().zip(expected.iter()) {
                            let (typed_pattern, mut pattern_traces) =
                                self.infer_pattern(env, pattern, ty.clone(), value_span)?;
                            traces.append(&mut pattern_traces);
                            typed.push(typed_pattern);
                        }
                        TypedEnumPatternPayload::Tuple(typed)
                    }
                    (EnumPatternPayload::Struct(fields), EnumVariantInfoKind::Struct(expected)) => {
                        let mut expected_fields = expected
                            .iter()
                            .map(|field| (field.name.name.clone(), field.clone()))
                            .collect::<HashMap<_, _>>();
                        let mut seen = HashMap::new();
                        let mut typed_fields = Vec::with_capacity(fields.len());
                        let struct_name =
                            format!("{}::{}", enum_name.name.clone(), variant.name.clone());

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
                                expected_fields.remove(&field.name.name).ok_or_else(|| {
                                    TypeError::new(
                                        TypeErrorKind::UnknownStructField {
                                            struct_name: struct_name.clone(),
                                            field_name: field.name.name.clone(),
                                        },
                                        field.name.span(),
                                    )
                                })?;

                            let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                                env,
                                &field.pattern,
                                expected_field.ty.clone(),
                                value_span,
                            )?;
                            traces.append(&mut pattern_traces);
                            typed_fields.push(TypedStructPatternField {
                                name: field.name.clone(),
                                pattern: Box::new(typed_pattern),
                            });
                        }

                        if let Some((missing_field, _)) = expected_fields.into_iter().next() {
                            return Err(TypeError::new(
                                TypeErrorKind::MissingStructField {
                                    struct_name,
                                    field_name: missing_field,
                                },
                                variant.span(),
                            ));
                        }

                        TypedEnumPatternPayload::Struct(typed_fields)
                    }
                    (found_payload, expected_kind) => {
                        return Err(TypeError::new(
                            TypeErrorKind::EnumPayloadMismatch {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                                expected: enum_payload_kind_from_variant(expected_kind),
                                found: enum_payload_kind_from_pattern(found_payload),
                            },
                            variant.span(),
                        ));
                    }
                };

                Ok((
                    TypedPattern::EnumVariant {
                        enum_name: enum_name.clone(),
                        variant: variant.clone(),
                        payload: typed_payload,
                    },
                    traces,
                ))
            }
            Pattern::Struct { name, fields } => {
                // TODO: support ScopedName here
                let info = self.lookup_struct_info(std::slice::from_ref(name))?.clone();
                let (.., unify_trace) = self.unify(
                    expected_ty.clone(),
                    info.ty.clone(),
                    value_span,
                    name.span(),
                    TypeErrorKind::GeneralMismatch {
                        expected: info.ty.clone(),
                        found: expected_ty.clone(),
                    },
                )?;
                let mut traces = vec![unify_trace];

                let mut expected_fields = info
                    .record_ty()
                    .fields
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
                                    struct_name: name.name.clone(),
                                    field_name: field.name.name.clone(),
                                },
                                field.name.span(),
                            )
                        })?;

                    let (typed_pattern, mut pattern_traces) = self.infer_pattern(
                        env,
                        &field.pattern,
                        expected_field.ty.clone(),
                        value_span,
                    )?;
                    traces.append(&mut pattern_traces);
                    typed_fields.push(TypedStructPatternField {
                        name: field.name.clone(),
                        pattern: Box::new(typed_pattern),
                    });
                }

                if let Some((field_name, _)) = expected_fields.into_iter().next() {
                    return Err(TypeError::new(
                        TypeErrorKind::MissingStructField {
                            struct_name: name.name.clone(),
                            field_name,
                        },
                        name.span(),
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
        }
    }

    /// Type-check a top-level definition.
    fn infer_definition(
        &mut self,
        env: &mut TypeEnv,
        definition: &Definition,
    ) -> Result<(TypedDefinition, InferenceTree), TypeError> {
        match definition {
            Definition::Contract => Ok((TypedDefinition::Contract, InferenceTree::default())),
            Definition::Import(import) => {
                self.register_import(env, import)?;
                let typed = self.build_typed_import(import);
                Ok((TypedDefinition::Import(typed), InferenceTree::default()))
            }
            Definition::Function(function) => {
                let (typed_function, trace) = self.infer_function(env, function)?;

                Ok((TypedDefinition::Function(typed_function), trace))
            }
            Definition::Struct(def) => {
                let typed = self.build_typed_struct(def)?;

                Ok((TypedDefinition::Struct(typed), InferenceTree::default()))
            }
            Definition::Enum(def) => {
                let typed = self.build_typed_enum(def)?;

                Ok((TypedDefinition::Enum(typed), InferenceTree::default()))
            }
            Definition::Utxo(def) => {
                let (utxo, trace) = self.infer_utxo(env, def)?;

                Ok((TypedDefinition::Utxo(utxo), trace))
            }
            Definition::Token(def) => {
                // Parse-only for now: lower to a shallow marker without
                // type-checking the body. Semantics land in a follow-up.
                let typed = TypedTokenDef {
                    name: def.name.clone(),
                    span: def.name.span(),
                };

                Ok((TypedDefinition::Token(typed), InferenceTree::default()))
            }
            Definition::Abi(def) => {
                let typed = self.build_typed_abi(def)?;

                Ok((TypedDefinition::Abi(typed), InferenceTree::default()))
            }
        }
    }

    fn infer_function(
        &mut self,
        env: &mut TypeEnv,
        function: &FunctionDef,
    ) -> Result<(TypedFunctionDef, InferenceTree), TypeError> {
        // Visit param & return types.
        let param_types = function
            .params
            .iter()
            .map(|param| self.type_from_annotation(&param.ty))
            .collect::<Result<Vec<_>, _>>()?;
        let (expected_return, return_span) = match &function.return_type {
            Some(annotation) => (
                self.type_from_annotation(annotation)?,
                annotation.name.span_or(function.name.span()),
            ),
            None => (Type::unit(), function.name.span()),
        };

        // Insert function into environment. Happens before code so that recursion is allowed.
        if let Some(existing) = env.get_in_current_scope(function.name.as_str()) {
            return Err(TypeError::new(
                TypeErrorKind::FunctionAlreadyDefined {
                    name: function.name.to_string(),
                },
                function.name.span,
            )
            .with_secondary(existing.decl_span, "previously defined here"));
        }
        let param_spans = function
            .params
            .iter()
            .map(|param| param.ty.name.span)
            .collect::<Vec<_>>();
        env.insert(
            function.name.to_string(),
            Binding {
                decl_span: function.name.span,
                mutable: false,
                scheme: Scheme::monomorphic(Type::Function {
                    params: param_types.clone(),
                    param_spans,
                    result: Box::new(expected_return.clone()),
                    kind: FunctionKind::Normal,
                    name_span: function.name.span,
                    callee: None,
                }),
                class: BindingClass::Local,
                visibility: BindingVisibility::Private,
            },
        );

        env.push_scope();
        let mut typed_params = Vec::with_capacity(function.params.len());
        let mut private_param_decl_spans = Vec::new();
        for (param, ty) in function.params.iter().zip(param_types) {
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
                ty,
            });
        }

        let mut ctx = FunctionCtx {
            expected_return: expected_return.clone(),
            return_span,
            saw_return: false,
            private_param_decl_spans,
            is_coroutine: function.export == Some(starstream_types::FunctionExport::UtxoMain),
        };

        let (typed_body, body_traces) = self.infer_block(env, &function.body, &mut ctx, true)?;

        env.pop_scope();

        if expected_return != Type::unit()
            && !ctx.saw_return
            && typed_body.tail_expression.is_none()
        {
            return Err(TypeError::new(
                TypeErrorKind::MissingReturn {
                    expected: expected_return,
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
                params: typed_params,
                return_type: ctx.expected_return,
                body: typed_body,
            },
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
                    let expected_type = self.type_from_annotation(ty)?;
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
                        TypeErrorKind::UnknownVariable {
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
                let tree = self.make_trace("T-Resume", env_context, stmt_repr, result_repr, || {
                    vec![unify_trace]
                });
                Ok((TypedStatement::Resume, tree))
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
                            self.int_literal_values.insert(*id, (*value, expr.span));
                        }
                        (
                            ty,
                            TypedExprKind::Literal(Literal::Integer(*value)),
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
                let ty = if let Some(binding) = env.get_scoped(name).cloned() {
                    self.instantiate(&binding.scheme)
                } else {
                    let span = name.last().unwrap().span_or(expr.span);
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownVariable {
                            name: name.last().unwrap().to_string(),
                        },
                        span,
                    ));
                };
                let typed = Spanned::new(
                    TypedExpr::new(ty.clone(), TypedExprKind::ScopedName(name.clone())),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&ty));
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
                                left_display.to_compact_string()
                            ))
                            .with_secondary(
                                right_label_span,
                                format!("has type `{}`", right_display.to_compact_string()),
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
                                left_display.to_compact_string()
                            ))
                            .with_secondary(
                                right_label_span,
                                format!("has type `{}`", right_display.to_compact_string()),
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
                let info = self.lookup_struct_info(name)?.clone();
                let mut expected = info
                    .record_ty()
                    .fields
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
                        info.ty.clone(),
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
                        .into_iter()
                        .find(|entry| entry.name.as_str() == field.name.as_str())
                        .map(|entry| entry.ty)
                        .ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::FieldAccessUnknownField {
                                    field_name: field.name.clone(),
                                    ty: target_ty.clone(),
                                },
                                field.span(),
                            )
                        })?,
                    Type::AbiNarrow(abi) => {
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
                        Type::Function {
                            params: method.params.iter().map(|p| p.ty.clone()).collect(),
                            param_spans: method.params.iter().map(|p| p.name.span).collect(),
                            result: Box::new(method.return_type.clone()),
                            kind: FunctionKind::Normal,
                            name_span: method.name.span,
                            callee: None,
                        }
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
            /*
            Expr::EnumConstructor {
                enum_name,
                variant,
                payload,
            } => {
                // First, check if this is a namespace-qualified function call
                // e.g., `cardano::blockHeight()` where `cardano` is an imported namespace
                let ns_func_info = self
                    .namespaces
                    .get(&enum_name.name)
                    .and_then(|funcs| funcs.get(&variant.name))
                    .cloned();

                if let Some(func_info) = ns_func_info {
                    // This is a namespace call
                    // Found the function in the namespace
                    let args: &[Spanned<Expr>] = match payload {
                        EnumConstructorPayload::Tuple(args) => args,
                        EnumConstructorPayload::Unit => {
                            // Unit payload means no-arg call: `cardano::blockHeight`
                            // Treat as zero-arg call
                            &[]
                        }
                        EnumConstructorPayload::Struct(_) => {
                            return Err(TypeError::new(
                                TypeErrorKind::ArityMismatch {
                                    expected: func_info.param_types.len(),
                                    found: 0,
                                },
                                expr.span,
                            )
                            .with_help(
                                "namespace functions use tuple-style arguments, not struct syntax",
                            ));
                        }
                    };

                    let param_types = &func_info.param_types;
                    let return_type = &func_info.return_type;
                    let kind = func_info.kind;

                    // Check effect: runtime functions need `runtime` keyword
                    if kind == FunctionKind::Runtime && !ctx.inside_runtime {
                        return Err(TypeError::new(
                            TypeErrorKind::RuntimeWithoutKeyword {
                                function_name: variant.name.clone(),
                            },
                            variant.span_or(expr.span),
                        )
                        .with_help(format!(
                            "use `runtime` to call runtime functions, e.g., `runtime {}::{}()`",
                            enum_name.name, variant.name
                        )));
                    }

                    // Check effect: effectful functions need `raise` keyword
                    if kind == FunctionKind::Raise && !ctx.inside_raise {
                        return Err(TypeError::new(
                            TypeErrorKind::EffectfulWithoutRaise {
                                function_name: variant.name.clone(),
                            },
                            variant.span_or(expr.span),
                        )
                        .with_help(format!(
                            "use `raise` to call effectful functions, e.g., `raise {}::{}()`",
                            enum_name.name, variant.name
                        )));
                    }

                    // Check arity
                    if args.len() != param_types.len() {
                        return Err(TypeError::new(
                            TypeErrorKind::ArityMismatch {
                                expected: param_types.len(),
                                found: args.len(),
                            },
                            expr.span,
                        ));
                    }

                    // Typecheck arguments
                    let mut children = Vec::new();
                    let mut typed_args = Vec::with_capacity(args.len());

                    for (index, (arg, expected_ty)) in
                        args.iter().zip(param_types.iter()).enumerate()
                    {
                        let (typed_arg, arg_trace) = self.infer_expr(env, arg, ctx)?;
                        let actual_ty = typed_arg.node.ty.clone();

                        let (_, unify_trace) = self.unify(
                            actual_ty.clone(),
                            expected_ty.clone(),
                            arg.span,
                            arg.span,
                            TypeErrorKind::ArgumentTypeMismatch {
                                expected: expected_ty.clone(),
                                found: self.apply_for_display(&actual_ty),
                                position: index + 1,
                                param_span: func_info.param_spans.get(index).copied(),
                            },
                        )?;

                        children.push(arg_trace);
                        children.push(unify_trace);
                        typed_args.push(typed_arg);
                    }

                    // Build the typed call - we use TypedExprKind::Call with a synthetic callee
                    let callee_ty = Type::Function {
                        params: param_types.clone(),
                        param_spans: Vec::new(),
                        result: Box::new(return_type.clone()),
                        kind,
                        name_span: func_info.name_span,
                    };

                    let callee_ident = Identifier::new(
                        format!("{}::{}", enum_name.name, variant.name),
                        variant.span(),
                    );

                    let typed_callee = Spanned::new(
                        TypedExpr::new(callee_ty, TypedExprKind::Identifier(callee_ident)),
                        variant.span_or(expr.span),
                    );

                    let typed = Spanned::new(
                        TypedExpr::new(
                            return_type.clone(),
                            TypedExprKind::Call {
                                callee: Box::new(typed_callee),
                                args: typed_args,
                            },
                        ),
                        expr.span,
                    );

                    let result_repr = self.maybe_string(|| self.format_type(return_type));
                    let tree = self.make_trace(
                        "T-NamespaceCall",
                        env_context,
                        subject_repr,
                        result_repr,
                        || children,
                    );

                    return Ok((typed, tree));
                } else if self.namespaces.contains(&enum_name.name) {
                    // Namespace exists but function not found
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownImportFunction {
                            path: enum_name.name.clone(),
                            name: variant.name.clone(),
                        },
                        variant.span(),
                    ));
                }

                // Not a namespace - check for enum
                // Use casing heuristic for better error messages
                let is_likely_namespace = enum_name
                    .name
                    .chars()
                    .next()
                    .map(|c| c.is_lowercase())
                    .unwrap_or(false);

                let info = self.instantiate_enum_info(&enum_name.name).ok_or_else(|| {
                    if is_likely_namespace {
                        TypeError::new(
                            TypeErrorKind::UnknownImportPackage {
                                namespace: enum_name.name.clone(),
                                package: "".to_string(),
                            },
                            enum_name.span(),
                        )
                        .with_primary_message(format!("unknown namespace `{}`", enum_name.name))
                        .with_help(format!(
                            "did you forget to import? try: `import {} from starstream:std/{};`",
                            enum_name.name, enum_name.name
                        ))
                    } else {
                        TypeError::new(
                            TypeErrorKind::UnknownEnum {
                                name: enum_name.name.clone(),
                            },
                            enum_name.span(),
                        )
                    }
                })?;
                let variant_info = info
                    .variants
                    .iter()
                    .find(|entry| entry.name.name == variant.name)
                    .ok_or_else(|| {
                        TypeError::new(
                            TypeErrorKind::UnknownEnumVariant {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                            },
                            variant.span(),
                        )
                    })?;

                let mut children = Vec::new();
                let typed_payload = match (&payload, &variant_info.kind) {
                    (EnumConstructorPayload::Unit, EnumVariantInfoKind::Unit) => {
                        TypedEnumConstructorPayload::Unit
                    }
                    (
                        EnumConstructorPayload::Tuple(values),
                        EnumVariantInfoKind::Tuple(expected),
                    ) => {
                        if values.len() != expected.len() {
                            return Err(TypeError::new(
                                TypeErrorKind::EnumPayloadMismatch {
                                    enum_name: enum_name.name.clone(),
                                    variant_name: variant.name.clone(),
                                    expected: EnumPayloadKind::tuple(expected.len()),
                                    found: EnumPayloadKind::tuple(values.len()),
                                },
                                variant.span(),
                            ));
                        }

                        let mut typed_values = Vec::with_capacity(values.len());
                        for (expr, expected_ty) in values.iter().zip(expected.iter()) {
                            let (typed_expr, value_trace) = self.infer_expr(env, expr, ctx)?;
                            let actual_ty = typed_expr.node.ty.clone();
                            let (_, unify_trace) = self.unify(
                                actual_ty.clone(),
                                expected_ty.clone(),
                                expr.span,
                                variant.span_or(expr.span),
                                TypeErrorKind::GeneralMismatch {
                                    expected: expected_ty.clone(),
                                    found: self.apply_for_display(&actual_ty),
                                },
                            )?;
                            children.push(value_trace);
                            children.push(unify_trace);
                            typed_values.push(typed_expr);
                        }
                        TypedEnumConstructorPayload::Tuple(typed_values)
                    }
                    (
                        EnumConstructorPayload::Struct(fields),
                        EnumVariantInfoKind::Struct(expected),
                    ) => {
                        let mut expected_fields = expected
                            .iter()
                            .map(|field| (field.name.name.clone(), field.clone()))
                            .collect::<HashMap<_, _>>();
                        let mut seen = HashMap::new();
                        let mut typed_fields = Vec::with_capacity(fields.len());
                        let struct_name =
                            format!("{}::{}", enum_name.name.clone(), variant.name.clone());

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
                                expected_fields.remove(&field.name.name).ok_or_else(|| {
                                    TypeError::new(
                                        TypeErrorKind::UnknownStructField {
                                            struct_name: struct_name.clone(),
                                            field_name: field.name.name.clone(),
                                        },
                                        field.name.span(),
                                    )
                                })?;

                            let (typed_value, value_trace) =
                                self.infer_expr(env, &field.value, ctx)?;
                            let actual_ty = typed_value.node.ty.clone();
                            let (_, unify_trace) = self.unify(
                                actual_ty.clone(),
                                expected_field.ty.clone(),
                                field.value.span,
                                field.name.span_or(field.value.span),
                                TypeErrorKind::GeneralMismatch {
                                    expected: expected_field.ty.clone(),
                                    found: self.apply_for_display(&actual_ty),
                                },
                            )?;
                            children.push(value_trace);
                            children.push(unify_trace);

                            typed_fields.push(TypedStructLiteralField {
                                name: field.name.clone(),
                                value: typed_value,
                            });
                        }

                        if let Some((missing_field, _)) = expected_fields.into_iter().next() {
                            return Err(TypeError::new(
                                TypeErrorKind::MissingStructField {
                                    struct_name,
                                    field_name: missing_field,
                                },
                                variant.span(),
                            ));
                        }

                        TypedEnumConstructorPayload::Struct(typed_fields)
                    }
                    (found_payload, expected_kind) => {
                        return Err(TypeError::new(
                            TypeErrorKind::EnumPayloadMismatch {
                                enum_name: enum_name.name.clone(),
                                variant_name: variant.name.clone(),
                                expected: enum_payload_kind_from_variant(expected_kind),
                                found: enum_payload_kind_from_constructor(found_payload),
                            },
                            variant.span(),
                        ));
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        info.ty.clone(),
                        TypedExprKind::EnumConstructor {
                            enum_name: enum_name.clone(),
                            variant: variant.clone(),
                            payload: typed_payload,
                        },
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree =
                    self.make_trace("T-EnumCtor", env_context, subject_repr, result_repr, || {
                        children
                    });
                Ok((typed, tree))
            }
            */
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
                                    TypeErrorKind::UnknownVariable {
                                        name: name.name.clone(),
                                    },
                                    name.span(),
                                )
                            })?;
                            let var_ty = self.apply(&binding.scheme.ty);

                            match &var_ty {
                                Type::UtxoAny | Type::UtxoNamed(_) => {}
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

                            let Some(abi) = self.abis.get(&abi_name.name) else {
                                return Err(TypeError::new(
                                    TypeErrorKind::UnknownAbi {
                                        name: abi_name.name.clone(),
                                    },
                                    abi_name.span(),
                                ));
                            };

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
                                    scheme: Scheme::monomorphic(Type::AbiNarrow(abi.clone())),
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
                        typed_scrutinee.node.ty.clone(),
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
                self.has_yields = true;
                // Assert that we are inside a `main fn`.
                if !ctx.is_coroutine {
                    return Err(TypeError::new(TypeErrorKind::YieldOutsideMainFn, expr.span));
                }
                // TODO: assert that this utxo impls each abi named
                let abis = abis
                    .iter()
                    .map(|abi| {
                        let abi = self.abis.get(abi.as_str()).ok_or_else(|| {
                            TypeError::new(
                                TypeErrorKind::UnknownAbi {
                                    name: abi.to_string(),
                                },
                                expr.span,
                            )
                        })?;
                        Ok(abi.clone())
                    })
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

        let Type::Function {
            params: ref param_types,
            ref param_spans,
            result: ref return_type,
            kind,
            name_span: _,
            callee: _,
        } = callee_ty
        else {
            return Err(TypeError::new(
                TypeErrorKind::NotAFunction { found: callee_ty },
                callee.span,
            ));
        };

        // Check kind: `event` requires `emit`; `effect` requires `raise`; runtime fns require `runtime`
        if used_kind != kind {
            if used_kind == FunctionKind::Normal {
                // Called without keyword, a keyword is required
                return Err(TypeError::new(
                    TypeErrorKind::EmitRaiseRuntimeNeeded {
                        function_name: callee_name.to_owned(),
                        needed_keyword: kind,
                    },
                    callee.span,
                ));
            } else if kind == FunctionKind::Normal {
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
                        needed_keyword: kind,
                        wrong_keyword: used_kind,
                    },
                    callee.span,
                ));
            }
        }

        // Check linearity: if the callee is a field access on an AbiNarrow target,
        // enforce the one-method-call-per-block constraint.
        if let TypedExprKind::FieldAccess { target, .. } = &typed_callee.node.kind
            && let Type::AbiNarrow(_) = &target.node.ty
            && let TypedExprKind::ScopedName(name) = &target.node.kind
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

        if args.len() != param_types.len() {
            return Err(TypeError::new(
                TypeErrorKind::ArityMismatch {
                    expected: param_types.len(),
                    found: args.len(),
                },
                expr.span,
            ));
        }

        let mut children = vec![callee_trace];
        let mut typed_args = Vec::with_capacity(args.len());

        for (index, (arg, expected_ty)) in args.iter().zip(param_types.iter()).enumerate() {
            let (typed_arg, arg_trace) = self.infer_expr(env, arg, ctx)?;
            let actual_ty = typed_arg.node.ty.clone();

            let param_span = param_spans.get(index).copied();

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
                (**return_type).clone(),
                TypedExprKind::Call {
                    callee: Box::new(typed_callee),
                    args: typed_args,
                },
            ),
            callee.span,
        );

        let result_repr = self.maybe_string(|| self.format_type(return_type));

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
        let binding = env.get_scoped(name)?;
        if ctx.private_param_decl_spans.contains(&binding.decl_span) {
            Some(name.last().unwrap().to_string())
        } else {
            None
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn source_expr_visibility(&self, env: &TypeEnv, expr: &Spanned<Expr>) -> BindingVisibility {
        match &expr.node {
            Expr::Literal(_) => BindingVisibility::Public,
            Expr::ScopedName(name) => env
                .get_scoped(&name)
                .map(|binding| binding.visibility)
                .unwrap_or(BindingVisibility::Private),
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
            /*
            Expr::EnumConstructor { payload, .. } => match payload {
                EnumConstructorPayload::Unit => BindingVisibility::Public,
                EnumConstructorPayload::Tuple(values) => {
                    if values.iter().all(|value| {
                        self.source_expr_visibility(env, value) == BindingVisibility::Public
                    }) {
                        BindingVisibility::Public
                    } else {
                        BindingVisibility::Private
                    }
                }
                EnumConstructorPayload::Struct(fields) => {
                    if fields.iter().all(|field| {
                        self.source_expr_visibility(env, &field.value) == BindingVisibility::Public
                    }) {
                        BindingVisibility::Public
                    } else {
                        BindingVisibility::Private
                    }
                }
            },
            */
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

    fn type_from_annotation(&mut self, annotation: &TypeAnnotation) -> Result<Type, TypeError> {
        if !annotation.generics.is_empty() {
            let name = &annotation.name.name;

            // Check if this type exists and has type_params
            let (has_params, param_count) = match self.types.get(name) {
                Some(entry) => (!entry.type_params.is_empty(), entry.type_params.len()),
                None => (false, 0),
            };

            if !has_params {
                return Err(TypeError::new(
                    TypeErrorKind::UnsupportedTypeFeature {
                        description: "generic type parameters are not supported yet".to_string(),
                    },
                    annotation.name.span(),
                )
                .with_help("remove `<...>` until generics are implemented"));
            }

            // Check arity
            if annotation.generics.len() != param_count {
                return Err(TypeError::new(
                    TypeErrorKind::WrongGenericArity {
                        type_name: name.clone(),
                        expected: param_count,
                        found: annotation.generics.len(),
                    },
                    annotation.name.span(),
                ));
            }

            // Resolve each generic arg
            let type_args: Vec<Type> = annotation
                .generics
                .iter()
                .map(|g| self.type_from_annotation(g))
                .collect::<Result<_, _>>()?;

            let info = self.instantiate_enum_with_args(name, &type_args).unwrap();
            return Ok(info.ty);
        }

        match annotation.name.name.as_str() {
            "i8" => Ok(Type::int_of(IntWidth::I8)),
            "i16" => Ok(Type::int_of(IntWidth::I16)),
            "i32" => Ok(Type::int_of(IntWidth::I32)),
            "i64" => Ok(Type::int()),
            "u8" => Ok(Type::int_of(IntWidth::U8)),
            "u16" => Ok(Type::int_of(IntWidth::U16)),
            "u32" => Ok(Type::int_of(IntWidth::U32)),
            "u64" => Ok(Type::int_of(IntWidth::U64)),
            "bool" => Ok(Type::bool()),
            "()" => Ok(Type::unit()),
            "_" => Ok(self.fresh_var()),
            "Utxo" => Ok(Type::UtxoAny),
            other => match self.types.get(other) {
                Some(entry) if !entry.type_params.is_empty() => Err(TypeError::new(
                    TypeErrorKind::WrongGenericArity {
                        type_name: other.to_string(),
                        expected: entry.type_params.len(),
                        found: 0,
                    },
                    annotation.name.span(),
                )),
                Some(entry) => Ok(entry.ty.clone()),
                None => Err(TypeError::new(
                    TypeErrorKind::UnknownTypeAnnotation {
                        name: other.to_string(),
                    },
                    annotation.name.span(),
                )),
            },
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
            .with_primary_message(format!("has type `{}`", left_display.to_compact_string()))
            .with_secondary(
                right_span,
                format!("has type `{}`", right_display.to_compact_string()),
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
            .with_primary_message(format!("has type `{}`", left_display.to_compact_string()))
            .with_secondary(
                right_span,
                format!("has type `{}`", right_display.to_compact_string()),
            ))
        }
    }

    /// Normalize a type for use in user-facing error messages.
    ///
    /// Like [`apply`], but also defaults unresolved int-constrained type
    /// variables to `i64` so that error messages show a concrete type name
    /// instead of an internal type variable like `t3`.
    fn apply_for_display(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => match self.subst.get(id) {
                Some(ty) => self.apply_for_display(ty),
                None if self.int_vars.contains(id) => Type::int(),
                None => Type::Var(*id),
            },
            Type::Function {
                params,
                param_spans,
                result,
                kind,
                name_span,
                callee,
            } => Type::Function {
                params: params.iter().map(|t| self.apply_for_display(t)).collect(),
                param_spans: param_spans.clone(),
                result: Box::new(self.apply_for_display(result)),
                kind: *kind,
                name_span: *name_span,
                callee: callee.clone(),
            },
            Type::Tuple(items) => {
                Type::Tuple(items.iter().map(|t| self.apply_for_display(t)).collect())
            }
            Type::Record(record) => Type::Record(RecordType {
                name: record.name.clone(),
                fields: record
                    .fields
                    .iter()
                    .map(|field| TypeRecordField {
                        name: field.name.clone(),
                        ty: self.apply_for_display(&field.ty),
                    })
                    .collect(),
            }),
            Type::Enum(enum_type) => Type::Enum(EnumType {
                name: enum_type.name.clone(),
                type_args: enum_type
                    .type_args
                    .iter()
                    .map(|t| self.apply_for_display(t))
                    .collect(),
                variants: enum_type
                    .variants
                    .iter()
                    .map(|variant| TypeEnumVariant {
                        name: variant.name.clone(),
                        kind: match &variant.kind {
                            TypeEnumVariantKind::Unit => TypeEnumVariantKind::Unit,
                            TypeEnumVariantKind::Tuple(payload) => TypeEnumVariantKind::Tuple(
                                payload
                                    .iter()
                                    .map(|ty| self.apply_for_display(ty))
                                    .collect(),
                            ),
                            TypeEnumVariantKind::Struct(fields) => TypeEnumVariantKind::Struct(
                                fields
                                    .iter()
                                    .map(|field| {
                                        TypeRecordField::new(
                                            field.name.clone(),
                                            self.apply_for_display(&field.ty),
                                        )
                                    })
                                    .collect(),
                            ),
                        },
                    })
                    .collect(),
            }),
            _ => ty.clone(),
        }
    }

    /// Fully normalize a type by applying the current substitution set.
    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => match self.subst.get(id) {
                Some(ty) => self.apply(ty),
                None => Type::Var(*id),
            },
            Type::Function {
                params,
                param_spans,
                result,
                kind,
                name_span,
                callee,
            } => Type::Function {
                params: params.iter().map(|t| self.apply(t)).collect(),
                param_spans: param_spans.clone(),
                result: Box::new(self.apply(result)),
                kind: *kind,
                name_span: *name_span,
                callee: callee.clone(),
            },
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| self.apply(t)).collect()),
            Type::Record(record) => Type::Record(RecordType {
                name: record.name.clone(),
                fields: record
                    .fields
                    .iter()
                    .map(|field| TypeRecordField {
                        name: field.name.clone(),
                        ty: self.apply(&field.ty),
                    })
                    .collect(),
            }),
            Type::Enum(enum_type) => Type::Enum(EnumType {
                name: enum_type.name.clone(),
                variants: enum_type
                    .variants
                    .iter()
                    .map(|variant| TypeEnumVariant {
                        name: variant.name.clone(),
                        kind: match &variant.kind {
                            TypeEnumVariantKind::Unit => TypeEnumVariantKind::Unit,
                            TypeEnumVariantKind::Tuple(payload) => TypeEnumVariantKind::Tuple(
                                payload.iter().map(|ty| self.apply(ty)).collect(),
                            ),
                            TypeEnumVariantKind::Struct(fields) => TypeEnumVariantKind::Struct(
                                fields
                                    .iter()
                                    .map(|field| {
                                        TypeRecordField::new(
                                            field.name.clone(),
                                            self.apply(&field.ty),
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
                    .map(|ty| self.apply(ty))
                    .collect(),
            }),
            Type::Int(w) => Type::Int(*w),
            Type::Bool => Type::Bool,
            Type::Unit => Type::Unit,
            Type::UtxoAny => Type::UtxoAny,
            Type::UtxoNamed(id) => Type::UtxoNamed(id.clone()),
            Type::AbiNarrow(name) => Type::AbiNarrow(name.clone()),
        }
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
            TypedDefinition::Import(_)
            | TypedDefinition::Struct(_)
            | TypedDefinition::Enum(_)
            | TypedDefinition::Abi(_)
            | TypedDefinition::Token(_)
            | TypedDefinition::Contract => {}
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
                    *abi = self.apply(abi);
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
            TypedExprKind::Literal(_) | TypedExprKind::ScopedName(_) => {}
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
        substitute_type(&scheme.ty, &mapping)
    }

    /// Allocate a new inference variable unique to this inferencer.
    fn fresh_var_id(&mut self) -> TypeVarId {
        let var = TypeVarId(self.next_type_var);
        self.next_type_var += 1;
        var
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
        for (&id, &(value, span)) in &self.int_literal_values {
            let resolved = self.apply(&Type::Var(id));
            if let Type::Int(w) = resolved
                && !w.fits(value)
            {
                errors.push(TypeError::new(
                    TypeErrorKind::LiteralOutOfRange {
                        value,
                        ty: Type::Int(w),
                    },
                    span,
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
                    format!("{name}: {}", display_ty.to_compact_string())
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
        self.apply_for_display(ty).to_compact_string()
    }

    /// Format the difference between the stored substitution map and a prior snapshot.
    fn format_subst_diff(&self, before: &HashMap<TypeVarId, Type>) -> String {
        let mut entries = Vec::new();
        for (key, value) in &self.subst {
            if before.get(key) != Some(value) {
                entries.push(format!("{}/{}", self.format_type(value), key.as_str()));
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
        match (left.clone(), right.clone()) {
            (Type::Int(w1), Type::Int(w2)) if w1 == w2 => {
                Ok((Type::Int(w1), Vec::new(), "Unify-Const"))
            }
            (Type::Bool, Type::Bool) => Ok((Type::Bool, Vec::new(), "Unify-Const")),
            (Type::Unit, Type::Unit) => Ok((Type::Unit, Vec::new(), "Unify-Const")),
            (Type::Tuple(ls), Type::Tuple(rs)) if ls.len() == rs.len() => {
                let mut children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                Ok((Type::Tuple(ls), children, "Unify-Tuple"))
            }
            (
                Type::Function {
                    params: lp,
                    param_spans: lps,
                    result: lr,
                    kind: le,
                    name_span: lns,
                    callee: lcl,
                },
                Type::Function {
                    params: rp,
                    result: rr,
                    callee: rcl,
                    ..
                },
            ) if lp.len() == rp.len() => {
                let mut children = Vec::new();
                for (l, r) in lp.iter().zip(rp.iter()) {
                    let (_, child, _) = self.unify_inner(l.clone(), r.clone())?;
                    children.extend(child);
                }
                let (_, ret_child, _) = self.unify_inner((*lr).clone(), (*rr).clone())?;
                children.extend(ret_child);
                Ok((
                    Type::Function {
                        params: lp,
                        param_spans: lps,
                        result: lr,
                        kind: le,
                        name_span: lns,
                        callee: if lcl == rcl { lcl.clone() } else { None },
                    },
                    children,
                    "Unify-Arrow",
                ))
            }
            (Type::Record(ls), Type::Record(rs))
                if ls.name == rs.name && ls.fields.len() == rs.fields.len() =>
            {
                let mut children = Vec::new();
                for (lf, rf) in ls.fields.iter().zip(rs.fields.iter()) {
                    if lf.name != rf.name {
                        return Err(());
                    }
                    let (_, child, _) = self.unify_inner(lf.ty.clone(), rf.ty.clone())?;
                    children.extend(child);
                }
                Ok((Type::Record(ls), children, "Unify-Record"))
            }
            (Type::Enum(mut ls), Type::Enum(mut rs))
                if ls.name == rs.name && ls.variants.len() == rs.variants.len() =>
            {
                ls.variants.sort_by(|a, b| a.name.cmp(&b.name));
                rs.variants.sort_by(|a, b| a.name.cmp(&b.name));
                let mut children = Vec::new();
                for (lv, rv) in ls.variants.iter().zip(rs.variants.iter()) {
                    if lv.name != rv.name {
                        return Err(());
                    }
                    match (&lv.kind, &rv.kind) {
                        (TypeEnumVariantKind::Unit, TypeEnumVariantKind::Unit) => {}
                        (TypeEnumVariantKind::Tuple(lt), TypeEnumVariantKind::Tuple(rt))
                            if lt.len() == rt.len() =>
                        {
                            for (l, r) in lt.iter().zip(rt.iter()) {
                                let (_, c, _) = self.unify_inner(l.clone(), r.clone())?;
                                children.extend(c);
                            }
                        }
                        (TypeEnumVariantKind::Struct(lf), TypeEnumVariantKind::Struct(rf))
                            if lf.len() == rf.len() =>
                        {
                            for (l, r) in lf.iter().zip(rf.iter()) {
                                if l.name != r.name {
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
                            if let Some(val) = self.int_literal_values.get(&id).copied() {
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
                            if let Some(val) = self.int_literal_values.get(&id).copied() {
                                self.int_literal_values.entry(*other_id).or_insert(val);
                            }
                        }
                        _ => return Err(()),
                    }
                }
                self.subst.insert(id, ty.clone());
                Ok((ty, Vec::new(), "Unify-Var"))
            }
            (Type::AbiNarrow(l), Type::AbiNarrow(r)) if l == r => {
                Ok((Type::AbiNarrow(l), Vec::new(), "Unify-Const"))
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

        let (result_ty, children, rule) = match (left.clone(), right.clone()) {
            (Type::Int(w1), Type::Int(w2)) if w1 == w2 => {
                (Type::Int(w1), Vec::new(), "Unify-Const")
            }
            (Type::Bool, Type::Bool) => (Type::Bool, Vec::new(), "Unify-Const"),
            (Type::Unit, Type::Unit) => (Type::Unit, Vec::new(), "Unify-Const"),
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
            (
                Type::Function {
                    params: lp,
                    param_spans: lps,
                    result: lr,
                    kind: le,
                    name_span: lns,
                    callee: lcl,
                },
                Type::Function {
                    params: rp,
                    result: rr,
                    callee: rcl,
                    ..
                },
            ) => {
                if lp.len() != rp.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "function arity mismatch"));
                }

                let mut arrow_children = Vec::new();
                for (l, r) in lp.iter().zip(rp.iter()) {
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
                    (*lr).clone(),
                    (*rr).clone(),
                    left_span,
                    right_span,
                    TypeErrorKind::GeneralMismatch {
                        expected: (*lr).clone(),
                        found: (*rr).clone(),
                    },
                )?;
                arrow_children.push(ret_child);

                (
                    Type::Function {
                        params: lp,
                        param_spans: lps,
                        result: lr,
                        kind: le,
                        name_span: lns,
                        callee: if lcl == rcl { lcl.clone() } else { None },
                    },
                    arrow_children,
                    "Unify-Arrow",
                )
            }
            (Type::Record(mut ls), Type::Record(mut rs)) => {
                ls.fields
                    .sort_by(|a, b| a.name.as_str().cmp(b.name.as_str()));
                rs.fields
                    .sort_by(|a, b| a.name.as_str().cmp(b.name.as_str()));
                if ls.fields.len() != rs.fields.len()
                    || ls
                        .fields
                        .iter()
                        .zip(rs.fields.iter())
                        .any(|(l, r)| l.name != r.name)
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
            (Type::Enum(mut ls), Type::Enum(mut rs)) => {
                ls.variants.sort_by(|a, b| a.name.cmp(&b.name));
                rs.variants.sort_by(|a, b| a.name.cmp(&b.name));
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
                        (TypeEnumVariantKind::Unit, TypeEnumVariantKind::Unit) => {}
                        (
                            TypeEnumVariantKind::Tuple(left_payload),
                            TypeEnumVariantKind::Tuple(right_payload),
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
                            TypeEnumVariantKind::Struct(left_fields),
                            TypeEnumVariantKind::Struct(right_fields),
                        ) => {
                            if left_fields.len() != right_fields.len()
                                || left_fields
                                    .iter()
                                    .zip(right_fields.iter())
                                    .any(|(l, r)| l.name != r.name)
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
            (Type::Var(id), ty) => {
                self.bind(id, ty.clone(), left_span, right_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            (ty, Type::Var(id)) => {
                self.bind(id, ty.clone(), right_span, left_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            _ => {
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
                    if let Some(val) = self.int_literal_values.get(&var).copied() {
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
fn substitute_type(ty: &Type, mapping: &HashMap<TypeVarId, Type>) -> Type {
    match ty {
        Type::Var(id) => mapping.get(id).cloned().unwrap_or(Type::Var(*id)),
        Type::Function {
            params,
            param_spans,
            result,
            kind,
            name_span,
            callee,
        } => Type::Function {
            params: params
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
            param_spans: param_spans.clone(),
            result: Box::new(substitute_type(result, mapping)),
            kind: *kind,
            name_span: *name_span,
            callee: callee.clone(),
        },
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
        ),
        Type::Record(record) => Type::Record(RecordType {
            name: record.name.clone(),
            fields: record
                .fields
                .iter()
                .map(|field| TypeRecordField {
                    name: field.name.clone(),
                    ty: substitute_type(&field.ty, mapping),
                })
                .collect(),
        }),
        Type::Enum(enum_type) => Type::Enum(EnumType {
            name: enum_type.name.clone(),
            variants: enum_type
                .variants
                .iter()
                .map(|variant| TypeEnumVariant {
                    name: variant.name.clone(),
                    kind: match &variant.kind {
                        TypeEnumVariantKind::Unit => TypeEnumVariantKind::Unit,
                        TypeEnumVariantKind::Tuple(payload) => TypeEnumVariantKind::Tuple(
                            payload
                                .iter()
                                .map(|ty| substitute_type(ty, mapping))
                                .collect(),
                        ),
                        TypeEnumVariantKind::Struct(fields) => TypeEnumVariantKind::Struct(
                            fields
                                .iter()
                                .map(|field| {
                                    TypeRecordField::new(
                                        field.name.clone(),
                                        substitute_type(&field.ty, mapping),
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
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
        }),
        Type::Int(w) => Type::Int(*w),
        Type::Bool => Type::Bool,
        Type::Unit => Type::Unit,
        Type::UtxoAny => Type::UtxoAny,
        Type::UtxoNamed(id) => Type::UtxoNamed(id.clone()),
        Type::AbiNarrow(name) => Type::AbiNarrow(name.clone()),
    }
}

/// Return `true` if `var` appears anywhere inside `ty`, expanding substitutions as needed.
fn occurs_in(var: TypeVarId, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> bool {
    match ty {
        Type::Var(id) => {
            if id == &var {
                true
            } else {
                subst
                    .get(id)
                    .map(|ty| occurs_in(var, ty, subst))
                    .unwrap_or(false)
            }
        }
        Type::Function { params, result, .. } => {
            params.iter().any(|t| occurs_in(var, t, subst)) || occurs_in(var, result, subst)
        }
        Type::Tuple(items) => items.iter().any(|t| occurs_in(var, t, subst)),
        Type::Record(record) => record
            .fields
            .iter()
            .any(|field| occurs_in(var, &field.ty, subst)),
        Type::Enum(enum_type) => enum_type
            .variants
            .iter()
            .any(|variant| match &variant.kind {
                TypeEnumVariantKind::Unit => false,
                TypeEnumVariantKind::Tuple(payload) => {
                    payload.iter().any(|ty| occurs_in(var, ty, subst))
                }
                TypeEnumVariantKind::Struct(fields) => {
                    fields.iter().any(|field| occurs_in(var, &field.ty, subst))
                }
            }),
        Type::Int(_)
        | Type::Bool
        | Type::Unit
        | Type::UtxoAny
        | Type::UtxoNamed(_)
        | Type::AbiNarrow(_) => false,
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
        Type::Function { params, result, .. } => {
            for ty in params {
                collect_free_type_vars(ty, set);
            }
            collect_free_type_vars(result, set);
        }
        Type::Tuple(items) => {
            for ty in items {
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
                    TypeEnumVariantKind::Unit => {}
                    TypeEnumVariantKind::Tuple(payload) => {
                        for ty in payload {
                            collect_free_type_vars(ty, set);
                        }
                    }
                    TypeEnumVariantKind::Struct(fields) => {
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
        | Type::UtxoNamed(_)
        | Type::AbiNarrow(_) => {}
    }
}

fn enum_payload_kind_from_variant(kind: &EnumVariantInfoKind) -> EnumPayloadKind {
    match kind {
        EnumVariantInfoKind::Unit => EnumPayloadKind::unit(),
        EnumVariantInfoKind::Tuple(payload) => EnumPayloadKind::tuple(payload.len()),
        EnumVariantInfoKind::Struct(fields) => EnumPayloadKind::struct_payload(fields.len()),
    }
}

fn enum_payload_kind_from_pattern(payload: &EnumPatternPayload) -> EnumPayloadKind {
    match payload {
        EnumPatternPayload::Unit => EnumPayloadKind::unit(),
        EnumPatternPayload::Tuple(patterns) => EnumPayloadKind::tuple(patterns.len()),
        EnumPatternPayload::Struct(fields) => EnumPayloadKind::struct_payload(fields.len()),
    }
}
