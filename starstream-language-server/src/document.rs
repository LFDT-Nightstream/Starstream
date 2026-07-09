//! State tracking for an open text document.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ropey::Rope;
use starstream_types::TypedEnumVariantPayload;
use tower_lsp_server::lsp_types::{
    DocumentSymbol, DocumentSymbolResponse, Hover, HoverContents, Location, MarkupContent,
    MarkupKind, Position, Range, SymbolKind, TextEdit, Uri,
};

use starstream_compiler::{
    formatter, parse_program,
    parser::ParseError,
    typecheck::typecheck_program,
    typecheck::{TypeError, TypeWarning, TypecheckOptions, TypecheckSuccess},
};
use starstream_types::{
    CommentMap, DUMMY_SPAN, FunctionDef, GenericTypeDef, Span, Spanned, TypeVarId, TypedUtxoDef,
    TypedUtxoPart,
    ast::{self as untyped_ast, Program, TypeAnnotation},
    typed_ast::{
        TypedAbiDef, TypedAbiPart, TypedBlock, TypedDefinition, TypedEnumDef, TypedExpr,
        TypedExprKind, TypedFunctionDef, TypedIfCondition, TypedImportDef, TypedImportItems,
        TypedMatchArm, TypedPattern, TypedProgram, TypedStatement, TypedStructDef,
        TypedStructFieldInitializer,
    },
    types::{EnumType, EnumVariantKind, Type},
};

use crate::diagnostics::diagnostic_to_lsp;

/// Analysis data cached for an open document.
#[derive(Debug)]
pub struct DocumentState {
    /// Rope representation of the document text for efficient position/offset conversions.
    rope: Rope,
    /// LSP document version, used to track incremental updates.
    version: Option<i32>,
    /// Untyped AST produced by the parser, if parsing succeeded.
    program: Option<Arc<Program>>,
    /// Typed AST and inference results, if type-checking succeeded.
    typed: Option<TypecheckSuccess>,
    /// Parse and type diagnostics converted to LSP diagnostics.
    diagnostics: Vec<tower_lsp_server::lsp_types::Diagnostic>,
    /// Hover information indexed by source span.
    hover_entries: Vec<HoverEntry>,
    /// Go-to-definition mappings from usage spans to definition spans.
    definition_entries: Vec<DefinitionEntry>,
    /// Hierarchical document symbols for the outline view.
    document_symbols: Vec<DocumentSymbol>,
    /// Maps type names (structs, enums) to their definition spans.
    type_definitions: HashMap<String, Span>,
    /// Maps function names to their definition spans.
    function_definitions: HashMap<String, Span>,
    /// Maps struct name -> field name -> field definition span.
    struct_field_definitions: HashMap<String, HashMap<String, Span>>,
    /// Maps struct name -> field name -> field type.
    struct_field_types: HashMap<String, HashMap<String, Type>>,
    /// Maps enum name -> variant name -> variant definition span.
    enum_variant_definitions: HashMap<String, HashMap<String, Span>>,
    /// Maps (enum, variant) -> field name -> field definition span for struct variants.
    enum_variant_field_definitions: HashMap<EnumVariantKey, HashMap<String, Span>>,
    /// Maps (enum, variant) -> payload info (tuple types or struct field types).
    enum_variant_infos: HashMap<EnumVariantKey, EnumVariantPayloadInfo>,
    /// List of struct types for quick lookup by name.
    struct_type_index: Vec<StructTypeEntry>,
    /// Maps struct name -> its resolved Type.
    struct_types: HashMap<String, Type>,
    /// Maps enum name -> its resolved Type.
    enum_types: HashMap<String, Type>,
    /// Maps imported local names to their definition spans (in import statement).
    import_definitions: HashMap<String, Span>,
    /// Maps namespace alias -> function name -> (alias span, function type).
    namespace_functions: HashMap<String, HashMap<String, (Span, Type)>>,
    /// Maps function name -> (signature, doc comment) for call-site hover.
    function_docs: HashMap<String, (String, String)>,
    /// Maps struct name -> doc comment for type annotation hover.
    struct_docs: HashMap<String, String>,
    /// Maps enum name -> doc comment for type annotation hover.
    enum_docs: HashMap<String, String>,
    /// Maps struct name -> field name -> doc comment for field access hover.
    struct_field_docs: HashMap<String, HashMap<String, String>>,
    /// Maps ABI name -> method name -> (signature label, doc comment) for method call hover.
    abi_method_info: HashMap<String, HashMap<String, (String, Option<String>)>>,
    /// Maps (enum, variant) -> doc comment for variant usage hover.
    enum_variant_docs: HashMap<EnumVariantKey, String>,
    /// Generic type definitions from the type checker (Option, Result, etc.).
    generic_types: HashMap<String, GenericTypeDef>,
    /// Comment map for efficient span-based comment lookups.
    comment_map: CommentMap,
}

#[allow(unused)]
impl DocumentState {
    /// Create initial document state from raw text. `workspace_folders` is
    /// the list of roots the editor announced during `initialize`; the
    /// document picks the first one that contains it as its scan root.
    pub fn from_text(
        uri: &Uri,
        text: &str,
        version: Option<i32>,
        workspace_folders: &[PathBuf],
    ) -> Self {
        let mut state = Self {
            rope: Rope::from_str(text),
            version,
            program: None,
            typed: None,
            diagnostics: Vec::new(),
            hover_entries: Vec::new(),
            definition_entries: Vec::new(),
            document_symbols: Vec::new(),
            type_definitions: HashMap::new(),
            function_definitions: HashMap::new(),
            struct_field_definitions: HashMap::new(),
            struct_field_types: HashMap::new(),
            enum_variant_definitions: HashMap::new(),
            enum_variant_field_definitions: HashMap::new(),
            enum_variant_infos: HashMap::new(),
            struct_type_index: Vec::new(),
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            import_definitions: HashMap::new(),
            namespace_functions: HashMap::new(),
            function_docs: HashMap::new(),
            struct_docs: HashMap::new(),
            enum_docs: HashMap::new(),
            struct_field_docs: HashMap::new(),
            abi_method_info: HashMap::new(),
            enum_variant_docs: HashMap::new(),
            generic_types: HashMap::new(),
            comment_map: CommentMap::new(),
        };

        state.reanalyse(uri, text, workspace_folders);

        state
    }

    /// Update the stored text + version and recompute analysis artifacts.
    pub fn update(
        &mut self,
        uri: &Uri,
        text: &str,
        version: Option<i32>,
        workspace_folders: &[PathBuf],
    ) {
        self.rope = Rope::from_str(text);

        self.version = version;

        self.reanalyse(uri, text, workspace_folders);
    }

    /// Most recent diagnostics derived from parsing and typechecking.
    pub fn diagnostics(&self) -> &[tower_lsp_server::lsp_types::Diagnostic] {
        &self.diagnostics
    }

    /// Document version reflected in the currently cached state.
    pub fn version(&self) -> Option<i32> {
        self.version
    }

    /// Typed AST produced by the typechecker, if type-checking succeeded.
    #[allow(dead_code)]
    pub fn typed(&self) -> Option<&TypecheckSuccess> {
        self.typed.as_ref()
    }

    /// Untyped AST produced by the parser, if parsing succeeded.
    pub fn program(&self) -> Option<&Program> {
        self.program.as_deref()
    }

    /// Rope representation covering the latest snapshot of the document.
    #[allow(dead_code)]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    /// Cached document symbols if type-checking succeeded.
    pub fn document_symbols(&self) -> Option<DocumentSymbolResponse> {
        let _ = self.typed.as_ref()?;

        Some(DocumentSymbolResponse::Nested(
            self.document_symbols.clone(),
        ))
    }

    fn reanalyse(&mut self, uri: &Uri, text: &str, workspace_folders: &[PathBuf]) {
        self.diagnostics.clear();
        self.program = None;
        self.typed = None;
        self.hover_entries.clear();
        self.definition_entries.clear();
        self.document_symbols.clear();
        self.type_definitions.clear();
        self.function_definitions.clear();
        self.struct_field_definitions.clear();
        self.struct_field_types.clear();
        self.enum_variant_definitions.clear();
        self.enum_variant_field_definitions.clear();
        self.enum_variant_infos.clear();
        self.struct_type_index.clear();
        self.struct_types.clear();
        self.enum_types.clear();
        self.import_definitions.clear();
        self.namespace_functions.clear();
        self.function_docs.clear();
        self.struct_docs.clear();
        self.enum_docs.clear();
        self.struct_field_docs.clear();
        self.abi_method_info.clear();
        self.enum_variant_docs.clear();
        self.generic_types.clear();

        let parse_output = parse_program(text);

        // Get the CommentMap before consuming parse_output
        self.comment_map = parse_output.comment_map();

        for error in parse_output.errors() {
            self.push_parse_error(uri, error);
        }

        let program = parse_output.program().cloned().map(Arc::new);

        self.program = program;

        if self.program.is_some() {
            // For URIs that point at a real file on disk, always go through
            // the multi-file graph driver — never fall through to single-file
            // mode. That avoids surfacing the W0002 ("path import ignored
            // in single-file mode") warning anywhere except the browser
            // playground, where there's no filesystem to scan.
            if !self.try_typecheck_via_workspace(uri, text, workspace_folders) {
                let program = self.program.clone().unwrap();
                match typecheck_program(program.as_ref(), TypecheckOptions::default()) {
                    Ok(typed) => {
                        for warning in &typed.warnings {
                            self.push_type_warning(uri, warning.clone());
                        }

                        self.generic_types = typed.generic_types.clone();

                        let program_ast = self.program.clone();
                        self.build_indexes(&typed.program, program_ast.as_deref(), text);

                        self.typed = Some(typed);
                    }
                    Err(failure) => {
                        for warning in failure.warnings {
                            self.push_type_warning(uri, warning);
                        }
                        for error in failure.errors {
                            self.push_type_error(uri, error);
                        }
                    }
                }
            }
        }
    }

    /// Type-check this document using the multi-file graph driver.
    ///
    /// Strategy:
    ///   1. Pick a scan root: the first `workspace_folders` entry that
    ///      contains the open file. If the editor didn't announce any
    ///      folders (or none of them contain this file), fall back to the
    ///      file's parent directory.
    ///   2. Build one workspace module graph rooted at that directory. If
    ///      the open file is a node in that graph, drive type-checking
    ///      through the graph and surface diagnostics for the open file.
    ///   3. Otherwise (file is outside the scan root, or the workspace scan
    ///      itself errored), fall back to a single-file graph rooted at
    ///      the open file. Still goes through `typecheck_modules`, so
    ///      W0002 doesn't fire.
    ///
    /// Returns `false` only when the URI isn't a filesystem path (browser
    /// playground, `untitled:`, etc.) — in that case the caller falls back
    /// to `typecheck_program`, where W0002 *will* fire on path imports.
    fn try_typecheck_via_workspace(
        &mut self,
        uri: &Uri,
        text: &str,
        workspace_folders: &[PathBuf],
    ) -> bool {
        let Some(file_path) = uri_to_file_path(uri) else {
            return false;
        };
        let Ok(canonical_file) = fs::canonicalize(&file_path) else {
            return false;
        };

        let workspace_root = workspace_root_for(&canonical_file, workspace_folders);

        // One workspace graph for the whole project; contracts are codegen
        // entries inside it. The open file is just a node — it might be a
        // contract, an imported helper, or a loose orphan. Either way the
        // graph knows about it.
        let mut fs = starstream_types::FileSystem::new();
        let graph = if let Ok(g) =
            starstream_compiler::module_graph::load_workspace(&workspace_root, &mut fs)
        {
            g
        } else {
            // If the workspace scan blew up (e.g. cross-contract import
            // somewhere we don't own), still try to give *this* file
            // diagnostics by rooting a single-file graph at it.
            let mut local_fs = starstream_types::FileSystem::new();
            let Ok(local_graph) =
                starstream_compiler::module_graph::load_from_entry(&canonical_file, &mut local_fs)
            else {
                return false;
            };
            let module_id = local_graph
                .find_by_path(&canonical_file)
                .expect("entry module is always in its own graph");
            self.run_graph_typecheck(uri, text, &local_graph, module_id);
            return true;
        };

        if let Some(module_id) = graph.find_by_path(&canonical_file) {
            self.run_graph_typecheck(uri, text, &graph, module_id);
            return true;
        }

        // The open file isn't reachable from the workspace scan (e.g. an
        // ad-hoc file outside the scanned tree). Build a graph rooted at it.
        let mut local_fs = starstream_types::FileSystem::new();
        match starstream_compiler::module_graph::load_from_entry(&canonical_file, &mut local_fs) {
            Ok(local_graph) => {
                let module_id = local_graph
                    .find_by_path(&canonical_file)
                    .expect("entry module is always in its own graph");
                self.run_graph_typecheck(uri, text, &local_graph, module_id);
                true
            }
            Err(_) => false,
        }
    }

    fn run_graph_typecheck(
        &mut self,
        uri: &Uri,
        text: &str,
        graph: &starstream_compiler::ModuleGraph,
        module_id: starstream_compiler::ModuleId,
    ) {
        match starstream_compiler::typecheck_modules(graph, TypecheckOptions::default()) {
            Ok(success) => {
                for (id, warning) in &success.warnings {
                    if *id == module_id {
                        self.push_type_warning(uri, warning.clone());
                    }
                }
                self.apply_typed_module(text, &success, module_id);
            }
            Err(failure) => {
                for (id, warning) in failure.warnings {
                    if id == module_id {
                        self.push_type_warning(uri, warning);
                    }
                }
                for (id, error) in failure.errors {
                    if id == module_id {
                        self.push_type_error(uri, error);
                    }
                }
            }
        }
    }

    fn apply_typed_module(
        &mut self,
        text: &str,
        success: &starstream_compiler::TypedModuleGraph,
        module_id: starstream_compiler::ModuleId,
    ) {
        self.generic_types = success.generic_types.clone();

        let entry_typed = success
            .modules
            .iter()
            .find(|m| m.id == module_id)
            .expect("module id we just observed must be present");
        let entry_program = entry_typed.program.clone();

        let program_ast = self.program.clone();
        self.build_indexes(&entry_program, program_ast.as_deref(), text);

        self.typed = Some(TypecheckSuccess {
            program: entry_program,
            traces: Vec::new(),
            generic_types: success.generic_types.clone(),
            warnings: Vec::new(),
        });
    }

    fn push_parse_error(&mut self, uri: &Uri, error: &ParseError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, error);

        self.diagnostics.push(diag);
    }

    fn push_type_error(&mut self, uri: &Uri, error: TypeError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, &error);

        self.diagnostics.push(diag);
    }

    fn push_type_warning(&mut self, uri: &Uri, warning: TypeWarning) {
        let diag = diagnostic_to_lsp(&self.rope, uri, &warning);

        self.diagnostics.push(diag);
    }

    /// Format the cached AST. Returns `None` if parsing failed.
    pub fn format(&self) -> Result<Option<String>, fmt::Error> {
        let program = match self.program() {
            Some(program) => program,
            None => return Ok(None),
        };

        let str = self.rope.to_string();
        formatter::program(program, &str, &self.comment_map).map(Some)
    }

    /// Resolve hover information for the given cursor position.
    pub fn hover(&self, position: Position) -> Option<Hover> {
        let offset = self.position_to_offset(position)?;

        let entry = self
            .hover_entries
            .iter()
            .filter(|entry| entry.contains(offset))
            .min_by_key(|entry| (entry.len(), entry.span.start))?;

        let range = self.span_to_range(entry.span);

        let value = match &entry.doc {
            Some(doc) => format!("```star\n{}\n```\n---\n{}", entry.label, doc),
            None => format!("```star\n{}\n```", entry.label),
        };

        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        });

        Some(Hover {
            contents,
            range: Some(range),
        })
    }

    /// Resolve go-to-definition for the given position.
    pub fn goto_definition(&self, uri: &Uri, position: Position) -> Option<Location> {
        let offset = self.position_to_offset(position)?;

        let entry = self
            .definition_entries
            .iter()
            .filter(|entry| entry.contains(offset))
            .min_by_key(|entry| (entry.len(), entry.usage.start))?;

        let range = self.span_to_range(entry.target);

        Some(Location {
            uri: uri.clone(),
            range,
        })
    }

    /// Produce text edits for renaming the symbol at the given position.
    pub fn rename_edits(&self, position: Position, new_name: &str) -> Option<Vec<TextEdit>> {
        let offset = self.position_to_offset(position)?;

        let base_entry = self
            .definition_entries
            .iter()
            .filter(|entry| entry.contains(offset))
            .min_by_key(|entry| (entry.len(), entry.usage.start))?;

        let target = base_entry.target;

        let mut seen = HashSet::new();
        let mut edits = Vec::new();

        for entry in &self.definition_entries {
            if entry.target == target && seen.insert((entry.usage.start, entry.usage.end)) {
                edits.push(TextEdit {
                    range: self.span_to_range(entry.usage),
                    new_text: new_name.to_string(),
                });
            }
        }

        if edits.is_empty() { None } else { Some(edits) }
    }

    /// Find all references to the symbol at the given position.
    pub fn references(
        &self,
        uri: &Uri,
        position: Position,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        let offset = self.position_to_offset(position)?;

        let base_entry = self
            .definition_entries
            .iter()
            .filter(|entry| entry.contains(offset))
            .min_by_key(|entry| (entry.len(), entry.usage.start))?;

        let target = base_entry.target;

        let mut seen = HashSet::new();
        let mut locations = Vec::new();

        for entry in &self.definition_entries {
            if entry.target == target && seen.insert((entry.usage.start, entry.usage.end)) {
                locations.push(Location {
                    uri: uri.clone(),
                    range: self.span_to_range(entry.usage),
                });
            }
        }

        if include_declaration && seen.insert((target.start, target.end)) {
            locations.push(Location {
                uri: uri.clone(),
                range: self.span_to_range(target),
            });
        }

        if locations.is_empty() {
            None
        } else {
            Some(locations)
        }
    }

    fn build_indexes(&mut self, program: &TypedProgram, ast: Option<&Program>, source: &str) {
        self.hover_entries.clear();
        self.definition_entries.clear();
        self.type_definitions.clear();

        self.register_docs_from_generics();

        let mut scopes: Vec<HashMap<String, Span>> = vec![HashMap::new()];

        // Collect definitions, pairing typed and untyped for doc comment extraction
        let untyped_defs: Vec<_> = ast
            .map(|p| p.definitions.iter().collect())
            .unwrap_or_default();

        for (i, definition) in program.definitions.iter().enumerate() {
            let untyped_def = untyped_defs.get(i).copied();
            self.collect_definition(definition, &mut scopes, untyped_def, source);
        }

        self.document_symbols = self.collect_document_symbols(program);

        if let Some(program_ast) = ast {
            self.collect_type_annotations_from_ast(program_ast);
        }
    }

    /// Register doc comments and variant info from generic type definitions
    /// so that hover shows documentation and syntax-highlighted labels.
    fn register_docs_from_generics(&mut self) {
        for (name, def) in &self.generic_types {
            if let Some(doc) = &def.doc {
                self.enum_docs.insert(name.clone(), doc.clone());
            }
            for (variant_name, doc) in &def.variant_docs {
                self.enum_variant_docs
                    .insert(EnumVariantKey::new(name, variant_name), doc.clone());
            }
            // Register unit variant infos for pattern matching hover
            if let Type::Enum(e) = &def.ty {
                for variant in &e.variants {
                    if matches!(variant.kind, EnumVariantKind::Unit) {
                        self.enum_variant_infos.insert(
                            EnumVariantKey::new(name, &variant.name),
                            EnumVariantPayloadInfo::Unit,
                        );
                    }
                }
            }
        }
    }

    fn collect_definition(
        &mut self,
        definition: &TypedDefinition,
        scopes: &mut Vec<HashMap<String, Span>>,
        untyped: Option<&Spanned<untyped_ast::Definition>>,
        source: &str,
    ) {
        // Extract doc comment using CommentMap
        // The span now directly covers just the definition content (no comments)
        let doc = untyped.and_then(|u| self.comment_map.doc_comments(u.span, source));

        match definition {
            TypedDefinition::Import(import) => self.collect_import(import),
            TypedDefinition::Function(function) => {
                self.collect_function(function, scopes, doc.clone());
            }
            TypedDefinition::Struct(definition) => {
                let untyped_struct = untyped.and_then(|u| match &u.node {
                    untyped_ast::Definition::Struct(s) => Some(s),
                    _ => None,
                });
                self.collect_struct(definition, untyped_struct, source, doc.clone());
            }
            TypedDefinition::Enum(definition) => {
                let untyped_enum = untyped.and_then(|u| match &u.node {
                    untyped_ast::Definition::Enum(e) => Some(e),
                    _ => None,
                });
                self.collect_enum(definition, untyped_enum, source, doc.clone());
            }
            TypedDefinition::Utxo(definition) => self.collect_utxo(definition, scopes),
            TypedDefinition::Token(_) => {
                // `token` is parse-only for now: no symbols/hover yet.
            }
            TypedDefinition::Abi(definition) => {
                let untyped_abi = untyped.and_then(|u| match &u.node {
                    untyped_ast::Definition::Abi(a) => Some(a),
                    _ => None,
                });
                self.collect_abi(definition, untyped_abi, source, doc.clone());
            }
            TypedDefinition::Contract => {
                // `contract;` is a pure marker — no symbols, no hover info.
            }
        }
    }

    fn collect_import(&mut self, import: &TypedImportDef) {
        match &import.items {
            TypedImportItems::Named(items) => {
                for item in items {
                    if let Some(span) = item.local.opt_span() {
                        self.definition_entries.push(DefinitionEntry {
                            usage: span,
                            target: span,
                        });

                        self.import_definitions
                            .insert(item.local.name.clone(), span);

                        self.add_hover_span(span, &item.ty);
                    }
                }
            }
            TypedImportItems::Namespace { alias, functions } => {
                if let Some(alias_span) = alias.opt_span() {
                    self.definition_entries.push(DefinitionEntry {
                        usage: alias_span,
                        target: alias_span,
                    });

                    let func_names: Vec<_> =
                        functions.iter().map(|f| f.local.name.clone()).collect();
                    let label = format!("namespace {} ({})", alias.name, func_names.join(", "));
                    self.add_hover_label(alias_span, label);

                    let mut ns_funcs = HashMap::new();
                    for func in functions {
                        ns_funcs.insert(func.local.name.clone(), (alias_span, func.ty.clone()));
                    }
                    self.namespace_functions
                        .insert(alias.name.clone(), ns_funcs);
                }
            }
        }
    }

    fn collect_struct(
        &mut self,
        definition: &TypedStructDef,
        untyped: Option<&untyped_ast::StructDef>,
        source: &str,
        doc: Option<String>,
    ) {
        if let Some(span) = definition.name.opt_span() {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            self.type_definitions
                .insert(definition.name.name.clone(), span);

            self.add_hover_span_with_doc(span, &definition.ty, doc.clone());

            self.struct_type_index.push(StructTypeEntry {
                name: definition.name.name.clone(),
                ty: definition.ty.clone(),
            });
        }

        // Store doc comment for type annotation hover
        if let Some(d) = doc {
            self.struct_docs.insert(definition.name.name.clone(), d);
        }

        self.struct_types
            .insert(definition.name.name.clone(), definition.ty.clone());

        {
            let entry = self
                .struct_field_definitions
                .entry(definition.name.name.clone())
                .or_default();
            let type_entry = self
                .struct_field_types
                .entry(definition.name.name.clone())
                .or_default();
            for field in &definition.fields {
                if let Some(span) = field.name.opt_span() {
                    entry.insert(field.name.name.clone(), span);
                }
                type_entry.insert(field.name.name.clone(), field.ty.clone());
            }
        }

        // Extract field-level doc comments using untyped AST spans
        let untyped_fields = untyped.map(|u| &u.fields[..]).unwrap_or(&[]);
        for (field, untyped_field) in definition.fields.iter().zip(untyped_fields.iter()) {
            let field_doc = self.comment_map.doc_comments(untyped_field.span, source);
            if let Some(span) = field.name.opt_span() {
                self.add_hover_span_with_doc(span, &field.ty, field_doc.clone());

                // Store field doc for use at field access sites
                if let Some(doc) = field_doc {
                    self.struct_field_docs
                        .entry(definition.name.name.clone())
                        .or_default()
                        .insert(field.name.name.clone(), doc);
                }
            }
        }
    }

    fn collect_enum(
        &mut self,
        definition: &TypedEnumDef,
        untyped: Option<&untyped_ast::EnumDef>,
        source: &str,
        doc: Option<String>,
    ) {
        if let Some(span) = definition.name.opt_span() {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            self.type_definitions
                .insert(definition.name.name.clone(), span);

            self.add_hover_span_with_doc(span, &definition.ty, doc.clone());
        }

        // Store doc comment for type annotation hover
        if let Some(d) = doc {
            self.enum_docs.insert(definition.name.name.clone(), d);
        }

        self.enum_types
            .insert(definition.name.name.clone(), definition.ty.clone());

        {
            let entry = self
                .enum_variant_definitions
                .entry(definition.name.name.clone())
                .or_default();

            entry.clear();

            for variant in &definition.variants {
                if let Some(span) = variant.name.opt_span() {
                    self.definition_entries.push(DefinitionEntry {
                        usage: span,
                        target: span,
                    });

                    entry.insert(variant.name.name.clone(), span);
                }
            }
        }

        // Extract variant-level doc comments using untyped AST spans
        let untyped_variants = untyped.map(|u| &u.variants[..]).unwrap_or(&[]);
        for (variant, untyped_variant) in definition.variants.iter().zip(untyped_variants.iter()) {
            let variant_doc = self.comment_map.doc_comments(untyped_variant.span, source);
            let key = EnumVariantKey::new(&definition.name.name, &variant.name.name);

            let info = match &variant.payload {
                TypedEnumVariantPayload::Unit => {
                    self.enum_variant_field_definitions.remove(&key);
                    EnumVariantPayloadInfo::Unit
                }
                TypedEnumVariantPayload::Tuple(types) => {
                    self.enum_variant_field_definitions.remove(&key);
                    EnumVariantPayloadInfo::Tuple(types.clone())
                }
                TypedEnumVariantPayload::Struct(fields) => {
                    let info = EnumVariantPayloadInfo::Struct(
                        fields
                            .iter()
                            .map(|field| (field.name.name.clone(), field.ty.clone()))
                            .collect(),
                    );
                    let mut field_hovers = Vec::new();
                    {
                        let def_entry = self
                            .enum_variant_field_definitions
                            .entry(key.clone())
                            .or_default();
                        def_entry.clear();
                        for field in fields {
                            if let Some(span) = field.name.opt_span() {
                                def_entry.insert(field.name.name.clone(), span);
                                field_hovers.push((span, field.ty.clone()));
                            }
                        }
                    }
                    for (span, ty) in field_hovers {
                        self.add_hover_span(span, &ty);
                    }
                    info
                }
            };

            self.enum_variant_infos.insert(key.clone(), info.clone());

            // Store variant doc for use at variant usage sites
            if let Some(ref doc) = variant_doc {
                self.enum_variant_docs.insert(key, doc.clone());
            }

            if let Some(span) = variant.name.opt_span() {
                self.add_hover_label_with_doc(
                    span,
                    format_enum_variant_hover_from_info(
                        &definition.name.name,
                        &variant.name.name,
                        &info,
                    ),
                    variant_doc,
                );
            }
        }
    }

    fn collect_utxo(&mut self, definition: &TypedUtxoDef, scopes: &mut Vec<HashMap<String, Span>>) {
        for part in &definition.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    let global = scopes.first_mut().unwrap();
                    for var in vars {
                        if let Some(span) = var.name.opt_span() {
                            global.insert(var.name.name.clone(), span);
                            self.definition_entries.push(DefinitionEntry {
                                usage: span,
                                target: span,
                            });
                            self.add_hover_span(span, &var.ty);
                        }
                    }
                }
                TypedUtxoPart::Function(function) => {
                    self.collect_function(function, scopes, None);
                }
                TypedUtxoPart::AbiImpl {
                    abi: _,
                    span: _,
                    parts,
                } => {
                    for part in parts {
                        self.collect_function(part, scopes, None);
                    }
                }
            }
        }
    }

    fn collect_abi(
        &mut self,
        definition: &TypedAbiDef,
        _untyped: Option<&untyped_ast::AbiDef>,
        source: &str,
        doc: Option<String>,
    ) {
        if let Some(span) = definition.name.opt_span() {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            // Add hover for ABI name with doc comment
            self.add_hover_label_with_doc(span, format!("abi {}", definition.name.name), doc);
        }

        for part in &definition.parts {
            match part {
                TypedAbiPart::Event(event) => {
                    if let Some(span) = event.name.opt_span() {
                        self.definition_entries.push(DefinitionEntry {
                            usage: span,
                            target: span,
                        });

                        self.add_hover_label(span, format!("event {}", event.name.name));
                    }
                }
                TypedAbiPart::Effect(effect) => {
                    if let Some(span) = effect.name.opt_span() {
                        self.definition_entries.push(DefinitionEntry {
                            usage: span,
                            target: span,
                        });

                        self.add_hover_label(span, format!("effect {}", effect.name.name));
                    }
                }
                TypedAbiPart::FnDecl(decl) => {
                    let method_doc = self.comment_map.doc_comments(decl.span, source);

                    let params = decl
                        .params
                        .iter()
                        .map(|p| format!("{}: {}", p.name.name, p.ty.to_compact_string()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let label = if decl.return_type == Type::Unit {
                        format!("fn {}({})", decl.name.name, params)
                    } else {
                        format!(
                            "fn {}({}) -> {}",
                            decl.name.name,
                            params,
                            decl.return_type.to_compact_string()
                        )
                    };

                    // Store for field access hover on narrowed ABI variables
                    self.abi_method_info
                        .entry(definition.name.name.clone())
                        .or_default()
                        .insert(decl.name.name.clone(), (label.clone(), method_doc.clone()));

                    if let Some(span) = decl.name.opt_span() {
                        self.definition_entries.push(DefinitionEntry {
                            usage: span,
                            target: span,
                        });

                        self.add_hover_label_with_doc(span, label, method_doc);
                    }
                }
            }
        }
    }

    fn collect_function(
        &mut self,
        function: &TypedFunctionDef,
        scopes: &mut Vec<HashMap<String, Span>>,
        doc: Option<String>,
    ) {
        if let Some(span) = function.name.opt_span() {
            self.function_definitions
                .insert(function.name.name.clone(), span);

            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            // Format full function signature: fn(param1: Type1, param2: Type2) -> ReturnType
            // Use to_compact_string() to avoid expanding struct/enum definitions
            // Include effect prefix to match Type::Function display format
            let params = function
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name.name, p.ty.to_compact_string()))
                .collect::<Vec<_>>()
                .join(", ");
            let signature = format!(
                "({}) -> {}",
                params,
                function.return_type.to_compact_string()
            );

            self.add_hover_label_with_doc(span, signature.clone(), doc.clone());

            // Store the doc for call-site lookups
            if let Some(d) = doc {
                self.function_docs
                    .insert(function.name.name.clone(), (signature, d));
            }
        }

        scopes.push(HashMap::new());

        if let Some(scope) = scopes.last_mut() {
            for param in &function.params {
                if let Some(span) = param.name.opt_span() {
                    scope.insert(param.name.name.clone(), span);

                    self.definition_entries.push(DefinitionEntry {
                        usage: span,
                        target: span,
                    });

                    self.add_hover_span(span, &param.ty);
                }
            }
        }

        self.collect_block(&function.body, scopes);

        scopes.pop();
    }

    fn collect_statement(
        &mut self,
        statement: &TypedStatement,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        match statement {
            TypedStatement::VariableDeclaration {
                public: _,
                mutable: _,
                name,
                value,
            } => {
                self.collect_expr(value, scopes);

                if let Some(span) = name.opt_span() {
                    self.definition_entries.push(DefinitionEntry {
                        usage: span,
                        target: span,
                    });

                    if let Some(scope) = scopes.last_mut() {
                        scope.insert(name.name.clone(), span);
                    }
                }

                if let Some(span) = name.opt_span() {
                    self.add_hover_span(span, &value.node.ty);
                }
            }
            TypedStatement::Assignment { target, value } => {
                self.collect_expr(value, scopes);

                self.add_usage(target.span(), &target.name, scopes);

                if let Some(span) = target.opt_span() {
                    self.add_hover_span(span, &value.node.ty);
                }
            }
            TypedStatement::While { condition, body } => {
                self.collect_expr(condition, scopes);

                self.collect_block(body, scopes);
            }
            TypedStatement::Expression(expr) => self.collect_expr(expr, scopes),
            TypedStatement::Return(Some(expr)) => self.collect_expr(expr, scopes),
            TypedStatement::Return(None) => {}
            TypedStatement::Resume => {}
        }
    }

    fn collect_block(&mut self, block: &TypedBlock, scopes: &mut Vec<HashMap<String, Span>>) {
        scopes.push(HashMap::new());

        for statement in &block.statements {
            self.collect_statement(statement, scopes);
        }

        if let Some(expr) = &block.tail_expression {
            self.collect_expr(expr, scopes);
        }

        scopes.pop();
    }

    fn collect_expr(&mut self, expr: &Spanned<TypedExpr>, scopes: &mut Vec<HashMap<String, Span>>) {
        let doc = self.doc_for_type(&expr.node.ty);
        self.add_hover_label_with_doc(expr.span, expr.node.ty.to_compact_string(), doc);

        match &expr.node.kind {
            TypedExprKind::ScopedName { name, .. } => {
                // TODO: handle scoping properly
                let name = name.last().unwrap();
                let usage_span = name.span_or(expr.span);
                self.add_usage(usage_span, name.as_str(), scopes);
            }
            TypedExprKind::Unary { expr: inner, .. } => self.collect_expr(inner, scopes),
            TypedExprKind::Binary { left, right, .. } => {
                self.collect_expr(left, scopes);

                self.collect_expr(right, scopes);
            }
            TypedExprKind::Grouping(inner) => self.collect_expr(inner, scopes),
            TypedExprKind::Literal(_) => {}
            TypedExprKind::StructConstructor {
                name,
                fields,
                enum_variant: _,
            } => {
                // TODO: handle scoping properly
                let name = name.last().unwrap();
                self.add_type_usage(name.opt_span(), &name.name);

                // Add hover for struct name with doc comment
                if let Some(span) = name.opt_span()
                    && let Some(ty) = self.struct_types.get(&name.name).cloned()
                {
                    let struct_doc = self.struct_docs.get(&name.name).cloned();
                    self.add_hover_span_with_doc(span, &ty, struct_doc);
                }

                for field in fields {
                    self.collect_struct_literal_field(&name.name, field, scopes);
                }
            }
            TypedExprKind::FieldAccess { target, field } => {
                self.collect_expr(target, scopes);
                self.add_field_access_usage(field.opt_span(), &target.node.ty, &field.name);

                // Add hover with doc comment for field access
                if let Some(field_span) = field.opt_span() {
                    if let Type::AbiNarrow(ref abi) = target.node.ty {
                        // ABI method access — show method signature with doc comment
                        if let Some((label, doc)) = self
                            .abi_method_info
                            .get(abi.name.as_str())
                            .and_then(|methods| methods.get(&field.name))
                        {
                            self.add_hover_label_with_doc(field_span, label.clone(), doc.clone());
                        }
                    } else if let Some(struct_name) =
                        self.find_struct_name_for_type(&target.node.ty)
                    {
                        // Struct field access
                        let field_type = self.lookup_struct_field_type(&struct_name, &field.name);
                        let field_doc = self
                            .struct_field_docs
                            .get(&struct_name)
                            .and_then(|fields| fields.get(&field.name))
                            .cloned();

                        if let Some(ty) = field_type {
                            self.add_hover_span_with_doc(field_span, &ty, field_doc);
                        }
                    }
                }
            }
            /*
            TypedExprKind::EnumConstructor {
                enum_name,
                variant,
                payload,
            } => {
                self.add_type_usage(enum_name.opt_span(), &enum_name.name);

                // Add hover for enum name with doc comment
                if let Some(span) = enum_name.opt_span() {
                    let enum_doc = self.enum_docs.get(&enum_name.name).cloned();
                    self.add_generic_or_concrete_type_hover(
                        span,
                        &enum_name.name,
                        Some(&expr.node.ty),
                        enum_doc,
                    );
                }

                self.add_enum_variant_usage(variant.opt_span(), &enum_name.name, &variant.name);
                self.add_variant_hover(
                    variant.opt_span(),
                    &enum_name.name,
                    &variant.name,
                    Some(&expr.node.ty),
                );

                match payload {
                    TypedEnumConstructorPayload::Unit => {}
                    TypedEnumConstructorPayload::Tuple(values) => {
                        for expr in values {
                            self.collect_expr(expr, scopes);
                        }
                    }
                    TypedEnumConstructorPayload::Struct(fields) => {
                        for field in fields {
                            self.collect_expr(&field.value, scopes);
                            self.add_enum_variant_field_usage(
                                field.name.opt_span(),
                                &enum_name.name,
                                &variant.name,
                                &field.name.name,
                            );
                        }
                    }
                }
            }
            */
            TypedExprKind::Block(block) => self.collect_block(block, scopes),
            TypedExprKind::If {
                branches,
                else_branch,
            } => {
                for (condition, then_branch) in branches {
                    match condition {
                        TypedIfCondition::Bool(condition) => {
                            self.collect_expr(condition, scopes);
                        }
                        TypedIfCondition::Is {
                            name,
                            abi_name,
                            original_type,
                        } => {
                            // Hover on the variable shows its original type
                            if let Some(span) = name.opt_span() {
                                self.add_hover_span(span, original_type);
                            }
                            // Hover on the ABI name shows the ABI definition label
                            if let Some(span) = abi_name.opt_span() {
                                self.add_hover_label(span, format!("abi {}", abi_name.name));
                            }
                        }
                    }
                    self.collect_block(then_branch, scopes);
                }

                if let Some(block) = else_branch {
                    self.collect_block(block, scopes);
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.collect_expr(scrutinee, scopes);

                for arm in arms {
                    self.collect_match_arm(arm, scopes, scrutinee.node.ty.clone());
                }
            }
            TypedExprKind::Yield { abis } => {
                for abi in abis {
                    self.add_usage(expr.span, abi.name.as_str(), scopes);
                }
            }
            TypedExprKind::Call { callee, args } => {
                // Check if callee is an identifier to look up function doc
                if let TypedExprKind::ScopedName { name, .. } = &callee.node.kind {
                    // TODO: handle scoping properly
                    let identifier = name.last().unwrap();
                    if let Some((signature, doc)) =
                        self.function_docs.get(&identifier.name).cloned()
                    {
                        let usage_span = identifier.span_or(callee.span);
                        self.add_hover_label_with_doc(usage_span, signature, Some(doc));
                        self.add_usage(usage_span, &identifier.name, scopes);
                    } else {
                        self.collect_expr(callee, scopes);
                    }
                } else {
                    self.collect_expr(callee, scopes);
                }

                for arg in args {
                    self.collect_expr(arg, scopes);
                }
            }
            TypedExprKind::Disclose { expr: inner } => {
                self.collect_expr(inner, scopes);
            }
            TypedExprKind::Emit { callee, args } => {
                self.collect_expr(callee, scopes);
                for arg in args {
                    self.collect_expr(arg, scopes);
                }
            }
            TypedExprKind::Raise { callee, args } => {
                self.collect_expr(callee, scopes);
                for arg in args {
                    self.collect_expr(arg, scopes);
                }
            }
            TypedExprKind::Runtime { callee, args } => {
                self.collect_expr(callee, scopes);
                for arg in args {
                    self.collect_expr(arg, scopes);
                }
            }
        }
    }

    fn collect_match_arm(
        &mut self,
        arm: &TypedMatchArm,
        scopes: &mut Vec<HashMap<String, Span>>,
        scrutinee_ty: Type,
    ) {
        scopes.push(HashMap::new());

        self.collect_pattern(&arm.pattern, scopes, Some(scrutinee_ty));

        self.collect_block(&arm.body, scopes);

        scopes.pop();
    }

    fn collect_pattern(
        &mut self,
        pattern: &TypedPattern,
        scopes: &mut Vec<HashMap<String, Span>>,
        expected_ty: Option<Type>,
    ) {
        match pattern {
            TypedPattern::Binding(identifier) => {
                if let Some(span) = identifier.opt_span() {
                    self.definition_entries.push(DefinitionEntry {
                        usage: span,
                        target: span,
                    });

                    if let Some(scope) = scopes.last_mut() {
                        scope.insert(identifier.name.clone(), span);
                    }

                    if let Some(ty) = expected_ty.as_ref() {
                        self.add_hover_span(span, ty);
                    }
                }
            }
            TypedPattern::Wildcard => {
                // Wildcard patterns don't introduce bindings or references
            }
            TypedPattern::Literal(_) => {
                // Literal patterns don't introduce bindings or references
            }
            TypedPattern::Struct { name, fields } => {
                // TODO: in `Foo::Bar`, add usage for `Foo` too
                let name = name.last().unwrap();
                self.add_type_usage(name.opt_span(), &name.name);

                // Add hover for struct name with doc comment
                if let Some(span) = name.opt_span()
                    && let Some(ty) = self.struct_types.get(&name.name).cloned()
                {
                    let struct_doc = self.struct_docs.get(&name.name).cloned();
                    self.add_hover_span_with_doc(span, &ty, struct_doc);
                }

                for field in fields {
                    self.add_struct_field_usage(
                        field.name.opt_span(),
                        &name.name,
                        &field.name.name,
                    );

                    let field_ty = self.lookup_struct_field_type(&name.name, &field.name.name);

                    if let Some(span) = field.name.opt_span()
                        && let Some(ty) = field_ty.as_ref()
                    {
                        self.add_hover_span(span, ty);
                    }

                    self.collect_pattern(&field.pattern, scopes, field_ty);
                }
            }
            TypedPattern::Tuple { .. } => {
                // TODO
            }
            TypedPattern::Constant { .. } => {
                // TODO
            }
        }
    }

    fn collect_struct_literal_field(
        &mut self,
        struct_name: &str,
        field: &TypedStructFieldInitializer,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        self.collect_expr(&field.value, scopes);

        self.add_struct_field_usage(field.name.opt_span(), struct_name, &field.name.name);
    }

    fn add_hover_span(&mut self, span: Span, ty: &Type) {
        self.add_hover_span_with_doc(span, ty, None);
    }

    fn add_hover_span_with_doc(&mut self, span: Span, ty: &Type, doc: Option<String>) {
        if span.end <= span.start {
            return;
        }

        self.hover_entries.push(HoverEntry {
            span,
            label: ty.to_string(),
            doc,
        });
    }

    fn add_hover_label(&mut self, span: Span, label: impl Into<String>) {
        self.add_hover_label_with_doc(span, label, None);
    }

    fn add_hover_label_with_doc(
        &mut self,
        span: Span,
        label: impl Into<String>,
        doc: Option<String>,
    ) {
        if span.end <= span.start {
            return;
        }

        self.hover_entries.push(HoverEntry {
            span,
            label: label.into(),
            doc,
        });
    }

    fn add_usage(&mut self, span: Span, name: &str, scopes: &[HashMap<String, Span>]) {
        if span == DUMMY_SPAN {
            return;
        }

        if let Some(target_span) = self.lookup_definition(name, scopes) {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: target_span,
            });
        }
    }

    fn add_type_usage(&mut self, span: Option<Span>, name: &str) {
        let Some(usage_span) = span else { return };

        if let Some(target_span) = self.type_definitions.get(name).copied() {
            self.definition_entries.push(DefinitionEntry {
                usage: usage_span,
                target: target_span,
            });
        }
    }

    fn add_struct_field_usage(&mut self, span: Option<Span>, struct_name: &str, field_name: &str) {
        let Some(usage_span) = span else { return };

        if let Some(target_span) = self
            .struct_field_definitions
            .get(struct_name)
            .and_then(|fields| fields.get(field_name))
        {
            self.definition_entries.push(DefinitionEntry {
                usage: usage_span,
                target: *target_span,
            });
        }
    }

    fn add_enum_variant_field_usage(
        &mut self,
        span: Option<Span>,
        enum_name: &str,
        variant_name: &str,
        field_name: &str,
    ) {
        let Some(usage_span) = span else { return };

        let key = EnumVariantKey::new(enum_name, variant_name);
        if let Some(target_span) = self
            .enum_variant_field_definitions
            .get(&key)
            .and_then(|fields| fields.get(field_name))
        {
            self.definition_entries.push(DefinitionEntry {
                usage: usage_span,
                target: *target_span,
            });
        }
    }

    fn add_enum_variant_usage(&mut self, span: Option<Span>, enum_name: &str, variant_name: &str) {
        let Some(usage_span) = span else { return };

        if let Some(target_span) = self
            .enum_variant_definitions
            .get(enum_name)
            .and_then(|variants| variants.get(variant_name))
        {
            self.definition_entries.push(DefinitionEntry {
                usage: usage_span,
                target: *target_span,
            });
        }
    }

    fn add_field_access_usage(&mut self, span: Option<Span>, ty: &Type, field_name: &str) {
        let Some(usage_span) = span else { return };

        if !matches!(ty, Type::Record(_)) {
            return;
        }

        for entry in &self.struct_type_index {
            if entry.ty == *ty
                && let Some(target_span) = self
                    .struct_field_definitions
                    .get(&entry.name)
                    .and_then(|fields| fields.get(field_name))
            {
                self.definition_entries.push(DefinitionEntry {
                    usage: usage_span,
                    target: *target_span,
                });

                break;
            }
        }
    }

    fn lookup_struct_field_type(&self, struct_name: &str, field_name: &str) -> Option<Type> {
        self.struct_field_types
            .get(struct_name)
            .and_then(|fields| fields.get(field_name))
            .cloned()
    }

    /// Find the struct name for a given type by checking the `struct_type_index`.
    fn find_struct_name_for_type(&self, ty: &Type) -> Option<String> {
        if !matches!(ty, Type::Record(_)) {
            return None;
        }

        self.struct_type_index
            .iter()
            .find(|entry| &entry.ty == ty)
            .map(|entry| entry.name.clone())
    }

    /// Look up the doc comment for a type (struct or enum).
    fn doc_for_type(&self, ty: &Type) -> Option<String> {
        if let Some(struct_name) = self.find_struct_name_for_type(ty) {
            return self.struct_docs.get(&struct_name).cloned();
        }

        if matches!(ty, Type::Enum(_)) {
            return self
                .enum_types
                .iter()
                .find(|(_, enum_ty)| *enum_ty == ty)
                .and_then(|(name, _)| self.enum_docs.get(name))
                .cloned();
        }

        None
    }

    fn lookup_enum_struct_field_type(
        &self,
        enum_name: &str,
        variant_name: &str,
        field_name: &str,
    ) -> Option<Type> {
        match self
            .enum_variant_infos
            .get(&EnumVariantKey::new(enum_name, variant_name))
        {
            Some(EnumVariantPayloadInfo::Struct(fields)) => fields
                .iter()
                .find(|(name, _)| name == field_name)
                .map(|(_, ty)| ty.clone()),
            _ => None,
        }
    }

    fn lookup_enum_tuple_types(&self, enum_name: &str, variant_name: &str) -> Option<Vec<Type>> {
        match self
            .enum_variant_infos
            .get(&EnumVariantKey::new(enum_name, variant_name))
        {
            Some(EnumVariantPayloadInfo::Tuple(types)) => Some(types.clone()),
            _ => None,
        }
    }

    fn collect_type_annotations_from_ast(&mut self, program: &Program) {
        for definition in &program.definitions {
            match &definition.node {
                untyped_ast::Definition::Function(function) => {
                    self.collect_type_annotation_function(function);
                }
                untyped_ast::Definition::Struct(definition) => {
                    for field in &definition.fields {
                        self.collect_type_annotation_node(&field.ty);
                    }
                }
                untyped_ast::Definition::Enum(definition) => {
                    for variant in &definition.variants {
                        match &variant.payload {
                            untyped_ast::EnumVariantPayload::Unit => {}
                            untyped_ast::EnumVariantPayload::Tuple(types) => {
                                for ty in types {
                                    self.collect_type_annotation_node(ty);
                                }
                            }
                            untyped_ast::EnumVariantPayload::Struct(fields) => {
                                for field in fields {
                                    self.collect_type_annotation_node(&field.ty);
                                }
                            }
                        }
                    }
                }
                untyped_ast::Definition::Utxo(definition) => {
                    for part in &definition.parts {
                        match part {
                            untyped_ast::UtxoPart::Storage(vars) => {
                                for var in vars {
                                    self.collect_type_annotation_node(&var.ty);
                                }
                            }
                            untyped_ast::UtxoPart::Function(function) => {
                                self.collect_type_annotation_function(function);
                            }
                            untyped_ast::UtxoPart::AbiImpl { abi: _, parts } => {
                                for part in parts {
                                    self.collect_type_annotation_function(part);
                                }
                            }
                        }
                    }
                }
                untyped_ast::Definition::Abi(definition) => {
                    for part in &definition.parts {
                        match part {
                            untyped_ast::AbiPart::Event(event) => {
                                for param in &event.params {
                                    self.collect_type_annotation_node(&param.ty);
                                }
                            }
                            untyped_ast::AbiPart::Effect(effect) => {
                                for param in &effect.params {
                                    self.collect_type_annotation_node(&param.ty);
                                }
                                if let Some(ret) = &effect.return_type {
                                    self.collect_type_annotation_node(ret);
                                }
                            }
                            untyped_ast::AbiPart::FnDecl(decl) => {
                                for param in &decl.params {
                                    self.collect_type_annotation_node(&param.ty);
                                }
                                if let Some(ret) = &decl.return_type {
                                    self.collect_type_annotation_node(ret);
                                }
                            }
                        }
                    }
                }
                untyped_ast::Definition::Token(_) => {
                    // `token` is parse-only for now: no annotations collected yet.
                }
                untyped_ast::Definition::Import(_) => {
                    // Imports don't have type annotations to collect
                }
                untyped_ast::Definition::Contract => {
                    // `contract;` has no annotations.
                }
            }
        }
    }

    fn collect_type_annotation_function(&mut self, function: &FunctionDef) {
        for param in &function.params {
            self.collect_type_annotation_node(&param.ty);
        }
        if let Some(ret) = &function.return_type {
            self.collect_type_annotation_node(ret);
        }
        self.collect_block_annotations_from_ast(&function.body);
    }

    fn collect_block_annotations_from_ast(&mut self, block: &untyped_ast::Block) {
        for statement in &block.statements {
            self.collect_statement_annotations_from_ast(&statement.node);
        }
    }

    fn collect_statement_annotations_from_ast(&mut self, statement: &untyped_ast::Statement) {
        match statement {
            untyped_ast::Statement::VariableDeclaration { ty, value, .. } => {
                if let Some(ty) = ty {
                    self.collect_type_annotation_node(ty);
                }
                self.collect_expr_annotations_from_ast(&value.node);
            }
            untyped_ast::Statement::While { body, .. } => {
                self.collect_block_annotations_from_ast(body);
            }
            untyped_ast::Statement::Expression(expr) => {
                self.collect_expr_annotations_from_ast(&expr.node);
            }
            untyped_ast::Statement::Return(Some(expr)) => {
                self.collect_expr_annotations_from_ast(&expr.node);
            }
            untyped_ast::Statement::Return(None) => {}
            untyped_ast::Statement::Resume => {}
            untyped_ast::Statement::Assignment { target: _, value } => {
                self.collect_expr_annotations_from_ast(&value.node);
            }
        }
    }

    fn collect_expr_annotations_from_ast(&mut self, expr: &untyped_ast::Expr) {
        match expr {
            untyped_ast::Expr::Block(block) => self.collect_block_annotations_from_ast(block),
            untyped_ast::Expr::If {
                branches,
                else_branch,
            } => {
                for (_, block) in branches {
                    self.collect_block_annotations_from_ast(block);
                }
                if let Some(block) = else_branch {
                    self.collect_block_annotations_from_ast(block);
                }
            }
            untyped_ast::Expr::Match { scrutinee, arms } => {
                self.collect_expr_annotations_from_ast(&scrutinee.node);
                for arm in arms {
                    self.collect_block_annotations_from_ast(&arm.body);
                }
            }
            untyped_ast::Expr::Disclose { expr } => {
                self.collect_expr_annotations_from_ast(&expr.node);
            }
            _ => {}
        }
    }

    fn collect_type_annotation_node(&mut self, annotation: &TypeAnnotation) {
        self.add_type_usage(annotation.name.opt_span(), &annotation.name.name);

        if let Some(span) = annotation.name.opt_span()
            && let Some(label) = self.type_label_for_name(&annotation.name.name)
        {
            // Look up doc comment for struct or enum types
            let doc = self
                .struct_docs
                .get(&annotation.name.name)
                .or_else(|| self.enum_docs.get(&annotation.name.name))
                .cloned();
            self.add_hover_label_with_doc(span, label, doc);
        }

        for generic in &annotation.generics {
            self.collect_type_annotation_node(generic);
        }
    }

    fn type_label_for_name(&self, name: &str) -> Option<String> {
        // Generic types: show generic definition with named params (e.g. `enum Option<T> { ... }`)
        if let Some(def) = self.generic_types.get(name) {
            return Some(def.ty.display_with_params(&def.param_name_map()));
        }
        match name {
            "i64" => Some(Type::int().to_string()),
            "bool" => Some(Type::bool().to_string()),
            "()" => Some(Type::unit().to_string()),
            _ => self
                .struct_types
                .get(name)
                .or_else(|| self.enum_types.get(name))
                .map(ToString::to_string),
        }
    }

    /// Resolve and add hover for an enum variant, trying generic param labels,
    /// then indexed variant info, then a concrete type fallback.
    fn add_variant_hover(
        &mut self,
        span: Option<Span>,
        enum_name: &str,
        variant_name: &str,
        fallback_ty: Option<&Type>,
    ) {
        let Some(span) = span else { return };

        let label = self
            .generic_variant_label(enum_name, variant_name)
            .or_else(|| self.enum_variant_label_from_maps(enum_name, variant_name))
            .or_else(|| {
                fallback_ty.and_then(|ty| Self::enum_variant_label_from_type(ty, variant_name))
            });

        if let Some(label) = label {
            let key = EnumVariantKey::new(enum_name, variant_name);
            let variant_doc = self.enum_variant_docs.get(&key).cloned();
            self.add_hover_label_with_doc(span, label, variant_doc);
        }
    }

    /// Add hover for an enum type name, preferring the generic definition (e.g. `enum Option<T> { ... }`)
    /// over a concrete instantiation (e.g. `enum Option<i64> { ... }`).
    fn add_generic_or_concrete_type_hover(
        &mut self,
        span: Span,
        name: &str,
        fallback_ty: Option<&Type>,
        doc: Option<String>,
    ) {
        if let Some(def) = self.generic_types.get(name) {
            let label = def.ty.display_with_params(&def.param_name_map());
            self.add_hover_label_with_doc(span, label, doc);
        } else {
            let ty = self.enum_types.get(name).or(fallback_ty).cloned();
            if let Some(ty) = ty {
                self.add_hover_span_with_doc(span, &ty, doc);
            }
        }
    }

    fn enum_variant_label_from_maps(&self, enum_name: &str, variant_name: &str) -> Option<String> {
        self.enum_variant_infos
            .get(&EnumVariantKey::new(enum_name, variant_name))
            .map(|info| format_enum_variant_hover_from_info(enum_name, variant_name, info))
    }

    /// Get variant label from generic type definition using named type params.
    fn generic_variant_label(&self, enum_name: &str, variant_name: &str) -> Option<String> {
        let def = self.generic_types.get(enum_name)?;
        let params = def.param_name_map();
        let Type::Enum(enum_type) = &def.ty else {
            return None;
        };
        format_variant_with_params(&enum_type.name, enum_type, variant_name, &params)
    }

    /// Extract variant label from a concrete `Type::Enum` value.
    fn enum_variant_label_from_type(ty: &Type, variant_name: &str) -> Option<String> {
        let Type::Enum(enum_type) = ty else {
            return None;
        };
        let variant = enum_type.variants.iter().find(|v| v.name == variant_name)?;
        let info = match &variant.kind {
            EnumVariantKind::Unit => EnumVariantPayloadInfo::Unit,
            EnumVariantKind::Tuple(types) => EnumVariantPayloadInfo::Tuple(types.clone()),
            EnumVariantKind::Struct(fields) => EnumVariantPayloadInfo::Struct(
                fields
                    .iter()
                    .map(|f| (f.name.to_string(), f.ty.clone()))
                    .collect(),
            ),
        };
        Some(format_enum_variant_hover_from_info(
            &enum_type.name,
            variant_name,
            &info,
        ))
    }

    /// Extract tuple payload types from a concrete `Type::Enum` value.
    fn enum_variant_tuple_types_from_type(ty: &Type, variant_name: &str) -> Option<Vec<Type>> {
        let Type::Enum(enum_type) = ty else {
            return None;
        };
        let variant = enum_type.variants.iter().find(|v| v.name == variant_name)?;
        match &variant.kind {
            EnumVariantKind::Tuple(types) => Some(types.clone()),
            _ => None,
        }
    }

    fn lookup_definition(&self, name: &str, scopes: &[HashMap<String, Span>]) -> Option<Span> {
        // Check local scopes first (variables, parameters)
        for scope in scopes.iter().rev() {
            if let Some(span) = scope.get(name) {
                return Some(*span);
            }
        }

        // Check function definitions
        if let Some(span) = self.function_definitions.get(name) {
            return Some(*span);
        }

        // Check imported names
        if let Some(span) = self.import_definitions.get(name) {
            return Some(*span);
        }

        None
    }

    fn span_to_range(&self, span: Span) -> Range {
        Range {
            start: self.offset_to_position(span.start),
            end: self.offset_to_position(span.end),
        }
    }

    fn position_to_offset(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;

        if line >= self.rope.len_lines() {
            return None;
        }

        let line_start = self.rope.line_to_char(line);

        let line_length = self.rope.line(line).len_chars();

        let column = position.character as usize;

        let clamped_column = column.min(line_length);

        Some(line_start + clamped_column)
    }

    fn offset_to_position(&self, mut offset: usize) -> Position {
        let total = self.rope.len_chars();

        if offset > total {
            offset = total;
        }

        let line = self.rope.char_to_line(offset);

        let line_start = self.rope.line_to_char(line);

        let column = offset.saturating_sub(line_start);

        Position {
            line: line as u32,
            character: column as u32,
        }
    }

    fn collect_document_symbols(&self, program: &TypedProgram) -> Vec<DocumentSymbol> {
        program
            .definitions
            .iter()
            .filter_map(|definition| match definition {
                TypedDefinition::Import(_)
                | TypedDefinition::Token(_)
                | TypedDefinition::Contract => None,
                TypedDefinition::Function(function) => self.function_symbol(function),
                TypedDefinition::Struct(definition) => self.struct_symbol(definition),
                TypedDefinition::Enum(definition) => self.enum_symbol(definition),
                TypedDefinition::Utxo(definition) => self.utxo_symbol(definition),
                TypedDefinition::Abi(definition) => self.abi_symbol(definition),
            })
            .collect()
    }

    fn function_symbol(&self, function: &TypedFunctionDef) -> Option<DocumentSymbol> {
        let name_span = function.name.opt_span()?;

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
            name: function.name.name.clone(),
            detail: None,
            kind: SymbolKind::FUNCTION,
            tags: None,
            deprecated: None,
            range: self.span_to_range(name_span),
            selection_range: self.span_to_range(name_span),
            children: None,
        };

        Some(symbol)
    }

    fn struct_symbol(&self, definition: &TypedStructDef) -> Option<DocumentSymbol> {
        let name_span = definition.name.opt_span()?;
        let mut children = Vec::new();
        for field in &definition.fields {
            if let Some(span) = field.name.opt_span() {
                #[allow(deprecated)]
                let child = DocumentSymbol {
                    name: field.name.name.clone(),
                    detail: Some(field.ty.to_string()),
                    kind: SymbolKind::FIELD,
                    tags: None,
                    deprecated: None,
                    range: self.span_to_range(span),
                    selection_range: self.span_to_range(span),
                    children: None,
                };
                children.push(child);
            }
        }

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
            name: definition.name.name.clone(),
            detail: Some(definition.ty.to_string()),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            range: self.span_to_range(name_span),
            selection_range: self.span_to_range(name_span),
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        };

        Some(symbol)
    }

    fn enum_symbol(&self, definition: &TypedEnumDef) -> Option<DocumentSymbol> {
        let name_span = definition.name.opt_span()?;
        let mut children = Vec::new();
        for variant in &definition.variants {
            if let Some(span) = variant.name.opt_span() {
                let detail = match &variant.payload {
                    TypedEnumVariantPayload::Unit => None,
                    TypedEnumVariantPayload::Tuple(types) => {
                        if types.is_empty() {
                            Some("()".to_string())
                        } else {
                            Some(
                                types
                                    .iter()
                                    .map(ToString::to_string)
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            )
                        }
                    }
                    TypedEnumVariantPayload::Struct(fields) => {
                        if fields.is_empty() {
                            Some("{ }".to_string())
                        } else {
                            Some(
                                fields
                                    .iter()
                                    .map(|field| format!("{}: {}", field.name.name, field.ty))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            )
                        }
                    }
                };
                #[allow(deprecated)]
                let child = DocumentSymbol {
                    name: variant.name.name.clone(),
                    detail,
                    kind: SymbolKind::ENUM_MEMBER,
                    tags: None,
                    deprecated: None,
                    range: self.span_to_range(span),
                    selection_range: self.span_to_range(span),
                    children: None,
                };
                children.push(child);
            }
        }

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
            name: definition.name.name.clone(),
            detail: Some(definition.ty.to_string()),
            kind: SymbolKind::ENUM,
            tags: None,
            deprecated: None,
            range: self.span_to_range(name_span),
            selection_range: self.span_to_range(name_span),
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        };

        Some(symbol)
    }

    fn utxo_symbol(&self, definition: &TypedUtxoDef) -> Option<DocumentSymbol> {
        let name_span = definition.name.opt_span()?;
        let mut children = Vec::new();
        for part in &definition.parts {
            match part {
                TypedUtxoPart::Storage(vars) => {
                    for var in vars {
                        if let Some(span) = var.name.opt_span() {
                            #[allow(deprecated)]
                            let child = DocumentSymbol {
                                name: var.name.name.clone(),
                                detail: Some(var.ty.to_string()),
                                kind: SymbolKind::ENUM_MEMBER,
                                tags: None,
                                deprecated: None,
                                range: self.span_to_range(span),
                                selection_range: self.span_to_range(span),
                                children: None,
                            };
                            children.push(child);
                        }
                    }
                }
                TypedUtxoPart::Function(function) => {
                    children.extend(self.function_symbol(function));
                }
                TypedUtxoPart::AbiImpl { abi, span, parts } => {
                    let impl_children = parts
                        .iter()
                        .filter_map(|function| self.function_symbol(function))
                        .collect::<Vec<_>>();
                    children.push(DocumentSymbol {
                        name: abi.to_compact_string(),
                        detail: Some(abi.to_string()),
                        kind: SymbolKind::INTERFACE,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: self.span_to_range(*span),
                        selection_range: self.span_to_range(*span),
                        children: Some(impl_children),
                    });
                }
            }
        }

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
            name: definition.name.name.clone(),
            detail: None,
            kind: SymbolKind::CLASS,
            tags: None,
            deprecated: None,
            range: self.span_to_range(name_span),
            selection_range: self.span_to_range(name_span),
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        };

        Some(symbol)
    }

    fn abi_symbol(&self, definition: &TypedAbiDef) -> Option<DocumentSymbol> {
        let name_span = definition.name.opt_span()?;
        let mut children = Vec::new();

        for part in &definition.parts {
            match part {
                TypedAbiPart::Event(event) => {
                    if let Some(span) = event.name.opt_span() {
                        let params = event
                            .params
                            .iter()
                            .map(|p| format!("{}: {}", p.name.name, p.ty))
                            .collect::<Vec<_>>()
                            .join(", ");

                        let detail = Some(format!("({params})"));

                        #[allow(deprecated)]
                        let child = DocumentSymbol {
                            name: event.name.name.clone(),
                            detail,
                            kind: SymbolKind::EVENT,
                            tags: None,
                            deprecated: None,
                            range: self.span_to_range(span),
                            selection_range: self.span_to_range(span),
                            children: None,
                        };

                        children.push(child);
                    }
                }
                TypedAbiPart::Effect(effect) => {
                    if let Some(span) = effect.name.opt_span() {
                        let params = effect
                            .params
                            .iter()
                            .map(|p| format!("{}: {}", p.name.name, p.ty))
                            .collect::<Vec<_>>()
                            .join(", ");

                        let ret = match &effect.return_type {
                            Type::Unit => String::new(),
                            ty => format!(" -> {ty}"),
                        };

                        let detail = Some(format!("({params}){ret}"));

                        #[allow(deprecated)]
                        let child = DocumentSymbol {
                            name: effect.name.name.clone(),
                            detail,
                            kind: SymbolKind::EVENT,
                            tags: None,
                            deprecated: None,
                            range: self.span_to_range(span),
                            selection_range: self.span_to_range(span),
                            children: None,
                        };

                        children.push(child);
                    }
                }
                TypedAbiPart::FnDecl(decl) => {
                    if let Some(span) = decl.name.opt_span() {
                        let params = decl
                            .params
                            .iter()
                            .map(|p| format!("{}: {}", p.name.name, p.ty))
                            .collect::<Vec<_>>()
                            .join(", ");

                        let detail = Some(format!(
                            "({}) -> {}",
                            params,
                            decl.return_type.to_compact_string()
                        ));

                        #[allow(deprecated)]
                        let child = DocumentSymbol {
                            name: decl.name.name.clone(),
                            detail,
                            kind: SymbolKind::METHOD,
                            tags: None,
                            deprecated: None,
                            range: self.span_to_range(span),
                            selection_range: self.span_to_range(span),
                            children: None,
                        };

                        children.push(child);
                    }
                }
            }
        }

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
            name: definition.name.name.clone(),
            detail: None,
            kind: SymbolKind::INTERFACE,
            tags: None,
            deprecated: None,
            range: self.span_to_range(name_span),
            selection_range: self.span_to_range(name_span),
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        };

        Some(symbol)
    }
}

#[derive(Debug, Clone)]
struct HoverEntry {
    span: Span,
    label: String,
    doc: Option<String>,
}

impl HoverEntry {
    fn contains(&self, offset: usize) -> bool {
        self.span.start <= offset && offset < self.span.end
    }

    fn len(&self) -> usize {
        self.span.end.saturating_sub(self.span.start)
    }
}

#[derive(Debug, Clone)]
struct DefinitionEntry {
    usage: Span,
    target: Span,
}

const HOVER_WIDTH: usize = 80;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct EnumVariantKey {
    enum_name: String,
    variant_name: String,
}

impl EnumVariantKey {
    fn new(enum_name: &str, variant_name: &str) -> Self {
        Self {
            enum_name: enum_name.to_string(),
            variant_name: variant_name.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum EnumVariantPayloadInfo {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<(String, Type)>),
}

fn format_enum_variant_hover_from_info(
    enum_name: &str,
    variant_name: &str,
    info: &EnumVariantPayloadInfo,
) -> String {
    match info {
        EnumVariantPayloadInfo::Unit => format!("{enum_name}::{variant_name}"),
        EnumVariantPayloadInfo::Tuple(types) => {
            if types.is_empty() {
                format!("{enum_name}::{variant_name}()")
            } else {
                let payload = types
                    .iter()
                    .map(starstream_types::Type::to_compact_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{enum_name}::{variant_name}({payload})")
            }
        }
        EnumVariantPayloadInfo::Struct(fields) => {
            let rendered_fields = fields
                .iter()
                .map(|(name, ty)| (name.clone(), ty.to_compact_string()))
                .collect::<Vec<_>>();
            format_struct_variant_hover(enum_name, variant_name, &rendered_fields)
        }
    }
}

fn format_struct_variant_hover(
    enum_name: &str,
    variant_name: &str,
    fields: &[(String, String)],
) -> String {
    if fields.is_empty() {
        return format!("{enum_name}::{variant_name} {{}}");
    }

    let inline_body = fields
        .iter()
        .map(|(name, ty)| format!("{name}: {ty}"))
        .collect::<Vec<_>>()
        .join(", ");

    let inline = format!("{enum_name}::{variant_name} {{ {inline_body} }}");

    if fields.len() < 3 && inline.len() <= HOVER_WIDTH {
        return inline;
    }

    let mut out = format!("{enum_name}::{variant_name} {{\n");

    for (name, ty) in fields {
        out.push_str(&format!("    {name}: {ty},\n"));
    }

    out.push('}');

    out
}

fn format_variant_with_params(
    enum_name: &str,
    enum_type: &EnumType,
    variant_name: &str,
    params: &HashMap<TypeVarId, String>,
) -> Option<String> {
    let variant = enum_type.variants.iter().find(|v| v.name == variant_name)?;
    Some(match &variant.kind {
        EnumVariantKind::Unit => format!("{enum_name}::{variant_name}"),
        EnumVariantKind::Tuple(types) => {
            if types.is_empty() {
                format!("{enum_name}::{variant_name}()")
            } else {
                let payload = types
                    .iter()
                    .map(|ty| ty.compact_display_with_params(params))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{enum_name}::{variant_name}({payload})")
            }
        }
        EnumVariantKind::Struct(fields) => {
            let rendered: Vec<(String, String)> = fields
                .iter()
                .map(|f| (f.name.to_string(), f.ty.compact_display_with_params(params)))
                .collect();
            format_struct_variant_hover(enum_name, variant_name, &rendered)
        }
    })
}

impl DefinitionEntry {
    fn contains(&self, offset: usize) -> bool {
        self.usage.start <= offset && offset < self.usage.end
    }

    fn len(&self) -> usize {
        self.usage.end.saturating_sub(self.usage.start)
    }
}

#[derive(Debug, Clone)]
struct StructTypeEntry {
    name: String,
    ty: Type,
}

/// Convert an LSP `Uri` (`file:///abs/path/to/foo.star`) to a `PathBuf`.
/// Returns `None` for non-file URIs (e.g. `untitled:`, `vscode-vfs:`) or if
/// the URI can't be percent-decoded.
pub fn uri_to_file_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    let raw = s.strip_prefix("file://")?;
    // On Windows VS Code emits `file:///C:/...`; on Unix it's `file:///abs/...`.
    // After stripping `file://`, both look like `/...` (Unix) or `/C:/...` (Win).
    let decoded = percent_decode(raw);
    Some(PathBuf::from(decoded))
}

/// Minimal percent-decoder. The LSP only sends URIs the editor produced, so
/// we don't need to handle every edge case — only `%20`-style escapes.
fn percent_decode(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let (Some(h), Some(l)) = (hex_digit(bytes[i + 1]), hex_digit(bytes[i + 2]))
        {
            out.push((h << 4) | l);
            i += 3;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

fn hex_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    }
}

/// Pick the workspace root for typechecking `file_path`.
///
/// First, look for a `workspace_folders` entry that contains `file_path` —
/// these come from the editor's `initialize` params (LSP
/// `workspaceFolders`). If multiple folders contain the file, the deepest
/// one wins, so nested workspaces still get sensible scoping.
///
/// If no announced folder contains the file (or the editor announced
/// none), fall back to the file's parent directory.
fn workspace_root_for(file_path: &Path, workspace_folders: &[PathBuf]) -> PathBuf {
    let mut best: Option<&Path> = None;
    for folder in workspace_folders {
        let candidate = fs::canonicalize(folder).unwrap_or_else(|_| folder.clone());
        if file_path.starts_with(&candidate) {
            // Prefer the deepest containing folder.
            if best.is_none_or(|cur| {
                candidate.as_path().components().count() > cur.components().count()
            }) {
                // Storing as PathBuf via unsafe gymnastics is overkill;
                // re-derive at the end.
                let _ = best.replace(folder.as_path());
            }
        }
    }
    if let Some(found) = best {
        return fs::canonicalize(found).unwrap_or_else(|_| found.to_path_buf());
    }

    file_path
        .parent()
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf)
}
