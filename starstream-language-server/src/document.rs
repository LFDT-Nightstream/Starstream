//! State tracking for an open text document.
use std::{collections::HashMap, sync::Arc};

use ropey::Rope;
use tower_lsp_server::lsp_types::{
    DocumentSymbol, DocumentSymbolResponse, Hover, HoverContents, Location, MarkupContent,
    MarkupKind, Position, Range, SymbolKind, Uri,
};

use starstream_compiler::{
    formatter, parse_program,
    parser::ParseError,
    typecheck::typecheck_program,
    typecheck::{TypeError, TypecheckOptions, TypecheckSuccess},
};
use starstream_types::{
    Span, Spanned,
    ast::{self as untyped_ast, Program, TypeAnnotation},
    typed_ast::{
        TypedBlock, TypedDefinition, TypedEnumDef, TypedExpr, TypedExprKind, TypedFunctionDef,
        TypedMatchArm, TypedPattern, TypedProgram, TypedStatement, TypedStructDef,
        TypedStructLiteralField,
    },
    types::Type,
};

use crate::diagnostics::diagnostic_to_lsp;

/// Analysis data cached for an open document.
#[derive(Debug)]
pub struct DocumentState {
    rope: Rope,
    version: Option<i32>,
    program: Option<Arc<Program>>,
    typed: Option<TypecheckSuccess>,
    diagnostics: Vec<tower_lsp_server::lsp_types::Diagnostic>,
    hover_entries: Vec<HoverEntry>,
    definition_entries: Vec<DefinitionEntry>,
    document_symbols: Vec<DocumentSymbol>,
    type_definitions: HashMap<String, Span>,
    struct_field_definitions: HashMap<String, HashMap<String, Span>>,
    enum_variant_definitions: HashMap<String, HashMap<String, Span>>,
    struct_type_index: Vec<StructTypeEntry>,
}

impl DocumentState {
    /// Create initial document state from raw text.
    pub fn from_text(uri: &Uri, text: &str, version: Option<i32>) -> Self {
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
            struct_field_definitions: HashMap::new(),
            enum_variant_definitions: HashMap::new(),
            struct_type_index: Vec::new(),
        };

        state.reanalyse(uri, text);

        state
    }

    /// Update the stored text + version and recompute analysis artifacts.
    pub fn update(&mut self, uri: &Uri, text: &str, version: Option<i32>) {
        self.rope = Rope::from_str(text);

        self.version = version;

        self.reanalyse(uri, text);
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

    fn reanalyse(&mut self, uri: &Uri, text: &str) {
        self.diagnostics.clear();
        self.program = None;
        self.typed = None;
        self.hover_entries.clear();
        self.definition_entries.clear();
        self.document_symbols.clear();
        self.type_definitions.clear();
        self.struct_field_definitions.clear();
        self.enum_variant_definitions.clear();
        self.struct_type_index.clear();

        let parse_output = parse_program(text);

        for error in parse_output.errors() {
            self.push_parse_error(uri, error);
        }

        let program = parse_output.program().cloned().map(Arc::new);

        self.program = program;

        if let Some(program) = self.program.as_ref() {
                match typecheck_program(program.as_ref(), TypecheckOptions::default()) {
                    Ok(typed) => {
                        let program_ast = self.program.clone();
                        self.build_indexes(&typed.program, program_ast.as_deref());

                        self.typed = Some(typed);
                }
                Err(type_errors) => {
                    for error in type_errors {
                        self.push_type_error(uri, error);
                    }
                }
            }
        }
    }

    fn push_parse_error(&mut self, uri: &Uri, error: &ParseError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, error);

        self.diagnostics.push(diag);
    }

    fn push_type_error(&mut self, uri: &Uri, error: TypeError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, &error);

        self.diagnostics.push(diag);
    }

    /// Format the cached AST. Returns `None` if parsing failed.
    pub fn format(&self) -> Result<Option<String>, std::fmt::Error> {
        let program = match self.program() {
            Some(program) => program,
            None => return Ok(None),
        };

        formatter::program(program).map(Some)
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

        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("`{}`", entry.label),
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

    fn build_indexes(&mut self, program: &TypedProgram, ast: Option<&Program>) {
        self.hover_entries.clear();
        self.definition_entries.clear();
        self.type_definitions.clear();

        let mut scopes: Vec<HashMap<String, Span>> = vec![HashMap::new()];

        for definition in &program.definitions {
            self.collect_definition(definition, &mut scopes);
        }

        self.document_symbols = self.collect_document_symbols(program);

        if let Some(program_ast) = ast {
            self.collect_type_annotations_from_ast(program_ast);
        }
    }

    fn collect_definition(
        &mut self,
        definition: &TypedDefinition,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        match definition {
            TypedDefinition::Function(function) => self.collect_function(function, scopes),
            TypedDefinition::Struct(definition) => self.collect_struct(definition),
            TypedDefinition::Enum(definition) => self.collect_enum(definition),
        }
    }

    fn collect_struct(&mut self, definition: &TypedStructDef) {
        if let Some(span) = definition.name.span {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            self.type_definitions
                .insert(definition.name.name.clone(), span);

            self.add_hover_span(span, &definition.ty);

            self.struct_type_index.push(StructTypeEntry {
                name: definition.name.name.clone(),
                ty: definition.ty.clone(),
            });
        }

        let entry = self
            .struct_field_definitions
            .entry(definition.name.name.clone())
            .or_default();

        for field in &definition.fields {
            if let Some(span) = field.name.span {
                entry.insert(field.name.name.clone(), span);
            }
        }
    }

    fn collect_enum(&mut self, definition: &TypedEnumDef) {
        if let Some(span) = definition.name.span {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            self.type_definitions
                .insert(definition.name.name.clone(), span);

            self.add_hover_span(span, &definition.ty);
        }

        let entry = self
            .enum_variant_definitions
            .entry(definition.name.name.clone())
            .or_default();

        for variant in &definition.variants {
            if let Some(span) = variant.name.span {
                entry.insert(variant.name.name.clone(), span);
            }
        }
    }

    fn collect_function(
        &mut self,
        function: &TypedFunctionDef,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        if let Some(span) = function.name.span {
            self.definition_entries.push(DefinitionEntry {
                usage: span,
                target: span,
            });

            self.add_hover_span(span, &function.return_type);
        }

        scopes.push(HashMap::new());

        if let Some(scope) = scopes.last_mut() {
            for param in &function.params {
                if let Some(span) = param.name.span {
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
                mutable: _,
                name,
            value,
        } => {
            self.collect_expr(value, scopes);

            if let Some(span) = name.span {
                self.definition_entries.push(DefinitionEntry {
                    usage: span,
                    target: span,
                });

                if let Some(scope) = scopes.last_mut() {
                    scope.insert(name.name.clone(), span);
                }
            }

            if let Some(span) = name.span {
                self.add_hover_span(span, &value.node.ty);
            }

        }
            TypedStatement::Assignment { target, value } => {
                self.collect_expr(value, scopes);

                self.add_usage(target.span, &target.name, scopes);

                if let Some(span) = target.span {
                    self.add_hover_span(span, &value.node.ty);
                }
            }
            TypedStatement::If {
                branches,
                else_branch,
            } => {
                for (condition, then_branch) in branches {
                    self.collect_expr(condition, scopes);
                    self.collect_block(then_branch, scopes);
                }

                if let Some(block) = else_branch {
                    self.collect_block(block, scopes);
                }
            }
            TypedStatement::While { condition, body } => {
                self.collect_expr(condition, scopes);

                self.collect_block(body, scopes);
            }
            TypedStatement::Block(block) => self.collect_block(block, scopes),
            TypedStatement::Expression(expr) => self.collect_expr(expr, scopes),
            TypedStatement::Return(Some(expr)) => self.collect_expr(expr, scopes),
            TypedStatement::Return(None) => {}
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
        self.add_hover_span(expr.span, &expr.node.ty);

        match &expr.node.kind {
            TypedExprKind::Identifier(identifier) => {
                let usage_span = identifier.span.unwrap_or(expr.span);

                self.add_usage(Some(usage_span), &identifier.name, scopes);
            }
            TypedExprKind::Unary { expr: inner, .. } => self.collect_expr(inner, scopes),
            TypedExprKind::Binary { left, right, .. } => {
                self.collect_expr(left, scopes);

                self.collect_expr(right, scopes);
            }
            TypedExprKind::Grouping(inner) => self.collect_expr(inner, scopes),
            TypedExprKind::Literal(_) => {}
            TypedExprKind::StructLiteral { name, fields } => {
                self.add_type_usage(name.span, &name.name);

                for field in fields {
                    self.collect_struct_literal_field(&name.name, field, scopes);
                }
            }
            TypedExprKind::FieldAccess { target, field } => {
                self.collect_expr(target, scopes);
                self.add_field_access_usage(field.span, &target.node.ty, &field.name);
            }
            TypedExprKind::EnumConstructor {
                enum_name,
                variant,
                payload,
            } => {
                self.add_type_usage(enum_name.span, &enum_name.name);

                self.add_enum_variant_usage(variant.span, &enum_name.name, &variant.name);

                for expr in payload {
                    self.collect_expr(expr, scopes);
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.collect_expr(scrutinee, scopes);

                for arm in arms {
                    self.collect_match_arm(arm, scopes);
                }
            }
        }
    }

    fn collect_match_arm(&mut self, arm: &TypedMatchArm, scopes: &mut Vec<HashMap<String, Span>>) {
        scopes.push(HashMap::new());

        self.collect_pattern(&arm.pattern, scopes);

        self.collect_block(&arm.body, scopes);

        scopes.pop();
    }

    fn collect_pattern(&mut self, pattern: &TypedPattern, scopes: &mut Vec<HashMap<String, Span>>) {
        match pattern {
            TypedPattern::Binding(identifier) => {
                if let Some(span) = identifier.span {
                    self.definition_entries.push(DefinitionEntry {
                        usage: span,
                        target: span,
                    });

                    if let Some(scope) = scopes.last_mut() {
                        scope.insert(identifier.name.clone(), span);
                    }
                }
            }
            TypedPattern::Struct { name, fields } => {
                self.add_type_usage(name.span, &name.name);

                for field in fields {
                    self.add_struct_field_usage(field.name.span, &name.name, &field.name.name);

                    self.collect_pattern(&field.pattern, scopes);
                }
            }
            TypedPattern::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                self.add_type_usage(enum_name.span, &enum_name.name);
                self.add_enum_variant_usage(variant.span, &enum_name.name, &variant.name);
                for pattern in payload {
                    self.collect_pattern(pattern, scopes);
                }
            }
        }
    }

    fn collect_struct_literal_field(
        &mut self,
        struct_name: &str,
        field: &TypedStructLiteralField,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        self.collect_expr(&field.value, scopes);

        self.add_struct_field_usage(field.name.span, struct_name, &field.name.name);
    }

    fn add_hover_span(&mut self, span: Span, ty: &Type) {
        if span.end <= span.start {
            return;
        }

        self.hover_entries.push(HoverEntry {
            span,
            label: ty.to_string(),
        });
    }

    fn add_usage(&mut self, span: Option<Span>, name: &str, scopes: &[HashMap<String, Span>]) {
        let Some(usage_span) = span else { return };

        if let Some(target_span) = self.lookup_definition(name, scopes) {
            self.definition_entries.push(DefinitionEntry {
                usage: usage_span,
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
            if entry.ty == *ty {
                if let Some(target_span) = self
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
    }

    fn collect_type_annotations_from_ast(&mut self, program: &Program) {
        for definition in &program.definitions {
            match definition {
                untyped_ast::Definition::Function(function) => {
                    for param in &function.params {
                        self.collect_type_annotation_node(&param.ty);
                    }
                    if let Some(ret) = &function.return_type {
                        self.collect_type_annotation_node(ret);
                    }
                    self.collect_block_annotations_from_ast(&function.body);
                }
                untyped_ast::Definition::Struct(definition) => {
                    for field in &definition.fields {
                        self.collect_type_annotation_node(&field.ty);
                    }
                }
                untyped_ast::Definition::Enum(definition) => {
                    for variant in &definition.variants {
                        for ty in &variant.payload {
                            self.collect_type_annotation_node(ty);
                        }
                    }
                }
            }
        }
    }

    fn collect_block_annotations_from_ast(&mut self, block: &untyped_ast::Block) {
        for statement in &block.statements {
            self.collect_statement_annotations_from_ast(statement);
        }
    }

    fn collect_statement_annotations_from_ast(&mut self, statement: &untyped_ast::Statement) {
        match statement {
            untyped_ast::Statement::VariableDeclaration { ty: Some(annotation), .. } => {
                self.collect_type_annotation_node(annotation);
            }
            untyped_ast::Statement::If { branches, else_branch } => {
                for (_, block) in branches {
                    self.collect_block_annotations_from_ast(block);
                }
                if let Some(block) = else_branch {
                    self.collect_block_annotations_from_ast(block);
                }
            }
            untyped_ast::Statement::While { body, .. } => {
                self.collect_block_annotations_from_ast(body);
            }
            untyped_ast::Statement::Block(block) => self.collect_block_annotations_from_ast(block),
            _ => {}
        }
    }

    fn collect_type_annotation_node(&mut self, annotation: &TypeAnnotation) {
        self.add_type_usage(annotation.name.span, &annotation.name.name);
        for generic in &annotation.generics {
            self.collect_type_annotation_node(generic);
        }
    }

    fn lookup_definition(&self, name: &str, scopes: &[HashMap<String, Span>]) -> Option<Span> {
        for scope in scopes.iter().rev() {
            if let Some(span) = scope.get(name) {
                return Some(*span);
            }
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
                TypedDefinition::Function(function) => self.function_symbol(function),
                TypedDefinition::Struct(definition) => self.struct_symbol(definition),
                TypedDefinition::Enum(definition) => self.enum_symbol(definition),
            })
            .collect()
    }

    fn function_symbol(&self, function: &TypedFunctionDef) -> Option<DocumentSymbol> {
        let name_span = function.name.span?;

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
        let name_span = definition.name.span?;
        let mut children = Vec::new();
        for field in &definition.fields {
            if let Some(span) = field.name.span {
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
        let name_span = definition.name.span?;
        let mut children = Vec::new();
        for variant in &definition.variants {
            if let Some(span) = variant.name.span {
                let detail = if variant.payload.is_empty() {
                    None
                } else {
                    Some(
                        variant
                            .payload
                            .iter()
                            .map(|ty| ty.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
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
}

#[derive(Debug, Clone)]
struct HoverEntry {
    span: Span,
    label: String,
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
