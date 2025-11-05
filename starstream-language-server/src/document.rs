//! State tracking for an open text document.
use std::{collections::HashMap, sync::Arc};

use ropey::Rope;
use tower_lsp_server::lsp_types::{
    Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, Range, Uri,
};

use starstream_compiler::{
    formatter, parse_program,
    parser::ParseError,
    typecheck::typecheck_program,
    typecheck::{TypeError, TypecheckOptions, TypecheckSuccess},
};
use starstream_types::{
    Span, Spanned,
    ast::Program,
    typed_ast::{TypedBlock, TypedExpr, TypedExprKind, TypedProgram, TypedStatement},
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

    fn reanalyse(&mut self, uri: &Uri, text: &str) {
        self.diagnostics.clear();
        self.program = None;
        self.typed = None;
        self.hover_entries.clear();
        self.definition_entries.clear();

        let parse_output = parse_program(text);

        for error in parse_output.errors() {
            self.push_parse_error(uri, error);
        }

        let program = parse_output.program().cloned().map(Arc::new);
        self.program = program;

        if let Some(program) = self.program.as_ref() {
            match typecheck_program(program.as_ref(), TypecheckOptions::default()) {
                Ok(typed) => {
                    self.build_indexes(&typed.program);
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

    fn build_indexes(&mut self, program: &TypedProgram) {
        self.hover_entries.clear();
        self.definition_entries.clear();

        let mut scopes: Vec<HashMap<String, Span>> = vec![HashMap::new()];
        for statement in &program.statements {
            self.collect_statement(statement, &mut scopes);
        }
    }

    fn collect_statement(
        &mut self,
        statement: &TypedStatement,
        scopes: &mut Vec<HashMap<String, Span>>,
    ) {
        match statement {
            TypedStatement::VariableDeclaration { name, value } => {
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
        }
    }

    fn collect_block(&mut self, block: &TypedBlock, scopes: &mut Vec<HashMap<String, Span>>) {
        scopes.push(HashMap::new());
        for statement in &block.statements {
            self.collect_statement(statement, scopes);
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
        }
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
