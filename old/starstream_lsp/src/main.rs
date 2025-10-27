use chumsky::Parser as _;
use chumsky::span::SimpleSpan;
use dashmap::DashMap;
use ropey::Rope;
use starstream_compiler::ast::StarstreamProgram;
use starstream_compiler::error::DiagnosticAnnotation;
use starstream_compiler::{
    Symbols, do_scope_analysis, do_type_inference, error::DiagnosticError as _, starstream_program,
};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    documents: DashMap<Url, (StarstreamProgram, Symbols, Rope)>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "starstream lsp server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let text = params.text_document.text;

        let rope = Rope::from_str(&text);

        let mut ast = StarstreamProgram::default();
        let mut symbols = Symbols::default();

        self.run_compiler_stages(
            &mut ast,
            &mut symbols,
            &rope,
            params.text_document.uri.clone(),
            params.text_document.version,
        )
        .await;

        self.documents
            .insert(params.text_document.uri, (ast, symbols, rope));
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let data = self.documents.get_mut(&params.text_document.uri);

        if let Some(mut data) = data {
            let (ast, symbols, rope) = data.value_mut();

            for change in params.content_changes {
                if let Some(range) = change.range {
                    let mut start = rope.line_to_char(range.start.line as usize);

                    start += range.start.character as usize;

                    let mut end = rope.line_to_char(range.end.line as usize);

                    end += range.end.character as usize;

                    rope.remove(start..end);

                    rope.insert(start, &change.text);
                }
            }

            self.run_compiler_stages(
                ast,
                symbols,
                rope,
                params.text_document.uri.clone(),
                params.text_document.version,
            )
            .await;
        }
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {}

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }
}

impl Backend {
    async fn run_compiler_stages(
        &self,
        ast: &mut StarstreamProgram,
        symbols: &mut Symbols,
        rope: &Rope,
        uri: Url,
        version: i32,
    ) {
        if let Some(new_ast) = self.parse(rope, uri.clone(), version).await {
            *ast = new_ast;
        } else {
            return;
        };

        if let Some(new_symbols) = self.scope_resolution(ast, rope, uri.clone(), version).await {
            *symbols = new_symbols;
        } else {
            return;
        }

        self.typechecking(ast, symbols, rope, uri.clone(), version)
            .await;
    }

    async fn parse(&self, rope: &Rope, uri: Url, version: i32) -> Option<StarstreamProgram> {
        let updated_code = rope.to_string();
        let (new_ast, errors) = starstream_program()
            .parse(&updated_code)
            .into_output_errors();

        self.add_diagnostics(
            rope,
            uri,
            version,
            "parser".to_string(),
            DiagnosticSeverity::ERROR,
            errors
                .into_iter()
                .map(|error| (*error.span(), error.into_reason().to_string(), vec![])),
        )
        .await;

        new_ast
    }

    async fn scope_resolution(
        &self,
        ast: &mut StarstreamProgram,
        rope: &Rope,
        uri: Url,
        version: i32,
    ) -> Option<Symbols> {
        let mut new_ast = StarstreamProgram::default();

        std::mem::swap(ast, &mut new_ast);

        match do_scope_analysis(new_ast) {
            Ok((new_ast, symbols)) => {
                *ast = new_ast;

                Some(symbols)
            }
            Err(errors) => {
                self.add_diagnostics(
                    rope,
                    uri,
                    version,
                    "name resolution".to_string(),
                    DiagnosticSeverity::ERROR,
                    errors.into_iter().map(|error| {
                        (error.main_location(), error.message(), error.related_info())
                    }),
                )
                .await;

                None
            }
        }
    }

    async fn typechecking(
        &self,
        ast: &mut StarstreamProgram,
        symbols: &mut Symbols,
        rope: &Rope,
        uri: Url,
        version: i32,
    ) {
        let mut new_ast = StarstreamProgram::default();

        std::mem::swap(ast, &mut new_ast);

        match do_type_inference(new_ast, symbols) {
            Ok((new_ast, warnings)) => {
                self.add_diagnostics(
                    rope,
                    uri,
                    version,
                    "typechecking".to_string(),
                    DiagnosticSeverity::WARNING,
                    warnings.into_iter().map(|error| {
                        (error.main_location(), error.message(), error.related_info())
                    }),
                )
                .await;

                *ast = new_ast;
            }
            Err(errors) => {
                self.add_diagnostics(
                    rope,
                    uri,
                    version,
                    "typechecking".to_string(),
                    DiagnosticSeverity::ERROR,
                    errors.into_iter().map(|error| {
                        (error.main_location(), error.message(), error.related_info())
                    }),
                )
                .await;
            }
        }
    }

    async fn add_diagnostics(
        &self,
        rope: &Rope,
        uri: Url,
        version: i32,
        source: String,
        severity: DiagnosticSeverity,
        errors: impl Iterator<Item = (SimpleSpan, String, Vec<DiagnosticAnnotation>)>,
    ) {
        let mut diags = vec![];
        for (span, message, related_info) in errors {
            let range = span_to_lsp_range(rope, span);

            diags.push(Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: Some(source.clone()),
                message,
                related_information: Some(
                    related_info
                        .into_iter()
                        .map(
                            |DiagnosticAnnotation {
                                 location,
                                 message,
                                 color: _,
                             }| {
                                let range = span_to_lsp_range(rope, location);

                                DiagnosticRelatedInformation {
                                    location: Location {
                                        uri: uri.clone(),
                                        range,
                                    },
                                    message,
                                }
                            },
                        )
                        .collect::<Vec<_>>(),
                )
                .filter(|info| !info.is_empty()),
                tags: None,
                data: None,
            });
        }

        let clean_diagnostics = diags.is_empty();

        self.client
            .publish_diagnostics(uri.clone(), diags, Some(version))
            .await;

        if clean_diagnostics {
            self.client
                .publish_diagnostics(uri.clone(), vec![], Some(version))
                .await;
        }
    }
}

fn span_to_lsp_range(rope: &Rope, span: SimpleSpan) -> Range {
    let start_line = rope.char_to_line(span.start);
    let start_char = span.start - rope.line_to_char(start_line);

    let start = Position {
        line: start_line as u32,
        character: start_char as u32,
    };

    let end_line = rope.char_to_line(span.end);
    let end_char = span.end - rope.char_to_line(end_line);

    let end = Position {
        line: end_line as u32,
        character: end_char as u32,
    };
    Range { start, end }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: DashMap::new(),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
