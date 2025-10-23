mod capabilities;

use capabilities::capabilities;

#[cfg(debug_assertions)]
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use dashmap::DashMap;
use ropey::Rope;
use tokio::sync::OnceCell;
use tower_lsp_server::{
    Client, LanguageServer,
    jsonrpc::{self, Error, ErrorCode, Result},
    lsp_types::*,
};

#[derive(Debug)]
pub struct Server {
    pub client: Client,

    /// keep track if the server was initialised already or not
    ///
    #[cfg(debug_assertions)]
    pub initialized: AtomicBool,

    /// store the list of the workspace folders
    pub workspace_folders: OnceCell<Vec<WorkspaceFolder>>,

    document_map: DashMap<String, Rope>,
}

struct TextDocumentItem<'a> {
    uri: Uri,
    text: &'a str,
    #[allow(dead_code)]
    version: Option<i32>,
}

impl Server {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            #[cfg(debug_assertions)]
            initialized: AtomicBool::new(false),
            workspace_folders: OnceCell::new(),
            document_map: DashMap::new(),
        }
    }

    pub(crate) fn initialise_workspace_folders(
        &self,
        workspace_folders: Vec<WorkspaceFolder>,
    ) -> jsonrpc::Result<()> {
        self.workspace_folders
            .set(workspace_folders)
            .map_err(|error| jsonrpc::Error {
                code: ErrorCode::ParseError,
                message: error.to_string().into(),
                data: None,
            })?;

        Ok(())
    }

    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        let rope = ropey::Rope::from_str(params.text);

        self.document_map.insert(params.uri.to_string(), rope);
    }
}

impl LanguageServer for Server {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        #[cfg(debug_assertions)]
        debug_assert!(
            !self.initialized.load(Ordering::Acquire),
            "The server was already initialised."
        );

        let capabilities = capabilities();

        if let Some(workspace_folders) = params.workspace_folders {
            self.initialise_workspace_folders(workspace_folders)?;
        }

        #[cfg(debug_assertions)]
        self.initialized.store(true, Ordering::Release);

        self.client
            .log_message(MessageType::LOG, "Initialise")
            .await;

        Ok(InitializeResult {
            capabilities,
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        self.client
            .log_message(MessageType::INFO, "server shutdown!")
            .await;

        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Opened file: {}", params.text_document.uri.as_str()),
            )
            .await;

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Changed file: {}", params.text_document.uri.as_str()),
            )
            .await;

        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Saved file: {}", params.text_document.uri.as_str()),
            )
            .await;

        if let Some(text) = params.text {
            let item = TextDocumentItem {
                uri: params.text_document.uri,
                text: &text,
                version: None,
            };

            self.on_change(item).await;

            // _ = self.client.semantic_tokens_refresh().await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Closed file: {}", params.text_document.uri.as_str()),
            )
            .await;

        _ = self.document_map.remove(params.text_document.uri.as_str())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let text_document = params.text_document;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Formatting request: {}", text_document.uri.as_str()),
            )
            .await;

        let Some(document) = self.document_map.get(text_document.uri.as_str()) else {
            return Ok(None);
        };

        let document_string = document.to_string();
        let parse_result = starstream_compiler::parse_program(&document_string);

        for _error in parse_result.errors() {
            // todo: report parse errors, the thing is,
            //       we will likely keep a map of
            //       text_document.uri => Program
            //
            //       this means in here, instead of looking up the string
            //       we will lookup the raw ast instead of parsing in here
            //       so really we will want to surface parse errors to the editor
            //       at a different location in this code.
        }

        let Some(program) = parse_result.into_output() else {
            std::process::exit(1)
        };

        let Ok(new_text) = starstream_compiler::formatter::program(&program) else {
            self.client
                .log_message(MessageType::ERROR, "failed to format file")
                .await;

            let mut error = Error::internal_error();

            error.message = "failed to format file".into();

            return Err(error);
        };

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: u32::MAX,
            },
        };

        let text_edit = TextEdit { range, new_text };

        Ok(Some(vec![text_edit]))
    }
}
