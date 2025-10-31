mod capabilities;
mod diagnostics;
mod document;

use capabilities::capabilities;
use document::DocumentState;

use dashmap::{DashMap, mapref::entry::Entry};
use tower_lsp_server::{
    Client, ClientSocket, LanguageServer, LspService,
    jsonrpc::{self, Error, Result},
    lsp_types::*,
};

// At the moment LSP version == CLI version, but for completeness's sake:
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct Server {
    client: Client,

    // /// store the list of the workspace folders
    // pub workspace_folders: OnceCell<Vec<WorkspaceFolder>>,
    document_map: DashMap<Uri, DocumentState>,
}

struct TextDocumentItem<'a> {
    uri: Uri,
    text: &'a str,
    #[allow(dead_code)]
    version: Option<i32>,
}

impl Server {
    /// Create a new [LspService] configured for the Starstream language server.
    pub fn new() -> (LspService<Self>, ClientSocket) {
        // Any custom methods can be set here.
        LspService::new(Self::with_client)
    }

    fn with_client(client: Client) -> Self {
        Self {
            client,
            // workspace_folders: OnceCell::new(),
            document_map: DashMap::new(),
        }
    }

    pub(crate) fn initialise_workspace_folders(
        &self,
        _workspace_folders: Vec<WorkspaceFolder>,
    ) -> jsonrpc::Result<()> {
        // self.workspace_folders
        //     .set(workspace_folders)
        //     .map_err(|error| jsonrpc::Error {
        //         code: ErrorCode::ParseError,
        //         message: error.to_string().into(),
        //         data: None,
        //     })?;

        Ok(())
    }

    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        let uri = params.uri.clone();
        let (diagnostics, version) = match self.document_map.entry(uri.clone()) {
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().update(&uri, params.text, params.version);
                let diagnostics = occupied.get().diagnostics().to_vec();
                let version = occupied.get().version();
                (diagnostics, version)
            }
            Entry::Vacant(vacant) => {
                let state = DocumentState::from_text(&uri, params.text, params.version);
                let diagnostics = state.diagnostics().to_vec();
                let version = state.version();
                vacant.insert(state);
                (diagnostics, version)
            }
        };

        self.client
            .publish_diagnostics(uri, diagnostics, version)
            .await;
    }
}

impl LanguageServer for Server {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Server initializing...")
            .await;

        let capabilities = capabilities(params.capabilities);

        if let Some(workspace_folders) = params.workspace_folders {
            self.initialise_workspace_folders(workspace_folders)?;
        }

        Ok(InitializeResult {
            capabilities,
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Client acknowledged initialization")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        self.client
            .log_message(MessageType::INFO, "Server shutting down...")
            .await;

        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let DidOpenTextDocumentParams { text_document } = params;
        let uri = text_document.uri;
        let version = text_document.version;
        let text = text_document.text;

        self.client
            .log_message(MessageType::INFO, format!("Opened file: {}", uri.as_str()))
            .await;

        self.on_change(TextDocumentItem {
            uri,
            text: text.as_str(),
            version: Some(version),
        })
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let DidChangeTextDocumentParams {
            text_document,
            content_changes,
        } = params;
        let uri = text_document.uri;
        let version = text_document.version;
        let mut changes = content_changes.into_iter();
        let text = changes.next().map(|change| change.text).unwrap_or_default();

        self.client
            .log_message(MessageType::INFO, format!("Changed file: {}", uri.as_str()))
            .await;

        self.on_change(TextDocumentItem {
            uri,
            text: text.as_str(),
            version: Some(version),
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
            let uri = params.text_document.uri;
            let item = TextDocumentItem {
                uri,
                text: text.as_str(),
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

        if let Some((uri, _)) = self.document_map.remove(&params.text_document.uri) {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let HoverParams {
            text_document_position_params,
            ..
        } = params;
        let TextDocumentPositionParams {
            text_document,
            position,
        } = text_document_position_params;
        let uri = text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Hover request: {}", uri.as_str()),
            )
            .await;

        let hover = self
            .document_map
            .get(&uri)
            .and_then(|document| document.hover(position));

        Ok(hover)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let text_document = params.text_document;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Formatting request: {}", text_document.uri.as_str()),
            )
            .await;

        let Some(document) = self.document_map.get(&text_document.uri) else {
            return Ok(None);
        };
        let formatted = document.format();
        drop(document);

        let Ok(Some(new_text)) = formatted else {
            if formatted.is_err() {
                self.client
                    .log_message(MessageType::ERROR, "failed to format file")
                    .await;

                let mut error = Error::internal_error();
                error.message = "failed to format file".into();
                return Err(error);
            }

            return Ok(None);
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
