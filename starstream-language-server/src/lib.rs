mod capabilities;

use capabilities::capabilities;

use dashmap::DashMap;
use ropey::Rope;
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
    document_map: DashMap<String, Rope>,
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
        let rope = ropey::Rope::from_str(params.text);

        self.document_map.insert(params.uri.to_string(), rope);
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
        let parse_output = starstream_compiler::parse_program(&document_string);

        for _error in parse_output.errors() {
            // todo: report parse errors, the thing is,
            //       we will likely keep a map of
            //       text_document.uri => Program
            //
            //       this means in here, instead of looking up the string
            //       we will lookup the raw ast instead of parsing in here
            //       so really we will want to surface parse errors to the editor
            //       at a different location in this code.
        }

        let Some(program) = parse_output.into_program() else {
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
