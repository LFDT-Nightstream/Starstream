mod capabilities;
mod diagnostics;
mod document;

use capabilities::capabilities;
use document::DocumentState;

use base64::Engine;
use dashmap::{DashMap, mapref::entry::Entry};
use serde::Deserialize;
use starstream_types::{MemoryFs, NativeFs, OverlayFs, Vfs, normalize_path};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use tower_lsp_server::{
    Client, ClientSocket, LanguageServer, LspService,
    jsonrpc::{self, Error, Result},
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
        DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
        DocumentSymbolParams, DocumentSymbolResponse, FileSystemWatcher, GlobPattern,
        GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, InitializeParams,
        InitializeResult, InitializedParams, Location, MessageType, Position, Range,
        ReferenceParams, Registration, RenameParams, TextDocumentPositionParams, TextEdit, Uri,
        WorkspaceEdit, WorkspaceFolder,
    },
};

// At the moment LSP version == CLI version, but for completeness's sake:
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct Server {
    client: Client,
    /// Workspace roots announced by the editor during `initialize`. The
    /// LSP uses these as the scan root for multi-file type-checking: when a
    /// file is edited, the first workspace folder that contains it becomes
    /// the `from_workspace` root. If the editor didn't announce any, the
    /// document falls back to its parent directory.
    workspace_folders: RwLock<Vec<PathBuf>>,
    document_map: DashMap<Uri, DocumentState>,
    /// Where files that aren't open (or synced) are read from.
    base: Arc<dyn Vfs>,
    /// Workspace files pushed by the client via `starstream/syncFiles`, for
    /// hosts where the server can't read the filesystem itself (the Wasm
    /// build running in a browser). Shadows `base`; open documents shadow
    /// this in turn.
    synced_files: RwLock<MemoryFs>,
    /// Whether to register a `workspace/didChangeWatchedFiles` watcher: the
    /// client supports it, and isn't already pushing changes to us via
    /// `starstream/syncFiles` (which would re-check everything twice).
    watch_files: AtomicBool,
}

/// `initializationOptions` understood by the server.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializationOptions {
    /// The client sends workspace files via `starstream/syncFiles`.
    #[serde(default)]
    sync_files: bool,
}

/// Params of the `starstream/syncFiles` notification. The client side lives
/// in `vscode-starstream/src/extension.ts`.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SyncFilesParams {
    /// Discard all previously synced files first.
    #[serde(default)]
    replace: bool,
    /// Files to add or update.
    #[serde(default)]
    files: Vec<SyncedFile>,
    /// Files that no longer exist.
    #[serde(default)]
    removed: Vec<Uri>,
}

/// One file in [`SyncFilesParams`].
#[derive(Debug, Deserialize)]
struct SyncedFile {
    uri: Uri,
    #[serde(flatten)]
    contents: SyncedContents,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum SyncedContents {
    /// Source files.
    Text { text: String },
    /// Binary files like `.wasm`.
    Base64 { base64: String },
}

struct TextDocumentItem<'a> {
    uri: Uri,
    text: &'a str,
    #[allow(dead_code)]
    version: Option<i32>,
}

impl Server {
    /// Create a new [`LspService`] configured for the Starstream language server.
    pub fn new() -> (LspService<Self>, ClientSocket) {
        LspService::build(Self::with_client)
            .custom_method("starstream/syncFiles", Self::sync_files)
            .finish()
    }

    fn with_client(client: Client) -> Self {
        Self {
            client,
            workspace_folders: RwLock::new(Vec::new()),
            document_map: DashMap::new(),
            base: Arc::new(NativeFs),
            synced_files: RwLock::new(MemoryFs::new()),
            watch_files: AtomicBool::new(false),
        }
    }

    pub(crate) fn initialise_workspace_folders(
        &self,
        workspace_folders: Vec<WorkspaceFolder>,
    ) -> jsonrpc::Result<()> {
        let mut folders = self.workspace_folders.write().map_err(|_| jsonrpc::Error {
            code: jsonrpc::ErrorCode::InternalError,
            message: "workspace_folders lock poisoned".into(),
            data: None,
        })?;
        folders.clear();
        for wf in workspace_folders {
            if let Some(path) = document::uri_to_file_path(&wf.uri) {
                folders.push(path);
            }
        }
        Ok(())
    }

    fn workspace_folders_snapshot(&self) -> Vec<PathBuf> {
        self.workspace_folders
            .read()
            .map(|f| f.clone())
            .unwrap_or_default()
    }

    /// The path a document is keyed by in the overlay, if it's a file URI.
    fn canonical_path(&self, uri: &Uri) -> Option<PathBuf> {
        let path = document::uri_to_file_path(uri)?;
        Some(
            self.base
                .canonicalize(&path)
                .unwrap_or_else(|_| normalize_path(&path)),
        )
    }

    /// Snapshot of the filesystem as analysis should see it: open documents
    /// over synced files over `base`. `changed` overrides one document's
    /// text, for a document whose stored state hasn't been updated yet.
    fn analysis_vfs(&self, changed: Option<(&Uri, &str)>) -> OverlayFs {
        let mut upper = self
            .synced_files
            .read()
            .map(|f| f.clone())
            .unwrap_or_default();
        for doc in self.document_map.iter() {
            if let Some(path) = self.canonical_path(doc.key()) {
                upper.insert(path, doc.rope().to_string().into_bytes());
            }
        }
        if let Some((uri, text)) = changed
            && let Some(path) = self.canonical_path(uri)
        {
            upper.insert(path, text.as_bytes());
        }
        OverlayFs::new(upper, self.base.clone())
    }

    /// Re-analyse every open document matching `filter` (other than `skip`)
    /// and republish its diagnostics.
    async fn recheck(&self, skip: Option<&Uri>, filter: impl Fn(&DocumentState) -> bool) {
        let targets: Vec<Uri> = self
            .document_map
            .iter()
            .filter(|doc| Some(doc.key()) != skip && filter(doc.value()))
            .map(|doc| doc.key().clone())
            .collect();
        if targets.is_empty() {
            return;
        }

        let folders = self.workspace_folders_snapshot();
        let vfs = self.analysis_vfs(None);
        for uri in targets {
            let Some(mut doc) = self.document_map.get_mut(&uri) else {
                continue;
            };
            let text = doc.rope().to_string();
            let version = doc.version();
            doc.update(&uri, &text, version, &folders, &vfs);
            let diagnostics = doc.diagnostics().to_vec();
            drop(doc);
            self.client
                .publish_diagnostics(uri, diagnostics, version)
                .await;
        }
    }

    /// Re-analyse open documents that import the file at `uri`.
    async fn recheck_dependents(&self, uri: &Uri) {
        if let Some(path) = self.canonical_path(uri) {
            self.recheck(Some(uri), |doc| doc.depends_on(&path)).await;
        }
    }

    async fn sync_files(&self, params: SyncFilesParams) {
        let mut skipped = Vec::new();
        {
            let Ok(mut synced) = self.synced_files.write() else {
                self.client
                    .log_message(MessageType::ERROR, "synced files lock poisoned")
                    .await;
                return;
            };
            if params.replace {
                synced.clear();
            }
            for uri in &params.removed {
                if let Some(path) = self.canonical_path(uri) {
                    synced.remove(path);
                }
            }
            for file in params.files {
                let Some(path) = self.canonical_path(&file.uri) else {
                    skipped.push(format!("{}: not a file URI", file.uri.as_str()));
                    continue;
                };
                let contents = match file.contents {
                    SyncedContents::Text { text } => text.into_bytes(),
                    SyncedContents::Base64 { base64 } => {
                        match base64::engine::general_purpose::STANDARD.decode(base64) {
                            Ok(bytes) => bytes,
                            Err(error) => {
                                skipped.push(format!("{}: {error}", file.uri.as_str()));
                                continue;
                            }
                        }
                    }
                };
                synced.insert(path, contents);
            }
        }
        for reason in skipped {
            self.client
                .log_message(MessageType::WARNING, format!("syncFiles skipped {reason}"))
                .await;
        }

        // Any open document could have been affected, including ones whose
        // imports previously failed to resolve.
        self.recheck(None, |_| true).await;
    }

    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        let uri = params.uri.clone();
        let folders = self.workspace_folders_snapshot();
        // Build before taking the entry lock: this iterates the map.
        let vfs = self.analysis_vfs(Some((&uri, params.text)));
        let (diagnostics, version) = match self.document_map.entry(uri.clone()) {
            Entry::Occupied(mut occupied) => {
                occupied
                    .get_mut()
                    .update(&uri, params.text, params.version, &folders, &vfs);
                let diagnostics = occupied.get().diagnostics().to_vec();
                let version = occupied.get().version();
                (diagnostics, version)
            }
            Entry::Vacant(vacant) => {
                let state =
                    DocumentState::from_text(&uri, params.text, params.version, &folders, &vfs);
                let diagnostics = state.diagnostics().to_vec();
                let version = state.version();
                vacant.insert(state);
                (diagnostics, version)
            }
        };

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;

        // Files importing this one see its new contents.
        self.recheck_dependents(&uri).await;
    }
}

impl LanguageServer for Server {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Server initializing...")
            .await;

        let options: InitializationOptions = params
            .initialization_options
            .and_then(|options| serde_json::from_value(options).ok())
            .unwrap_or_default();
        let can_watch = params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.did_change_watched_files.as_ref())
            .and_then(|w| w.dynamic_registration)
            .unwrap_or(false);
        self.watch_files
            .store(can_watch && !options.sync_files, Ordering::Relaxed);

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

        // Re-check open documents when imported files change on disk.
        if self.watch_files.load(Ordering::Relaxed) {
            let options = DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![FileSystemWatcher {
                    glob_pattern: GlobPattern::String("**/*.{star,wasm}".into()),
                    kind: None,
                }],
            };
            let registration = Registration {
                id: "starstream-watch-files".into(),
                method: "workspace/didChangeWatchedFiles".into(),
                register_options: serde_json::to_value(options).ok(),
            };
            if let Err(error) = self.client.register_capability(vec![registration]).await {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("failed to register file watcher: {error}"),
                    )
                    .await;
            }
        }
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let paths: Vec<PathBuf> = params
            .changes
            .iter()
            .filter_map(|change| self.canonical_path(&change.uri))
            .collect();
        // A created file may satisfy an import that previously failed, which
        // isn't in anyone's dependencies yet, so be generous.
        let created = params
            .changes
            .iter()
            .any(|change| change.typ == tower_lsp_server::lsp_types::FileChangeType::CREATED);
        self.recheck(None, |doc| {
            created || paths.iter().any(|p| doc.depends_on(p))
        })
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
        .await;
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
            self.client
                .publish_diagnostics(uri.clone(), Vec::new(), None)
                .await;
            // Dependents now see the on-disk (or synced) contents again.
            self.recheck_dependents(&uri).await;
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let GotoDefinitionParams {
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
                format!("GotoDefinition request: {}", uri.as_str()),
            )
            .await;

        let location = self
            .document_map
            .get(&uri)
            .and_then(|document| document.goto_definition(&uri, position));

        Ok(location.map(GotoDefinitionResponse::Scalar))
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

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let RenameParams {
            text_document_position,
            new_name,
            ..
        } = params;

        let TextDocumentPositionParams {
            text_document,
            position,
        } = text_document_position;

        let uri = text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!("Rename request: {}", uri.as_str()),
            )
            .await;

        let edits = self
            .document_map
            .get(&uri)
            .and_then(|document| document.rename_edits(position, &new_name));

        if let Some(edits) = edits {
            // no clue what else to do tbh, sorry clippy
            #[allow(clippy::mutable_key_type)]
            let mut changes = HashMap::new();

            changes.insert(uri.clone(), edits);

            Ok(Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }))
        } else {
            Ok(None)
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let ReferenceParams {
            text_document_position,
            context,
            ..
        } = params;

        let TextDocumentPositionParams {
            text_document,
            position,
        } = text_document_position;

        let uri = text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!("References request: {}", uri.as_str()),
            )
            .await;

        let locations = self
            .document_map
            .get(&uri)
            .and_then(|document| document.references(&uri, position, context.include_declaration));

        Ok(locations)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!("DocumentSymbol request: {}", uri.as_str()),
            )
            .await;

        let symbols = self
            .document_map
            .get(&uri)
            .and_then(|document| document.document_symbols());

        Ok(symbols)
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

#[cfg(test)]
mod tests {
    use super::*;
    use futures::StreamExt;
    use futures::executor::LocalPool;
    use futures::task::LocalSpawnExt;
    use serde_json::{Value, json};
    use std::cell::RefCell;
    use std::rc::Rc;
    use tower_lsp_server::jsonrpc::Request;
    use tower_service::Service;

    /// A `publishDiagnostics` notification: URI and messages.
    type Published = (String, Vec<String>);

    /// Drives a [`Server`] through its JSON-RPC interface, recording the
    /// diagnostics it publishes.
    struct Harness {
        pool: LocalPool,
        service: LspService<Server>,
        published: Rc<RefCell<Vec<Published>>>,
        next_id: i64,
    }

    impl Harness {
        fn new() -> Harness {
            let (service, socket) = Server::new();
            let pool = LocalPool::new();
            let published = Rc::new(RefCell::new(Vec::new()));
            let sink = published.clone();
            pool.spawner()
                .spawn_local(socket.for_each(move |request: Request| {
                    if request.method() == "textDocument/publishDiagnostics" {
                        let params = request.params().unwrap();
                        let messages = params["diagnostics"]
                            .as_array()
                            .unwrap()
                            .iter()
                            .map(|d| d["message"].as_str().unwrap().to_owned())
                            .collect();
                        sink.borrow_mut()
                            .push((params["uri"].as_str().unwrap().to_owned(), messages));
                    }
                    async {}
                }))
                .unwrap();
            Harness {
                pool,
                service,
                published,
                next_id: 0,
            }
        }

        fn send(&mut self, method: &'static str, params: Value, is_request: bool) {
            let mut request = Request::build(method).params(params);
            if is_request {
                self.next_id += 1;
                request = request.id(self.next_id);
            }
            let call = self.service.call(request.finish());
            self.pool.run_until(call).unwrap();
            self.pool.run_until_stalled();
        }

        fn notify(&mut self, method: &'static str, params: Value) {
            self.send(method, params, false);
        }

        /// The most recently published diagnostics for `uri`.
        fn diagnostics(&self, uri: &str) -> Vec<String> {
            self.published
                .borrow()
                .iter()
                .rev()
                .find(|(u, _)| u == uri)
                .map(|(_, messages)| messages.clone())
                .unwrap_or_else(|| panic!("no diagnostics published for {uri}"))
        }
    }

    const LIB: &str = "fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n";
    const MAIN: &str =
        "import { add } from \"./lib.star\";\n\nfn main() -> i64 {\n    add(1, 2)\n}\n";

    #[test]
    fn synced_and_open_files_recheck_dependents() {
        let mut h = Harness::new();
        // `/virtual-ws` doesn't exist on disk: everything comes from the
        // client, as in the browser.
        h.send(
            "initialize",
            json!({
                "capabilities": {},
                "initializationOptions": { "syncFiles": true },
                "workspaceFolders": [{ "uri": "file:///virtual-ws", "name": "ws" }],
            }),
            true,
        );
        h.notify("initialized", json!({}));
        h.notify(
            "starstream/syncFiles",
            json!({
                "replace": true,
                "files": [
                    { "uri": "file:///virtual-ws/lib.star", "text": LIB },
                    { "uri": "file:///virtual-ws/main.star", "text": MAIN },
                ],
            }),
        );

        let main = "file:///virtual-ws/main.star";
        h.notify(
            "textDocument/didOpen",
            json!({ "textDocument": {
                "uri": main, "languageId": "starstream", "version": 1, "text": MAIN,
            }}),
        );
        assert_eq!(h.diagnostics(main), Vec::<String>::new());

        // Breaking the synced lib.star re-checks main.star.
        h.notify(
            "starstream/syncFiles",
            json!({ "files": [{ "uri": "file:///virtual-ws/lib.star", "text": "fn add(" }] }),
        );
        assert_eq!(
            h.diagnostics(main),
            vec!["imported module `./lib.star` has errors".to_owned()]
        );

        // An open buffer shadows the synced file, and editing it re-checks
        // main.star too.
        let lib = "file:///virtual-ws/lib.star";
        h.notify(
            "textDocument/didOpen",
            json!({ "textDocument": {
                "uri": lib, "languageId": "starstream", "version": 1, "text": LIB,
            }}),
        );
        h.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": lib, "version": 2 },
                "contentChanges": [{ "text": LIB }],
            }),
        );
        assert_eq!(h.diagnostics(main), Vec::<String>::new());

        // Closing it falls back to the (still broken) synced contents.
        h.notify(
            "textDocument/didClose",
            json!({ "textDocument": { "uri": lib } }),
        );
        assert_eq!(h.diagnostics(main).len(), 1);
    }
}
