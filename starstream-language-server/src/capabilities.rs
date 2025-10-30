use tower_lsp_server::lsp_types::*;

pub fn capabilities(_: ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        // The position encoding the server picked from the encodings offered
        // by the client via the client capability `general.positionEncodings`.
        //
        // If the client didn't provide any position encodings the only valid
        // value that a server can return is 'utf-16'.
        //
        // If omitted it defaults to 'utf-16'.
        //
        // @since 3.17.0
        position_encoding: None,

        // Defines how text documents are synced.
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                // open and close notifications are sent to the server
                open_close: Some(true),
                // change notifications are sent to the server
                //
                // Documents are synced by always sending the full content of the document
                change: Some(TextDocumentSyncKind::FULL),
                ..TextDocumentSyncOptions::default()
            },
        )),

        // Defines how notebook documents are synced.
        //
        // @since 3.17.0
        notebook_document_sync: None,

        // Capabilities specific to `textDocument/selectionRange` requests.
        selection_range_provider: None,

        // The server provides hover support.
        hover_provider: None,

        // The server provides completion support.
        completion_provider: None,

        // The server provides signature help support.
        signature_help_provider: None,

        // The server provides goto definition support.
        definition_provider: None,

        // The server provides goto type definition support.
        type_definition_provider: None,

        // The server provides goto implementation support.
        implementation_provider: None,

        // The server provides find references support.
        references_provider: None,

        // The server provides document highlight support.
        document_highlight_provider: None,

        // The server provides document symbol support.
        document_symbol_provider: None,

        // The server provides workspace symbol support.
        workspace_symbol_provider: None,

        // The server provides code actions.
        code_action_provider: None,

        // The server provides code lens.
        code_lens_provider: None,

        // The server provides document formatting.
        document_formatting_provider: Some(OneOf::Left(true)),

        // The server provides document range formatting.
        document_range_formatting_provider: None,

        // The server provides document formatting on typing.
        document_on_type_formatting_provider: None,

        // The server provides rename support.
        rename_provider: None,

        // The server provides document link support.
        document_link_provider: None,

        // The server provides color provider support.
        color_provider: None,

        // The server provides folding provider support.
        folding_range_provider: None,

        // The server provides go to declaration support.
        declaration_provider: None,

        // The server provides execute command support.
        execute_command_provider: None,

        // Workspace specific server capabilities
        workspace: None,

        // Call hierarchy provider capabilities.
        call_hierarchy_provider: None,

        // Semantic tokens server capabilities.
        semantic_tokens_provider: None,

        // Whether server provides moniker support.
        moniker_provider: None,

        // The server provides linked editing range support.
        //
        // @since 3.16.0
        linked_editing_range_provider: None,

        // The server provides inline values.
        //
        // @since 3.17.0
        inline_value_provider: None,

        // The server provides inlay hints.
        //
        // @since 3.17.0
        inlay_hint_provider: None,

        // The server has support for pull model diagnostics.
        //
        // @since 3.17.0
        diagnostic_provider: None,

        // Experimental server capabilities.
        experimental: None, // Option<Value>,
    }
}
