use clap::Args;
use miette::IntoDiagnostic;
use starstream_language_server::Server as StarstreamServer;
use tokio::runtime::Builder;
use tower_lsp_server::{LspService, Server};

/// Start the Starstream language server
#[derive(Args, Debug)]
pub struct Lsp {}

impl Lsp {
    pub fn exec(self) -> miette::Result<()> {
        miette::set_panic_hook();

        let stdin = async_std::io::stdin();
        let stdout = async_std::io::stdout();

        let (service, messages) = LspService::new(StarstreamServer::new);

        let runtime = Builder::new_multi_thread().build().into_diagnostic()?;

        runtime.block_on(async { Server::new(stdin, stdout, messages).serve(service).await });

        Ok(())
    }
}
