use std::io::IsTerminal;

use clap::Args;
use miette::{Diagnostic, IntoDiagnostic};
use starstream_language_server::Server as StarstreamServer;
use tokio::runtime::Builder;
use tower_lsp_server::{LspService, Server};

/// Start the Starstream language server
#[derive(Args, Debug)]
#[clap(version(starstream_language_server::VERSION))]
pub struct Lsp {
    /// Read/write on standard in/out.
    // VSC semi-forces passing this argument.
    #[clap(long)]
    stdio: bool,
}

impl Lsp {
    pub fn exec(self) -> miette::Result<()> {
        // Log version and debug info.
        eprintln!("starstream-cli-lsp {}", starstream_language_server::VERSION);
        match std::env::current_exe() {
            Ok(path) => eprintln!("executable: {}", path.display()),
            Err(e) => eprintln!("executable unknown: {e}"),
        }
        match std::env::current_dir() {
            Ok(path) => eprintln!("directory: {}", path.display()),
            Err(e) => eprintln!("directory unknown: {e}"),
        }
        eprintln!();

        if !self.stdio && std::io::stdin().is_terminal() {
            // Show a helpful message to someone who runs `starstream lsp` in a terminal by mistake.
            return Err(miette::Report::new(UnexpectedTerminal));
        }

        let stdin = async_std::io::stdin();
        let stdout = async_std::io::stdout();

        let (service, messages) = LspService::new(StarstreamServer::new);

        let runtime = Builder::new_multi_thread().build().into_diagnostic()?;

        runtime.block_on(async { Server::new(stdin, stdout, messages).serve(service).await });

        Ok(())
    }
}

/// Error indicating that stdin was unexpectedly a terminal.
#[derive(Debug, Diagnostic)]
#[diagnostic(help("The language server should be piped to/from its client.\nPass `--stdio` to skip this check."))]
struct UnexpectedTerminal;

impl std::fmt::Display for UnexpectedTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Input appears to be a terminal.")
    }
}

impl std::error::Error for UnexpectedTerminal {}
