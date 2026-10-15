//! Starstream CLI parser and executor as a library.
use std::path::{Path, PathBuf};

use clap::Parser;
use miette::{Context, IntoDiagnostic};
use starstream_compiler::ModuleGraph;
use starstream_types::FileSystem;

mod build;
mod check;
mod diagnostics;
mod docs;
mod explain;
mod format;
mod lsp;
mod run;
mod style;
mod test;
mod wasm;

pub use build::Build;
pub use check::Check;
pub use docs::Docs;
pub use explain::Explain;
pub use format::Format;
pub use lsp::Lsp;
pub use run::Run;
pub use test::Test;
pub use wasm::Wasm;

/// The Starstream language and toolchain CLI.
#[derive(Debug, clap::Subcommand)]
pub enum Command {
    Docs(Docs),
    Wasm(Wasm),
    Build(Build),
    #[clap(visible_alias("fmt"))]
    Format(Format),
    Check(Check),
    Explain(Explain),
    #[clap(hide = true)]
    Lsp(Lsp),
    Run(Run),
    Test(Test),
    Ledger(starstream_ledger_cli::Args),
    Node(starstream_ledger_node::Args),
}

#[derive(Parser, Debug)]
#[clap(version)]
#[clap(propagate_version = true)]
#[command(arg_required_else_help(true))]
pub struct Cli {
    #[command(subcommand)]
    pub cmd: Command,
}

impl Cli {
    pub async fn exec(self) -> miette::Result<()> {
        match self.cmd {
            Command::Docs(d) => d.exec(),
            Command::Wasm(w) => w.exec(),
            Command::Build(b) => b.exec().map(|_| ()),
            Command::Format(f) => f.exec(),
            Command::Check(c) => c.exec(),
            Command::Explain(e) => e.exec(),
            Command::Lsp(l) => l.exec().await,
            Command::Run(r) => r.exec().await,
            Command::Test(t) => t.exec().await,
            Command::Ledger(l) => l.exec().await.map_err(|err| miette::miette!("{err}")),
            Command::Node(n) => n.exec().await.map_err(|err| miette::miette!("{err}")),
        }
    }
}

/// Yield all `.star` files under `dir`, respecting gitignore-style filtering.
pub fn starstream_files_excluding_gitignore(dir: &Path) -> impl Iterator<Item = PathBuf> + '_ {
    ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .require_git(false)
        .build()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_some_and(|t| t.is_file()))
        .map(ignore::DirEntry::into_path)
        .filter(move |d| is_starstream_path(d))
}

fn is_starstream_path(path: &Path) -> bool {
    path.extension()
        .is_some_and(|e| e == starstream_compiler::FILE_EXTENSION)
}

#[derive(clap::Args, Debug)]
pub struct InputArg {
    /// Input directory to scan for `contract;` entry points.
    /// Defaults to the current directory.
    #[arg(default_value = ".")]
    pub input: PathBuf,

    #[clap(long)]
    pub artifacts_dir: Option<PathBuf>,
}

impl InputArg {
    fn get_artifacts_dir(&self) -> miette::Result<PathBuf> {
        // Use `--artifacts-dir` if specified.
        if let Some(path) = &self.artifacts_dir {
            std::fs::create_dir_all(path)
                .into_diagnostic()
                .wrap_err_with(|| format!("error creating `{}`", path.display()))?;
            return Ok(path.clone());
        }

        // Otherwise use the input directory, or parent if it was a file, + "artifacts"
        let mut path = std::fs::canonicalize(&self.input).unwrap_or_else(|_| self.input.clone());
        if let Ok(meta) = path.metadata()
            && meta.is_file()
        {
            path.pop();
        }
        path.push("artifacts");
        std::fs::create_dir_all(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("error creating `{}`", path.display()))?;
        Ok(path)
    }

    fn to_module_graph(&self, fs: &mut FileSystem) -> miette::Result<ModuleGraph> {
        let graph = match ModuleGraph::from(fs, &self.input) {
            Ok(g) => g,
            Err(errors) => {
                for error in errors {
                    diagnostics::print_report(miette::Report::new(error))?;
                }
                std::process::exit(1);
            }
        };

        if graph.contract_entries().is_empty() {
            return Err(miette::miette!(
                "no contracts found under `{}` (looking for `.star` files containing `contract;`)",
                self.input.display(),
            ));
        }

        Ok(graph)
    }
}

fn wasmtime_error_to_report(error: wasmtime::Error) -> miette::Report {
    miette::Report::msg(format!("{:?}", error))
}
