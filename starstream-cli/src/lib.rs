//! Starstream CLI parser and executor as a library.
use std::path::{Path, PathBuf};

use clap::Parser;

mod build;
mod check;
mod diagnostics;
mod docs;
mod explain;
mod format;
mod lsp;
mod project;
mod style;
mod wasm;

pub use build::Build;
pub use check::Check;
pub use docs::Docs;
pub use explain::Explain;
pub use format::Format;
pub use lsp::Lsp;
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
    pub fn exec(self) -> miette::Result<()> {
        match self.cmd {
            Command::Docs(d) => d.exec(),
            Command::Wasm(w) => w.exec(),
            Command::Build(b) => b.exec(),
            Command::Format(f) => f.exec(),
            Command::Check(c) => c.exec(),
            Command::Explain(e) => e.exec(),
            Command::Lsp(l) => l.exec(),
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
