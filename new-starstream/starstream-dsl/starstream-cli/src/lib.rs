//! Starstream CLI parser and executor as a library.
use clap::Parser;

mod format;
mod style;
mod wasm;

pub use format::Format;
pub use wasm::Wasm;

/// The Starstream language and toolchain CLI.
#[derive(Debug, clap::Subcommand)]
pub enum Command {
    Wasm(Wasm),
    #[clap(visible_alias("fmt"))]
    Format(Format),
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
            Command::Wasm(w) => w.exec(),
            Command::Format(f) => f.exec(),
        }
    }
}
