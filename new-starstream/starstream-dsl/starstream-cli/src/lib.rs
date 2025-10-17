//! Starstream CLI parser and executor as a library.
use clap::Parser;

mod wasm;

pub use wasm::Wasm;

/// The Starstream language and toolchain CLI.
#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
pub enum Cli {
    Wasm(Wasm),
}

impl Cli {
    pub fn exec(&self) {
        match self {
            Self::Wasm(w) => w.exec(),
        }
    }
}
