//! The unified command-line interface to the Starstream language compiler and
//! test environment.

use clap::Parser;
use starstream_cli::Cli;

fn main() {
    Cli::parse().exec();
}
