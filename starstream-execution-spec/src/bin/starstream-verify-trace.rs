use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::process::ExitCode;

use starstream_execution_spec::{ExecutionTrace, QuintError, QuintVerifier};

fn main() -> ExitCode {
    match run() {
        Ok(()) => {
            println!("trace satisfies the Starstream execution specification");
            ExitCode::SUCCESS
        }
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), String> {
    let mut args = std::env::args_os().skip(1);
    let Some(path) = args.next() else {
        return Err("usage: starstream-verify-trace <trace.json|->".into());
    };
    if args.next().is_some() {
        return Err("usage: starstream-verify-trace <trace.json|->".into());
    }

    let source = if path == "-" {
        let mut source = String::new();
        io::stdin()
            .read_to_string(&mut source)
            .map_err(|error| format!("failed to read trace from stdin: {error}"))?;
        source
    } else {
        fs::read_to_string(Path::new(&path)).map_err(|error| {
            format!(
                "failed to read trace at {}: {error}",
                path.to_string_lossy()
            )
        })?
    };
    let trace: ExecutionTrace = serde_json::from_str(&source)
        .map_err(|error| format!("invalid execution trace: {error}"))?;

    QuintVerifier::default()
        .verify(&trace)
        .map_err(format_quint_error)
}

fn format_quint_error(error: QuintError) -> String {
    match error {
        QuintError::Rejected(failure) => {
            format!(
                "trace violates the Starstream execution specification\n{}",
                failure.stderr
            )
        }
        other => other.to_string(),
    }
}
