use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::process::ExitCode;

use starstream_interleaving_spec::{
    ExecutionTrace, QuintError, QuintVerifier, VerificationFailure,
};

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
                format_quint_diagnostics(&failure)
            )
        }
        QuintError::Typecheck(failure) => format!(
            "generated Quint replay module failed typechecking\n{}",
            format_quint_diagnostics(&failure)
        ),
        other => other.to_string(),
    }
}

fn format_quint_diagnostics(failure: &VerificationFailure) -> String {
    [failure.stdout.trim(), failure.stderr.trim()]
        .into_iter()
        .filter(|output| !output.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quint_diagnostics_include_stdout_and_stderr() {
        let failure = VerificationFailure {
            generated_source: String::new(),
            stdout: "failed action trace".into(),
            stderr: "error: Tests failed".into(),
        };

        assert_eq!(
            format_quint_diagnostics(&failure),
            "failed action trace\nerror: Tests failed"
        );
    }
}
