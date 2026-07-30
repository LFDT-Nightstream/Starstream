use std::ffi::{OsStr, OsString};
use std::fmt::Write as _;
use std::fs;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

use crate::{ExecutionEvent, ExecutionTrace};

#[derive(Clone, Debug)]
pub struct QuintVerifier {
    quint: OsString,
    spec: PathBuf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VerificationFailure {
    pub generated_source: String,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, thiserror::Error)]
pub enum QuintError {
    #[error("cannot validate an empty execution trace")]
    EmptyTrace,

    #[error("execution trace must start with Init")]
    MissingInit,

    #[error("execution trace contains another Init at index {index}")]
    RepeatedInit { index: usize },

    #[error("failed to read Quint specification at {path}: {source}")]
    ReadSpec {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("failed to write generated trace to stdin of Quint executable `{program}`: {source}")]
    WriteInput {
        program: String,
        #[source]
        source: std::io::Error,
    },

    #[error("failed to invoke Quint executable `{program}`: {source}")]
    Invoke {
        program: String,
        #[source]
        source: std::io::Error,
    },

    #[error("generated Quint replay module failed typechecking")]
    Typecheck(VerificationFailure),

    #[error("Quint rejected the execution trace")]
    Rejected(VerificationFailure),
}

impl Default for QuintVerifier {
    fn default() -> Self {
        Self::new("quint")
    }
}

impl QuintVerifier {
    #[must_use]
    pub fn new(quint: impl AsRef<OsStr>) -> Self {
        Self {
            quint: quint.as_ref().to_owned(),
            spec: Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("spec")
                .join("starstream.qnt"),
        }
    }

    #[must_use]
    pub fn with_spec(mut self, spec: impl Into<PathBuf>) -> Self {
        self.spec = spec.into();
        self
    }

    /// Render a self-contained Quint test module that replays `trace`.
    pub fn render(&self, trace: &ExecutionTrace) -> Result<String, QuintError> {
        let Some((first, rest)) = trace.0.split_first() else {
            return Err(QuintError::EmptyTrace);
        };
        let spec_source =
            fs::read_to_string(&self.spec).map_err(|source| QuintError::ReadSpec {
                path: self.spec.clone(),
                source,
            })?;

        let ExecutionEvent::Init = first else {
            return Err(QuintError::MissingInit);
        };
        if let Some(index) = rest
            .iter()
            .position(|event| matches!(event, ExecutionEvent::Init))
        {
            return Err(QuintError::RepeatedInit { index: index + 1 });
        }

        let mut source = spec_source;
        if !source.ends_with('\n') {
            source.push('\n');
        }
        source.push('\n');
        writeln!(source, "module replay_trace {{").unwrap();
        writeln!(source, "  import starstream_execution.*").unwrap();
        writeln!(source).unwrap();
        write!(source, "  run execution_satisfies_spec = init").unwrap();
        for event in rest {
            write!(source, "\n    .then({})", render_event(event)).unwrap();
        }
        write!(source, "\n    .then(execution_complete)").unwrap();
        writeln!(source).unwrap();
        writeln!(source, "}}").unwrap();
        Ok(source)
    }

    /// Replay `trace` through Quint. Success means every concrete transition
    /// was enabled by the specification and the execution reached its terminal
    /// coordinator state.
    pub fn verify(&self, trace: &ExecutionTrace) -> Result<(), QuintError> {
        let generated_source = self.render(trace)?;
        let typecheck = self.run_quint(&generated_source, &["typecheck", "/dev/stdin"])?;
        if !typecheck.status.success() {
            return Err(QuintError::Typecheck(verification_failure(
                &generated_source,
                &typecheck,
            )));
        }

        let output = self.run_quint(
            &generated_source,
            &[
                "test",
                "/dev/stdin",
                "--main=replay_trace",
                "--match=execution_satisfies_spec",
                "--verbosity=3",
                "--backend=rust",
            ],
        )?;
        if output.status.success() {
            Ok(())
        } else {
            Err(QuintError::Rejected(verification_failure(
                &generated_source,
                &output,
            )))
        }
    }

    fn run_quint(&self, generated_source: &str, args: &[&str]) -> Result<Output, QuintError> {
        let program = self.quint.to_string_lossy().into_owned();
        let mut child = Command::new(&self.quint)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|source| QuintError::Invoke {
                program: program.clone(),
                source,
            })?;

        let write_result = child
            .stdin
            .take()
            .expect("piped child stdin must be available")
            .write_all(generated_source.as_bytes());
        if let Err(source) = write_result {
            let _ = child.kill();
            let _ = child.wait();
            return Err(QuintError::WriteInput { program, source });
        }

        child
            .wait_with_output()
            .map_err(|source| QuintError::Invoke { program, source })
    }
}

fn render_event(event: &ExecutionEvent) -> String {
    match event {
        ExecutionEvent::Init => unreachable!("render validates that Init appears only once"),
        ExecutionEvent::BeginNewUtxo { arguments } => {
            format!("begin_new_utxo({})", qnt_value(&arguments.0))
        }
        ExecutionEvent::NewUtxoReturn { resource } => {
            format!("new_utxo_return({})", resource.0)
        }
        ExecutionEvent::ClearAbi => "abis_clear".to_owned(),
        ExecutionEvent::AdvertiseMethod { method } => {
            format!("advertise_method({})", qnt_string(&method.to_hex()))
        }
        ExecutionEvent::ReturnControl => "return_control".to_owned(),
        ExecutionEvent::CallMethod {
            resource,
            method,
            arguments,
        } => {
            format!(
                "call_method({}, {}, {})",
                resource.0,
                qnt_string(&method.to_hex()),
                qnt_value(&arguments.0)
            )
        }
        ExecutionEvent::CoroutineReturn => "coroutine_return".to_owned(),
        ExecutionEvent::CoordReturn => "coord_return".to_owned(),
    }
}

fn verification_failure(generated_source: &str, output: &Output) -> VerificationFailure {
    VerificationFailure {
        generated_source: generated_source.to_owned(),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn qnt_string(value: &str) -> String {
    serde_json::to_string(value).expect("serializing a Rust string cannot fail")
}

fn qnt_value(value: &[u32]) -> String {
    let bytes = value
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("List({bytes})")
}
