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
            return Ok(render_rejected_shape(
                &spec_source,
                "trace must start with Init",
            ));
        };

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
        writeln!(source).unwrap();
        writeln!(source, "}}").unwrap();
        Ok(source)
    }

    /// Replay `trace` through Quint. Success means every concrete transition
    /// was enabled by the specification.
    pub fn verify(&self, trace: &ExecutionTrace) -> Result<(), QuintError> {
        let generated_source = self.render(trace)?;
        let output = self.run_quint(&generated_source)?;
        if output.status.success() {
            Ok(())
        } else {
            Err(QuintError::Rejected(VerificationFailure {
                generated_source,
                stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            }))
        }
    }

    fn run_quint(&self, generated_source: &str) -> Result<Output, QuintError> {
        let program = self.quint.to_string_lossy().into_owned();
        let mut child = Command::new(&self.quint)
            .args([
                OsStr::new("test"),
                OsStr::new("/dev/stdin"),
                OsStr::new("--main=replay_trace"),
                OsStr::new("--match=execution_satisfies_spec"),
                OsStr::new("--verbosity=1"),
                OsStr::new("--backend=rust"),
            ])
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
        ExecutionEvent::Init => "false".to_owned(),
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

fn render_rejected_shape(spec_source: &str, reason: &str) -> String {
    format!(
        "{spec_source}\n\nmodule replay_trace {{\n  import starstream_execution.*\n  // {reason}\n  run execution_satisfies_spec = false\n}}\n"
    )
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
