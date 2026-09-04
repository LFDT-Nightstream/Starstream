use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, OnceLock};

use tempfile::TempDir;

use crate::trace::{MethodHash, Out, ResourceHandle, StarstreamValue, Step, Trace};

const SPEC_MODULE: &str = "starstream";
const MODULE_NAME: &str = "replay_trace";
const RUN_NAME: &str = "execution_satisfies_spec";

/// `npm install` writes a `.cmd` shim on Windows, which `Command` cannot
/// execute under its extensionless name.
const QUINT_COMMAND: &str = if cfg!(windows) { "quint.cmd" } else { "quint" };

#[derive(Clone, Debug)]
pub struct QuintVerifier {
    quint: OsString,
    staged_spec: Arc<TempDir>,
    next_module: Arc<AtomicU64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VerificationFailure {
    pub generated_source: String,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, thiserror::Error)]
pub enum QuintError {
    #[error("failed to stage the Quint specification from {from} into {into}: {source}")]
    StageSpec {
        from: PathBuf,
        into: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error("failed to write the generated Quint module to {path}: {source}")]
    WriteModule {
        path: PathBuf,
        #[source]
        source: io::Error,
    },

    #[error(
        "failed to invoke Quint executable `{program}`: {source}\nrun `npm install` in this crate to provide it"
    )]
    Invoke {
        program: String,
        #[source]
        source: io::Error,
    },

    #[error("the generated Quint module failed to typecheck\n{0}")]
    Typecheck(Box<VerificationFailure>),

    #[error("Quint rejected step {index} of the execution trace: {step:?}\n{failure}")]
    RejectedStep {
        index: usize,
        step: Box<Step>,
        failure: Box<VerificationFailure>,
    },

    #[error("the execution trace did not reach a complete state\n{0}")]
    IncompleteExecution(Box<VerificationFailure>),

    #[error("Quint rejected the execution trace\n{0}")]
    Rejected(Box<VerificationFailure>),

    #[error("Quint did not run `{RUN_NAME}`\n{0}")]
    NotRun(Box<VerificationFailure>),

    #[error(
        "`{command}` is Quint {found}, but package.json pins {pinned}; run `npm ci` in this crate"
    )]
    VersionMismatch {
        command: String,
        found: String,
        pinned: &'static str,
    },

    #[error(
        "no runnable Quint at `{command}`, which package.json pins at {pinned}; run `npm ci` in this crate"
    )]
    NoQuint {
        command: String,
        pinned: &'static str,
    },
}

impl fmt::Display for VerificationFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "--- generated module ---")?;
        write!(f, "{}", self.generated_source)?;

        for (label, stream) in [("stdout", &self.stdout), ("stderr", &self.stderr)] {
            if !stream.trim().is_empty() {
                writeln!(f, "--- quint {label} ---")?;
                write!(f, "{stream}")?;
            }
        }

        Ok(())
    }
}

impl QuintVerifier {
    /// Returns the local Quint executable, falling back to `PATH`.
    #[must_use]
    pub fn resolved_command() -> OsString {
        let npm_local = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("node_modules")
            .join(".bin")
            .join(QUINT_COMMAND);

        if npm_local.is_file() {
            npm_local.into_os_string()
        } else {
            OsString::from(QUINT_COMMAND)
        }
    }

    /// Stages the specification after verifying the resolved Quint version.
    pub fn new() -> Result<Self, QuintError> {
        let command = Self::resolved_command();
        check_pinned_version(&command)?;

        Self::with_command(command)
    }

    /// Stages the specification with `quint`, without checking its version.
    pub fn with_command(quint: impl AsRef<OsStr>) -> Result<Self, QuintError> {
        let spec_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("spec");

        let staged_spec = tempfile::tempdir().map_err(|source| QuintError::StageSpec {
            from: spec_dir.clone(),
            into: std::env::temp_dir(),
            source,
        })?;

        copy_files(&spec_dir, staged_spec.path()).map_err(|source| QuintError::StageSpec {
            from: spec_dir,
            into: staged_spec.path().to_owned(),
            source,
        })?;

        Ok(Self {
            quint: quint.as_ref().to_owned(),
            staged_spec: Arc::new(staged_spec),
            next_module: Arc::new(AtomicU64::new(0)),
        })
    }

    /// Replay `trace` through Quint. Success means every concrete transition
    /// was enabled by the specification and the execution reached its terminal
    /// coordinator state.
    pub fn verify(&self, trace: &Trace) -> Result<(), QuintError> {
        let module = render(trace);
        let id = self.next_module.fetch_add(1, Ordering::Relaxed);

        let module_path = self.staged_spec.path().join(format!("replay_{id}.qnt"));
        fs::write(&module_path, &module.source).map_err(|source| QuintError::WriteModule {
            path: module_path.clone(),
            source,
        })?;

        let module_arg = module_path.display().to_string();

        let typecheck = self.run_quint(&["typecheck", &module_arg])?;
        if !typecheck.status.success() {
            return Err(QuintError::Typecheck(module.failure(&typecheck)));
        }

        let itf_prefix = format!("itf_{id}_");
        let itf_pattern = self
            .staged_spec
            .path()
            .join(format!("{itf_prefix}{{test}}_{{seq}}.json"));

        let test = self.run_quint(&[
            "test",
            &module_arg,
            &format!("--main={MODULE_NAME}"),
            &format!("--match={RUN_NAME}"),
            &format!("--out-itf={}", itf_pattern.display()),
            "--verbosity=3",
            "--backend=typescript",
        ])?;

        if !test.status.success() {
            let failure = module.failure(&test);

            return Err(match diagnose(&failure, &module) {
                Some(Rejection::Step(index)) => QuintError::RejectedStep {
                    index,
                    step: Box::new(trace.0[index].clone()),
                    failure,
                },
                Some(Rejection::Incomplete) => QuintError::IncompleteExecution(failure),
                None => QuintError::Rejected(failure),
            });
        }

        if !self.ran_any_test(&itf_prefix) {
            return Err(QuintError::NotRun(module.failure(&test)));
        }

        Ok(())
    }

    fn ran_any_test(&self, itf_prefix: &str) -> bool {
        let Ok(entries) = fs::read_dir(self.staged_spec.path()) else {
            return false;
        };

        entries
            .flatten()
            .any(|entry| entry.file_name().to_string_lossy().starts_with(itf_prefix))
    }

    fn run_quint(&self, args: &[&str]) -> Result<Output, QuintError> {
        let program = self.quint.to_string_lossy().into_owned();

        Command::new(&self.quint)
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(|source| QuintError::Invoke { program, source })
    }
}

fn pinned_version() -> &'static str {
    static PINNED: LazyLock<String> = LazyLock::new(|| {
        let manifest: serde_json::Value =
            serde_json::from_str(include_str!("../package.json")).expect("package.json parses");

        manifest["devDependencies"]["@informalsystems/quint"]
            .as_str()
            .expect("package.json pins @informalsystems/quint")
            .to_owned()
    });

    &PINNED
}

fn installed_version(quint: &OsStr) -> Option<String> {
    let output = Command::new(quint)
        .arg("--version")
        .stdin(Stdio::null())
        .output()
        .ok()?;

    output
        .status
        .success()
        .then(|| String::from_utf8_lossy(&output.stdout).trim().to_owned())
}

// Cache only successful checks so installation can recover from a failure.
fn check_pinned_version(command: &OsStr) -> Result<(), QuintError> {
    static MATCHED: OnceLock<()> = OnceLock::new();

    if MATCHED.get().is_some() {
        return Ok(());
    }

    let pinned = pinned_version();

    match installed_version(command) {
        Some(found) if found == pinned => {
            let _ = MATCHED.set(());
            Ok(())
        }
        Some(found) => Err(QuintError::VersionMismatch {
            command: command.to_string_lossy().into_owned(),
            found,
            pinned,
        }),
        None => Err(QuintError::NoQuint {
            command: command.to_string_lossy().into_owned(),
            pinned,
        }),
    }
}

fn copy_files(source: &Path, destination: &Path) -> io::Result<()> {
    for entry in fs::read_dir(source)? {
        let entry = entry?;

        if entry.file_type()?.is_file() {
            fs::copy(entry.path(), destination.join(entry.file_name()))?;
        }
    }

    Ok(())
}

/// A Quint module replaying a trace, alongside the source lines each step was
/// rendered to, so that a rejection can be attributed back to a [`Step`].
struct RenderedModule {
    source: String,
    step_lines: Vec<usize>,
    complete_line: usize,
}

impl RenderedModule {
    fn failure(&self, output: &Output) -> Box<VerificationFailure> {
        Box::new(VerificationFailure {
            generated_source: self.source.clone(),
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        })
    }
}

fn render(trace: &Trace) -> RenderedModule {
    let mut lines = vec![
        format!("module {MODULE_NAME} {{"),
        format!("  import {SPEC_MODULE}.* from \"./{SPEC_MODULE}\""),
        String::new(),
        "  action init = { state' = new_tx }".to_owned(),
        String::new(),
        format!("  run {RUN_NAME} = init"),
    ];

    let mut step_lines = Vec::with_capacity(trace.0.len());

    for step in &trace.0 {
        step_lines.push(lines.len() + 1);
        lines.push(format!("    .then({})", Qnt(step)));
    }

    let complete_line = lines.len() + 1;
    lines.push("    .then(execution_complete)".to_owned());
    lines.push("}".to_owned());

    RenderedModule {
        source: lines.join("\n") + "\n",
        step_lines,
        complete_line,
    }
}

enum Rejection {
    Step(usize),
    Incomplete,
}

/// Attribute a Quint test failure to a step of the replayed trace.
///
/// `QNT513` points at the `.then(..)` whose action was not enabled, while
/// `QNT511` means the chain ran to the end and only `execution_complete` was
/// left unsatisfied.
fn diagnose(failure: &VerificationFailure, module: &RenderedModule) -> Option<Rejection> {
    let mut lines = failure
        .stdout
        .lines()
        .chain(failure.stderr.lines())
        .skip_while(|line| !line.contains("Error [QNT51"));

    let code = lines.next()?;

    if code.contains("Error [QNT511]") {
        return Some(Rejection::Incomplete);
    }

    let location = lines
        .take(4)
        .find_map(|line| source_line(line.trim().strip_prefix("at ")?))?;

    if location == module.complete_line {
        return Some(Rejection::Incomplete);
    }

    module
        .step_lines
        .iter()
        .position(|line| *line == location)
        .map(Rejection::Step)
}

fn source_line(location: &str) -> Option<usize> {
    let mut fields = location.rsplit(':');
    let _column = fields.next()?;

    fields.next()?.parse().ok()
}

/// Renders `T` as the Quint literal the specification expects.
struct Qnt<T>(T);

impl fmt::Display for Qnt<&StarstreamValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("List(")?;

        for (index, limb) in self.0.0.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{limb}")?;
        }

        f.write_str(")")
    }
}

impl fmt::Display for Qnt<&ResourceHandle> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.0)
    }
}

impl fmt::Display for Qnt<&MethodHash> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0.to_hex())
    }
}

impl<T> fmt::Display for Qnt<&Out<T>>
where
    for<'a> Qnt<&'a T>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ value: {} }}", Qnt(&self.0.0))
    }
}

impl fmt::Display for Qnt<&Step> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Step::NewUtxo {
                arguments,
                resource,
            } => write!(f, "new_utxo({}, {})", Qnt(arguments), Qnt(resource)),
            Step::EnterConstructor { arguments } => {
                write!(f, "enter_constructor({})", Qnt(arguments))
            }
            Step::YieldBegin => f.write_str("yield_begin"),
            Step::RegisterMethod { method } => write!(f, "register_method({})", Qnt(method)),
            Step::Return { result } => write!(f, "return({})", Qnt(result)),
            Step::CallMethod {
                resource,
                method,
                arguments,
                result,
            } => write!(
                f,
                "call_method({}, {}, {}, {})",
                Qnt(resource),
                Qnt(method),
                Qnt(arguments),
                Qnt(result)
            ),
            Step::EnterMethod { method, arguments } => {
                write!(f, "enter_method({}, {})", Qnt(method), Qnt(arguments))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use Step::*;

    use super::*;
    use crate::trace::{MethodHash, ResourceHandle, StarstreamValue};

    // Middleware smoke tests: one accepted trace and one rejected trace ensure
    // the Rust-to-Quint translation, CLI invocation, and error mapping work.
    #[test]
    #[ignore = "requires Quint; run `npm test` in starstream-interleaving-spec"]
    fn replays_a_utxo_constructor() {
        let trace = Trace::new([
            NewUtxo {
                arguments: vec![0, 1, 2, 3].into(),
                resource: ResourceHandle(0).into(),
            },
            EnterConstructor {
                arguments: vec![0, 1, 2, 3].into(),
            },
            YieldBegin,
            RegisterMethod {
                method: MethodHash([1, 1, 1, 1]),
            },
            Return {
                result: StarstreamValue::from(vec![]).into(),
            },
            Return {
                result: StarstreamValue::from(vec![]).into(),
            },
        ]);

        QuintVerifier::new()
            .unwrap()
            .verify(&trace)
            .unwrap_or_else(|error| panic!("{error}"));
    }

    #[test]
    #[ignore = "requires Quint; run `npm test` in starstream-interleaving-spec"]
    fn rejects_a_return_without_entering_constructor() {
        let trace = Trace::new([
            NewUtxo {
                arguments: vec![0, 1, 2, 3].into(),
                resource: ResourceHandle(0).into(),
            },
            Return {
                result: StarstreamValue::from(vec![]).into(),
            },
        ]);

        let error = QuintVerifier::new().unwrap().verify(&trace).unwrap_err();

        assert!(
            matches!(error, QuintError::RejectedStep { index: 1, .. }),
            "{error}"
        );
    }
}
