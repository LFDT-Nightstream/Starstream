//! Executable semantics and trace validation for Starstream execution.
//!
//! This crate deliberately contains no proof-circuit code. It defines the
//! prover-facing semantic trace, records those events independently of their
//! Wasmtime source, and replays concrete traces against the Quint
//! specification in `spec/starstream.qnt`.

pub mod nightstream;

mod interleave;
mod quint;
mod trace;

pub use interleave::{InterleavingError, interleave_traces};
pub use quint::{QuintError, QuintVerifier, VerificationFailure};
pub use trace::{
    ExecutionEvent, ExecutionTrace, MethodHash, ProcessId, ResourceHandle, StarstreamValue,
    TraceRecorder, TraceSink,
};
