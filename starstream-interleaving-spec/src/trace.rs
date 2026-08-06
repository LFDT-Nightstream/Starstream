use serde::{Deserialize, Serialize};

/// Projector-local process identity.
///
/// Process IDs are allocated while the per-instance traces are scheduled.
/// They are not guest-visible values and are not committed host-call payload.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ProcessId(pub u64);

/// Guest-visible Component Model resource-table index.
///
/// Unlike [`ProcessId`], this identity is local to the component instance that
/// owns the handle and may be reused after the resource is dropped.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ResourceHandle(pub u32);

/// Canonical field-element encoding of a Starstream value or argument tuple.
///
/// The semantic model observes the value itself. Transcript commitments,
/// digests, and shared-memory transports are separate representations. The
/// current encoding uses 32-bit limbs as described by `WASM.md`; it is
/// intentionally not self-describing, since the function type supplies its
/// shape.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StarstreamValue(pub Vec<u32>);

/// SHA-256 method identity as four little-endian `u64` limbs, matching the
/// current `starstream-to-wasm`/WIT ABI.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct MethodHash(pub [u64; 4]);

impl MethodHash {
    /// Stable textual form used by the Quint model.
    #[must_use]
    pub fn to_hex(self) -> String {
        self.0
            .into_iter()
            .map(|limb| format!("{limb:016x}"))
            .collect()
    }
}

/// Control-relevant events projected from component execution.
///
/// Application `emit` events are intentionally absent: they do not influence
/// control ownership, coroutine state, or ledger validity. Environment reads
/// such as slot/block height will belong to a separate environment/ledger
/// model.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum ExecutionEvent {
    /// Start executing the transaction entrypoint coordination script.
    ///
    /// The transaction/proof context binds this execution to a concrete
    /// program and on-ledger identity; neither is part of the control trace.
    Init,

    /// A coordinator atomically called an imported UTXO constructor.
    ///
    /// Neo-Wasm observes the import arguments and result together. The
    /// resource is the caller-local `own<utxo>` handle returned by the
    /// Component Model lowering. The scheduler keeps that handle pending while
    /// the constructed UTXO runs and binds it when the UTXO returns control
    /// from its initial yield.
    NewUtxo {
        arguments: StarstreamValue,
        resource: ResourceHandle,
    },

    /// The running UTXO reached a yield point and began a fresh ABI epoch.
    /// The protocol emits this even when the new ABI has no methods.
    ClearAbi,

    /// The running UTXO advertised a method at its current yield point.
    AdvertiseMethod { method: MethodHash },

    /// The current UTXO export returned control to its caller.
    ///
    /// `ClearAbi` determines whether this closes a fresh yield point or a
    /// method handler returned while preserving the previous one. Method
    /// calls bind this value to the result observed atomically by the caller;
    /// constructor returns use an empty semantic result because the callee's
    /// internal resource representation is not the caller-local handle.
    ReturnControl { result: StarstreamValue },

    /// A running coordinator called a method advertised by a yielded UTXO.
    CallMethod {
        resource: ResourceHandle,
        method: MethodHash,
        /// User arguments observed at the atomic import boundary. The next
        /// event in the selected UTXO turn must be an [`Self::EnterMethod`]
        /// carrying the same value.
        arguments: StarstreamValue,
        /// Result observed atomically at the imported call site. The
        /// interleaving model retains it while the callee executes and checks
        /// it against the callee's `ReturnControl` value.
        result: StarstreamValue,
    },

    /// The selected UTXO method export began executing with these user
    /// arguments. The callee's internal resource receiver is deliberately
    /// absent: it is bootstrap advice local to that component instance, not
    /// the caller-local resource handle carried by [`Self::CallMethod`].
    EnterMethod { arguments: StarstreamValue },

    /// The running coroutine called the provisional terminal `return`/`burn`
    /// host operation instead of yielding again.
    ///
    /// The precise ledger meaning is deliberately deferred. For now, the
    /// important property is that coroutine return is an explicit
    /// transcript-visible call rather than something inferred from a
    /// particular Wasm return PC.
    CoroutineReturn,

    /// The transaction entrypoint coordination script returned.
    CoordReturn,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ExecutionTrace(pub Vec<ExecutionEvent>);

impl ExecutionTrace {
    #[must_use]
    pub fn new(events: impl IntoIterator<Item = ExecutionEvent>) -> Self {
        Self(events.into_iter().collect())
    }

    pub fn push(&mut self, event: ExecutionEvent) {
        self.0.push(event);
    }

    pub fn iter(&self) -> impl Iterator<Item = &ExecutionEvent> {
        self.0.iter()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Minimal sink implemented by trace collectors at Wasmtime/runtime hooks.
pub trait TraceSink {
    fn record(&mut self, event: ExecutionEvent);
}

/// In-memory recorder useful for runtime integration and tests.
#[derive(Clone, Debug, Default)]
pub struct TraceRecorder {
    trace: ExecutionTrace,
}

impl TraceRecorder {
    #[must_use]
    pub fn trace(&self) -> &ExecutionTrace {
        &self.trace
    }

    #[must_use]
    pub fn into_trace(self) -> ExecutionTrace {
        self.trace
    }
}

impl TraceSink for TraceRecorder {
    fn record(&mut self, event: ExecutionEvent) {
        self.trace.push(event);
    }
}
