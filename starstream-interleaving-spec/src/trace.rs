use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::METHOD_WORDS;

/// An arbitrary value.
///
/// The interleaving proof only cares about equality for these, as the receiver
/// and the sender need to agree, but it doesn't have any direct effect on the
/// control flow.
///
/// Four canonical Goldilocks words of an opaque object root. Payload hashing
/// (including schema and length) belongs to the program proof, not this model.
/// Unit uses the direct constant encoding [`Self::UNIT_VALUE`] instead.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StarstreamValue(pub [u64; 4]);

impl StarstreamValue {
    /// Unit (no arguments/result), encoded directly as four zero words.
    /// This is a protocol convention, not the hash of an empty opaque object.
    pub const UNIT_VALUE: Self = Self([0; 4]);

    pub fn words32(&self) -> [u32; 8] {
        std::array::from_fn(|i| (self.0[i / 2] >> (32 * (i % 2))) as u32)
    }
}

/// Models a WASM Component Model resource:
///
/// See: https://component-model.bytecodealliance.org/design/wit.html#resources
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ResourceHandle(pub u32);

/// SHA-256 event identity as eight little-endian `u32` limbs, in event/RAM
/// order. Each pair is the low then high half of a `starstream-to-wasm`/WIT
/// `u64` limb.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct EventIdentity(pub [u32; METHOD_WORDS]);

/// Selector registered by a UTXO for method dispatch.
pub use EventIdentity as MethodSelector;
/// Contract-bound identity of a UTXO constructor.
pub use EventIdentity as ConstructorIdentity;
/// Contract-bound identity of a coordination script.
pub use EventIdentity as ScriptIdentity;
pub use EventIdentity as EffectIdentity;

pub use EventIdentity as MethodHash;
pub use EventIdentity as EffectHash;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Out<T>(pub T);

impl<T> From<T> for Out<T> {
    fn from(value: T) -> Self {
        Out(value)
    }
}

impl EventIdentity {
    /// Split the four WIT words into low/high `u32` limb pairs.
    pub fn from_u64_words(words: [u64; 4]) -> Self {
        Self(std::array::from_fn(|i| {
            (words[i / 2] >> (32 * (i % 2))) as u32
        }))
    }

    /// Stable textual form used by the Quint model. Preserve the original
    /// four-u64 formatting: high half then low half within each pair.
    #[must_use]
    pub fn to_hex(self) -> String {
        self.0
            .as_chunks::<2>()
            .0
            .iter()
            .map(|pair| format!("{:08x}{:08x}", pair[1], pair[0]))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::MethodHash;

    #[test]
    fn method_hash_splits_words_low_half_first() {
        assert_eq!(
            MethodHash::from_u64_words(
                [0x0123_4567_89ab_cdef, 0xfedc_ba98_7654_3210, 0, u64::MAX,]
            ),
            MethodHash([
                0x89ab_cdef,
                0x0123_4567,
                0x7654_3210,
                0xfedc_ba98,
                0,
                0,
                u32::MAX,
                u32::MAX,
            ])
        );
    }
}

impl From<Vec<u32>> for StarstreamValue {
    fn from(value: Vec<u32>) -> Self {
        // Convenience for symbolic roots in fixtures, not payload hashing.
        assert!(value.len() <= 4, "an opaque root has four words");
        let mut root = [0; 4];
        for (out, word) in root.iter_mut().zip(value) {
            *out = u64::from(word);
        }
        Self(root)
    }
}

/// The resource whose storage is restored by a set-storage boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StorageKind {
    Utxo,
    /// Index of the token interface in this component's sorted token exports.
    /// Binds callbacks to the type restored by this setter.
    Token {
        interface: u32,
    },
}

/// A single observable transition of an execution, as attributed to the
/// coroutine that produced it.
///
/// Execution-only replay starts in `new_tx`; transaction replay starts in
/// `new_transaction` and includes loading/finalization steps. Both open with
/// `BeginExecution` once any inputs are loaded.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Step {
    AssertSignedBy {
        key: u64,
    },
    SetStorage {
        kind: StorageKind,
        storage: StarstreamValue,
        /// Host-supplied Coord(1)-local binding, not a wasm return value.
        /// Excluded from both the program event and transaction IO commitments.
        entry_handle: ResourceHandle,
    },
    /// Ledger-supplied ABI entry for the most recently loaded UTXO; no program event.
    PreloadMethod {
        method: MethodSelector,
    },
    GetStorage {
        storage: Out<StarstreamValue>,
    },
    GetTokenStorage {
        interface: u32,
        storage: Out<StarstreamValue>,
    },
    /// Host-synthesized enumeration of the last exported UTXO's ABI; no program event.
    ReadAbi {
        method: MethodSelector,
    },
    /// Advance the finalization scan without exporting a consumed UTXO.
    SkipConsumed,
    /// Enumerate the next coordinator's final trace root after all UTXOs and tokens; no program event.
    FinalizeCoordinator,
    FinishTransaction,
    /// Host-synthesized call from the transaction context into the entry
    /// script, after loading; no program event. Bound by the statement's
    /// [`EntryCall`] and matched by the entry coordinator's `EnterCall`.
    BeginExecution {
        script: ScriptIdentity,
        arguments: StarstreamValue,
    },
    /// Imported script call. Identity binds contract, export name, and scalar ABI.
    /// The fresh callee opens with a matching `EnterCall`.
    CallCoordinator {
        script: ScriptIdentity,
        arguments: StarstreamValue,
    },
    /// Constructor call. Identity binds the contract and its UTXO export.
    /// The new UTXO opens with a matching `EnterConstructor`.
    NewUtxo {
        constructor: ConstructorIdentity,
        arguments: StarstreamValue,
    },
    ReceiveNewUtxoHandle {
        resource: Out<ResourceHandle>,
    },
    EnterConstructor {
        constructor: ConstructorIdentity,
        arguments: StarstreamValue,
    },
    YieldBegin,
    RegisterMethod {
        method: MethodSelector,
    },
    Return {
        result: Out<StarstreamValue>,
    },
    /// A typed handle is upcast to a fresh builtin handle for the same resource.
    Upcast {
        source: ResourceHandle,
        result: Out<ResourceHandle>,
    },
    /// Caller-local handles passed to the builtin attach import.
    Attach {
        token: ResourceHandle,
        destination: ResourceHandle,
    },
    /// The destination handle as lowered in the token callback's instance.
    EnterAttach {
        interface: u32,
        destination: ResourceHandle,
    },
    CallMethod {
        resource: ResourceHandle,
        method: MethodSelector,
        arguments: StarstreamValue,
    },
    /// Receive the completed top frame's result, from a UTXO method or an
    /// imported script alike; the Wasm proof pairs this return with its
    /// suspended import.
    ReceiveCallResult {
        result: Out<StarstreamValue>,
    },
    /// Entry into a called export: a UTXO method (method hash) or a
    /// coordination script (script identity), including the entry script.
    EnterCall {
        method: EventIdentity,
        arguments: StarstreamValue,
    },
    /// Begin a dynamically handled effect call. The result is committed when
    /// the import resumes, after one or more handler activations.
    RaiseEffect {
        effect: EffectIdentity,
        arguments: StarstreamValue,
    },
    /// Open a `try` scope owned by the current invocation. `frame_id`
    /// identifies the compiled handler code, not a unique dynamic scope.
    PushEffectHandlerFrame {
        frame_id: u32,
    },
    /// Register an effect on the innermost scope of the current invocation.
    AddEffectHandler {
        effect: EffectIdentity,
    },
    /// Close the innermost scope of the current invocation.
    PopEffectHandlerFrame,
    /// Host search step past a scope that does not register the pending
    /// effect; no program event. The effect is the pending raise's.
    SkipEffectHandler,
    EnterEffectHandler {
        frame_id: u32,
        effect: EffectIdentity,
        arguments: StarstreamValue,
    },
    ExitEffectHandler {
        handled: bool,
        result: Out<StarstreamValue>,
    },
    /// Receive the top effect frame's result. The Wasm proof pairs the return
    /// with its suspended import, including across same-instance reentry.
    ResumeEffect {
        result: Out<StarstreamValue>,
    },
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Trace(pub Vec<Step>);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputUtxo {
    pub storage: StarstreamValue,
    /// Sorted, deduplicated ABI methods, preloaded in this order.
    pub methods: Vec<MethodSelector>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputUtxo {
    pub utxo: u32,
    pub storage: StarstreamValue,
    /// Final-generation registration sequence, including duplicates.
    pub methods: Vec<MethodSelector>,
}

/// A transaction-relevant token, numbered by position in the input token list.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputToken {
    /// Transaction-local ID of the UTXO holding this token.
    pub owner: u32,
    pub storage: StarstreamValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputToken {
    /// Transaction-local token ID, preserved across its input and output records.
    pub token: u32,
    pub owner: u32,
    pub storage: StarstreamValue,
}

/// The script the transaction runs, and its (opaque) argument root.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EntryCall {
    pub script: ScriptIdentity,
    pub arguments: StarstreamValue,
}

/// Inputs receive consecutive UTXO IDs; outputs are ordered by surviving ID.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransactionStatement {
    /// Only tokens relevant to this transaction appear here.
    pub input_tokens: Vec<InputToken>,
    /// Ordered by transaction-local token ID.
    pub output_tokens: Vec<OutputToken>,
    /// Public signer IDs whose signatures the verifier requires on this transaction.
    pub signers: BTreeSet<u64>,
    pub entry: EntryCall,
    pub inputs: Vec<InputUtxo>,
    pub outputs: Vec<OutputUtxo>,
}

impl Trace {
    pub fn new(steps: impl IntoIterator<Item = Step>) -> Self {
        Self(steps.into_iter().collect())
    }

    /// An interleaved execution of `body`, the entry coordinator's steps after
    /// its entry event: `BeginExecution` and the matching `EnterCall` first.
    pub fn with_entry(entry: &EntryCall, body: impl IntoIterator<Item = Step>) -> Self {
        Self(
            [
                Step::BeginExecution {
                    script: entry.script,
                    arguments: entry.arguments.clone(),
                },
                Step::EnterCall {
                    method: entry.script,
                    arguments: entry.arguments.clone(),
                },
            ]
            .into_iter()
            .chain(body)
            .collect(),
        )
    }
}
