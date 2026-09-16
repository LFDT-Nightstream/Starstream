use serde::{Deserialize, Serialize};

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

/// SHA-256 method identity as eight little-endian `u32` limbs, in event/RAM
/// order. Each pair is the low then high half of a `starstream-to-wasm`/WIT
/// `u64` limb.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct MethodHash(pub [u32; 8]);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Out<T>(pub T);

impl<T> From<T> for Out<T> {
    fn from(value: T) -> Self {
        Out(value)
    }
}

impl MethodHash {
    /// Stable textual form used by the Quint model. Preserve the original
    /// four-u64 formatting: high half then low half within each pair.
    #[must_use]
    pub fn to_hex(self) -> String {
        self.0
            .chunks_exact(2)
            .map(|pair| format!("{:08x}{:08x}", pair[1], pair[0]))
            .collect()
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

/// A single observable transition of an execution, as attributed to the
/// coroutine that produced it.
///
/// Execution-only replay starts in `new_tx`; transaction replay starts in
/// `new_transaction` and includes loading/finalization steps.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Step {
    SetStorage {
        storage: StarstreamValue,
        /// Witnessed coordinator-local binding, not part of transaction IO.
        resource: Out<ResourceHandle>,
    },
    /// Ledger-supplied ABI entry for the most recently loaded UTXO; no program event.
    PreloadMethod {
        method: MethodHash,
    },
    GetStorage {
        storage: Out<StarstreamValue>,
    },
    /// Host-synthesized enumeration of the last exported UTXO's ABI; no program event.
    ReadAbi {
        method: MethodHash,
    },
    /// Advance the finalization scan without exporting a consumed UTXO.
    SkipConsumed,
    /// Enumerate Coord(1)'s final trace root after all UTXOs; no program event.
    FinalizeCoordinator,
    FinishTransaction,
    NewUtxo {
        arguments: StarstreamValue,
        resource: Out<ResourceHandle>,
    },
    EnterConstructor {
        arguments: StarstreamValue,
    },
    YieldBegin,
    RegisterMethod {
        method: MethodHash,
    },
    Return {
        result: Out<StarstreamValue>,
    },
    CallMethod {
        resource: ResourceHandle,
        method: MethodHash,
        arguments: StarstreamValue,
        result: Out<StarstreamValue>,
    },
    EnterMethod {
        method: MethodHash,
        arguments: StarstreamValue,
    },
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Trace(pub Vec<Step>);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputUtxo {
    pub storage: StarstreamValue,
    /// Ordered initial preload sequence, including duplicates.
    pub methods: Vec<MethodHash>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OutputUtxo {
    pub utxo: u32,
    pub storage: StarstreamValue,
    /// Final-generation registration sequence, including duplicates.
    pub methods: Vec<MethodHash>,
}

/// Inputs receive consecutive UTXO IDs; outputs are ordered by surviving ID.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransactionStatement {
    pub inputs: Vec<InputUtxo>,
    pub outputs: Vec<OutputUtxo>,
}

impl Trace {
    pub fn new(steps: impl IntoIterator<Item = Step>) -> Self {
        Self(steps.into_iter().collect())
    }
}
