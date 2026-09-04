use serde::{Deserialize, Serialize};

/// An arbitrary value.
///
/// The interleaving proof only cares about equality for these, as the receiver
/// and the sender need to agree, but it doesn't have any direct effect on the
/// control flow.
///
/// Currently we are using the Goldilocks prime field for proving, which can't
/// fit 64bits, so values bigger than 32bits get split into limbs.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StarstreamValue(pub Vec<u32>);

/// Models a WASM Component Model resource:
///
/// See: https://component-model.bytecodealliance.org/design/wit.html#resources
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ResourceHandle(pub u32);

/// SHA-256 method identity as four little-endian `u64` limbs, matching the
/// current `starstream-to-wasm`/WIT ABI.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct MethodHash(pub [u64; 4]);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Out<T>(pub T);

impl<T> From<T> for Out<T> {
    fn from(value: T) -> Self {
        Out(value)
    }
}

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

impl From<Vec<u32>> for StarstreamValue {
    fn from(value: Vec<u32>) -> Self {
        Self(value)
    }
}

/// A single observable transition of an execution, as attributed to the
/// coroutine that produced it.
///
/// The initial transition is not part of this enum: every [`Trace`] starts from
/// the specification's initial state by construction.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum Step {
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

impl Trace {
    pub fn new(steps: impl IntoIterator<Item = Step>) -> Self {
        Self(steps.into_iter().collect())
    }
}
