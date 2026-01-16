use crate::{
    Hash, Ref, Value, WasmModule,
    transaction_effects::{InterfaceId, ProcessId},
};

// Both used to indicate which fields are outputs, and to have a placeholder
// value for the runtime executor (trace generator)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum WitEffectOutput<T> {
    Resolved(T),
    Thunk,
}

/// One entry in the per-process trace.
//
// Note that since these are witnesses, they include the inputs and the outputs
// for each operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WitLedgerEffect {
    Resume {
        // in
        target: ProcessId,
        val: Ref,
        // out
        ret: WitEffectOutput<Ref>,
        id_prev: WitEffectOutput<Option<ProcessId>>,
    },
    Yield {
        // in
        val: Ref,
        // out
        ret: WitEffectOutput<Ref>,
        id_prev: WitEffectOutput<Option<ProcessId>>,
    },
    ProgramHash {
        // in
        target: ProcessId,
        // out
        program_hash: WitEffectOutput<Hash<WasmModule>>,
    },
    NewUtxo {
        // in
        program_hash: Hash<WasmModule>,
        val: Ref,
        // out
        id: WitEffectOutput<ProcessId>,
    },
    NewCoord {
        // int
        program_hash: Hash<WasmModule>,
        val: Ref,

        // out
        id: WitEffectOutput<ProcessId>,
    },
    // Scoped handlers for custom effects
    //
    // coord only (mainly because utxos can't resume utxos anyway)
    InstallHandler {
        // in
        interface_id: InterfaceId,
    },
    UninstallHandler {
        // in
        interface_id: InterfaceId,
        // out
        //
        // does not return anything
    },
    GetHandlerFor {
        // in
        interface_id: InterfaceId,
        // out
        handler_id: WitEffectOutput<ProcessId>,
    },

    // UTXO-only
    Burn {
        // out
        ret: WitEffectOutput<Ref>,
    },

    Activation {
        // in
        val: Ref,
        // out
        caller: WitEffectOutput<ProcessId>,
    },

    Init {
        // in
        val: Ref,
        // out
        caller: WitEffectOutput<ProcessId>,
    },

    NewRef {
        // in
        size: usize,
        // out
        ret: WitEffectOutput<Ref>,
    },
    RefPush {
        // in
        val: Value,
        // out
        // does not return anything
    },
    Get {
        // in
        reff: Ref,
        offset: usize,

        // out
        ret: WitEffectOutput<Value>,
    },

    // Tokens
    Bind {
        owner_id: ProcessId,
        // does not return anything
    },
    Unbind {
        token_id: ProcessId,
        // does not return anything
    },
}

impl<T> WitEffectOutput<T> {
    pub fn unwrap(self) -> T {
        match self {
            WitEffectOutput::Resolved(v) => v,
            WitEffectOutput::Thunk => panic!("Called unwrap on a Thunk"),
        }
    }

    pub fn is_resolved(&self) -> bool {
        matches!(self, WitEffectOutput::Resolved(_))
    }

    pub fn to_option(self) -> Option<T> {
        match self {
            WitEffectOutput::Resolved(t) => Some(t),
            WitEffectOutput::Thunk => None,
        }
    }
}

impl<T> From<T> for WitEffectOutput<T> {
    fn from(value: T) -> WitEffectOutput<T> {
        WitEffectOutput::Resolved(value)
    }
}

impl<T> From<Option<T>> for WitEffectOutput<T> {
    fn from(value: Option<T>) -> WitEffectOutput<T> {
        match value {
            Some(t) => WitEffectOutput::Resolved(t),
            None => WitEffectOutput::Thunk,
        }
    }
}
