use crate::{
    Hash, Ref, Value, WasmModule,
    transaction_effects::{InterfaceId, ProcessId},
};

// Discriminants for host calls
#[derive(Debug)]
pub enum EffectDiscriminant {
    Resume = 0,
    Yield = 1,
    NewUtxo = 2,
    NewCoord = 3,
    InstallHandler = 4,
    UninstallHandler = 5,
    GetHandlerFor = 6,
    Burn = 7,
    Activation = 8,
    Init = 9,
    NewRef = 10,
    RefPush = 11,
    RefGet = 12,
    Bind = 13,
    Unbind = 14,
    ProgramHash = 15,
    RefWrite = 16,
}

pub const REF_PUSH_WIDTH: usize = 7;
pub const REF_GET_WIDTH: usize = 5;
pub const REF_WRITE_WIDTH: usize = 4;

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
        // in
        ret: Ref,
    },

    Activation {
        // out
        val: WitEffectOutput<Ref>,
        caller: WitEffectOutput<ProcessId>,
    },

    Init {
        // out
        val: WitEffectOutput<Ref>,
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
        vals: [Value; REF_PUSH_WIDTH],
        // out
        // does not return anything
    },
    RefGet {
        // in
        reff: Ref,
        offset: usize,

        // out
        ret: WitEffectOutput<[Value; REF_GET_WIDTH]>,
    },
    RefWrite {
        // in
        reff: Ref,
        offset: usize,
        len: usize,
        vals: [Value; REF_WRITE_WIDTH],
        // out
        // does not return anything
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

impl From<u64> for EffectDiscriminant {
    fn from(value: u64) -> Self {
        match value {
            0 => EffectDiscriminant::Resume,
            1 => EffectDiscriminant::Yield,
            2 => EffectDiscriminant::NewUtxo,
            3 => EffectDiscriminant::NewCoord,
            4 => EffectDiscriminant::InstallHandler,
            5 => EffectDiscriminant::UninstallHandler,
            6 => EffectDiscriminant::GetHandlerFor,
            7 => EffectDiscriminant::Burn,
            8 => EffectDiscriminant::Activation,
            9 => EffectDiscriminant::Init,
            10 => EffectDiscriminant::NewRef,
            11 => EffectDiscriminant::RefPush,
            12 => EffectDiscriminant::RefGet,
            13 => EffectDiscriminant::Bind,
            14 => EffectDiscriminant::Unbind,
            15 => EffectDiscriminant::ProgramHash,
            16 => EffectDiscriminant::RefWrite,
            _ => todo!(),
        }
    }
}
