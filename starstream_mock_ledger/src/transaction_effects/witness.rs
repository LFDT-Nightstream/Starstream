use crate::{
    Hash, Value, WasmModule,
    transaction_effects::{InterfaceId, ProcessId},
};

/// One entry in the per-process trace.
//
// Note that since these are witnesses, they include the inputs and the outputs
// for each operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WitLedgerEffect {
    Resume {
        target: ProcessId,
        val: Value,
        ret: Value,
        id_prev: Option<ProcessId>,
    },
    Yield {
        val: Value,
        ret: Option<Value>,
        id_prev: Option<ProcessId>,
    },
    ProgramHash {
        target: ProcessId,
        program_hash: Hash<WasmModule>,
    },
    NewUtxo {
        program_hash: Hash<WasmModule>,
        val: Value,
        id: ProcessId,
    },
    NewCoord {
        program_hash: Hash<WasmModule>,
        val: Value,
        id: ProcessId,
    },
    // Scoped handlers for custom effects
    //
    // coord only (mainly because utxos can't resume utxos anyway)
    InstallHandler {
        interface_id: InterfaceId,
    },
    UninstallHandler {
        interface_id: InterfaceId,
    },
    GetHandlerFor {
        interface_id: InterfaceId,
        handler_id: ProcessId,
    },

    // UTXO-only
    Burn {
        ret: Value,
    },

    Activation {
        val: Value,
        caller: ProcessId,
    },

    Init {
        val: Value,
        caller: ProcessId,
    },

    // Tokens
    Bind {
        owner_id: ProcessId,
    },
    Unbind {
        token_id: ProcessId,
    },
}
