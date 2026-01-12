use crate::{
    Hash, Ref, Value, WasmModule,
    transaction_effects::{InterfaceId, ProcessId},
};

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
        ret: Ref,
        id_prev: Option<ProcessId>,
    },
    Yield {
        // in
        val: Ref,
        // out
        ret: Option<Ref>,
        id_prev: Option<ProcessId>,
    },
    ProgramHash {
        // in
        target: ProcessId,
        // out
        program_hash: Hash<WasmModule>,
    },
    NewUtxo {
        // in
        program_hash: Hash<WasmModule>,
        val: Ref,
        // out
        id: ProcessId,
    },
    NewCoord {
        // int
        program_hash: Hash<WasmModule>,
        val: Ref,

        // out
        id: ProcessId,
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
        handler_id: ProcessId,
    },

    // UTXO-only
    Burn {
        // out
        ret: Ref,
    },

    Activation {
        // in
        val: Ref,
        // out
        caller: ProcessId,
    },

    Init {
        // in
        val: Ref,
        // out
        caller: ProcessId,
    },

    NewRef {
        // in
        size: usize,
        // out
        ret: Ref,
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
        ret: Value,
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
