use crate::optional::OptionalF;
use ark_ff::PrimeField;

pub const REF_PUSH_BATCH_SIZE: usize = 4;
pub const REF_GET_BATCH_SIZE: usize = 4;
pub const REF_WRITE_BATCH_SIZE: usize = 4;

#[derive(Debug, Clone)]
pub enum LedgerOperation<F: PrimeField> {
    /// A call to starstream_resume.
    ///
    /// This stores the input and outputs in memory, and sets the
    /// current_program for the next iteration to `utxo_id`.
    ///
    /// Then, when evaluating Yield and YieldResume, we match the input/output
    /// with the corresponding value.
    Resume {
        target: F,
        val: F,
        ret: F,
        caller: OptionalF<F>,
    },
    /// Called by utxo to yield.
    ///
    Yield {
        val: F,
    },
    Return {},
    ProgramHash {
        target: F,
        program_hash: [F; 4],
    },
    NewUtxo {
        program_hash: [F; 4],
        val: F,
        target: F,
    },
    NewCoord {
        program_hash: [F; 4],
        val: F,
        target: F,
    },
    Burn {
        ret: F,
    },
    Activation {
        val: F,
        caller: F,
    },
    Init {
        val: F,
        caller: F,
    },
    Bind {
        owner_id: F,
    },
    Unbind {
        token_id: F,
    },

    NewRef {
        size: F,
        ret: F,
    },
    RefPush {
        vals: [F; REF_PUSH_BATCH_SIZE],
    },
    RefGet {
        reff: F,
        offset: F,
        ret: [F; REF_GET_BATCH_SIZE],
    },
    RefWrite {
        reff: F,
        offset: F,
        vals: [F; REF_WRITE_BATCH_SIZE],
    },
    InstallHandler {
        interface_id: F,
    },
    UninstallHandler {
        interface_id: F,
    },
    GetHandlerFor {
        interface_id: F,
        handler_id: F,
    },
    /// Auxiliary instructions.
    ///
    /// Nop is used as a dummy instruction to build the circuit layout on the
    /// verifier side.
    Nop {},
}
