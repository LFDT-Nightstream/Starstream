use crate::{F, OptionalF, ledger_operation::LedgerOperation};
use ark_ff::Zero;
use starstream_interleaving_spec::{EffectDiscriminant, LedgerEffectsCommitment, WitLedgerEffect};

pub const OPCODE_ARG_COUNT: usize = 7;

pub fn commit(prev: LedgerEffectsCommitment, op: WitLedgerEffect) -> LedgerEffectsCommitment {
    let ledger_op = ledger_operation_from_wit(&op);
    let opcode_discriminant = opcode_discriminant(&ledger_op);
    let opcode_args = opcode_args(&ledger_op);

    let mut concat = [F::zero(); 12];
    concat[..4].copy_from_slice(&prev.0);
    concat[4] = opcode_discriminant;
    concat[5..].copy_from_slice(&opcode_args);

    let compressed =
        ark_poseidon2::compress_12_trace(&concat).expect("poseidon2 compress_12_trace");
    LedgerEffectsCommitment(compressed)
}

#[derive(Copy, Clone, Debug)]
pub enum ArgName {
    Target,
    Val,
    Ret,
    Caller,
    Offset,
    Size,
    ProgramHash0,
    ProgramHash1,
    ProgramHash2,
    ProgramHash3,
    ActivationCaller,
    OwnerId,
    TokenId,
    InterfaceId0,
    InterfaceId1,
    InterfaceId2,
    InterfaceId3,

    PackedRef0,
    PackedRef1,
    PackedRef2,
    PackedRef3,
    PackedRef4,
    PackedRef5,
}

impl ArgName {
    // maps argument names to positional indices
    //
    // these need to match the order in the ABI used by the wasm/program vm.
    pub const fn idx(self) -> usize {
        match self {
            ArgName::Target | ArgName::OwnerId | ArgName::TokenId => 0,
            ArgName::Val => 1,
            ArgName::Ret => 2,
            ArgName::Caller | ArgName::Offset | ArgName::Size | ArgName::ActivationCaller => 3,
            ArgName::InterfaceId0 => 3,
            ArgName::InterfaceId1 => 4,
            ArgName::InterfaceId2 => 5,
            ArgName::InterfaceId3 => 6,
            ArgName::ProgramHash0 => 3,
            ArgName::ProgramHash1 => 4,
            ArgName::ProgramHash2 => 5,
            ArgName::ProgramHash3 => 6,

            // Packed ref args for RefPush/RefGet/RefWrite.
            ArgName::PackedRef0 => 0,
            ArgName::PackedRef1 => 1,
            ArgName::PackedRef2 => 2,
            ArgName::PackedRef3 => 3,
            ArgName::PackedRef4 => 4,
            ArgName::PackedRef5 => 5,
        }
    }
}

pub(crate) fn ledger_operation_from_wit(op: &WitLedgerEffect) -> LedgerOperation<F> {
    match op {
        WitLedgerEffect::Resume {
            target,
            val,
            ret,
            caller,
        } => LedgerOperation::Resume {
            target: F::from(target.0 as u64),
            val: F::from(val.0),
            ret: ret.to_option().map(|r| F::from(r.0)).unwrap_or_default(),
            caller: OptionalF::from_option(
                caller.to_option().flatten().map(|p| F::from(p.0 as u64)),
            ),
        },
        WitLedgerEffect::Yield { val } => LedgerOperation::Yield {
            val: F::from(val.0),
        },
        WitLedgerEffect::Return {} => LedgerOperation::Return {},
        WitLedgerEffect::Burn { ret } => LedgerOperation::Burn {
            ret: F::from(ret.0),
        },
        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => LedgerOperation::ProgramHash {
            target: F::from(target.0 as u64),
            program_hash: program_hash.unwrap().0.map(F::from),
        },
        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => LedgerOperation::NewUtxo {
            program_hash: program_hash.0.map(F::from),
            val: F::from(val.0),
            target: F::from(id.unwrap().0 as u64),
        },
        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => LedgerOperation::NewCoord {
            program_hash: program_hash.0.map(F::from),
            val: F::from(val.0),
            target: F::from(id.unwrap().0 as u64),
        },
        WitLedgerEffect::Activation { val, caller } => LedgerOperation::Activation {
            val: F::from(val.unwrap().0),
            caller: F::from(caller.unwrap().0 as u64),
        },
        WitLedgerEffect::Init { val, caller } => LedgerOperation::Init {
            val: F::from(val.unwrap().0),
            caller: F::from(caller.unwrap().0 as u64),
        },
        WitLedgerEffect::Bind { owner_id } => LedgerOperation::Bind {
            owner_id: F::from(owner_id.0 as u64),
        },
        WitLedgerEffect::Unbind { token_id } => LedgerOperation::Unbind {
            token_id: F::from(token_id.0 as u64),
        },
        WitLedgerEffect::NewRef { size, ret } => LedgerOperation::NewRef {
            size: F::from(*size as u64),
            ret: F::from(ret.unwrap().0),
        },
        WitLedgerEffect::RefPush { vals } => LedgerOperation::RefPush {
            vals: vals.map(value_to_field),
        },
        WitLedgerEffect::RefGet { reff, offset, ret } => LedgerOperation::RefGet {
            reff: F::from(reff.0),
            offset: F::from(*offset as u64),
            ret: ret.unwrap().map(value_to_field),
        },
        WitLedgerEffect::RefWrite { reff, offset, vals } => LedgerOperation::RefWrite {
            reff: F::from(reff.0),
            offset: F::from(*offset as u64),
            vals: vals.map(value_to_field),
        },
        WitLedgerEffect::InstallHandler { interface_id } => LedgerOperation::InstallHandler {
            interface_id: interface_id.0.map(F::from),
        },
        WitLedgerEffect::UninstallHandler { interface_id } => LedgerOperation::UninstallHandler {
            interface_id: interface_id.0.map(F::from),
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id,
            handler_id,
        } => LedgerOperation::GetHandlerFor {
            interface_id: interface_id.0.map(F::from),
            handler_id: F::from(handler_id.unwrap().0 as u64),
        },
        WitLedgerEffect::CallEffectHandler {
            interface_id,
            val,
            ret,
            ..
        } => LedgerOperation::CallEffectHandler {
            interface_id: interface_id.0.map(F::from),
            val: F::from(val.0),
            ret: ret.to_option().map(|r| F::from(r.0)).unwrap_or_default(),
        },
    }
}

pub(crate) fn opcode_discriminant(op: &LedgerOperation<F>) -> F {
    match op {
        LedgerOperation::Nop {} => F::zero(),
        LedgerOperation::Resume { .. } => F::from(EffectDiscriminant::Resume as u64),
        LedgerOperation::CallEffectHandler { .. } => {
            F::from(EffectDiscriminant::CallEffectHandler as u64)
        }
        LedgerOperation::Yield { .. } => F::from(EffectDiscriminant::Yield as u64),
        LedgerOperation::Return { .. } => F::from(EffectDiscriminant::Return as u64),
        LedgerOperation::Burn { .. } => F::from(EffectDiscriminant::Burn as u64),
        LedgerOperation::ProgramHash { .. } => F::from(EffectDiscriminant::ProgramHash as u64),
        LedgerOperation::NewUtxo { .. } => F::from(EffectDiscriminant::NewUtxo as u64),
        LedgerOperation::NewCoord { .. } => F::from(EffectDiscriminant::NewCoord as u64),
        LedgerOperation::Activation { .. } => F::from(EffectDiscriminant::Activation as u64),
        LedgerOperation::Init { .. } => F::from(EffectDiscriminant::Init as u64),
        LedgerOperation::Bind { .. } => F::from(EffectDiscriminant::Bind as u64),
        LedgerOperation::Unbind { .. } => F::from(EffectDiscriminant::Unbind as u64),
        LedgerOperation::NewRef { .. } => F::from(EffectDiscriminant::NewRef as u64),
        LedgerOperation::RefPush { .. } => F::from(EffectDiscriminant::RefPush as u64),
        LedgerOperation::RefGet { .. } => F::from(EffectDiscriminant::RefGet as u64),
        LedgerOperation::RefWrite { .. } => F::from(EffectDiscriminant::RefWrite as u64),
        LedgerOperation::InstallHandler { .. } => {
            F::from(EffectDiscriminant::InstallHandler as u64)
        }
        LedgerOperation::UninstallHandler { .. } => {
            F::from(EffectDiscriminant::UninstallHandler as u64)
        }
        LedgerOperation::GetHandlerFor { .. } => F::from(EffectDiscriminant::GetHandlerFor as u64),
    }
}

pub(crate) fn opcode_args(op: &LedgerOperation<F>) -> [F; OPCODE_ARG_COUNT] {
    let mut args = [F::zero(); OPCODE_ARG_COUNT];
    match op {
        LedgerOperation::Nop {} => {}
        LedgerOperation::Resume {
            target,
            val,
            ret,
            caller,
        } => {
            args[ArgName::Target.idx()] = *target;
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Ret.idx()] = *ret;
            args[ArgName::Caller.idx()] = caller.encoded();
        }
        LedgerOperation::CallEffectHandler {
            interface_id,
            val,
            ret,
        } => {
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Ret.idx()] = *ret;
            args[ArgName::InterfaceId0.idx()] = interface_id[0];
            args[ArgName::InterfaceId1.idx()] = interface_id[1];
            args[ArgName::InterfaceId2.idx()] = interface_id[2];
            args[ArgName::InterfaceId3.idx()] = interface_id[3];
        }
        LedgerOperation::Yield { val } => {
            args[ArgName::Val.idx()] = *val;
        }
        LedgerOperation::Return {} => {}
        LedgerOperation::Burn { ret } => {
            args[ArgName::Target.idx()] = F::zero();
            args[ArgName::Ret.idx()] = *ret;
        }
        LedgerOperation::ProgramHash {
            target,
            program_hash,
        } => {
            args[ArgName::Target.idx()] = *target;
            args[ArgName::ProgramHash0.idx()] = program_hash[0];
            args[ArgName::ProgramHash1.idx()] = program_hash[1];
            args[ArgName::ProgramHash2.idx()] = program_hash[2];
            args[ArgName::ProgramHash3.idx()] = program_hash[3];
        }
        LedgerOperation::NewUtxo {
            program_hash,
            val,
            target,
        }
        | LedgerOperation::NewCoord {
            program_hash,
            val,
            target,
        } => {
            args[ArgName::Target.idx()] = *target;
            args[ArgName::Val.idx()] = *val;
            args[ArgName::ProgramHash0.idx()] = program_hash[0];
            args[ArgName::ProgramHash1.idx()] = program_hash[1];
            args[ArgName::ProgramHash2.idx()] = program_hash[2];
            args[ArgName::ProgramHash3.idx()] = program_hash[3];
        }
        LedgerOperation::Activation { val, caller } => {
            args[ArgName::Val.idx()] = *val;
            args[ArgName::ActivationCaller.idx()] = *caller;
        }
        LedgerOperation::Init { val, caller } => {
            args[ArgName::Val.idx()] = *val;
            args[ArgName::ActivationCaller.idx()] = *caller;
        }
        LedgerOperation::Bind { owner_id } => {
            args[ArgName::OwnerId.idx()] = *owner_id;
        }
        LedgerOperation::Unbind { token_id } => {
            args[ArgName::TokenId.idx()] = *token_id;
        }
        LedgerOperation::NewRef { size, ret } => {
            args[ArgName::Size.idx()] = *size;
            args[ArgName::Ret.idx()] = *ret;
        }
        LedgerOperation::RefPush { vals } => {
            args[ArgName::PackedRef0.idx()] = vals[0];
            args[ArgName::PackedRef1.idx()] = vals[1];
            args[ArgName::PackedRef2.idx()] = vals[2];
            args[ArgName::PackedRef3.idx()] = vals[3];
        }
        LedgerOperation::RefGet { reff, offset, ret } => {
            args[ArgName::Val.idx()] = *reff;
            args[ArgName::Offset.idx()] = *offset;

            // Pack 4 return values, leaving slots 1 and 3 for reff/offset.
            args[ArgName::PackedRef0.idx()] = ret[0];
            args[ArgName::PackedRef2.idx()] = ret[1];
            args[ArgName::PackedRef4.idx()] = ret[2];
            args[ArgName::PackedRef5.idx()] = ret[3];
        }
        LedgerOperation::RefWrite { reff, offset, vals } => {
            args[ArgName::Val.idx()] = *reff;
            args[ArgName::Offset.idx()] = *offset;
            // Avoid collisions with Val(idx=1) and Offset(idx=3).
            args[ArgName::PackedRef0.idx()] = vals[0];
            args[ArgName::PackedRef2.idx()] = vals[1];
            args[ArgName::PackedRef4.idx()] = vals[2];
            args[ArgName::PackedRef5.idx()] = vals[3];
        }
        LedgerOperation::InstallHandler { interface_id }
        | LedgerOperation::UninstallHandler { interface_id } => {
            args[ArgName::InterfaceId0.idx()] = interface_id[0];
            args[ArgName::InterfaceId1.idx()] = interface_id[1];
            args[ArgName::InterfaceId2.idx()] = interface_id[2];
            args[ArgName::InterfaceId3.idx()] = interface_id[3];
        }
        LedgerOperation::GetHandlerFor {
            interface_id,
            handler_id,
        } => {
            args[ArgName::InterfaceId0.idx()] = interface_id[0];
            args[ArgName::InterfaceId1.idx()] = interface_id[1];
            args[ArgName::InterfaceId2.idx()] = interface_id[2];
            args[ArgName::InterfaceId3.idx()] = interface_id[3];
            args[ArgName::Ret.idx()] = *handler_id;
        }
    }
    args
}

pub(crate) fn value_to_field(val: starstream_interleaving_spec::Value) -> F {
    F::from(val.0)
}
