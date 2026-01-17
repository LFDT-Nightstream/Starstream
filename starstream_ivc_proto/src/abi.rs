use crate::{ArgName, F, LedgerOperation, OPCODE_ARG_COUNT, OptionalF};
use ark_ff::Zero;
use starstream_mock_ledger::{EffectDiscriminant, LedgerEffectsCommitment, WitLedgerEffect};

pub fn commit(prev: LedgerEffectsCommitment, op: WitLedgerEffect) -> LedgerEffectsCommitment {
    let ledger_op = ledger_operation_from_wit(&op);
    let opcode_discriminant = opcode_discriminant(&ledger_op);
    let opcode_args = opcode_args(&ledger_op);

    let mut concat = [F::zero(); 12];
    concat[..4].copy_from_slice(&prev.0);
    concat[4] = opcode_discriminant;
    concat[5..9].copy_from_slice(&opcode_args);

    let compressed =
        ark_poseidon2::compress_12_trace(&concat).expect("poseidon2 compress_12_trace");
    LedgerEffectsCommitment(compressed)
}

pub(crate) fn ledger_operation_from_wit(op: &WitLedgerEffect) -> LedgerOperation<F> {
    match op {
        WitLedgerEffect::Resume {
            target,
            val,
            ret,
            id_prev,
        } => LedgerOperation::Resume {
            target: F::from(target.0 as u64),
            val: F::from(val.0),
            ret: ret.to_option().map(|r| F::from(r.0)).unwrap_or_default(),
            id_prev: OptionalF::from_option(
                id_prev.to_option().flatten().map(|p| F::from(p.0 as u64)),
            ),
        },
        WitLedgerEffect::Yield { val, ret, id_prev } => LedgerOperation::Yield {
            val: F::from(val.0),
            ret: ret.to_option().map(|r| F::from(r.0)),
            id_prev: OptionalF::from_option(
                id_prev.to_option().flatten().map(|p| F::from(p.0 as u64)),
            ),
        },
        WitLedgerEffect::Burn { ret } => LedgerOperation::Burn {
            ret: F::from(ret.unwrap().0),
        },
        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => LedgerOperation::ProgramHash {
            target: F::from(target.0 as u64),
            program_hash: F::from(program_hash.unwrap().0[0] as u64),
        },
        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => LedgerOperation::NewUtxo {
            program_hash: F::from(program_hash.0[0] as u64),
            val: F::from(val.0),
            target: F::from(id.unwrap().0 as u64),
        },
        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => LedgerOperation::NewCoord {
            program_hash: F::from(program_hash.0[0] as u64),
            val: F::from(val.0),
            target: F::from(id.unwrap().0 as u64),
        },
        WitLedgerEffect::Activation { val, caller } => LedgerOperation::Activation {
            val: F::from(val.0),
            caller: F::from(caller.unwrap().0 as u64),
        },
        WitLedgerEffect::Init { val, caller } => LedgerOperation::Init {
            val: F::from(val.0),
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
        WitLedgerEffect::RefPush { val } => LedgerOperation::RefPush {
            val: value_to_field(*val),
        },
        WitLedgerEffect::Get { reff, offset, ret } => LedgerOperation::Get {
            reff: F::from(reff.0),
            offset: F::from(*offset as u64),
            ret: value_to_field(ret.unwrap()),
        },
        WitLedgerEffect::InstallHandler { interface_id } => LedgerOperation::InstallHandler {
            interface_id: F::from(interface_id.0[0] as u64),
        },
        WitLedgerEffect::UninstallHandler { interface_id } => LedgerOperation::UninstallHandler {
            interface_id: F::from(interface_id.0[0] as u64),
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id,
            handler_id,
        } => LedgerOperation::GetHandlerFor {
            interface_id: F::from(interface_id.0[0] as u64),
            handler_id: F::from(handler_id.unwrap().0 as u64),
        },
    }
}

pub(crate) fn opcode_discriminant(op: &LedgerOperation<F>) -> F {
    match op {
        LedgerOperation::Nop {} => F::zero(),
        LedgerOperation::Resume { .. } => F::from(EffectDiscriminant::Resume as u64),
        LedgerOperation::Yield { .. } => F::from(EffectDiscriminant::Yield as u64),
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
        LedgerOperation::Get { .. } => F::from(EffectDiscriminant::Get as u64),
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
            id_prev,
        } => {
            args[ArgName::Target.idx()] = *target;
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Ret.idx()] = *ret;
            args[ArgName::IdPrev.idx()] = id_prev.encoded();
        }
        LedgerOperation::Yield { val, ret, id_prev } => {
            args[ArgName::Target.idx()] = id_prev.decode_or_zero();
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Ret.idx()] = ret.unwrap_or_default();
            args[ArgName::IdPrev.idx()] = id_prev.encoded();
        }
        LedgerOperation::Burn { ret } => {
            args[ArgName::Target.idx()] = F::zero();
            args[ArgName::Ret.idx()] = *ret;
        }
        LedgerOperation::ProgramHash {
            target,
            program_hash,
        } => {
            args[ArgName::Target.idx()] = *target;
            args[ArgName::ProgramHash.idx()] = *program_hash;
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
            args[ArgName::ProgramHash.idx()] = *program_hash;
        }
        LedgerOperation::Activation { val, caller } => {
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Caller.idx()] = *caller;
        }
        LedgerOperation::Init { val, caller } => {
            args[ArgName::Val.idx()] = *val;
            args[ArgName::Caller.idx()] = *caller;
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
        LedgerOperation::RefPush { val } => {
            args[ArgName::Val.idx()] = *val;
        }
        LedgerOperation::Get { reff, offset, ret } => {
            args[ArgName::Val.idx()] = *reff;
            args[ArgName::Offset.idx()] = *offset;
            args[ArgName::Ret.idx()] = *ret;
        }
        LedgerOperation::InstallHandler { interface_id }
        | LedgerOperation::UninstallHandler { interface_id } => {
            args[ArgName::InterfaceId.idx()] = *interface_id;
        }
        LedgerOperation::GetHandlerFor {
            interface_id,
            handler_id,
        } => {
            args[ArgName::InterfaceId.idx()] = *interface_id;
            args[ArgName::Ret.idx()] = *handler_id;
        }
    }
    args
}

pub(crate) fn value_to_field(val: starstream_mock_ledger::Value) -> F {
    F::from(val.0)
}
