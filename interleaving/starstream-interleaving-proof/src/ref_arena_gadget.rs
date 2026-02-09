use crate::circuit::MemoryTag;
use crate::opcode_dsl::{OpcodeDsl, OpcodeSynthDsl, OpcodeTraceDsl};
use crate::switchboard::{RefArenaSwitchboard, RefArenaSwitchboardWires};
use crate::{
    F, LedgerOperation,
    abi::{ArgName, OPCODE_ARG_COUNT},
    ledger_operation::{REF_GET_BATCH_SIZE, REF_PUSH_BATCH_SIZE, REF_WRITE_BATCH_SIZE},
    memory::{IVCMemory, IVCMemoryAllocated},
};
use ark_ff::{AdditiveGroup as _, PrimeField as _};
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::{
    GR1CSVar as _,
    eq::EqGadget,
    fields::{FieldVar as _, fp::FpVar},
    prelude::Boolean,
};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use std::ops::Not;

fn ref_sizes_access_ops<D: OpcodeDsl>(
    dsl: &mut D,
    write_cond: &D::Bool,
    write_addr: &D::Val,
    write_val: &D::Val,
    read_cond: &D::Bool,
    read_addr: &D::Val,
) -> Result<D::Val, D::Error> {
    dsl.write(write_cond, MemoryTag::RefSizes, write_addr, write_val)?;
    dsl.read(read_cond, MemoryTag::RefSizes, read_addr)
}

#[allow(clippy::too_many_arguments)]
fn ref_arena_access_ops<D: OpcodeDsl>(
    dsl: &mut D,
    read_cond: &D::Bool,
    write_cond: &D::Bool,
    write_is_push: &D::Bool,
    push_vals: &[D::Val; REF_PUSH_BATCH_SIZE],
    write_vals: &[D::Val; REF_WRITE_BATCH_SIZE],
    ref_building_ptr: &D::Val,
    val: &D::Val,
    offset: &D::Val,
) -> Result<[D::Val; REF_GET_BATCH_SIZE], D::Error> {
    let scale_get = dsl.const_u64(REF_GET_BATCH_SIZE as u64)?;
    let offset_scaled_get = dsl.mul(offset, &scale_get)?;
    let get_base_addr = dsl.add(val, &offset_scaled_get)?;

    let mut ref_arena_read_vec = Vec::with_capacity(REF_GET_BATCH_SIZE);
    for i in 0..REF_GET_BATCH_SIZE {
        let off = dsl.const_u64(i as u64)?;
        let addr = dsl.add(&get_base_addr, &off)?;
        let read = dsl.read(read_cond, MemoryTag::RefArena, &addr)?;
        ref_arena_read_vec.push(read);
    }

    let scale_write = dsl.const_u64(REF_WRITE_BATCH_SIZE as u64)?;
    let offset_scaled_write = dsl.mul(offset, &scale_write)?;
    let write_base_write = dsl.add(val, &offset_scaled_write)?;
    let write_base_sel = dsl.select(write_is_push, ref_building_ptr, &write_base_write)?;
    let zero = dsl.zero();
    let write_base = dsl.select(write_cond, &write_base_sel, &zero)?;

    for i in 0..REF_WRITE_BATCH_SIZE {
        let off = dsl.const_u64(i as u64)?;
        let addr = dsl.add(&write_base, &off)?;
        let val_sel = dsl.select(write_is_push, &push_vals[i], &write_vals[i])?;
        let val = dsl.select(write_cond, &val_sel, &zero)?;
        dsl.write(write_cond, MemoryTag::RefArena, &addr, &val)?;
    }

    let ref_arena_read: [D::Val; REF_GET_BATCH_SIZE] = ref_arena_read_vec
        .try_into()
        .expect("ref arena read batch length");

    Ok(ref_arena_read)
}

pub(crate) fn trace_ref_arena_ops<M: IVCMemory<F>>(
    mb: &mut M,
    ref_building_id: &mut F,
    ref_building_offset: &mut F,
    ref_building_remaining: &mut F,
    switches: &RefArenaSwitchboard,
    instr: &LedgerOperation<F>,
) {
    let mut ref_push_vals = std::array::from_fn(|_| F::ZERO);
    let mut ref_write_vals = std::array::from_fn(|_| F::ZERO);
    let ref_sizes_write = switches.ref_sizes_write;
    let ref_sizes_read = switches.ref_sizes_read;
    let ref_arena_read = switches.ref_arena_read;
    let ref_arena_write = switches.ref_arena_write;
    let write_is_push = switches.ref_arena_write_is_push;

    let mut ref_get_ref = F::ZERO;
    let mut ref_get_offset = F::ZERO;
    let mut ref_write_ref = F::ZERO;
    let mut ref_write_offset = F::ZERO;
    match instr {
        LedgerOperation::NewRef { size, ret } => {
            *ref_building_id = *ret;
            *ref_building_offset = F::ZERO;
            *ref_building_remaining = *size;
        }
        LedgerOperation::RefPush { vals } => {
            ref_push_vals = *vals;
        }
        LedgerOperation::RefGet {
            reff,
            offset,
            ret: _,
        } => {
            ref_get_ref = *reff;
            ref_get_offset = *offset;
        }
        LedgerOperation::RefWrite { reff, offset, vals } => {
            ref_write_ref = *reff;
            ref_write_offset = *offset;
            ref_write_vals = *vals;
        }
        _ => {}
    };

    let ref_sizes_ref_id = if ref_arena_read {
        ref_get_ref
    } else if ref_arena_write && !write_is_push {
        ref_write_ref
    } else {
        F::ZERO
    };

    let mut dsl = OpcodeTraceDsl { mb };
    let _ = ref_sizes_access_ops(
        &mut dsl,
        &ref_sizes_write,
        ref_building_id,
        ref_building_remaining,
        &ref_sizes_read,
        &ref_sizes_ref_id,
    )
    .expect("trace ref sizes access");

    let op_val = if ref_arena_read {
        ref_get_ref
    } else if ref_arena_write && !write_is_push {
        ref_write_ref
    } else {
        F::ZERO
    };
    let op_offset = if ref_arena_read {
        ref_get_offset
    } else if ref_arena_write && !write_is_push {
        ref_write_offset
    } else {
        F::ZERO
    };

    let push_ptr = *ref_building_id + *ref_building_offset;
    let _ = ref_arena_access_ops(
        &mut dsl,
        &ref_arena_read,
        &ref_arena_write,
        &write_is_push,
        &ref_push_vals,
        &ref_write_vals,
        &push_ptr,
        &op_val,
        &op_offset,
    )
    .expect("trace ref arena access");

    let remaining = ref_building_remaining.into_bigint().0[0] as usize;

    if ref_arena_write && write_is_push {
        *ref_building_offset += F::from(REF_PUSH_BATCH_SIZE as u64);
        *ref_building_remaining = F::from(remaining.saturating_sub(1) as u64);
    }
}

pub(crate) fn ref_arena_read_size<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &RefArenaSwitchboardWires,
    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
    addr: &FpVar<F>,
) -> Result<FpVar<F>, SynthesisError> {
    let write_cond = switches.ref_sizes_write.clone();
    let write_addr = opcode_args[ArgName::Ret.idx()].clone();
    let write_val = opcode_args[ArgName::Size.idx()].clone();

    let read_cond = switches.ref_sizes_read.clone();
    let read_addr = read_cond.select(addr, &FpVar::zero())?;

    let mut dsl = OpcodeSynthDsl { cs, rm };
    ref_sizes_access_ops(
        &mut dsl,
        &write_cond,
        &write_addr,
        &write_val,
        &read_cond,
        &read_addr,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn ref_arena_access_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &RefArenaSwitchboardWires,
    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
    ref_building_ptr: &FpVar<F>,
    ref_building_remaining: &FpVar<F>,
    val: &FpVar<F>,
    offset: &FpVar<F>,
    ref_size_read: &FpVar<F>,
) -> Result<[FpVar<F>; REF_GET_BATCH_SIZE], SynthesisError> {
    let _ = ref_building_remaining;

    let ref_push_vals = [
        opcode_args[ArgName::PackedRef0.idx()].clone(),
        opcode_args[ArgName::PackedRef1.idx()].clone(),
        opcode_args[ArgName::PackedRef2.idx()].clone(),
        opcode_args[ArgName::PackedRef3.idx()].clone(),
    ];
    let ref_write_vals = [
        opcode_args[ArgName::PackedRef0.idx()].clone(),
        opcode_args[ArgName::PackedRef2.idx()].clone(),
        opcode_args[ArgName::PackedRef4.idx()].clone(),
        opcode_args[ArgName::PackedRef5.idx()].clone(),
    ];

    let size_sel_get = switches
        .ref_arena_read
        .select(ref_size_read, &FpVar::zero())?;
    let offset_sel_get = switches.ref_arena_read.select(offset, &FpVar::zero())?;
    let one_if_on_get = switches
        .ref_arena_read
        .select(&FpVar::one(), &FpVar::zero())?;
    let offset_plus_one_get = &offset_sel_get + one_if_on_get;
    let diff_get = &size_sel_get - &offset_plus_one_get;

    range_check_u16(cs.clone(), &switches.ref_arena_read, &size_sel_get)?;
    range_check_u16(cs.clone(), &switches.ref_arena_read, &offset_sel_get)?;
    range_check_u16(cs.clone(), &switches.ref_arena_read, &diff_get)?;

    let write_check_cond =
        &switches.ref_arena_write & switches.ref_arena_write_is_push.clone().not();

    let size_sel_write = write_check_cond.select(ref_size_read, &FpVar::zero())?;
    let offset_sel_write = write_check_cond.select(offset, &FpVar::zero())?;
    let one_if_on_write = write_check_cond.select(&FpVar::one(), &FpVar::zero())?;
    let offset_plus_one_write = &offset_sel_write + one_if_on_write;
    let diff_write = &size_sel_write - &offset_plus_one_write;

    range_check_u16(cs.clone(), &write_check_cond, &size_sel_write)?;
    range_check_u16(cs.clone(), &write_check_cond, &offset_sel_write)?;
    range_check_u16(cs.clone(), &write_check_cond, &diff_write)?;

    let mut dsl = OpcodeSynthDsl { cs: cs.clone(), rm };

    ref_arena_access_ops(
        &mut dsl,
        &switches.ref_arena_read,
        &switches.ref_arena_write,
        &switches.ref_arena_write_is_push,
        &ref_push_vals,
        &ref_write_vals,
        ref_building_ptr,
        val,
        offset,
    )
}

fn range_check_u16(
    cs: ConstraintSystemRef<F>,
    switch: &Boolean<F>,
    value: &FpVar<F>,
) -> Result<(), SynthesisError> {
    let value_u64 = value.value().unwrap().into_bigint().as_ref()[0] & 0xFFFF;
    let mut bits = Vec::with_capacity(16);
    for i in 0..16 {
        let bit = Boolean::new_witness(cs.clone(), || Ok(((value_u64 >> i) & 1) == 1))?;
        bits.push(bit);
    }

    let mut recomposed = FpVar::zero();
    for (i, bit) in bits.iter().enumerate() {
        let coeff = FpVar::new_constant(cs.clone(), F::from(1u64 << i))?;
        let term = bit.select(&coeff, &FpVar::zero())?;
        recomposed += term;
    }

    recomposed.conditional_enforce_equal(value, switch)?;

    Ok(())
}
