use crate::circuit::ExecutionSwitches;
use crate::{
    F, LedgerOperation,
    abi::{ArgName, OPCODE_ARG_COUNT},
    circuit::MemoryTag,
    ledger_operation::{REF_GET_BATCH_SIZE, REF_PUSH_BATCH_SIZE, REF_WRITE_BATCH_SIZE},
    memory::{Address, IVCMemory, IVCMemoryAllocated},
};
use ark_ff::{AdditiveGroup, PrimeField as _};
use ark_r1cs_std::{
    GR1CSVar as _,
    alloc::AllocVar as _,
    eq::EqGadget,
    fields::{FieldVar as _, fp::FpVar},
    prelude::Boolean,
};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

pub(crate) fn trace_ref_arena_ops<M: IVCMemory<F>>(
    mb: &mut M,
    ref_building_id: &mut F,
    ref_building_offset: &mut F,
    ref_building_remaining: &mut F,
    instr: &LedgerOperation<F>,
) {
    let mut ref_push_vals = std::array::from_fn(|_| F::ZERO);
    let mut ref_write_vals = std::array::from_fn(|_| F::ZERO);
    let mut ref_push = false;
    let mut ref_get = false;
    let mut ref_write = false;
    let mut new_ref = false;

    let mut ref_get_ref = F::ZERO;
    let mut ref_get_offset = F::ZERO;
    let mut ref_write_ref = F::ZERO;
    let mut ref_write_offset = F::ZERO;
    match instr {
        LedgerOperation::NewRef { size, ret } => {
            *ref_building_id = *ret;
            *ref_building_offset = F::ZERO;
            *ref_building_remaining = *size;

            new_ref = true;
        }
        LedgerOperation::RefPush { vals } => {
            ref_push_vals = *vals;
            ref_push = true;
        }
        LedgerOperation::RefGet {
            reff,
            offset,
            ret: _,
        } => {
            ref_get = true;

            ref_get_ref = *reff;
            ref_get_offset = *offset;
        }
        LedgerOperation::RefWrite { reff, offset, vals } => {
            ref_write = true;
            ref_write_ref = *reff;
            ref_write_offset = *offset;
            ref_write_vals = *vals;
        }
        _ => {}
    };

    mb.conditional_write(
        new_ref,
        Address {
            tag: MemoryTag::RefSizes.into(),
            addr: ref_building_id.into_bigint().0[0],
        },
        vec![*ref_building_remaining],
    );

    let ref_sizes_read = ref_get || ref_write;
    let ref_sizes_ref_id = if ref_get {
        ref_get_ref
    } else if ref_write {
        ref_write_ref
    } else {
        F::ZERO
    };

    mb.conditional_read(
        ref_sizes_read,
        Address {
            tag: MemoryTag::RefSizes.into(),
            addr: ref_sizes_ref_id.into_bigint().0[0],
        },
    );

    let offset = ref_get_offset.into_bigint().0[0];

    let base = ref_get_ref.into_bigint().0[0] + (offset * REF_GET_BATCH_SIZE as u64);

    for i in 0..REF_GET_BATCH_SIZE {
        let addr = base + i as u64;
        let should_read = ref_get;
        mb.conditional_read(
            should_read,
            Address {
                tag: MemoryTag::RefArena.into(),
                addr,
            },
        );
    }

    let write_offset = ref_write_offset.into_bigint().0[0];
    let write_base =
        ref_write_ref.into_bigint().0[0] + (write_offset * REF_WRITE_BATCH_SIZE as u64);
    let push_base = ref_building_id.into_bigint().0[0] + ref_building_offset.into_bigint().0[0];
    let should_write = ref_push || ref_write;
    let write_base = if ref_push {
        push_base
    } else if ref_write {
        write_base
    } else {
        0
    };
    let write_vals = if ref_push {
        ref_push_vals
    } else {
        ref_write_vals
    };
    for (i, val) in write_vals.iter().enumerate().take(REF_WRITE_BATCH_SIZE) {
        let addr = write_base + i as u64;
        mb.conditional_write(
            should_write,
            Address {
                tag: MemoryTag::RefArena.into(),
                addr,
            },
            vec![*val],
        );
    }

    let remaining = ref_building_remaining.into_bigint().0[0] as usize;
    if ref_push {
        *ref_building_offset += F::from(REF_PUSH_BATCH_SIZE as u64);
        *ref_building_remaining = F::from(remaining.saturating_sub(1) as u64);
    }
}

pub(crate) fn ref_arena_read_size<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &ExecutionSwitches<Boolean<F>>,
    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
    addr: &FpVar<F>,
) -> Result<FpVar<F>, SynthesisError> {
    rm.conditional_write(
        &switches.new_ref,
        &Address {
            tag: MemoryTag::RefSizes.allocate(cs.clone())?,
            addr: opcode_args[ArgName::Ret.idx()].clone(),
        },
        &[opcode_args[ArgName::Size.idx()].clone()],
    )?;

    let read_switch = &switches.get | &switches.ref_write;
    let read_addr = read_switch.select(addr, &FpVar::zero())?;

    let ref_size_read = rm.conditional_read(
        &read_switch,
        &Address {
            tag: MemoryTag::RefSizes.allocate(cs.clone())?,
            addr: read_addr,
        },
    )?[0]
        .clone();

    Ok(ref_size_read)
}

pub(crate) fn ref_arena_access_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &ExecutionSwitches<Boolean<F>>,
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

    let size_sel_get = switches.get.select(ref_size_read, &FpVar::zero())?;
    let offset_sel_get = switches.get.select(offset, &FpVar::zero())?;
    let one_if_on_get = switches.get.select(&FpVar::one(), &FpVar::zero())?;
    let offset_plus_one_get = &offset_sel_get + one_if_on_get;
    let diff_get = &size_sel_get - &offset_plus_one_get;

    range_check_u16(cs.clone(), &switches.get, &size_sel_get)?;
    range_check_u16(cs.clone(), &switches.get, &offset_sel_get)?;
    range_check_u16(cs.clone(), &switches.get, &diff_get)?;

    let size_sel_write = switches.ref_write.select(ref_size_read, &FpVar::zero())?;
    let offset_sel_write = switches.ref_write.select(offset, &FpVar::zero())?;
    let one_if_on_write = switches.ref_write.select(&FpVar::one(), &FpVar::zero())?;
    let offset_plus_one_write = &offset_sel_write + one_if_on_write;
    let diff_write = &size_sel_write - &offset_plus_one_write;

    range_check_u16(cs.clone(), &switches.ref_write, &size_sel_write)?;
    range_check_u16(cs.clone(), &switches.ref_write, &offset_sel_write)?;
    range_check_u16(cs.clone(), &switches.ref_write, &diff_write)?;

    let scale = FpVar::new_constant(cs.clone(), F::from(REF_GET_BATCH_SIZE as u64))?;
    let get_base_addr = val + (offset * scale);
    let mut ref_arena_read_vec = Vec::with_capacity(REF_GET_BATCH_SIZE);
    for i in 0..REF_GET_BATCH_SIZE {
        let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
        let get_addr = &get_base_addr + offset;
        let read = rm.conditional_read(
            &switches.get,
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: get_addr,
            },
        )?[0]
            .clone();
        ref_arena_read_vec.push(read);
    }

    let ref_arena_read: [FpVar<F>; REF_GET_BATCH_SIZE] = ref_arena_read_vec
        .try_into()
        .expect("ref arena read batch length");

    let scale = FpVar::new_constant(cs.clone(), F::from(REF_WRITE_BATCH_SIZE as u64))?;
    let write_base_push = ref_building_ptr.clone();
    let write_base_write = val + (offset * scale);
    let write_cond = &switches.ref_push | &switches.ref_write;
    let write_base_sel = switches
        .ref_push
        .select(&write_base_push, &write_base_write)?;
    let write_base = write_cond.select(&write_base_sel, &FpVar::zero())?;

    for i in 0..REF_WRITE_BATCH_SIZE {
        let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
        let write_addr = &write_base + offset;
        let push_val = ref_push_vals[i].clone();
        let write_val = ref_write_vals[i].clone();
        let val_sel = switches.ref_push.select(&push_val, &write_val)?;
        let val = write_cond.select(&val_sel, &FpVar::zero())?;

        rm.conditional_write(
            &write_cond,
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: write_addr,
            },
            &[val],
        )?;
    }

    Ok(ref_arena_read)
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
