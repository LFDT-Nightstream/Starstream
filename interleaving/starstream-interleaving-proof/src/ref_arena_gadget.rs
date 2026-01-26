use std::collections::BTreeMap;

use crate::{
    F, LedgerOperation,
    abi::{ArgName, OPCODE_ARG_COUNT},
    circuit::MemoryTag,
    ledger_operation::{REF_GET_BATCH_SIZE, REF_PUSH_BATCH_SIZE},
    memory::{Address, IVCMemory, IVCMemoryAllocated},
};
use crate::{circuit::ExecutionSwitches, rem_wires_gadget::alloc_rem_one_hot_selectors};
use ark_ff::{AdditiveGroup as _, Field as _, PrimeField as _};
use ark_r1cs_std::{
    alloc::AllocVar as _,
    cmp::CmpGadget as _,
    fields::{FieldVar as _, fp::FpVar},
    prelude::Boolean,
    uint::UInt,
};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

pub(crate) fn trace_ref_arena_ops<M: IVCMemory<F>>(
    mb: &mut M,
    ref_building_id: &mut F,
    ref_building_offset: &mut F,
    ref_building_remaining: &mut F,
    ref_sizes: &mut BTreeMap<u64, u64>,
    instr: &LedgerOperation<F>,
) {
    let mut ref_push_vals = std::array::from_fn(|_| F::ZERO);
    let mut ref_push = false;
    let mut ref_get = false;
    let mut new_ref = false;

    let mut ref_get_ref = F::ZERO;
    let mut ref_get_offset = F::ZERO;

    match instr {
        LedgerOperation::NewRef { size, ret } => {
            *ref_building_id = *ret;
            *ref_building_offset = F::ZERO;
            *ref_building_remaining = *size;
            ref_sizes.insert(ret.into_bigint().0[0], size.into_bigint().0[0]);

            new_ref = true;
        }
        LedgerOperation::RefPush { vals } => {
            ref_push_vals = *vals;
            ref_push = true;
        }
        LedgerOperation::Get {
            reff,
            offset,
            ret: _,
        } => {
            ref_get = true;

            ref_get_ref = *reff;
            ref_get_offset = *offset;
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

    mb.conditional_read(
        ref_get,
        Address {
            tag: MemoryTag::RefSizes.into(),
            addr: ref_get_ref.into_bigint().0[0],
        },
    );

    let remaining = ref_building_remaining.into_bigint().0[0] as usize;
    let to_write = remaining.min(REF_PUSH_BATCH_SIZE);

    for (i, val) in ref_push_vals.iter().enumerate() {
        let should_write = i < to_write && ref_push;
        let addr = ref_building_id.into_bigint().0[0] + ref_building_offset.into_bigint().0[0];

        mb.conditional_write(
            should_write,
            Address {
                tag: MemoryTag::RefArena.into(),
                addr,
            },
            vec![*val],
        );

        if should_write {
            *ref_building_offset += F::ONE;
        }
    }

    if ref_push {
        *ref_building_remaining = F::from(remaining.saturating_sub(to_write) as u64);
    }

    let size = ref_sizes
        .get(&ref_get_ref.into_bigint().0[0])
        .copied()
        .unwrap_or(0);
    let offset = ref_get_offset.into_bigint().0[0];
    let remaining = size.saturating_sub(offset);
    let to_read = remaining.min(REF_GET_BATCH_SIZE as u64);
    for i in 0..REF_GET_BATCH_SIZE {
        let addr = ref_get_ref.into_bigint().0[0] + offset + i as u64;
        let should_read = (i as u64) < to_read && ref_get;
        mb.conditional_read(
            should_read,
            Address {
                tag: MemoryTag::RefArena.into(),
                addr,
            },
        );
    }

    for _ in 0..REF_PUSH_BATCH_SIZE - REF_GET_BATCH_SIZE {
        mb.conditional_read(
            false,
            Address {
                tag: MemoryTag::RefArena.into(),
                addr: 0,
            },
        );
    }
}

pub(crate) fn ref_arena_new_ref_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &ExecutionSwitches<Boolean<F>>,

    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
) -> Result<(), SynthesisError> {
    rm.conditional_write(
        &switches.new_ref,
        &Address {
            tag: MemoryTag::RefSizes.allocate(cs.clone())?,
            addr: opcode_args[ArgName::Ret.idx()].clone(),
        },
        &[opcode_args[ArgName::Size.idx()].clone()],
    )?;

    Ok(())
}

pub(crate) fn ref_arena_get_read_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &ExecutionSwitches<Boolean<F>>,
    val: FpVar<F>,
    offset: FpVar<F>,
) -> Result<
    (
        [FpVar<F>; REF_GET_BATCH_SIZE],
        [Boolean<F>; REF_GET_BATCH_SIZE],
    ),
    SynthesisError,
> {
    let ref_size_read = rm.conditional_read(
        &switches.get,
        &Address {
            tag: MemoryTag::RefSizes.allocate(cs.clone())?,
            addr: val.clone(),
        },
    )?[0]
        .clone();

    let ref_size_sel = switches.get.select(&ref_size_read, &FpVar::zero())?;
    let offset_sel = switches.get.select(&offset, &FpVar::zero())?;

    let (ref_size_u32, _) = UInt::<32, u32, F>::from_fp(&ref_size_sel)?;
    let (offset_u32, _) = UInt::<32, u32, F>::from_fp(&offset_sel)?;
    let size_ge_offset = ref_size_u32.is_ge(&offset_u32)?;
    let remaining = size_ge_offset.select(&(&ref_size_sel - &offset_sel), &FpVar::zero())?;

    let get_lane_switches =
        alloc_rem_one_hot_selectors::<REF_GET_BATCH_SIZE>(&cs, &remaining, &switches.get)?;

    // addr = ref + offset, read a packed batch (5) to match trace_ref_arena_ops
    let get_base_addr = &val + &offset;
    let mut ref_arena_read_vec = Vec::with_capacity(REF_GET_BATCH_SIZE);
    for i in 0..REF_GET_BATCH_SIZE {
        let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
        let get_addr = &get_base_addr + offset;
        let read = rm.conditional_read(
            &get_lane_switches[i],
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: get_addr,
            },
        )?[0]
            .clone();
        ref_arena_read_vec.push(read);
    }

    // we need the same number of reads from the RefArena as we do writes we do
    // more writes than reads, so we need to pad the rest.
    for _ in 0..REF_PUSH_BATCH_SIZE - REF_GET_BATCH_SIZE {
        let _ = rm.conditional_read(
            &Boolean::FALSE,
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: FpVar::zero(),
            },
        )?;
    }

    let ref_arena_read: [FpVar<F>; REF_GET_BATCH_SIZE] = ref_arena_read_vec
        .try_into()
        .expect("ref arena read batch length");

    Ok((ref_arena_read, get_lane_switches))
}

pub(crate) fn ref_arena_push_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    switches: &ExecutionSwitches<Boolean<F>>,
    opcode_args: &[FpVar<F>; OPCODE_ARG_COUNT],
    ref_building_ptr: &FpVar<F>,
    ref_building_remaining: &FpVar<F>,
) -> Result<[Boolean<F>; REF_PUSH_BATCH_SIZE], SynthesisError> {
    let ref_push_lane_switches =
        alloc_rem_one_hot_selectors(&cs, &ref_building_remaining, &switches.ref_push)?;

    // We also need to write for RefPush.
    for (i, ref_val) in opcode_args.iter().enumerate() {
        let offset = FpVar::new_constant(cs.clone(), F::from(i as u64))?;
        let push_addr = ref_building_ptr + offset;

        rm.conditional_write(
            &ref_push_lane_switches[i],
            &Address {
                tag: MemoryTag::RefArena.allocate(cs.clone())?,
                addr: push_addr,
            },
            &[ref_val.clone()],
        )?;
    }

    Ok(ref_push_lane_switches)
}
