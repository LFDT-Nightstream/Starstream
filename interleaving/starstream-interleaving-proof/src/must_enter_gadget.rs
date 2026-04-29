use crate::F;
use crate::opcode_dsl::{OpcodeDsl, OpcodeSynthDsl, OpcodeTraceDsl};
use crate::{memory::IVCMemory, memory::IVCMemoryAllocated};
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use starstream_interleaving_spec::RamMemoryTag;

fn must_enter_addr<D: OpcodeDsl>(dsl: &D, pid: &D::Val, lane: usize) -> Result<D::Val, D::Error> {
    let stride = dsl.const_u64(4)?;
    let offset = dsl.const_u64(lane as u64)?;
    dsl.add(&dsl.mul(pid, &stride)?, &offset)
}

fn must_enter_read_ops<D: OpcodeDsl>(
    dsl: &mut D,
    read_cond: &D::Bool,
    pid: &D::Val,
) -> Result<[D::Val; 4], D::Error> {
    let mut out = Vec::with_capacity(4);
    for lane in 0..4 {
        let addr = must_enter_addr(dsl, pid, lane)?;
        out.push(dsl.read(read_cond, RamMemoryTag::MustEnter, &addr)?);
    }
    Ok(out.try_into().expect("must enter length"))
}

fn must_enter_write_ops<D: OpcodeDsl>(
    dsl: &mut D,
    write_cond: &D::Bool,
    pid: &D::Val,
    vals: &[D::Val; 4],
) -> Result<(), D::Error> {
    for (lane, val) in vals.iter().enumerate() {
        let addr = must_enter_addr(dsl, pid, lane)?;
        dsl.write(write_cond, RamMemoryTag::MustEnter, &addr, val)?;
    }
    Ok(())
}

pub fn trace_must_enter_reads<M: IVCMemory<F>>(mb: &mut M, read_cond: bool, pid: u64) -> [F; 4] {
    let mut dsl = OpcodeTraceDsl { mb };
    must_enter_read_ops(&mut dsl, &read_cond, &F::from(pid)).expect("trace must enter")
}

pub fn trace_must_enter_writes<M: IVCMemory<F>>(
    mb: &mut M,
    write_cond: bool,
    pid: u64,
    vals: &[F; 4],
) {
    let mut dsl = OpcodeTraceDsl { mb };
    must_enter_write_ops(&mut dsl, &write_cond, &F::from(pid), vals)
        .expect("trace must enter writes");
}

pub fn must_enter_read_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    read_cond: &ark_r1cs_std::prelude::Boolean<F>,
    pid: &FpVar<F>,
) -> Result<[FpVar<F>; 4], SynthesisError> {
    let mut dsl = OpcodeSynthDsl { cs, rm };
    must_enter_read_ops(&mut dsl, read_cond, pid)
}

pub fn must_enter_write_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    write_cond: &ark_r1cs_std::prelude::Boolean<F>,
    pid: &FpVar<F>,
    vals: &[FpVar<F>; 4],
) -> Result<(), SynthesisError> {
    let mut dsl = OpcodeSynthDsl { cs, rm };
    must_enter_write_ops(&mut dsl, write_cond, pid, vals)
}
