use crate::F;
use crate::opcode_dsl::{OpcodeDsl, OpcodeSynthDsl, OpcodeTraceDsl};
use crate::{circuit::MemoryTag, memory::IVCMemory, memory::IVCMemoryAllocated};
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

fn program_hash_ops<D: OpcodeDsl>(
    dsl: &mut D,
    read_cond: &D::Bool,
    target: &D::Val,
) -> Result<[D::Val; 4], D::Error> {
    let mut out = Vec::with_capacity(4);
    let stride = dsl.const_u64(4)?;
    for i in 0..4 {
        let offset = dsl.const_u64(i as u64)?;
        let addr = dsl.add(&dsl.mul(target, &stride)?, &offset)?;
        let value = dsl.read(read_cond, MemoryTag::ProcessTable, &addr)?;
        out.push(value);
    }
    Ok(out.try_into().expect("program hash length"))
}

pub fn trace_program_hash_ops<M: IVCMemory<F>>(mb: &mut M, read_cond: bool, target: &F) -> [F; 4] {
    let mut dsl = OpcodeTraceDsl { mb };
    program_hash_ops(&mut dsl, &read_cond, target).expect("trace program hash")
}

pub fn program_hash_access_wires<M: IVCMemoryAllocated<F>>(
    cs: ConstraintSystemRef<F>,
    rm: &mut M,
    read_cond: &ark_r1cs_std::prelude::Boolean<F>,
    target: &FpVar<F>,
) -> Result<[FpVar<F>; 4], SynthesisError> {
    let mut dsl = OpcodeSynthDsl { cs, rm };
    program_hash_ops(&mut dsl, read_cond, target)
}
