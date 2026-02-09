use crate::F;
use crate::circuit::MemoryTag;
use crate::memory::{Address, IVCMemory, IVCMemoryAllocated};
use ark_ff::{AdditiveGroup as _, PrimeField as _};
use ark_r1cs_std::{
    alloc::AllocVar as _,
    fields::{FieldVar as _, fp::FpVar},
    prelude::Boolean,
};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use std::convert::Infallible;

// a higher level DSL on top of arkworks and the MCC api
//
// in most cases the memory tracing logic mirrors the circuit synthesis logic.
//
// the idea of this trait is that we can then provide two backends
//
// one that works out-of-circuit, whose only purpose is to feed the memory
// tracer with the right values, and another backend that just re-uses those
// values for witness assignment, and emits the constraints for r1cs shape
// construction.
//
// NOTE: not all the circuit is currently using this yet
// NOTE: after we get all the opcodes to use this abstraction (which may require
// more changes), we may also replace the arkworks DSL entirely.
pub trait OpcodeDsl {
    type Bool;
    type Val: std::fmt::Debug;
    type Error;

    fn zero(&self) -> Self::Val;
    fn const_u64(&self, val: u64) -> Result<Self::Val, Self::Error>;
    fn add(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error>;
    fn mul(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error>;
    fn select(
        &self,
        cond: &Self::Bool,
        t: &Self::Val,
        f: &Self::Val,
    ) -> Result<Self::Val, Self::Error>;
    fn read(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
    ) -> Result<Self::Val, Self::Error>;
    fn write(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
        val: &Self::Val,
    ) -> Result<(), Self::Error>;
}

pub struct OpcodeTraceDsl<'a, M> {
    pub mb: &'a mut M,
}

impl<'a, M: IVCMemory<F>> OpcodeDsl for OpcodeTraceDsl<'a, M> {
    type Bool = bool;
    type Val = F;
    type Error = Infallible;

    fn zero(&self) -> Self::Val {
        F::ZERO
    }

    fn const_u64(&self, val: u64) -> Result<Self::Val, Self::Error> {
        Ok(F::from(val))
    }

    fn add(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error> {
        Ok(*lhs + *rhs)
    }

    fn mul(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error> {
        Ok(*lhs * *rhs)
    }

    fn select(
        &self,
        cond: &Self::Bool,
        t: &Self::Val,
        f: &Self::Val,
    ) -> Result<Self::Val, Self::Error> {
        Ok(if *cond { *t } else { *f })
    }

    fn read(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
    ) -> Result<Self::Val, Self::Error> {
        let addr_u64 = addr.into_bigint().0[0];
        let read = self.mb.conditional_read(
            *cond,
            Address {
                tag: tag.into(),
                addr: addr_u64,
            },
        );
        Ok(read[0])
    }

    fn write(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
        val: &Self::Val,
    ) -> Result<(), Self::Error> {
        let addr_u64 = addr.into_bigint().0[0];
        self.mb.conditional_write(
            *cond,
            Address {
                tag: tag.into(),
                addr: addr_u64,
            },
            vec![*val],
        );
        Ok(())
    }
}

pub struct OpcodeSynthDsl<'a, M> {
    pub cs: ConstraintSystemRef<F>,
    pub rm: &'a mut M,
}

impl<'a, M: IVCMemoryAllocated<F>> OpcodeDsl for OpcodeSynthDsl<'a, M> {
    type Bool = Boolean<F>;
    type Val = FpVar<F>;
    type Error = SynthesisError;

    fn zero(&self) -> Self::Val {
        FpVar::zero()
    }

    fn const_u64(&self, val: u64) -> Result<Self::Val, Self::Error> {
        FpVar::new_constant(self.cs.clone(), F::from(val))
    }

    fn add(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error> {
        Ok(lhs + rhs)
    }

    fn mul(&self, lhs: &Self::Val, rhs: &Self::Val) -> Result<Self::Val, Self::Error> {
        Ok(lhs * rhs)
    }

    fn select(
        &self,
        cond: &Self::Bool,
        t: &Self::Val,
        f: &Self::Val,
    ) -> Result<Self::Val, Self::Error> {
        cond.select(t, f)
    }

    fn read(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
    ) -> Result<Self::Val, Self::Error> {
        let read = self.rm.conditional_read(
            cond,
            &Address {
                tag: tag.allocate(self.cs.clone())?,
                addr: addr.clone(),
            },
        )?[0]
            .clone();
        Ok(read)
    }

    fn write(
        &mut self,
        cond: &Self::Bool,
        tag: MemoryTag,
        addr: &Self::Val,
        val: &Self::Val,
    ) -> Result<(), Self::Error> {
        self.rm.conditional_write(
            cond,
            &Address {
                tag: tag.allocate(self.cs.clone())?,
                addr: addr.clone(),
            },
            std::slice::from_ref(val),
        )
    }
}
