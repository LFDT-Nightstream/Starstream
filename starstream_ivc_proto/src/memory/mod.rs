use crate::F;
use ark_ff::PrimeField;
use ark_r1cs_std::{alloc::AllocVar, fields::fp::FpVar, prelude::Boolean};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
pub use dummy::DummyMemory;

mod dummy;
pub mod nebula;

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone)]
pub struct Address<F> {
    pub addr: F,
    pub tag: u64,
}

impl Address<u64> {
    pub(crate) fn allocate(
        &self,
        cs: ConstraintSystemRef<F>,
    ) -> Result<Address<FpVar<F>>, SynthesisError> {
        Ok(Address {
            addr: FpVar::new_witness(cs, || Ok(F::from(self.addr)))?,
            tag: self.tag,
        })
    }
}

pub trait IVCMemory<F: PrimeField> {
    type Allocator: IVCMemoryAllocated<F>;
    type Params;

    fn new(info: Self::Params) -> Self;

    fn register_mem(&mut self, tag: u64, size: u64, debug_name: &'static str);

    fn init(&mut self, address: Address<u64>, values: Vec<F>);

    fn conditional_read(&mut self, cond: bool, address: Address<u64>) -> Vec<F>;
    fn conditional_write(&mut self, cond: bool, address: Address<u64>, value: Vec<F>);

    fn constraints(self) -> Self::Allocator;
}

pub trait IVCMemoryAllocated<F: PrimeField> {
    fn get_cs(&self) -> ConstraintSystemRef<F>;
    fn start_step(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError>;
    fn finish_step(&mut self, is_last_step: bool) -> Result<(), SynthesisError>;

    fn conditional_read(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
    ) -> Result<Vec<FpVar<F>>, SynthesisError>;

    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError>;
}
