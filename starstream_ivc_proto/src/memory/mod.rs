use crate::F;
use ark_ff::PrimeField;
use ark_r1cs_std::{GR1CSVar as _, alloc::AllocVar, fields::fp::FpVar, prelude::Boolean};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
pub use dummy::DummyMemory;

mod dummy;
pub mod nebula;

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone)]
pub struct Address<A = u64, T = u64> {
    pub tag: T,
    pub addr: A,
}

pub type AllocatedAddress = Address<FpVar<F>, FpVar<F>>;

impl Address {
    pub(crate) fn allocate(
        &self,
        cs: ConstraintSystemRef<F>,
    ) -> Result<AllocatedAddress, SynthesisError> {
        Ok(Address {
            addr: FpVar::new_witness(cs.clone(), || Ok(F::from(self.addr)))?,
            tag: FpVar::new_witness(cs, || Ok(F::from(self.tag)))?,
        })
    }
}

impl AllocatedAddress {
    pub fn address_value(&self) -> u64 {
        self.addr.value().unwrap().into_bigint().as_ref()[0]
    }

    pub fn tag_value(&self) -> u64 {
        self.tag.value().unwrap().into_bigint().as_ref()[0]
    }

    pub fn values(&self) -> Address<u64, u64> {
        Address {
            tag: self.tag_value(),
            addr: self.address_value(),
        }
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
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError>;

    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError>;
}
