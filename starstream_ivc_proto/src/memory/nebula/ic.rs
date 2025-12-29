use super::Address;
use super::MemOp;
use crate::F;
use crate::memory::AllocatedAddress;
use crate::nebula::MemOpAllocated;
use crate::poseidon2::compress;
use ark_ff::AdditiveGroup as _;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::ConstraintSystemRef;
use ark_relations::gr1cs::SynthesisError;
use std::array;

pub struct ICPlain {
    pub comm: [F; 4],
}

impl ICPlain {
    pub fn zero() -> Self {
        Self { comm: [F::ZERO; 4] }
    }

    pub fn increment(
        &mut self,
        a: &Address<u64>,
        vt: &MemOp<F>,
        unsound_make_nop: bool,
    ) -> Result<(), SynthesisError> {
        if !unsound_make_nop {
            let hash_input = array::from_fn(|i| {
                if i == 0 {
                    F::from(a.addr)
                } else if i == 1 {
                    F::from(a.tag)
                } else if i == 2 {
                    F::from(vt.timestamp)
                } else {
                    vt.values.get(i - 3).copied().unwrap_or(F::ZERO)
                }
            });

            let hash_to_field = crate::poseidon2::compress_trace(&hash_input)?;

            let concat = array::from_fn(|i| {
                if i < 4 {
                    hash_to_field[i]
                } else {
                    self.comm[i - 4]
                }
            });

            self.comm = crate::poseidon2::compress_trace(&concat)?;
        }

        Ok(())
    }

    pub fn allocate(&self, cs: ConstraintSystemRef<F>) -> Result<IC, SynthesisError> {
        Ok(IC {
            comm: [
                FpVar::new_witness(cs.clone(), || Ok(self.comm[0]))?,
                FpVar::new_witness(cs.clone(), || Ok(self.comm[1]))?,
                FpVar::new_witness(cs.clone(), || Ok(self.comm[2]))?,
                FpVar::new_witness(cs.clone(), || Ok(self.comm[3]))?,
            ],
        })
    }
}

pub struct IC {
    pub comm: [FpVar<F>; 4],
}

impl IC {
    pub fn zero(cs: ConstraintSystemRef<F>) -> Result<IC, SynthesisError> {
        Ok(IC {
            comm: [
                FpVar::new_witness(cs.clone(), || Ok(F::from(0)))?,
                FpVar::new_witness(cs.clone(), || Ok(F::from(0)))?,
                FpVar::new_witness(cs.clone(), || Ok(F::from(0)))?,
                FpVar::new_witness(cs.clone(), || Ok(F::from(0)))?,
            ],
        })
    }

    pub fn increment(
        &mut self,
        a: &AllocatedAddress,
        vt: &MemOpAllocated<F>,
        unsound_make_nop: bool,
    ) -> Result<(), SynthesisError> {
        if !unsound_make_nop {
            let cs = self.comm.cs();

            let hash_to_field = compress(&array::from_fn(|i| {
                if i == 0 {
                    a.addr.clone()
                } else if i == 1 {
                    a.tag.clone()
                } else if i == 2 {
                    vt.timestamp.clone()
                } else {
                    vt.values
                        .get(i - 3)
                        .cloned()
                        .unwrap_or_else(|| FpVar::new_witness(cs.clone(), || Ok(F::ZERO)).unwrap())
                }
            }))?;

            let concat = array::from_fn(|i| {
                if i < 4 {
                    hash_to_field[i].clone()
                } else {
                    self.comm[i - 4].clone()
                }
            });

            self.comm = compress(&concat)?;
        }

        Ok(())
    }

    pub fn values(&self) -> ICPlain {
        ICPlain {
            comm: [
                self.comm[0].value().unwrap(),
                self.comm[1].value().unwrap(),
                self.comm[2].value().unwrap(),
                self.comm[3].value().unwrap(),
            ],
        }
    }
}
