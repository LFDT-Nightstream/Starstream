use super::Address;
use super::IVCMemory;
use super::IVCMemoryAllocated;
use crate::memory::AllocatedAddress;
use ark_ff::PrimeField;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::ConstraintSystemRef;
use ark_relations::gr1cs::SynthesisError;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::marker::PhantomData;

#[allow(dead_code)]
pub struct DummyMemory<F> {
    pub(crate) phantom: PhantomData<F>,
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) init: BTreeMap<Address<u64>, Vec<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, &'static str)>,
}

impl<F: PrimeField> IVCMemory<F> for DummyMemory<F> {
    type Allocator = DummyMemoryConstraints<F>;

    type Params = ();

    fn new(_params: Self::Params) -> Self {
        DummyMemory {
            phantom: PhantomData,
            reads: BTreeMap::default(),
            writes: BTreeMap::default(),
            init: BTreeMap::default(),
            mems: BTreeMap::default(),
        }
    }

    fn register_mem(&mut self, tag: u64, size: u64, debug_name: &'static str) {
        self.mems.insert(tag, (size, debug_name));
    }

    fn init(&mut self, address: Address<u64>, values: Vec<F>) {
        self.init.insert(address, values.clone());
    }

    fn conditional_read(&mut self, cond: bool, address: Address<u64>) -> Vec<F> {
        let reads = self.reads.entry(address.clone()).or_default();

        if cond {
            let last = self
                .writes
                .get(&address)
                .and_then(|writes| writes.back().cloned())
                .unwrap_or_else(|| self.init.get(&address).unwrap().clone());

            reads.push_back(last.clone());

            last
        } else {
            let mem_value_size = self.mems.get(&address.tag).unwrap().0;
            std::iter::repeat_n(F::from(0), mem_value_size as usize).collect()
        }
    }

    fn conditional_write(&mut self, cond: bool, address: Address<u64>, values: Vec<F>) {
        assert_eq!(
            self.mems.get(&address.tag).unwrap().0 as usize,
            values.len(),
            "write doesn't match mem value size"
        );

        if cond {
            self.writes.entry(address).or_default().push_back(values);
        }
    }

    fn required_steps(&self) -> usize {
        0
    }

    fn constraints(self) -> Self::Allocator {
        DummyMemoryConstraints {
            cs: None,
            reads: self.reads,
            writes: self.writes,
            mems: self.mems,
        }
    }
}

#[allow(dead_code)]
pub struct DummyMemoryConstraints<F: PrimeField> {
    pub(crate) cs: Option<ConstraintSystemRef<F>>,
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,

    pub(crate) mems: BTreeMap<u64, (u64, &'static str)>,
}

impl<F: PrimeField> IVCMemoryAllocated<F> for DummyMemoryConstraints<F> {
    fn start_step(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        self.cs.replace(cs);

        Ok(())
    }

    fn finish_step(&mut self, _is_last_step: bool) -> Result<(), SynthesisError> {
        self.cs = None;
        Ok(())
    }

    fn get_cs(&self) -> ConstraintSystemRef<F> {
        self.cs.as_ref().unwrap().clone()
    }

    fn conditional_read(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let _guard = tracing::debug_span!("conditional_read").entered();

        let mem = self.mems.get(&address.tag_value()).copied().unwrap();

        if cond.value().unwrap() {
            let address = Address {
                addr: address.address_value(),
                tag: address.tag_value(),
            };

            let vals = self.reads.get_mut(&address).unwrap();

            let v = vals.pop_front().unwrap().clone();

            let vals = v
                .into_iter()
                .map(|v| FpVar::new_witness(self.get_cs(), || Ok(v)).unwrap())
                .collect::<Vec<_>>();

            tracing::debug!(
                "read {:?} at address {} in segment {}",
                vals.iter()
                    .map(|v| v.value().unwrap().into_bigint())
                    .collect::<Vec<_>>(),
                address.addr,
                mem.1,
            );

            Ok(vals)
        } else {
            let vals = std::iter::repeat_with(|| {
                FpVar::new_witness(self.get_cs(), || Ok(F::from(0))).unwrap()
            })
            .take(mem.0 as usize);

            Ok(vals.collect())
        }
    }

    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError> {
        let _guard = tracing::debug_span!("conditional_write").entered();

        if cond.value().unwrap() {
            let address = Address {
                addr: address.address_value(),
                tag: address.tag_value(),
            };

            let writes = self.writes.get_mut(&address).unwrap();

            let expected_vals = writes.pop_front().unwrap().clone();

            for ((_, val), expected) in vals.iter().enumerate().zip(expected_vals.iter()) {
                assert_eq!(val.value().unwrap(), *expected);
            }

            let mem = self.mems.get(&address.tag).copied().unwrap();

            assert_eq!(
                mem.0 as usize,
                vals.len(),
                "write doesn't match mem value size"
            );

            tracing::debug!(
                "write values {:?} at address {} in segment {}",
                vals.iter()
                    .map(|v| v.value().unwrap().into_bigint())
                    .collect::<Vec<_>>(),
                address.addr,
                mem.1,
            );
        }

        Ok(())
    }
}
