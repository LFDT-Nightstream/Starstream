use ark_ff::PrimeField;
use ark_r1cs_std::{GR1CSVar, alloc::AllocVar, fields::fp::FpVar, prelude::Boolean};
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use std::{
    collections::{BTreeMap, VecDeque},
    marker::PhantomData,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone)]
pub struct Address<F> {
    pub addr: F,
    pub tag: u64,
}

pub trait IVCMemory<F: PrimeField> {
    type Allocator: IVCMemoryAllocated<F>;
    type Params;

    fn new(info: Self::Params) -> Self;

    fn register_mem(&mut self, tag: u64, size: u64);

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

pub struct DummyMemory<F> {
    phantom: PhantomData<F>,
    reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    init: BTreeMap<Address<u64>, Vec<F>>,

    mems: BTreeMap<u64, u64>,
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

    fn register_mem(&mut self, tag: u64, size: u64) {
        self.mems.insert(tag, size);
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
            vec![F::from(0), F::from(0), F::from(0), F::from(0)]
        }
    }

    fn conditional_write(&mut self, cond: bool, address: Address<u64>, values: Vec<F>) {
        if cond {
            self.writes.entry(address).or_default().push_back(values);
        }
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

pub struct DummyMemoryConstraints<F: PrimeField> {
    cs: Option<ConstraintSystemRef<F>>,
    reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,

    mems: BTreeMap<u64, u64>,
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
        address: &Address<FpVar<F>>,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        if cond.value().unwrap() {
            let address = Address {
                addr: address.addr.value().unwrap().into_bigint().as_ref()[0],
                tag: address.tag,
            };

            let vals = self.reads.get_mut(&address).unwrap();

            let v = vals.pop_front().unwrap().clone();

            let vals = v
                .into_iter()
                .map(|v| FpVar::new_witness(self.get_cs(), || Ok(v)).unwrap())
                .collect::<Vec<_>>();

            Ok(vals)
        } else {
            Ok(std::iter::repeat_n(
                FpVar::new_constant(self.get_cs(), F::from(0)).unwrap(),
                self.mems.get(&address.tag).copied().unwrap() as usize,
            )
            .collect())
        }
    }

    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError> {
        if cond.value().unwrap() {
            let address = Address {
                addr: address.addr.value().unwrap().into_bigint().as_ref()[0],
                tag: address.tag,
            };

            let writes = self.writes.get_mut(&address).unwrap();

            let expected_vals = writes.pop_front().unwrap().clone();

            for ((_, val), expected) in vals.iter().enumerate().zip(expected_vals.iter()) {
                assert_eq!(val.value().unwrap(), *expected);
            }
        }

        Ok(())
    }
}
