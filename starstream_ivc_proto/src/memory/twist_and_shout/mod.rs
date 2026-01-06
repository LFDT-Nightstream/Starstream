use super::Address;
use super::IVCMemory;
use super::IVCMemoryAllocated;
use crate::memory::AllocatedAddress;
use crate::memory::MemType;
use ark_ff::PrimeField;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::eq::EqGadget as _;
use ark_r1cs_std::fields::FieldVar as _;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::ConstraintSystemRef;
use ark_relations::gr1cs::SynthesisError;
use neo_vm_trace::Shout;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::marker::PhantomData;

/// CPU binding mapping for Shout protocol
#[derive(Debug, Clone)]
pub struct ShoutCpuBinding {
    /// Witness column index for the lookup flag
    pub has_lookup: usize,
    /// Witness column index for the lookup address
    pub addr: usize,
    /// Witness column index for the lookup value
    pub val: usize,
}

/// Event representing a shout lookup operation
#[derive(Debug, Clone)]
pub struct ShoutEvent {
    pub shout_id: u32,
    pub key: u64,
    pub value: u64,
}

#[derive(Clone)]
pub struct TSMemory<F> {
    pub(crate) phantom: PhantomData<F>,
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) init: BTreeMap<Address<u64>, Vec<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,

    /// Captured shout events for witness generation, organized by address
    pub(crate) shout_events: BTreeMap<Address<u64>, VecDeque<ShoutEvent>>,
}

/// Initial ROM tables computed by TSMemory
pub struct TSMemInitTables<F> {
    pub mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,
    pub rom_sizes: BTreeMap<u64, usize>,
    pub init: BTreeMap<u64, Vec<F>>,
}

/// Layout/bindings computed by TSMemoryConstraints
pub struct TSMemLayouts {
    pub shout_bindings: BTreeMap<u64, Vec<ShoutCpuBinding>>,
}

#[derive(Clone, Copy)]
pub struct Lanes(pub usize);

impl Default for Lanes {
    fn default() -> Self {
        Self(1)
    }
}

impl<F: PrimeField> IVCMemory<F> for TSMemory<F> {
    type Allocator = TSMemoryConstraints<F>;
    type Params = ();

    fn new(_params: Self::Params) -> Self {
        TSMemory {
            phantom: PhantomData,
            reads: BTreeMap::default(),
            writes: BTreeMap::default(),
            init: BTreeMap::default(),
            mems: BTreeMap::default(),
            shout_events: BTreeMap::default(),
        }
    }

    fn register_mem_with_lanes(
        &mut self,
        tag: u64,
        size: u64,
        mem_type: MemType,
        lanes: Lanes,
        debug_name: &'static str,
    ) {
        self.mems.insert(tag, (size, lanes, mem_type, debug_name));
    }

    fn init(&mut self, address: Address<u64>, values: Vec<F>) {
        self.init.insert(address, values.clone());
    }

    fn conditional_read(&mut self, cond: bool, address: Address<u64>) -> Vec<F> {
        if let Some(&(_, _, MemType::Rom, _)) = self.mems.get(&address.tag) {
            // For ROM memories, we need to capture the shout event
            if cond {
                // Get the value from init (ROM is read-only)
                let value = self.init.get(&address).unwrap().clone();

                // Record this as a shout event for witness generation
                let shout_event = ShoutEvent {
                    shout_id: address.tag as u32,
                    key: address.addr,
                    value: value[0].into_bigint().as_ref()[0], // Convert F to u64
                };
                let shout_events = self.shout_events.entry(address.clone()).or_default();
                shout_events.push_back(shout_event);

                value
            } else {
                // No lookup, return zero
                let mem_value_size = self.mems.get(&address.tag).unwrap().0;
                std::iter::repeat_n(F::from(0), mem_value_size as usize).collect()
            }
        } else {
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
        TSMemoryConstraints {
            cs: None,
            reads: self.reads,
            writes: self.writes,
            mems: self.mems,
            shout_events: self.shout_events,
            shout_bindings: BTreeMap::new(),
            step_events: vec![],
            is_first_step: true,
        }
    }
}

impl<F: PrimeField> TSMemory<F> {
    pub fn init_tables(&self) -> TSMemInitTables<F> {
        let mut rom_sizes = BTreeMap::new();
        let mut init = BTreeMap::new();

        for (address, val) in &self.init {
            if let Some((_, _, MemType::Rom, _)) = self.mems.get(&address.tag) {
                *rom_sizes.entry(address.tag).or_insert(0) += 1;
            }
            init.entry(address.tag).or_insert(vec![]).push(val[0]);
        }

        TSMemInitTables {
            mems: self.mems.clone(),
            rom_sizes,
            init,
        }
    }
}

impl Shout<u64> for TSMemory<crate::F> {
    fn lookup(&mut self, shout_id: neo_vm_trace::ShoutId, key: u64) -> u64 {
        let value = self
            .init
            .get(&Address {
                tag: shout_id.0 as u64,
                addr: key,
            })
            .unwrap()
            .clone();

        value[0].into_bigint().0[0]
    }
}

pub struct TSMemoryConstraints<F: PrimeField> {
    pub(crate) cs: Option<ConstraintSystemRef<F>>,
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,

    pub(crate) mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,

    /// Captured shout events for witness generation, organized by address
    pub(crate) shout_events: BTreeMap<Address<u64>, VecDeque<ShoutEvent>>,

    /// Captured shout CPU bindings with actual witness indices
    pub(crate) shout_bindings: BTreeMap<u64, Vec<ShoutCpuBinding>>,

    step_events: Vec<ShoutEvent>,

    /// We only need to compute ShoutBinding layouts once
    is_first_step: bool,
}

impl<F: PrimeField> TSMemoryConstraints<F> {
    /// Generate the memory layouts with actual witness indices
    pub fn ts_mem_layouts(&self) -> TSMemLayouts {
        TSMemLayouts {
            shout_bindings: self.shout_bindings.clone(),
        }
    }

    /// Allocate witness variables for shout protocol based on address
    pub fn allocate_shout_witnesses(
        &mut self,
        address: &Address<u64>,
    ) -> Result<(FpVar<F>, FpVar<F>, FpVar<F>), SynthesisError> {
        let cs = self.get_cs();

        let (has_lookup_val, addr_val, val_val) = {
            let event = self
                .shout_events
                .get_mut(dbg!(address))
                .unwrap()
                .pop_front()
                .unwrap();

            self.step_events.push(event.clone());

            (F::from(1), F::from(event.key), F::from(event.value))
        };

        let has_lookup = FpVar::new_witness(cs.clone(), || Ok(has_lookup_val))?;
        let addr = FpVar::new_witness(cs.clone(), || Ok(addr_val))?;
        let val = FpVar::new_witness(cs.clone(), || Ok(val_val))?;

        Ok((has_lookup, addr, val))
    }
}

impl<F: PrimeField> IVCMemoryAllocated<F> for TSMemoryConstraints<F> {
    type FinishStepPayload = Vec<ShoutEvent>;

    fn start_step(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        self.cs.replace(cs);

        Ok(())
    }

    fn finish_step(
        &mut self,
        _is_last_step: bool,
    ) -> Result<Self::FinishStepPayload, SynthesisError> {
        self.cs = None;

        let mut step_events = vec![];
        std::mem::swap(&mut step_events, &mut self.step_events);

        self.is_first_step = false;

        Ok(step_events)
    }

    fn get_cs(&self) -> ConstraintSystemRef<F> {
        self.cs.as_ref().unwrap().clone()
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn conditional_read(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let _guard = tracing::debug_span!("conditional_read").entered();

        let mem = self.mems.get(&address.tag_value()).copied().unwrap();

        // Check if this is a ROM memory that should use Shout protocol
        if mem.2 == MemType::Rom {
            // For ROM memories, allocate witnesses using Shout protocol
            let address_val = Address {
                addr: address.address_value(),
                tag: address.tag_value(),
            };

            let cs = self.get_cs();
            let (has_lookup, addr_witness, val_witness) = if cond.value()? {
                self.allocate_shout_witnesses(&address_val)?
            } else {
                (
                    FpVar::new_witness(cs.clone(), || Ok(F::ZERO))?,
                    FpVar::new_witness(cs.clone(), || Ok(F::ZERO))?,
                    FpVar::new_witness(cs.clone(), || Ok(F::ZERO))?,
                )
            };

            let tag = address.tag_value();

            if let Some(&(_, _lanes, MemType::Rom, _)) = self.mems.get(&tag)
                && self.is_first_step
            {
                let binding = ShoutCpuBinding {
                    has_lookup: cs.num_witness_variables() - 3,
                    addr: cs.num_witness_variables() - 2,
                    val: cs.num_witness_variables() - 1,
                };

                self.shout_bindings.entry(tag).or_default().push(binding);
                // TODO: maybe check that size is <= lanes
            }

            tracing::debug!(
                "read ({}) {:?} at address {} in segment {}",
                cond.value()?,
                val_witness.value()?,
                address_val.addr,
                mem.2,
            );

            // Enforce that has_lookup matches the condition
            FpVar::from(cond.clone()).enforce_equal(&has_lookup)?;

            // Enforce that addr_witness matches the address when lookup is active
            let addr_fp =
                FpVar::new_witness(self.get_cs(), || Ok(F::from(address.address_value())))?;
            let addr_constraint = cond.select(&addr_fp, &FpVar::zero())?;
            addr_witness.enforce_equal(&addr_constraint)?;

            // Return the value witness as a single-element vector
            Ok(vec![val_witness])
        } else {
            // Existing logic for RAM memories
            if cond.value().unwrap() {
                let address_val = Address {
                    addr: address.address_value(),
                    tag: address.tag_value(),
                };

                let vals = self.reads.get_mut(&address_val).unwrap();
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
                    address_val.addr,
                    mem.2,
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
                mem.2,
            );
        }

        Ok(())
    }
}
