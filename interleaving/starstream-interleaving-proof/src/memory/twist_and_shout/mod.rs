use super::Address;
use super::IVCMemory;
use super::IVCMemoryAllocated;
use crate::circuit::MemoryTag;
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
use neo_vm_trace::{Shout, Twist, TwistId, TwistOpKind};
use std::collections::BTreeMap;
use std::collections::VecDeque;

pub const TWIST_DEBUG_FILTER: &[u32] = &[
    MemoryTag::ExpectedInput as u32,
    MemoryTag::ExpectedResumer as u32,
    MemoryTag::Activation as u32,
    MemoryTag::Counters as u32,
    MemoryTag::Initialized as u32,
    MemoryTag::Finalized as u32,
    MemoryTag::DidBurn as u32,
    MemoryTag::Ownership as u32,
    MemoryTag::Init as u32,
    MemoryTag::RefArena as u32,
    MemoryTag::HandlerStackArenaProcess as u32,
    MemoryTag::HandlerStackArenaNextPtr as u32,
    MemoryTag::HandlerStackHeads as u32,
    MemoryTag::TraceCommitments as u32,
];

#[derive(Debug, Clone)]
pub struct ShoutCpuBinding {
    pub has_lookup: usize,
    pub addr: usize,
    pub val: usize,
}

#[derive(Debug, Clone)]
pub struct TwistCpuBinding {
    pub ra: usize,
    pub has_read: usize,
    pub rv: usize,
    pub wa: usize,
    pub has_write: usize,
    pub wv: usize,
}

#[derive(Debug, Clone, Default)]
pub struct PartialTwistCpuBinding {
    pub ra: Option<usize>,
    pub has_read: Option<usize>,
    pub rv: Option<usize>,
    pub wa: Option<usize>,
    pub has_write: Option<usize>,
    pub wv: Option<usize>,
}

impl PartialTwistCpuBinding {
    pub fn to_complete(&self) -> TwistCpuBinding {
        TwistCpuBinding {
            ra: self.ra.unwrap(),
            has_read: self.has_read.unwrap(),
            rv: self.rv.unwrap(),
            wa: self.wa.unwrap(),
            has_write: self.has_write.unwrap(),
            wv: self.wv.unwrap(),
        }
    }
}

/// Event representing a shout lookup operation
#[derive(Debug, Clone)]
pub struct ShoutEvent {
    pub shout_id: u32,
    pub key: u64,
    pub value: u64,
}

/// Event representing a twist memory operation
#[derive(Debug, Clone)]
pub struct TwistEvent {
    pub twist_id: u32,
    pub addr: u64,
    pub val: u64,
    pub op: TwistOpKind,
    pub cond: bool,
    pub lane: Option<u32>,
}

#[derive(Clone)]
pub struct TSMemory<F> {
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<Vec<F>>>,
    pub(crate) init: BTreeMap<Address<u64>, Vec<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,

    /// Captured shout events for witness generation, organized by address
    pub(crate) shout_events: BTreeMap<Address<u64>, VecDeque<ShoutEvent>>,
    /// Captured twist events for witness generation, organized by address
    pub(crate) twist_events: BTreeMap<Address<u64>, VecDeque<TwistEvent>>,

    pub(crate) current_step_read_lanes: BTreeMap<u64, usize>,
    pub(crate) current_step_write_lanes: BTreeMap<u64, usize>,
}

/// Initial ROM tables computed by TSMemory
pub struct TSMemInitTables<F> {
    pub mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,
    pub rom_sizes: BTreeMap<u64, usize>,
    pub init: BTreeMap<u64, BTreeMap<u64, F>>,
}

/// Layout/bindings computed by TSMemoryConstraints
pub struct TSMemLayouts {
    pub shout_bindings: BTreeMap<u64, Vec<ShoutCpuBinding>>,
    pub twist_bindings: BTreeMap<u64, Vec<TwistCpuBinding>>,
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
            reads: BTreeMap::default(),
            writes: BTreeMap::default(),
            init: BTreeMap::default(),
            mems: BTreeMap::default(),
            shout_events: BTreeMap::default(),
            twist_events: BTreeMap::default(),
            current_step_read_lanes: BTreeMap::default(),
            current_step_write_lanes: BTreeMap::default(),
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
        *self.current_step_read_lanes.entry(address.tag).or_default() += 1;

        if let Some(&(_, _, MemType::Rom, _)) = self.mems.get(&address.tag) {
            if cond {
                let value = self.init.get(&address).unwrap().clone();
                let shout_event = ShoutEvent {
                    shout_id: address.tag as u32,
                    key: address.addr,
                    value: value[0].into_bigint().as_ref()[0],
                };
                let shout_events = self.shout_events.entry(address.clone()).or_default();
                shout_events.push_back(shout_event);
                value
            } else {
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

                let twist_event = TwistEvent {
                    twist_id: address.tag as u32,
                    addr: address.addr,
                    val: last[0].into_bigint().as_ref()[0],
                    op: TwistOpKind::Read,
                    cond,
                    lane: None,
                };

                self.twist_events
                    .entry(address.clone())
                    .or_default()
                    .push_back(twist_event);

                last
            } else {
                let mem_value_size = self.mems.get(&address.tag).unwrap().0;
                std::iter::repeat_n(F::from(0), mem_value_size as usize).collect()
            }
        }
    }

    fn conditional_write(&mut self, cond: bool, address: Address<u64>, values: Vec<F>) {
        *self
            .current_step_write_lanes
            .entry(address.tag)
            .or_default() += 1;

        assert_eq!(
            self.mems.get(&address.tag).unwrap().0 as usize,
            values.len(),
            "write doesn't match mem value size"
        );
        if cond {
            self.writes
                .entry(address.clone())
                .or_default()
                .push_back(values.clone());

            let twist_event = TwistEvent {
                twist_id: address.tag as u32,
                addr: address.addr,
                val: values[0].into_bigint().as_ref()[0],
                op: TwistOpKind::Write,
                cond,
                lane: None,
            };

            self.twist_events
                .entry(address)
                .or_default()
                .push_back(twist_event);
        }
    }

    fn finish_step(&mut self) {
        let mut current_step_read_lanes = BTreeMap::new();
        let mut current_step_write_lanes = BTreeMap::new();

        std::mem::swap(
            &mut current_step_read_lanes,
            &mut self.current_step_read_lanes,
        );

        std::mem::swap(
            &mut current_step_write_lanes,
            &mut self.current_step_write_lanes,
        );

        for (tag, reads) in current_step_read_lanes {
            if let Some(writes) = current_step_write_lanes.get(&tag) {
                assert_eq!(
                    reads, *writes,
                    "each step must have the same number of (conditional) reads and writes per memory"
                );
            }

            if let Some(entry) = self.mems.get_mut(&tag) {
                entry.1 = Lanes(reads);
            }
        }

        self.current_step_read_lanes.clear();
        self.current_step_write_lanes.clear();
    }

    fn required_steps(&self) -> usize {
        0
    }

    fn constraints(self) -> Self::Allocator {
        TSMemoryConstraints {
            cs: None,
            mems: self.mems,
            shout_events: self.shout_events,
            twist_events: self.twist_events,
            shout_bindings: BTreeMap::new(),
            partial_twist_bindings: BTreeMap::new(),
            step_events_shout: vec![],
            step_events_twist: vec![],
            is_first_step: true,
            write_lanes: BTreeMap::new(),
            read_lanes: BTreeMap::new(),
        }
    }
}

impl<F: PrimeField> TSMemory<F> {
    pub fn init_tables(&self) -> TSMemInitTables<F> {
        let mut rom_sizes = BTreeMap::new();
        let mut init = BTreeMap::<u64, BTreeMap<u64, F>>::new();

        for (address, val) in &self.init {
            let is_rom = if let Some((_, _, MemType::Rom, _)) = self.mems.get(&address.tag) {
                *rom_sizes.entry(address.tag).or_insert(0) += 1;

                true
            } else {
                false
            };

            if is_rom || TWIST_DEBUG_FILTER.contains(&(address.tag as u32)) {
                init.entry(address.tag)
                    .or_default()
                    .insert(address.addr, val[0]);
            }
        }

        TSMemInitTables {
            mems: self.mems.clone(),
            rom_sizes,
            init,
        }
    }

    pub fn split(self) -> (TSMemoryConstraints<F>, TracedShout<F>, TracedTwist) {
        let mems = self.mems.clone();

        let (init, twist_events, _mems, shout_events) =
            (self.init, self.twist_events, self.mems, self.shout_events);

        let traced_shout = TracedShout { init };
        let traced_twist = TracedTwist {
            twist_events: twist_events.clone(),
        };

        let constraints = TSMemoryConstraints {
            cs: None,
            mems,
            shout_events,
            twist_events,
            shout_bindings: BTreeMap::new(),
            partial_twist_bindings: BTreeMap::new(),
            step_events_shout: vec![],
            step_events_twist: vec![],
            is_first_step: true,
            write_lanes: BTreeMap::new(),
            read_lanes: BTreeMap::new(),
        };

        (constraints, traced_shout, traced_twist)
    }
}

pub struct TracedShout<F> {
    pub init: BTreeMap<Address<u64>, Vec<F>>,
}

impl<F: PrimeField> Shout<u64> for TracedShout<F> {
    fn lookup(&mut self, shout_id: neo_vm_trace::ShoutId, key: u64) -> u64 {
        let value = self
            .init
            .get(&Address {
                tag: shout_id.0 as u64,
                addr: key,
            })
            .unwrap()
            .clone();

        value[0].into_bigint().as_ref()[0]
    }
}

pub struct TracedTwist {
    pub twist_events: BTreeMap<Address<u64>, VecDeque<TwistEvent>>,
}

impl Twist<u64, u64> for TracedTwist {
    fn load(&mut self, id: TwistId, addr: u64) -> u64 {
        let address = Address {
            tag: id.0 as u64,
            addr,
        };

        let event = self
            .twist_events
            .get_mut(&address)
            .unwrap()
            .pop_front()
            .unwrap();

        assert_eq!(event.op, TwistOpKind::Read);
        event.val
    }

    fn store(&mut self, id: TwistId, addr: u64, val: u64) {
        let address = Address {
            tag: id.0 as u64,
            addr,
        };
        let event = self
            .twist_events
            .get_mut(&address)
            .unwrap()
            .pop_front()
            .unwrap();

        assert_eq!(event.op, TwistOpKind::Write);
        assert_eq!(event.val, val);
    }
}

pub struct TSMemoryConstraints<F: PrimeField> {
    pub(crate) cs: Option<ConstraintSystemRef<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, Lanes, MemType, &'static str)>,

    pub(crate) shout_events: BTreeMap<Address<u64>, VecDeque<ShoutEvent>>,
    pub(crate) twist_events: BTreeMap<Address<u64>, VecDeque<TwistEvent>>,

    pub(crate) shout_bindings: BTreeMap<u64, Vec<ShoutCpuBinding>>,
    pub(crate) partial_twist_bindings: BTreeMap<u64, Vec<PartialTwistCpuBinding>>,

    step_events_shout: Vec<ShoutEvent>,
    step_events_twist: Vec<TwistEvent>,

    is_first_step: bool,

    write_lanes: BTreeMap<u32, u32>,
    read_lanes: BTreeMap<u32, u32>,
}

impl<F: PrimeField> TSMemoryConstraints<F> {
    pub fn ts_mem_layouts(&self) -> TSMemLayouts {
        let mut twist_bindings = BTreeMap::new();

        for (tag, partials) in &self.partial_twist_bindings {
            let mut complete = Vec::new();
            for p in partials {
                complete.push(p.to_complete());
            }

            if TWIST_DEBUG_FILTER.contains(&(*tag as u32)) {
                twist_bindings.insert(*tag, complete);
            }
        }

        TSMemLayouts {
            shout_bindings: self.shout_bindings.clone(),
            twist_bindings,
        }
    }

    pub fn get_shout_traced_values(
        &mut self,
        address: &Address<u64>,
    ) -> Result<(F, F, F), SynthesisError> {
        let (has_lookup_val, addr_val, val_val) = {
            let event = self
                .shout_events
                .get_mut(address)
                .unwrap()
                .pop_front()
                .unwrap();

            self.step_events_shout.push(event.clone());

            (F::from(1), F::from(event.key), F::from(event.value))
        };

        Ok((has_lookup_val, addr_val, val_val))
    }

    pub fn get_twist_traced_values(
        &mut self,
        address: &Address<u64>,
        lane: u32,
        kind: TwistOpKind,
    ) -> Result<(F, F, F), SynthesisError> {
        let (ra, rv) = {
            let mut event = self
                .twist_events
                .get_mut(address)
                .unwrap()
                .pop_front()
                .unwrap();

            event.lane.replace(lane);

            assert_eq!(event.op, kind);

            if TWIST_DEBUG_FILTER.contains(&event.twist_id) {
                self.step_events_twist.push(event.clone());
            }

            (F::from(event.addr), F::from(event.val))
        };

        Ok((F::one(), ra, rv))
    }
}

impl<F: PrimeField> TSMemoryConstraints<F> {
    fn get_next_read_lane(&mut self, twist_id: u64) -> u32 {
        *self
            .read_lanes
            .entry(twist_id.try_into().unwrap())
            .and_modify(|l| *l += 1)
            .or_insert(0)
    }

    fn get_next_write_lane(&mut self, twist_id: u64) -> u32 {
        *self
            .write_lanes
            .entry(twist_id.try_into().unwrap())
            .and_modify(|l| *l += 1)
            .or_insert(0)
    }

    fn update_partial_twist_bindings_read(&mut self, tag: u64, base_index: usize, lane: usize) {
        let bindings = self.partial_twist_bindings.entry(tag).or_default();

        while bindings.len() <= lane {
            bindings.push(PartialTwistCpuBinding::default());
        }

        let b = &mut bindings[lane];
        b.has_read = Some(base_index);
        b.ra = Some(base_index + 1);
        b.rv = Some(base_index + 2);
    }

    fn update_partial_twist_bindings_write(&mut self, tag: u64, base_index: usize, lane: usize) {
        let bindings = self.partial_twist_bindings.entry(tag).or_default();

        while bindings.len() <= lane {
            bindings.push(PartialTwistCpuBinding::default());
        }

        let b = &mut bindings[lane];
        b.has_write = Some(base_index);
        b.wa = Some(base_index + 1);
        b.wv = Some(base_index + 2);
    }

    fn conditional_read_rom(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let address_val = Address {
            addr: address.address_value(),
            tag: address.tag_value(),
        };

        let cs = self.get_cs();

        let base_index = cs.num_witness_variables();

        let (has_lookup_val, addr_witness_val, val_witness_val) = if cond.value()? {
            self.get_shout_traced_values(&address_val)?
        } else {
            (F::ZERO, F::from(address.address_value()), F::ZERO)
        };

        let has_lookup = FpVar::new_witness(cs.clone(), || Ok(has_lookup_val))?;
        let addr_witness = FpVar::new_witness(cs.clone(), || Ok(addr_witness_val))?;
        let val_witness = FpVar::new_witness(cs.clone(), || Ok(val_witness_val))?;

        let tag = address.tag_value();

        if let Some(&(_, _lanes, MemType::Rom, _)) = self.mems.get(&tag)
            && self.is_first_step
        {
            let binding = ShoutCpuBinding {
                has_lookup: base_index,
                addr: base_index + 1,
                val: base_index + 2,
            };
            self.shout_bindings.entry(tag).or_default().push(binding);
        }

        FpVar::from(cond.clone()).enforce_equal(&has_lookup)?;

        let addr_fp = FpVar::new_witness(self.get_cs(), || Ok(F::from(address.address_value())))?;
        addr_witness.enforce_equal(&addr_fp)?;

        Ok(vec![val_witness])
    }

    fn conditional_read_ram(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let twist_id = address.tag_value();
        let address_val = Address {
            addr: address.address_value(),
            tag: twist_id,
        };

        let cs = self.get_cs();
        let base_index = cs.num_witness_variables();

        let cond_val = cond.value()?;

        let lane = self.get_next_read_lane(twist_id);

        let (has_read_val, ra_val, rv_val) = if cond_val {
            self.get_twist_traced_values(&address_val, lane, TwistOpKind::Read)?
        } else {
            if TWIST_DEBUG_FILTER.contains(&(twist_id as u32)) {
                self.step_events_twist.push(TwistEvent {
                    twist_id: twist_id as u32,
                    addr: 0,
                    val: 0,
                    op: TwistOpKind::Write,
                    cond: cond_val,
                    lane: Some(lane),
                });
            }

            (F::ZERO, F::ZERO, F::ZERO)
        };

        let has_read = FpVar::new_witness(cs.clone(), || Ok(has_read_val))?;
        let ra = FpVar::new_witness(cs.clone(), || Ok(ra_val))?;
        let rv = FpVar::new_witness(cs.clone(), || Ok(rv_val))?;

        assert_eq!(cs.num_witness_variables(), base_index + 3);

        let tag = address.tag_value();

        if let Some(&(_, _lanes, MemType::Ram, _)) = self.mems.get(&tag)
            && self.is_first_step
            && TWIST_DEBUG_FILTER.contains(&(tag as u32))
        {
            self.update_partial_twist_bindings_read(tag, base_index, lane as usize);
        }

        FpVar::from(cond.clone()).enforce_equal(&has_read)?;

        let addr_fp = FpVar::new_witness(self.get_cs(), || Ok(F::from(address.address_value())))?;
        let addr_constraint = cond.select(&addr_fp, &FpVar::zero())?;
        ra.enforce_equal(&addr_constraint)?;

        Ok(vec![rv])
    }
}

impl<F: PrimeField> IVCMemoryAllocated<F> for TSMemoryConstraints<F> {
    type FinishStepPayload = (Vec<ShoutEvent>, Vec<TwistEvent>);

    fn start_step(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        self.cs.replace(cs.clone());

        Ok(())
    }

    fn finish_step(
        &mut self,
        _is_last_step: bool,
    ) -> Result<Self::FinishStepPayload, SynthesisError> {
        self.cs = None;

        let mut step_events_shout = vec![];
        std::mem::swap(&mut step_events_shout, &mut self.step_events_shout);

        let mut step_events_twist = vec![];
        std::mem::swap(&mut step_events_twist, &mut self.step_events_twist);

        self.is_first_step = false;

        self.read_lanes.clear();
        self.write_lanes.clear();

        Ok((step_events_shout, step_events_twist))
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

        if mem.2 == MemType::Rom {
            self.conditional_read_rom(cond, address)
        } else {
            self.conditional_read_ram(cond, address)
        }
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError> {
        let _guard = tracing::debug_span!("conditional_write").entered();

        let mem_tag = address.tag_value();
        let mem = self.mems.get(&mem_tag).copied().unwrap();

        if mem.2 != MemType::Ram {
            unreachable!("can't write to Rom memory");
        }

        if cond.value().unwrap() {
            assert_eq!(
                mem.0 as usize,
                vals.len(),
                "write doesn't match mem value size"
            );
        }

        let twist_id = address.tag_value();
        let address_cpu = Address {
            addr: address.address_value(),
            tag: twist_id,
        };

        let cs = self.get_cs();
        let base_index = cs.num_witness_variables();

        let cond_val = cond.value()?;

        let lane = self.get_next_write_lane(twist_id);

        let (has_write_val, wa_val, wv_val) = if cond_val {
            self.get_twist_traced_values(&address_cpu, lane, TwistOpKind::Write)?
        } else {
            if TWIST_DEBUG_FILTER.contains(&(twist_id as u32)) {
                self.step_events_twist.push(TwistEvent {
                    twist_id: twist_id as u32,
                    addr: 0,
                    val: 0,
                    op: TwistOpKind::Write,
                    cond: cond_val,
                    lane: Some(lane),
                });
            }

            (
                F::ZERO,
                F::from(address.address_value()),
                vals[0].value().unwrap_or(F::ZERO),
            )
        };

        let has_write = FpVar::new_witness(cs.clone(), || Ok(has_write_val))?;
        let wa = FpVar::new_witness(cs.clone(), || Ok(wa_val))?;
        let wv = FpVar::new_witness(cs.clone(), || Ok(wv_val))?;

        if self.is_first_step && TWIST_DEBUG_FILTER.contains(&(address_cpu.tag as u32)) {
            self.update_partial_twist_bindings_write(mem_tag, base_index, lane as usize);
        }

        FpVar::from(cond.clone()).enforce_equal(&has_write)?;

        let addr_fp = FpVar::new_witness(self.get_cs(), || Ok(F::from(address.address_value())))?;
        wa.enforce_equal(&addr_fp)?;
        wv.enforce_equal(&vals[0])?;

        Ok(())
    }
}
