use super::Address;
use super::ic::{IC, ICPlain};
use super::{MemOp, MemOpAllocated};
use crate::F;
use crate::memory::nebula::tracer::NebulaMemoryParams;
use crate::memory::{AllocatedAddress, IVCMemoryAllocated};
use ark_ff::Field;
use ark_ff::PrimeField;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::eq::EqGadget;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::ConstraintSystemRef;
use ark_relations::gr1cs::SynthesisError;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::iter::repeat_with;

pub struct NebulaMemoryConstraints<F: PrimeField> {
    pub(crate) cs: Option<ConstraintSystemRef<F>>,
    pub(crate) reads: BTreeMap<Address<u64>, VecDeque<MemOp<F>>>,
    pub(crate) writes: BTreeMap<Address<u64>, VecDeque<MemOp<F>>>,

    pub(crate) fs: BTreeMap<Address<u64>, MemOp<F>>,
    pub(crate) is: BTreeMap<Address<u64>, MemOp<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, &'static str)>,

    pub(crate) ic_rs_ws: ICPlain,
    pub(crate) ic_is_fs: ICPlain,

    pub(crate) step_ic_rs_ws: Option<IC>,
    pub(crate) step_ic_is_fs: Option<IC>,

    pub(crate) expected_rw_ws: ICPlain,
    pub(crate) expected_is_fs: ICPlain,

    pub(crate) ts: F,
    pub(crate) step_ts: Option<FpVar<F>>,

    pub(crate) current_step: usize,
    pub(crate) params: NebulaMemoryParams,
    pub(crate) scan_batch_size: usize,

    pub(crate) c0: F,
    pub(crate) c0_wire: Option<FpVar<F>>,
    pub(crate) c1: F,
    pub(crate) c1_wire: Option<FpVar<F>>,
    pub(crate) c1_powers_cache: Option<Vec<FpVar<F>>>,

    pub(crate) multiset_fingerprints: FingerPrintPreWires,
    pub(crate) fingerprint_wires: Option<FingerPrintWires>,

    pub(crate) scan_monotonic_last_addr: Option<Address<u64, u64>>,
    pub(crate) scan_monotonic_last_addr_wires: Option<AllocatedAddress>,

    pub(crate) debug_sets: Multisets,
}

#[derive(Default)]
pub struct Multisets {
    is: BTreeMap<Address<F>, MemOp<F>>,
    fs: BTreeMap<Address<F>, MemOp<F>>,
    rs: BTreeMap<Address<F>, MemOp<F>>,
    ws: BTreeMap<Address<F>, MemOp<F>>,
}

impl std::fmt::Debug for Multisets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format_set = |set: &BTreeMap<Address<F>, MemOp<F>>| {
            let entries: Vec<String> = set
                .iter()
                .map(|(addr, op)| format!("({}, {:?}, {})", addr.addr, op.values, op.timestamp))
                .collect();
            format!("[{}]", entries.join(", "))
        };

        writeln!(f, "\n")?;

        writeln!(f, "is: {}", format_set(&self.is))?;
        writeln!(f, "fs: {}", format_set(&self.fs))?;
        writeln!(f, "rs: {}", format_set(&self.rs))?;
        write!(f, "ws: {}", format_set(&self.ws))
    }
}

pub struct FingerPrintPreWires {
    pub is: F,
    pub fs: F,
    pub rs: F,
    pub ws: F,
}

impl FingerPrintPreWires {
    fn allocate(&self, cs: ConstraintSystemRef<F>) -> Result<FingerPrintWires, SynthesisError> {
        Ok(FingerPrintWires {
            is: FpVar::new_witness(cs.clone(), || Ok(self.is))?,
            fs: FpVar::new_witness(cs.clone(), || Ok(self.fs))?,
            rs: FpVar::new_witness(cs.clone(), || Ok(self.rs))?,
            ws: FpVar::new_witness(cs.clone(), || Ok(self.ws))?,
        })
    }

    fn check(&self) -> bool {
        let result = self.is * self.ws == self.fs * self.rs;

        if !result {
            tracing::error!(
                "multiset safety check failed: is={:?}, ws={:?}, fs={:?}, rs={:?}",
                self.is,
                self.ws,
                self.fs,
                self.rs
            );
        }

        result
    }
}

pub struct FingerPrintWires {
    pub is: FpVar<F>,
    pub fs: FpVar<F>,
    pub rs: FpVar<F>,
    pub ws: FpVar<F>,
}

impl FingerPrintWires {
    fn values(&self) -> Result<FingerPrintPreWires, SynthesisError> {
        Ok(FingerPrintPreWires {
            is: self.is.value()?,
            fs: self.fs.value()?,
            rs: self.rs.value()?,
            ws: self.ws.value()?,
        })
    }
}

impl IVCMemoryAllocated<F> for NebulaMemoryConstraints<F> {
    type FinishStepPayload = ();

    #[tracing::instrument(target = "gr1cs", skip(self, cs))]
    fn start_step(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        self.cs.replace(cs.clone());

        self.step_ic_rs_ws
            .replace(self.ic_rs_ws.allocate(cs.clone())?);

        self.step_ts
            .replace(FpVar::new_witness(cs.clone(), || Ok(self.ts))?);

        self.step_ic_is_fs
            .replace(self.ic_is_fs.allocate(cs.clone())?);

        self.fingerprint_wires
            .replace(self.multiset_fingerprints.allocate(cs.clone())?);

        self.c0_wire
            .replace(FpVar::new_witness(cs.clone(), || Ok(self.c0))?);

        self.c1_wire
            .replace(FpVar::new_witness(cs.clone(), || Ok(self.c1))?);

        // Precompute and cache c1 powers
        let max_segment_size = self.max_segment_size() as usize;
        let c1_wire = self.c1_wire.as_ref().unwrap();
        let mut c1_powers = Vec::with_capacity(max_segment_size);
        let mut c1_p = c1_wire.clone();
        for _ in 0..max_segment_size {
            c1_p *= c1_wire;
            c1_powers.push(c1_p.clone());
        }
        self.c1_powers_cache = Some(c1_powers);

        self.scan_monotonic_last_addr_wires.replace(
            self.scan_monotonic_last_addr
                .clone()
                .unwrap_or(Address { addr: 0, tag: 0 })
                .allocate(cs.clone())?,
        );

        self.scan(cs)?;

        Ok(())
    }

    #[tracing::instrument(target = "gr1cs", skip(self))]
    fn finish_step(&mut self, is_last_step: bool) -> Result<(), SynthesisError> {
        self.cs = None;
        self.c1_powers_cache = None;

        self.current_step += 1;

        self.ic_rs_ws = self.step_ic_rs_ws.take().unwrap().values();
        self.ic_is_fs = self.step_ic_is_fs.take().unwrap().values();

        self.multiset_fingerprints = self.fingerprint_wires.take().unwrap().values()?;

        self.scan_monotonic_last_addr
            .replace(self.scan_monotonic_last_addr_wires.take().unwrap().values());

        if is_last_step {
            assert!(
                self.ic_rs_ws
                    .comm
                    .iter()
                    .zip(self.expected_rw_ws.comm.iter())
                    .all(|(x, y)| x == y)
            );

            assert!(
                self.ic_is_fs
                    .comm
                    .iter()
                    .zip(self.expected_is_fs.comm.iter())
                    .all(|(x, y)| x == y)
            );

            for ops in self.reads.values() {
                assert!(ops.is_empty());
            }

            for ops in self.writes.values() {
                assert!(ops.is_empty());
            }

            if !self.multiset_fingerprints.check() {
                dbg!(&self.debug_sets);
                tracing::debug!(sets=?self.debug_sets);
                panic!("sanity check of multisets failed");
            }
        }

        Ok(())
    }

    fn get_cs(&self) -> ConstraintSystemRef<F> {
        self.cs.as_ref().unwrap().clone()
    }

    #[tracing::instrument(target = "gr1cs", skip(self, cond, address))]
    fn conditional_read(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let _guard = tracing::debug_span!("nebula_conditional_read").entered();

        // ts <- ts + 1
        self.inc_ts(cond)?;

        let mem = self.get_mem_info(address);
        let address_val = self.get_address_val(address);

        let rv = self.get_read_op(cond, &address_val, mem.0)?;
        let wv = self.get_write_op(cond, &address_val, mem.0)?;

        self.update_ic_with_ops(cond, address, &rv, &wv)?;

        tracing::debug!(
            "nebula {} read {:?} at address {} in segment {}",
            cond.value()?,
            rv.values
                .iter()
                .map(|v| v.value().unwrap().into_bigint())
                .collect::<Vec<_>>(),
            address_val.addr,
            mem.1,
        );

        Ok(rv.values)
    }

    #[tracing::instrument(target = "gr1cs", skip(self, cond, address, vals))]
    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
        vals: &[FpVar<F>],
    ) -> Result<(), SynthesisError> {
        let _guard = tracing::debug_span!("nebula_conditional_write").entered();

        let mem = self.get_mem_info(address);

        assert_eq!(
            mem.0 as usize,
            vals.len(),
            "write doesn't match mem value size"
        );

        // ts <- ts + 1
        self.inc_ts(cond)?;

        let address_val = self.get_address_val(address);

        let rv = self.get_read_op(cond, &address_val, mem.0)?;
        let wv = self.get_write_op(cond, &address_val, mem.0)?;

        self.update_ic_with_ops(cond, address, &rv, &wv)?;

        tracing::debug!(
            "nebula ({}) write values {:?} at address {} in segment {}",
            cond.value()?,
            vals.iter()
                .map(|v| v.value().unwrap().into_bigint())
                .collect::<Vec<_>>(),
            address_val.addr,
            mem.1,
        );

        for ((index, val), expected) in vals.iter().enumerate().zip(wv.values.iter()) {
            assert_eq!(
                val.value().unwrap(),
                expected.value().unwrap(),
                "write doesn't match expectation at index {index}."
            );
        }

        Ok(())
    }
}

impl NebulaMemoryConstraints<F> {
    fn inc_ts(&mut self, cond: &Boolean<F>) -> Result<(), SynthesisError> {
        let ts = self.step_ts.as_mut().unwrap();
        let ts_plus_one = &*ts + FpVar::Constant(F::from(1));
        *ts = cond.select(&ts_plus_one, ts)?;
        Ok(())
    }

    fn get_address_val(&self, address: &AllocatedAddress) -> Address<u64> {
        Address {
            addr: address.address_value(),
            tag: address.tag_value(),
        }
    }

    fn get_mem_info(&self, address: &AllocatedAddress) -> (u64, &'static str) {
        self.mems.get(&address.tag_value()).copied().unwrap()
    }

    fn get_read_op(
        &mut self,
        cond: &Boolean<F>,
        address_val: &Address<u64>,
        mem_size: u64,
    ) -> Result<MemOpAllocated<F>, SynthesisError> {
        let cs = self.get_cs();

        if cond.value()? {
            let a_reads = self.reads.get_mut(address_val).unwrap();
            a_reads
                .pop_front()
                .expect("no entry in read set")
                .allocate(cs, mem_size as usize)
        } else {
            MemOp::padding().allocate(cs, mem_size as usize)
        }
    }

    fn get_write_op(
        &mut self,
        cond: &Boolean<F>,
        address_val: &Address<u64>,
        mem_size: u64,
    ) -> Result<MemOpAllocated<F>, SynthesisError> {
        let cs = self.get_cs();

        if cond.value()? {
            let a_writes = self.writes.get_mut(address_val).unwrap();
            a_writes
                .pop_front()
                .expect("no entry in write set")
                .allocate(cs, mem_size as usize)
        } else {
            MemOp::padding().allocate(cs, mem_size as usize)
        }
    }

    #[tracing::instrument(target = "gr1cs", skip_all)]
    fn update_ic_with_ops(
        &mut self,
        cond: &Boolean<F>,
        address: &AllocatedAddress,
        rv: &MemOpAllocated<F>,
        wv: &MemOpAllocated<F>,
    ) -> Result<(), SynthesisError> {
        Self::hash_avt(
            cond,
            &mut self.fingerprint_wires.as_mut().unwrap().rs,
            self.c0_wire.as_ref().unwrap(),
            self.c1_powers_cache.as_ref().unwrap(),
            address,
            rv,
            &mut self.debug_sets.rs,
        )?;

        self.step_ic_rs_ws.as_mut().unwrap().increment(
            address,
            rv,
            self.params.unsound_disable_poseidon_commitment,
        )?;

        Self::hash_avt(
            cond,
            &mut self.fingerprint_wires.as_mut().unwrap().ws,
            self.c0_wire.as_ref().unwrap(),
            self.c1_powers_cache.as_ref().unwrap(),
            address,
            wv,
            &mut self.debug_sets.ws,
        )?;

        self.step_ic_rs_ws.as_mut().unwrap().increment(
            address,
            wv,
            self.params.unsound_disable_poseidon_commitment,
        )?;

        Ok(())
    }

    fn max_segment_size(&mut self) -> u64 {
        let max_segment_size = self.mems.values().map(|(sz, _)| sz).max().unwrap();
        *max_segment_size
    }

    fn scan(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        let address_padding = Address { addr: 0, tag: 0 };
        let mem_padding = MemOp::padding();
        let max_segment_size = self.max_segment_size() as usize;

        let _: () = for (addr, is_v) in self
            .is
            .iter()
            .skip(self.scan_batch_size * self.current_step)
            .chain(std::iter::repeat((&address_padding, &mem_padding)))
            // TODO: padding
            .take(self.scan_batch_size)
        {
            let fs_v = self.fs.get(addr).unwrap_or(&mem_padding);

            let address = addr.allocate(cs.clone())?;

            // ensure commitment is monotonic
            // so that it's not possible to insert an address twice
            //
            // we get disjoint ranges anyway because of the segments so we
            // can have different memories with different sizes, but each
            // segment is contiguous
            let last_addr = self.scan_monotonic_last_addr_wires.as_mut().unwrap();

            enforce_monotonic_commitment(&cs, &address, last_addr)?;

            *last_addr = address.clone();

            let is_entry = is_v.allocate(cs.clone(), max_segment_size)?;

            self.step_ic_is_fs.as_mut().unwrap().increment(
                &address,
                &is_entry,
                self.params.unsound_disable_poseidon_commitment,
            )?;

            let fs_entry = fs_v.allocate(cs.clone(), max_segment_size)?;

            self.step_ic_is_fs.as_mut().unwrap().increment(
                &address,
                &fs_entry,
                self.params.unsound_disable_poseidon_commitment,
            )?;

            Self::hash_avt(
                &Boolean::constant(true),
                &mut self.fingerprint_wires.as_mut().unwrap().is,
                self.c0_wire.as_ref().unwrap(),
                self.c1_powers_cache.as_ref().unwrap(),
                &address,
                &is_entry,
                &mut self.debug_sets.is,
            )?;

            Self::hash_avt(
                &Boolean::constant(true),
                &mut self.fingerprint_wires.as_mut().unwrap().fs,
                self.c0_wire.as_ref().unwrap(),
                self.c1_powers_cache.as_ref().unwrap(),
                &address,
                &fs_entry,
                &mut self.debug_sets.fs,
            )?;
        };
        Ok(())
    }

    fn hash_avt(
        cond: &Boolean<F>,
        wire: &mut FpVar<F>,
        c0: &FpVar<F>,
        c1_powers: &[FpVar<F>],
        address: &AllocatedAddress,
        vt: &MemOpAllocated<F>,
        debug_set: &mut BTreeMap<Address<F, u64>, MemOp<F>>,
    ) -> Result<(), SynthesisError> {
        let fingerprint = fingerprint_with_cached_powers(
            c0,
            c1_powers,
            &vt.timestamp,
            &address.addr,
            vt.values.as_ref(),
        )?;

        if cond.value()? {
            debug_set.insert(
                Address {
                    addr: address.addr.value()?,
                    tag: address.tag_value(),
                },
                MemOp {
                    values: vt.debug_values(),
                    timestamp: vt.timestamp.value()?.into_bigint().as_ref()[0],
                },
            );
        }

        *wire *= cond.select(&fingerprint, &FpVar::Constant(F::ONE))?;

        Ok(())
    }
}

fn enforce_monotonic_commitment(
    cs: &ConstraintSystemRef<F>,
    address: &Address<FpVar<F>, FpVar<F>>,
    last_addr: &mut Address<FpVar<F>, FpVar<F>>,
) -> Result<(), SynthesisError> {
    let same_segment = &address.tag.is_eq(&last_addr.tag)?;

    let next_segment = address
        .tag
        .is_eq(&(&last_addr.tag + FpVar::new_constant(cs.clone(), F::from(1))?))?;

    let is_padding = address
        .tag
        .is_eq(&FpVar::new_constant(cs.clone(), F::from(0))?)?;

    let segment_monotonic_constraint = same_segment | &next_segment | &is_padding;

    address.addr.conditional_enforce_equal(
        &(&last_addr.addr + FpVar::new_constant(cs.clone(), F::from(1))?),
        &(same_segment & !is_padding),
    )?;

    segment_monotonic_constraint.enforce_equal(&Boolean::TRUE)?;

    Ok(())
}

fn fingerprint_with_cached_powers(
    c0: &FpVar<F>,
    c1_powers: &[FpVar<F>],
    timestamp: &FpVar<F>,
    addr: &FpVar<F>,
    values: &[FpVar<F>],
) -> Result<FpVar<F>, SynthesisError> {
    let cs = c0.cs();

    let mut x = timestamp + &c1_powers[0] * addr;

    for (v, c1_p) in values
        .iter()
        .cloned()
        .chain(repeat_with(|| {
            FpVar::new_witness(cs.clone(), || Ok(F::from(0))).unwrap()
        }))
        .zip(c1_powers.iter())
    {
        x += v * c1_p;
    }

    Ok(c0 - x)
}
