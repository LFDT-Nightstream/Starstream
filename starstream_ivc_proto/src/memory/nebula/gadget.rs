use super::Address;
use super::MemOp;
use super::ic::{IC, ICPlain};
use crate::F;
use crate::memory::IVCMemoryAllocated;
use crate::memory::nebula::tracer::NebulaMemoryParams;
use ark_ff::Field;
use ark_ff::PrimeField;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar as _;
use ark_r1cs_std::fields::fp::FpVar;
use ark_r1cs_std::prelude::Boolean;
use ark_relations::gr1cs::ConstraintSystemRef;
use ark_relations::gr1cs::SynthesisError;
use std::collections::BTreeMap;
use std::collections::VecDeque;

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

    pub(crate) c0: F,
    pub(crate) c0_wire: Option<FpVar<F>>,
    pub(crate) c1: F,
    pub(crate) c1_wire: Option<FpVar<F>>,

    pub(crate) multiset_fingerprints: FingerPrintPreWires,
    pub(crate) fingerprint_wires: Option<FingerPrintWires>,

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

        self.scan(cs)?;

        Ok(())
    }

    fn finish_step(&mut self, is_last_step: bool) -> Result<(), SynthesisError> {
        self.cs = None;

        self.current_step += 1;

        self.ic_rs_ws = self.step_ic_rs_ws.take().unwrap().values();
        self.ic_is_fs = self.step_ic_is_fs.take().unwrap().values();

        self.multiset_fingerprints = self.fingerprint_wires.take().unwrap().values()?;

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

    fn conditional_read(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
    ) -> Result<Vec<FpVar<F>>, SynthesisError> {
        let _guard = tracing::debug_span!("nebula_conditional_read").entered();

        // ts <- ts + 1
        self.inc_ts(cond)?;

        let mem = self.get_mem_info(address);
        let address_val = self.get_address_val(address);

        let rv = self.get_read_op(cond, &address_val, mem.0)?;
        let wv = self.get_write_op(cond, &address_val, mem.0)?;

        self.update_ic_with_ops(&cond, address, &rv, &wv)?;

        tracing::debug!(
            "nebula read {:?} at address {} in segment {}",
            rv.values
                .iter()
                .map(|v| v.value().unwrap().into_bigint())
                .collect::<Vec<_>>(),
            address_val.addr,
            mem.1,
        );

        Ok(rv.values)
    }

    fn conditional_write(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
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

        for ((_, val), expected) in vals.iter().enumerate().zip(wv.values.iter()) {
            assert_eq!(val.value().unwrap(), expected.value().unwrap());
        }

        tracing::debug!(
            "nebula write values {:?} at address {} in segment {}",
            vals.iter()
                .map(|v| v.value().unwrap().into_bigint())
                .collect::<Vec<_>>(),
            address_val.addr,
            mem.1,
        );

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

    fn get_address_val(&self, address: &Address<FpVar<F>>) -> Address<u64> {
        Address {
            addr: address.addr.value().unwrap().into_bigint().as_ref()[0],
            tag: address.tag,
        }
    }

    fn get_mem_info(&self, address: &Address<FpVar<F>>) -> (u64, &'static str) {
        self.mems.get(&address.tag).copied().unwrap()
    }

    fn get_read_op(
        &mut self,
        cond: &Boolean<F>,
        address_val: &Address<u64>,
        mem_size: u64,
    ) -> Result<MemOp<FpVar<F>>, SynthesisError> {
        let cs = self.get_cs();

        if cond.value()? {
            let a_reads = self.reads.get_mut(address_val).unwrap();
            a_reads
                .pop_front()
                .expect("no entry in read set")
                .allocate(cs)
        } else {
            MemOp::padding(mem_size).allocate(cs)
        }
    }

    fn get_write_op(
        &mut self,
        cond: &Boolean<F>,
        address_val: &Address<u64>,
        mem_size: u64,
    ) -> Result<MemOp<FpVar<F>>, SynthesisError> {
        let cs = self.get_cs();

        if cond.value()? {
            let a_writes = self.writes.get_mut(address_val).unwrap();
            a_writes
                .pop_front()
                .expect("no entry in write set")
                .allocate(cs)
        } else {
            MemOp::padding(mem_size).allocate(cs)
        }
    }

    fn update_ic_with_ops(
        &mut self,
        cond: &Boolean<F>,
        address: &Address<FpVar<F>>,
        rv: &MemOp<FpVar<F>>,
        wv: &MemOp<FpVar<F>>,
    ) -> Result<(), SynthesisError> {
        let cs = self.get_cs();

        Self::hash_avt(
            cond,
            &mut self.fingerprint_wires.as_mut().unwrap().rs,
            self.c0_wire.as_ref().unwrap(),
            self.c1_wire.as_ref().unwrap(),
            &cs,
            &address,
            rv,
            &mut self.debug_sets.rs,
        )?;

        self.step_ic_rs_ws
            .as_mut()
            .unwrap()
            .increment(address, rv)?;

        Self::hash_avt(
            cond,
            &mut self.fingerprint_wires.as_mut().unwrap().ws,
            self.c0_wire.as_ref().unwrap(),
            self.c1_wire.as_ref().unwrap(),
            &cs,
            &address,
            wv,
            &mut self.debug_sets.ws,
        )?;

        self.step_ic_rs_ws
            .as_mut()
            .unwrap()
            .increment(address, wv)?;

        Ok(())
    }

    fn scan(&mut self, cs: ConstraintSystemRef<F>) -> Result<(), SynthesisError> {
        Ok(
            for (addr, is_v) in self
                .is
                .iter()
                .skip(self.params.scan_batch_size * self.current_step)
                .take(self.params.scan_batch_size)
            {
                let fs_v = self.fs.get(addr).unwrap();

                let address = addr.allocate(cs.clone())?;
                let is_entry = is_v.allocate(cs.clone())?;

                self.step_ic_is_fs
                    .as_mut()
                    .unwrap()
                    .increment(&address, &is_entry)?;

                let fs_entry = fs_v.allocate(cs.clone())?;

                self.step_ic_is_fs
                    .as_mut()
                    .unwrap()
                    .increment(&address, &fs_entry)?;

                Self::hash_avt(
                    &Boolean::constant(true),
                    &mut self.fingerprint_wires.as_mut().unwrap().is,
                    self.c0_wire.as_ref().unwrap(),
                    self.c1_wire.as_ref().unwrap(),
                    &cs,
                    &address,
                    &is_entry,
                    &mut self.debug_sets.is,
                )?;

                Self::hash_avt(
                    &Boolean::constant(true),
                    &mut self.fingerprint_wires.as_mut().unwrap().fs,
                    self.c0_wire.as_ref().unwrap(),
                    self.c1_wire.as_ref().unwrap(),
                    &cs,
                    &address,
                    &fs_entry,
                    &mut self.debug_sets.fs,
                )?;
            },
        )
    }

    fn hash_avt(
        cond: &Boolean<F>,
        wire: &mut FpVar<F>,
        c0: &FpVar<F>,
        c1: &FpVar<F>,
        cs: &ConstraintSystemRef<F>,
        address: &Address<FpVar<F>>,
        vt: &MemOp<FpVar<F>>,
        debug_set: &mut BTreeMap<Address<F>, MemOp<F>>,
    ) -> Result<(), SynthesisError> {
        // TODO: I think this is incorrect, why isn't this allocated before?
        let ts = FpVar::new_witness(cs.clone(), || Ok(F::from(vt.timestamp))).unwrap();
        let fingerprint = fingerprint(c0, c1, &ts, &address.addr, vt.values.as_ref())?;

        if cond.value()? {
            debug_set.insert(
                Address {
                    addr: address.addr.value()?,
                    tag: address.tag,
                },
                MemOp {
                    values: vt.debug_values(),
                    timestamp: vt.timestamp,
                },
            );
        }

        *wire *= cond.select(&fingerprint, &FpVar::Constant(F::ONE))?;

        Ok(())
    }
}

fn fingerprint(
    c0: &FpVar<F>,
    c1: &FpVar<F>,
    timestamp: &FpVar<F>,
    addr: &FpVar<F>,
    values: &[FpVar<F>],
) -> Result<FpVar<F>, SynthesisError> {
    let mut x = timestamp + c1 * addr;

    let mut c1_p = c1.clone();

    for v in values {
        c1_p = c1_p * c1;

        x += v * &c1_p;
    }

    Ok(c0 - x)
}
