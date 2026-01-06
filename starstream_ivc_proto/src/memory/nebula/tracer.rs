use ark_ff::AdditiveGroup;
use ark_ff::Field as _;

use super::Address;
use super::MemOp;
use super::NebulaMemoryConstraints;
use super::ic::ICPlain;
use crate::F;
use crate::memory::IVCMemory;
use crate::memory::MemType;
use crate::memory::nebula::gadget::FingerPrintPreWires;
use crate::memory::twist_and_shout::Lanes;
use std::collections::BTreeMap;
use std::collections::VecDeque;

pub struct NebulaMemory<const SCAN_BATCH_SIZE: usize = 1> {
    pub(crate) rs: BTreeMap<Address<u64>, VecDeque<MemOp<F>>>,
    pub(crate) ws: BTreeMap<Address<u64>, VecDeque<MemOp<F>>>,
    pub(crate) is: BTreeMap<Address<u64>, MemOp<F>>,

    pub(crate) mems: BTreeMap<u64, (u64, &'static str)>,

    ic_rs_ws: ICPlain,

    ts: u64,

    params: NebulaMemoryParams,
}

impl<const SCAN_BATCH_SIZE: usize> NebulaMemory<SCAN_BATCH_SIZE> {
    fn perform_memory_operation(
        &mut self,
        cond: bool,
        address: &Address<u64>,
        new_values: Option<Vec<F>>,
    ) -> Vec<F> {
        if cond {
            self.ts += 1;
        }

        let rv = if cond {
            self.ws
                .get(address)
                .and_then(|writes| writes.back().cloned())
                .unwrap_or_else(|| {
                    self.is
                        .get(address)
                        .expect(&format!("read uninitialized address: {address:?}"))
                        .clone()
                })
        } else {
            MemOp::padding()
        };

        assert!(!cond || rv.timestamp < self.ts);

        let wv = if cond {
            MemOp {
                values: new_values.unwrap_or_else(|| rv.values.clone()),
                timestamp: self.ts,
            }
        } else {
            MemOp::padding()
        };

        // println!(
        //     "Tracing: incrementing ic_rs_ws with rv: {:?}, wv: {:?}",
        //     rv, wv
        // );
        self.ic_rs_ws
            .increment(
                address,
                &rv,
                self.params.unsound_disable_poseidon_commitment,
            )
            .unwrap();
        self.ic_rs_ws
            .increment(
                address,
                &wv,
                self.params.unsound_disable_poseidon_commitment,
            )
            .unwrap();
        // println!(
        //     "Tracing: ic_rs_ws after increment: {:?}",
        //     self.ic_rs_ws.comm
        // );

        if !cond {
            let mem_value_size = self.mems.get(&address.tag).unwrap().0;
            return std::iter::repeat_n(F::from(0), mem_value_size as usize).collect();
        }

        let reads = self.rs.entry(address.clone()).or_default();
        reads.push_back(rv.clone());

        self.ws
            .entry(address.clone())
            .or_default()
            .push_back(wv.clone());

        rv.values
    }

    pub fn get_ic_rs_ws(&self) -> [F; 4] {
        self.ic_rs_ws.comm
    }
}

#[derive(Default)]
pub struct NebulaMemoryParams {
    pub unsound_disable_poseidon_commitment: bool,
}

impl<const SCAN_BATCH_SIZE: usize> IVCMemory<F> for NebulaMemory<SCAN_BATCH_SIZE> {
    type Allocator = NebulaMemoryConstraints<F>;

    type Params = NebulaMemoryParams;

    fn new(params: Self::Params) -> Self {
        NebulaMemory {
            rs: BTreeMap::default(),
            ws: BTreeMap::default(),
            is: BTreeMap::default(),
            mems: BTreeMap::default(),
            ts: 0,
            ic_rs_ws: ICPlain::zero(),
            params,
        }
    }

    fn register_mem_with_lanes(
        &mut self,
        tag: u64,
        size: u64,
        _mem_type: MemType,
        // TODO: this is not generic
        _extra_info: Lanes,
        debug_name: &'static str,
    ) {
        self.mems.insert(tag, (size, debug_name));
    }

    fn init(&mut self, address: Address<u64>, values: Vec<F>) {
        self.is.insert(
            address,
            MemOp {
                values: values.clone(),
                timestamp: 0,
            },
        );
    }

    fn conditional_read(&mut self, cond: bool, address: Address<u64>) -> Vec<F> {
        self.perform_memory_operation(cond, &address, None)
    }

    fn conditional_write(&mut self, cond: bool, address: Address<u64>, values: Vec<F>) {
        assert_eq!(
            self.mems.get(&address.tag).unwrap().0 as usize,
            values.len(),
            "write doesn't match mem value size"
        );

        self.perform_memory_operation(cond, &address, Some(values));
    }

    fn required_steps(&self) -> usize {
        self.is.len() / SCAN_BATCH_SIZE
    }

    fn constraints(mut self) -> Self::Allocator {
        let mut ic_is_fs = ICPlain::zero();

        let padding_required = SCAN_BATCH_SIZE - (self.is.len() % SCAN_BATCH_SIZE);

        let mut max_address = self.is.keys().rev().next().unwrap().clone();

        max_address.tag += 1;

        self.register_mem(max_address.tag, 1, MemType::Ram, "PADDING_SEGMENT");

        for _ in 0..padding_required {
            self.is.insert(
                max_address.clone(),
                MemOp {
                    values: vec![F::ZERO],
                    timestamp: 0,
                },
            );
            max_address.addr += 1;
        }

        // compute FS such that:
        //
        // IS U WS = RS U FS
        //
        // (Lemma 2 in the Nebula paper)
        let mut fs = BTreeMap::default();

        for (addr, vt) in self.is.iter().chain(
            self.ws
                .iter()
                .flat_map(|(addr, queue)| queue.iter().map(move |vt| (addr, vt))),
        ) {
            if !self.rs.get(addr).is_some_and(|vals| vals.contains(vt)) {
                fs.insert(addr.clone(), vt.clone());
            }
        }

        for (addr, is_v) in self.is.iter() {
            let fs_v = fs.get(addr).unwrap_or(is_v);

            ic_is_fs
                .increment(addr, is_v, self.params.unsound_disable_poseidon_commitment)
                .unwrap();
            ic_is_fs
                .increment(addr, fs_v, self.params.unsound_disable_poseidon_commitment)
                .unwrap();
        }

        NebulaMemoryConstraints {
            cs: None,
            reads: self.rs,
            writes: self.ws,
            mems: self.mems,
            ic_rs_ws: ICPlain::zero(),
            ic_is_fs: ICPlain::zero(),
            ts: F::ZERO,
            step_ts: None,
            expected_rw_ws: self.ic_rs_ws,
            expected_is_fs: ic_is_fs,
            fs,
            is: self.is,
            current_step: 0,
            params: self.params,
            scan_batch_size: SCAN_BATCH_SIZE,
            step_ic_rs_ws: None,
            step_ic_is_fs: None,
            // TODO: fiat-shamir, these are derived by hashing the multisets
            c0: F::from(1),
            c1: F::from(2),
            c0_wire: None,
            c1_wire: None,
            multiset_fingerprints: FingerPrintPreWires {
                is: F::ONE,
                fs: F::ONE,
                rs: F::ONE,
                ws: F::ONE,
            },
            fingerprint_wires: None,

            debug_sets: Default::default(),
            c1_powers_cache: None,
            scan_monotonic_last_addr: None,
            scan_monotonic_last_addr_wires: None,
        }
    }
}
