use crate::{
    ccs_step_shape,
    circuit::{InterRoundWires, IvcWireLayout, StepCircuitBuilder},
    memory::twist_and_shout::{
        TSMemInitTables, TSMemLayouts, TSMemory, TSMemoryConstraints, TWIST_DEBUG_FILTER,
    },
};
use ark_ff::PrimeField;
use ark_goldilocks::FpGoldilocks;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_ccs::{CcsMatrix, CcsStructure, CscMat, SparsePoly, Term};
use neo_fold::session::{NeoCircuit, SharedBusR1csPreprocessing, SharedBusResources, WitnessLayout};
use neo_fold::PiCcsError;
use neo_memory::{ShoutCpuBinding, TwistCpuBinding};
use neo_vm_trace::{Shout, StepTrace, Twist, VmCpu};
use p3_field::PrimeCharacteristicRing;
use std::collections::HashMap;
use std::sync::Arc;

// TODO: benchmark properly
pub(crate) const CHUNK_SIZE: usize = 50;
const PER_STEP_COLS: usize = 929;
const BASE_INSTANCE_COLS: usize = 1;
const EXTRA_INSTANCE_COLS: usize = IvcWireLayout::FIELD_COUNT * 2;
const M_IN: usize = BASE_INSTANCE_COLS + EXTRA_INSTANCE_COLS;
const USED_COLS: usize = M_IN + (PER_STEP_COLS - BASE_INSTANCE_COLS) * CHUNK_SIZE;

pub(crate) struct StepCircuitNeo {
    pub(crate) matrices: Vec<Vec<Vec<(crate::F, usize)>>>,
    pub(crate) num_constraints: usize,
    pub(crate) ts_mem_spec: TSMemLayouts,
    pub(crate) ts_mem_init: TSMemInitTables<crate::F>,
    pub(crate) ivc_layout: crate::circuit::IvcWireLayout,
}

impl StepCircuitNeo {
    pub fn new(ts_mem_init: TSMemInitTables<crate::F>) -> Self {
        let (ark_cs, ts_mem_spec, ivc_layout) = ccs_step_shape().unwrap();

        let num_constraints = ark_cs.num_constraints();
        let num_instance_variables = ark_cs.num_instance_variables();
        let num_variables = ark_cs.num_variables();

        tracing::info!("num constraints {}", num_constraints);
        tracing::info!("num instance variables {}", num_instance_variables);
        tracing::info!("num variables {}", num_variables);

        assert_eq!(num_variables, PER_STEP_COLS);

        let matrices = ark_cs
            .into_inner()
            .unwrap()
            .to_matrices()
            .unwrap()
            .remove("R1CS")
            .unwrap();

        Self {
            matrices,
            num_constraints,
            ts_mem_spec,
            ts_mem_init,
            ivc_layout,
        }
    }

    fn get_mem_content_iter<'a>(
        &'a self,
        tag: &'a u64,
    ) -> impl Iterator<Item = (u64, neo_math::F)> + 'a {
        self.ts_mem_init
            .init
            .get(tag)
            .into_iter()
            .flat_map(|content| content.iter())
            .map(|(addr, val)| (*addr, ark_field_to_p3_goldilocks(val)))
    }

    fn build_base_ccs_sparse(
        &self,
        const_one_col: usize,
        m_in: usize,
        used_cols: usize,
        bus_region_len: usize,
        reserved_trailing_rows: usize,
    ) -> Result<CcsStructure<neo_math::F>, String> {
        if const_one_col >= m_in {
            return Err(format!(
                "const_one_col({const_one_col}) must be < m_in({m_in})"
            ));
        }

        let m = used_cols
            .checked_add(bus_region_len)
            .ok_or_else(|| "CCS width overflow".to_string())?;

        let base_rows = self
            .num_constraints
            .checked_mul(CHUNK_SIZE)
            .ok_or_else(|| "cpu row count overflow".to_string())?;

        let ivc_rows = IvcWireLayout::FIELD_COUNT
            .checked_mul(
                (CHUNK_SIZE - 1)
                    .checked_add(2)
                    .ok_or_else(|| "ivc row count overflow".to_string())?,
            )
            .ok_or_else(|| "ivc row count overflow".to_string())?;

        let cpu_rows = base_rows
            .checked_add(ivc_rows)
            .ok_or_else(|| "cpu row count overflow".to_string())?;

        let n = cpu_rows
            .checked_add(reserved_trailing_rows)
            .ok_or_else(|| "CCS row count overflow".to_string())?;

        if m_in > m {
            return Err(format!("m_in({m_in}) exceeds CCS width m({m})"));
        }

        let base_a_terms: usize = self.matrices[0]
            .iter()
            .take(self.num_constraints)
            .map(|row| row.len())
            .sum();
        let base_b_terms: usize = self.matrices[1]
            .iter()
            .take(self.num_constraints)
            .map(|row| row.len())
            .sum();
        let base_c_terms: usize = self.matrices[2]
            .iter()
            .take(self.num_constraints)
            .map(|row| row.len())
            .sum();

        let eq_constraints = ivc_rows;

        let mut a_trips = Vec::with_capacity(base_a_terms.saturating_mul(CHUNK_SIZE) + eq_constraints * 2);
        let mut b_trips = Vec::with_capacity(base_b_terms.saturating_mul(CHUNK_SIZE) + eq_constraints);
        let mut c_trips = Vec::with_capacity(base_c_terms.saturating_mul(CHUNK_SIZE));

        let base_m_in = BASE_INSTANCE_COLS;

        // 1) Base per-step constraints replicated across the chunk (column-major interleaving).
        for r in 0..self.num_constraints {
            let a_row = ark_matrix_to_neo(&self.matrices[0][r]);
            let b_row = ark_matrix_to_neo(&self.matrices[1][r]);
            let c_row = ark_matrix_to_neo(&self.matrices[2][r]);

            let row_base = r * CHUNK_SIZE;
            for j in 0..CHUNK_SIZE {
                let row = row_base + j;
                for &(idx, coeff) in &a_row {
                    let col = if idx < base_m_in {
                        idx
                    } else {
                        m_in + (idx - base_m_in) * CHUNK_SIZE + j
                    };
                    a_trips.push((row, col, coeff));
                }
                for &(idx, coeff) in &b_row {
                    let col = if idx < base_m_in {
                        idx
                    } else {
                        m_in + (idx - base_m_in) * CHUNK_SIZE + j
                    };
                    b_trips.push((row, col, coeff));
                }
                for &(idx, coeff) in &c_row {
                    let col = if idx < base_m_in {
                        idx
                    } else {
                        m_in + (idx - base_m_in) * CHUNK_SIZE + j
                    };
                    c_trips.push((row, col, coeff));
                }
            }
        }

        // 2) IVC continuity + wiring constraints (all encoded as (left-right)*1 = 0).
        let one = neo_math::F::ONE;
        let minus_one = -neo_math::F::ONE;
        let input_base = BASE_INSTANCE_COLS;
        let output_base = BASE_INSTANCE_COLS + IvcWireLayout::FIELD_COUNT;

        let mut row = base_rows;

        for (field_offset, (&input_idx, &output_idx)) in self
            .ivc_layout
            .input_indices()
            .iter()
            .zip(self.ivc_layout.output_indices().iter())
            .enumerate()
        {
            for j in 0..(CHUNK_SIZE - 1) {
                let out_col = m_in + output_idx * CHUNK_SIZE + j;
                let in_col = m_in + input_idx * CHUNK_SIZE + (j + 1);

                a_trips.push((row, out_col, one));
                a_trips.push((row, in_col, minus_one));
                b_trips.push((row, const_one_col, one));
                row += 1;
            }

            let first_chunk_in_col = m_in + input_idx * CHUNK_SIZE;
            let last_chunk_out_col = m_in + output_idx * CHUNK_SIZE + (CHUNK_SIZE - 1);

            let in_instance_col = input_base + field_offset;
            let out_instance_col = output_base + field_offset;

            a_trips.push((row, in_instance_col, one));
            a_trips.push((row, first_chunk_in_col, minus_one));
            b_trips.push((row, const_one_col, one));
            row += 1;

            a_trips.push((row, out_instance_col, one));
            a_trips.push((row, last_chunk_out_col, minus_one));
            b_trips.push((row, const_one_col, one));
            row += 1;
        }

        debug_assert_eq!(row, cpu_rows, "unexpected CPU row count");

        let a = CscMat::from_triplets(a_trips, n, m);
        let b = CscMat::from_triplets(b_trips, n, m);
        let c = CscMat::from_triplets(c_trips, n, m);

        // f(X1,X2,X3) = X1*X2 - X3 (standard R1CS→CCS embedding).
        let f = SparsePoly::new(
            3,
            vec![
                Term {
                    coeff: neo_math::F::ONE,
                    exps: vec![1, 1, 0],
                },
                Term {
                    coeff: -neo_math::F::ONE,
                    exps: vec![0, 0, 1],
                },
            ],
        );

        CcsStructure::new_sparse(
            vec![CcsMatrix::Csc(a), CcsMatrix::Csc(b), CcsMatrix::Csc(c)],
            f,
        )
        .map_err(|e| format!("failed to build sparse CCS: {e:?}"))
    }
}

#[derive(Clone)]
pub struct CircuitLayout {}

impl WitnessLayout for CircuitLayout {
    // instance.len()
    const M_IN: usize = M_IN;

    // instance.len()+witness.len()
    const USED_COLS: usize = USED_COLS;

    fn new_layout() -> Self {
        CircuitLayout {}
    }
}

impl NeoCircuit for StepCircuitNeo {
    type Layout = CircuitLayout;

    fn chunk_size(&self) -> usize {
        CHUNK_SIZE
    }

    fn const_one_col(&self, _layout: &Self::Layout) -> usize {
        0
    }

    fn resources(&self, resources: &mut neo_fold::session::SharedBusResources) {
        let max_rom_size = self.ts_mem_init.rom_sizes.values().max();

        for (tag, (_dims, lanes, ty, _)) in &self.ts_mem_init.mems {
            match ty {
                crate::memory::MemType::Rom => {
                    let size = *max_rom_size.unwrap();
                    let mut dense_content = vec![neo_math::F::ZERO; size];

                    for (addr, val) in self.get_mem_content_iter(tag) {
                        dense_content[addr as usize] = val;
                    }

                    resources
                        .shout(*tag as u32)
                        .lanes(lanes.0)
                        .padded_binary_table(dense_content);
                }
                crate::memory::MemType::Ram => {
                    if !TWIST_DEBUG_FILTER.iter().any(|f| *tag == *f as u64) {
                        continue;
                    }

                    let twist_id = *tag as u32;
                    let k = 256usize; // TODO: hardcoded number
                    assert!(k > 0, "set_binary_mem_layout: k must be > 0");
                    assert!(
                        k.is_power_of_two(),
                        "set_binary_mem_layout: k must be a power of two"
                    );
                    resources
                        .twist(twist_id)
                        .layout(neo_memory::PlainMemLayout {
                            k,
                            d: k.trailing_zeros() as usize,
                            n_side: 2,
                            lanes: lanes.0,
                        })
                        .init(self.get_mem_content_iter(tag));
                }
            }
        }
    }

    fn define_cpu_constraints(
        &self,
        cs: &mut neo_fold::session::CcsBuilder<neo_math::F>,
        _layout: &Self::Layout,
    ) -> Result<(), String> {
        let matrices = &self.matrices;
        let m_in = <Self::Layout as WitnessLayout>::M_IN;
        let base_m_in = BASE_INSTANCE_COLS;

        for ((matrix_a, matrix_b), matrix_c) in matrices[0]
            .iter()
            .zip(&matrices[1])
            .zip(&matrices[2])
            .take(self.num_constraints)
        {
            let a_row = ark_matrix_to_neo(matrix_a);
            let b_row = ark_matrix_to_neo(matrix_b);
            let c_row = ark_matrix_to_neo(matrix_c);

            for j in 0..CHUNK_SIZE {
                let map_idx = |idx: usize| {
                    if idx < base_m_in {
                        idx
                    } else {
                        // NOTE: m_in includes the per folding step IVC
                        // variables (inputs and outputs)
                        m_in + (idx - base_m_in) * CHUNK_SIZE + j
                    }
                };

                cs.r1cs_terms(
                    a_row.iter().map(|(i, v)| (map_idx(*i), *v)),
                    b_row.iter().map(|(i, v)| (map_idx(*i), *v)),
                    c_row.iter().map(|(i, v)| (map_idx(*i), *v)),
                );
            }
        }

        let one = neo_math::F::ONE;
        let minus_one = -neo_math::F::ONE;
        let input_base = BASE_INSTANCE_COLS;
        let output_base = BASE_INSTANCE_COLS + IvcWireLayout::FIELD_COUNT;

        for (field_offset, (&input_idx, &output_idx)) in self
            .ivc_layout
            .input_indices()
            .iter()
            .zip(self.ivc_layout.output_indices().iter())
            .enumerate()
        {
            for j in 0..(CHUNK_SIZE - 1) {
                // The chunked matrix is interleaved column-major:
                // for variable x, step 0 is at `m_in + x * CHUNK_SIZE`, step 1 is at
                // `m_in + x * CHUNK_SIZE + 1`, etc. So `x * CHUNK_SIZE` picks the
                // contiguous block for that variable, and `j` selects the step.
                //
                // We enforce continuity inside a chunk:
                //   wire[in][s+1] == wire[out][s]
                let out_col = m_in + output_idx * CHUNK_SIZE + j;
                let in_col = m_in + input_idx * CHUNK_SIZE + (j + 1);
                cs.r1cs_terms(
                    vec![(out_col, one), (in_col, minus_one)],
                    vec![(0, one)],
                    vec![],
                );
            }

            let first_chunk_in_col = m_in + input_idx * CHUNK_SIZE;
            let last_chunk_out_col = m_in + output_idx * CHUNK_SIZE + (CHUNK_SIZE - 1);

            // The per-folding-step public instance is:
            //   [1, inputs, outputs]
            // We wire the first chunk input to the instance inputs...
            let in_instance_col = input_base + field_offset;
            // ...and the last chunk output to the instance outputs.
            let out_instance_col = output_base + field_offset;

            // This means step_linking in the IVC setup should link pairs:
            //   (i, i + IvcWireLayout::FIELD_COUNT)

            cs.r1cs_terms(
                vec![(in_instance_col, one), (first_chunk_in_col, minus_one)],
                vec![(0, one)],
                vec![],
            );
            cs.r1cs_terms(
                vec![(out_instance_col, one), (last_chunk_out_col, minus_one)],
                vec![(0, one)],
                vec![],
            );
        }

        Ok(())
    }

    fn build_witness_prefix(
        &self,
        _layout: &Self::Layout,
        chunk: &[StepTrace<u64, u64>],
    ) -> Result<Vec<neo_math::F>, String> {
        if chunk.len() != CHUNK_SIZE {
            return Err(format!(
                "chunk len {} != CHUNK_SIZE {}",
                chunk.len(),
                CHUNK_SIZE
            ));
        }

        let m_in = <Self::Layout as WitnessLayout>::M_IN;
        let base_m_in = BASE_INSTANCE_COLS;
        let per_step_cols = chunk[0].regs_after.len();
        if per_step_cols != PER_STEP_COLS {
            return Err(format!(
                "per-step witness len {} != PER_STEP_COLS {}",
                per_step_cols, PER_STEP_COLS
            ));
        }

        let mut witness = vec![neo_math::F::ZERO; USED_COLS];

        witness[0] = neo_math::F::from_u64(chunk[0].regs_after[0]);

        let input_base = BASE_INSTANCE_COLS;
        let output_base = BASE_INSTANCE_COLS + IvcWireLayout::FIELD_COUNT;
        let last_step = chunk.len() - 1;

        for (field_idx, (&input_idx, &output_idx)) in self
            .ivc_layout
            .input_indices()
            .iter()
            .zip(self.ivc_layout.output_indices().iter())
            .enumerate()
        {
            let input_full_idx = base_m_in + input_idx;
            let output_full_idx = base_m_in + output_idx;
            witness[input_base + field_idx] =
                neo_math::F::from_u64(chunk[0].regs_after[input_full_idx]);
            witness[output_base + field_idx] =
                neo_math::F::from_u64(chunk[last_step].regs_after[output_full_idx]);
        }

        for (j, step) in chunk.iter().enumerate() {
            for i in base_m_in..per_step_cols {
                let idx = m_in + (i - base_m_in) * CHUNK_SIZE + j;
                witness[idx] = neo_math::F::from_u64(step.regs_after[i]);
            }
        }

        Ok(witness)
    }

    fn cpu_bindings(
        &self,
        _layout: &Self::Layout,
    ) -> Result<
        (
            HashMap<u32, Vec<ShoutCpuBinding>>,
            HashMap<u32, Vec<TwistCpuBinding>>,
        ),
        String,
    > {
        let m_in = <Self::Layout as WitnessLayout>::M_IN;
        let map_idx = |witness_idx: usize| m_in + witness_idx * CHUNK_SIZE;

        let mut shout_map: HashMap<u32, Vec<ShoutCpuBinding>> = HashMap::new();

        for (tag, layouts) in &self.ts_mem_spec.shout_bindings {
            for layout in layouts {
                let entry = shout_map.entry(*tag as u32).or_default();
                entry.push(ShoutCpuBinding {
                    has_lookup: map_idx(layout.has_lookup),
                    addr: Some(map_idx(layout.addr)),
                    val: map_idx(layout.val),
                });
            }
        }

        let mut twist_map: HashMap<u32, Vec<TwistCpuBinding>> = HashMap::new();

        for (tag, layouts) in &self.ts_mem_spec.twist_bindings {
            for layout in layouts {
                let entry = twist_map.entry(*tag as u32).or_default();
                entry.push(TwistCpuBinding {
                    read_addr: map_idx(layout.ra),
                    has_read: map_idx(layout.has_read),
                    rv: map_idx(layout.rv),
                    write_addr: map_idx(layout.wa),
                    has_write: map_idx(layout.has_write),
                    wv: map_idx(layout.wv),
                    inc: None,
                });
            }
        }

        Ok((shout_map, twist_map))
    }
}

fn ark_matrix_to_neo(sparse_row: &[(FpGoldilocks, usize)]) -> Vec<(usize, neo_math::F)> {
    let mut row = Vec::with_capacity(sparse_row.len());

    for (col_v, col_i) in sparse_row.iter() {
        row.push((*col_i, ark_field_to_p3_goldilocks(col_v)));
    }

    row
}

pub struct StarstreamVm {
    step_circuit_builder: StepCircuitBuilder<TSMemory<crate::F>>,
    step_i: usize,
    mem: TSMemoryConstraints<crate::F>,
    irw: InterRoundWires,
    regs: Vec<u64>,
}

impl StarstreamVm {
    pub fn new(
        step_circuit_builder: StepCircuitBuilder<TSMemory<crate::F>>,
        mem: TSMemoryConstraints<crate::F>,
    ) -> Self {
        let irw = InterRoundWires::new(step_circuit_builder.instance.entrypoint.0 as u64);

        Self {
            step_circuit_builder,
            step_i: 0,
            mem,
            irw,
            regs: vec![0; PER_STEP_COLS],
        }
    }
}

impl VmCpu<u64, u64> for StarstreamVm {
    type Error = SynthesisError;

    fn snapshot_regs(&self) -> Vec<u64> {
        self.regs.clone()
    }

    fn pc(&self) -> u64 {
        self.step_i as u64
    }

    fn halted(&self) -> bool {
        self.step_i == self.step_circuit_builder.ops.len()
    }

    fn step<T, S>(
        &mut self,
        twist: &mut T,
        shout: &mut S,
    ) -> Result<neo_vm_trace::StepMeta<u64>, Self::Error>
    where
        T: Twist<u64, u64>,
        S: Shout<u64>,
    {
        let cs = ConstraintSystem::<crate::F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        let (irw, (shout_events, twist_events), _ivc_layout) =
            self.step_circuit_builder.make_step_circuit(
                self.step_i,
                &mut self.mem,
                cs.clone(),
                self.irw.clone(),
                false,
            )?;

        if let Some(unsat) = cs.which_is_unsatisfied().unwrap() {
            tracing::error!(location = unsat, "step CCS is unsat");
        }

        assert!(cs.is_satisfied().unwrap());

        self.irw = irw;

        self.step_i += 1;

        self.regs = cs
            .instance_assignment()?
            .into_iter()
            .map(|input| input.into_bigint().0[0])
            .chain(
                cs.witness_assignment()?
                    .into_iter()
                    .map(|wit| wit.into_bigint().0[0]),
            )
            .collect();

        for event in shout_events {
            assert_eq!(
                shout.lookup(neo_vm_trace::ShoutId(event.shout_id), event.key),
                event.value
            );
        }

        for event in twist_events {
            match event.op {
                neo_vm_trace::TwistOpKind::Read => {
                    assert_eq!(
                        twist.load_if_lane(
                            event.cond,
                            neo_vm_trace::TwistId(event.twist_id),
                            event.addr,
                            event.val,
                            event.lane.unwrap(),
                        ),
                        event.val,
                    );
                }
                neo_vm_trace::TwistOpKind::Write => {
                    twist.store_if_lane(
                        event.cond,
                        neo_vm_trace::TwistId(event.twist_id),
                        event.addr,
                        event.val,
                        event.lane.unwrap(),
                    );
                }
            }
        }

        Ok(neo_vm_trace::StepMeta {
            pc_after: self.step_i as u64,
            opcode: 0,
        })
    }
}

pub fn ark_field_to_p3_goldilocks(v: &FpGoldilocks) -> p3_goldilocks::Goldilocks {
    let original_u64 = v.into_bigint().0[0];
    let result = neo_math::F::from_u64(original_u64);

    // Assert that we can convert back and get the same element
    let converted_back = FpGoldilocks::from(original_u64);
    debug_assert_eq!(
        *v, converted_back,
        "Field element conversion is not reversible"
    );

    result
}

pub(crate) fn preprocess_shared_bus_r1cs_sparse(
    circuit: Arc<StepCircuitNeo>,
) -> Result<SharedBusR1csPreprocessing<StepCircuitNeo>, PiCcsError> {
    let layout = circuit.layout();
    let m_in = <CircuitLayout as WitnessLayout>::M_IN;
    let used_cols = <CircuitLayout as WitnessLayout>::USED_COLS;
    let chunk_size = circuit.chunk_size();
    if chunk_size == 0 {
        return Err(PiCcsError::InvalidInput("chunk_size must be >= 1".into()));
    }

    let const_one_col = circuit.const_one_col(&layout);
    if const_one_col >= m_in {
        return Err(PiCcsError::InvalidInput(format!(
            "const_one_col({const_one_col}) must be < m_in({m_in})"
        )));
    }

    let mut resources = SharedBusResources::new();
    circuit.resources(&mut resources);

    let (shout_cpu, twist_cpu) = circuit
        .cpu_bindings(&layout)
        .map_err(|e| PiCcsError::InvalidInput(format!("cpu bindings invalid: {e}")))?;

    // Ensure Shout lane counts are consistent across resources + cpu bindings.
    {
        let mut table_ids: Vec<u32> = resources
            .lut_tables
            .keys()
            .copied()
            .chain(resources.lut_table_specs.keys().copied())
            .collect();
        table_ids.sort_unstable();
        table_ids.dedup();

        for table_id in table_ids {
            let bindings = shout_cpu
                .get(&table_id)
                .ok_or_else(|| PiCcsError::InvalidInput(format!("missing shout_cpu binding for table_id={table_id}")))?;
            if bindings.is_empty() {
                return Err(PiCcsError::InvalidInput(format!(
                    "shout_cpu bindings for table_id={table_id} must be non-empty"
                )));
            }
            match resources.lut_lanes.get(&table_id) {
                Some(&lanes) => {
                    if lanes.max(1) != bindings.len() {
                        return Err(PiCcsError::InvalidInput(format!(
                            "shout lanes mismatch for table_id={table_id}: resources.lut_lanes={} but cpu_bindings provides {}",
                            lanes,
                            bindings.len()
                        )));
                    }
                }
                None => {
                    resources.lut_lanes.insert(table_id, bindings.len());
                }
            }
        }
    }

    let (bus_region_len, bus_constraints) = shared_bus_buslen_and_constraints(
        used_cols,
        m_in,
        chunk_size,
        const_one_col,
        &resources,
        &shout_cpu,
        &twist_cpu,
    )
    .map_err(|e| PiCcsError::InvalidInput(format!("shared-bus sizing failed: {e}")))?;

    let base_ccs = circuit
        .build_base_ccs_sparse(const_one_col, m_in, used_cols, bus_region_len, bus_constraints)
        .map_err(|e| PiCcsError::InvalidInput(format!("build sparse CCS failed: {e}")))?;

    Ok(SharedBusR1csPreprocessing {
        circuit,
        layout,
        resources,
        shout_cpu,
        twist_cpu,
        m_in,
        const_one_col,
        chunk_size,
        base_ccs,
    })
}

fn shared_bus_buslen_and_constraints(
    cpu_used_cols: usize,
    m_in: usize,
    chunk_size: usize,
    const_one_col: usize,
    resources: &SharedBusResources,
    shout_cpu: &HashMap<u32, Vec<ShoutCpuBinding>>,
    twist_cpu: &HashMap<u32, Vec<TwistCpuBinding>>,
) -> Result<(usize, usize), String> {
    use neo_memory::cpu::CpuConstraintBuilder;
    use neo_memory::witness::LutTableSpec;

    fn shout_meta_for_bus(
        table_id: u32,
        tables: &HashMap<u32, neo_memory::plain::LutTable<neo_math::F>>,
        specs: &HashMap<u32, LutTableSpec>,
    ) -> Result<(usize, usize), String> {
        if let Some(t) = tables.get(&table_id) {
            return Ok((t.d, t.n_side));
        }
        if let Some(spec) = specs.get(&table_id) {
            match spec {
                LutTableSpec::RiscvOpcode { xlen, .. } => {
                    let d = xlen
                        .checked_mul(2)
                        .ok_or_else(|| "2*xlen overflow for RISC-V shout table".to_string())?;
                    Ok((d, 2usize))
                }
            }
        } else {
            Err(format!("missing shout table metadata for table_id={table_id}"))
        }
    }

    // Deterministic bus order: Shout tables (sorted), then Twist mems (sorted).
    let mut table_ids: Vec<u32> = resources
        .lut_tables
        .keys()
        .copied()
        .chain(resources.lut_table_specs.keys().copied())
        .collect();
    table_ids.sort_unstable();
    table_ids.dedup();

    let mut mem_ids: Vec<u32> = resources.mem_layouts.keys().copied().collect();
    mem_ids.sort_unstable();

    // Compute ell_addr (d * ell) and lanes for each instance.
    let mut shout_ell_addrs_and_lanes = Vec::with_capacity(table_ids.len());
    for table_id in &table_ids {
        let (d, n_side) = shout_meta_for_bus(*table_id, &resources.lut_tables, &resources.lut_table_specs)?;
        if n_side == 0 || !n_side.is_power_of_two() {
            return Err(format!("shout n_side must be power-of-two, got {n_side}"));
        }
        let ell = n_side.trailing_zeros() as usize;
        let ell_addr = d
            .checked_mul(ell)
            .ok_or_else(|| format!("ell_addr overflow for shout table_id={table_id}"))?;
        let lanes = resources.lut_lanes.get(table_id).copied().unwrap_or(1).max(1);
        let bindings = shout_cpu
            .get(table_id)
            .ok_or_else(|| format!("missing shout_cpu binding for table_id={table_id}"))?;
        if bindings.len() != lanes {
            return Err(format!(
                "shout_cpu bindings for table_id={table_id} has len={}, expected lanes={lanes}",
                bindings.len()
            ));
        }
        shout_ell_addrs_and_lanes.push((ell_addr, lanes));
    }

    let mut twist_ell_addrs_and_lanes = Vec::with_capacity(mem_ids.len());
    for mem_id in &mem_ids {
        let layout = resources
            .mem_layouts
            .get(mem_id)
            .ok_or_else(|| format!("missing mem_layout for mem_id={mem_id}"))?;
        if layout.n_side == 0 || !layout.n_side.is_power_of_two() {
            return Err(format!("twist n_side must be power-of-two, got {}", layout.n_side));
        }
        let ell = layout.n_side.trailing_zeros() as usize;
        let ell_addr = layout
            .d
            .checked_mul(ell)
            .ok_or_else(|| format!("ell_addr overflow for twist mem_id={mem_id}"))?;
        let lanes = layout.lanes.max(1);
        twist_ell_addrs_and_lanes.push((ell_addr, lanes));
    }

    let bus_cols = shout_ell_addrs_and_lanes
        .iter()
        .map(|&(ell, lanes)| lanes * (ell + 2))
        .chain(
            twist_ell_addrs_and_lanes
                .iter()
                .map(|&(ell, lanes)| lanes * (2 * ell + 5)),
        )
        .sum::<usize>();
    let bus_region_len = bus_cols
        .checked_mul(chunk_size)
        .ok_or_else(|| "bus_region_len overflow".to_string())?;

    let m_min = cpu_used_cols
        .checked_add(bus_region_len)
        .ok_or_else(|| "shared-bus witness width overflow".to_string())?;

    let bus_layout = neo_memory::cpu::build_bus_layout_for_instances_with_shout_and_twist_lanes(
        m_min,
        m_in,
        chunk_size,
        shout_ell_addrs_and_lanes.iter().copied(),
        twist_ell_addrs_and_lanes.iter().copied(),
    )?;

    let mut builder = CpuConstraintBuilder::<neo_math::F>::new(/*n=*/ 1, /*m=*/ m_min, const_one_col);
    for (i, table_id) in table_ids.iter().enumerate() {
        let cpus = shout_cpu
            .get(table_id)
            .ok_or_else(|| format!("missing shout_cpu binding for table_id={table_id}"))?;
        let inst_cols = &bus_layout.shout_cols[i];
        if cpus.len() != inst_cols.lanes.len() {
            return Err(format!(
                "shared-bus shout lanes mismatch for table_id={table_id}: shout_cpu has len={} but bus layout expects {}",
                cpus.len(),
                inst_cols.lanes.len()
            ));
        }
        for lane_idx in 0..cpus.len() {
            builder.add_shout_instance_bound(&bus_layout, &inst_cols.lanes[lane_idx], &cpus[lane_idx]);
        }
    }
    for (i, mem_id) in mem_ids.iter().enumerate() {
        let inst_layout = resources
            .mem_layouts
            .get(mem_id)
            .ok_or_else(|| format!("missing mem_layout for mem_id={mem_id}"))?;
        let lanes = inst_layout.lanes.max(1);
        let cpus = twist_cpu
            .get(mem_id)
            .ok_or_else(|| format!("missing twist_cpu binding for mem_id={mem_id}"))?;
        if cpus.len() != lanes {
            return Err(format!(
                "twist_cpu bindings for mem_id={mem_id} has len={}, expected lanes={lanes}",
                cpus.len()
            ));
        }
        for lane in 0..lanes {
            builder.add_twist_instance_bound(&bus_layout, &bus_layout.twist_cols[i].lanes[lane], &cpus[lane]);
        }
    }

    Ok((bus_region_len, builder.constraints().len()))
}
