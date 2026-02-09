use crate::{
    ccs_step_shape,
    circuit::{InterRoundWires, IvcWireLayout, StepCircuitBuilder},
    memory::twist_and_shout::{TSMemInitTables, TSMemLayouts, TSMemory, TSMemoryConstraints},
};
use ark_ff::PrimeField;
use ark_goldilocks::FpGoldilocks;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_fold::session::{NeoCircuit, WitnessLayout};
use neo_memory::{ShoutCpuBinding, TwistCpuBinding};
use neo_vm_trace::{Shout, StepTrace, Twist, VmCpu};
use p3_field::PrimeCharacteristicRing;
use std::collections::HashMap;

// TODO: benchmark properly
pub(crate) const CHUNK_SIZE: usize = 40;
const PER_STEP_COLS: usize = 1162;
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
}

pub(crate) fn ivc_step_linking_pairs() -> Vec<(usize, usize)> {
    // Per-step instance vector is [1, inputs..., outputs...].
    // Enforce prev outputs == next inputs.
    let input_base = BASE_INSTANCE_COLS;
    let output_base = BASE_INSTANCE_COLS + IvcWireLayout::FIELD_COUNT;
    (0..IvcWireLayout::FIELD_COUNT)
        .map(|i| (output_base + i, input_base + i))
        .collect()
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
        for (tag, (_dims, lanes, ty, _)) in &self.ts_mem_init.mems {
            match ty {
                crate::memory::MemType::Rom => {
                    let size = self
                        .ts_mem_init
                        .rom_sizes
                        .get(tag)
                        .copied()
                        // it can't be empty
                        .unwrap_or(1usize);

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

                let a_row: Vec<_> = a_row.iter().map(|(i, v)| (map_idx(*i), *v)).collect();
                let b_row: Vec<_> = b_row.iter().map(|(i, v)| (map_idx(*i), *v)).collect();
                let c_row: Vec<_> = c_row.iter().map(|(i, v)| (map_idx(*i), *v)).collect();

                cs.r1cs_terms(a_row, b_row, c_row);
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
            //   (output_base + i, input_base + i)

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
    let mut row = vec![];

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
    assert_eq!(
        *v, converted_back,
        "Field element conversion is not reversible"
    );

    result
}
