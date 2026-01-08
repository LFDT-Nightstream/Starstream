use crate::{
    ccs_step_shape,
    circuit::{InterRoundWires, StepCircuitBuilder},
    goldilocks::FpGoldilocks,
    memory::twist_and_shout::{TSMemInitTables, TSMemLayouts, TSMemory, TSMemoryConstraints},
};
use ark_ff::PrimeField;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_fold::session::{NeoCircuit, WitnessLayout};
use neo_memory::{ShoutCpuBinding, TwistCpuBinding};
use neo_vm_trace::{Shout, StepTrace, Twist, VmCpu};
use p3_field::PrimeCharacteristicRing;
use std::collections::HashMap;

pub(crate) struct StepCircuitNeo {
    pub(crate) matrices: Vec<Vec<Vec<(crate::F, usize)>>>,
    pub(crate) num_constraints: usize,
    pub(crate) ts_mem_spec: TSMemLayouts,
    pub(crate) ts_mem_init: TSMemInitTables<crate::F>,
}

impl StepCircuitNeo {
    pub fn new(ts_mem_init: TSMemInitTables<crate::F>) -> Self {
        let (ark_cs, ts_mem_spec) = ccs_step_shape().unwrap();

        let num_constraints = ark_cs.num_constraints();
        let num_instance_variables = ark_cs.num_instance_variables();
        let num_variables = ark_cs.num_variables();

        tracing::info!("num constraints {}", num_constraints);
        tracing::info!("num instance variables {}", num_instance_variables);
        tracing::info!("num variables {}", num_variables);

        assert_eq!(num_variables, <Self as NeoCircuit>::Layout::USED_COLS);

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
        }
    }
}

#[derive(Clone)]
pub struct CircuitLayout {}

impl WitnessLayout for CircuitLayout {
    // instance.len()
    const M_IN: usize = 1;

    // instance.len()+witness.len()
    const USED_COLS: usize = 182;

    fn new_layout() -> Self {
        CircuitLayout {}
    }
}

impl NeoCircuit for StepCircuitNeo {
    type Layout = CircuitLayout;

    fn chunk_size(&self) -> usize {
        1 // for now, this is simpler to debug
    }

    fn const_one_col(&self, _layout: &Self::Layout) -> usize {
        0
    }

    fn resources(&self, resources: &mut neo_fold::session::SharedBusResources) {
        let max_rom_size = self.ts_mem_init.rom_sizes.values().max();

        for (tag, (_dims, lanes, ty, _)) in &self.ts_mem_init.mems {
            match ty {
                crate::memory::MemType::Rom => {
                    let mut content: Vec<neo_math::F> = self
                        .ts_mem_init
                        .init
                        .get(tag)
                        .map(|content| {
                            content
                                .iter()
                                .map(ark_field_to_p3_goldilocks)
                                .collect()
                        })
                        .unwrap_or(vec![]);

                    content.resize(max_rom_size.copied().unwrap(), neo_math::F::ZERO);

                    resources
                        .shout(*tag as u32)
                        .lanes(lanes.0)
                        .padded_binary_table(content);
                }
                crate::memory::MemType::Ram => {
                    // TODO
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

        for ((matrix_a, matrix_b), matrix_c) in matrices[0]
            .iter()
            .zip(&matrices[1])
            .zip(&matrices[2])
            .take(self.num_constraints)
        {
            let a_row = ark_matrix_to_neo(matrix_a);
            let b_row = ark_matrix_to_neo(matrix_b);
            let c_row = ark_matrix_to_neo(matrix_c);

            cs.r1cs_terms(a_row, b_row, c_row);
        }

        tracing::info!("constraints defined");

        Ok(())
    }

    fn build_witness_prefix(
        &self,
        _layout: &Self::Layout,
        chunk: &[StepTrace<u64, u64>],
    ) -> Result<Vec<neo_math::F>, String> {
        let mut witness = vec![];

        let c = &chunk[0];

        for v in &c.regs_after {
            witness.push(neo_math::F::from_u64(*v));
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
        let mut shout_map: HashMap<u32, Vec<ShoutCpuBinding>> = HashMap::new();

        for (tag, layouts) in &self.ts_mem_spec.shout_bindings {
            for layout in layouts {
                let entry = shout_map.entry(*tag as u32).or_default();
                entry.push(ShoutCpuBinding {
                    has_lookup: Self::Layout::M_IN + layout.has_lookup,
                    addr: Self::Layout::M_IN + layout.addr,
                    val: Self::Layout::M_IN + layout.val,
                });
            }
        }

        Ok((shout_map, HashMap::new()))
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
        let irw = InterRoundWires::new(
            crate::F::from(step_circuit_builder.p_len() as u64),
            step_circuit_builder.instance.entrypoint.0 as u64,
        );

        Self {
            step_circuit_builder,
            step_i: 0,
            mem,
            irw,
            regs: vec![0; <StepCircuitNeo as NeoCircuit>::Layout::USED_COLS],
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
        _twist: &mut T,
        shout: &mut S,
    ) -> Result<neo_vm_trace::StepMeta<u64>, Self::Error>
    where
        T: Twist<u64, u64>,
        S: Shout<u64>,
    {
        let cs = ConstraintSystem::<crate::F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        let (irw, mem_trace_data) = self.step_circuit_builder.make_step_circuit(
            self.step_i,
            &mut self.mem,
            cs.clone(),
            self.irw.clone(),
        )?;

        dbg!(cs.which_is_unsatisfied().unwrap());
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

        for event in mem_trace_data {
            assert_eq!(
                shout.lookup(neo_vm_trace::ShoutId(event.shout_id), event.key),
                event.value
            );
        }

        Ok(neo_vm_trace::StepMeta {
            pc_after: self.step_i as u64,
            opcode: 0,
        })
    }
}

pub fn ark_field_to_p3_goldilocks(col_v: &FpGoldilocks) -> p3_goldilocks::Goldilocks {
    let original_u64 = col_v.into_bigint().0[0];
    let result = neo_math::F::from_u64(original_u64);

    // Assert that we can convert back and get the same element
    let converted_back = FpGoldilocks::from(original_u64);
    assert_eq!(
        *col_v, converted_back,
        "Field element conversion is not reversible"
    );

    result
}
