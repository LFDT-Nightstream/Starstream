use crate::{
    SCAN_BATCH_SIZE, ccs_step_shape,
    circuit::{InterRoundWires, StepCircuitBuilder},
    goldilocks::FpGoldilocks,
    memory::IVCMemory,
    nebula::{
        NebulaMemoryConstraints,
        tracer::{NebulaMemory, NebulaMemoryParams},
    },
};
use ark_ff::PrimeField;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_ccs::CcsStructure;
use neo_fold::session::{NeoCircuit, WitnessLayout};
use neo_memory::{ShoutCpuBinding, TwistCpuBinding};
use neo_vm_trace::{Shout, StepTrace, Twist, VmCpu};
use p3_field::PrimeCharacteristicRing;
use std::collections::HashMap;

pub(crate) struct StepCircuitNeo {
    pub(crate) matrices: Vec<Vec<Vec<(crate::F, usize)>>>,
    pub(crate) num_constraints: usize,
    pub(crate) num_instance_variables: usize,
    pub(crate) num_variables: usize,
}

impl StepCircuitNeo {
    pub fn new() -> Self {
        let ark_cs = ccs_step_shape().unwrap();

        let num_constraints = ark_cs.num_constraints();
        let num_instance_variables = ark_cs.num_instance_variables();
        let num_variables = ark_cs.num_constraints();

        tracing::info!("num constraints {}", num_constraints);
        tracing::info!("num instance variables {}", num_instance_variables);
        tracing::info!("num variables {}", num_variables);

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
            num_instance_variables,
            num_variables,
        }
    }
}

#[derive(Clone)]
pub struct CircuitLayout {}

impl WitnessLayout for CircuitLayout {
    // instance.len()
    const M_IN: usize = 1;

    // instance.len()+witness.len()
    const USED_COLS: usize = 1224 + 1;

    fn new_layout() -> Self {
        CircuitLayout {}
    }
}

impl NeoCircuit for StepCircuitNeo {
    type Layout = CircuitLayout;

    fn chunk_size(&self) -> usize {
        1 // for now, this is simpler to debug
    }

    fn const_one_col(&self, layout: &Self::Layout) -> usize {
        0
    }

    fn resources(&self, resources: &mut neo_fold::session::SharedBusResources) {
        // TODO: (no memory for now)
        //
        // . Define Memory Layouts (for Twist): You specify the geometry of your read-write memories, such as their size and address structure.
        //      // In resources()
        //      resources
        //          .twist(0) // Configure Twist memory with op_id = 0
        //          // Define its layout: k=size, d=dimensions, etc.
        //          .layout(PlainMemLayout { k: 2, d: 1, n_side: 2 });
        // . Set Initial Memory Values (for Twist): If your memory isn't zero-initialized, you set the starting values of specific memory cells here. This is how you would "pre-load" a program or
        //   initial data into memory before execution begins.

        //      // In resources()
        //      resources
        //          .twist(0)
        //          // ... after .layout(...)
        //          // Initialize the cell at address 0 with the value F::ONE
        //          .init_cell(0, F::ONE);
        // . Provide Lookup Table Contents (for Shout): You define the complete contents of any read-only tables that your circuit will look up into. This is how you would define a boot ROM or a
        //   fixed data table (like a sine wave table).

        //      // In resources()
        //      // Set the data for the lookup table with op_id = 0
        //      resources.set_binary_table(0, vec![F::ZERO, F::ONE]);
    }

    fn define_cpu_constraints(
        &self,
        cs: &mut neo_fold::session::CcsBuilder<neo_math::F>,
        layout: &Self::Layout,
    ) -> Result<(), String> {
        let matrices = &self.matrices;

        for row in 0..self.num_constraints {
            let a_row = ark_matrix_to_neo(&matrices[0][row]);
            let b_row = ark_matrix_to_neo(&matrices[1][row]);
            let c_row = ark_matrix_to_neo(&matrices[2][row]);

            cs.r1cs_terms(a_row, b_row, c_row);
        }

        tracing::info!("constraints defined");

        Ok(())
    }

    fn build_witness_prefix(
        &self,
        layout: &Self::Layout,
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
        layout: &Self::Layout,
    ) -> Result<(HashMap<u32, ShoutCpuBinding>, HashMap<u32, TwistCpuBinding>), String> {
        // Create the mapping for Shout (read-only) operations
        // let shout_map = HashMap::from([
        //     (1u32, layout.boot_rom.cpu_binding()), // op_id 1 -> boot_rom port
        //     (2u32, layout.trig_table.cpu_binding()), // op_id 2 -> trig_table port
        // ]);

        // // Create the mapping for Twist (read-write) operations
        // let twist_map = HashMap::from([
        //     (0u32, layout.main_ram.cpu_binding()), // op_id 0 -> main_ram port
        // ]);

        Ok((HashMap::new(), HashMap::new()))
    }
}

fn ark_matrix_to_neo(sparse_row: &[(FpGoldilocks, usize)]) -> Vec<(usize, neo_math::F)> {
    let mut row = vec![];

    for (col_v, col_i) in sparse_row.iter() {
        row.push((*col_i, ark_field_to_p3_goldilocks(col_v)));
    }

    row
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

pub struct StarstreamVm {
    step_circuit_builder: StepCircuitBuilder<NebulaMemory<SCAN_BATCH_SIZE>>,
    step_i: usize,
    mem: NebulaMemoryConstraints<crate::F>,
    irw: InterRoundWires,
    regs: Vec<u64>,
}

impl StarstreamVm {
    pub fn new(
        mut step_circuit_builder: StepCircuitBuilder<NebulaMemory<SCAN_BATCH_SIZE>>,
    ) -> Self {
        let irw = InterRoundWires::new(
            crate::F::from(step_circuit_builder.p_len() as u64),
            step_circuit_builder.instance.entrypoint.0 as u64,
        );

        let params = NebulaMemoryParams {
            unsound_disable_poseidon_commitment: true,
        };

        let mb = step_circuit_builder.trace_memory_ops(params);

        Self {
            step_circuit_builder,
            step_i: 0,
            mem: mb.constraints(),
            irw,
            regs: vec![0; 1224 + 1],
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

        let irw = self.step_circuit_builder.make_step_circuit(
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

        Ok(neo_vm_trace::StepMeta {
            pc_after: self.step_i as u64,
            opcode: 0,
        })
    }
}
