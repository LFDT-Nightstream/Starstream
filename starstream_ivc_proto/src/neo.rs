use crate::goldilocks::FpGoldilocks;
use ark_ff::PrimeField;
use ark_relations::gr1cs::{ConstraintSystemRef, OptimizationGoal, ConstraintSystem};
use neo::{CcsStructure, F};
use p3_field::PrimeCharacteristicRing;
use crate::circuit::{InterRoundWires, StepCircuitBuilder};
use crate::memory::{DummyMemory, IVCMemory};
use ::neo::{NeoStep as SessionNeoStep, StepArtifacts, StepSpec, StepDescriptor};

pub(crate) struct NeoStep {
    pub(crate) ccs: CcsStructure<F>,
    /// Full variable vector expected by CCS: [instance || witness]
    pub(crate) witness: Vec<F>,
}

pub(crate) fn arkworks_to_neo(cs: ConstraintSystemRef<FpGoldilocks>) -> NeoStep {
    cs.finalize();

    let matrices = &cs.to_matrices().unwrap()["R1CS"];

    #[cfg(debug_assertions)]
    {
        dbg!(cs.num_constraints());
        dbg!(cs.num_instance_variables());
        dbg!(cs.num_witness_variables());
    }

    let a_mat = ark_matrix_to_neo(&cs, &matrices[0]);
    let b_mat = ark_matrix_to_neo(&cs, &matrices[1]);
    let c_mat = ark_matrix_to_neo(&cs, &matrices[2]);

    let ccs = neo_ccs::r1cs_to_ccs(a_mat, b_mat, c_mat);

    let instance = cs
        .instance_assignment()
        .unwrap()
        .iter()
        .map(ark_field_to_p3_goldilocks)
        .collect::<Vec<_>>();

    let witness = cs
        .witness_assignment()
        .unwrap()
        .iter()
        .map(ark_field_to_p3_goldilocks)
        .collect::<Vec<_>>();

    NeoStep { ccs, witness: [instance, witness].concat() }
}

fn ark_matrix_to_neo(
    cs: &ConstraintSystemRef<FpGoldilocks>,
    sparse_matrix: &[Vec<(FpGoldilocks, usize)>],
) -> neo_ccs::Mat<F> {
    let n_rows = cs.num_constraints();
    let n_cols = cs.num_variables();

    // TODO: would be nice to just be able to construct the sparse matrix
    let mut dense = vec![F::from_u64(0); n_rows * n_cols];

    for (row_i, row) in sparse_matrix.iter().enumerate() {
        for (col_v, col_i) in row.iter() {
            dense[n_cols * row_i + col_i] = ark_field_to_p3_goldilocks(col_v);
        }
    }

    neo_ccs::Mat::from_row_major(n_rows, n_cols, dense)
}

fn ark_field_to_p3_goldilocks(col_v: &FpGoldilocks) -> p3_goldilocks::Goldilocks {
    F::from_u64(col_v.into_bigint().0[0])
}

// High-level session adapter that synthesizes Arkworks steps and exposes them to Neo
pub(crate) struct ArkStepAdapter {
    pub builder: StepCircuitBuilder<DummyMemory<FpGoldilocks>>,
    pub mem_setup: <DummyMemory<FpGoldilocks> as IVCMemory<FpGoldilocks>>::Allocator,
    pub irw: InterRoundWires,
    shape_ccs: Option<CcsStructure<::neo::F>>, // stable shape across steps
}

impl ArkStepAdapter {
    pub fn new(mut builder: StepCircuitBuilder<DummyMemory<FpGoldilocks>>) -> Self {
        let mb = builder.trace_memory_ops(());
        let mem_setup = mb.constraints();
        let irw = InterRoundWires::new(builder.rom_offset());
        Self { builder, mem_setup, irw, shape_ccs: None }
    }

    pub fn descriptor(&self) -> StepDescriptor {
        StepDescriptor {
            ccs: self.shape_ccs.as_ref().expect("shape CCS available").clone(),
            spec: self.step_spec(),
        }
    }
}

impl SessionNeoStep for ArkStepAdapter {
    type ExternalInputs = ();

    fn state_len(&self) -> usize { 0 }

    fn step_spec(&self) -> StepSpec {
        StepSpec {
            y_len: 0,
            const1_index: 0,
            y_step_indices: vec![],
            y_prev_indices: None,
            // Do not bind app inputs in CCS (transcript-only app inputs)
            app_input_indices: None,
        }
    }

    fn synthesize_step(
        &mut self,
        step_idx: usize,
        _z_prev: &[::neo::F],
        _inputs: &Self::ExternalInputs,
    ) -> StepArtifacts {
        let cs = ConstraintSystem::<FpGoldilocks>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        let next_irw = self
            .builder
            .make_step_circuit(step_idx, &mut self.mem_setup, cs.clone(), self.irw.clone())
            .expect("step synthesis");
        self.irw = next_irw;

        let step = arkworks_to_neo(cs.clone());

        if self.shape_ccs.is_none() {
            self.shape_ccs = Some(step.ccs.clone());
        }

        StepArtifacts {
            ccs: step.ccs,
            witness: step.witness,
            // Provide one app input matching const-1 to satisfy X-binder checks
            public_app_inputs: vec![::neo::F::ONE],
            spec: self.step_spec(),
        }
    }
}
