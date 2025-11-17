use crate::{
    circuit::{InterRoundWires, StepCircuitBuilder},
    goldilocks::FpGoldilocks,
    memory::IVCMemory,
};
use ark_ff::{Field, PrimeField};
use ark_relations::gr1cs::{ConstraintSystem, ConstraintSystemRef, OptimizationGoal};
use neo_ccs::CcsStructure;
use neo_fold::session::{NeoStep, StepArtifacts, StepSpec};
use neo_math::D;
use p3_field::PrimeCharacteristicRing;
use rand::SeedableRng as _;

pub(crate) struct StepCircuitNeo<M>
where
    M: IVCMemory<crate::F>,
{
    pub(crate) shape_ccs: CcsStructure<neo_math::F>, // stable shape across steps
    pub(crate) circuit_builder: StepCircuitBuilder<M>,
    pub(crate) irw: InterRoundWires,
    pub(crate) mem: M::Allocator,
}

impl<M> StepCircuitNeo<M>
where
    M: IVCMemory<crate::F, Params = ()>,
{
    pub fn new(
        mut circuit_builder: StepCircuitBuilder<M>,
        shape_ccs: CcsStructure<neo_math::F>,
    ) -> Self {
        let irw = InterRoundWires::new(circuit_builder.rom_offset());

        let mb = circuit_builder.trace_memory_ops(());

        Self {
            shape_ccs,
            circuit_builder,
            irw,
            mem: mb.constraints(),
        }
    }
}

impl<M> NeoStep for StepCircuitNeo<M>
where
    M: IVCMemory<crate::F>,
{
    type ExternalInputs = ();

    fn state_len(&self) -> usize {
        3
    }

    fn step_spec(&self) -> StepSpec {
        StepSpec {
            y_len: self.state_len(),
            const1_index: 0,
            y_step_indices: vec![2, 4, 6],
            app_input_indices: Some(vec![1, 3, 5]),
            m_in: 7,
        }
    }

    fn synthesize_step(
        &mut self,
        step_idx: usize,
        _z_prev: &[::neo_math::F],
        _inputs: &Self::ExternalInputs,
    ) -> StepArtifacts {
        let cs = ConstraintSystem::<crate::F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        self.irw = self
            .circuit_builder
            .make_step_circuit(step_idx, &mut self.mem, cs.clone(), self.irw.clone())
            .unwrap();

        let spec = self.step_spec();

        let mut step = arkworks_to_neo(cs.clone());

        assert!(cs.is_satisfied().unwrap());

        let padded_witness_len = step.ccs.n.max(step.ccs.m);
        step.witness.resize(padded_witness_len, neo_math::F::ZERO);

        neo_ccs::check_ccs_rowwise_zero(&step.ccs, &[], &step.witness).unwrap();
        neo_ccs::check_ccs_rowwise_zero(&self.shape_ccs, &[], &step.witness).unwrap();

        StepArtifacts {
            ccs: step.ccs,
            witness: step.witness,
            public_app_inputs: vec![],
            spec,
        }
    }
}

pub(crate) struct NeoInstance {
    pub(crate) ccs: CcsStructure<neo_math::F>,
    // instance + witness assignments
    pub(crate) witness: Vec<neo_math::F>,
}

pub(crate) fn arkworks_to_neo(cs: ConstraintSystemRef<FpGoldilocks>) -> NeoInstance {
    cs.finalize();

    let ccs = arkworks_to_neo_ccs(&cs);

    let instance_assignment = cs.instance_assignment().unwrap();
    assert_eq!(instance_assignment[0], FpGoldilocks::ONE);

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

    NeoInstance {
        ccs,
        witness: [instance, witness].concat(),
    }
}

pub(crate) fn arkworks_to_neo_ccs(
    cs: &ConstraintSystemRef<crate::F>,
) -> neo_ccs::CcsStructure<neo_math::F> {
    let matrices = &cs.to_matrices().unwrap()["R1CS"];

    let a_mat = ark_matrix_to_neo(cs, &matrices[0]);
    let b_mat = ark_matrix_to_neo(cs, &matrices[1]);
    let c_mat = ark_matrix_to_neo(cs, &matrices[2]);

    let ccs = neo_ccs::r1cs_to_ccs(a_mat, b_mat, c_mat);

    ccs.ensure_identity_first()
        .expect("ensure_identity_first should succeed");

    ccs
}

fn ark_matrix_to_neo(
    cs: &ConstraintSystemRef<FpGoldilocks>,
    sparse_matrix: &[Vec<(FpGoldilocks, usize)>],
) -> neo_ccs::Mat<neo_math::F> {
    // the final result should be a square matrix (but the sparse matrix may not be)
    let n = cs.num_constraints().max(cs.num_variables());

    dbg!(cs.num_constraints());
    dbg!(cs.num_variables());

    // TODO: would be nice to just be able to construct the sparse matrix
    let mut dense = vec![neo_math::F::from_u64(0); n * n];

    for (row_i, row) in sparse_matrix.iter().enumerate() {
        for (col_v, col_i) in row.iter() {
            dense[n * row_i + col_i] = ark_field_to_p3_goldilocks(col_v);
        }
    }

    neo_ccs::Mat::from_row_major(n, n, dense)
}

pub fn ark_field_to_p3_goldilocks(col_v: &FpGoldilocks) -> p3_goldilocks::Goldilocks {
    neo_math::F::from_u64(col_v.into_bigint().0[0])
}

pub(crate) fn setup_ajtai_for_dims(m: usize) {
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);
    let pp = neo_ajtai::setup(&mut rng, D, 4, m).expect("Ajtai setup should succeed");
    let _ = neo_ajtai::set_global_pp(pp);
}

#[cfg(test)]
mod tests {
    use crate::{
        F,
        neo::{ark_field_to_p3_goldilocks, arkworks_to_neo},
    };
    use ark_r1cs_std::{alloc::AllocVar, eq::EqGadget as _, fields::fp::FpVar};
    use ark_relations::gr1cs::ConstraintSystem;
    use p3_field::PrimeCharacteristicRing;

    #[test]
    fn test_ark_field() {
        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(20)),
            ::neo_math::F::from_u64(20)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(100)),
            ::neo_math::F::from_u64(100)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(400)),
            ::neo_math::F::from_u64(400)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(u64::MAX)),
            ::neo_math::F::from_u64(u64::MAX)
        );
    }

    #[test]
    fn test_r1cs_conversion_sat() {
        let cs = ConstraintSystem::<F>::new_ref();

        let var1 = FpVar::new_witness(cs.clone(), || Ok(F::from(1_u64))).unwrap();
        let var2 = FpVar::new_witness(cs.clone(), || Ok(F::from(1_u64))).unwrap();

        var1.enforce_equal(&var2).unwrap();

        let step = arkworks_to_neo(cs.clone());

        let neo_check =
            neo_ccs::relations::check_ccs_rowwise_zero(&step.ccs, &[], &step.witness).is_ok();

        assert_eq!(cs.is_satisfied().unwrap(), neo_check);
    }

    #[test]
    fn test_r1cs_conversion_unsat() {
        let cs = ConstraintSystem::<F>::new_ref();

        let var1 = FpVar::new_witness(cs.clone(), || Ok(F::from(1_u64))).unwrap();
        let var2 = FpVar::new_witness(cs.clone(), || Ok(F::from(2_u64))).unwrap();

        var1.enforce_equal(&var2).unwrap();

        let step = arkworks_to_neo(cs.clone());

        let neo_check =
            neo_ccs::relations::check_ccs_rowwise_zero(&step.ccs, &[], &step.witness).is_ok();

        assert_eq!(cs.is_satisfied().unwrap(), neo_check);
    }
}
