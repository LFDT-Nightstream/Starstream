use crate::{
    circuit::{InterRoundWires, StepCircuitBuilder},
    goldilocks::FpGoldilocks,
    memory::IVCMemory,
};
use ark_ff::{Field, PrimeField};
use ark_relations::gr1cs::{ConstraintSystem, ConstraintSystemRef, OptimizationGoal};
use neo::{CcsStructure, F, NeoStep, StepArtifacts, StepSpec};
use p3_field::PrimeCharacteristicRing;

pub(crate) struct StepCircuitNeo<M>
where
    M: IVCMemory<crate::F>,
{
    pub(crate) shape_ccs: Option<CcsStructure<::neo::F>>, // stable shape across steps
    pub(crate) circuit_builder: StepCircuitBuilder<M>,
    pub(crate) irw: InterRoundWires,
    pub(crate) mem: M::Allocator,

    debug_prev_state: Option<Vec<neo::F>>,
}

impl<M> StepCircuitNeo<M>
where
    M: IVCMemory<crate::F, Params = ()>,
{
    pub fn new(mut circuit_builder: StepCircuitBuilder<M>) -> Self {
        let irw = InterRoundWires::new(circuit_builder.rom_offset());

        let mb = circuit_builder.trace_memory_ops(());

        Self {
            shape_ccs: None,
            circuit_builder,
            irw,
            mem: mb.constraints(),
            debug_prev_state: None,
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
            app_input_indices: None,
        }
    }

    fn synthesize_step(
        &mut self,
        step_idx: usize,
        _z_prev: &[::neo::F],
        _inputs: &Self::ExternalInputs,
    ) -> StepArtifacts {
        let cs = ConstraintSystem::<crate::F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        self.irw = self
            .circuit_builder
            .make_step_circuit(step_idx, &mut self.mem, cs.clone(), self.irw.clone())
            .unwrap();

        let spec = self.step_spec();

        let step = arkworks_to_neo(cs.clone());

        if self.shape_ccs.is_none() {
            self.shape_ccs = Some(step.ccs.clone());
        }

        // State chaining validation removed - no longer needed with updated neo version

        self.debug_prev_state.replace(
            spec.y_step_indices
                .iter()
                .map(|i| step.witness[*i])
                .collect::<Vec<_>>(),
        );

        StepArtifacts {
            ccs: step.ccs,
            witness: step.witness,
            public_app_inputs: vec![],
            spec,
        }
    }
}

pub(crate) struct NeoInstance {
    pub(crate) ccs: CcsStructure<F>,
    // instance + witness assignments
    pub(crate) witness: Vec<F>,
}

pub(crate) fn arkworks_to_neo(cs: ConstraintSystemRef<FpGoldilocks>) -> NeoInstance {
    cs.finalize();

    let matrices = &cs.to_matrices().unwrap()["R1CS"];

    let a_mat = ark_matrix_to_neo(&cs, &matrices[0]);
    let b_mat = ark_matrix_to_neo(&cs, &matrices[1]);
    let c_mat = ark_matrix_to_neo(&cs, &matrices[2]);

    let ccs = neo_ccs::r1cs_to_ccs(a_mat, b_mat, c_mat);

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

#[cfg(test)]
mod tests {
    use crate::{
        F,
        neo::{ark_field_to_p3_goldilocks, arkworks_to_neo},
    };
    use ark_r1cs_std::{alloc::AllocVar, eq::EqGadget as _, fields::fp::FpVar};
    use ark_relations::gr1cs::{self, ConstraintSystem};
    use neo::{
        CcsStructure, FoldingSession, NeoParams, NeoStep, StepArtifacts, StepDescriptor, StepSpec,
    };
    use p3_field::PrimeCharacteristicRing;
    use p3_field::PrimeField;

    #[test]
    fn test_ark_field() {
        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(20)),
            ::neo::F::from_u64(20)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(100)),
            ::neo::F::from_u64(100)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(400)),
            ::neo::F::from_u64(400)
        );

        assert_eq!(
            ark_field_to_p3_goldilocks(&F::from(u64::MAX)),
            ::neo::F::from_u64(u64::MAX)
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

    pub(crate) struct ArkStepAdapter {
        shape_ccs: Option<CcsStructure<::neo::F>>, // stable shape across steps
    }

    impl ArkStepAdapter {
        pub fn new() -> Self {
            Self { shape_ccs: None }
        }
    }

    impl NeoStep for ArkStepAdapter {
        type ExternalInputs = ();

        fn state_len(&self) -> usize {
            1
        }

        fn step_spec(&self) -> StepSpec {
            StepSpec {
                y_len: 1,
                const1_index: 0,
                y_step_indices: vec![3],
                app_input_indices: None,
            }
        }

        fn synthesize_step(
            &mut self,
            _step_idx: usize,
            z_prev: &[::neo::F],
            _inputs: &Self::ExternalInputs,
        ) -> StepArtifacts {
            let i = z_prev
                .first()
                .map(|z_prev| z_prev.as_canonical_biguint().to_u64_digits()[0])
                .unwrap_or(0);

            // TODO: i should really be step_idx here
            let cs = make_step(i);

            let step = arkworks_to_neo(cs.clone());

            if self.shape_ccs.is_none() {
                self.shape_ccs = Some(step.ccs.clone());
            }

            StepArtifacts {
                ccs: step.ccs,
                witness: step.witness,
                public_app_inputs: vec![],
                spec: self.step_spec(),
            }
        }
    }
    #[test]
    fn test_arkworks_to_neo() {
        let params = NeoParams::goldilocks_small_circuits();

        let mut session = FoldingSession::new(&params, None, 0, neo::AppInputBinding::WitnessBound);

        let mut adapter = ArkStepAdapter::new();
        let _step_result = session.prove_step(&mut adapter, &()).unwrap();
        let _step_result = session.prove_step(&mut adapter, &()).unwrap();

        let (chain, step_ios) = session.finalize();
        let descriptor = StepDescriptor {
            ccs: adapter.shape_ccs.as_ref().unwrap().clone(),
            spec: adapter.step_spec().clone(),
        };

        let ok = neo::verify_chain_with_descriptor(
            &descriptor,
            &chain,
            &[::neo::F::from_u64(0)],
            &params,
            &step_ios,
            neo::AppInputBinding::WitnessBound,
        )
        .unwrap();

        assert!(ok, "verify chain");
    }

    fn make_step(i: u64) -> gr1cs::ConstraintSystemRef<F> {
        let cs = ConstraintSystem::<F>::new_ref();

        let var1 = FpVar::new_input(cs.clone(), || Ok(F::from(i))).unwrap();
        let delta = FpVar::new_input(cs.clone(), || Ok(F::from(1))).unwrap();
        let var2 = FpVar::new_input(cs.clone(), || Ok(F::from(i + 1))).unwrap();

        (var1.clone() + delta.clone()).enforce_equal(&var2).unwrap();
        (var1.clone() + delta.clone()).enforce_equal(&var2).unwrap();
        (var1 + delta).enforce_equal(&var2).unwrap();

        let is_sat = cs.is_satisfied().unwrap();

        if !is_sat {
            let trace = cs.which_is_unsatisfied().unwrap().unwrap();
            panic!(
                "The constraint system was not satisfied; here is a trace indicating which constraint was unsatisfied: \n{trace}",
            )
        }

        cs
    }
}
