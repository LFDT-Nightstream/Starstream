pub mod gadget;
pub mod ic;
pub mod tracer;

use super::Address;
use crate::F;
use ark_ff::PrimeField;
use ark_r1cs_std::GR1CSVar as _;
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
pub use gadget::NebulaMemoryConstraints;
use std::iter::repeat;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemOp<F> {
    pub values: Vec<F>,
    pub timestamp: u64,
}

pub struct MemOpAllocated<F: PrimeField> {
    pub values: Vec<FpVar<F>>,
    pub timestamp: FpVar<F>,
}

impl MemOp<F> {
    pub fn padding() -> MemOp<F> {
        MemOp {
            values: vec![],
            timestamp: 0,
        }
    }

    pub fn allocate(
        &self,
        cs: ConstraintSystemRef<F>,
        pad_to: usize,
    ) -> Result<MemOpAllocated<F>, SynthesisError>
    where
        F: PrimeField,
    {
        let fp = F::from(0);
        Ok(MemOpAllocated {
            values: self
                .values
                .iter()
                .chain(repeat(&fp))
                .take(pad_to)
                .map(|v| FpVar::new_witness(cs.clone(), || Ok(*v)))
                .collect::<Result<Vec<_>, _>>()?,
            timestamp: FpVar::new_witness(cs.clone(), || Ok(F::from(self.timestamp)))?,
        })
    }
}

impl MemOpAllocated<F>
where
    F: PrimeField,
{
    // pub fn padding(
    //     cs: ConstraintSystemRef<F>,
    //     segment_size: u64,
    // ) -> Result<MemOpAllocated<F>, SynthesisError> {
    //     Ok(MemOpAllocated {
    //         values: std::iter::repeat_with(|| FpVar::new_witness(cs.clone(), || Ok(F::ZERO)))
    //             .take(segment_size as usize)
    //             .collect::<Result<Vec<_>, _>>()?,
    //         timestamp: FpVar::new_witness(cs, || Ok(F::from(0)))?,
    //     })
    // }

    pub fn debug_values(&self) -> Vec<F> {
        self.values.iter().map(|v| v.value().unwrap()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logging::setup_logger;
    use crate::memory::IVCMemory;
    use crate::memory::IVCMemoryAllocated;
    use crate::memory::MemType;
    use crate::memory::nebula::tracer::NebulaMemory;
    use crate::memory::nebula::tracer::NebulaMemoryParams;
    use ark_r1cs_std::alloc::AllocVar;
    use ark_r1cs_std::fields::fp::FpVar;
    use ark_r1cs_std::prelude::Boolean;
    use ark_relations::gr1cs::ConstraintSystem;

    #[test]
    fn test_nebula_memory_constraints_satisfiability() {
        setup_logger();

        let mut memory = NebulaMemory::<1>::new(NebulaMemoryParams {
            unsound_disable_poseidon_commitment: false,
        });

        memory.register_mem(1, 2, MemType::Ram, "test_segment");

        let address = Address { addr: 10, tag: 1 };
        let initial_values = vec![F::from(42), F::from(100)];
        memory.init(address.clone(), initial_values.clone());
        memory.conditional_read(true, address.clone());

        memory.conditional_write(true, address.clone(), vec![F::from(123), F::from(456)]);
        memory.conditional_write(false, address.clone(), vec![F::from(0), F::from(0)]);

        assert_eq!(
            memory.conditional_read(false, address.clone()),
            vec![F::from(0), F::from(0)]
        );

        assert_eq!(
            memory.conditional_read(true, address.clone()),
            vec![F::from(123), F::from(456)]
        );

        let mut constraints = memory.constraints();

        let cs = ConstraintSystem::<F>::new_ref();

        constraints.start_step(cs.clone()).unwrap();

        let address_var = Address { addr: 10, tag: 1 }.allocate(cs.clone()).unwrap();

        let true_cond = Boolean::new_witness(cs.clone(), || Ok(true)).unwrap();
        let false_cond = Boolean::new_witness(cs.clone(), || Ok(false)).unwrap();

        let _read_result = constraints
            .conditional_read(&true_cond, &address_var)
            .unwrap();

        let write_vals = vec![
            FpVar::new_witness(cs.clone(), || Ok(F::from(123))).unwrap(),
            FpVar::new_witness(cs.clone(), || Ok(F::from(456))).unwrap(),
        ];

        let false_write_vals = vec![
            FpVar::new_witness(cs.clone(), || Ok(F::from(0))).unwrap(),
            FpVar::new_witness(cs.clone(), || Ok(F::from(0))).unwrap(),
        ];

        constraints
            .conditional_write(&true_cond, &address_var, &write_vals)
            .unwrap();

        constraints
            .conditional_write(&false_cond, &address_var, &false_write_vals)
            .unwrap();

        constraints
            .conditional_read(&false_cond, &address_var)
            .unwrap();

        let _read_result2 = constraints
            .conditional_read(&true_cond, &address_var)
            .unwrap();

        constraints.finish_step(true).unwrap();

        assert!(
            cs.is_satisfied().unwrap(),
            "Constraint system should be satisfiable"
        );
    }

    // NOTE: for folding, we need conditional reads and conditional writes to be
    // gated, but we still want to keep the same shape across steps
    #[test]
    fn test_circuit_shape_consistency_across_conditions() {
        setup_logger();
        fn create_constraint_system_with_conditions(
            read_cond: bool,
            write_cond: bool,
        ) -> ark_relations::gr1cs::ConstraintSystemRef<F> {
            let mut memory = NebulaMemory::<1>::new(NebulaMemoryParams {
                unsound_disable_poseidon_commitment: false,
            });
            memory.register_mem(1, 2, MemType::Ram, "test_segment");

            let address = Address { addr: 10, tag: 1 };
            let initial_values = vec![F::from(42), F::from(100)];
            memory.init(address.clone(), initial_values);

            memory.conditional_read(read_cond, address.clone());
            memory.conditional_write(
                write_cond,
                address.clone(),
                vec![F::from(123), F::from(456)],
            );

            let mut constraints = memory.constraints();
            let cs = ConstraintSystem::<F>::new_ref();

            constraints.start_step(cs.clone()).unwrap();

            let address_var = Address { addr: 10, tag: 1 }.allocate(cs.clone()).unwrap();

            let cond_read = Boolean::new_witness(cs.clone(), || Ok(read_cond)).unwrap();
            let cond_write = Boolean::new_witness(cs.clone(), || Ok(write_cond)).unwrap();

            let _read_result = constraints
                .conditional_read(&cond_read, &address_var)
                .unwrap();

            let write_vals = vec![
                FpVar::new_witness(cs.clone(), || Ok(F::from(if write_cond { 123 } else { 0 })))
                    .unwrap(),
                FpVar::new_witness(cs.clone(), || Ok(F::from(if write_cond { 456 } else { 0 })))
                    .unwrap(),
            ];

            constraints
                .conditional_write(&cond_write, &address_var, &write_vals)
                .unwrap();

            constraints.finish_step(true).unwrap();

            std::mem::drop(constraints);

            cs
        }

        let condition_combinations = [(true, true), (true, false), (false, true), (false, false)];
        let constraint_systems: Vec<_> = condition_combinations
            .iter()
            .map(|&(read_cond, write_cond)| {
                create_constraint_system_with_conditions(read_cond, write_cond)
            })
            .collect();

        let reference_cs = &constraint_systems[0];
        let expected_constraints = reference_cs.num_constraints();
        let expected_instance_vars = reference_cs.num_instance_variables();
        let expected_witness_vars = reference_cs.num_witness_variables();

        for (i, cs) in constraint_systems.iter().enumerate() {
            let (read_cond, write_cond) = condition_combinations[i];

            assert_eq!(
                cs.num_constraints(),
                expected_constraints,
                "Number of constraints should be the same for ({},{})",
                read_cond,
                write_cond
            );

            assert_eq!(
                cs.num_instance_variables(),
                expected_instance_vars,
                "Number of instance variables should be the same for ({},{})",
                read_cond,
                write_cond
            );

            assert_eq!(
                cs.num_witness_variables(),
                expected_witness_vars,
                "Number of witness variables should be the same for ({},{})",
                read_cond,
                write_cond
            );

            assert!(
                cs.is_satisfied().unwrap(),
                "Constraint system ({},{}) should be satisfiable",
                read_cond,
                write_cond
            );
        }

        println!(
            "Circuit shape consistency verified: {} constraints, {} instance vars, {} witness vars",
            expected_constraints, expected_instance_vars, expected_witness_vars
        );
    }

    #[test]
    fn test_scan_batch_size_multi_step() {
        setup_logger();

        const SCAN_BATCH_SIZE: usize = 2;
        let num_steps = 3;
        let total_addresses = SCAN_BATCH_SIZE * num_steps; // 6 addresses

        let mut memory = NebulaMemory::<SCAN_BATCH_SIZE>::new(NebulaMemoryParams::default());
        memory.register_mem(1, 2, MemType::Ram, "test_segment");

        let addresses: Vec<Address<u64>> = (0..total_addresses)
            .map(|i| Address {
                addr: i as u64,
                tag: 1,
            })
            .collect();

        for (i, addr) in addresses.iter().enumerate() {
            memory.init(
                addr.clone(),
                vec![F::from(i as u64 * 10), F::from(i as u64 * 10 + 1)],
            );
        }

        for (step, addr) in addresses.iter().enumerate().take(num_steps) {
            memory.conditional_read(true, addr.clone());
            memory.conditional_write(
                true,
                addr.clone(),
                vec![F::from(100 + step as u64), F::from(200 + step as u64)],
            );
        }

        let mut constraints = memory.constraints();

        for step in 0..num_steps {
            let cs = ConstraintSystem::<F>::new_ref();
            constraints.start_step(cs.clone()).unwrap();

            let address_var = Address {
                addr: step as u64,
                tag: 1,
            }
            .allocate(cs.clone())
            .unwrap();

            let true_cond = Boolean::new_witness(cs.clone(), || Ok(true)).unwrap();

            let _read_result = constraints
                .conditional_read(&true_cond, &address_var)
                .unwrap();

            let write_vals = vec![
                FpVar::new_witness(cs.clone(), || Ok(F::from(100 + step as u64))).unwrap(),
                FpVar::new_witness(cs.clone(), || Ok(F::from(200 + step as u64))).unwrap(),
            ];

            constraints
                .conditional_write(&true_cond, &address_var, &write_vals)
                .unwrap();

            let is_last_step = step == num_steps - 1;
            constraints.finish_step(is_last_step).unwrap();

            assert!(
                cs.is_satisfied().unwrap(),
                "Constraint system should be satisfiable for step {}",
                step
            );
        }

        println!(
            "Multi-step scan batch test completed: {} addresses, {} steps, batch size {}",
            total_addresses, num_steps, SCAN_BATCH_SIZE
        );
    }
}
