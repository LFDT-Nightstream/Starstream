mod ccs;
mod ivc_state;
mod memory;
mod opcode;
mod step;
mod witness;

use neo_application::{MemoryCheckError, check_memory_rows};
pub use relation::RelationLayout;
use starstream_interleaving_spec::Trace;

use crate::{
    ccs::{PUBLIC_INPUTS, build_relation},
    memory::MemoryId,
    witness::build_witness_vector,
};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum Error {
    #[error(transparent)]
    R1csBuildError(#[from] neo_application::R1csBuildError),
    #[error(transparent)]
    ApplicationRelationError(#[from] neo_application::ApplicationRelationError),
    #[error(transparent)]
    ColumnRegistryError(#[from] neo_application::ColumnRegistryError),
    #[error(transparent)]
    MemoryCheckError(#[from] MemoryCheckError<MemoryId>),
}

// TODO: maybe move to the spec package
pub struct TransactionProof {}

pub struct Instance {}

pub fn prove(trace: Trace) -> Result<TransactionProof, Error> {
    let relation = build_relation()?;

    let memory = crate::memory::build_memory_layout();

    let wit = step::normalize(trace);

    let mut all_rows = vec![];

    for (i, w) in wit.iter().enumerate() {
        let z = build_witness_vector(&w);

        // sanity checking
        match neo_ccs::check_ccs_rowwise_zero(
            relation.r1cs().structure(),
            &z[0..PUBLIC_INPUTS],
            &z[PUBLIC_INPUTS..],
        ) {
            Ok(_) => (),
            Err(neo_ccs::CcsError::RowFail { row }) => {
                panic!(
                    "ccs error at step: [{i}] |= {:?}",
                    relation.r1cs().catalog().rows()[row]
                );
            }
            Err(e) => panic!("{:?}", e),
        }

        all_rows.push(z);
    }

    let policy = crate::memory::sanity_checking_policy(&memory);
    let preload = crate::memory::preload_tables();

    check_memory_rows(&memory, relation.columns(), &all_rows, &preload, &policy)?;

    Ok(TransactionProof {})
}

pub fn verify(proof: TransactionProof, instance: Instance) -> Result<bool, Error> {
    // TODO: implement
    Ok(true)
}

#[cfg(test)]
mod tests {
    use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step};

    use super::*;

    #[test]
    fn test_utxo_constructor() {
        let trace = Trace::new([
            Step::NewUtxo {
                arguments: vec![0, 1, 2, 3].into(),
                resource: ResourceHandle(0).into(),
            },
            Step::EnterConstructor {
                arguments: vec![0, 1, 2, 3].into(),
            },
            Step::YieldBegin,
            Step::RegisterMethod {
                method: MethodHash([1, 1, 1, 1]),
            },
            Step::Return {
                result: StarstreamValue::from(vec![]).into(),
            },
            Step::Return {
                result: StarstreamValue::from(vec![]).into(),
            },
        ]);

        let proof = prove(trace).unwrap();
        let instance = Instance {};

        assert!(verify(proof, instance).unwrap());
    }

    #[test]
    fn test_utxo_constructor_mismatched_arg() {
        let trace = Trace::new([
            Step::NewUtxo {
                arguments: vec![1, 2, 3, 4].into(),
                resource: ResourceHandle(0).into(),
            },
            Step::EnterConstructor {
                arguments: vec![0, 1, 2, 3].into(),
            },
            Step::YieldBegin,
            Step::RegisterMethod {
                method: MethodHash([1, 1, 1, 1]),
            },
            Step::Return {
                result: StarstreamValue::from(vec![]).into(),
            },
            Step::Return {
                result: StarstreamValue::from(vec![]).into(),
            },
        ]);

        assert!(matches!(
            dbg!(prove(trace).err().unwrap()),
            Error::MemoryCheckError(MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedArgument,
                address: _,
                row: 1,
                expected: 0,
                actual: 1
            })
        ));

        let _instance = Instance {};

        // assert!(!verify(proof, instance).unwrap());
    }
}
