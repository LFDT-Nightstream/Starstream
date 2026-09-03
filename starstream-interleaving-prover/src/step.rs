use neo_math::F;
use starstream_interleaving_spec::{Step, Trace};

use crate::opcode::Opcode;

pub(crate) fn normalize(trace: &Trace) -> Vec<Wit> {
    let mut wit: Vec<Wit> = vec![];
    for step in &trace.0 {
        let opcode = Opcode::from(step);

        let mut expected_arguments = None;

        match step {
            Step::NewUtxo {
                arguments,
                resource: _,
            } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
            }
            Step::EnterConstructor { arguments } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
            }
            Step::YieldBegin => {}
            Step::RegisterMethod { method: _ } => {}
            Step::Return { result: _ } => {}
            Step::CallMethod {
                resource: _,
                method: _,
                arguments,
                result: _,
            } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
            }
            Step::EnterMethod {
                method: _,
                arguments,
            } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
            }
        }

        wit.push(Wit {
            opcode,
            expected_arguments,
        })
    }

    wit
}

pub(crate) struct Wit {
    pub(crate) opcode: Opcode,
    pub(crate) expected_arguments: Option<Vec<F>>,
}
