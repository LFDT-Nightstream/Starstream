use neo_math::F;
use starstream_interleaving_spec::{Step, Trace};

use crate::opcode::Opcode;

pub(crate) fn normalize(trace: Trace) -> Vec<Wit> {
    let mut wit: Vec<Wit> = vec![];
    for step in trace.0 {
        let opcode = Opcode::from(&step);

        let mut expected_arguments = None;

        match step {
            Step::NewUtxo {
                arguments,
                resource,
            } => {
                expected_arguments
                    .replace(arguments.0.into_iter().map(|x| F::new(x as u64)).collect());
            }
            Step::EnterConstructor { arguments } => {
                expected_arguments
                    .replace(arguments.0.into_iter().map(|x| F::new(x as u64)).collect());
            }
            Step::YieldBegin => {}
            Step::RegisterMethod { method } => {}
            Step::Return { result } => {}
            Step::CallMethod {
                resource,
                method,
                arguments,
                result,
            } => {
                expected_arguments
                    .replace(arguments.0.into_iter().map(|x| F::new(x as u64)).collect());
            }
            Step::EnterMethod { method, arguments } => {
                expected_arguments
                    .replace(arguments.0.into_iter().map(|x| F::new(x as u64)).collect());
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
