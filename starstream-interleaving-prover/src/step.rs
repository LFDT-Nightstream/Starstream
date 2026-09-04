use neo_math::F;
use starstream_interleaving_spec::{Step, Trace};

use crate::{ivc_state::CurrPhase, opcode::Opcode};

pub(crate) fn normalize(trace: &Trace) -> Vec<Wit> {
    let mut wit: Vec<Wit> = vec![];
    let mut curr_phase = CurrPhase::Executing;
    // Quint's initial coordinator frame occupies the first stack slot.
    let mut call_sp = F::new(1);

    for step in &trace.0 {
        let opcode = Opcode::from(step);
        let curr_phase_before = curr_phase;
        let curr_phase_after = match opcode {
            Opcode::NewUtxo => CurrPhase::CtorEnterPending,
            Opcode::CallMethod => CurrPhase::MethodEnterPending,
            Opcode::EnterConstructor | Opcode::EnterMethod | Opcode::Return => CurrPhase::Executing,
            Opcode::YieldBegin => CurrPhase::Yield,
            Opcode::RegisterMethod => curr_phase_before,
        };
        curr_phase = curr_phase_after;

        let call_sp_before = call_sp;
        let call_sp_after = if opcode.pushes_to_call_stack() {
            call_sp_before + F::new(1)
        } else if opcode.pops_from_call_stack() {
            call_sp_before - F::new(1)
        } else {
            call_sp_before
        };
        call_sp = call_sp_after;

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
            curr_phase_before,
            curr_phase_after,
            call_sp_before,
            call_sp_after,
        })
    }

    wit
}

pub(crate) struct Wit {
    pub(crate) opcode: Opcode,
    pub(crate) expected_arguments: Option<Vec<F>>,
    pub(crate) curr_phase_before: CurrPhase,
    pub(crate) curr_phase_after: CurrPhase,
    pub(crate) call_sp_before: F,
    pub(crate) call_sp_after: F,
}
