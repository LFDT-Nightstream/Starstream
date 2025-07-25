use ff::{PrimeField, PrimeFieldBits};
use nova::frontend::gadgets::Assignment;
use nova::frontend::{
    ConstraintSystem, SynthesisError,
    num::AllocatedNum,
    {AllocatedBit, Boolean},
};
use nova::nebula::rs::StepCircuit;
use std::sync::{Arc, Mutex};

// FIXME: implement coordination script support
pub struct StarstreamCircuit<W>(Arc<Mutex<Witness<W>>>);

impl<W> Clone for StarstreamCircuit<W> {
    fn clone(&self) -> Self {
        let StarstreamCircuit(r) = self;
        StarstreamCircuit(r.clone())
    }
}

trait WitnessImpl<F>: Sync {
    fn get(&mut self) -> F;
}

struct Witness<W>(W);

impl<W> Witness<W> {
    fn get<F>(&mut self) -> impl FnOnce() -> Result<F, SynthesisError>
    where
        W: WitnessImpl<F>,
    {
        let f = self.0.get();
        || Ok(f)
    }
}

fn visit_enter<CS, F>(
    mut cs: CS,
    switchboard_vars: &mut SwitchBoardCircuitVars<F>,
    rs: &mut AllocatedNum<F>,
    ws: &mut AllocatedNum<F>,
    w: &mut Witness<impl WitnessImpl<F>>,
) -> Result<(), SynthesisError>
where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let switch = AllocatedNum::alloc(cs, w.get());
    unimplemented!()
}

fn visit_dummy<CS, F>(
    mut cs: CS,
    switchboard_vars: &mut SwitchBoardCircuitVars<F>,
) -> Result<(), SynthesisError>
where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    unimplemented!()
}

struct PublicInput<F: PrimeField> {
    rs: AllocatedNum<F>,
    ws: AllocatedNum<F>,
}

impl<F: PrimeField> PublicInput<F> {
    fn of(fields: &[AllocatedNum<F>]) -> PublicInput<F> {
        let [rs, ws] = fields else {
            unreachable!();
        };
        let rs = rs.clone();
        let ws = ws.clone();
        PublicInput { rs, ws }
    }
    fn to(self) -> Vec<AllocatedNum<F>> {
        let PublicInput { rs, ws } = self;
        vec![rs, ws]
    }
}

impl<F, W: Send + WitnessImpl<F>> StepCircuit<F> for StarstreamCircuit<W>
where
    F: PrimeField + PrimeFieldBits,
{
    fn arity(&self) -> usize {
        /* Public input is as follows:
         * RS of table of UTXO interactions
         * WS of table of UTXO interactions
         */
        2
    }

    fn synthesize<CS: ConstraintSystem<F>>(
        &self,
        cs: &mut CS,
        public_input: &[AllocatedNum<F>],
    ) -> Result<Vec<AllocatedNum<F>>, SynthesisError> {
        let mut w_guard = self.0.lock().unwrap();

        let w = &mut *w_guard;

        let mut switchboard_vars = SwitchBoardCircuitVars::new();

        let mut public_input = PublicInput::of(public_input);

        visit_enter(
            cs.namespace(|| "enter"),
            &mut switchboard_vars,
            &mut public_input.rs,
            &mut public_input.ws,
            w,
        )?;
        visit_dummy(cs.namespace(|| "dummy"), &mut switchboard_vars)?;

        let switches = switchboard_vars.switches();

        // 1. Single switch constraint:
        cs.enforce(
            || "single switch",
            |lc| {
                switches
                    .iter()
                    .fold(lc, |acc, switch| acc + switch.get_variable())
            },
            |lc| lc + CS::one(),
            |lc| lc + CS::one(),
        );

        // 2. Binary switch constraints:
        for (i, switch) in switches.iter().enumerate() {
            cs.enforce(
                || format!("binary switch {i}"),
                |lc| lc + switch.get_variable(),
                |lc| lc + CS::one() - switch.get_variable(),
                |lc| lc,
            );
        }

        Ok(public_input.to())
    }

    fn non_deterministic_advice(&self) -> Vec<F> {
        Vec::new()
    }
}

pub struct SwitchBoardCircuitVars<F>
where
    F: PrimeField,
{
    switches: Vec<AllocatedNum<F>>,
}

impl<F> SwitchBoardCircuitVars<F>
where
    F: PrimeField,
{
    fn new() -> Self {
        Self {
            switches: Vec::new(),
        }
    }

    fn push_switch(&mut self, switch: AllocatedNum<F>) {
        self.switches.push(switch);
    }

    fn switches(&self) -> &Vec<AllocatedNum<F>> {
        &self.switches
    }
}
