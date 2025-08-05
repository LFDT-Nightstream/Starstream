#[cfg(test)]
use ff::Field;
use ff::{PrimeField, PrimeFieldBits};
use nova::frontend::PoseidonConstants;
use nova::frontend::gadgets::poseidon::poseidon_hash_allocated;
#[cfg(test)]
use nova::frontend::test_cs::TestConstraintSystem;
use nova::frontend::{ConstraintSystem, SynthesisError, num::AllocatedNum};
use nova::nebula::rs::StepCircuit;
#[cfg(test)]
use nova::nebula::rs::{PublicParams, RecursiveSNARK};
#[cfg(test)]
use nova::provider::PallasEngine;
#[cfg(test)]
use nova::traits::{Engine, snark::default_ck_hint};
use std::sync::{Arc, Mutex};
use typenum::U4;

// FIXME: horrifying
static CONSTRAINT_COUNTER: Mutex<i64> = Mutex::new(0);

macro_rules! label_ {
    () => {{ || format!("{}:{}:{}", file!(), line!(), column!()).replace("/", ".") }};
}

macro_rules! label {
    () => {{
        || {
            let mut counter = CONSTRAINT_COUNTER.lock().unwrap();
            let n = *counter;
            *counter += 1;
            format!("{}:{}:{}:{}", file!(), line!(), column!(), n).replace("/", ".")
        }
    }};
}

macro_rules! nest {
    ($cs:expr) => {{ $cs.namespace(label_!()) }};
}

macro_rules! alloc {
    ($name:literal, $cs:expr, $w:expr) => {{
        match $w.get($name, label_!()) {
            Some(val) => AllocatedNum::alloc_infallible(nest!($cs), || val),
            None => AllocatedNum::alloc_infallible(nest!($cs), || {
                unreachable!("witness requested when unavailable")
            }),
        }
    }};
}

// FIXME: implement coordination script support
pub struct StarstreamCircuit<W>(Arc<Mutex<W>>);

impl<W> Clone for StarstreamCircuit<W> {
    fn clone(&self) -> Self {
        let StarstreamCircuit(r) = self;
        StarstreamCircuit(r.clone())
    }
}

trait Witness<F>: Sync {
    fn get(&mut self, name: &'static str, label: impl FnOnce() -> String) -> Option<F>;
}

struct Switches<F: PrimeField>(Vec<AllocatedNum<F>>);
#[must_use]
struct ConsumeSwitches;

impl<F> Switches<F>
where
    F: PrimeField,
{
    fn new() -> (Switches<F>, ConsumeSwitches) {
        (Switches(vec![]), ConsumeSwitches)
    }
    fn alloc(
        &mut self,
        mut cs: impl ConstraintSystem<F>,
        w: &mut impl Witness<F>,
    ) -> AllocatedNum<F> {
        let switch = alloc!("switch", cs, w);
        self.0.push(switch.clone());
        switch
    }
    fn consume<CS: ConstraintSystem<F>>(self, _marker: ConsumeSwitches, mut cs: CS) {
        cs.enforce(
            label!(),
            |lc| {
                self.0
                    .iter()
                    .fold(lc, |acc, switch| acc + switch.get_variable())
            },
            |lc| lc + CS::one(),
            |lc| lc + CS::one(),
        );

        for switch in &self.0 {
            cs.enforce(
                label!(),
                |lc| lc + switch.get_variable(),
                |lc| lc + CS::one() - switch.get_variable(),
                |lc| lc,
            );
        }

        std::mem::forget(self)
    }
}

fn if_switch<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    switch: AllocatedNum<F>,
    n: AllocatedNum<F>,
) -> AllocatedNum<F> {
    let r = alloc!("if_result", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + switch.get_variable(),
        |lc| lc + n.get_variable(),
        |lc| lc + r.get_variable(),
    );
    r
}
fn hash<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    switch: AllocatedNum<F>,
    input: Vec<AllocatedNum<F>>,
) -> AllocatedNum<F> {
    let constants = PoseidonConstants::<F, U4>::new();
    // FIXME: poseidon_hash_allocated doesn't take a switch,
    // so all its constraints cost even if they aren't used.
    let hash = poseidon_hash_allocated(nest!(cs), input, &constants).expect("unreachable");
    if_switch(nest!(cs), w, switch, hash)
}

// adds H(a, v, t) to the multiset
fn memory<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    switch: AllocatedNum<F>,
    multiset: AllocatedNum<F>,
    a: AllocatedNum<F>,
    v: AllocatedNum<F>,
    t: AllocatedNum<F>,
) -> AllocatedNum<F> {
    let zero = alloc!("zero", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + zero.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc,
    );
    let preimage = vec![a, v, t, zero];
    let hash = hash(nest!(cs), w, switch, preimage);
    multiset.add(nest!(cs), &hash).expect("unreachable")
}

fn visit_enter<CS, F>(
    mut cs: CS,
    switches: &mut Switches<F>,
    rs: &mut AllocatedNum<F>,
    ws: &mut AllocatedNum<F>,
    w: &mut impl Witness<F>,
) where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let switch = switches.alloc(nest!(cs), w);
    let utxo_index = alloc!("utxo_index", cs, w);
    let input = alloc!("input", cs, w);
    let output = alloc!("output", cs, w);
    let prev = alloc!("prev", cs, w);
    let timestamp = alloc!("timestamp", cs, w);
    let new_timestamp = alloc!("new_timestamp", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + new_timestamp.get_variable() - timestamp.get_variable(),
        |lc| lc + switch.get_variable(),
        |lc| lc + switch.get_variable(),
    );
    let zero = alloc!("zero", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + zero.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc,
    );
    let preimage = vec![input, output, prev.clone(), zero];
    let updated = hash(nest!(cs), w, switch.clone(), preimage);
    *rs = memory(
        nest!(cs),
        w,
        switch.clone(),
        rs.clone(),
        utxo_index.clone(),
        prev,
        timestamp,
    );
    *ws = memory(
        nest!(cs),
        w,
        switch,
        ws.clone(),
        utxo_index,
        updated,
        new_timestamp,
    );
}

fn visit_nop<CS, F>(mut cs: CS, switches: &mut Switches<F>, w: &mut impl Witness<F>)
where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let _switch = switches.alloc(nest!(cs), w);
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

impl<F, W: Send + Witness<F>> StepCircuit<F> for StarstreamCircuit<W>
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

        let (mut switches, consume_switches) = Switches::new();

        let mut public_input = PublicInput::of(public_input);

        visit_enter(
            nest!(cs),
            &mut switches,
            &mut public_input.rs,
            &mut public_input.ws,
            w,
        );
        visit_nop(nest!(cs), &mut switches, w);

        switches.consume(consume_switches, nest!(cs));

        Ok(public_input.to())
    }

    fn non_deterministic_advice(&self) -> Vec<F> {
        Vec::new()
    }
}

pub struct WitnessFromVec<F>(Vec<(F, &'static str)>);

impl<F: PrimeField> Witness<F> for WitnessFromVec<F> {
    fn get(&mut self, name: &'static str, label: impl FnOnce() -> String) -> Option<F> {
        let Some((val, name_)) = self.0.pop() else {
            eprintln!("not enough witnesses available: {} at {}", name, label());
            return Some(F::ZERO);
        };
        if name != name_ {
            eprintln!("{}: expected {}, got {}", label(), name, name_);
        }
        Some(val)
    }
}

pub struct NoWitness;

impl<F> Witness<F> for NoWitness {
    fn get(&mut self, _name: &'static str, _label: impl FnOnce() -> String) -> Option<F> {
        None
    }
}

#[test]
fn prove_dummy() {
    let mut test = TestConstraintSystem::new();
    type F = <PallasEngine as Engine>::Scalar;
    let input = [F::ZERO, F::ZERO];
    let zero = AllocatedNum::alloc_infallible(&mut test, || F::ZERO);
    let allocated_input = [zero.clone(), zero];
    let witness = || {
        let mut v = vec![
            (F::ZERO, "switch"),
            (F::ZERO, "utxo_index"),
            (F::ZERO, "input"),
            (F::ZERO, "output"),
            (F::ZERO, "prev"),
            (F::ZERO, "timestamp"),
            (F::ZERO, "new_timestamp"),
            (F::ZERO, "zero"),
            (F::ZERO, "if_result"),
            (F::ZERO, "zero"),
            (F::ZERO, "if_result"),
            (F::ZERO, "zero"),
            (F::ZERO, "if_result"),
            (F::ONE, "switch"),
        ];
        v.reverse();
        v
    };
    StarstreamCircuit(Arc::new(Mutex::new(WitnessFromVec(witness()))))
        .synthesize(&mut test, &allocated_input)
        .expect(label_!()().as_ref());
    println!(
        "printing unsatisfied constraints\n{:?}\nprinting unsatisfied constraints done",
        test.which_is_unsatisfied()
    );
    assert!(test.is_satisfied());
    let pp: PublicParams<PallasEngine> = PublicParams::setup(
        &StarstreamCircuit(Arc::new(Mutex::new(NoWitness))),
        &*default_ck_hint(),
        &*default_ck_hint(),
    );
    let mut rs = RecursiveSNARK::new(
        &pp,
        &StarstreamCircuit(Arc::new(Mutex::new(WitnessFromVec(witness())))),
        &input,
    )
    .expect(label_!()().as_ref());
    let ic = F::ZERO;
    rs.prove_step(
        &pp,
        &StarstreamCircuit(Arc::new(Mutex::new(WitnessFromVec(witness())))),
        ic,
    )
    .expect(label_!()().as_ref());
    let ic = rs.increment_commitment(
        &pp,
        &StarstreamCircuit(Arc::new(Mutex::new(WitnessFromVec(witness())))),
    );
    let num_steps = rs.num_steps();
    rs.verify(&pp, num_steps, &input, ic)
        .expect(label_!()().as_ref());
}
