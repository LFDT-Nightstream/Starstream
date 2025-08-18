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

fn either_switch<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    switch: AllocatedNum<F>,
    case_true: AllocatedNum<F>,
    case_false: AllocatedNum<F>,
) -> AllocatedNum<F> {
    let r_true = alloc!("either_switch_true", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + switch.get_variable(),
        |lc| lc + case_true.get_variable(),
        |lc| lc + r_true.get_variable(),
    );
    let r_false = alloc!("either_switch_false", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + CS::one() - switch.get_variable(),
        |lc| lc + case_false.get_variable(),
        |lc| lc + r_false.get_variable(),
    );
    cs.enforce(
        label!(),
        |lc| lc + r_true.get_variable(),
        |lc| lc + r_false.get_variable(),
        |lc| lc,
    );
    let r = alloc!("either_switch_r", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + r_true.get_variable() + r_false.get_variable(),
        |lc| lc + CS::one(),
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

// FIXME: bad hack, AllocatedNum is bad
fn lit<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    n: F,
) -> AllocatedNum<F> {
    let lit = alloc!("lit", cs, w);
    cs.enforce(
        label!(),
        |lc| lc + lit.get_variable(),
        |lc| lc + CS::one(),
        |lc| lc + (n, CS::one()),
    );
    lit
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
    let preimage = vec![a, v, t, lit(nest!(cs), w, F::ZERO)];
    let hash = hash(nest!(cs), w, switch, preimage);
    multiset.add(nest!(cs), &hash).expect("unreachable")
}

fn push<F: PrimeField, CS: ConstraintSystem<F>>(
    mut cs: CS,
    w: &mut impl Witness<F>,
    switch: AllocatedNum<F>,
    rs: &mut AllocatedNum<F>,
    ws: &mut AllocatedNum<F>,
    idx: AllocatedNum<F>,
    data: &[AllocatedNum<F>],
) {
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
    let preimage = {
        let mut v = Vec::from(data);
        v.push(prev.clone());
        if v.len() % 2 == 1 {
            v.push(zero);
        }
        v
    };
    let updated = hash(nest!(cs), w, switch.clone(), preimage);
    *rs = memory(
        nest!(cs),
        w,
        switch.clone(),
        rs.clone(),
        idx.clone(),
        prev,
        timestamp,
    );
    *ws = memory(
        nest!(cs),
        w,
        switch,
        ws.clone(),
        idx,
        updated,
        new_timestamp,
    );
}

fn visit_enter<CS, F>(
    mut cs: CS,
    switches: &mut Switches<F>,
    io: &mut StarstreamIO<F>,
    w: &mut impl Witness<F>,
) where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let switch = switches.alloc(nest!(cs), w);
    let utxo_idx = alloc!("utxo_idx", cs, w);
    let input = alloc!("input", cs, w);
    push(
        nest!(cs),
        w,
        switch.clone(),
        &mut io.rs,
        &mut io.ws,
        utxo_idx.clone(),
        &vec![input.clone()],
    );
    let zero = lit(nest!(cs), w, F::ZERO);
    push(
        nest!(cs),
        w,
        switch,
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &vec![zero, utxo_idx, input],
    );
}

fn visit_push_coord<CS, F>(
    mut cs: CS,
    switches: &mut Switches<F>,
    io: &mut StarstreamIO<F>,
    w: &mut impl Witness<F>,
) where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let switch = switches.alloc(nest!(cs), w);
    let new_coord_idx = alloc!("new_coord_idx", cs, w);
    let input = alloc!("input", cs, w);
    let one = lit(nest!(cs), w, F::ONE);
    push(
        nest!(cs),
        w,
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        // FIXME: use hash instead of idx
        &vec![one, new_coord_idx.clone(), input.clone()],
    );
    let two = lit(nest!(cs), w, F::from_u128(2));
    push(
        nest!(cs),
        w,
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        new_coord_idx.clone(),
        // FIXME: use hash instead of idx
        &vec![two, io.coord_idx.clone(), input],
    );
    let preimage = vec![io.coord_idx.clone(), io.coord_stack.clone()];
    let new_coord_stack = hash(nest!(cs), w, switch.clone(), preimage);
    io.coord_idx = either_switch(
        nest!(cs),
        w,
        switch.clone(),
        new_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        nest!(cs),
        w,
        switch,
        new_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_pop_coord<CS, F>(
    mut cs: CS,
    switches: &mut Switches<F>,
    io: &mut StarstreamIO<F>,
    w: &mut impl Witness<F>,
) where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let switch = switches.alloc(nest!(cs), w);
    let old_coord_idx = alloc!("old_coord_idx", cs, w);
    let old_coord_stack = alloc!("old_coord_stack", cs, w);
    let three = lit(nest!(cs), w, F::from_u128(3));
    push(
        nest!(cs),
        w,
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &vec![three],
    );
    let preimage = vec![old_coord_idx.clone(), old_coord_stack.clone()];
    let should_be = hash(nest!(cs), w, switch.clone(), preimage);
    cs.enforce(
        label!(),
        |lc| lc + should_be.get_variable(),
        |lc| lc + switch.get_variable(),
        |lc| lc + io.coord_stack.get_variable(),
    );
    io.coord_idx = either_switch(
        nest!(cs),
        w,
        switch.clone(),
        old_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        nest!(cs),
        w,
        switch,
        old_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_nop<CS, F>(mut cs: CS, switches: &mut Switches<F>, w: &mut impl Witness<F>)
where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let _switch = switches.alloc(nest!(cs), w);
}

struct StarstreamIO<F: PrimeField> {
    rs: AllocatedNum<F>,
    ws: AllocatedNum<F>,
    rs_coord: AllocatedNum<F>,
    ws_coord: AllocatedNum<F>,
    coord_idx: AllocatedNum<F>,
    coord_stack: AllocatedNum<F>,
}

impl<F: PrimeField> StarstreamIO<F> {
    fn of(fields: &[AllocatedNum<F>]) -> StarstreamIO<F> {
        let [rs, ws, rs_coord, ws_coord, coord_idx, coord_stack] = fields else {
            unreachable!("maybe you passed in incorrect public input?");
        };
        let rs = rs.clone();
        let ws = ws.clone();
        let rs_coord = rs_coord.clone();
        let ws_coord = ws_coord.clone();
        let coord_idx = coord_idx.clone();
        let coord_stack = coord_stack.clone();
        StarstreamIO {
            rs,
            ws,
            rs_coord,
            ws_coord,
            coord_idx,
            coord_stack,
        }
    }
    fn to(self) -> Vec<AllocatedNum<F>> {
        let StarstreamIO {
            rs,
            ws,
            rs_coord,
            ws_coord,
            coord_idx,
            coord_stack,
        } = self;
        vec![rs, ws, rs_coord, ws_coord, coord_idx, coord_stack]
    }
}

pub struct StarstreamCircuit<W>(Arc<Mutex<W>>);

impl<W> Clone for StarstreamCircuit<W> {
    fn clone(&self) -> Self {
        let StarstreamCircuit(r) = self;
        StarstreamCircuit(r.clone())
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
         * RS of table of coordination script interactions
         * WS of table of coordination script interactions
         * idx of coordination script currently running
         * hash of coordination script stack (zero when empty)
         */
        6
    }

    fn synthesize<CS: ConstraintSystem<F>>(
        &self,
        cs: &mut CS,
        io: &[AllocatedNum<F>],
    ) -> Result<Vec<AllocatedNum<F>>, SynthesisError> {
        let mut w_guard = self.0.lock().unwrap();

        let w = &mut *w_guard;

        let (mut switches, consume_switches) = Switches::new();

        let mut io = StarstreamIO::of(io);

        visit_enter(nest!(cs), &mut switches, &mut io, w);
        visit_push_coord(nest!(cs), &mut switches, &mut io, w);
        visit_pop_coord(nest!(cs), &mut switches, &mut io, w);
        visit_nop(nest!(cs), &mut switches, w);

        switches.consume(consume_switches, nest!(cs));

        Ok(io.to())
    }

    fn non_deterministic_advice(&self) -> Vec<F> {
        Vec::new()
    }
}

struct ScriptAccIO<F: PrimeField> {
    utxo: AllocatedNum<F>,
    coord: AllocatedNum<F>,
}

impl<F: PrimeField> ScriptAccIO<F> {
    fn of(fields: &[AllocatedNum<F>]) -> ScriptAccIO<F> {
        let [utxo, coord] = fields else {
            unreachable!("maybe you passed in incorrect public input?");
        };
        let utxo = utxo.clone();
        let coord = coord.clone();
        ScriptAccIO { utxo, coord }
    }
    fn to(self) -> Vec<AllocatedNum<F>> {
        let ScriptAccIO { utxo, coord } = self;
        vec![utxo, coord]
    }
}

fn visit_utxo_script<CS, F>(
    mut cs: CS,
    switches: &mut Switches<F>,
    _io: &mut ScriptAccIO<F>,
    w: &mut impl Witness<F>,
) where
    F: PrimeField,
    CS: ConstraintSystem<F>,
{
    let _switch = switches.alloc(nest!(cs), w);
    let _interactions = alloc!("interactions", cs, w);
    unimplemented!()
}

pub struct ScriptAccCircuit<W>(Arc<Mutex<W>>);

impl<W> Clone for ScriptAccCircuit<W> {
    fn clone(&self) -> Self {
        let ScriptAccCircuit(r) = self;
        ScriptAccCircuit(r.clone())
    }
}

impl<F, W: Send + Witness<F>> StepCircuit<F> for ScriptAccCircuit<W>
where
    F: PrimeField + PrimeFieldBits,
{
    fn arity(&self) -> usize {
        /* Public input is as follows:
         * Stack of UTXO script interactions
         * Stack of coordination script interactions
         */
        2
    }

    fn synthesize<CS: ConstraintSystem<F>>(
        &self,
        cs: &mut CS,
        io: &[AllocatedNum<F>],
    ) -> Result<Vec<AllocatedNum<F>>, SynthesisError> {
        let mut w_guard = self.0.lock().unwrap();

        let w = &mut *w_guard;

        let (mut switches, consume_switches) = Switches::new();

        let mut io = ScriptAccIO::of(io);

        visit_utxo_script(nest!(cs), &mut switches, &mut io, w);

        switches.consume(consume_switches, nest!(cs));

        Ok(io.to())
    }

    fn non_deterministic_advice(&self) -> Vec<F> {
        Vec::new()
    }
}

pub struct WitnessFromVec<F>(Vec<(F, &'static str)>);

impl<F: PrimeField> Witness<F> for WitnessFromVec<F> {
    fn get(&mut self, name: &'static str, label: impl FnOnce() -> String) -> Option<F> {
        let Some((val, name_)) = self.0.pop() else {
            eprintln!("{}: expected {}", label(), name);
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
#[ignore]
fn prove_dummy() {
    let mut test = TestConstraintSystem::new();
    type F = <PallasEngine as Engine>::Scalar;
    let input = [F::ZERO, F::ZERO];
    let zero = AllocatedNum::alloc_infallible(&mut test, || F::ZERO);
    let allocated_input = [
        zero.clone(),
        zero.clone(),
        zero.clone(),
        zero.clone(),
        zero.clone(),
        zero,
    ];
    let witness = || {
        let mut v = vec![
            (F::ZERO, "switch"),
            (F::ZERO, "utxo_idx"),
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
