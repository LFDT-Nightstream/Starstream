use ff::{PrimeField, PrimeFieldBits};
use nova::frontend::test_cs::TestConstraintSystem;
use std::marker::PhantomData;

use nova::frontend::gadgets::poseidon::poseidon_hash_allocated;
use nova::frontend::{ConstraintSystem, SynthesisError, num::AllocatedNum};
use nova::frontend::{LinearCombination, PoseidonConstants, Variable};
use nova::nebula::rs::StepCircuit;
use std::ops::{Add, Mul, Sub};
use std::sync::{Arc, Mutex};
use typenum::U4;

#[derive(Clone, Copy, Default)]
pub struct Location {
    pub label: &'static str,
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
}

pub enum Locations<'a> {
    Nil,
    Cons(Location, &'a Locations<'a>),
}

fn format_location(
    Location {
        label,
        file: _,
        line,
        column: _,
    }: Location,
) -> String {
    if label.is_empty() {
        format!("{}", line)
    } else {
        format!("{}@{}", label, line)
    }
}

macro_rules! l {
    ($name:literal) => {{
        Location {
            label: $name,
            file: file!(),
            line: line!(),
            column: column!(),
        }
    }};
    () => {{
        Location {
            label: "",
            file: file!(),
            line: line!(),
            column: column!(),
        }
    }};
}

pub trait Witness<F>: Sync + Send {
    fn get(&mut self, location: Location, locations: &Locations<'_>) -> F;
}

pub trait CircuitBuilderVar:
    Add<Self, Output = Self> + Sub<Self, Output = Self> + Mul<u128, Output = Self> + Clone
{
}

pub trait CircuitBuilder {
    type F: PrimeField;
    type Var: CircuitBuilderVar;

    fn zero(&mut self) -> Self::Var;
    fn one(&mut self) -> Self::Var;
    fn lit(&mut self, n: u128) -> Self::Var;
    fn alloc(&mut self, location: Location) -> Self::Var;
    fn enforce(&mut self, location: Location, a: Self::Var, b: Self::Var, a_times_b: Self::Var);
    fn nest<'a>(
        &'a mut self,
        location: Location,
    ) -> impl CircuitBuilder<Var = Self::Var, F = Self::F> + 'a;
    fn from_allocated_num(&mut self, var: AllocatedNum<Self::F>) -> Self::Var;
    fn to_allocated_num(&mut self, var: Self::Var) -> AllocatedNum<Self::F>;
    fn inner(self) -> impl ConstraintSystem<Self::F>;
}

pub trait Circuit: Send + Sync {
    fn run<CB: CircuitBuilder>(&self, builder: CB, io: Vec<CB::Var>) -> Vec<CB::Var>;
}

pub fn test_circuit<F: PrimeField + PrimeFieldBits, W: Witness<F>, C: Circuit>(
    io: Vec<F>,
    circuit: C,
    witness: W,
) {
    struct Builder<'a, F, W, CS> {
        cs: CS,
        locations: Locations<'a>,
        witness: &'a mut W,
        counter: &'a mut usize,
        _f: PhantomData<F>,
    }

    #[derive(Clone)]
    struct Single<F> {
        inner: Variable,
        scale: F,
        data: Option<F>,
    }

    #[derive(Clone)]
    enum VarImpl<F> {
        Zero,
        VarImpl(Vec<Single<F>>),
    }

    fn to_lc<F: PrimeField>(lc: LinearCombination<F>, var: VarImpl<F>) -> LinearCombination<F> {
        match var {
            VarImpl::Zero => LinearCombination::zero(),
            VarImpl::VarImpl(vars) => vars.into_iter().fold(
                lc,
                |lc,
                 Single {
                     inner,
                     scale,
                     data: _,
                 }| lc + (scale, inner),
            ),
        }
    }

    fn var_impl_data<F: PrimeField>(var: VarImpl<F>) -> Option<F> {
        match var {
            VarImpl::Zero => Some(F::ZERO),
            VarImpl::VarImpl(vec) => vec.into_iter().fold(
                Some(F::ZERO),
                |acc,
                 Single {
                     inner: _,
                     scale: _,
                     data,
                 }| {
                    match (acc, data) {
                        (Some(acc), Some(data)) => Some(acc * data),
                        _ => None,
                    }
                },
            ),
        }
    }

    impl<F: PrimeField> Add<VarImpl<F>> for VarImpl<F> {
        type Output = VarImpl<F>;

        fn add(self, rhs: VarImpl<F>) -> VarImpl<F> {
            match (self, rhs) {
                (VarImpl::Zero, rhs) => rhs,
                (lhs, VarImpl::Zero) => lhs,
                (VarImpl::VarImpl(lhs), VarImpl::VarImpl(rhs)) => {
                    VarImpl::VarImpl(lhs.into_iter().chain(rhs.into_iter()).collect())
                }
            }
        }
    }
    impl<F: PrimeField> Sub<VarImpl<F>> for VarImpl<F> {
        type Output = VarImpl<F>;

        fn sub(self, rhs: VarImpl<F>) -> VarImpl<F> {
            match rhs {
                VarImpl::Zero => self,
                VarImpl::VarImpl(rhs) => {
                    self + VarImpl::VarImpl(
                        rhs.into_iter()
                            .map(|Single { inner, scale, data }| Single {
                                inner,
                                scale: scale * (F::ZERO - F::ONE),
                                data: data.map(|d| d * (F::ZERO - F::ONE)),
                            })
                            .collect(),
                    )
                }
            }
        }
    }
    impl<F: PrimeField> Mul<u128> for VarImpl<F> {
        type Output = VarImpl<F>;

        fn mul(self, rhs: u128) -> VarImpl<F> {
            match self {
                VarImpl::Zero => VarImpl::Zero,
                VarImpl::VarImpl(lhs) => VarImpl::VarImpl(
                    lhs.into_iter()
                        .map(|Single { inner, scale, data }| {
                            let rhs = F::from_u128(rhs);
                            Single {
                                inner,
                                scale: scale * rhs,
                                data: data.map(|d| d * rhs),
                            }
                        })
                        .collect(),
                ),
            }
        }
    }
    impl<F: PrimeField> CircuitBuilderVar for VarImpl<F> {}

    fn suffix(counter: &mut usize, s: String) -> String {
        let suffix = format!("[{}]", counter);
        *counter += 1;
        s + &suffix
    }

    impl<'a, F: PrimeField, W: Witness<F>, CS: ConstraintSystem<F>> CircuitBuilder
        for Builder<'a, F, W, CS>
    {
        type F = F;
        type Var = VarImpl<F>;
        fn zero(&mut self) -> VarImpl<F> {
            VarImpl::Zero
        }
        fn one(&mut self) -> VarImpl<F> {
            VarImpl::VarImpl(vec![Single {
                inner: CS::one(),
                scale: F::ONE,
                data: Some(F::ONE),
            }])
        }
        fn lit(&mut self, n: u128) -> VarImpl<F> {
            let n = F::from_u128(n);
            VarImpl::VarImpl(vec![Single {
                inner: CS::one(),
                scale: n,
                data: Some(n),
            }])
        }
        fn alloc(&mut self, location: Location) -> VarImpl<F> {
            let w = self.witness.get(location, &self.locations);
            let var = self
                .cs
                .alloc(
                    || suffix(self.counter, "CircuitBuilder::alloc".to_owned()),
                    || Ok(w),
                )
                .expect("impossible");
            VarImpl::VarImpl(vec![Single {
                inner: var,
                scale: F::ONE,
                data: Some(w),
            }])
        }
        fn enforce(
            &mut self,
            location: Location,
            a: VarImpl<F>,
            b: VarImpl<F>,
            a_times_b: VarImpl<F>,
        ) {
            self.cs.enforce(
                || suffix(self.counter, format_location(location)),
                |lc| to_lc(lc, a),
                |lc| to_lc(lc, b),
                |lc| to_lc(lc, a_times_b),
            );
            *self.counter += 1;
        }
        fn nest<'b>(
            &'b mut self,
            location: Location,
        ) -> impl CircuitBuilder<Var = Self::Var, F = Self::F> + 'b {
            Builder {
                cs: self
                    .cs
                    .namespace(|| suffix(self.counter, format_location(location))),
                locations: Locations::Cons(location, &self.locations),
                witness: self.witness,
                counter: self.counter,
                _f: PhantomData,
            }
        }
        fn from_allocated_num(&mut self, var: AllocatedNum<F>) -> VarImpl<F> {
            VarImpl::VarImpl(vec![Single {
                inner: var.get_variable(),
                scale: F::ONE,
                data: var.get_value(),
            }])
        }
        fn to_allocated_num(&mut self, var: Self::Var) -> AllocatedNum<Self::F> {
            // NB: because this is for testing, we don't need to enforce anything here,
            // but be careful if you copy this code for use in circuits.
            AllocatedNum::alloc(
                self.cs
                    .namespace(|| suffix(self.counter, "to_allocated_num".to_owned())),
                || var_impl_data(var).ok_or(SynthesisError::AssignmentMissing),
            )
            .expect("to_allocated_num; shouldn't happen")
        }
        fn inner(self) -> impl ConstraintSystem<F> {
            self.cs
        }
    }

    struct StepCircuitImpl<C, W> {
        arity: usize,
        circuit: Arc<C>,
        witness: Arc<Mutex<W>>,
    }

    impl<C, W> Clone for StepCircuitImpl<C, W> {
        fn clone(&self) -> Self {
            let StepCircuitImpl {
                arity,
                circuit,
                witness,
            } = self;
            let arity = *arity;
            let circuit = circuit.clone();
            let witness = witness.clone();
            StepCircuitImpl {
                arity,
                circuit,
                witness,
            }
        }
    }

    impl<F, C: Circuit, W: Witness<F>> StepCircuit<F> for StepCircuitImpl<C, W>
    where
        F: PrimeField + PrimeFieldBits,
    {
        fn arity(&self) -> usize {
            self.arity
        }

        fn synthesize<CS: ConstraintSystem<F>>(
            &self,
            cs: &mut CS,
            io: &[AllocatedNum<F>],
        ) -> Result<Vec<AllocatedNum<F>>, SynthesisError> {
            let mut w_guard = self.witness.lock().unwrap();

            let w = &mut *w_guard;

            let mut counter: usize = 0;

            let io = {
                let mut builder = Builder {
                    cs: cs.namespace(|| "internal0"),
                    locations: Locations::Nil,
                    witness: w,
                    counter: &mut counter,
                    _f: PhantomData,
                };

                io.iter()
                    .map(|var| builder.from_allocated_num(var.clone()))
                    .collect()
            };

            let io = self.circuit.run(
                Builder {
                    cs: cs.namespace(|| "circuit"),
                    locations: Locations::Nil,
                    witness: w,
                    counter: &mut counter,
                    _f: PhantomData,
                },
                io,
            );
            let mut builder = Builder {
                cs: cs.namespace(|| "internal1"),
                locations: Locations::Nil,
                witness: w,
                counter: &mut counter,
                _f: PhantomData,
            };

            Ok(io
                .into_iter()
                .map(|var| builder.to_allocated_num(var))
                .collect())
        }

        fn non_deterministic_advice(&self) -> Vec<F> {
            Vec::new()
        }
    }
    let mut test = TestConstraintSystem::new();
    let im = StepCircuitImpl {
        arity: io.len(),
        witness: Arc::new(Mutex::new(witness)),
        circuit: Arc::new(circuit),
    };
    let counter: &mut usize = &mut 0;
    let io: Vec<_> = io
        .iter()
        .map(|v| {
            AllocatedNum::alloc_infallible(
                test.namespace(|| suffix(counter, "input".to_owned())),
                || *v,
            )
        })
        .collect();
    im.synthesize(&mut test, &io)
        .expect("test_circuit synthesis failed");
    eprintln!(
        "printing unsatisfied constraints\n{:?}\nprinting unsatisfied constraints done",
        test.which_is_unsatisfied()
    );
    assert!(test.is_satisfied());
}

struct Switches<Var>(Vec<Var>);
#[must_use]
struct ConsumeSwitches;

// FIXME: Report compiler bug or find issue
// This constraint is redundant, but rustc errs without it./
impl<Var: CircuitBuilderVar> Switches<Var> {
    fn new() -> (Switches<Var>, ConsumeSwitches) {
        (Switches(vec![]), ConsumeSwitches)
    }
    fn alloc(&mut self, mut cb: impl CircuitBuilder<Var = Var>) -> Var {
        let switch = cb.alloc(l!("switch"));
        self.0.push(switch.clone());
        switch
    }
    fn consume(self, _marker: ConsumeSwitches, mut cb: impl CircuitBuilder<Var = Var>) {
        let zero = cb.zero();
        let one = cb.one();
        cb.enforce(
            l!("only one switch must be on"),
            self.0
                .iter()
                .fold(zero.clone(), |acc, switch| acc + switch.clone()),
            one.clone(),
            one.clone(),
        );

        for switch in &self.0 {
            cb.enforce(
                l!("switch must be binary"),
                switch.clone(),
                one.clone() - switch.clone(),
                zero.clone(),
            );
        }

        std::mem::forget(self)
    }
}

fn if_switch<CB: CircuitBuilder>(mut cb: CB, switch: CB::Var, n: CB::Var) -> CB::Var {
    let r = cb.alloc(l!("if_result"));
    cb.enforce(l!("if_switch"), switch, n, r.clone());
    r
}

fn either_switch<CB: CircuitBuilder>(
    mut cb: CB,
    switch: CB::Var,
    case_true: CB::Var,
    case_false: CB::Var,
) -> CB::Var {
    let zero = cb.zero();
    let one = cb.one();
    let r_true = cb.alloc(l!("either_switch_true"));
    cb.enforce(
        l!("either_switch.case_true"),
        switch.clone(),
        case_true.clone(),
        r_true.clone(),
    );
    let r_false = cb.alloc(l!("either_switch_false"));
    cb.enforce(
        l!("either_switch.case_true"),
        one.clone() - switch.clone(),
        case_false.clone(),
        r_false.clone(),
    );
    cb.enforce(
        l!("either_switch.combine"),
        r_true.clone(),
        r_false.clone(),
        zero.clone(),
    );
    let r = cb.alloc(l!("either_switch_r"));
    cb.enforce(
        l!("either_switch.either"),
        r_true.clone() + r_false.clone(),
        one.clone(),
        r.clone(),
    );
    r
}

fn hash<CB: CircuitBuilder>(mut cb: CB, switch: CB::Var, input: Vec<CB::Var>) -> CB::Var {
    // FIXME: poseidon_hash_allocated doesn't take a switch,
    // so all its constraints cost even if they aren't used.
    let input: Vec<AllocatedNum<CB::F>> = input
        .into_iter()
        .map(|var| cb.to_allocated_num(var))
        .collect();
    let constants = PoseidonConstants::<CB::F, U4>::new();
    let hash: AllocatedNum<CB::F> =
        poseidon_hash_allocated(cb.nest(l!()).inner(), input, &constants)
            .expect("poseidon_hash_allocated failed");
    let hash = cb.from_allocated_num(hash);
    if_switch(cb.nest(l!()), switch, hash)
}

// adds H(a, v, t) to the multiset
fn memory<CB: CircuitBuilder>(
    mut cb: CB,
    switch: CB::Var,
    multiset: CB::Var,
    a: CB::Var,
    v: CB::Var,
    t: CB::Var,
) -> CB::Var {
    let preimage = vec![a, v, t, cb.zero()];
    let hash = hash(cb.nest(l!()), switch, preimage);
    multiset + hash
}

fn push<CB: CircuitBuilder>(
    mut cb: CB,

    switch: CB::Var,
    rs: &mut CB::Var,
    ws: &mut CB::Var,
    idx: CB::Var,
    data: &[CB::Var],
) {
    let prev = cb.alloc(l!("prev"));
    let timestamp = cb.alloc(l!("timestamp"));
    let new_timestamp = cb.alloc(l!("new_timestamp"));
    cb.enforce(
        l!(),
        new_timestamp.clone() - timestamp.clone(),
        switch.clone(),
        switch.clone(),
    );
    let zero = cb.zero();
    let one = cb.one();
    cb.enforce(l!(), zero.clone(), one, zero.clone());
    let preimage = {
        let mut v = Vec::from(data);
        v.push(prev.clone());
        if v.len() % 2 == 1 {
            v.push(zero);
        }
        v
    };
    let updated = hash(cb.nest(l!()), switch.clone(), preimage);
    *rs = memory(
        cb.nest(l!()),
        switch.clone(),
        rs.clone(),
        idx.clone(),
        prev,
        timestamp,
    );
    *ws = memory(
        cb.nest(l!()),
        switch,
        ws.clone(),
        idx,
        updated,
        new_timestamp,
    );
}

struct StarstreamIO<Var> {
    rs: Var,
    ws: Var,
    rs_coord: Var,
    ws_coord: Var,
    coord_idx: Var,
    coord_stack: Var,
}

impl<Var> StarstreamIO<Var> {
    fn of(fields: Vec<Var>) -> StarstreamIO<Var> {
        let Ok([rs, ws, rs_coord, ws_coord, coord_idx, coord_stack]): Result<[Var; 6], _> =
            fields.try_into()
        else {
            unreachable!("wrong number of elements in io");
        };
        StarstreamIO {
            rs,
            ws,
            rs_coord,
            ws_coord,
            coord_idx,
            coord_stack,
        }
    }
    fn to(self) -> Vec<Var> {
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

fn visit_enter<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    io: &mut StarstreamIO<CB::Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let utxo_idx = cb.alloc(l!("utxo_idx"));
    let input = cb.alloc(l!("input"));
    push(
        cb.nest(l!()),
        switch.clone(),
        &mut io.rs,
        &mut io.ws,
        utxo_idx.clone(),
        &vec![input.clone()],
    );
    let zero = cb.zero();
    push(
        cb.nest(l!()),
        switch,
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &vec![zero, utxo_idx, input],
    );
}

fn visit_push_coord<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    io: &mut StarstreamIO<CB::Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let new_coord_idx = cb.alloc(l!("new_coord_idx"));
    let input = cb.alloc(l!("input"));
    let one = cb.one();
    push(
        cb.nest(l!()),
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        // FIXME: use hash instead of idx
        &vec![one, new_coord_idx.clone(), input.clone()],
    );
    let two = cb.lit(2);
    push(
        cb.nest(l!()),
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        new_coord_idx.clone(),
        // FIXME: use hash instead of idx
        &vec![two, io.coord_idx.clone(), input],
    );
    let preimage = vec![io.coord_idx.clone(), io.coord_stack.clone()];
    let new_coord_stack = hash(cb.nest(l!()), switch.clone(), preimage);
    io.coord_idx = either_switch(
        cb.nest(l!()),
        switch.clone(),
        new_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        cb.nest(l!()),
        switch,
        new_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_pop_coord<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    io: &mut StarstreamIO<CB::Var>,
) {
    let switch = switches.alloc(cb.nest(l!()));
    let old_coord_idx = cb.alloc(l!("old_coord_idx"));
    let old_coord_stack = cb.alloc(l!("old_coord_stack"));
    let three = cb.lit(3);
    push(
        cb.nest(l!()),
        switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &vec![three],
    );
    let preimage = vec![old_coord_idx.clone(), old_coord_stack.clone()];
    let should_be = hash(cb.nest(l!()), switch.clone(), preimage);
    cb.enforce(
        l!(),
        should_be.clone(),
        switch.clone(),
        io.coord_stack.clone(),
    );
    io.coord_idx = either_switch(
        cb.nest(l!()),
        switch.clone(),
        old_coord_idx,
        io.coord_idx.clone(),
    );
    io.coord_stack = either_switch(
        cb.nest(l!()),
        switch,
        old_coord_stack,
        io.coord_stack.clone(),
    );
}

fn visit_nop<CB: CircuitBuilder>(mut cb: CB, switches: &mut Switches<CB::Var>) {
    let _switch = switches.alloc(cb.nest(l!()));
}

pub struct StarstreamCircuit;

impl Circuit for StarstreamCircuit {
    fn run<CB: CircuitBuilder>(&self, mut cb: CB, io: Vec<CB::Var>) -> Vec<CB::Var> {
        let (mut switches, consume_switches) = Switches::new();

        let mut io = StarstreamIO::of(io);

        visit_enter(cb.nest(l!()), &mut switches, &mut io);
        visit_push_coord(cb.nest(l!()), &mut switches, &mut io);
        visit_pop_coord(cb.nest(l!()), &mut switches, &mut io);
        visit_nop(cb.nest(l!()), &mut switches);

        switches.consume(consume_switches, cb.nest(l!()));

        io.to()
    }
}

#[test]
#[ignore]
fn prove_dummy() {
    use ff::Field;
    use nova::provider::PallasEngine;
    use nova::traits::Engine;
    use std::env::var_os;
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::path::Path;
    struct WitnessFromFile(BufReader<File>);

    fn format_locations(location: Location, locations: &Locations<'_>) -> String {
        fn go(mut out: Vec<String>, locations: &Locations<'_>) -> Vec<String> {
            match locations {
                Locations::Nil => out,
                Locations::Cons(location, locations) => {
                    if location.label.is_empty() {
                        out.push(format!("{}/", location.line));
                    } else {
                        out.push(format!("{}@{}/", location.label, location.line));
                    }
                    go(out, locations)
                }
            }
        }
        let v = vec![format!("{}/", location.line)];
        let mut out = go(v, &locations);
        out.reverse();
        let mut out = out.concat();
        let _ = out.pop();
        out
    }

    impl<F: PrimeField> Witness<F> for WitnessFromFile {
        fn get(&mut self, location: Location, locations: &Locations<'_>) -> F {
            let mut out = String::new();
            let _ = self.0.read_line(&mut out);
            let mut split = out.split_ascii_whitespace();
            let name_ = split.next();
            match name_ {
                None => {
                    eprintln!(
                        "{}: expected {}",
                        format_locations(location, locations),
                        location.label
                    );
                    return F::ZERO;
                }
                Some(name_) => {
                    let val = split
                        .next()
                        .and_then(F::from_str_vartime)
                        .expect("invalid witness file");
                    assert!(split.next().is_none());
                    if location.label != name_ {
                        eprintln!(
                            "{}: expected {}, got {}",
                            format_locations(location, locations),
                            location.label,
                            name_
                        );
                    }
                    val
                }
            }
        }
    }

    let witness = || {
        let var = var_os("SS_TEST_WITNESS").expect("provide SS_TEST_WITNESS env var");
        let path = Path::new(&var);
        let f = File::open(path).expect("file at SS_TEST_WITNESS not openable");
        let reader = BufReader::new(f);
        WitnessFromFile(reader)
    };
    type F = <PallasEngine as Engine>::Scalar;
    test_circuit(
        vec![F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO],
        StarstreamCircuit,
        witness(),
    );
    /*
    struct NoWitness;

    impl<F: PrimeField> Witness<F> for NoWitness {
        fn get(&mut self, _location: Location, _locations: &Locations<'_>) -> F {
            F::ZERO
        }
    }
    let pp: PublicParams<PallasEngine> = PublicParams::setup(
        &StarstreamCircuit(Arc::new(Mutex::new(NoWitness))),
        &*default_ck_hint(),
        &*default_ck_hint(),
    );
    let mut rs = RecursiveSNARK::new(
        &pp,
        &StarstreamCircuit(Arc::new(Mutex::new(witness()))),
        &input,
    )
    .expect(label_!()().as_ref());
    let ic = F::ZERO;
    rs.prove_step(&pp, &StarstreamCircuit(Arc::new(Mutex::new(witness()))), ic)
        .expect(label_!()().as_ref());
    let ic = rs.increment_commitment(&pp, &StarstreamCircuit(Arc::new(Mutex::new(witness()))));
    let num_steps = rs.num_steps();
    rs.verify(&pp, num_steps, &input, ic)
        .expect(label_!()().as_ref());
    */
}
