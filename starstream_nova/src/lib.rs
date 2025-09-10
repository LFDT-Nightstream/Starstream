#![allow(clippy::uninlined_format_args)]
#![allow(clippy::suspicious_arithmetic_impl)]

use ff::{PrimeField, PrimeFieldBits};
use nova::frontend::test_cs::TestConstraintSystem;
use nova::traits::snark::default_ck_hint;
use nova::traits::{CurveCycleEquipped, Engine};
use std::marker::PhantomData;

use nova::frontend::gadgets::poseidon::poseidon_hash_allocated;
use nova::frontend::{ConstraintSystem, SynthesisError, num::AllocatedNum};
use nova::frontend::{LinearCombination, PoseidonConstants, Variable};
use nova::nebula::rs::{PublicParams, RecursiveSNARK, StepCircuit};
use std::fmt::Debug;
use std::ops::{Add, Mul, Sub};
use std::sync::{Arc, Mutex};
use typenum::U2;

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
    fn get(&mut self, location: Location, locations: &Locations<'_>) -> Option<F>;
}

pub struct DummyWitness;

impl<F: PrimeField> Witness<F> for DummyWitness {
    fn get(&mut self, _: Location, _: &Locations<'_>) -> Option<F> {
        None
    }
}

// TODO: consider making Copy, or at least making implementations clone shallowly
pub trait CircuitBuilderVar:
    Add<Self, Output = Self> + Sub<Self, Output = Self> + Mul<u128, Output = Self> + Clone + Debug
{
}

pub trait CircuitBuilder {
    type F: PrimeField;
    type Var: CircuitBuilderVar;

    fn zero(&mut self) -> Self::Var;
    fn one(&mut self) -> Self::Var;
    #[must_use]
    fn lit(&mut self, n: u128) -> Self::Var;
    #[must_use]
    fn alloc(&mut self, location: Location) -> Self::Var;
    fn enforce(&mut self, location: Location, a: Self::Var, b: Self::Var, a_times_b: Self::Var);
    fn lookup(&mut self, namespace: Self::Var, address: Self::Var, val: Self::Var);
    fn memory(&mut self, namespace: Self::Var, address: Self::Var, old: Self::Var, new: Self::Var);
    #[must_use]
    fn nest<'a>(
        &'a mut self,
        location: Location,
    ) -> impl CircuitBuilder<Var = Self::Var, F = Self::F> + 'a;
    #[must_use]
    fn from_allocated_num(&mut self, var: AllocatedNum<Self::F>) -> Self::Var;
    #[must_use]
    fn to_allocated_num(&mut self, var: Self::Var) -> AllocatedNum<Self::F>;
    #[must_use]
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

    #[derive(Clone, Debug)]
    struct Single<F> {
        inner: Variable,
        scale: F,
        data: Option<F>,
    }

    #[derive(Clone, Debug)]
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
            VarImpl::VarImpl(vec) => vec.into_iter().try_fold(
                F::ZERO,
                |acc,
                 Single {
                     inner: _,
                     scale: _,
                     data,
                 }| { data.map(|data| acc * data) },
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
                    VarImpl::VarImpl(lhs.into_iter().chain(rhs).collect())
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
                    || w.ok_or(SynthesisError::AssignmentMissing),
                )
                .expect("impossible");
            VarImpl::VarImpl(vec![Single {
                inner: var,
                scale: F::ONE,
                data: w,
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
        fn lookup(&mut self, _namespace: Self::Var, _address: Self::Var, _val: Self::Var) {
            unimplemented!()
        }
        fn memory(
            &mut self,
            _namespace: VarImpl<F>,
            _address: VarImpl<F>,
            _old: VarImpl<F>,
            _new: VarImpl<F>,
        ) {
            unimplemented!()
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

pub fn prove_test_circuit<
    F: PrimeField + PrimeFieldBits,
    E: Engine<Scalar = F> + CurveCycleEquipped,
    W: Witness<F>,
    C: Circuit,
>(
    PhantomData: PhantomData<E>,
    io: Vec<F>,
    io_: Vec<F>,
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

    #[derive(Clone, Debug)]
    struct Single<F> {
        inner: Variable,
        scale: F,
        data: Option<F>,
    }

    #[derive(Clone, Debug)]
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
            VarImpl::VarImpl(vec) => vec.into_iter().try_fold(
                F::ZERO,
                |acc,
                 Single {
                     inner: _,
                     scale: _,
                     data,
                 }| data.map(|data| acc * data),
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
                    VarImpl::VarImpl(lhs.into_iter().chain(rhs).collect())
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
                    || w.ok_or(SynthesisError::AssignmentMissing),
                )
                .expect("impossible");
            VarImpl::VarImpl(vec![Single {
                inner: var,
                scale: F::ONE,
                data: w,
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
        fn lookup(&mut self, _namespace: VarImpl<F>, _address: Self::Var, _val: Self::Var) {
            unimplemented!()
        }
        fn memory(
            &mut self,
            _namespace: VarImpl<F>,
            _address: VarImpl<F>,
            _old: VarImpl<F>,
            _new: VarImpl<F>,
        ) {
            unimplemented!()
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
    let circuit = Arc::new(circuit);
    let im_pp = StepCircuitImpl {
        arity: io.len(),
        witness: Arc::new(Mutex::new(DummyWitness)),
        circuit: circuit.clone(),
    };
    let pp: PublicParams<E> = PublicParams::setup(&im_pp, &*default_ck_hint(), &*default_ck_hint());
    let im = StepCircuitImpl {
        arity: io.len(),
        witness: Arc::new(Mutex::new(witness)),
        circuit: circuit.clone(),
    };
    let mut rs = RecursiveSNARK::new(&pp, &im, &io).expect("creating recursive snark failed");
    let ic = F::ZERO;
    rs.prove_step(&pp, &im, ic).expect("proving step failed");
    let ic = rs.increment_commitment(&pp, &im);
    let num_steps = rs.num_steps();
    rs.verify(&pp, num_steps, &io_, ic)
        .expect("generated proof not valid");
}

#[derive(Clone)]
struct Switch<Var> {
    switch: Var,
}

struct Switches<Var>(Vec<Var>);
#[must_use]
struct ConsumeSwitches;

// FIXME: Report compiler bug or find issue
// This constraint is redundant, but rustc errs without it.
impl<Var: CircuitBuilderVar> Switches<Var> {
    fn new() -> (Switches<Var>, ConsumeSwitches) {
        (Switches(vec![]), ConsumeSwitches)
    }
    fn alloc(&mut self, mut cb: impl CircuitBuilder<Var = Var>) -> Switch<Var> {
        let switch = cb.alloc(l!("switch"));
        self.0.push(switch.clone());
        Switch { switch }
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

fn cond_assert<CB: CircuitBuilder>(mut cb: CB, switch: Switch<CB::Var>, a: CB::Var, b: CB::Var) {
    let maybe_b = cb.alloc(l!("maybe_b"));
    cb.enforce(l!(), switch.switch.clone(), b, maybe_b.clone());
    cb.enforce(l!(), switch.switch, a, maybe_b);
}

// NB: this requires that the 0th index of the memory is always zero,
// i.e. that *nullptr = 0
// FIXME: this could be done more efficiently probably
fn cond_memory<CB: CircuitBuilder>(
    mut cb: CB,
    switch: Switch<CB::Var>,
    namespace: CB::Var,
    addr: CB::Var,
    old: CB::Var,
    new: CB::Var,
) {
    let real_addr = cb.alloc(l!("real_addr"));
    let real_old = cb.alloc(l!("real_old"));
    let real_new = cb.alloc(l!("real_new"));
    cb.enforce(l!(), switch.switch.clone(), addr.clone(), real_addr.clone());
    cb.enforce(l!(), switch.switch.clone(), old.clone(), real_old.clone());
    cb.enforce(l!(), switch.switch.clone(), new.clone(), real_new.clone());
    cb.memory(namespace, addr, old, new);
}

fn if_switch<CB: CircuitBuilder>(mut cb: CB, switch: CB::Var, n: CB::Var) -> CB::Var {
    let r = cb.alloc(l!("if_result"));
    cb.enforce(l!("if_switch"), switch, n, r.clone());
    r
}

fn either_switch<CB: CircuitBuilder>(
    mut cb: CB,
    switch: Switch<CB::Var>,
    case_true: CB::Var,
    case_false: CB::Var,
) -> CB::Var {
    let zero = cb.zero();
    let one = cb.one();
    let r_true = cb.alloc(l!("either_switch_true"));
    cb.enforce(
        l!("either_switch.case_true"),
        switch.switch.clone(),
        case_true.clone(),
        r_true.clone(),
    );
    let r_false = cb.alloc(l!("either_switch_false"));
    cb.enforce(
        l!("either_switch.case_true"),
        one.clone() - switch.switch.clone(),
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
    let mut input: Vec<AllocatedNum<CB::F>> = input
        .into_iter()
        .map(|var| cb.to_allocated_num(var))
        .collect();
    // FIXME: HORRIBLE!
    // poseidon_hash_allocated is buggy I assume,
    // because it can't handle larger inputs,
    // or it expects us to pass in a `U` that fits
    // exactly, but that seems unlikely, and not really possible
    // to abstract over because of the private trait constraint.
    input.truncate(2);
    let constants = PoseidonConstants::<CB::F, U2>::new();
    let hash: AllocatedNum<CB::F> =
        poseidon_hash_allocated(cb.nest(l!()).inner(), input, &constants)
            .expect("poseidon_hash_allocated failed");
    let hash = cb.from_allocated_num(hash);
    if_switch(cb.nest(l!("switch_hash")), switch, hash)
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
    let preimage = vec![a, v, t];
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
        switch.switch.clone(),
        &mut io.rs,
        &mut io.ws,
        utxo_idx.clone(),
        &[input.clone()],
    );
    let zero = cb.zero();
    push(
        cb.nest(l!()),
        switch.switch,
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &[zero, utxo_idx, input],
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
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        // FIXME: use hash instead of idx
        &[one, new_coord_idx.clone(), input.clone()],
    );
    let two = cb.lit(2);
    push(
        cb.nest(l!()),
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        new_coord_idx.clone(),
        // FIXME: use hash instead of idx
        &[two, io.coord_idx.clone(), input],
    );
    let preimage = vec![io.coord_idx.clone(), io.coord_stack.clone()];
    let new_coord_stack = hash(cb.nest(l!()), switch.switch.clone(), preimage);
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
        switch.switch.clone(),
        &mut io.rs_coord,
        &mut io.ws_coord,
        io.coord_idx.clone(),
        &[three],
    );
    let preimage = vec![old_coord_idx.clone(), old_coord_stack.clone()];
    let should_be = hash(cb.nest(l!()), switch.switch.clone(), preimage);
    cb.enforce(
        l!(),
        should_be.clone(),
        switch.switch.clone(),
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

// NB: this is a really dumb "vm", which is just R1CS in R1CS basically.
// Even if we were to adopt this approach, it would probably be more
// efficient to use Spartan to reduce the amount of work necessary
// rather than naively embedding R1CS in R1CS.
#[allow(non_camel_case_types)]
pub struct R1CS_DUMMY_VM;

const R1CS_DUMMY_WITNESS_COUNT: usize = 254;
const R1CS_DUMMY_CONSTRAINT_COUNT: usize = 256;

fn mul<CB: CircuitBuilder>(mut cb: CB, x: CB::Var, y: CB::Var) -> CB::Var {
    let r = cb.alloc(l!("mul"));
    cb.enforce(l!("mul"), x, y, r.clone());
    r
}

impl Circuit for R1CS_DUMMY_VM {
    fn run<CB: CircuitBuilder>(&self, mut cb: CB, io: Vec<CB::Var>) -> Vec<CB::Var> {
        let [scripts, ios] = io.try_into().expect("arity wrong");
        let io = cb.alloc(l!("io"));
        let witnesses: Vec<_> = (0..R1CS_DUMMY_WITNESS_COUNT)
            .map(|_| cb.alloc(l!("witness")))
            .collect();
        let constraints_a: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_a")))
            .collect();
        let constraints_b: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_b")))
            .collect();
        let constraints_c: Vec<_> = (0..(R1CS_DUMMY_CONSTRAINT_COUNT
            * (R1CS_DUMMY_WITNESS_COUNT + 2)))
            .map(|_| cb.alloc(l!("factor_c")))
            .collect();
        let one = cb.one();
        for i in 0..R1CS_DUMMY_CONSTRAINT_COUNT {
            let a = constraints_a[i].clone();
            let a = a + mul(
                cb.nest(l!("io_a")),
                io.clone(),
                constraints_a[i + 1].clone(),
            );
            let a = (0..R1CS_DUMMY_WITNESS_COUNT).fold(a, |a, j| {
                a + mul(
                    cb.nest(l!("witness_factor_a_mul")),
                    witnesses[j].clone(),
                    constraints_a[i + j + 2].clone(),
                )
            });
            let b = constraints_b[i].clone();
            let b = b + mul(
                cb.nest(l!("io_b")),
                io.clone(),
                constraints_b[i + 1].clone(),
            );
            let b = (0..R1CS_DUMMY_WITNESS_COUNT).fold(b, |b, j| {
                b + mul(
                    cb.nest(l!("witness_factor_b_mul")),
                    witnesses[j].clone(),
                    constraints_b[i + j + 2].clone(),
                )
            });
            let c = constraints_c[i].clone();
            let c = c + mul(
                cb.nest(l!("io_c")),
                io.clone(),
                constraints_c[i + 1].clone(),
            );
            let c = (0..R1CS_DUMMY_WITNESS_COUNT).fold(c, |c, j| {
                c + mul(
                    cb.nest(l!("witness_factor_c_mul")),
                    witnesses[j].clone(),
                    constraints_c[i + j + 2].clone(),
                )
            });
            cb.enforce(l!(), a, b, c);
        }
        let script_hash = hash(
            cb.nest(l!()),
            one.clone(),
            constraints_a
                .into_iter()
                .chain(constraints_b)
                .chain(constraints_c)
                .collect(),
        );
        let scripts = hash(cb.nest(l!()), one, vec![script_hash, scripts]);
        vec![scripts, ios]
    }
}

/* This is supposed to be a WASM VM.
 * We don't support WASM as-is, but instead a simplified variant without types and
 * type checking, mainly affecting (dynamic) function calls which can be called invalidly
 * in our VM.
 *
 * TODO: We don't support memory for now as a simplification
 *       but will in the future.
 *
 * Local variables are represented as ordinary "slots" in the stack
 * and thus you can e.g. remove them with pop.
 * This simplifies function calling heavily.
 * Instructions to modify locals thus take an index into the stack,
 * meaning you need to calculate the correct index when compiling from
 * WASM to the IR (which should be trivial).
 * Most instructions work directly on fields and modulus operations
 * must be done separately for efficiency.
 *
 * TODO: range check via lookup
 *
 * We don't consider public input for now.
 *
 * TODO: implement techniques from EDEN/Zorp stuff for stack,
 *       figure out how to do memory better than Nebula,
 *       do lookups
 *
 * TODO: use struct to carry around "global" vars (opcode, sp, pc, new_sp, new_pc)
 */
#[allow(non_camel_case_types)]
pub struct WASM_VM;

// x -> {}
const WASM_INSTR_DROP: u128 = 0;
// {} -> code[pc+1]
const WASM_INSTR_CONST: u128 = 1;
// x y -> x+y
const WASM_INSTR_ADD: u128 = 2;
// {} -> stack[pc+1]
const WASM_INSTR_GET: u128 = 3;
// x -> stack[pc+1] := x
const WASM_INSTR_SET: u128 = 4;
// b new_pc_namespace new_pc -> {}; pc := new_pc, pc_namespace := new_pc_namespace if b == 1, else if b == 0 then pc := pc + 1
const WASM_INSTR_JUMP: u128 = 5;
// ptr -> memory[ptr]
const WASM_INSTR_READ: u128 = 6;
// ptr data -> {}; memory[ptr] = data
const WASM_INSTR_WRITE: u128 = 7;

#[allow(non_camel_case_types)]
struct WASM_IO<Var> {
    sp_namespace: Var,
    sp: Var,
    pc_namespace: Var,
    pc: Var,
}

#[derive(Clone)]
struct WASMGlobal<Var> {
    opcode: Var,
    sp_namespace: Var,
    sp: Var,
    pc_namespace: Var,
    pc: Var,
    new_sp_namespace: Var,
    new_sp: Var,
    new_pc_namespace: Var,
    new_pc: Var,
}

impl<Var> WASM_IO<Var> {
    fn of(fields: Vec<Var>) -> WASM_IO<Var> {
        let Ok([sp_namespace, sp, pc_namespace, pc]): Result<[Var; 4], _> = fields.try_into()
        else {
            unreachable!("wrong number of elements in io");
        };
        WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        }
    }
    fn to(self) -> Vec<Var> {
        let WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        } = self;
        vec![sp_namespace, sp, pc_namespace, pc]
    }
}

fn visit_drop<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_drop"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_DROP);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let prev = cb.alloc(l!("prev"));
    let zero = cb.zero();
    let one = cb.one();
    // FIXME: wrong
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        prev,
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_const<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_const"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_CONST);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let constant = cb.alloc(l!("constant"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        constant.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone(),
        zero,
        constant,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_add<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_add"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_ADD);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let first = cb.alloc(l!("first"));
    let second = cb.alloc(l!("second"));
    let one = cb.one();
    let zero = cb.zero();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        sp.clone(),
        first.clone(),
        zero,
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        second.clone(),
        first + second,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_get<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_get"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_GET);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let index = cb.alloc(l!("index"));
    let value = cb.alloc(l!("value"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        index.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - index,
        value.clone(),
        value.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone(),
        zero,
        value,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_set<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_set"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_SET);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let index = cb.alloc(l!("index"));
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    cb.lookup(
        pc_namespace.clone(),
        pc.clone() + one.clone(),
        index.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new.clone(),
        zero,
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - index,
        old.clone(),
        new.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        new_pc,
        pc + one.clone() + one,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_jump_b_0<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_jump_b_1"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_JUMP);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let unused_pc = cb.alloc(l!("unused_pc"));
    let unused_pc_namespace = cb.alloc(l!("unused_pc_namespace"));
    let two = cb.lit(2);
    let three = cb.lit(3);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - three.clone(),
        zero.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two,
        unused_pc_namespace,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        unused_pc,
        zero.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - three);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(cb.nest(l!()), switch, pc_namespace, new_pc_namespace);
}

fn visit_jump_b_1<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace: _,
        pc: _,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_jump_b_0"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_JUMP);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    let three = cb.lit(3);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - three.clone(),
        one.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two,
        new_pc_namespace,
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new_pc,
        zero,
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - three);
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
}

fn visit_read<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_read"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_READ);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let address = cb.alloc(l!("address"));
    let value = cb.alloc(l!("value"));
    let one = cb.one();
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        address.clone(),
        value.clone(),
        value.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        address.clone(),
        value.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace,
        new_pc_namespace,
    );
}

fn visit_write<CB: CircuitBuilder>(
    mut cb: CB,
    switches: &mut Switches<CB::Var>,
    WASMGlobal {
        opcode,
        sp_namespace,
        sp,
        pc_namespace,
        pc,
        new_sp_namespace,
        new_sp,
        new_pc_namespace,
        new_pc,
    }: WASMGlobal<CB::Var>,
) {
    let mut cb = cb.nest(l!("visit_write"));
    let switch = switches.alloc(cb.nest(l!()));
    let instr = cb.lit(WASM_INSTR_WRITE);
    cond_assert(cb.nest(l!()), switch.clone(), opcode, instr);
    let address = cb.alloc(l!("address"));
    let old = cb.alloc(l!("old"));
    let new = cb.alloc(l!("new"));
    let zero = cb.zero();
    let one = cb.one();
    let two = cb.lit(2);
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace.clone(),
        address.clone(),
        old.clone(),
        new.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - one.clone(),
        new.clone(),
        zero.clone(),
    );
    cond_memory(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace.clone(),
        sp.clone() - two.clone(),
        address.clone(),
        zero.clone(),
    );
    cond_assert(cb.nest(l!()), switch.clone(), new_sp, sp - two);
    cond_assert(cb.nest(l!()), switch.clone(), new_pc, pc + one.clone());
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        sp_namespace,
        new_sp_namespace,
    );
    cond_assert(
        cb.nest(l!()),
        switch.clone(),
        pc_namespace,
        new_pc_namespace,
    );
}

impl Circuit for WASM_VM {
    fn run<CB: CircuitBuilder>(&self, mut cb: CB, io: Vec<CB::Var>) -> Vec<CB::Var> {
        let (mut switches, consume_switches) = Switches::new();
        let WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        } = WASM_IO::of(io);
        let opcode = cb.alloc(l!("opcode"));
        cb.lookup(pc_namespace.clone(), pc.clone(), opcode.clone());
        let new_sp_namespace = cb.alloc(l!("new_sp_namespace"));
        let new_sp = cb.alloc(l!("new_sp"));
        let new_pc_namespace = cb.alloc(l!("new_pc_namespace"));
        let new_pc = cb.alloc(l!("new_pc"));
        let global = WASMGlobal {
            opcode,
            sp_namespace,
            sp,
            pc_namespace,
            pc,
            new_sp_namespace,
            new_sp,
            new_pc_namespace,
            new_pc,
        };
        visit_drop(cb.nest(l!()), &mut switches, global.clone());
        visit_const(cb.nest(l!()), &mut switches, global.clone());
        visit_add(cb.nest(l!()), &mut switches, global.clone());
        visit_get(cb.nest(l!()), &mut switches, global.clone());
        visit_set(cb.nest(l!()), &mut switches, global.clone());
        visit_jump_b_0(cb.nest(l!()), &mut switches, global.clone());
        visit_jump_b_1(cb.nest(l!()), &mut switches, global.clone());
        visit_read(cb.nest(l!()), &mut switches, global.clone());
        visit_write(cb.nest(l!()), &mut switches, global.clone());
        switches.consume(consume_switches, cb.nest(l!()));
        let WASMGlobal {
            opcode: _,
            sp_namespace: _,
            sp: _,
            pc_namespace: _,
            pc: _,
            new_sp_namespace: sp_namespace,
            new_sp: sp,
            new_pc_namespace: pc_namespace,
            new_pc: pc,
        } = global;
        WASM_IO {
            sp_namespace,
            sp,
            pc_namespace,
            pc,
        }
        .to()
    }
}

#[cfg(test)]
fn get_combined_label(location: Location, locations: &Locations<'_>) -> String {
    fn go(mut out: Vec<String>, locations: &Locations<'_>) -> Vec<String> {
        match locations {
            Locations::Nil => out,
            Locations::Cons(location, locations) => {
                if !location.label.is_empty() {
                    out.push(format!("{}.", location.label));
                }
                go(out, locations)
            }
        }
    }
    let v = vec![format!("{}/", location.label)];
    let mut out = go(v, locations);
    out.reverse();
    let mut out = out.concat();
    let _ = out.pop();
    out
}

#[test]
fn test_dummy() {
    use ff::Field;
    use nova::provider::PallasEngine;
    use nova::traits::Engine;
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::path::Path;
    struct WitnessFromFile(BufReader<File>);

    fn format_locations(location: Location, locations: &Locations<'_>) -> String {
        fn go(mut out: Vec<String>, locations: &Locations<'_>) -> Vec<String> {
            match locations {
                Locations::Nil => out,
                Locations::Cons(location, locations) => {
                    out.push(format!("{}/", location.line));
                    go(out, locations)
                }
            }
        }
        let v = vec![format!("{}/", location.line)];
        let mut out = go(v, locations);
        out.reverse();
        let mut out = out.concat();
        let _ = out.pop();
        out
    }

    impl<F: PrimeField> Witness<F> for WitnessFromFile {
        fn get(&mut self, location: Location, locations: &Locations<'_>) -> Option<F> {
            let mut out = String::new();
            let _ = self.0.read_line(&mut out);
            let mut split = out.split_ascii_whitespace();
            let label = get_combined_label(location, locations);
            let label_ = split.next();
            match label_ {
                None => {
                    eprintln!(
                        "{}: expected {}",
                        format_locations(location, locations),
                        label
                    );
                    None
                }
                Some(label_) => {
                    let val = split
                        .next()
                        .and_then(F::from_str_vartime)
                        .expect("invalid witness file");
                    assert!(split.next().is_none());
                    if label != label_ {
                        eprintln!(
                            "{}: expected {}, got {}",
                            format_locations(location, locations),
                            label,
                            label_
                        );
                        None
                    } else {
                        eprintln!(
                            "{}: accepted {}",
                            format_locations(location, locations),
                            label_
                        );
                        Some(val)
                    }
                }
            }
        }
    }

    let path = Path::new("./test_witness.txt");
    let f = File::open(path).expect("./test_witness.txt not openable");
    let reader = BufReader::new(f);
    let witness = WitnessFromFile(reader);
    type F = <PallasEngine as Engine>::Scalar;
    test_circuit(
        vec![F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO],
        StarstreamCircuit,
        witness,
    );
}

#[test]
fn prove_dummy() {
    use ff::Field;
    use nova::provider::PallasEngine;
    use nova::traits::Engine;
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::path::Path;
    struct WitnessFromFile(BufReader<File>);

    impl<F: PrimeField> Witness<F> for WitnessFromFile {
        fn get(&mut self, location: Location, locations: &Locations<'_>) -> Option<F> {
            let mut out = String::new();
            let _ = self.0.read_line(&mut out);
            let mut split = out.split_ascii_whitespace();
            let label_ = split.next().expect("missing witness");
            let val = split
                .next()
                .and_then(F::from_str_vartime)
                .expect("invalid witness file");
            assert!(split.next().is_none());
            assert_eq!(get_combined_label(location, locations), label_);
            Some(val)
        }
    }

    let path = Path::new("./test_witness.txt");
    let f = File::open(path).expect("./test_witness.txt not openable");
    let reader = BufReader::new(f);
    let witness = WitnessFromFile(reader);
    type F = <PallasEngine as Engine>::Scalar;
    prove_test_circuit(
        PhantomData::<PallasEngine>,
        vec![F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO],
        vec![F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO, F::ZERO],
        StarstreamCircuit,
        witness,
    );
}
