#![allow(clippy::uninlined_format_args)]
#![allow(clippy::suspicious_arithmetic_impl)]

pub mod interface;
pub mod circuits;
pub mod test;
pub mod wasm_parser;
pub mod r1cs;

/*
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

pub struct DummyWitness;

impl<F: PrimeField> Witness<F> for DummyWitness {
    fn get(&mut self, _: Location, _: &Locations<'_>) -> Option<F> {
        None
    }
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
    use nova_snark::provider::PallasEngine;
    use nova_snark::traits::Engine;
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
    use nova_snark::provider::PallasEngine;
    use nova_snark::traits::Engine;
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
*/
