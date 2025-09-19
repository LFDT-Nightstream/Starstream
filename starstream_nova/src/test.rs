use std::{
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Add, Mul, Sub},
};

use crate::interface::{Circuit, CircuitBuilder, CircuitBuilderVar, Location};

#[derive(Clone, Copy)]
pub enum Locations<'a> {
    Nil,
    Cons {
        tag: u64,
        head: Location,
        tail: &'a Locations<'a>,
    },
}

fn cons<'a>(head: Location, tail: &'a Locations<'a>) -> Locations<'a> {
    match tail {
        Locations::Nil => Locations::Cons {
            tag: head.tag,
            head,
            tail: &Locations::Nil,
        },
        Locations::Cons {
            tag,
            head: _,
            tail: _,
        } => Locations::Cons {
            tag: if *tag == 0 {
                head.tag
            } else {
                let mut hasher = DefaultHasher::new();
                head.tag.hash(&mut hasher);
                tag.hash(&mut hasher);
                hasher.finish()
            },
            head,
            tail,
        },
    }
}

fn calculate_tag(locations: Locations<'_>) -> u64 {
    match locations {
        Locations::Nil => 0,
        Locations::Cons { tag, .. } => tag,
    }
}

// grok generated goldilocks impl
const P: u128 = 18446744069414584321u128;

pub fn add(a: u64, b: u64) -> u64 {
    let sum = a as u128 + b as u128;
    if sum >= P {
        (sum - P) as u64
    } else {
        sum as u64
    }
}

pub fn neg(a: u64) -> u64 {
    if a == 0 { 0 } else { (P - a as u128) as u64 }
}

pub fn mul(a: u64, scalar: u128) -> u64 {
    let prod = a as u128 * scalar;
    (prod % P) as u64
}

pub trait Handler {
    fn failed_enforce(
        &mut self,
        locations: Locations<'_>,
        lhs: u64,
        rhs: u64,
        actual: u64,
        expected: u64,
    );
    fn lookup(&mut self, locations: Locations<'_>, namespace: u64, address: u64, val: u64);
    fn invalid_memory(
        &mut self,
        locations: Locations<'_>,
        namespace: u64,
        address: u64,
        expected: u64,
        actual: u64,
        new: u64,
    );
    fn mismatching_witness(&mut self, locations: Locations<'_>, provided_tag: u64);
}

pub fn test_circuit_goldilocks<C: Circuit>(
    io: Vec<u64>,
    circuit: C,
    mut witness: impl Iterator<Item = (u64, u64)>,
    mut handler: impl Handler,
) {
    #[derive(Clone, Copy, Debug)]
    struct Var(u64);

    impl Add<Var> for Var {
        type Output = Var;
        fn add(self, Var(rhs): Var) -> Var {
            Var(add(self.0, rhs))
        }
    }

    impl Sub<Var> for Var {
        type Output = Var;
        fn sub(self, Var(rhs): Var) -> Var {
            Var(add(self.0, neg(rhs)))
        }
    }

    impl Mul<u128> for Var {
        type Output = Var;
        fn mul(self, rhs: u128) -> Var {
            Var(mul(self.0, rhs))
        }
    }

    impl CircuitBuilderVar for Var {}

    struct Builder<'a, W, H> {
        locations: Locations<'a>,
        witness: &'a mut W,
        handler: &'a mut H,
        memories: &'a mut Vec<Vec<u64>>,
    }

    impl<'a, W: Iterator<Item = (u64, u64)>, H: Handler> CircuitBuilder<Var> for Builder<'a, W, H> {
        fn zero(&mut self) -> Var {
            Var(0)
        }
        fn one(&mut self) -> Var {
            Var(1)
        }
        fn lit(&mut self, n: u128) -> Var {
            Var((n % P) as u64)
        }
        fn alloc(&mut self, location: Location) -> Var {
            let locations = cons(location, &self.locations);
            let (val, tag) = self.witness.next().unwrap_or((0, 0));
            let expected_tag = calculate_tag(locations);
            if tag != expected_tag {
                self.handler.mismatching_witness(locations, tag);
            }
            Var(val)
        }
        fn enforce(&mut self, location: Location, Var(a): Var, Var(b): Var, Var(a_times_b): Var) {
            let locations = cons(location, &self.locations);
            let r = mul(a, b as u128);
            if r != a_times_b {
                self.handler.failed_enforce(locations, a, b, r, a_times_b);
            }
        }
        fn lookup(&mut self, Var(namespace): Var, Var(address): Var, Var(val): Var) {
            self.handler.lookup(self.locations, namespace, address, val);
        }
        fn memory(&mut self, Var(namespace): Var, Var(address): Var, Var(old): Var, Var(new): Var) {
            self.memories
                .resize_with(namespace as usize + 1, Vec::default);
            let m = &mut self.memories[namespace as usize];
            m.resize_with(address as usize + 1, u64::default);
            let old_ = m[address as usize];
            if old_ != old {
                self.handler
                    .invalid_memory(self.locations, namespace, address, old, old_, new);
            }
            m[address as usize] = new;
        }
        fn nest<'b>(&'b mut self, location: Location) -> impl CircuitBuilder<Var> + 'b {
            Builder {
                locations: cons(location, &self.locations),
                witness: self.witness,
                handler: self.handler,
                memories: self.memories,
            }
        }
    }

    let mut memories = Vec::new();

    let builder = Builder {
        locations: Locations::Nil,
        witness: &mut witness,
        handler: &mut handler,
        memories: &mut memories,
    };

    circuit.run(builder, io.into_iter().map(Var).collect());
}
