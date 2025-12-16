use std::marker::PhantomData;
use std::ops::{Add, Mul, Sub};

use crate::interface::Location;
use crate::interface::{BranchedCircuit, Circuit, CircuitBuilder, CircuitBuilderVar};
use crate::l;

/**
 * This module implements generating the R1CS structure
 * for a given circuit, or rather, a set of circuits with a switchboard
 * to control which circuit to enable.
 *
 * The first witness is the constant one.
 * For every public input, there are two more witnesses,
 * one for the previous state, and one for the next,
 * representing a transition.
 * For every circuit, we then have another witness for the switch,
 * and one for every input or output used, and one for every witness
 * used by the circuit itself.
 * Every constraint in the circuit operates only on the witnesses specific
 * to the circuit, with the switch witness being used as the global "one",
 * such that any constraint in the circuit is always satisfied when all of its
 * witnesses are zero (a linear function of zeroes is always zero, thus A0 * B0 = C0).
 * Auiliary constraints are added to check that the switch is either one or zero,
 * that only one or zero switches are enabled,
 * that every input/output used in the circuit is linked to the "global" one,
 * and that if no switch is enabled, the inputs are the same as the outputs.
 *
 * FIXME: make lookups and memory operations optional by adding (switched) one to
 * each address, such that if the switch is off, the address will be zero,
 * thus the zero-entry of each table can be kept zero for efficiency purposes.
 * The lookup and memory operations can then be batched across all switches,
 * such that the effective address and values are the sums of each branch,
 * meaning that only one of them will be effective, since only
 * one will be non-zero.
 */

struct SwitchedCircuitBuilder<Var, CB> {
    switch: Var,
    cb: CB,
}
pub struct SwitchedCircuit<B, C>(pub PhantomData<B>, pub C);

impl<Var, IO, L, M, CB> CircuitBuilder<Var, IO, L, M> for SwitchedCircuitBuilder<Var, CB>
where
    CB: CircuitBuilder<Var, IO, L, M>,
    Var: CircuitBuilderVar,
{
    fn alloc(&mut self, location: Location) -> Var {
        self.cb.alloc(location)
    }
    fn enforce(&mut self, location: Location, a: Var, b: Var, a_times_b: Var) {
        self.cb.enforce(location, a, b, a_times_b)
    }
    fn lookup(&mut self, namespace: L, address: Var, val: Var) {
        self.cb.lookup(namespace, address, val)
    }
    fn memory(&mut self, namespace: M, address: Var, old: Var, new: Var) {
        self.cb.memory(namespace, address, old, new)
    }
    fn nest<'b>(&'b mut self, location: Location) -> impl CircuitBuilder<Var, IO, L, M> + 'b {
        SwitchedCircuitBuilder {
            switch: self.switch.clone(),
            cb: self.cb.nest(location),
        }
    }
    fn assert_size(&mut self, offset_size: (u32, u32)) {
        self.cb.assert_size(offset_size)
    }
    fn input(&mut self, name: IO) -> Var {
        // FIXME: use name somehow here for better diagnostics
        let local = self.cb.alloc(l!("switched_input"));
        let global = self.cb.input(name);
        self.cb
            .enforce(l!(), self.switch.clone(), global, local.clone());
        local
    }
    fn output(&mut self, name: IO) -> Var {
        // FIXME: use name somehow here for better diagnostics
        let local = self.cb.alloc(l!("switched_output"));
        let global = self.cb.output(name);
        self.cb
            .enforce(l!(), self.switch.clone(), global, local.clone());
        local
    }
    fn link(&mut self, x: Var, y: Var) {
        unimplemented!("FIXME")
    }
}

struct Counter<'a> {
    counter: &'a mut usize,
}
#[derive(Clone, Debug)]
struct Var;

impl<'a, IO, L, M> CircuitBuilder<Var, IO, L, M> for Counter<'a> {
    fn alloc(&mut self, _: Location) -> Var {
        *self.counter += 1;
        Var
    }
    fn enforce(&mut self, _: Location, _: Var, _: Var, _: Var) {}
    fn lookup(&mut self, _: L, _: Var, _: Var) {}
    fn memory(&mut self, _: M, _: Var, _: Var, _: Var) {}
    fn nest<'b>(&'b mut self, _: Location) -> impl CircuitBuilder<Var, IO, L, M> + 'b {
        Counter {
            counter: self.counter,
        }
    }
    fn assert_size(&mut self, _: (u32, u32)) {}
    fn input(&mut self, _: IO) -> Var {
        *self.counter += 1;
        Var
    }
    fn output(&mut self, _: IO) -> Var {
        *self.counter += 1;
        Var
    }
    fn link(&mut self, x: Var, y: Var) {
        unimplemented!("FIXME")
    }
}

impl From<i128> for Var {
    fn from(_: i128) -> Self {
        Var
    }
}

impl From<(i128, i128)> for Var {
    fn from(_: (i128, i128)) -> Self {
        Var
    }
}

impl<T> Add<T> for Var {
    type Output = Var;
    fn add(self, _: T) -> Self::Output {
        Var
    }
}

impl<T> Sub<T> for Var {
    type Output = Var;
    fn sub(self, _: T) -> Self::Output {
        Var
    }
}

impl Mul<(i128, i128)> for Var {
    type Output = Var;
    fn mul(self, _: (i128, i128)) -> Self::Output {
        Var
    }
}
impl Mul<i128> for Var {
    type Output = Var;
    fn mul(self, _: i128) -> Self::Output {
        Var
    }
}

impl CircuitBuilderVar for Var {}

impl<B, C, IO: Clone, L, M> Circuit<IO, L, M> for SwitchedCircuit<B, C>
where
    C: BranchedCircuit<B, IO, L, M>,
    B: Send + Sync,
{
    fn run<Var: CircuitBuilderVar, Builder: CircuitBuilder<Var, IO, L, M>>(&self, mut cb: Builder) {
        let mut running_sum = Var::from(0);
        let branches = self.1.branches();
        for branch in branches {
            let switch = cb.alloc(l!("switch"));
            cb.enforce(
                l!("switch must be binary"),
                switch.clone(),
                Var::from(1) - switch.clone(),
                0.into(),
            );
            self.1.run(
                branch,
                SwitchedCircuitBuilder {
                    switch: switch.clone(),
                    // FIXME: don't nest!
                    cb: cb.nest(l!()),
                },
            );
            running_sum = running_sum + switch;
        }
        {
            cb.enforce(
                l!("one switch or no switch is on"),
                running_sum.clone(),
                Var::from(1) - running_sum.clone(),
                0.into(),
            );
        }
        {
            let names = self.1.io();
            let is_empty_step = Var::from(1) - running_sum;
            for name in names {
                let input = cb.input(name.clone());
                let output = cb.output(name);
                cb.enforce(
                    l!("empty step thus no change"),
                    is_empty_step.clone(),
                    input - output,
                    0.into(),
                );
            }
        }
    }
}

pub fn calculate_offsets<B, IO, L, M, C: BranchedCircuit<B, IO, L, M>>(
    c: &C,
) -> impl Iterator<Item = usize> {
    let mut offset = 0;
    c.branches().map(move |branch| {
        let this_offset = offset;
        // switch variable is implicit
        let mut counter = 1;
        c.run(
            branch,
            Counter {
                counter: &mut counter,
            },
        );
        offset += counter;
        this_offset
    })
}
