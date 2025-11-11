use std::marker::PhantomData;

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
    fn zero(&mut self) -> Var {
        self.cb.zero()
    }
    fn one(&mut self) -> Var {
        self.switch.clone()
    }
    fn lit(&mut self, n: i128) -> Var {
        self.switch.clone() * n
    }
    fn lit_rat(&mut self, n: i128, d: i128) -> Var {
        self.switch.clone() * (n, d)
    }
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
    fn assert_size(&mut self, offset_size: (usize, usize)) {
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
}

impl<B, C, IO: Clone, L, M> Circuit<IO, L, M> for SwitchedCircuit<B, C>
where
    C: BranchedCircuit<B, IO, L, M>,
    B: Send + Sync,
{
    fn run<Var: CircuitBuilderVar, Builder: CircuitBuilder<Var, IO, L, M>>(&self, mut cb: Builder) {
        let mut running_sum = cb.zero();
        let branches = self.1.branches();
        for branch in branches {
            let switch = cb.alloc(l!("switch"));
            let zero = cb.zero();
            let one = cb.one();
            cb.enforce(
                l!("switch must be binary"),
                switch.clone(),
                one - switch.clone(),
                zero,
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
            let zero = cb.zero();
            let one = cb.one();
            cb.enforce(
                l!("one switch or no switch is on"),
                running_sum.clone(),
                one.clone() - running_sum.clone(),
                zero,
            );
        }
        {
            let names = self.1.io();
            let one = cb.one();
            let is_empty_step = one - running_sum;
            for name in names {
                let input = cb.input(name.clone());
                let output = cb.output(name);
                let zero = cb.zero();
                cb.enforce(
                    l!("empty step thus no change"),
                    is_empty_step.clone(),
                    input - output,
                    zero,
                );
            }
        }
    }
}
