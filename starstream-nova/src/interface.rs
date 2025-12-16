use std::fmt::Debug;
use std::ops::{Add, Mul, Sub};

#[derive(Clone, Copy, Default)]
pub struct Location {
    pub tag: u64,
    pub label: &'static str,
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
}

pub const fn const_hash_str(s: &str) -> u64 {
    const C: u64 = 17023828661126941424;
    let mut acc: u64 = 0;
    let mut i = 0;
    let bytes = s.as_bytes();
    while i < bytes.len() {
        acc = acc.wrapping_mul(C).wrapping_add(bytes[i] as u64);
        i += 1;
    }
    acc
}

#[macro_export]
macro_rules! l {
    ($name:literal) => {{
        crate::interface::Location {
            label: $name,
            tag: const { crate::interface::const_hash_str($name) },
            file: file!(),
            line: line!(),
            column: column!(),
        }
    }};
    () => {{
        crate::interface::Location {
            label: "",
            tag: 0,
            file: file!(),
            line: line!(),
            column: column!(),
        }
    }};
}

// TODO: consider making Copy, or at least making implementations clone shallowly
/// Literals are always specified as i128,
/// albeit the actual value in the field is `n % p`,
/// such that `-1i128` will be `p - 1` in the field, and so on.
pub trait CircuitBuilderVar:
    From<i128>
    + From<(i128, i128)>
    + Add<Self, Output = Self>
    + Add<i128, Output = Self>
    + Add<(i128, i128), Output = Self>
    + Sub<Self, Output = Self>
    + Sub<i128, Output = Self>
    + Sub<(i128, i128), Output = Self>
    + Mul<(i128, i128), Output = Self> // multiplication by rational (n, d)
    + Mul<i128, Output = Self>
    // TODO: add Div
    + Clone
    + Debug
{
}

pub trait CircuitBuilder<Var, IO, L, M> {
    #[must_use]
    fn alloc(&mut self, location: Location) -> Var;
    fn enforce(&mut self, location: Location, a: Var, b: Var, a_times_b: Var);
    fn lookup(&mut self, namespace: L, address: Var, val: Var);
    fn memory(&mut self, namespace: M, address: Var, old: Var, new: Var);
    #[must_use]
    fn nest<'a>(&'a mut self, location: Location) -> impl CircuitBuilder<Var, IO, L, M> + 'a;
    /// Check that `offset` witnesses have been allocated until now.
    /// Useful for ensuring witness generation is done correctly.
    fn assert_size(&mut self, offset_size: (u32, u32));
    /*
    /// If both represent raw witnesses from `alloc`, then
    fn link(&mut self, x: Var, y: Var);
    */
    fn input(&mut self, name: IO) -> Var;
    fn output(&mut self, name: IO) -> Var;
    /* FIXME: add
    /// Mark output as unconstrained,
    /// to avoid errors when no constraints are added for an output
    fn unconstrained(&mut self, name: IO);
    */
    /// "Link" the two variables if possible,
    /// and eliminate an allocated variable.
    /// If both sides are computations, this will err.
    /// If both sides are incompatible literals, this will err.
    /// If one side is a computation and the other is a literal, this will err.
    /// If both sides are inputs/outputs, this will err.
    /// If one side is an input/output, and the other is a computation or literal, this will err.
    /// This could in principle be done automatically.
    /// TODO: should this become an equality constraint instead of erring?
    fn link(&mut self, x: Var, y: Var);
}

pub trait Circuit<IO, L, M>: Send + Sync {
    fn run<Var: CircuitBuilderVar, Builder: CircuitBuilder<Var, IO, L, M>>(&self, builder: Builder);
}

pub trait BranchedCircuit<B, IO, L, M>: Send + Sync {
    fn branches(&self) -> impl Iterator<Item = B>;
    fn io(&self) -> impl Iterator<Item = IO>;
    fn run<Var: CircuitBuilderVar, Builder: CircuitBuilder<Var, IO, L, M>>(
        &self,
        branch: B,
        builder: Builder,
    );
}
