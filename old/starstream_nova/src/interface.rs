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
pub trait CircuitBuilderVar:
    Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<(i128, i128), Output = Self> // multiplication by rational (n, d)
    + Mul<i128, Output = Self>
    // TODO: add Div
    + Clone
    + Debug
{
}

pub trait CircuitBuilder<Var, L, M> {
    fn zero(&mut self) -> Var;
    fn one(&mut self) -> Var;
    // Literals are always specified as i128,
    // albeit the actual value in the field is `n % p`,
    // such that `-1i128` will be `p - 1` in the field, and so on.
    #[must_use]
    fn lit(&mut self, n: i128) -> Var;
    #[must_use]
    fn lit_rat(&mut self, n: i128, d: i128) -> Var;
    #[must_use]
    fn alloc(&mut self, location: Location) -> Var;
    fn enforce(&mut self, location: Location, a: Var, b: Var, a_times_b: Var);
    fn lookup(&mut self, namespace: L, address: Var, val: Var);
    fn memory(&mut self, namespace: M, address: Var, old: Var, new: Var);
    #[must_use]
    fn nest<'a>(&'a mut self, location: Location) -> impl CircuitBuilder<Var, L, M> + 'a;
    /// Check that `offset` witnesses have been allocated until now.
    /// Useful for ensuring witness generation is done correctly.
    fn assert_offset(&mut self, offset: usize);
}

pub trait Circuit<IO, L, M>: Send + Sync {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var, L, M>>(
        &self,
        builder: B,
        // TODO: maybe you should get it from the builder?
        input: impl Fn(IO) -> Var,
        output: impl Fn(IO) -> Var,
    );
}
