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

#[macro_export]
macro_rules! l {
    ($name:literal) => {{
        crate::interface::Location {
            label: $name,
            tag: {
                let mut hasher = ::std::hash::DefaultHasher::new();
                ::std::hash::Hash::hash(&$name, &mut hasher);
                ::std::hash::Hasher::finish(&mut hasher)
            },
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
    Add<Self, Output = Self> + Sub<Self, Output = Self> + Mul<u128, Output = Self> + Clone + Debug
{
}

pub trait CircuitBuilder<Var> {
    fn zero(&mut self) -> Var;
    fn one(&mut self) -> Var;
    #[must_use]
    fn lit(&mut self, n: u128) -> Var;
    #[must_use]
    fn alloc(&mut self, location: Location) -> Var;
    fn enforce(&mut self, location: Location, a: Var, b: Var, a_times_b: Var);
    fn lookup(&mut self, namespace: Var, address: Var, val: Var);
    fn memory(&mut self, namespace: Var, address: Var, old: Var, new: Var);
    #[must_use]
    fn nest<'a>(&'a mut self, location: Location) -> impl CircuitBuilder<Var> + 'a;
}

pub trait Circuit: Send + Sync {
    fn run<Var: CircuitBuilderVar, B: CircuitBuilder<Var>>(
        &self,
        builder: B,
        io: Vec<Var>,
    ) -> Vec<Var>;
}
