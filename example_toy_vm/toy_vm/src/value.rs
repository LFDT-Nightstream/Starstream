use alloc::{alloc::Allocator, rc::Rc, vec::Vec};

use crate::TestResult;

#[derive(Clone)]
pub enum Value<A: Allocator + Copy> {
    Array(Rc<Vec<u32, A>, A>),
    TestResult(TestResult),
}

impl<A: Allocator + Copy> Eq for Value<A> {}

impl<A: Allocator + Copy> PartialEq for Value<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::TestResult(l0), Self::TestResult(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<A: Allocator + Copy> Value<A> {
    pub fn from_slice(slice: &[u32], allocator: A) -> Self {
        let mut a = Vec::new_in(allocator);
        a.extend(slice);

        Self::Array(Rc::new_in(a, allocator))
    }

    pub fn from_iter(iter: impl Iterator<Item = u32>, allocator: A) -> Self {
        let mut a = Vec::new_in(allocator);
        a.extend(iter);

        Self::Array(Rc::new_in(a, allocator))
    }
}

impl<A: Allocator + Copy> core::fmt::Debug for Value<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Value::Array(array) => f.debug_tuple("Array").field(&array.as_slice()).finish(),
            Value::TestResult(result) => f.debug_tuple("TestResult").field(result).finish(),
        }
    }
}
