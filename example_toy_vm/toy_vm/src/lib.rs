#![no_std]
#![feature(allocator_api)]

use alloc::{alloc::Allocator, rc::Rc, vec};
use core::{cmp::Ordering, fmt};
use rand::RngCore;
pub use value::Value;
use vec::Vec;
pub mod value;

extern crate alloc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TestResult {
    L = 0,
    E = 1,
    G = 2,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op<A: Allocator + Copy> {
    Push(Value<A>),
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Dup { i: u32 },
    Test,
    CondJump { i: u32, on: TestResult },
    Jump { i: u32 },
    Swap { i: u32 },
    Rand { len: u32 },
    Sort,
}

pub struct Stack<A: Allocator + Copy> {
    inner: Vec<Value<A>, A>,
}

impl<A: Allocator + Copy> core::fmt::Debug for Stack<A>
where
    Vec<Value<A>, A>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Stack").field("inner", &self.inner).finish()
    }
}
impl<A: Allocator + Copy> Eq for Stack<A> {}

impl<A: Allocator + Copy> PartialEq for Stack<A> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<A: Allocator + Copy> Stack<A> {
    pub fn new(allocator: A) -> Self {
        let stack: Vec<Value<A>, A> = Vec::new_in(allocator);

        Stack { inner: stack }
    }
    pub fn push(&mut self, value: Value<A>) {
        self.inner.push(value);
    }

    pub fn pop(&mut self) -> Result<Value<A>, Error> {
        self.inner.pop().ok_or(Error::EmptyStackError)
    }

    pub fn swap(&mut self, a: usize, b: usize) {
        self.inner.swap(a, b);
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn get(&self, pos: usize) -> Result<&Value<A>, Error> {
        self.inner.get(pos).ok_or(Error::EmptyStackError)
    }
}

pub enum Error {
    EmptyStackError,
    TypeError,
}

// toy vm implementing a basic stack machine
//
// arithmetic operations work on vectors
//
// the allocator is parametric, so the strategy can be controlled by the caller
// NOTE: the allocator must be the same across calls if the resulting stack is re-used.
pub fn run<A: Allocator + Copy, R: RngCore>(
    ops: &[Op<A>],
    allocator: A,
    mut stack: Stack<A>,
    mut rng: R,
) -> Result<Stack<A>, Error> {
    let mut pc = 0u32;

    fn combine_arrays<A: Allocator + Copy>(
        a: &[u32],
        b: &[u32],
        f: impl Fn(u32, u32) -> u32,
        allocator: A,
    ) -> Value<A> {
        let result_len = core::cmp::max(a.len(), b.len());

        let mut result = Vec::new_in(allocator);

        for e in a
            .iter()
            .cycle()
            .zip(b.iter().cycle())
            .take(result_len)
            .map(|(a, b)| f(*a, *b))
        {
            result.push(e);
        }

        Value::Array(Rc::new_in(result, allocator))
    }

    while let Some(op) = ops.get(pc as usize) {
        match op {
            Op::Push(imm) => {
                stack.push(imm.clone());
            }
            Op::Pop => {
                stack.pop()?;
            }
            op @ (Op::Add | Op::Sub | Op::Mul | Op::Div) => {
                let a = stack.pop()?;
                let b = stack.pop()?;

                match (a, b) {
                    (Value::Array(a), Value::Array(b)) => {
                        stack.push(combine_arrays(
                            &a,
                            &b,
                            match op {
                                Op::Add => |a: u32, b: u32| a.wrapping_add(b),
                                Op::Sub => |a: u32, b: u32| a.wrapping_sub(b),
                                Op::Mul => |a: u32, b: u32| a.wrapping_mul(b),
                                Op::Div => |a: u32, b: u32| a.wrapping_div(b),
                                _ => unreachable!(),
                            },
                            allocator,
                        ));
                    }
                    _ => return Err(Error::TypeError),
                }
            }
            Op::Dup { i } => {
                let x = stack.get(stack.len() - 1 - *i as usize)?;

                stack.push(x.clone());
            }
            Op::Test => {
                let a = stack.pop()?;
                let b = stack.pop()?;

                match (a, b) {
                    (Value::Array(a), Value::Array(b)) => {
                        let res = match a.cmp(&b) {
                            Ordering::Less => TestResult::L,
                            Ordering::Equal => TestResult::E,
                            Ordering::Greater => TestResult::G,
                        };

                        stack.push(Value::TestResult(res));
                    }
                    _ => return Err(Error::TypeError),
                }
            }
            Op::CondJump { i, on } => {
                let test_result = stack.pop()?;

                let Value::TestResult(test_result) = test_result else {
                    return Err(Error::TypeError);
                };

                if on == &test_result {
                    pc = *i;
                    continue;
                }
            }
            Op::Jump { i } => {
                pc = *i;
                continue;
            }
            Op::Swap { i } => {
                let len = stack.len();

                if len < 2 {
                    return Err(Error::EmptyStackError);
                }

                stack.swap(len - 1, len - 1 - *i as usize);
            }
            Op::Sort => {
                let arr = stack.pop()?;

                let Value::Array(mut arr) = arr else {
                    return Err(Error::TypeError);
                };

                #[cfg(feature = "starstream")]
                {
                    let sorted = starstream_lookup_arguments::array_sort(&arr, allocator);

                    let arr_borrow = Rc::make_mut(&mut arr);

                    *arr_borrow = sorted;
                }

                // outside of starstream we just run sort directly.
                // this is needed for the unit tests to work in particular
                #[cfg(not(feature = "starstream"))]
                {
                    Rc::make_mut(&mut arr).sort();
                }

                stack.push(Value::Array(arr));
            }
            Op::Rand { len } => {
                let mut result = Vec::new_in(allocator);

                result.reserve_exact(*len as usize);

                for _ in 0..*len {
                    result.push(rng.next_u32())
                }

                stack.push(Value::Array(Rc::new_in(result, allocator)));
            }
        }

        pc += 1;
    }

    Ok(stack)
}

#[cfg(feature = "starstream")]
mod starstream_lookup_arguments {
    use alloc::{alloc::Allocator, vec::Vec};

    #[cfg(feature = "starstream")]
    pub(crate) struct SortLookupArguments {
        pub(crate) input_ptr: *const u8,
        pub(crate) output_ptr: *mut u8,
        pub(crate) len: usize,
    }

    pub(crate) fn array_sort<A: Allocator + Copy>(arr: &[u32], allocator: A) -> Vec<u32, A> {
        // validation part:
        // we want to check that:
        //   1. The result is sorted.
        //   2. The result is made of elements of the original array.
        //   3. No repetitions were introduced.
        //
        // we sort the indices instead of the actual array, since that way it's
        // easy to check 2 an 3.
        let mut argsort = Vec::new_in(allocator);
        argsort.extend(0u32..arr.len() as u32);

        let mut seen_before = Vec::new_in(allocator);
        seen_before.extend(core::iter::repeat_n(false, arr.len()));

        let args = SortLookupArguments {
            input_ptr: (&arr[0]) as *const u32 as *const u8,
            output_ptr: (&mut argsort[0]) as *mut u32 as *mut u8,
            len: core::mem::size_of_val(arr),
        };

        unsafe {
            starstream::starstream_run_unconstrained(
                sort_lookup_argument_external as *const (),
                &args as *const _ as *const (),
            )
        };

        for i in 0..argsort.len() {
            //   3. No repetitions were introduced.
            assert!(!seen_before[i]);
            seen_before[i] = true;
            // 2. The result is made of elements of the original array (otherwise
            // the indexes will be out of bounds).
            argsort[i] = arr[argsort[i] as usize];
            if i > 0 {
                // 1. The result is actually a sort.
                assert!(argsort[i] >= argsort[i - 1]);
            }
        }

        argsort
    }

    #[cfg(feature = "starstream")]
    pub(crate) unsafe extern "C" fn sort_lookup_argument_external(args_erased: *const ()) {
        let args = unsafe { args_erased.cast::<SortLookupArguments>().as_ref().unwrap() };

        let value_array_in_bytes = unsafe { core::slice::from_raw_parts(args.input_ptr, args.len) };

        let (_, s, _) = unsafe { value_array_in_bytes.align_to::<u32>() };

        let index_array_in_bytes =
            unsafe { core::slice::from_raw_parts_mut(args.output_ptr, args.len) };

        let (_, si, _) = unsafe { index_array_in_bytes.align_to_mut::<u32>() };

        si.sort_by_key(|i| s[*i as usize]);
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::EmptyStackError => write!(f, "attempted to pop from an empty stack"),
            Error::TypeError => write!(f, "Type error"),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl core::error::Error for Error {}

#[cfg(test)]
mod tests {
    use super::*;

    use rand::SeedableRng as _;
    use talc::*;

    const MEMORY_SIZE: usize = 10 * 1024;

    // max size of the stack
    static mut ARENA: [u8; MEMORY_SIZE] = [0; MEMORY_SIZE];

    static ALLOCATOR: Talck<spin::Mutex<()>, ClaimOnOom> = Talc::new(unsafe {
        ClaimOnOom::new(Span::from_array(core::ptr::addr_of!(ARENA).cast_mut()))
    })
    .lock();

    #[test]
    fn add_x_y() {
        let stack = Stack::new(&ALLOCATOR);

        let rng = rand_chacha::ChaCha12Rng::seed_from_u64(42);

        run(
            &[
                Op::Push(Value::from_slice(&[3u32, 5], &ALLOCATOR)),
                Op::Push(Value::from_slice(&[1, 2], &ALLOCATOR)),
                Op::Add,
            ],
            &ALLOCATOR,
            stack,
            rng,
        )
        .unwrap();
    }

    #[test]
    fn test_dup() {
        let stack = Stack::new(&ALLOCATOR);

        let mut rng = rand_chacha::ChaCha12Rng::seed_from_u64(42);

        let stack1 = run(
            &[
                Op::Push(Value::from_slice(&[3, 1], &ALLOCATOR)),
                Op::Push(Value::from_slice(&[5, 5], &ALLOCATOR)),
                Op::Dup { i: 1 },
            ],
            &ALLOCATOR,
            stack,
            &mut rng,
        )
        .unwrap();

        let stack = Stack::new(&ALLOCATOR);

        let stack2 = run(
            &[
                Op::Push(Value::from_slice(&[3, 1], &ALLOCATOR)),
                Op::Push(Value::from_slice(&[5, 5], &ALLOCATOR)),
                Op::Push(Value::from_slice(&[3, 1], &ALLOCATOR)),
            ],
            &ALLOCATOR,
            stack,
            &mut rng,
        )
        .unwrap();

        assert_eq!(stack1, stack2);
    }

    #[test]
    fn test_sort() {
        let stack = Stack::new(&ALLOCATOR);

        let mut rng = rand_chacha::ChaCha12Rng::seed_from_u64(42);

        let mut stack = run(
            &[
                Op::Push(Value::from_slice(&[3, 1, 2], &ALLOCATOR)),
                Op::Sort,
            ],
            &ALLOCATOR,
            stack,
            &mut rng,
        )
        .unwrap();

        let res = stack.pop().unwrap();

        match res {
            Value::Array(items) => assert!(items.is_sorted()),
            Value::TestResult(_) => todo!(),
        }
    }
}
