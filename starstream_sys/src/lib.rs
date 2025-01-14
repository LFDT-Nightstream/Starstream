#![feature(coroutine_trait)]
#![no_std]

use core::{any::{Any, TypeId}, marker::PhantomData, mem::MaybeUninit, ops::{CoroutineState, Deref}, panic::PanicInfo, pin::Pin};

extern "C" {
    fn abort();
    fn ss_yield(data: *const (), data_size: usize, resume_arg: *mut (), resume_arg_size: usize);
    fn ss_resume(resume_arg: *const (), resume_arg_size: usize);
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    unsafe {
        abort();
        // abort() is meant to not return, but just in case:
        loop {}
    }
}

#[repr(C)]
pub struct Utxo<Data> {
    data: Data,
}

impl<Data> Deref for Utxo<Data> {
    type Target = Data;
    fn deref(&self) -> &Data {
        &self.data
    }
}

// yield = fn(a...) -> (b...)
// resume = (b...) -> (a...)

pub fn yield_<ResumeArg, Data>(data: Data) -> ResumeArg
    // where Data: Coroutine<ResumeArg>
{
    unsafe {
        let mut resume_arg = MaybeUninit::<ResumeArg>::uninit();
        ss_yield(
            &raw const data as *const (),
            size_of::<Data>(),
            resume_arg.as_mut_ptr() as *mut (),
            size_of::<ResumeArg>(),
        );
        resume_arg.assume_init()
    }
}

pub fn resume<Data, ResumeArg>(utxo: &mut Utxo<Data>, resume: ResumeArg) -> CoroutineState<(), ()>
    // where Data: Coroutine<ResumeArg>
{
    unimplemented!()
}
