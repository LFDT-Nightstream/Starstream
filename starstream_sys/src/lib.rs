#![feature(coroutine_trait)]
#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit, panic::PanicInfo};

#[link(wasm_import_module = "env")]
extern "C" {
    fn abort();
    fn starstream_yield(data: *const (), data_size: usize, resume_arg: *mut (), resume_arg_size: usize);
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    unsafe {
        abort();
        // abort() is meant to not return, but just in case:
        loop {}
    }
}

pub trait UtxoCoroutine {
    type Resume;
    unsafe fn ffi_status(utxo: &Utxo<Self>) -> bool;
    unsafe fn ffi_resume(utxo: &mut Utxo<Self>, arg: Self::Resume);
}

#[repr(C)]
pub struct Utxo<T: ?Sized> {
    ptr: usize,
    _phantom: PhantomData<*mut T>,
}

impl<T: ?Sized + UtxoCoroutine> Utxo<T> {
    pub fn can_resume(&self) -> bool {
        unsafe { T::ffi_status(self) }
    }

    pub fn resume(&mut self, arg: T::Resume) {
        unsafe { T::ffi_resume(self, arg) }
    }

    pub fn next(&mut self)
    where
        T: UtxoCoroutine<Resume = ()>,
    {
        self.resume(())
    }
}

// yield = fn(a...) -> (b...)
// resume = (b...) -> (a...)

pub fn sleep<Resume, Yield>(data: Yield) -> Resume {
    unsafe {
        let mut resume_arg = MaybeUninit::<Resume>::uninit();
        starstream_yield(
            &raw const data as *const (),
            size_of::<Yield>(),
            resume_arg.as_mut_ptr() as *mut (),
            size_of::<Resume>(),
        );
        resume_arg.assume_init()
    }
}
