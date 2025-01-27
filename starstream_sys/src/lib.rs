#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit, panic::PanicInfo};

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    #[link_name = "starstream_log"]
    pub safe fn log(value: u32);
    unsafe fn abort();
    unsafe fn starstream_yield(data: *const (), data_size: usize, resume_arg: *mut (), resume_arg_size: usize);
}

#[macro_export]
macro_rules! metadata {
    ($x:expr) => {{
        #[link_section = "starstream"]
        static FOO: [u8; $x.len()] = *$x;
        core::hint::black_box(FOO);
    }}
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
    fn ffi_status(utxo: Utxo<Self>) -> bool;
    fn ffi_resume(utxo: Utxo<Self>, arg: Self::Resume);
}

#[repr(C)]
pub struct Utxo<T: ?Sized> {
    ptr: u32,
    _phantom: PhantomData<*mut T>,
}

impl<T: ?Sized + UtxoCoroutine> Utxo<T> {
    pub fn can_resume(self) -> bool {
        T::ffi_status(self)
    }

    pub fn resume(self, arg: T::Resume) {
        T::ffi_resume(self, arg)
    }

    pub fn next(self)
    where
        T: UtxoCoroutine<Resume = ()>,
    {
        self.resume(())
    }
}

impl<T: ?Sized> Clone for Utxo<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr.clone(), _phantom: self._phantom.clone() }
    }
}

impl<T: ?Sized> Copy for Utxo<T> {}

// yield = fn(a...) -> (b...)
// resume = (b...) -> (a...)

pub fn sleep<Resume, Yield>(data: &Yield) -> Resume {
    unsafe {
        let mut resume_arg = MaybeUninit::<Resume>::uninit();
        starstream_yield(
            data as *const Yield as *const (),
            size_of::<Yield>(),
            resume_arg.as_mut_ptr() as *mut (),
            size_of::<Resume>(),
        );
        resume_arg.assume_init()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct PublicKey {
    _0: (),
}

#[derive(Clone, Copy)]
pub struct PrivateKey;

#[derive(Clone, Copy)]
pub struct SignedMessage;

impl PrivateKey {
    pub fn public_key(&self) -> PublicKey {
        PublicKey { _0: () }
    }

    pub fn sign(&self, message: &[u8]) -> SignedMessage {
        SignedMessage
    }
}

impl SignedMessage {
    pub fn is_valid(&self, message: &[u8]) -> bool {
        true
    }
}
