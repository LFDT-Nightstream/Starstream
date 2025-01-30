#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit, panic::PanicInfo};

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    #[link_name = "starstream_log"]
    pub safe fn log(value: u32);
    unsafe fn abort();
    unsafe fn starstream_yield(
        name: *const u8, name_len: usize,
        data: *const (), data_size: usize,
        resume_arg: *mut (), resume_arg_size: usize,
    );

    #[link_name = "starstream_coordination_code"]
    pub safe fn coordination_code() -> CodeHash;

    #[link_name = "starstream_this_code"]
    pub safe fn this_code() -> CodeHash;
}

#[macro_export]
macro_rules! metadata {
    ($x:expr) => {{
        #[link_section = "starstream"]
        static FOO: [u8; $x.len()] = *$x;
        core::hint::black_box(FOO);
    }}
}

#[cfg_attr(not(test), panic_handler)]
#[allow(dead_code)]
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
        *self
    }
}

impl<T: ?Sized> Copy for Utxo<T> {}

// yield = fn(a...) -> (b...)
// resume = (b...) -> (a...)

pub fn sleep<Resume, Yield>(data: &Yield) -> Resume {
    let name = core::any::type_name::<Yield>();

    let mut resume_arg = MaybeUninit::<Resume>::uninit();
    unsafe {
        starstream_yield(
            name.as_ptr(),
            name.len(),
            data as *const Yield as *const (),
            size_of::<Yield>(),
            resume_arg.as_mut_ptr() as *mut (),
            size_of::<Resume>(),
        );
        // SAFETY TODO: unsound if we're resumed with a value that isn't
        // actually a valid instance of Resume due to ABI trouble.
        resume_arg.assume_init()
    }
}

pub fn sleep_mut<Resume, Yield>(data: &mut Yield) -> Resume {
    sleep(data)
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct CodeHash {
    raw: [u8; 32],
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

    pub fn sign(&self, _message: &[u8]) -> SignedMessage {
        SignedMessage
    }
}

impl SignedMessage {
    pub fn is_valid(&self, _message: &[u8]) -> bool {
        true
    }
}

pub fn assert_call_signature(key: PublicKey /*, message: &[u8]  */) {
    // TODO: assert that this coordination-script-call is signed by `key`
}
