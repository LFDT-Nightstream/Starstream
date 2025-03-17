#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit, panic::PanicInfo};

#[macro_export]
macro_rules! metadata {
    ($x:expr) => {{
        #[link_section = "starstream"]
        static FOO: [u8; $x.len()] = *$x;
        core::hint::black_box(FOO);
    }};
}

// ----------------------------------------------------------------------------
// Model types

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct CodeHash {
    raw: [u8; 32],
}

impl CodeHash {
    pub const fn zero() -> Self {
        CodeHash { raw: [0; 32] }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct PublicKey {
    _0: (),
}

impl PublicKey {
    pub fn zero() -> PublicKey {
        PublicKey { _0: () }
    }
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

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum UtxoStatus {
    Returned = 0,
    Yielded = 1,
}

impl UtxoStatus {
    #[inline]
    pub fn can_resume(&self) -> bool {
        matches!(self, UtxoStatus::Yielded)
    }
}

// ----------------------------------------------------------------------------
// Common import environment

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn abort();

    // Debug log
    #[link_name = "starstream_log"]
    unsafe fn starstream_log(buf: *const u8, len: usize);

    #[link_name = "starstream_coordination_code"]
    pub safe fn coordination_code() -> CodeHash;

    #[link_name = "starstream_this_code"]
    pub safe fn this_code() -> CodeHash;
}

#[inline]
pub fn log(buf: &[u8]) {
    unsafe { starstream_log(buf.as_ptr(), buf.len()) }
}

pub fn panic_handler(_: &PanicInfo) -> ! {
    unsafe {
        abort();
        // abort() is meant to not return, but just in case:
        loop {}
    }
}

/// Set up Starstream's panic handler for this crate.
///
/// Exported as a macro because `cargo test`ing a contract will add a `test`
/// -> `std` dependency that conflicts with the non-test version of this crate.
#[macro_export]
macro_rules! panic_handler {
    () => {
        #[cfg(not(test))]
        #[panic_handler]
        fn panic_handler(info: &core::panic::PanicInfo) -> ! {
            $crate::panic_handler(info)
        }
    };
}

pub fn assert_tx_signed_by(_key: PublicKey) {
    // TODO: assert that this coordination-script-call is signed by `key`
}

// ----------------------------------------------------------------------------
// Common export environment

#[macro_export]
macro_rules! effect {
    (
        $(#[$attr:meta])*
        pub effect
        $name:ident
        (
            $($param:ident: $pty:ty),*
            $(,)?
        )
        -> $return_ty:ty
    ) => {
        $(#[$attr])*
        pub struct $name;
        impl $name {
            #[inline]
            pub fn raise($($param: $pty),*) -> $return_ty {
                #[link(wasm_import_module = "starstream_raise_effect")]
                unsafe extern "C" {
                    #[link_name = stringify!($name)]
                    safe fn inner($($param: $pty),*) -> $return_ty;
                }
                inner($($param),*)
            }

            #[inline]
            pub fn handle<Output, Body, Handler>(
                body: Body,
                handler: Handler,
            ) -> Body::Output
            where
                Body: FnOnce() -> Output,
                Handler: FnMut($($pty),*) -> Result<$return_ty, Body::Output>,
            {
                // TODO
                body()
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Token export environment

#[repr(C)]
pub struct TokenStorage {
    pub id: u64,
    pub amount: u64,
}

/*
pub trait TokenIntermediate {
    /// Called when the token is minted. Panics if the mint is invalid.
    fn mint(self) -> TokenStorage;
    /// Called when the token is burned. Panics if the burn is invalid.
    fn burn(storage: TokenStorage) -> Self;
}
*/

#[macro_export]
macro_rules! token_export {
    (
        for $intermediate:ty;
        mint fn $mint_fn:ident($self:ident: Self) -> TokenStorage $mint_body:block
        burn fn $burn_fn:ident($storage:ident: TokenStorage) -> Self $burn_body:block
    ) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $mint_fn($self: $intermediate) -> $crate::TokenStorage $mint_body

        #[unsafe(no_mangle)]
        pub extern "C" fn $burn_fn($storage: $crate::TokenStorage) -> $intermediate $burn_body
    }
}

// ----------------------------------------------------------------------------
// Token import environment

#[repr(C)]
pub struct TokenHandle<T: ?Sized> {
    ptr: u32,
    _phantom: PhantomData<*mut T>,
}

impl<T: ?Sized> Clone for TokenHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for TokenHandle<T> {}

pub trait Token {
    type Intermediate;
    fn mint(i: Self::Intermediate) -> Self;
    fn burn(self) -> Self::Intermediate;
}

#[macro_export]
macro_rules! token_import {
    (
        from $module:expr;
        type $handle_name:ident;
        intermediate struct $intermediate_name:ident {
            $($contents:tt)*
        }
        mint fn $mint_fn:ident;
        burn fn $burn_fn:ident;
    ) => {
        #[repr(C)]
        pub struct $intermediate_name {
            $($contents)*
        }

        impl $intermediate_name {
            #[inline]
            pub fn mint(self) -> $handle_name {
                <$handle_name as $crate::Token>::mint(self)
            }
        }

        #[link(wasm_import_module = $module)]
        unsafe extern "C" {
            safe fn $mint_fn(intermediate: $intermediate_name) -> $crate::TokenHandle<$handle_name>;
            safe fn $burn_fn(handle: $crate::TokenHandle<$handle_name>) -> $intermediate_name;
        }

        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $handle_name($crate::TokenHandle<$handle_name>);

        impl $crate::Token for $handle_name {
            type Intermediate = $intermediate_name;

            #[inline]
            fn mint(i: Self::Intermediate) -> Self {
                Self($mint_fn(i))
            }

            #[inline]
            fn burn(self) -> Self::Intermediate {
                $burn_fn(self.0)
            }
        }
    };
}

// ----------------------------------------------------------------------------
// UTXO export (main/implementation) environment

#[link(wasm_import_module = "starstream_utxo_env")]
unsafe extern "C" {
    unsafe fn starstream_yield(
        name: *const u8,
        name_len: usize,
        data: *const (),
        data_size: usize,
        resume_arg: *mut (),
        resume_arg_size: usize,
    );
}

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

// ----------------------------------------------------------------------------
// UTXO import (lib) interface

#[repr(C)]
pub struct UtxoHandle<T: ?Sized> {
    ptr: u32,
    _phantom: PhantomData<*mut T>,
}

impl<T: ?Sized> Clone for UtxoHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for UtxoHandle<T> {}

pub trait Utxo {
    type Resume;

    fn status(self) -> UtxoStatus;
    fn resume(self, arg: Self::Resume);

    fn can_resume(self) -> bool
    where
        Self: Sized,
    {
        self.status().can_resume()
    }

    fn next(self)
    where
        Self: Sized + Utxo<Resume = ()>,
    {
        self.resume(())
    }
}

#[macro_export]
macro_rules! utxo_import {
    (
        $module:expr;
        $name:ident;
        $status_fn:ident;
        $resume_fn:ident;
        $resume_ty:ty;
    ) => {
        #[link(wasm_import_module = $module)]
        unsafe extern "C" {
            safe fn $status_fn(utxo: $name) -> $crate::UtxoStatus;
            unsafe fn $resume_fn(utxo: $name, resume_arg: *const (), resume_arg_size: usize);
        }

        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $name($crate::UtxoHandle<$name>);

        impl $crate::Utxo for $name {
            type Resume = $resume_ty;

            #[inline]
            fn status(self) -> $crate::UtxoStatus {
                $status_fn(self)
            }

            #[inline]
            fn resume(self, arg: Self::Resume) {
                unsafe {
                    $resume_fn(
                        self,
                        &raw const arg as *const (),
                        core::mem::size_of_val(&arg),
                    );
                }
            }
        }
    };
}

// ----------------------------------------------------------------------------
// Coordination script environment
