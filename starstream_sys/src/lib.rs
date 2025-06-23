#![no_std]
#![feature(coroutine_trait)]

use core::{fmt::Write, marker::PhantomData, mem::MaybeUninit, panic::PanicInfo};
#[doc(hidden)]
#[macro_use]
pub mod static_coroutine;
mod effects;

pub use effects::*;

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
// Common platform: Debug logging

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    #[link_name = "eprint"]
    unsafe fn eprint(buf: *const u8, len: usize);
}

struct DebugLog;

impl Write for DebugLog {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        unsafe {
            eprint(s.as_ptr(), s.len());
        }
        Ok(())
    }
}

#[doc(hidden)]
pub fn _eprint(args: core::fmt::Arguments) {
    let _ = DebugLog.write_fmt(args);
}

/// Print to the debug log.
#[macro_export]
macro_rules! eprint {
    ($($arg:tt)*) => {{
        $crate::_eprint(core::format_args!($($arg)*));
    }}
}

/// Print to the debug log, with a newline.
#[macro_export]
macro_rules! eprintln {
    () => {
        $crate::eprint!("\n")
    };
    ($($arg:tt)*) => {{
        $crate::_eprint(core::format_args!($($arg)*));
        // Separate because format_args_nl! is unstable and concat! breaks {var} syntax.
        $crate::_eprint(core::format_args!("\n"));
    }};
}

// ----------------------------------------------------------------------------
// Common platform: Panic and abort handling

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn abort();
}

#[doc(hidden)]
#[allow(clippy::empty_loop)]
pub fn _panic_handler(info: &PanicInfo) -> ! {
    unsafe {
        eprintln!("{info}");
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
            $crate::_panic_handler(info)
        }
    };
}

// ----------------------------------------------------------------------------
// Common import environment

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Get the hash of the code calling `this_code()`.
    #[link_name = "starstream_this_code"]
    pub safe fn this_code() -> CodeHash;

    /// Get the hash of the top-level coordination script currently executing.
    #[link_name = "starstream_coordination_code"]
    pub safe fn coordination_code() -> CodeHash;

    #[link_name = "starstream_keccak256"]
    unsafe fn precompile_keccak256(buf: *const u8, len: usize, result: *mut u8);
}

#[inline]
pub fn keccak256(buf: &[u8]) -> [u8; 32] {
    let mut out = [0u8; 32];
    unsafe { precompile_keccak256(buf.as_ptr(), buf.len(), out.as_mut_slice().as_mut_ptr()) };
    out
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
            ) -> Output
            where
                Body: FnOnce() -> Output,
                Handler: FnMut($($pty),*) -> Result<$return_ty, Output>,
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
        bind fn $bind_fn:ident($self:ident: Self) -> TokenStorage $bind_body:block
        unbind fn $unbind_fn:ident($storage:ident: TokenStorage) -> Self $unbind_body:block
    ) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $bind_fn($self: $intermediate) -> $crate::TokenStorage $bind_body

        #[unsafe(no_mangle)]
        pub extern "C" fn $unbind_fn($storage: $crate::TokenStorage) -> $intermediate $unbind_body
    }
}

// ----------------------------------------------------------------------------
// Token import environment

#[repr(C)]
pub struct TokenHandle<T: ?Sized> {
    ptr: u64,
    _phantom: PhantomData<*mut T>,
}

impl<T: ?Sized> TokenHandle<T> {
    fn from_raw(ptr: u64) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }

    pub fn unsafe_coerce<U: ?Sized>(self) -> TokenHandle<U> {
        TokenHandle::<U> {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }
}

impl<T: ?Sized> Clone for TokenHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for TokenHandle<T> {}

pub trait Token {
    type Intermediate;
    fn bind(i: Self::Intermediate) -> Self;
    fn unbind(self) -> Self::Intermediate;
}

#[macro_export]
macro_rules! token_import {
    (
        from $module:expr;
        type $handle_name:ident;
        intermediate struct $intermediate_name:ident {
            $($contents:tt)*
        }
        bind fn $bind_fn:ident;
        unbind fn $unbind_fn:ident;
    ) => {
        #[repr(C)]
        pub struct $intermediate_name {
            $($contents)*
        }

        impl $intermediate_name {
            #[inline]
            pub fn bind(self) -> $handle_name {
                <$handle_name as $crate::Token>::bind(self)
            }
        }

        #[link(wasm_import_module = $module)]
        unsafe extern "C" {
            safe fn $bind_fn(intermediate: $intermediate_name) -> $crate::TokenHandle<$handle_name>;
            safe fn $unbind_fn(handle: $crate::TokenHandle<$handle_name>) -> $intermediate_name;
        }

        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $handle_name($crate::TokenHandle<$handle_name>);

        impl $crate::Token for $handle_name {
            type Intermediate = $intermediate_name;

            #[inline]
            fn bind(i: Self::Intermediate) -> Self {
                Self($bind_fn(i))
            }

            #[inline]
            fn unbind(self) -> Self::Intermediate {
                $unbind_fn(self.0)
            }
        }

        impl From<$crate::TokenHandle<$handle_name>> for $handle_name {
            fn from(handle: $crate::TokenHandle<$handle_name>) -> $handle_name {
                Self(handle)
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
    ptr: u64,
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
                    )
                }
            }
        }
    };
}

// ----------------------------------------------------------------------------
// Coordination script environment
//

// ----------------------------------------------------------------------------
// Tokens getter
//
#[link(wasm_import_module = "starstream_utxo_env")]
unsafe extern "C" {
    unsafe fn starstream_get_tokens(data: *mut (), data_len: u32, skip: u32) -> u32;
}

pub struct BoundTokenIter {
    skip: usize,
}

#[repr(C)]
struct TokenStorageId {
    token_handle: u64,
    storage: TokenStorage,
}

pub fn get_tokens_in_current_utxo() -> BoundTokenIter {
    BoundTokenIter { skip: 0 }
}

pub enum AnyToken {}

impl Iterator for BoundTokenIter {
    type Item = (TokenHandle<AnyToken>, TokenStorage);

    fn next(&mut self) -> Option<Self::Item> {
        let mut tokens = [MaybeUninit::<TokenStorageId>::uninit(); 1];

        let r = unsafe {
            starstream_get_tokens(&mut tokens as *mut _ as *mut (), tokens.len() as u32, 0)
        };

        if r == 1 {
            self.skip += 1;

            let token_with_id = unsafe { tokens.into_iter().next().unwrap().assume_init() };
            Some((
                TokenHandle::from_raw(token_with_id.token_handle),
                token_with_id.storage,
            ))
        } else {
            None
        }
    }
}
