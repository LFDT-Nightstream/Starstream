#![allow(non_snake_case)]

// ----------------------------------------------------------------------------
// Algebraic effects
use core::{marker::PhantomData, mem::MaybeUninit};
#[link(wasm_import_module = "starstream_utxo_env")]
unsafe extern "C" {
    unsafe fn starstream_raise(
        name: *const u8,
        name_len: usize,
        data: *const (),
        data_size: usize,
        resume_arg: *mut (),
        resume_arg_size: usize,
    );
}

fn raise<Yield, Resume>(name: &str, data: &Yield) -> Resume {
    let name = name.as_bytes();

    let mut resume_arg = MaybeUninit::<Resume>::uninit();
    unsafe {
        starstream_raise(
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

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn starstream_register_effect_handler(
        name: *const u8,
        name_len: usize,
        handler_addr: *const (),
    );
}

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn starstream_unregister_effect_handler(name: *const u8, name_len: usize);
}

pub struct DropGuard<'a> {
    name: &'a str,
}

impl Drop for DropGuard<'_> {
    fn drop(&mut self) {
        unsafe {
            starstream_unregister_effect_handler(self.name.as_ptr(), self.name.len());
        }
    }
}

#[must_use]
pub fn register_effect_handler<'a, T>(name: &'a str, handler: &T) -> DropGuard<'a> {
    unsafe {
        // NOTE: just in case the len of the string is not the same as the size
        // in bytes (utf-8)
        //
        // most likely this shouldn't be a string anyway
        //
        let name = name.as_bytes();
        starstream_register_effect_handler(
            name.as_ptr(),
            name.len(),
            handler as *const T as *const (),
        );
    }

    DropGuard { name }
}

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn starstream_get_raised_effect_data(
        name: *const u8,
        name_len: usize,
        output_ptr: *mut (),
        not_null: *mut u8,
    );
}

pub fn get_raised_effect_data<Effect>(name: &str) -> Option<Effect> {
    unsafe {
        // NOTE: just in case the len of the string is not the same as the size
        // in bytes (utf-8)
        //
        // most likely this shouldn't be a string anyway
        let name = name.as_bytes();

        let mut effect = MaybeUninit::<Effect>::uninit();

        let mut not_null = 0u8;

        starstream_get_raised_effect_data(
            name.as_ptr(),
            name.len(),
            effect.as_mut_ptr() as *mut (),
            &mut not_null as *mut u8,
        );

        if not_null == 1 {
            Some(effect.assume_init())
        } else {
            None
        }
    }
}

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn starstream_resume_throwing_program(
        name: *const u8,
        name_len: usize,
        input_ptr: *const (),
    );
}

pub fn resume_throwing_program<Data>(name: &str, data: &Data) {
    unsafe {
        // NOTE: just in case the len of the string is not the same as the size
        // in bytes (utf-8)
        //
        // most likely this shouldn't be a string anyway
        let name = name.as_bytes();

        starstream_resume_throwing_program(
            name.as_ptr(),
            name.len(),
            data as *const Data as *const (),
        );
    }
}

pub trait Effect {
    const NAME: &'static str;
    type Input;
    type Output;

    fn raise(data: &Self::Input) -> Self::Output {
        crate::effects::raise::<Self::Input, Self::Output>(Self::NAME, data)
    }
}

pub trait EffectHandlerSignature {}

pub struct EffectHandler<'a, E: Effect> {
    f: &'a dyn Fn(E::Input) -> E::Output,
    effect: PhantomData<E>,
}

pub trait Registrable {
    type DropGuard<'a>
    where
        Self: 'a;
    fn register(&self) -> Self::DropGuard<'_>;
}

impl<E: Effect> Registrable for EffectHandler<'_, E> {
    type DropGuard<'a>
        = DropGuard<'a>
    where
        Self: 'a;

    fn register(&self) -> Self::DropGuard<'_> {
        register_effect_handler(<E as Effect>::NAME, self)
    }
}

impl<E: Effect> Registrable for &EffectHandler<'_, E> {
    type DropGuard<'a>
        = DropGuard<'a>
    where
        Self: 'a;

    fn register(&self) -> Self::DropGuard<'_> {
        register_effect_handler(<E as Effect>::NAME, self)
    }
}

#[macro_export]
macro_rules! impl_registrable_for_tuple {
    ($($t:ident),+) => {
        impl<$($t),+> Registrable for ($($t),+)
        where
            $($t: Registrable),+
        {
            type DropGuard<'a>
                = ($(<$t as Registrable>::DropGuard<'a>),+)
            where
                $($t: 'a),+;

            fn register(&self) -> Self::DropGuard<'_> {
                let ($($t),+) = self;
                ($($t.register()),+)
            }
        }
    };
}

impl_registrable_for_tuple!(T, U);
impl_registrable_for_tuple!(T, U, V);
impl_registrable_for_tuple!(T, U, V, W);
impl_registrable_for_tuple!(T, U, V, W, X);
impl_registrable_for_tuple!(T, U, V, W, X, Y);
impl_registrable_for_tuple!(T, U, V, W, X, Y, Z);

pub fn run_effectful_computation<F, T>(handler: impl Registrable, f: F) -> T
where
    F: FnOnce() -> T,
{
    let _guard = handler.register();

    f()
}

impl<E: Effect> EffectHandler<'_, E> {
    pub fn handle(&self) {
        // NOTE: this could be part of the effect system instead of doing
        // with 2 interrupts however, this way it's a bit simpler to handle
        // passing arbitrary values, since get_raised_effect_data takes care of
        // allocating memory on the stack, for the response.
        //
        // Similarly for the resume.
        let effect_data = get_raised_effect_data::<E::Input>(E::NAME).unwrap();
        let res = (self.f)(effect_data);
        resume_throwing_program::<E::Output>(E::NAME, &res);
    }
}

impl<'a, E: Effect> EffectHandler<'a, E> {
    pub fn with<F>(f: &'a F) -> Self
    where
        F: Fn(E::Input) -> E::Output,
    {
        Self {
            f,
            effect: PhantomData,
        }
    }
}
