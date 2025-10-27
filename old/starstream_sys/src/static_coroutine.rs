use core::{ops::Coroutine, pin::Pin};

pub fn __unreachable<T>() -> T {
    unreachable!()
}

pub const fn __layout_of_result<R, F: FnOnce() -> R>(_: &F) -> (usize, usize) {
    (core::mem::size_of::<R>(), core::mem::align_of::<R>())
}

pub unsafe fn __resume_by_result<R, C: Coroutine<R> + Unpin, F: FnOnce() -> C>(
    _: &F,
    ptr: *mut (),
    arg: R,
) {
    let ptr = ptr as *mut C;
    unsafe {
        Pin::new(&mut *ptr).resume(arg);
    }
}

#[macro_export]
macro_rules! static_coroutine {
    (
        #[static_coroutine(init = $init:ident, resume = $resume:ident)]
        fn $coro:ident( $($start_arg:ident : $start_arg_ty:ty),* $(,)? ) -> impl Coroutine $(<$resume_arg_ty:ty>)? $body:block
    ) => {
        fn $coro( $($start_arg : $start_arg_ty),* ) -> impl Coroutine $(<$resume_arg_ty>)? $body

        mod $coro {
            const LAYOUT: (usize, usize) = $crate::static_coroutine::__layout_of_result(&|| super::$coro( $($crate::static_coroutine::__unreachable::<$start_arg_ty>()),* ));
            type Aligner = u64;
            const _: () = assert!(LAYOUT.1 <= core::mem::align_of::<Aligner>());
            const NUM_ALIGNERS: usize = LAYOUT.0.div_ceil(core::mem::size_of::<Aligner>());
            static mut HOLDER_: [Aligner; NUM_ALIGNERS] = [0; NUM_ALIGNERS];
            pub const HOLDER: *mut () = &raw mut HOLDER_ as *mut ();
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn $init( $($start_arg: $start_arg_ty),* ) {
            unsafe {
                core::ptr::write($coro::HOLDER as *mut _, $coro( $($start_arg),* ));
            }
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn $resume( $(resume_arg: $resume_arg_ty),* ) {
            unsafe {
                $crate::static_coroutine::__resume_by_result(
                    &|| $coro( $($crate::static_coroutine::__unreachable::<$start_arg_ty>()),* ),
                    $coro::HOLDER,
                    ( $(resume_arg as $resume_arg_ty)? )
                )
            }
        }
    };
}
