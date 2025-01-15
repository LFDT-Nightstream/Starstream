#![no_std]
#![no_main]

// fn foo(_: A, _: B, sleep: fn(Yield) -> (E, F)) -> Yield
// entry point name: "foo"
// init args: A, B
// yield: C, D
// ^ the resume result determines the available effect handlers
// resume: E, F
// terminate result: G, H
// ^ can still call stuff on these maybe

// init args are unconstrained
// resume result should be a single struct w/ methods on it
// yield = terminate

pub struct MyMain {
    supply: u32,
}

impl MyMain {
    //#[starstream::main]
    pub fn new(sleep: fn(MyMain) /* other args can appear here */) {
        let mut supply = 0;
        loop {
            supply += 1;
            sleep(MyMain { supply });
        }
    }

    //#[starstream::export]
    pub fn get_supply(&self) -> u32 {
        self.supply
    }
}

// ----------------------------------------------------------------------------
// Generated

#[no_mangle]
pub extern "C" fn starstream_new_MyMain_new() {
    MyMain::new(starstream::sleep::<(), MyMain>)
}

#[no_mangle]
pub extern "C" fn starstream_effect_MyMain_get_supply(this: &MyMain) -> u32 {
    this.get_supply()
}
