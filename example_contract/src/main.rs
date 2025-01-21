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

//#[starstream::event]
//fn my_event(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_event_my_event"]
    safe fn my_event(supply: u32);
}

//#[starstream::effect]
//fn my_effect(supply: u32);
unsafe extern "C" {
    #[link_name = "starstream_effect_my_effect"]
    safe fn my_effect(supply: u32);
}

//#[starstream::error]
//fn my_error(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_error_my_error"]
    safe fn my_error(supply: u32);
}

pub struct MyMain {
    supply: u32,
}

impl MyMain {
    //#[starstream::new]
    pub fn new(sleep: fn(&MyMain)) {
        let mut supply = 0;
        loop {
            supply += 1;
            my_event(supply);
            //my_effect(supply);
            //my_error(supply);
            sleep(&MyMain { supply });
        }
    }

    //#[starstream::query]
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
pub extern "C" fn starstream_query_MyMain_get_supply(this: &MyMain) -> u32 {
    this.get_supply()
}
