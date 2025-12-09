use chumsky::{
    error::Rich,
    extra::{self, SimpleState},
    input,
};
use starstream_types::Span;

/// Starsteam Parser State
#[derive(Default)]
pub struct State {
    pub comments: Vec<Span>,
}

impl State {
    pub fn new() -> SimpleState<Self> {
        SimpleState(State::default())
    }
}

pub type Extra<'a> = extra::Full<Rich<'a, char>, SimpleState<State>, ()>;
#[allow(dead_code)]
pub type MapExtra<'a, 'b> = input::MapExtra<'a, 'b, &'a str, Extra<'a>>;
