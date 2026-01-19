use chumsky::{
    error::Rich,
    extra::{self, SimpleState},
    input,
};
use starstream_types::{Comment, CommentMap};

/// Starstream Parser State
#[derive(Default)]
pub struct State {
    pub comments: Vec<Comment>,
}

impl State {
    pub fn new() -> SimpleState<Self> {
        SimpleState(State::default())
    }

    /// Convert collected comments into a CommentMap.
    pub fn into_comment_map(self) -> CommentMap {
        CommentMap::from_comments(self.comments)
    }
}

pub type Extra<'a> = extra::Full<Rich<'a, char>, SimpleState<State>, ()>;
#[allow(dead_code)]
pub type MapExtra<'a, 'b> = input::MapExtra<'a, 'b, &'a str, Extra<'a>>;
