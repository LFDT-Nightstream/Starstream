use chumsky::{
    error::Rich,
    extra::{self, SimpleState},
    input::{self, WithContext},
};
use starstream_types::{Comment, CommentMap, Span};

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

/// Source text tagged with the module it belongs to, so every span the parser
/// produces records which file it points into.
pub type ParserInput<'a> = WithContext<Span, &'a str>;

pub type Extra<'a> = extra::Full<Rich<'a, char, Span>, SimpleState<State>, ()>;
#[allow(dead_code)]
pub type MapExtra<'a, 'b> = input::MapExtra<'a, 'b, ParserInput<'a>, Extra<'a>>;
