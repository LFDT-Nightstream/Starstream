use std::fmt::Display;

use combine::{Positioned, StreamOnce, stream::ResetStream};

#[derive(Clone, Debug, PartialEq)]
pub struct DisplayAsHex<'a> {
    pub slice: &'a [u8],
}

impl<'a> Display for DisplayAsHex<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02x?}", self.slice)
    }
}

impl<'a> StreamOnce for DisplayAsHex<'a> {
    type Token = <&'a [u8] as StreamOnce>::Token;
    type Range = DisplayAsHex<'a>;
    type Position = <&'a [u8] as StreamOnce>::Position;
    type Error = <&'a [u8] as StreamOnce>::Error;
    fn uncons(&mut self) -> Result<Self::Token, combine::stream::StreamErrorFor<Self>> {
        self.slice.uncons()
    }
    fn is_partial(&self) -> bool {
        self.slice.is_partial()
    }
}

impl<'a> ResetStream for DisplayAsHex<'a> {
    type Checkpoint = <&'a [u8] as ResetStream>::Checkpoint;

    fn checkpoint(&self) -> Self::Checkpoint {
        self.slice.checkpoint()
    }

    fn reset(&mut self, checkpoint: Self::Checkpoint) -> Result<(), Self::Error> {
        self.slice.reset(checkpoint)
    }
}

impl<'a> Positioned for DisplayAsHex<'a> {
    fn position(&self) -> Self::Position {
        self.slice.position()
    }
}
