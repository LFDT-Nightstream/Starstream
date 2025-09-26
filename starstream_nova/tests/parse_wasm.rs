use std::fmt::Display;

use combine::{Positioned, StreamOnce, stream::ResetStream};
use starstream_nova::wasm_parser;

#[derive(Clone, Debug, PartialEq)]
struct DisplayAsHex<'a>(&'a [u8]);

impl<'a> Display for DisplayAsHex<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02x?}", self.0)
    }
}

impl<'a> StreamOnce for DisplayAsHex<'a> {
    type Token = <&'a [u8] as StreamOnce>::Token;
    type Range = DisplayAsHex<'a>;
    type Position = <&'a [u8] as StreamOnce>::Position;
    type Error = <&'a [u8] as StreamOnce>::Error;
    fn uncons(&mut self) -> Result<Self::Token, combine::stream::StreamErrorFor<Self>> {
        self.0.uncons()
    }
    fn is_partial(&self) -> bool {
        self.0.is_partial()
    }
}

impl<'a> ResetStream for DisplayAsHex<'a> {
    type Checkpoint = <&'a [u8] as ResetStream>::Checkpoint;

    fn checkpoint(&self) -> Self::Checkpoint {
        self.0.checkpoint()
    }

    fn reset(&mut self, checkpoint: Self::Checkpoint) -> Result<(), Self::Error> {
        self.0.reset(checkpoint)
    }
}

impl<'a> Positioned for DisplayAsHex<'a> {
    fn position(&self) -> Self::Position {
        self.0.position()
    }
}

#[test]
#[ignore]
fn parse_wasm() {
    let example_module = r#"
        (module
          (func $f (param i32 i32 i32 i32) (result i32)
            local.get 0
            local.get 1
            i32.add
            local.get 2
            i32.add
            local.get 3
            i32.add
          )
          (func $g (result i32)
            i32.const 1
            i32.const 2
            i32.const 3
            i32.const 4
            call $f
          )
        )
    "#;
    let binary: Vec<u8> = wat::parse_str(example_module).unwrap();
    println!("{}", DisplayAsHex(binary.as_slice()));
    let out = wasm_parser::parse(DisplayAsHex(binary.as_slice()));
    println!("{out:?}");
}
