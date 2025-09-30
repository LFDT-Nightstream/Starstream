use std::fmt::Display;

use combine::{Positioned, StreamOnce, stream::ResetStream};
use starstream_nova::{
    circuits::{WASM_IO, WASM_VM, WITNESS_OFFSET_OPCODE_LOOKUP},
    exec,
    test::{Handler, Locations, test_circuit_goldilocks},
    wasm_parser,
};

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

struct H<'a>(&'a mut bool, &'a [i32]);

impl<'a> Handler for H<'a> {
    fn failed_enforce(
        &mut self,
        locations: Locations<'_>,
        lhs: i128,
        rhs: i128,
        actual: i128,
        expected: i128,
    ) {
        eprintln!("failed_enforce {locations} {lhs} {rhs} {actual} {expected}");
        *self.0 = true;
    }
    fn lookup(&mut self, locations: Locations<'_>, namespace: i128, address: i128, val: i128) {
        if namespace != 1 || self.1[address as usize] as i128 != val {
            eprintln!(
                "lookup {locations} {namespace} {address} {val} {}",
                self.1[address as usize] as i128
            );
            *self.0 = true;
        }
    }
    fn invalid_memory(
        &mut self,
        locations: Locations<'_>,
        namespace: i128,
        address: i128,
        expected: i128,
        actual: i128,
        new: i128,
    ) {
        eprintln!("invalid_memory {locations} {namespace} {address} {expected} {actual} {new}");
        *self.0 = true;
    }
    fn mismatching_witness(
        &mut self,
        locations: Locations<'_>,
        provided_tag: u64,
        expected_tag: u64,
    ) {
        eprintln!("mismatching_witness {locations} {provided_tag:0X} {expected_tag:0X}");
        *self.0 = true;
    }
    fn missing_witness(&mut self, locations: Locations<'_>, expected_tag: u64) {
        eprintln!("missing_witness {locations} {expected_tag:0X}");
        *self.0 = true;
    }
}

#[test]
fn parse_wasm() {
    let example_module = r#"
        (module
          (func $f (result i32)
            i32.const 1
            i32.const 2
            i32.const 3
            i32.const 4
            call $g
          )
          (func $g (param i32 i32 i32 i32) (result i32)
            local.get 0
            local.get 1
            i32.add
            local.get 2
            i32.add
            local.get 3
            i32.add
          )
        )
    "#;
    let binary: Vec<u8> = wat::parse_str(example_module).unwrap();
    println!("{}", DisplayAsHex(binary.as_slice()));
    let code = wasm_parser::parse(DisplayAsHex(binary.as_slice()));
    println!("{code:?}");
    let mut state = exec::State {
        memory: Vec::new(),
        stack: vec![0],
        pc: 1,
    };
    let opcode = code[1];
    let param = code[2];

    let exec::Witnesses { v, offset } = exec::step(code.as_slice(), &mut state);
    let input = |idx: WASM_IO| match idx {
        WASM_IO::sp_namespace => (0, 1),
        WASM_IO::sp => (1, 1),
        WASM_IO::pc_namespace => (1, 1),
        WASM_IO::pc => (1, 1),
    };
    let sp = state.stack.len() as i128;
    let pc = state.pc as i128;
    let output = |idx: WASM_IO| match idx {
        WASM_IO::sp_namespace => (0, 1),
        WASM_IO::sp => (sp, 1),
        WASM_IO::pc_namespace => (1, 1),
        WASM_IO::pc => (pc, 1),
    };
    let opcode_tag = 0xAEBEAEB02806E525;
    let param_tag = 0x9552D766C092875D;
    let real_opcode_idx_tag = 0x64B3A4ACE18BE178;
    let real_param_idx_tag = 0xEC10C83AB9C3E178;
    let v_len = v.len();
    let witness = [
        (opcode_tag, (opcode as i128, 1)),
        (param_tag, (param as i128, 1)),
    ]
    .into_iter()
    .chain((2..offset).map(|_| (0, (0, 1))))
    .chain(
        v.into_iter()
            .map(|(tag, n, d)| (tag, (n as i128, d as i128))),
    )
    .chain(((offset + v_len)..WITNESS_OFFSET_OPCODE_LOOKUP).map(|_| (0, (0, 1))))
    .chain([(real_opcode_idx_tag, (1, 1)), (real_param_idx_tag, (2, 1))]);
    // we start with an empty stack and an empty memory
    let memories = vec![vec![(0, 1)], Vec::new()];
    let mut failed = false;
    test_circuit_goldilocks(
        input,
        output,
        WASM_VM,
        witness,
        H(&mut failed, &code),
        memories,
    );
    if failed {
        panic!("failed")
    }
}
