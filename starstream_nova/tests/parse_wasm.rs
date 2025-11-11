use std::fmt::Display;

use combine::{Positioned, StreamOnce, stream::ResetStream};
use starstream_nova::{
    circuits::{self, WASM_IO, WASM_VM},
    exec,
    switchboard::SwitchedCircuit,
    test::{Handler, Locations, init_memories, test_circuit_goldilocks},
    wasm_parser,
};

#[derive(Clone, Debug, PartialEq)]
struct DisplayAsHex<'a> {
    slice: &'a [u8],
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

struct H<'a>(&'a mut bool, &'a [i32]);

impl<'a> Handler<circuits::LookupTables, circuits::Memories> for H<'a> {
    fn failed_enforce(
        &mut self,
        locations: Locations<'_>,
        lhs: i128,
        rhs: i128,
        actual: i128,
        expected: i128,
    ) {
        eprintln!("failed_enforce {locations}: {lhs} * {rhs} = {actual} != {expected}");
        *self.0 = true;
    }
    fn lookup(
        &mut self,
        locations: Locations<'_>,
        namespace: circuits::LookupTables,
        address: i128,
        val: i128,
    ) {
        if address != 0 || val != 0 {
            match namespace {
                circuits::LookupTables::Code if self.1.len() <= address as usize => {
                    if val != 0 {
                        eprintln!("code invalid {locations} {address} {val} 0",);
                        *self.0 = true;
                    }
                }
                circuits::LookupTables::Code if self.1[address as usize] as i128 != val => {
                    eprintln!(
                        "code invalid {locations} {address} {val} {}",
                        self.1[address as usize] as i128
                    );
                    *self.0 = true;
                }
                circuits::LookupTables::HostInteractions => {
                    eprintln!("host {locations} {address} {val}");
                }
                _ => {}
            }
        }
    }
    fn invalid_memory(
        &mut self,
        locations: Locations<'_>,
        namespace: circuits::Memories,
        address: i128,
        expected: i128,
        actual: i128,
        new: i128,
    ) {
        let name = match namespace {
            circuits::Memories::Stack => "stack",
            circuits::Memories::HelperStack => "helper stack",
            circuits::Memories::Memory => "memory",
        };
        eprintln!(
            "invalid_memory {name} {locations} {address}: wanted {expected}, found {actual}, new {new}"
        );
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
const OPCODE_TAG: u64 = 0xAEBEAEB02806E525;
const PARAM_TAG: u64 = 0x9552D766C092875D;
const REAL_OPCODE_IDX_TAG: u64 = 0xDE9FA51A3F8BE178;
const REAL_PARAM_IDX_TAG: u64 = 0x65FCC8A817C3E178;

const EXAMPLE_UTXO: &'static str = r#"
    (module
      (import "" "yield" (func $yield (param i32) (result i32 i32)))
      (import "" "witness" (func $witness (result i32)))
      (import "" "own_hash" (global i32))
      (memory 1)
      (func $main (param i32)
        local.get 0
        call $yield
        drop
        drop
      )
      (export "main" (func $main))
      (export "memory" (memory 0))
    )
"#;

const EXAMPLE_COORD: &'static str = r#"
    (module
      (import "" "enter" (func $enter (param i32 i32) (result i32)))
      (import "" "coord" (func $coord (param i32 i32) (result i32)))
      (import "" "witness" (func $witness (result i32)))
      (import "" "own_hash" (global i32))
      (memory 1)
      (func $main
        i32.const 0 ;; hash of example_utxo, NB: make sure it matches
        i32.const 1
        i32.const 2
        i32.const 3
        i32.const 4
        call $f
        call $enter
        drop
      )
      (func $f (param i32 i32 i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.add
        local.get 2
        i32.add
        local.get 3
        i32.add
      )
      (export "main" (func $main))
      (export "memory" (memory 0))
    )
"#;

fn parse(name: &str, wat: &str) -> Vec<i32> {
    let binary: Vec<u8> = wat::parse_str(wat).unwrap();
    {
        println!("printing WASM of {name}");
        let len = binary.len();
        for i in 0..len {
            let b = binary[i];
            print!("{b:02X} ");
            if i % 16 == 15 {
                println!();
            }
        }
        if len % 16 != 0 {
            println!();
        }
        println!("printing WASM done");
    }
    // for now hash is always zero
    let globals = [0];
    let code = wasm_parser::parse(
        DisplayAsHex {
            slice: binary.as_slice(),
        },
        &globals,
        |p| format!("0x{:X}", p.translate_position(binary.as_slice())),
    );
    {
        println!("printing IR of {name}");
        let len = code.len();
        for i in 0..len {
            let b = code[i];
            print!("{b:02X} ");
            if i % 16 == 15 {
                println!();
            }
        }
        if len % 16 != 0 {
            println!();
        }
        println!("printing IR done");
    }
    code
}

fn run(
    name: &'static str,
    code: &'static str,
    mut host_calls: impl FnMut(i32, Vec<i32>) -> Vec<i32>,
) {
    let code = parse(name, code);
    // we start with an empty stack and an empty memory
    let mut state = exec::State {
        memory: Vec::new(),
        // one element to allow conditional memory,
        // one for the final return pointer, which
        // will point to a 0 element, to allow for conditional
        // code lookups, and will coincidentally also mean "Unreachable",
        // which can be understood as a permanent stopping point anyway.
        stack: vec![0, 0],
        // also zero element for conditional memory here
        helper_stack: vec![0],
        pc: 1,
        cc: 1,
        reg: 0,
        host_call: 0,
        host_args: Vec::new(),
        getting_from_host: false,
    };
    let mut memories = init_memories(vec![
        state.stack.iter().map(|v| (*v as i128, 1)).collect(),
        state.helper_stack.iter().map(|v| (*v as i128, 1)).collect(),
        Vec::new(),
    ]);

    loop {
        let sp = state.stack.len() as i128;
        let pc = state.pc as i128;
        let helper_sp = state.helper_stack.len() as i128;
        let reg = state.reg as i128;
        let cc = state.cc as i128;
        eprintln!("stack: {:?}", state.stack);
        let opcode = code[pc as usize];
        let param = *code.get(pc as usize + 1).unwrap_or(&0);
        let exec::Witnesses { v, branch } =
            exec::step(code.as_slice(), &mut state, &mut host_calls);
        let input = |idx: WASM_IO| match idx {
            WASM_IO::sp => (sp, 1),
            WASM_IO::pc => (pc, 1),
            WASM_IO::helper_sp => (helper_sp, 1),
            WASM_IO::reg => (reg, 1),
            WASM_IO::cc => (cc, 1),
        };
        let new_sp = state.stack.len() as i128;
        let new_pc = state.pc as i128;
        let new_helper_sp = state.helper_stack.len() as i128;
        let new_reg = state.reg as i128;
        let new_cc = state.cc as i128;
        let output = |idx: WASM_IO| match idx {
            WASM_IO::sp => (new_sp, 1),
            WASM_IO::pc => (new_pc, 1),
            WASM_IO::helper_sp => (new_helper_sp, 1),
            WASM_IO::reg => (new_reg, 1),
            WASM_IO::cc => (new_cc, 1),
        };
        let v_len = v.len();
        let offset = unimplemented!();
        let witness = [
            (OPCODE_TAG, (opcode as i128, 1)),
            (PARAM_TAG, (param as i128, 1)),
        ]
        .into_iter()
        .chain((2..offset).map(|_| (0, (0, 1))))
        .chain(
            v.into_iter()
                .map(|(tag, n, d)| (tag, (n as i128, d as i128))),
        );
        let mut failed = false;
        let memory_mapping = |namespace| match namespace {
            circuits::Memories::Stack => (circuits::Memories::Stack, 0),
            circuits::Memories::HelperStack => (circuits::Memories::HelperStack, 1),
            circuits::Memories::Memory => (circuits::Memories::Memory, 2),
        };
        test_circuit_goldilocks(
            input,
            output,
            SwitchedCircuit(std::marker::PhantomData, WASM_VM),
            witness,
            H(&mut failed, &code),
            &mut memories,
            memory_mapping,
        );
        if failed {
            panic!("failed")
        }
        if new_pc == 0 {
            eprintln!("top-level return probably");
            break;
        }
    }
}

#[test]
#[ignore]
fn coord() {
    let host_calls = |idx: i32, mut args: Vec<i32>| -> Vec<i32> {
        match idx {
            // enter
            0 => {
                args.pop().expect("no utxo hash");
                args.pop().expect("no arg to utxo");
                vec![0] // FIXME: dummy
            }
            // coord
            1 => {
                args.pop().expect("no coord hash");
                args.pop().expect("no arg to coord");
                vec![0] // FIXME: dummy
            }
            // witness
            2 => {
                vec![0] // FIXME: dummy
            }
            _ => unimplemented!(),
        }
    };
    run("example_coord", EXAMPLE_COORD, host_calls);
}

#[test]
#[ignore]
fn utxo() {
    let host_calls = |idx: i32, mut args: Vec<i32>| -> Vec<i32> {
        match idx {
            // yield
            0 => {
                args.pop().expect("nothing to yield");
                vec![0, 0] // FIXME: dummy
            }
            // witness
            1 => {
                vec![0] // FIXME: dummy
            }
            _ => unimplemented!(),
        }
    };
    run("example_utxo", EXAMPLE_UTXO, host_calls);
}
