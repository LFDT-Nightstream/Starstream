use starstream_nova::{
    circuits::{self, MemoryTable, WASM_IO, WASM_VM},
    display_as_hex::DisplayAsHex,
    exec,
    interface::BranchedCircuit,
    switchboard::{SwitchedCircuit, calculate_offsets},
    test::{Handler, Locations, Memory, MemoryMapping, init_memory, test_circuit_goldilocks},
    wasm_parser,
};

struct H<'a>(&'a mut bool, &'a [i32]);

impl<'a> Handler<circuits::LookupTable, circuits::MemoryTable> for H<'a> {
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
        namespace: circuits::LookupTable,
        address: i128,
        val: i128,
    ) {
        if address != 0 || val != 0 {
            match namespace {
                circuits::LookupTable::Code if self.1.len() <= address as usize => {
                    if val != 0 {
                        eprintln!("code invalid {locations} {address} {val} 0",);
                        *self.0 = true;
                    }
                }
                circuits::LookupTable::Code if self.1[address as usize] as i128 != val => {
                    eprintln!(
                        "code invalid {locations} {address} {val} {}",
                        self.1[address as usize] as i128
                    );
                    *self.0 = true;
                }
                circuits::LookupTable::HostInteractions => {
                    eprintln!("host {locations} {address} {val}");
                }
                _ => {}
            }
        }
    }
    fn invalid_memory(
        &mut self,
        locations: Locations<'_>,
        namespace: MemoryTable,
        address: i128,
        expected: i128,
        actual: i128,
        new: i128,
    ) {
        let name = match namespace {
            MemoryTable::Stack => "stack",
            MemoryTable::HelperStack => "helper stack",
            MemoryTable::Memory => "memory",
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
    let mut memory_mapping = {
        let stack = init_memory(state.stack.iter().map(|v| (*v as i128, 1)));
        let helper_stack = init_memory(state.helper_stack.iter().map(|v| (*v as i128, 1)));
        let linear_memory = init_memory(None.into_iter());
        struct Mapper {
            stack: Memory,
            helper_stack: Memory,
            linear_memory: Memory,
        }
        impl MemoryMapping<MemoryTable> for Mapper {
            fn get<'a>(&'a mut self, m: &MemoryTable) -> &'a mut Memory {
                match m {
                    MemoryTable::Stack => &mut self.stack,
                    MemoryTable::HelperStack => &mut self.helper_stack,
                    MemoryTable::Memory => &mut self.linear_memory,
                }
            }
        }
        Mapper {
            stack,
            helper_stack,
            linear_memory,
        }
    };

    let offsets: Vec<_> = WASM_VM
        .branches()
        .zip(calculate_offsets(&WASM_VM))
        .collect();

    loop {
        let sp = state.stack.len() as i128;
        let pc = state.pc as i128;
        let helper_sp = state.helper_stack.len() as i128;
        let reg = state.reg as i128;
        let cc = state.cc as i128;
        eprintln!("stack: {:?}", state.stack);
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
        let (_, offset) = offsets
            .iter()
            .find(|(branch_, _)| branch_ == &branch)
            .expect("impossible");
        let witness = (0..*offset).map(|_| (0, (0, 1))).chain(
            v.into_iter()
                .map(|(tag, n, d)| (tag, (n as i128, d as i128))),
        );
        let mut failed = false;
        test_circuit_goldilocks(
            input,
            output,
            SwitchedCircuit(std::marker::PhantomData, WASM_VM),
            witness,
            H(&mut failed, &code),
            &mut memory_mapping,
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
