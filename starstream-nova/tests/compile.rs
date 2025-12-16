use starstream_nova::{
    circuits::{WASM_IO, WASM_VM},
    r1cs::gen_r1cs_structure,
    switchboard::SwitchedCircuit,
};
use std::hash::{DefaultHasher, Hash, Hasher};

#[test]
fn compile_to_r1cs() {
    let io_mapping = |idx: WASM_IO| match idx {
        WASM_IO::sp => 0,
        WASM_IO::pc => 1,
        WASM_IO::helper_sp => 2,
        WASM_IO::reg => 3,
        WASM_IO::cc => 4,
    };
    let r1cs = gen_r1cs_structure(
        SwitchedCircuit(std::marker::PhantomData, WASM_VM),
        5,
        io_mapping,
    );
    let mut hasher = DefaultHasher::new();
    r1cs.structure.hash(&mut hasher);
    println!("{} {} {}", r1cs.n_io, r1cs.n_witnesses, r1cs.n_constraints);
    println!("structure hash: {}", hasher.finish());
}
