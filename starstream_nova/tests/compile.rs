use starstream_nova::{
    circuits::{WASM_IO, WASM_VM},
    r1cs::gen_r1cs_structure,
};
use std::{
    hash::{DefaultHasher, Hash, Hasher},
};

#[test]
fn compile_to_r1cs() {
    let io_mapping = |idx: WASM_IO| match idx {
        WASM_IO::sp_namespace => 0,
        WASM_IO::sp => 1,
        WASM_IO::pc_namespace => 2,
        WASM_IO::pc => 3,
    };
    let r1cs = gen_r1cs_structure(WASM_VM, 4, io_mapping);
    let mut hasher = DefaultHasher::new();
    r1cs.structure.hash(&mut hasher);
    println!("{} {} {}", r1cs.n_io, r1cs.n_witnesses, r1cs.n_constraints);
    println!("structure hash: {}", hasher.finish());
}
