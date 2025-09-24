use starstream_nova::{circuits::WASM_VM, r1cs::gen_r1cs_structure};
use std::hash::{DefaultHasher, Hash, Hasher};

#[test]
fn compile_to_r1cs() {
    let r1cs = gen_r1cs_structure(WASM_VM, 4);
    let mut hasher = DefaultHasher::new();
    r1cs.structure.hash(&mut hasher);
    println!("{} {} {}", r1cs.n_io, r1cs.n_witnesses, r1cs.n_constraints);
    println!("structure hash: {}", hasher.finish());
}
