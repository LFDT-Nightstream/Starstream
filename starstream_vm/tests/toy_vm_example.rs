use starstream_vm::*;

#[test]
pub fn main() {
    std::process::Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("example_toy_vm")
        .status()
        .unwrap();

    let mut tx = Transaction::new();
    dbg!(&tx);

    let example_contract = tx.code_cache().load_debug("example_toy_vm");

    let utxo = tx.run_coordination_script(&example_contract, "new_utxo", vec![]);
    // dbg!(&tx);

    tx.run_coordination_script(&example_contract, "state_transition", vec![utxo]);
    dbg!(&tx);

    // tx.do_nebula_stuff();
}
