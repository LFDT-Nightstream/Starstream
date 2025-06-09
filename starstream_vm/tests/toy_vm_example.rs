use starstream_vm::*;

pub fn main() {
    env_logger::Builder::new().init();

    let mut tx = Transaction::new();
    dbg!(&tx);

    let example_contract = tx.code_cache().load_debug("example_toy_vm");

    let utxo = tx.run_coordination_script(&example_contract, "new_utxo", vec![]);
    // dbg!(&tx);

    tx.run_coordination_script(&example_contract, "state_transition", vec![utxo]);
    dbg!(&tx);

    // tx.do_nebula_stuff();
}
