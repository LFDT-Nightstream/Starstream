use starstream_vm::*;

pub fn main() {
    let mut tx = Transaction::new();

    let example_contract = tx.code_cache().load_debug("impact_vm_example");

    let utxo = tx.run_coordination_script(&example_contract, "new_counter", vec![]);

    let counter = tx.run_coordination_script(&example_contract, "get_counter", vec![utxo.clone()]);
    assert_eq!(counter.i32().expect("invalid counter type") as u32, 1);

    tx.run_coordination_script(&example_contract, "increase_counter", vec![utxo.clone()]);

    let counter = tx.run_coordination_script(&example_contract, "get_counter", vec![utxo.clone()]);
    assert_eq!(counter.i32().expect("invalid counter type") as u32, 2);

    tx.run_coordination_script(&example_contract, "increase_counter", vec![utxo.clone()]);
    let counter = tx.run_coordination_script(&example_contract, "get_counter", vec![utxo.clone()]);
    assert_eq!(counter.i32().expect("invalid counter type") as u32, 3);
}
