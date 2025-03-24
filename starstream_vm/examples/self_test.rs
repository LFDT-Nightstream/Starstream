use starstream_vm::*;
use wasmi::Value;

pub fn main() {
    let mut tx = Transaction::new();
    dbg!(&tx);

    let example_contract = tx.code_cache().load_debug("example_contract");

    tx.run_coordination_script(&example_contract, "produce", vec![]);
    dbg!(&tx);

    let a = tx.run_coordination_script(&example_contract, "star_mint", vec![Value::I64(17).into()]);
    let b = tx.run_coordination_script(&example_contract, "star_mint", vec![Value::I64(20).into()]);
    let c = tx.run_coordination_script(&example_contract, "star_combine", vec![a, b]);
    tx.run_coordination_script(
        &example_contract,
        "star_split",
        vec![c, Value::I64(5).into()],
    );
    dbg!(&tx);

    let nft_contract = tx.run_coordination_script(&example_contract, "new_nft", vec![]);
    tx.run_coordination_script(
        &example_contract,
        "star_nft_mint_to",
        vec![nft_contract.clone() /* owner */],
    );
    tx.run_coordination_script(
        &example_contract,
        "star_nft_mint_count",
        vec![nft_contract, /* owner, */ Value::I64(4).into()],
    );
    dbg!(&tx);
}
