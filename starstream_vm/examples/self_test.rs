use wasmi::Value;
use starstream_vm::*;

pub fn main() {
    let mut tx = Transaction::isolated();
    dbg!(&tx);

    let example_contract = tx.code_cache.load_debug("example_contract");

    tx.run_coordination_script(&example_contract, "produce", &[]);
    dbg!(&tx);

    let a = tx.run_coordination_script(&example_contract, "star_mint", &[Value::I64(17).into()]);
    let b = tx.run_coordination_script(&example_contract, "star_mint", &[Value::I64(20).into()]);
    let c = tx.run_coordination_script(&example_contract, "star_combine", &[a, b]);
    tx.run_coordination_script(&example_contract, "star_split", &[c, Value::I64(5).into()]);
    dbg!(&tx);

    let nft_contract = tx.run_coordination_script(&example_contract, "new_nft", &[]);
    tx.run_coordination_script(
        &example_contract,
        "star_nft_mint_to",
        &[nft_contract.clone() /* owner */],
    );
    tx.run_coordination_script(
        &example_contract,
        "star_nft_mint_count",
        &[nft_contract, /* owner, */ Value::I64(4).into()],
    );
    dbg!(&tx);
}
