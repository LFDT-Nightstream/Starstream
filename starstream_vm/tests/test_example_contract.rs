use starstream_vm::*;
use wasmi::Value;

#[test]
pub fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    std::process::Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("example_contract")
        .status()
        .unwrap();

    let mut tx = Transaction::new();

    let example_contract = tx.code_cache().load_debug("example_contract");

    tx.run_coordination_script(&example_contract, "produce_effect", vec![]);
    dbg!(&tx);

    let a = tx.run_coordination_script(&example_contract, "star_mint", vec![Value::I64(17)]);
    let b = tx.run_coordination_script(&example_contract, "star_mint", vec![Value::I64(20)]);
    let c = tx.run_coordination_script(&example_contract, "star_combine", vec![a, b]);
    tx.run_coordination_script(&example_contract, "star_split", vec![c, Value::I64(5)]);
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
        vec![nft_contract, /* owner, */ Value::I64(4)],
    );
    dbg!(&tx);
}
