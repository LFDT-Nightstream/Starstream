use starstream_vm::*;

#[test]
pub fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    let mut tx = Transaction::new();

    let example_contract = tx.code_cache().load_debug("wat:status");

    tx.run_coordination_script(&example_contract, "coord", vec![]);
    dbg!(&tx);
}
