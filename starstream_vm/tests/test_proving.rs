use starstream_vm::*;

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

    tx.run_coordination_script(&example_contract, "produce_and_consume", vec![]);
    dbg!(&tx);
    dbg!(tx.map_continuations());

    // TODO: how do we auto-test this without eating infinite GitHub Actions runner time?
    //tx.prove();
}
