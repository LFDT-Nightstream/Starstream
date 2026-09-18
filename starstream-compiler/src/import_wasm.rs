use crate::typecheck::env::Namespace;

pub fn import_wasm(wasm: &[u8]) -> miette::Result<Namespace> {
    let mut namespace = Namespace::default();
    // TODO
    Ok(namespace)
}
