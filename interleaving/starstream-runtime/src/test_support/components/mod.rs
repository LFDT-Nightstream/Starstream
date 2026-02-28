use std::borrow::Cow;

const STARSTREAM_DSL_WIT: &str = r#"
package starstream:dsl@0.1.0;

world runtime {
    import env: interface {
        starstream-trace: func(a0: u64, a1: u64, a2: u64, a3: u64, a4: u64, a5: u64, a6: u64, a7: u64);
        starstream-get-datum: func(slot: u64) -> u64;
        starstream-set-datum: func(slot: u64, val: u64);
        starstream-activation: func() -> tuple<u64, u64>;
        starstream-get-program-hash: func(target: u64) -> tuple<u64, u64, u64, u64>;
        starstream-get-handler-for: func(h0: u64, h1: u64, h2: u64, h3: u64) -> u64;
        starstream-call-effect-handler: func(h0: u64, h1: u64, h2: u64, h3: u64, val: u64);
        starstream-install-handler: func(h0: u64, h1: u64, h2: u64, h3: u64);
        starstream-uninstall-handler: func(h0: u64, h1: u64, h2: u64, h3: u64);
        starstream-new-ref: func(size: u64) -> u64;
        starstream-ref-push: func(a0: u64, a1: u64, a2: u64, a3: u64);
        starstream-ref-get: func(reff: u64, offset: u64) -> tuple<u64, u64, u64, u64>;
        starstream-ref-write: func(reff: u64, offset: u64, a0: u64, a1: u64, a2: u64, a3: u64);
        starstream-resume: func(target: u64, val: u64);
        starstream-yield: func(val: u64);
        starstream-return: func();
        starstream-new-utxo: func(h0: u64, h1: u64, h2: u64, h3: u64, val: u64) -> u64;
        starstream-new-coord: func(h0: u64, h1: u64, h2: u64, h3: u64, val: u64) -> u64;
        starstream-burn: func(ret: u64);
        starstream-bind: func(owner-id: u64);
        starstream-unbind: func(token-id: u64);
        starstream-init: func() -> tuple<u64, u64>;
    }
    export step: func();
}
"#;

fn build_component_type_metadata() -> Result<Vec<u8>, String> {
    let mut resolve = wit_parser::Resolve::default();
    let pkg = resolve
        .push_str("starstream-dsl.wit", STARSTREAM_DSL_WIT)
        .map_err(|e| e.to_string())?;
    let world = *resolve
        .packages
        .get(pkg)
        .and_then(|p| p.worlds.get("runtime"))
        .ok_or_else(|| "failed to resolve `runtime` world".to_string())?;

    wit_component::metadata::encode(&resolve, world, wit_component::StringEncoding::UTF8, None)
        .map_err(|e| e.to_string())
}

fn inject_component_type_metadata(
    core_wasm: &[u8],
    component_type: &[u8],
) -> Result<Vec<u8>, String> {
    let mut module = wasm_encoder::Module::new();
    for payload in wasmparser::Parser::new(0).parse_all(core_wasm) {
        let payload = payload.map_err(|e| e.to_string())?;
        if let Some((id, range)) = payload.as_section() {
            module.section(&wasm_encoder::RawSection {
                id,
                data: &core_wasm[range],
            });
        }
    }
    module.section(&wasm_encoder::CustomSection {
        name: Cow::Borrowed("component-type"),
        data: Cow::Borrowed(component_type),
    });
    Ok(module.finish())
}

pub fn componentize(core_wasm: &[u8]) -> Result<Vec<u8>, String> {
    let component_type = build_component_type_metadata()?;
    let annotated_core = inject_component_type_metadata(core_wasm, &component_type)?;
    wit_component::metadata::decode(&annotated_core).map_err(|e| format!("{e:#}"))?;
    let mut strict = wit_component::ComponentEncoder::default()
        .validate(true)
        .module(&annotated_core)
        .map_err(|e| format!("{e:#}"))?;
    match strict.encode() {
        Ok(component) => Ok(component),
        Err(strict_err) => {
            eprintln!(
                "strict component validation failed, retrying without validation: {strict_err:#}"
            );
            wit_component::ComponentEncoder::default()
                .validate(false)
                .module(&annotated_core)
                .map_err(|e| format!("{e:#}"))?
                .encode()
                .map_err(|e| format!("{e:#}"))
        }
    }
}
