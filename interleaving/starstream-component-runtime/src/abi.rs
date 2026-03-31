use starstream_interleaving_spec::{Hash, InterfaceId, Ref, Value, WasmModule};

pub const FUNCTION_ID_MAIN: u32 = 0;
#[allow(dead_code)]
pub const FUNCTION_ID_AMOUNT: u32 = 2;

pub const TEMPORARY_STARSTREAM_WIT: &str = r#"
package starstream:executor@0.1.0;

world runtime {
    import env: interface {
        starstream-new-ref: func(size-words: u32) -> u64;
        starstream-ref-push: func(a0: u64, a1: u64, a2: u64, a3: u64);
        starstream-ref-get: func(reff: u64, offset: u32) -> tuple<u64, u64, u64, u64>;
        starstream-ref-write: func(reff: u64, offset: u32, a0: u64, a1: u64, a2: u64, a3: u64);
        starstream-resume: func(target: borrow<utxo>, payload: u64);
        starstream-yield: func(payload: u64);
        starstream-burn: func();
        starstream-return: func();
        starstream-new-utxo: func(h0: u64, h1: u64, h2: u64, h3: u64, init: u64) -> borrow<utxo>;
        starstream-new-coord: func(h0: u64, h1: u64, h2: u64, h3: u64, init: u64) -> borrow<coord>;
        starstream-install-handler: func(h0: u64, h1: u64, h2: u64, h3: u64);
        starstream-uninstall-handler: func(h0: u64, h1: u64, h2: u64, h3: u64);
        starstream-call-effect-handler: func(h0: u64, h1: u64, h2: u64, h3: u64, payload: u64);
    }

    export main: func();
}
"#;

pub const TEMPORARY_STARSTREAM_SCALAR_COMPONENT_WIT: &str = r#"
package starstream:executor@0.1.0;

world runtime {
    import ledger: interface {
        resource utxo;
        starstream-new-ref: func(size-words: u32) -> u64;
        starstream-ref-push: func(a0: u64, a1: u64, a2: u64, a3: u64);
        starstream-ref-get: func(reff: u64, offset: u32) -> tuple<u64, u64, u64, u64>;
        starstream-ref-write: func(reff: u64, offset: u32, a0: u64, a1: u64, a2: u64, a3: u64);
        input-utxo: func(index: u32) -> utxo;
        create-utxo: func(h0: u64, h1: u64, h2: u64, h3: u64, init: u64) -> utxo;
        resume: func(target: borrow<utxo>, payload: u64);
        yield: func(payload: u64);
        burn: func();
        return: func();
    }

    export main: func();
}
"#;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostImportCall<Resource> {
    NewRef {
        size_words: u32,
    },
    RefPush {
        lanes: [Value; 4],
    },
    RefGet {
        reff: Ref,
        offset_words: u32,
    },
    RefWrite {
        reff: Ref,
        offset_words: u32,
        lanes: [Value; 4],
    },
    Resume {
        target: Resource,
        payload: Ref,
        function_id: u32,
    },
    Yield {
        payload: Ref,
    },
    Burn {
        payload: Ref,
    },
    Return,
    NewUtxo {
        program_hash: Hash<WasmModule>,
        init: Ref,
    },
    NewCoord {
        program_hash: Hash<WasmModule>,
        init: Ref,
    },
    InstallHandler {
        interface_id: InterfaceId,
    },
    UninstallHandler {
        interface_id: InterfaceId,
    },
    CallEffectHandler {
        interface_id: InterfaceId,
        payload: Ref,
        function_id: u32,
    },
}

#[derive(Clone, Copy, Debug, Default)]
pub struct TemporaryAbi;

impl TemporaryAbi {
    pub fn wit_source(self) -> &'static str {
        TEMPORARY_STARSTREAM_WIT
    }

    pub fn scalar_component_wit_source(self) -> &'static str {
        TEMPORARY_STARSTREAM_SCALAR_COMPONENT_WIT
    }
}
