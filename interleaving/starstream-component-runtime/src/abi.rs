use starstream_interleaving_spec::{Hash, InterfaceId, Ref, Value, WasmModule};

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
        function_id: u64,
    },
    Yield {
        payload: Ref,
    },
    Burn,
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
        function_id: u64,
    },
}
