#![no_std]

use starstream::utxo_import;

#[link(wasm_import_module = "starstream_utxo:example_toy_vm")]
unsafe extern "C" {
    safe fn starstream_new_VmUtxo_new() -> VmUtxo;
}

utxo_import! {
    "starstream_utxo:example_toy_vm";
    VmUtxo;
    starstream_status_VmUtxo;
    starstream_resume_VmUtxo;
    ();
}

impl VmUtxo {
    #[allow(clippy::new_without_default)]
    #[inline]
    pub fn new() -> Self {
        starstream_new_VmUtxo_new()
    }
}
