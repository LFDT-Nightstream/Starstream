use starstream::utxo_import;

#[link(name = "impact_vm", kind = "static")]
unsafe extern "C" {
    pub fn run_program(
        program: *const u8,
        size: usize,
        state_in: *const (),
        out_ptr: *mut *const (),
    ) -> u32;
    pub fn deserialize_state(
        buffer: *const u8,
        buffer_len: usize,
        state_out_ptr: *mut *const (),
    ) -> u32;
    pub fn serialize_state(
        state_in_ptr: *const (),
        buffer: *mut *const u8,
        buffer_len: *mut usize,
    ) -> u32;
    pub fn free_state(state_ptr: *const ());
    pub fn free_buffer(buffer: *const u8, len: usize);
}

#[link(wasm_import_module = "starstream_utxo:impact_vm_example")]
unsafe extern "C" {
    safe fn starstream_new_ImpactVM_new() -> ImpactVM;
    safe fn starstream_mutate_ImpactVM_increment(utxo: ImpactVM);
    safe fn starstream_query_ImpactVM_get_counter(utxo: ImpactVM) -> u32;
}

utxo_import! {
    "starstream_utxo:impact_vm_example";
    ImpactVM;
    starstream_status_ImpactVM;
    starstream_resume_ImpactVM;
    ();
}

impl ImpactVM {
    #[inline]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        starstream_new_ImpactVM_new()
    }

    #[inline]
    pub fn increment(self) {
        starstream_mutate_ImpactVM_increment(self);
    }

    #[inline]
    pub fn get_counter(self) -> u32 {
        starstream_query_ImpactVM_get_counter(self)
    }
}
