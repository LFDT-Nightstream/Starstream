//! Shared demo geometry and deliberately small, insecure proof parameters.

use neo_fold_clean::frontends::nebula::layout::NebulaParams;
use neo_fold_clean::paper::params::Params;
use neo_wasm::{WasmNebulaLimits, WasmNebulaProfile, WasmProgramTables};

fn steps_per_segment() -> usize {
    std::env::var("STARSTREAM_PROOF_STEPS")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(4)
}

/// Geometry and limits for one core instance.
///
/// Match RAM to ROM to satisfy `r <= mu`, and choose a scan width and batch
/// size that fit the whole trace into one segment.
pub fn profile(rom_bits: u32, rows: usize) -> WasmNebulaProfile {
    let steps = steps_per_segment();
    let cells = (1_u64 << rom_bits) * 2;
    let b_scan = usize::try_from(cells / steps as u64).expect("scan width fits usize");
    let memory =
        NebulaParams::new(rom_bits, rom_bits, 64, b_scan, 16).expect("valid Nebula geometry");
    // Compiled contracts declare one linear-memory page but these traces never
    // touch it, a handful of globals (storage plus yield snapshots), and no
    // tables. The reduced-memory entrypoint skips the page-capacity check.
    let limits =
        WasmNebulaLimits::new(64, 16, 16, 16, 64, 8, 2, 4).expect("valid small WASM limits");
    WasmNebulaProfile::with_schedule(memory, limits, rows.div_ceil(steps).max(1))
}

/// Parameters for a relation whose ROM has `rom_bits` address bits.
///
/// The relation's row and column counts grow with the memory scan width and
/// the batch width (about 28M to 32M at `r = 14` with 4 steps per segment),
/// and the parameters' row-domain and packed-assignment bound `m` must
/// cover both.
pub fn params(rom_bits: u32) -> Params {
    let columns = if rom_bits >= 14 {
        1_u64 << 26
    } else {
        1_u64 << 25
    };
    if std::env::var_os("STARSTREAM_PROOF_PARAMS").is_some_and(|value| value == "production") {
        // Appendix B.2 core with the effective security level lowered to what
        // the WASM relation's shape (`t = 13`, degree 8) supports at `s = 2`.
        return Params::for_ccs_shape_with(
            usize::try_from(columns / 2).expect("row bound fits usize"),
            usize::try_from(columns).expect("column bound fits usize"),
            13,
            8,
            neo_fold_clean::config::MIN_EFFECTIVE_LAMBDA,
            neo_fold_clean::config::EXTENSION_SAFETY_MARGIN_BITS,
        )
        .expect("WASM parameters");
    }
    // INSECURE demo parameters (kappa = 1, λ = 20) matching Nightstream's
    // Metal demo fixture.
    let raw = neo_params::NeoParams::new(
        neo_params::goldilocks_paper_b2::Q,
        neo_params::goldilocks_paper_b2::ETA as u32,
        neo_params::goldilocks_paper_b2::D as u32,
        1,
        columns,
        neo_params::goldilocks_paper_b2::B_BASE,
        neo_params::goldilocks_paper_b2::K_RHO,
        neo_params::goldilocks_paper_b2::T,
        neo_params::goldilocks_paper_b2::EXTENSION_DEGREE,
        20,
    )
    .expect("demo SuperNeo parameters");
    Params::test_only_from_neo_params(raw)
}

pub fn entry_pc(tables: &WasmProgramTables, function_ref: u32) -> wasmtime::Result<u64> {
    tables
        .function_entries
        .iter()
        .find(|&&(fref, _)| fref == u64::from(function_ref))
        .map(|&(_, pc)| pc)
        .ok_or_else(|| wasmtime::format_err!("function {function_ref} has no entry pc"))
}
