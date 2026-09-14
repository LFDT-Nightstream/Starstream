// Small domains for bounded verification in CI.
module starstream_verify {
  pure val methods = Set("m0", "m1")

  import starstream_sim_core(
    SIM_METHODS = methods,
    SIM_VALUES = Set(List(0, 0, 0, 0)),
    SIM_RESOURCE_HANDLES = Set(0, 1),
    SIM_MAX_UTXOS = 2,
    SIM_MAX_METHODS = 2,
    SIM_MAX_INPUTS = 1,
    SIM_ABIS = methods.map(m => List(m))
  ).* from "./sim_core"
}
