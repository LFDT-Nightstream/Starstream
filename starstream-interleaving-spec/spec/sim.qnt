// Larger domains for fast randomized exploration and the REPL.
module starstream_sim {
  pure val identities = Set("m0", "m1", "m2")
  // Explicit bounded lists: allListsUpTo cannot be translated to Apalache.
  pure val abis = identities.map(m => List(m)).union(
    identities.map(a => identities.map(b => List(a, b))).flatten())

  import starstream_sim_core(
    SIM_IDENTITIES = identities,
    SIM_VALUES = Set(List(0, 0, 0, 0), List(1, 0, 0, 0), List(1, 2, 0, 0)),
    SIM_RESOURCE_HANDLES = Set(0, 1, 2, 3),
    SIM_MAX_UTXOS = 4,
    SIM_MAX_METHODS = 3,
    SIM_MAX_INPUTS = 2,
    SIM_ABIS = abis
  ).* from "./sim_core"
}
