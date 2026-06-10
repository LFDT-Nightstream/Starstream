import * as Starstream from "../src/index.ts";

// --------------------------------------------------------------------------
const contract = await Starstream.Contract.fromFile("../artifacts/yield.wasm");
const utxo = await Starstream.Utxo.spawnFake(contract, "hello-utxo", [], {});
utxo.call("foo", []);
console.log("done?", !utxo.isValid());
