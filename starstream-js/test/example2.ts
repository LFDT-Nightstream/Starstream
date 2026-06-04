import * as Starstream from "../src/index.ts";

// --------------------------------------------------------------------------
const contract = await Starstream.Contract.fromFile("../artifacts/yield.wasm");
console.log(await contract.getId());
console.log(await contract.getImports());
console.log(await contract.getModule());

const instance = new WebAssembly.Instance(await contract.getModule(), {
  ...new Starstream.std.Builtin(),
});
instance.exports["hello-utxo"]();
instance.exports["foo"]();
