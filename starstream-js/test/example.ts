import { readFile } from "node:fs/promises";
import * as Starstream from "../src/index.ts";

// --------------------------------------------------------------------------
// Starstream example code
/*
abi MyAbi {
    effect my_effect() -> i32;
    event my_event(number: i32);
    fn abi_method();
}
utxo MyUtxo {
    main fn new() { ... }
    impl MyAbi { ... }
}
script fn my_coordination() { ... }
*/

// --------------------------------------------------------------------------
// Pseudocode generated bindings
class Adder extends Starstream.Contract {
  // TODO: only what is needed for the below to typecheck is written here.
  // Generated bindings would also include Abi, events, etc.

  static MyAbi = {
    my_event: "my-event", // < whatever string or symbol to make the wasm import connect
    my_effect: "my-effect",
  };

  static MyUtxo = class MyUtxo {
    constructor(
      readonly ledger: Starstream.Ledger,
      readonly output: Starstream.TransactionInput,
    ) {}

    async abi_method(): Promise<void> {
      await this.ledger.callUtxoMethod(this.output, "abi-method", {}, []);
    }
  };

  get my_coordination(): Starstream.CoordinationScript<
    [Adder.MyUtxo],
    {},
    number
  > {
    return this.getCoordinationScript("my_coordination");
  }
}
declare namespace Adder {
  type MyUtxo = typeof Adder.MyUtxo.prototype;
}

// --------------------------------------------------------------------------
// 1. Acquire ledger and account
const ledger: Starstream.Ledger = new Starstream.RpcLedger(
  "http://localhost:8080",
);
declare const account: Starstream.Account;

// 2. Acquire Utxo
// Outputs are referenced by transaction digest and index, genesis by an empty digest
const input: Starstream.TransactionInput = { transaction: "", index: 0 };
const output = await ledger.getOutput(input);
if (!output) {
  throw new Error("no such output");
}
const utxo = new Adder.MyUtxo(ledger, input);
// Runs on the ledger, which discards the resulting state
await utxo.abi_method();

// 3. Acquire contract
// Can get a Utxo's contract
const wasm = await ledger.getContractWasm(output.contract);
if (!wasm) {
  throw new Error("no such contract");
}
const adder = new Adder(wasm);
// Local, non-published contracts are OK for coordination scripts
const localWasm = await readFile("./synthetic_coordination_script.wasm");
const localContract = new Starstream.Contract(localWasm);
// If you want to use Utxos from a contract, it must be published
const nonce = ((await ledger.getAccountNonce(account.publicKey)) ?? 0n) + 1n;
await ledger.publishContract(account, nonce, localWasm);

// 4. Call coordination script to produce proof
const trace = await Starstream.call(
  // coordination script
  adder.my_coordination,
  // array of arguments to coordination script
  [utxo],
  // "imports" = std lib + event + effect handlers
  {
    ...new Starstream.std.Cardano(ledger),
    [Adder.MyAbi.my_event](number: number) {
      console.log("Event emitted from wasm!", number);
    },
    [Adder.MyAbi.my_effect]: async () => {
      return 42; // resumes caller of effect
      // to kill tx, `throw` here
      // no direct `break`... maybe raising another effect should be allowed?
    },
  },
);
console.log("return value:", trace.returnValue);
const proof = await Starstream.prove(trace);
console.log("proof:", proof);

// 5. Post transaction to ledger
const tx = await ledger.postTransaction(account, {
  inputs: [input],
  outputs: [], // TODO: from the trace
  events: [], // TODO: from the trace
  proof: new Uint8Array(), // TODO: serialized `proof`
});
console.log("posted transaction:", tx);
