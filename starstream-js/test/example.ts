import { instantiate } from "./adder/jco-out/adder";
import * as Starstream from "./starstream";

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

  static MyUtxo = class MyUtxo extends Starstream.Utxo {
    new(): Promise<Adder.MyUtxo> {
      throw new Error("stub");
    }
  };

  get my_coordination(): Starstream.CoordinationScript<[Adder.MyUtxo], number> {
    return this.getCoordinationScript("my_coordination");
  }
}
declare namespace Adder {
  type MyUtxo = typeof Adder.MyUtxo.prototype;
}

// --------------------------------------------------------------------------
// 1. Acquire ledger
const ledger: Starstream.Ledger = new Starstream.RpcLedger(
  "http://localhost:8080",
);
const ledger2 = new Starstream.InMemoryLedger({ genesisUtxos: [] });

// 2. Acquire Utxo
const untypedUtxo: Starstream.Utxo = await ledger.getUtxo("0x42");
const typedUtxo: Adder.MyUtxo = await ledger.getUtxo("0x42", Adder.MyUtxo);
const manualDowncast = untypedUtxo.downcast(Adder.MyUtxo);

// 3. Acquire contract
const adder: Adder = await ledger.getContract("0x4494494", Adder);
// Local, non-uploaded contracts are OK for coordination scripts
const localContract = await Starstream.Contract.fromFile(
  "./synthetic_coordination_script.wasm",
);
// If you want to use Utxos from a contract, it must be uploaded
await ledger.uploadContract(localContract, { fee: 1 });
// Can get a Utxo's contract
const existingContract = await untypedUtxo.getContract();

// 4. Call coordination script to produce proof
const proof = await Starstream.prove(
  // coordination script
  adder.my_coordination,
  // array of arguments to coordination script
  [typedUtxo],
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
console.log("return value:", proof.returnValue);

// 5. Post proof to ledger as transaction
const tx = await ledger.postTransaction(proof, { fee: 1 });
console.log("posted transaction:", tx);
