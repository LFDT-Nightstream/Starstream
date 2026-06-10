// Starstream standard library for contracts to import.

import { inherits } from "node:util";
import type { Ledger, Utxo } from ".";

export class Builtin {
  private utxo?: Utxo;

  constructor() {
    const me = this;

    this["starstream:std/builtin"] = {
      "implements-method"(
        hash0: bigint,
        hash1: bigint,
        hash2: bigint,
        hash3: bigint,
      ) {
        if (me.utxo) {
          me.utxo._implements_method([hash0, hash1, hash2, hash3]);
        } else {
          throw new Error("implements-method called outside Utxo");
        }
      },
    };
  }

  initUtxo(utxo: Utxo) {
    if (this.utxo) {
      throw new Error("Cannot init Builtin twice");
    }
    this.utxo = utxo;
  }

  "starstream:std/builtin": {
    "implements-method"(
      hash0: bigint,
      hash1: bigint,
      hash2: bigint,
      hash3: bigint,
    ): void;
  };
}

export class Cardano {
  constructor(ledger: Ledger) {}

  "starstream:std/cardano" = {
    "block-height"() {
      return 0;
    },
    "current-slot"() {
      return 0;
    },
  };
}
