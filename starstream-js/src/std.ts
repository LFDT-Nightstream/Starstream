// Starstream standard library for contracts to import.

import type { Ledger } from ".";

export class Builtin {
  "starstream:std/builtin" = {
    "implements-method"(hash: [bigint, bigint, bigint, bigint]) {
      console.log("implements-method", hash);
    },
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
