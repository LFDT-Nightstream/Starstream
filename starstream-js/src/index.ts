// JS bindings to the Starstream runtime.

// ----------------------------------------------------------------------------
// Ledgers

export { Ledger, type FuncType } from "./ledger.ts";
export { t, type Type } from "@bytecodealliance/wrpc";

// ----------------------------------------------------------------------------
// Transactions

export type {
  DataState,
  GlobalValue,
  ModuleState,
  Transaction,
  TransactionEvent,
  TransactionInput,
  TransactionOutput,
} from "./bindings/starstream/ledger/types.js";

export { digest } from "./digest.ts";
