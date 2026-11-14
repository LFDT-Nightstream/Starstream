// JS bindings to the Starstream runtime.

// ----------------------------------------------------------------------------
// Ledgers

export { Ledger, type FuncType } from "./ledger.ts";
export {
  WasmLedger,
  type Genesis,
  type GenesisScript,
  type WasmLedgerOptions,
} from "./wasm-ledger.ts";
export {
  WasmClient,
  type CoordinationScriptCall,
  type CoordinationScriptOptions,
} from "./wasm-client.ts";
export { t, type Type } from "@bytecodealliance/wrpc";

// ----------------------------------------------------------------------------
// Transactions

export type {
  DataState,
  GlobalValue,
  ModuleState,
  Transaction,
  TransactionEvent,
  TransactionOutput,
} from "./bindings/starstream/ledger/types.js";
export { TransactionInput } from "./bindings/starstream/ledger/types.js";

export { type Signer } from "./envelope.ts";

export { digest } from "./digest.ts";

// ----------------------------------------------------------------------------
// Wasm embedding

export {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
  type RuntimeWebImports,
  unusedImports,
} from "./runtime-web.ts";
