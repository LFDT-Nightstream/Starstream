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
export { WasmCompiler, type Compiled } from "./wasm-compiler.ts";
export {
  WasmClient,
  type CoordinationScriptCall,
  type CoordinationScriptOptions,
  type EnvelopeOptions,
  type NonceOptions,
  type Signature,
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
  TransactionNonce,
  TransactionOutput,
} from "./bindings/starstream/ledger/types.js";
export { TransactionInput } from "./bindings/starstream/ledger/types.js";

export { digest } from "./digest.ts";

// ----------------------------------------------------------------------------
// Wasm embedding

export {
  promising,
  RuntimeWeb,
  suspending,
  type RuntimeWebExports,
  type RuntimeWebImports,
  unusedImports,
} from "./runtime-web.ts";
