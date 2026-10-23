// JS bindings to the Starstream runtime.

import type { Type } from "@bytecodealliance/wrpc";

// ----------------------------------------------------------------------------
// Ledgers

/** Ed25519 account signing ledger envelopes. */
export interface Account {
  /** Raw 32-byte Ed25519 public key identifying the account. */
  readonly publicKey: Uint8Array;
  /** Sign `message` with the account's Ed25519 secret key. */
  sign(message: Uint8Array): Promise<Uint8Array>;
}

export interface Ledger {
  /** Get the name of the ledger network. */
  getNetworkName(): Promise<string>;

  /** Get the height of the latest ledger block. */
  getBlockHeight(): Promise<bigint>;
  /** Get the signed envelopes of the block at `height`, if any. Genesis, at height 0, has none. */
  getBlock(height: bigint): Promise<Uint8Array[] | undefined>;

  /** Get the last nonce the ledger admin used for funding. */
  getAdminNonce(): Promise<bigint>;
  /** Get the balance of the account with the Ed25519 public key `key`, if it exists. */
  getAccountBalance(key: Uint8Array): Promise<bigint | undefined>;
  /** Get the last nonce the account `key` used to publish contracts, if it exists. */
  getAccountNonce(key: Uint8Array): Promise<bigint | undefined>;

  /** Get the Wasm of the published contract with `digest`, if any. */
  getContractWasm(digest: string): Promise<Uint8Array<ArrayBuffer> | undefined>;
  /** Get the signed envelope the contract with `digest` was published in, if any. */
  getContractEnvelope(digest: string): Promise<Uint8Array | undefined>;

  /** Get the transaction with `digest`, if any. */
  getTransaction(digest: string): Promise<Transaction | undefined>;
  /** Get the signed envelope the transaction with `digest` was submitted in, if any. */
  getTransactionEnvelope(digest: string): Promise<Uint8Array | undefined>;

  /** Get the outputs of the genesis block. */
  getGenesisOutputs(): Promise<TransactionOutput[]>;
  /** Get the output referenced by `output`, if any, whether or not it has been spent. */
  getOutput(output: TransactionInput): Promise<TransactionOutput | undefined>;

  /**
   * Call the method `name` of the UTXO created by `output` with `args` of
   * `ty.params`, discarding the resulting state, and return its result of
   * `ty.result`.
   */
  callUtxoMethod(
    output: TransactionInput,
    name: string,
    ty: FuncType,
    args: unknown[],
  ): Promise<unknown>;

  /**
   * Credit `account` with `amount`, signed by the ledger `admin`.
   * `nonce` must be greater than the last nonce `admin` used for funding.
   */
  fund(
    admin: Account,
    nonce: bigint,
    account: Uint8Array,
    amount: bigint,
  ): Promise<void>;
  /**
   * Publish the contract `wasm`, signed and paid for by `account`, and return
   * its digest. `nonce` must be greater than the last nonce of `account`.
   * Republishing a contract fails with `409 Conflict`.
   */
  publishContract(
    account: Account,
    nonce: bigint,
    wasm: Uint8Array,
  ): Promise<string>;
  /**
   * Submit `transaction` signed by `signer` and return its digest.
   * Spending genesis outputs requires the admin signature.
   * Resubmitting a transaction fails with `409 Conflict`.
   */
  postTransaction(signer: Account, transaction: Transaction): Promise<string>;
}

export { RpcLedger } from "./ledger_rpc.ts";
export { InMemoryLedger } from "./ledger_memory.ts";
export { t, type Type } from "@bytecodealliance/wrpc";

/** Parameter and result types of a function. */
export interface FuncType {
  params?: Type[];
  result?: Type;
}

// ----------------------------------------------------------------------------
// Transactions

/** Reference to a transaction output. An empty `transaction` refers to a genesis output. */
export interface TransactionInput {
  transaction: string;
  index: number;
}

export type GlobalValue =
  | { tag: "i32"; val: number }
  | { tag: "i64"; val: bigint }
  | { tag: "f32"; val: number }
  | { tag: "f64"; val: bigint }
  | { tag: "v128"; val: [bigint, bigint] };

export interface DataState {
  memoryIndex: number;
  offset: number;
  data: Uint8Array;
}

export interface ModuleState {
  memories: bigint[];
  globals: GlobalValue[];
  data: DataState[];
}

export interface TransactionOutput {
  contract: string;
  instance: string;
  methods: [bigint, bigint, bigint, bigint][];
  storage: Uint8Array;
  state: ModuleState[];
}

export interface TransactionEvent {
  abiName: string;
  name: string;
  params: Uint8Array;
}

export interface Transaction {
  inputs: TransactionInput[];
  outputs: TransactionOutput[];
  events: TransactionEvent[];
  proof: Uint8Array;
}

import { digest } from "./digest.ts";
export { digest };

// ----------------------------------------------------------------------------
// Contracts

type ContractConstructor<T> = new (wasm: Uint8Array<ArrayBuffer>) => T;

/** Starstream contract Wasm file loaded in memory. */
export class Contract {
  private readonly wasm: Uint8Array<ArrayBuffer>;
  private readonly module: Promise<WebAssembly.Module>;

  /** Load a contract from a Wasm module directly. */
  constructor(wasm: Uint8Array<ArrayBuffer>) {
    // TODO: use our Wasmtime variant as a library rather than browser's Wasm implementation
    this.wasm = wasm;
    this.module = WebAssembly.compile(wasm);
  }

  /** Load a contract from a file path (Node only). */
  static async fromFile(path: string): Promise<Contract> {
    const { readFile } = await import("node:fs/promises");
    const buffer = await readFile(path);
    return new this(buffer);
  }

  /** Get the ledger digest of the contract Wasm. */
  async getId(): Promise<string> {
    return digest(this.wasm);
  }

  downcast<T extends Contract>(type: ContractConstructor<T>): T {
    throw new Error("todo");
  }

  async getModule(): Promise<WebAssembly.Module> {
    return this.module;
  }

  async getImports(): Promise<WebAssembly.ModuleImportDescriptor[]> {
    return WebAssembly.Module.imports(await this.module);
  }

  // TODO: pass in real type somehow for runtime type checking.
  getCoordinationScript<TArgs extends unknown[], TImports, TReturn>(
    name: string,
  ): CoordinationScript<TArgs, TImports, TReturn> {
    throw new Error("todo");
  }
}

// ----------------------------------------------------------------------------
// UTXOs

type UtxoConstructor<T> = new (
  contract: Contract,
  instance: WebAssembly.Instance,
) => T;

/** Handle to a ledger Utxo */
export class Utxo {
  private readonly contract: Contract;
  private readonly instance: WebAssembly.Instance;
  private valid_methods: bigint[] = [];

  constructor(contract: Contract, instance: WebAssembly.Instance) {
    // Unfortunately, we seem to be unable to assert that the contract and instance correspond.
    this.contract = contract;
    this.instance = instance;
  }

  /** Spawn a free-floating Utxo of this type. Cannot be committed to a ledger later. */
  static async spawnFake(
    contract: Contract,
    main_fn: string,
    args: unknown[],
    imports: WebAssembly.Imports,
  ): Promise<Utxo> {
    const builtin = new std.Builtin();
    const instance = new WebAssembly.Instance(await contract.getModule(), {
      ...builtin,
      ...imports,
    });
    const utxo = new this(contract, instance);
    builtin.initUtxo(utxo);
    // TODO: type checking
    (instance.exports[main_fn] as Function)(...args);
    return utxo;
  }

  _implements_method(hash: [bigint, bigint, bigint, bigint]) {
    // TODO: real tracking
    this.valid_methods.push(hash[0]);
  }

  isValid() {
    return this.valid_methods.length !== 0;
  }

  call(exported_fn: string, args: unknown[]) {
    if (!this.isValid()) {
      throw new Error("Cannot call methods on spent Utxo");
    }
    // TODO: type checking
    this.valid_methods.splice(0);
    (this.instance.exports[exported_fn] as Function)(...args);
  }

  // Get the contract (wasm component) that contains the code for this Utxo. It may or may not contain coordination scripts, other Utxo codes, etc.
  async getContract(): Promise<Contract> {
    return this.contract;
  }

  downcast<T extends Utxo>(type: UtxoConstructor<T>): T {
    return new type(this.contract, this.instance);
  }
}

// TODO: Tokens

// ----------------------------------------------------------------------------
// Coordination scripts

/**  */
export interface CoordinationScript<
  TArgs extends unknown[],
  TImports,
  TReturn,
> {
  readonly contract: Contract;
  readonly name: string;
}

// ----------------------------------------------------------------------------
// Traces and proofs

export interface Trace<TReturn> {
  readonly returnValue: TReturn;
  // TODO: inputs, outputs, whatever other ancillary data
}

export interface Proof {
  // TODO: serialized proof object
}

export async function call<
  TArgs extends unknown[],
  TImports extends WebAssembly.Imports,
  TReturn,
>(
  script: CoordinationScript<TArgs, TImports, TReturn>,
  inputs: TArgs,
  imports: TImports,
): Promise<Trace<TReturn>> {
  const instance = new WebAssembly.Instance(await script.contract.getModule(), {
    ...new std.Builtin(),
    ...imports,
  });
  const func = instance.exports[script.name];
  if (typeof func !== "function") {
    throw new Error("Coordination script was not a function");
  }
  // TODO: verify that it's a `script fn` specifically, verify other stuff
  const returnValue = func(...inputs);
  return {
    returnValue,
  };
}

export function prove<TReturn>(trace: Trace<TReturn>): Promise<Proof> {
  throw new Error("todo");
}

// TODO: a "non-core" default L2 contract that implements combining of two Starstream proofs into one

// ----------------------------------------------------------------------------
// Standard library

import * as std from "./std.ts";
export { std };
