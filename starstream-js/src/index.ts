// JS bindings to the Starstream runtime.

// ----------------------------------------------------------------------------
// Ledgers

export interface UploadContractOptions {}
export interface PostTransactionOptions {}

export interface Ledger {
  /** Look up a Utxo on the ledger by ID. */
  getUtxo(id: string): Promise<Utxo>;
  /** Look up a Utxo on the ledger by ID, and cast it to a specific type. Throws on type mismatch. */
  getUtxo<T extends Utxo>(id: string, type: UtxoConstructor<T>): Promise<T>;

  /** Fetch an abstract contract from the ledger by ID. */
  getContract(id: string): Promise<Contract>;
  /** Fetch a typed contract from the ledger by ID. Throws on type mismatch. */
  getContract<T extends Contract>(
    id: string,
    type: ContractConstructor<T>,
  ): Promise<T>;
  /** Upload a contract to the ledger. Fast and cheap if it has already been uploaded. */
  uploadContract(
    contract: Contract,
    options?: UploadContractOptions,
  ): Promise<void>;

  postTransaction(
    proof: Proof,
    options?: PostTransactionOptions,
  ): Promise<Transaction>;
}

// TODO: class RpcLedger { constructor(url: string); }

export { InMemoryLedger } from "./ledger_memory.ts";

// ----------------------------------------------------------------------------
// Transactions

export class Transaction {}

// ----------------------------------------------------------------------------
// Contracts

type ContractConstructor<T> = new (wasm: Uint8Array<ArrayBuffer>) => T;

/** Starstream contract Wasm file loaded in memory. */
export class Contract {
  private readonly module: Promise<WebAssembly.Module>;
  private readonly id: Promise<ArrayBuffer>;

  /** Load a contract from a Wasm module directly. */
  constructor(wasm: Uint8Array<ArrayBuffer>) {
    // TODO: use our Wasmtime variant as a library rather than browser's Wasm implementation
    this.module = WebAssembly.compile(wasm);
    this.id = crypto.subtle.digest("sha-256", wasm);
  }

  /** Load a contract from a file path (Node only). */
  static async fromFile(path: string): Promise<Contract> {
    const { readFile } = await import("node:fs/promises");
    const buffer = await readFile(path);
    return new this(buffer);
  }

  // sha-256 of wasm
  async getId(): Promise<string> {
    return uint8ArrayToHexString(new Uint8Array(await this.id));
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

// ----------------------------------------------------------------------------
// Private utilities

function uint8ArrayToHexString(uint8Array: Uint8Array): string {
  return Array.prototype.map
    .call(uint8Array, (byte) => ("0" + byte.toString(16)).slice(-2))
    .join("");
}
