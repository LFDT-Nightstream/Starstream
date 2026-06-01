// Rough definitions for API.
// Actual VM would be a Rust crate importing wasmtime & nightstream, compiled to Wasm ideally but perhaps also having native binaries for speed or for initial support before wasmtime-on-wasm is ready.

type UtxoConstructor<T> = new () => T;
type ContractConstructor<T> = new (wasm: Uint8Array) => T;

// TODO: include import types here so they can be typechecked when passed into prove().
export interface CoordinationScript<TArgs extends unknown[], TReturn> {}

export interface UploadContractOptions {
  fee?: number; // < dummy
}
export interface PostTransactionOptions {
  fee?: number; // < dummy
}

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
    proof: Proof<unknown>,
    options?: PostTransactionOptions,
  ): Promise<Transaction>;
}
export class RpcLedger implements Ledger {
  constructor(url: string);
  postTransaction(
    proof: Proof<unknown>,
    options?: PostTransactionOptions,
  ): Promise<Transaction>;
  getUtxo(id: string): Promise<Utxo>;
  getUtxo<T extends Utxo>(id: string, type: UtxoConstructor<T>): Promise<T>;
  getContract(id: string): Promise<Contract>;
  getContract<T extends Contract>(
    id: string,
    type: ContractConstructor<T>,
  ): Promise<T>;
  uploadContract(
    contract: Contract,
    options?: UploadContractOptions,
  ): Promise<void>;
}
export class InMemoryLedger implements Ledger {
  constructor(options?: { genesisUtxos?: Utxo[] });
  postTransaction(
    proof: Proof<unknown>,
    options?: PostTransactionOptions,
  ): Promise<Transaction>;
  getUtxo(id: string): Promise<Utxo>;
  getContract(id: string): Promise<Contract>;
  uploadContract(
    contract: Contract,
    options?: UploadContractOptions,
  ): Promise<void>;

  genesisUtxos: Utxo[];
}

/** Starstream contract wasm file loaded in memory. */
export class Contract {
  constructor(wasm: Uint8Array);
  static fromFile(path: string): Promise<Contract>;

  getId(): string; // sha-256 of wasm

  // TODO: pass in real type somehow for runtime type checking.
  getCoordinationScript<TArgs extends unknown[], TReturn>(
    name: string,
  ): CoordinationScript<TArgs, TReturn>;

  downcast<T extends Contract>(type: ContractConstructor<T>): T;
}

/** Handle to a ledger Utxo */
export class Utxo {
  // Get the contract (wasm component) that contains the code for this Utxo. It may or may not contain coordination scripts, other Utxo codes, etc.
  getContract(): Promise<Contract>;

  downcast<T extends Utxo>(type: UtxoConstructor<T>): T;
}

// TODO: Tokens

export class Proof<TReturn> {
  readonly returnValue: TReturn;
  // TODO: inputs, outputs, whatever other ancillary data
  // TODO: serialized proof object
}

export class Transaction {}

export function prove<TArgs extends unknown[], TReturn>(
  script: CoordinationScript<TArgs, TReturn>,
  inputs: TArgs,
  imports: object,
): Promise<Proof<TReturn>>;

/** Like prove, but skips proof generation, just executes the VM */
export function call<TArgs extends unknown[], TReturn>(
  script: CoordinationScript<TArgs, TReturn>,
  inputs: TArgs,
  imports: object,
): Promise<TReturn>;
// ^ maybe could be changed to return a trace, or we could have trace/prove always be two steps

// TODO: a "non-core" default L2 contract that implements combining of two Starstream proofs into one

// Standard library stuff
export namespace std {
  export class Cardano {
    constructor(ledger: Ledger);

    "starstream:std/cardano": object;
  }
}
