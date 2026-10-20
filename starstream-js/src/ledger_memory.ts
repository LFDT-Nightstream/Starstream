import type {
  Account,
  FuncType,
  Ledger,
  Transaction,
  TransactionInput,
  TransactionOutput,
} from ".";

export class InMemoryLedger implements Ledger {
  constructor(options?: { genesisOutputs?: TransactionOutput[] }) {
    throw new Error("todo");
  }
  getNetworkName(): Promise<string> {
    throw new Error("todo");
  }
  getBlockHeight(): Promise<bigint> {
    throw new Error("todo");
  }
  getBlock(height: bigint): Promise<Uint8Array[] | undefined> {
    throw new Error("todo");
  }
  getAdminNonce(): Promise<bigint> {
    throw new Error("todo");
  }
  getAccountBalance(key: Uint8Array): Promise<bigint | undefined> {
    throw new Error("todo");
  }
  getAccountNonce(key: Uint8Array): Promise<bigint | undefined> {
    throw new Error("todo");
  }
  getContractWasm(
    digest: string,
  ): Promise<Uint8Array<ArrayBuffer> | undefined> {
    throw new Error("todo");
  }
  getContractEnvelope(digest: string): Promise<Uint8Array | undefined> {
    throw new Error("todo");
  }
  getTransaction(digest: string): Promise<Transaction | undefined> {
    throw new Error("todo");
  }
  getTransactionEnvelope(digest: string): Promise<Uint8Array | undefined> {
    throw new Error("todo");
  }
  getGenesisOutputs(): Promise<TransactionOutput[]> {
    throw new Error("todo");
  }
  getOutput(output: TransactionInput): Promise<TransactionOutput | undefined> {
    throw new Error("todo");
  }
  callUtxoMethod(
    output: TransactionInput,
    name: string,
    ty: FuncType,
    args: unknown[],
  ): Promise<unknown> {
    throw new Error("todo");
  }
  fund(
    admin: Account,
    nonce: bigint,
    account: Uint8Array,
    amount: bigint,
  ): Promise<void> {
    throw new Error("todo");
  }
  publishContract(
    account: Account,
    nonce: bigint,
    wasm: Uint8Array,
  ): Promise<string> {
    throw new Error("todo");
  }
  postTransaction(signer: Account, transaction: Transaction): Promise<string> {
    throw new Error("todo");
  }
}
