import type {
  Ledger,
  Utxo,
  Proof,
  PostTransactionOptions,
  Transaction,
  Contract,
  UploadContractOptions,
} from ".";

export class InMemoryLedger implements Ledger {
  constructor(options?: { genesisUtxos?: Utxo[] }) {
    throw new Error("todo");
  }
  postTransaction(
    proof: Proof,
    options?: PostTransactionOptions,
  ): Promise<Transaction> {
    throw new Error("todo");
  }
  getUtxo(id: string): Promise<Utxo> {
    throw new Error("todo");
  }
  getContract(id: string): Promise<Contract> {
    throw new Error("todo");
  }
  uploadContract(
    contract: Contract,
    options?: UploadContractOptions,
  ): Promise<void> {
    throw new Error("todo");
  }

  readonly genesisUtxos: readonly Utxo[];
}
