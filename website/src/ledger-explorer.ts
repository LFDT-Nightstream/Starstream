import { decode, encode, Tag } from "cbor2";
import {
  digest,
  type Ledger,
  type Transaction,
  type TransactionInput,
  type TransactionOutput,
} from "../../starstream-js/src/index";

const FUND_CONTEXT = 0;
const PUBLISH_CONTEXT = 1;
const TRANSACTION_CONTEXT = 2;
const COSE_SIGN = 98;
const COSE_SIGN1 = 18;
const KID = 4;

interface EnvelopeCommon {
  digest: string;
  /** Hex-encoded public keys of the signers. */
  signers: string[];
  /** Size of the envelope in bytes. */
  size: number;
}

/** Envelope of a block, decoded. */
export type EnvelopeInfo = EnvelopeCommon &
  (
    | { kind: "fund"; account: string; amount: bigint; nonce: bigint }
    | { kind: "publish"; nonce: bigint; wasmSize: number }
    | { kind: "transaction" }
  );

export interface BlockInfo {
  height: bigint;
  envelopes: EnvelopeInfo[];
}

export interface UtxoInfo {
  /** Digest of the transaction that created it, empty for genesis outputs. */
  transaction: string;
  index: number;
  contract: string;
  instance: string;
  /** Size of its storage in bytes. */
  storage: number;
  /** Height of the block that created it, 0 for genesis outputs. */
  height: bigint;
  /** Digest of the transaction that spent it, if any. */
  spentBy?: string;
}

export interface TransactionInfo {
  digest: string;
  height: bigint;
  signers: string[];
  inputs: TransactionInput[];
  outputs: number;
  events: { abiName: string; name: string; params: number }[];
  /** Size of the proof in bytes, 0 without one. */
  proof: number;
}

export interface ContractInfo {
  digest: string;
  /** Height of the block that published it, absent if not published. */
  height?: bigint;
  publisher?: string;
  /** Size of the component in bytes, if published on the ledger. */
  size?: number;
}

export interface AccountInfo {
  /** Hex-encoded Ed25519 public key. */
  key: string;
  balance?: bigint;
  nonce?: bigint;
  /** Sum of the amounts the account was funded with. */
  funded: bigint;
}

/** Everything the ledger exposes, indexed for display. */
export interface ExplorerState {
  network: string;
  height: bigint;
  adminNonce: bigint;
  /** Blocks in ascending order of height. */
  blocks: BlockInfo[];
  contracts: ContractInfo[];
  transactions: TransactionInfo[];
  /** Genesis outputs first, then transaction outputs in block order. */
  utxos: UtxoInfo[];
  accounts: AccountInfo[];
}

function hex(bytes: Uint8Array): string {
  return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

function bytes(v: unknown, what: string): Uint8Array {
  if (!(v instanceof Uint8Array)) {
    throw new Error(`${what} is not a byte string`);
  }
  return v;
}

function int(v: unknown, what: string): bigint {
  if (typeof v !== "number" && typeof v !== "bigint") {
    throw new Error(`${what} is not an integer`);
  }
  return BigInt(v);
}

function signer(header: unknown): string {
  const map = decode(bytes(header, "protected header"), { preferMap: true });
  if (!(map instanceof Map)) {
    throw new Error("protected header is not a map");
  }
  return hex(bytes(map.get(KID), "`kid` header"));
}

/** Decode `envelope`, a `COSE_Sign` or `COSE_Sign1`, without verifying it. */
export async function readEnvelope(
  envelope: Uint8Array,
): Promise<EnvelopeInfo> {
  const cose = decode(envelope, { preferMap: true });
  if (
    !(cose instanceof Tag) ||
    (cose.tag !== COSE_SIGN && cose.tag !== COSE_SIGN1) ||
    !Array.isArray(cose.contents)
  ) {
    throw new Error("envelope is not a `COSE_Sign` or `COSE_Sign1`");
  }
  const [header, , payloadValue, signatures] = cose.contents as unknown[];
  const signers =
    cose.tag === COSE_SIGN1
      ? [signer(header)]
      : (signatures as unknown[][]).map(([header]) => signer(header));
  const payload = bytes(payloadValue, "payload");
  const decoded = decode(payload, { preferMap: true });
  if (!Array.isArray(decoded) || decoded.length !== 3) {
    throw new Error("payload is not a `[network, context, message]` array");
  }
  const [network, context, message] = decoded as [string, number, unknown];
  const messageBytes = payload.subarray(1 + encode(network).length + 1);
  const common = { signers, size: envelope.length };
  switch (context) {
    case FUND_CONTEXT: {
      const [nonce, account, amount] = message as unknown[];
      return {
        ...common,
        kind: "fund",
        digest: await digest(messageBytes),
        account: hex(bytes(account, "account")),
        amount: int(amount, "amount"),
        nonce: int(nonce, "nonce"),
      };
    }
    case PUBLISH_CONTEXT: {
      const [nonce, wasm] = message as unknown[];
      const component = bytes(wasm, "contract");
      return {
        ...common,
        kind: "publish",
        digest: await digest(component),
        nonce: int(nonce, "nonce"),
        wasmSize: component.length,
      };
    }
    case TRANSACTION_CONTEXT:
      return {
        ...common,
        kind: "transaction",
        digest: await digest(messageBytes),
      };
  }
  throw new Error(`unknown envelope context \`${context}\``);
}

/**
 * Index of a ledger, which decodes each block and fetches each transaction
 * once, as neither change.
 */
export class Explorer {
  private readonly blocks = new Map<bigint, BlockInfo>();
  private readonly transactions = new Map<string, Transaction>();
  private genesis: TransactionOutput[] | undefined;

  constructor(
    private readonly ledger: Ledger,
    private readonly network: string,
    private readonly admin: string,
  ) {}

  private async block(height: bigint): Promise<BlockInfo> {
    let block = this.blocks.get(height);
    if (!block) {
      const envelopes = (await this.ledger.getBlock(height)) ?? [];
      block = {
        height,
        envelopes: await Promise.all(envelopes.map(readEnvelope)),
      };
      this.blocks.set(height, block);
    }
    return block;
  }

  private async transaction(digest: string): Promise<Transaction> {
    let tx = this.transactions.get(digest);
    if (!tx) {
      tx = await this.ledger.getTransaction(digest);
      if (!tx) {
        throw new Error(`transaction \`${digest}\` not found`);
      }
      this.transactions.set(digest, tx);
    }
    return tx;
  }

  async state(): Promise<ExplorerState> {
    const { ledger } = this;
    const [height, adminNonce] = await Promise.all([
      ledger.getBlockHeight(),
      ledger.getAdminNonce(),
    ]);
    this.genesis ??= await ledger.getGenesisOutputs();
    const blocks: BlockInfo[] = [];
    for (let h = 1n; h <= height; h++) {
      blocks.push(await this.block(h));
    }

    const contracts = new Map<string, ContractInfo>();
    const transactions: TransactionInfo[] = [];
    const utxos = new Map<string, UtxoInfo>();
    const funded = new Map<string, bigint>();
    const addOutputs = (
      transaction: string,
      outputs: TransactionOutput[],
      height: bigint,
    ) =>
      outputs.forEach(({ contract, instance, storage }, index) => {
        if (!contracts.has(contract)) {
          contracts.set(contract, { digest: contract });
        }
        utxos.set(`${transaction}:${index}`, {
          transaction,
          index,
          contract,
          instance,
          storage: storage.length,
          height,
        });
      });

    addOutputs("", this.genesis, 0n);
    for (const { height, envelopes } of blocks) {
      for (const envelope of envelopes) {
        for (const key of envelope.signers) {
          if (key !== this.admin && !funded.has(key)) {
            funded.set(key, 0n);
          }
        }
        switch (envelope.kind) {
          case "fund":
            funded.set(
              envelope.account,
              (funded.get(envelope.account) ?? 0n) + envelope.amount,
            );
            break;
          case "publish":
            contracts.set(envelope.digest, {
              digest: envelope.digest,
              height,
              publisher: envelope.signers[0],
              size: envelope.wasmSize,
            });
            break;
          case "transaction": {
            const tx = await this.transaction(envelope.digest);
            for (const { transaction, index } of tx.inputs) {
              const utxo = utxos.get(`${transaction}:${index}`);
              if (utxo) {
                utxo.spentBy = envelope.digest;
              }
            }
            addOutputs(envelope.digest, tx.outputs, height);
            transactions.push({
              digest: envelope.digest,
              height,
              signers: envelope.signers,
              inputs: tx.inputs,
              outputs: tx.outputs.length,
              events: tx.events.map(({ abiName, name, params }) => ({
                abiName,
                name,
                params: params.length,
              })),
              proof: tx.proof.length,
            });
          }
        }
      }
    }

    const accounts = await Promise.all(
      Array.from(funded, async ([key, funded]): Promise<AccountInfo> => {
        const account = Uint8Array.from(key.match(/../g)!, (b) =>
          parseInt(b, 16),
        );
        const [balance, nonce] = await Promise.all([
          ledger.getAccountBalance(account),
          ledger.getAccountNonce(account),
        ]);
        return { key, balance, nonce, funded };
      }),
    );
    return {
      network: this.network,
      height,
      adminNonce,
      blocks,
      contracts: [...contracts.values()],
      transactions,
      utxos: [...utxos.values()],
      accounts,
    };
  }
}
