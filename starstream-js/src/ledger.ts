import { digest as computeDigest } from "./digest.ts";
import {
  buildFundEnvelope,
  buildPublishEnvelope,
  buildTransactionEnvelope,
  type Signer,
} from "./envelope.ts";
import * as accountRpc from "./bindings/starstream/ledger/account.js";
import * as adminRpc from "./bindings/starstream/ledger/admin.js";
import * as blockRpc from "./bindings/starstream/ledger/block.js";
import * as contractRpc from "./bindings/starstream/ledger/contract.js";
import * as genesisRpc from "./bindings/starstream/ledger/genesis.js";
import * as networkRpc from "./bindings/starstream/ledger/network.js";
import * as transactionRpc from "./bindings/starstream/ledger/transaction.js";
import type {
  GetError,
  Transaction,
  TransactionInput,
  TransactionOutput,
} from "./bindings/starstream/ledger/types.js";
import {
  encodeValue,
  fromFetch,
  invoke,
  Mux,
  t,
  Writer,
  type InputValue,
  type Transport,
  type Type,
} from "@bytecodealliance/wrpc";

const APPLICATION_COSE = "application/cose";
const APPLICATION_WASM = "application/wasm";
const APPLICATION_WRPC = "application/x.wrpc";
const TEXT_PLAIN = "text/plain";

const LEDGER_UTXO_INSTANCE = "starstream:ledger/utxo";

const CANONICAL_DIGEST = /^b[a-z2-7]{55}$/;

/** Parameter and result types of a function. */
export interface FuncType {
  params?: Type[];
  result?: Type;
}

type GetResult<T> =
  { kind: "ok"; value: T } | { kind: "error"; value: GetError };

function unwrapGet<T>(res: GetResult<T>, what: string): T | undefined {
  if (res.kind === "ok") {
    return res.value;
  }
  if (res.value.kind === "not-found") {
    return undefined;
  }
  throw new Error(`invalid ${what}: ${res.value.value}`);
}

/** Ledger accessed over the HTTP API of `starstream-ledger-node`. */
export class Ledger {
  private readonly url: string;
  private readonly fetch: typeof fetch | undefined;

  constructor(url: string | URL, options?: { fetch?: typeof fetch }) {
    const base = String(url);
    if (/[?#]/.test(base)) {
      throw new Error(
        `base URL \`${base}\` must not contain a query or fragment`,
      );
    }
    this.url = `${base.replace(/\/+$/, "")}/rpc`;
    this.fetch = options?.fetch;
  }

  private transport(): Transport {
    return fromFetch(this.url, {
      fetch: this.fetch,
      buffer: true,
      headers: { "content-type": APPLICATION_WRPC },
    });
  }

  /** Get the name of the ledger network. */
  async getNetworkName(): Promise<string> {
    return networkRpc.getName(this.transport());
  }

  /** Get the height of the latest ledger block. */
  async getBlockHeight(): Promise<bigint> {
    return blockRpc.getHeight(this.transport());
  }

  /** Get the signed envelopes of the block at `height`, if any. Genesis, at height 0, has none. */
  async getBlock(height: bigint): Promise<Uint8Array[] | undefined> {
    return blockRpc.getBlock(this.transport(), height);
  }

  /** Get the last nonce the ledger admin used for funding. */
  async getAdminNonce(): Promise<bigint> {
    return adminRpc.getNonce(this.transport());
  }

  /** Get the balance of the account with the Ed25519 public key `key`, if it exists. */
  async getAccountBalance(key: Uint8Array): Promise<bigint | undefined> {
    const res = await accountRpc.getBalance(this.transport(), key);
    return unwrapGet(res, "key");
  }

  /** Get the last nonce the account `key` used to publish contracts, if it exists. */
  async getAccountNonce(key: Uint8Array): Promise<bigint | undefined> {
    const res = await accountRpc.getNonce(this.transport(), key);
    return unwrapGet(res, "key");
  }

  /** Get the Wasm of the published contract with `digest`, if any. */
  async getContractWasm(digest: string): Promise<Uint8Array | undefined> {
    if (!CANONICAL_DIGEST.test(digest)) {
      throw new Error(`invalid digest: \`${digest}\` is not canonical`);
    }
    const res = await contractRpc.getWasm(this.transport(), digest);
    const wasm = unwrapGet(res, "digest");
    if (!wasm) {
      return undefined;
    }
    if (!crypto.subtle) {
      console.warn(
        `\`crypto.subtle\` is unavailable, skipping validation of contract \`${digest}\``,
      );
      return wasm;
    }
    const got = await computeDigest(wasm);
    if (got !== digest) {
      throw new Error(
        `contract digest mismatch: expected \`${digest}\`, got \`${got}\``,
      );
    }
    return wasm;
  }

  /** Get the signed envelope the contract with `digest` was published in, if any. */
  async getContractEnvelope(digest: string): Promise<Uint8Array | undefined> {
    const res = await contractRpc.getEnvelope(this.transport(), digest);
    return unwrapGet(res, "digest");
  }

  /** Get the transaction with `digest`, if any. */
  async getTransaction(digest: string): Promise<Transaction | undefined> {
    const res = await transactionRpc.getTransaction(this.transport(), digest);
    return unwrapGet(res, "digest");
  }

  /** Get the signed envelope the transaction with `digest` was submitted in, if any. */
  async getTransactionEnvelope(
    digest: string,
  ): Promise<Uint8Array | undefined> {
    const res = await transactionRpc.getEnvelope(this.transport(), digest);
    return unwrapGet(res, "digest");
  }

  private async submit(
    envelope: Uint8Array<ArrayBuffer>,
    what: string,
  ): Promise<string> {
    const post = this.fetch ?? fetch;
    const res = await post(this.url, {
      method: "POST",
      headers: { "content-type": APPLICATION_COSE },
      body: envelope,
    });
    const body = await res.text();
    if (!res.ok) {
      throw new Error(`failed to ${what}: ${body}`);
    }
    return body;
  }

  /**
   * Credit the account with the Ed25519 public key `account` with `amount`,
   * signed by the admin `signer` for `network`, defaulting to the ledger's,
   * with `nonce`, defaulting to the one following the last admin nonce.
   */
  async fund(
    signer: Signer,
    account: Uint8Array,
    amount: bigint,
    options?: { network?: string; nonce?: bigint },
  ): Promise<void> {
    const [network, nonce] = await Promise.all([
      options?.network ?? this.getNetworkName(),
      options?.nonce ?? this.getAdminNonce().then((nonce) => nonce + 1n),
    ]);
    const envelope = await buildFundEnvelope(
      signer,
      network,
      nonce,
      account,
      amount,
    );
    await this.submit(envelope, "fund account");
  }

  /**
   * Publish the contract `wasm`, signed by `signer` for `network`, defaulting
   * to the ledger's, with `nonce`, defaulting to the one following the last
   * nonce of `signer`, and return its digest.
   */
  async publishContract(
    signer: Signer,
    wasm: Uint8Array,
    options?: { network?: string; nonce?: bigint },
  ): Promise<string> {
    const [network, nonce] = await Promise.all([
      options?.network ?? this.getNetworkName(),
      options?.nonce ??
        this.getAccountNonce(signer.publicKey).then((nonce) => {
          if (nonce === undefined) {
            throw new Error("account not found");
          }
          return nonce + 1n;
        }),
    ]);
    const envelope = await buildPublishEnvelope(signer, network, nonce, wasm);
    return this.submit(envelope, "publish contract");
  }

  /**
   * Sign `tx` by `signer` for `network`, defaulting to the ledger's, submit it
   * and return its digest.
   */
  async submitTransaction(
    signer: Signer,
    tx: Transaction,
    options?: { network?: string },
  ): Promise<string> {
    const network = options?.network ?? (await this.getNetworkName());
    const envelope = await buildTransactionEnvelope(signer, network, tx);
    return this.submit(envelope, "submit transaction");
  }

  /** Get the outputs of the genesis block. */
  async getGenesisOutputs(): Promise<TransactionOutput[]> {
    return genesisRpc.getOutputs(this.transport());
  }

  /**
   * Get the output referenced by `output`, if any, whether or not it has been
   * spent. An empty `output.transaction` refers to a genesis output.
   */
  async getOutput(
    output: TransactionInput,
  ): Promise<TransactionOutput | undefined> {
    if (!output.transaction) {
      const outputs = await this.getGenesisOutputs();
      return outputs[output.index];
    }
    const tx = await this.getTransaction(output.transaction);
    return tx?.outputs[output.index];
  }

  private fetchWit(accept: string): Promise<Response>;
  private fetchWit(
    accept: string,
    output: TransactionInput,
  ): Promise<Response | undefined>;
  private async fetchWit(
    accept: string,
    output?: TransactionInput,
  ): Promise<Response | undefined> {
    const query = new URLSearchParams();
    if (output) {
      query.set("index", String(output.index));
      if (output.transaction) {
        query.set("transaction", output.transaction);
      }
    }
    const get = this.fetch ?? fetch;
    const res = await get(`${this.url}?${query}`, { headers: { accept } });
    if (output && res.status === 404) {
      await res.body?.cancel();
      return undefined;
    }
    if (!res.ok) {
      throw new Error(`failed to get WIT: ${await res.text()}`);
    }
    return res;
  }

  /** Get the WIT of the ledger API. */
  async getWit(): Promise<string> {
    const res = await this.fetchWit(TEXT_PLAIN);
    return res.text();
  }

  /** Get the WIT of the ledger API, encoded as Wasm. */
  async getWitWasm(): Promise<Uint8Array> {
    const res = await this.fetchWit(APPLICATION_WASM);
    return new Uint8Array(await res.arrayBuffer());
  }

  /**
   * Get the WIT of the UTXO created by `output`, describing its methods as
   * `callUtxoMethod` invokes them. Returns `undefined` on a 404, which the
   * ledger responds with if the output does not exist or is spent, or if its
   * contract or instance is missing.
   */
  async getUtxoWit(output: TransactionInput): Promise<string | undefined> {
    const res = await this.fetchWit(TEXT_PLAIN, output);
    return res?.text();
  }

  /** Like `getUtxoWit`, but encoded as Wasm. */
  async getUtxoWitWasm(
    output: TransactionInput,
  ): Promise<Uint8Array | undefined> {
    const res = await this.fetchWit(APPLICATION_WASM, output);
    return res && new Uint8Array(await res.arrayBuffer());
  }

  /**
   * Call the method `name` of the UTXO created by `output` with `args` of
   * `ty.params`, discarding the resulting state, and return its result of
   * `ty.result`.
   */
  async callUtxoMethod(
    output: TransactionInput,
    name: string,
    ty: FuncType,
    args: unknown[],
  ): Promise<unknown> {
    const params = ty.params ?? [];
    if (args.length !== params.length) {
      throw new Error(
        `expected ${params.length} argument(s), got ${args.length}`,
      );
    }
    const { results, finished } = await invoke(
      this.transport(),
      LEDGER_UTXO_INSTANCE,
      name,
      [t.option(t.string), t.u32, ...params],
      [output.transaction || undefined, output.index, ...args] as InputValue[],
      ty.result ? [ty.result] : [],
    );
    await finished;
    return results[0] as unknown;
  }

  /**
   * Call the method `name` of the UTXO created by `output` with the
   * wRPC-encoded `params`, discarding the resulting state, and return its
   * wRPC-encoded results. The method must not take or return streams or
   * futures.
   */
  async callUtxoMethodEncoded(
    output: TransactionInput,
    name: string,
    params: Uint8Array,
  ): Promise<Uint8Array> {
    const w = new Writer();
    encodeValue(w, t.option(t.string), output.transaction || undefined);
    encodeValue(w, t.u32, output.index);
    w.bytes(params);
    const mux = new Mux(this.transport());
    try {
      await mux.writeHeader(LEDGER_UTXO_INSTANCE, name);
      mux.restrict([], 0);
      mux.start();
      await mux.outgoing([]).write(w.finish());
      await mux.closeWrite();
      const root = mux.incoming([]);
      const results = new Writer();
      while (await root.fill(1)) {
        const chunk = root.remaining();
        results.bytes(chunk);
        root.consume(chunk.length);
      }
      mux.finish([], root);
      await mux.ended;
      return results.finish();
    } catch (err) {
      mux.abort(err);
      throw err;
    }
  }
}
