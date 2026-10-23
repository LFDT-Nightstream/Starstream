import { digest as computeDigest } from "./digest.ts";
import type {
  Account,
  FuncType,
  Ledger,
  Transaction,
  TransactionInput,
  TransactionOutput,
} from "./index.ts";
import { invoke, t, type Transport, type Value } from "@bytecodealliance/wrpc";

const APPLICATION_WRPC = "application/x.wrpc";

const LEDGER_ADMIN_INSTANCE = "starstream:ledger/admin";
const LEDGER_ACCOUNT_INSTANCE = "starstream:ledger/account";
const LEDGER_BLOCK_INSTANCE = "starstream:ledger/block";
const LEDGER_CONTRACT_INSTANCE = "starstream:ledger/contract";
const LEDGER_TRANSACTION_INSTANCE = "starstream:ledger/transaction";
const LEDGER_GENESIS_INSTANCE = "starstream:ledger/genesis";
const LEDGER_UTXO_INSTANCE = "starstream:ledger/utxo";
const LEDGER_NETWORK_INSTANCE = "starstream:ledger/network";

const bytes = t.list(t.u8);

const getError = t.variant({ "not-found": null, malformed: t.string });

const getBytesResult = t.result(bytes, getError);

const getU64Result = t.result(t.u64, getError);

const transactionOutput = t.record({
  contract: t.string,
  instance: t.string,
  methods: t.list(t.tuple(t.u64, t.u64, t.u64, t.u64)),
  storage: bytes,
  state: t.list(
    t.record({
      memories: t.list(t.u64),
      globals: t.list(
        t.variant({
          i32: t.s32,
          i64: t.s64,
          f32: t.u32,
          f64: t.u64,
          v128: t.tuple(t.u64, t.u64),
        }),
      ),
      data: t.list(
        t.record({ memoryIndex: t.u32, offset: t.u32, data: bytes }),
      ),
    }),
  ),
});

const transaction = t.record({
  inputs: t.list(t.record({ transaction: t.string, index: t.u32 })),
  outputs: t.list(transactionOutput),
  events: t.list(
    t.record({ abiName: t.string, name: t.string, params: bytes }),
  ),
  proof: bytes,
});

const DIGEST = /^bciq[a-p][a-z2-7]{50}[aiqy]$/;

function checkDigest(digest: string) {
  if (!DIGEST.test(digest)) {
    throw new Error(
      `invalid digest \`${digest}\`: expected a multibase base32-lower sha2-256 multihash`,
    );
  }
}

const getTransactionResult = t.result(transaction, getError);

type Variant = { tag: string; val?: Value };

function unwrapGet<T>(res: unknown, what: string): T | undefined {
  const variant = res as Variant;
  if (variant.tag === "ok") {
    return variant.val as T;
  }
  const err = variant.val as Variant;
  if (err.tag === "not-found") {
    return undefined;
  }
  throw new Error(`invalid ${what}: ${err.val}`);
}

/** Ledger accessed over the HTTP API of `starstream-ledger-node`. */
export class RpcLedger implements Ledger {
  private readonly url: string;
  private readonly fetch: typeof fetch;

  constructor(url: string | URL, options?: { fetch?: typeof fetch }) {
    const base = new URL(url, globalThis.location?.href);
    if (/[?#]/.test(base.href)) {
      throw new Error(
        `base URL \`${base}\` must not contain a query or fragment`,
      );
    }
    this.url = base.href.replace(/\/+$/, "");
    this.fetch = options?.fetch ?? globalThis.fetch;
  }

  private async post(
    instance: string,
    name: string,
    body: Blob,
    signal: AbortSignal,
  ): Promise<ReadableStreamDefaultReader<Uint8Array>> {
    const fetch = this.fetch;
    const res = await fetch(`${this.url}/rpc`, {
      method: "POST",
      headers: { "content-type": APPLICATION_WRPC },
      body,
      signal,
    });
    if (!res.ok) {
      throw new Error(
        `\`${instance}#${name}\` failed with status ${res.status}: ${await res.text()}`,
      );
    }
    const contentType = res.headers.get("content-type");
    if (contentType?.split(";")[0].trim().toLowerCase() !== APPLICATION_WRPC) {
      await res.body?.cancel();
      throw new Error(
        `\`${instance}#${name}\` returned content-type \`${contentType}\`, expected \`${APPLICATION_WRPC}\``,
      );
    }
    return (res.body ?? new Blob().stream()).getReader();
  }

  private transport(instance: string, name: string): Transport {
    const req: Uint8Array<ArrayBuffer>[] = [];
    const res =
      Promise.withResolvers<ReadableStreamDefaultReader<Uint8Array>>();
    const ctrl = new AbortController();
    return {
      write: (buf) => {
        req.push(new Uint8Array(buf));
      },
      closeWrite: async () => {
        res.resolve(this.post(instance, name, new Blob(req), ctrl.signal));
        await res.promise;
      },
      read: async () => {
        const { value, done } = await (await res.promise).read();
        return done ? null : value;
      },
      abort: async (reason) => {
        ctrl.abort(reason);
        res.reject(reason);
        await (await res.promise).cancel(reason);
      },
    };
  }

  private async call<T>(
    instance: string,
    name: string,
    ty: FuncType,
    args: unknown[],
  ): Promise<T> {
    const { results, done } = await invoke(
      this.transport(instance, name),
      instance,
      name,
      ty.params ?? [],
      args as Value[],
      ty.result ? [ty.result] : [],
    );
    await done;
    return results[0] as T;
  }

  async getNetworkName(): Promise<string> {
    return this.call<string>(
      LEDGER_NETWORK_INSTANCE,
      "get-name",
      { result: t.string },
      [],
    );
  }

  async getBlockHeight(): Promise<bigint> {
    return this.call<bigint>(
      LEDGER_BLOCK_INSTANCE,
      "get-height",
      { result: t.u64 },
      [],
    );
  }

  async getBlock(height: bigint): Promise<Uint8Array[] | undefined> {
    return this.call<Uint8Array[] | undefined>(
      LEDGER_BLOCK_INSTANCE,
      "get-block",
      { params: [t.u64], result: t.option(t.list(bytes)) },
      [height],
    );
  }

  async getAdminNonce(): Promise<bigint> {
    return this.call<bigint>(
      LEDGER_ADMIN_INSTANCE,
      "get-nonce",
      { result: t.u64 },
      [],
    );
  }

  async getAccountBalance(key: Uint8Array): Promise<bigint | undefined> {
    const res = await this.call(
      LEDGER_ACCOUNT_INSTANCE,
      "get-balance",
      { params: [bytes], result: getU64Result },
      [key],
    );
    return unwrapGet(res, "key");
  }

  async getAccountNonce(key: Uint8Array): Promise<bigint | undefined> {
    const res = await this.call(
      LEDGER_ACCOUNT_INSTANCE,
      "get-nonce",
      { params: [bytes], result: getU64Result },
      [key],
    );
    return unwrapGet(res, "key");
  }

  async getContractWasm(
    digest: string,
  ): Promise<Uint8Array<ArrayBuffer> | undefined> {
    checkDigest(digest);
    const res = await this.call(
      LEDGER_CONTRACT_INSTANCE,
      "get-wasm",
      { params: [t.string], result: getBytesResult },
      [digest],
    );
    const wasm = unwrapGet<Uint8Array<ArrayBuffer>>(res, "digest");
    if (!wasm) {
      return undefined;
    }
    const got = await computeDigest(wasm);
    if (got !== digest) {
      throw new Error(
        `contract digest mismatch: expected \`${digest}\`, got \`${got}\``,
      );
    }
    return wasm;
  }

  async getContractEnvelope(digest: string): Promise<Uint8Array | undefined> {
    checkDigest(digest);
    const res = await this.call(
      LEDGER_CONTRACT_INSTANCE,
      "get-envelope",
      { params: [t.string], result: getBytesResult },
      [digest],
    );
    return unwrapGet(res, "digest");
  }

  async getTransaction(digest: string): Promise<Transaction | undefined> {
    checkDigest(digest);
    const res = await this.call(
      LEDGER_TRANSACTION_INSTANCE,
      "get-transaction",
      { params: [t.string], result: getTransactionResult },
      [digest],
    );
    return unwrapGet(res, "digest");
  }

  async getTransactionEnvelope(
    digest: string,
  ): Promise<Uint8Array | undefined> {
    checkDigest(digest);
    const res = await this.call(
      LEDGER_TRANSACTION_INSTANCE,
      "get-envelope",
      { params: [t.string], result: getBytesResult },
      [digest],
    );
    return unwrapGet(res, "digest");
  }

  async getGenesisOutputs(): Promise<TransactionOutput[]> {
    return this.call<TransactionOutput[]>(
      LEDGER_GENESIS_INSTANCE,
      "get-outputs",
      { result: t.list(transactionOutput) },
      [],
    );
  }

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

  async callUtxoMethod(
    output: TransactionInput,
    name: string,
    ty: FuncType,
    args: unknown[],
  ): Promise<unknown> {
    if (output.transaction) {
      checkDigest(output.transaction);
    }
    const params = ty.params ?? [];
    if (args.length !== params.length) {
      throw new RangeError(
        `expected ${params.length} argument(s), got ${args.length}`,
      );
    }
    return this.call(
      LEDGER_UTXO_INSTANCE,
      name,
      {
        params: [t.option(t.string), t.u32, ...params],
        result: ty.result,
      },
      [output.transaction || undefined, output.index, ...args],
    );
  }

  async fund(
    admin: Account,
    nonce: bigint,
    account: Uint8Array,
    amount: bigint,
  ): Promise<void> {
    throw new Error("todo");
  }

  async publishContract(
    account: Account,
    nonce: bigint,
    wasm: Uint8Array,
  ): Promise<string> {
    throw new Error("todo");
  }

  async postTransaction(
    signer: Account,
    transaction: Transaction,
  ): Promise<string> {
    throw new Error("todo");
  }
}
