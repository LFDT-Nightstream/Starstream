import { TransactionInput } from "./bindings/starstream/ledger/types.js";
import type { FuncType } from "./ledger.ts";
import {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
} from "./runtime-web.ts";
import {
  decoder as valueDecoder,
  encoder as valueEncoder,
  pack,
  Reader,
  t,
  unpack,
  Writer,
  type InputValue,
  type Type,
} from "@bytecodealliance/wrpc";

const decoder = new TextDecoder();
const encoder = new TextEncoder();

interface Exports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  client_alloc(len: number): number;
  client_start(url: number, len: number): number;
  client_script_type(
    contract: number,
    contractLen: number,
    script: number,
    scriptLen: number,
  ): number;
  client_call(
    contract: number,
    contractLen: number,
    script: number,
    scriptLen: number,
    args: number,
    argsLen: number,
    cardanoBlockHeight: bigint,
    cardanoCurrentSlot: bigint,
  ): number;
  client_fund(
    key: number,
    keyLen: number,
    account: number,
    accountLen: number,
    amount: bigint,
    network: number,
    networkLen: number,
    nonce: bigint,
    hasNonce: number,
  ): number;
  client_publish(
    key: number,
    keyLen: number,
    wasm: number,
    wasmLen: number,
    network: number,
    networkLen: number,
    nonce: bigint,
    hasNonce: number,
  ): number;
  client_transact(
    key: number,
    keyLen: number,
    tx: number,
    txLen: number,
    network: number,
    networkLen: number,
    nonce: bigint,
    hasNonce: number,
  ): number;
  client_public_key(key: number, len: number): number;
  client_fetch_response(
    reply: number,
    status: number,
    body: number,
    len: number,
  ): void;
  client_fetch_error(reply: number, err: number, len: number): void;
}

type Operation = Exclude<
  {
    [K in keyof Exports]: Exports[K] extends (...args: never[]) => number
      ? K
      : never;
  }[keyof Exports],
  "client_alloc" | "client_start" | "client_public_key" | undefined
>;

/** Type described by `client_script_type`. */
type Described =
  | {
      kind:
        | "bool"
        | "s8"
        | "u8"
        | "s16"
        | "u16"
        | "s32"
        | "u32"
        | "s64"
        | "u64"
        | "f32"
        | "f64"
        | "char"
        | "string"
        | "utxo";
    }
  | { kind: "list"; element: Described }
  | { kind: "record"; fields: { name: string; ty: Described }[] }
  | { kind: "tuple"; elements: Described[] }
  | { kind: "variant"; cases: { name: string; ty: Described | null }[] }
  | { kind: "enum" | "flags"; names: string[] }
  | { kind: "option"; some: Described }
  | { kind: "result"; ok: Described | null; err: Described | null };

/** Get the type `ty` describes, with UTXOs of type `utxo`. */
function described(ty: Described, utxo: Type): Type {
  const of = (ty: Described) => described(ty, utxo);
  switch (ty.kind) {
    case "utxo":
      return utxo;
    case "list":
      return t.list(of(ty.element));
    case "record":
      return t.record(ty.fields.map(({ name, ty }) => ({ name, ty: of(ty) })));
    case "tuple":
      return t.tuple(...ty.elements.map(of));
    case "variant":
      return t.variant(
        ty.cases.map(({ name, ty }) => ({ name, ty: ty && of(ty) })),
      );
    case "enum":
      return t.enumType(ty.names);
    case "flags":
      return t.flags(ty.names);
    case "option":
      return t.option(of(ty.some));
    case "result":
      return t.result(ty.ok && of(ty.ok), ty.err && of(ty.err));
    default:
      return t[ty.kind];
  }
}

interface Outcome {
  value: Uint8Array;
  results: Uint8Array;
}

/** Options of `WasmClient.callCoordinationScript`. */
export interface CoordinationScriptOptions {
  /** Cardano block height reported to the script (`cardano#block-height`). */
  cardanoBlockHeight?: bigint;
  /** Cardano current slot reported to the script (`cardano#current-slot`). */
  cardanoCurrentSlot?: bigint;
}

/** Result of `WasmClient.callCoordinationScript`. */
export interface CoordinationScriptCall<T = unknown> {
  /**
   * The CBOR-encoded transaction the call produced, unsigned, not submitted
   * and without nonces.
   */
  transaction: Uint8Array;
  /** The result of the script. */
  result: T;
}

/** Options of the envelopes signed by `WasmClient`. */
export interface EnvelopeOptions {
  /** Network the envelope is bound to, defaults to the one of the ledger. */
  network?: string;
}

/** Options of the envelopes signed by `WasmClient` with a nonce. */
export interface NonceOptions extends EnvelopeOptions {
  /** Nonce, defaults to the one following the last one used by the key. */
  nonce?: bigint;
}

/**
 * Ledger client running in this JS realm, as an instance of the
 * `starstream-ledger-client-web` Wasm module, which executes coordination
 * scripts locally and signs envelopes, sending its requests to the ledger
 * HTTP API. Its operations run one at a time. Requires JSPI
 * (`WebAssembly.Suspending`).
 */
export class WasmClient {
  private exports!: Exports;
  private runtime!: RuntimeWeb;
  private operations!: {
    [K in Operation]: (...args: Parameters<Exports[K]>) => Promise<number>;
  };
  private error: string | undefined;
  private outcome: Outcome | undefined;
  private failure: unknown;
  private interrupt: ((err: unknown) => void) | undefined;
  private queue = Promise.resolve();
  private scriptTypes = new Map<string, Promise<FuncType>>();

  private constructor(private readonly fetch: typeof globalThis.fetch) {}

  /**
   * Instantiate the compiled `starstream-ledger-client-web` module, sending
   * requests to the ledger HTTP API at the base `url` with `options.fetch`,
   * defaulting to the global `fetch`.
   */
  static async instantiate(
    module: WebAssembly.Module,
    url: string | URL,
    options?: { fetch?: typeof fetch },
  ): Promise<WasmClient> {
    const client = new WasmClient(
      options?.fetch ?? ((input, init) => fetch(input, init)),
    );
    client.runtime = new RuntimeWeb();
    const instance = await WebAssembly.instantiate(module, {
      env: {
        ...client.runtime.imports,
        client_error: (ptr: number, len: number) => {
          client.error = decoder.decode(client.bytes(ptr, len));
        },
        client_result: (
          value: number,
          valueLen: number,
          results: number,
          resultsLen: number,
        ) => {
          client.outcome = {
            value: client.bytes(value, valueLen).slice(),
            results: client.bytes(results, resultsLen).slice(),
          };
        },
        client_fetch: (
          head: number,
          headLen: number,
          body: number,
          bodyLen: number,
          reply: number,
        ) => {
          const [line, ...headers] = decoder
            .decode(client.bytes(head, headLen))
            .split("\n")
            .filter((line) => line);
          const i = line.indexOf(" ");
          const init: RequestInit = {
            method: line.slice(0, i),
            headers: headers.map((line): [string, string] => {
              const i = line.indexOf(": ");
              return [line.slice(0, i), line.slice(i + 2)];
            }),
          };
          if (bodyLen > 0) {
            init.body = client.bytes(body, bodyLen).slice();
          }
          void client.send(reply, line.slice(i + 1), init);
        },
      },
    });
    client.exports = instance.exports as unknown as Exports;
    client.runtime.init(client.exports);
    const { exports } = client;
    client.operations = {
      client_script_type: promising(exports.client_script_type),
      client_call: promising(exports.client_call),
      client_fund: promising(exports.client_fund),
      client_publish: promising(exports.client_publish),
      client_transact: promising(exports.client_transact),
    };
    const encoded = encoder.encode(String(url));
    if (exports.client_start(client.write(encoded), encoded.length) !== 0) {
      throw new Error(client.error);
    }
    return client;
  }

  /** Fail later calls, once those in flight complete. */
  dispose(): void {
    this.failure ??= new Error("client disposed");
  }

  /**
   * Get the type of the coordination script `script` of the published
   * contract with `contract` digest. UTXO parameters are of type
   * `TransactionInput`, and UTXO results are the index of their output in the
   * transaction, of type `t.u32`.
   */
  getCoordinationScriptType(
    contract: string,
    script: string,
  ): Promise<FuncType> {
    const key = `${contract}#${script}`;
    let ty = this.scriptTypes.get(key);
    if (!ty) {
      ty = this.run(
        "failed to get coordination script type",
        "client_script_type",
        [encoder.encode(contract), encoder.encode(script)],
      ).then(({ value }) => {
        const { params, result } = JSON.parse(decoder.decode(value)) as {
          params: { name: string; ty: Described }[];
          result: Described | null;
        };
        return {
          params: params.map(({ ty }) => described(ty, TransactionInput)),
          result: result ? described(result, t.u32) : undefined,
        };
      });
      this.scriptTypes.set(key, ty);
      ty.catch(() => this.scriptTypes.delete(key));
    }
    return ty;
  }

  /**
   * Call the coordination script `script` of the published contract with
   * `contract` digest with `args` and return the transaction it produces,
   * without submitting it, and its result, of the types given by
   * `getCoordinationScriptType`.
   */
  async callCoordinationScript(
    contract: string,
    script: string,
    args: unknown[],
    options?: CoordinationScriptOptions,
  ): Promise<CoordinationScriptCall> {
    const ty = await this.getCoordinationScriptType(contract, script);
    const params = ty.params ?? [];
    if (args.length !== params.length) {
      throw new Error(
        `expected ${params.length} argument(s), got ${args.length}`,
      );
    }
    const w = new Writer();
    params.forEach((ty, i) => pack(valueEncoder(ty), args[i] as InputValue, w));
    const { value, results } = await this.run(
      "failed to call coordination script",
      "client_call",
      [
        encoder.encode(contract),
        encoder.encode(script),
        w.finish(),
        options?.cardanoBlockHeight ?? 0n,
        options?.cardanoCurrentSlot ?? 0n,
      ],
    );
    return {
      transaction: value,
      result: ty.result
        ? unpack(valueDecoder(ty.result), new Reader(results))
        : undefined,
    };
  }

  /**
   * Credit the account with the Ed25519 public key `account` with `amount`,
   * signed by the admin Ed25519 secret key `key`.
   */
  async fund(
    key: Uint8Array,
    account: Uint8Array,
    amount: bigint,
    options?: NonceOptions,
  ): Promise<void> {
    const { nonce } = options ?? {};
    await this.run("failed to fund account", "client_fund", [
      key,
      account,
      amount,
      encoder.encode(options?.network ?? ""),
      nonce ?? 0n,
      nonce === undefined ? 0 : 1,
    ]);
  }

  /**
   * Publish the contract `wasm`, signed by the Ed25519 secret key `key`, and
   * return its digest.
   */
  async publishContract(
    key: Uint8Array,
    wasm: Uint8Array,
    options?: NonceOptions,
  ): Promise<string> {
    const { nonce } = options ?? {};
    const { value } = await this.run(
      "failed to publish contract",
      "client_publish",
      [
        key,
        wasm,
        encoder.encode(options?.network ?? ""),
        nonce ?? 0n,
        nonce === undefined ? 0 : 1,
      ],
    );
    return decoder.decode(value);
  }

  /**
   * Submit the CBOR-encoded transaction `tx` with the nonce of the Ed25519
   * secret key `key` as its only nonce, signed by `key`, and return its digest.
   */
  async submitTransaction(
    key: Uint8Array,
    tx: Uint8Array,
    options?: NonceOptions,
  ): Promise<string> {
    const { nonce } = options ?? {};
    const { value } = await this.run(
      "failed to submit transaction",
      "client_transact",
      [
        key,
        tx,
        encoder.encode(options?.network ?? ""),
        nonce ?? 0n,
        nonce === undefined ? 0 : 1,
      ],
    );
    return decoder.decode(value);
  }

  /** Get the Ed25519 public key of the Ed25519 secret key `key`. */
  publicKey(key: Uint8Array): Uint8Array {
    if (this.failure) {
      throw this.failure;
    }
    const { error, outcome } = this;
    try {
      if (this.exports.client_public_key(this.write(key), key.length) !== 0) {
        throw new Error(this.error);
      }
      return this.outcome!.value;
    } catch (err) {
      throw this.trap(err);
    } finally {
      this.error = error;
      this.outcome = outcome;
    }
  }

  /**
   * Run operation `op` with `args`, after the operations before it, passing
   * each `Uint8Array` as a pointer and length.
   */
  private run<K extends Operation>(
    what: string,
    op: K,
    args: (Uint8Array | number | bigint)[],
  ): Promise<Outcome> {
    const outcome = this.queue.then(async () => {
      if (this.failure) {
        throw this.failure;
      }
      this.error = undefined;
      this.outcome = undefined;
      let code: number;
      try {
        const params = args.flatMap((arg) =>
          arg instanceof Uint8Array ? [this.write(arg), arg.length] : [arg],
        );
        code = await Promise.race([
          (this.operations[op] as (...args: unknown[]) => Promise<number>)(
            ...params,
          ),
          new Promise<never>((_, reject) => {
            this.interrupt = reject;
          }),
        ]);
      } catch (err) {
        throw this.trap(err);
      } finally {
        this.interrupt = undefined;
      }
      if (code !== 0) {
        throw new Error(this.error ? `${what}: ${this.error}` : what);
      }
      return this.outcome!;
    });
    this.queue = outcome.then(
      () => {},
      () => {},
    );
    return outcome;
  }

  private async send(
    reply: number,
    uri: string,
    init: RequestInit,
  ): Promise<void> {
    let status = 0;
    let data: Uint8Array;
    try {
      const res = await this.fetch(uri, init);
      data = new Uint8Array(await res.arrayBuffer());
      status = res.status;
    } catch (err) {
      data = encoder.encode(err instanceof Error ? err.message : String(err));
    }
    try {
      const ptr = this.write(data);
      if (status) {
        this.exports.client_fetch_response(reply, status, ptr, data.length);
      } else {
        this.exports.client_fetch_error(reply, ptr, data.length);
      }
    } catch (err) {
      const failure = this.trap(err);
      this.failure ??= failure;
      this.interrupt?.(failure);
    }
  }

  /** Fail later calls with the error of the trap `cause`. */
  private trap(cause: unknown): unknown {
    if (!(cause instanceof WebAssembly.RuntimeError)) {
      return cause;
    }
    const err = this.error ? new Error(this.error, { cause }) : cause;
    this.failure ??= err;
    return err;
  }

  private bytes(ptr: number, len: number): Uint8Array<ArrayBuffer> {
    return new Uint8Array(this.exports.memory.buffer, ptr >>> 0, len);
  }

  private write(data: Uint8Array): number {
    const ptr = this.exports.client_alloc(data.length);
    this.bytes(ptr, data.length).set(data);
    return ptr;
  }
}
