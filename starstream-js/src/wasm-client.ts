import {
  Transaction as TransactionType,
  TransactionOutput as TransactionOutputType,
  type Transaction,
} from "./bindings/starstream/ledger/types.js";
import type { FuncType, Ledger } from "./ledger.ts";
import {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
} from "./runtime-web.ts";
import {
  decodeValue,
  encodeValue,
  Reader,
  Writer,
  type InputValue,
} from "@bytecodealliance/wrpc";

const decoder = new TextDecoder();
const encoder = new TextEncoder();

interface Exports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  client_alloc(len: number): number;
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
  client_reply_ok(reply: number, data: number, len: number): void;
  client_reply_error(reply: number, err: number, len: number): void;
}

/** Options of `WasmClient.callCoordinationScript`. */
export interface CoordinationScriptOptions {
  /** Cardano block height reported to the script (`cardano#block-height`). */
  cardanoBlockHeight?: bigint;
  /** Cardano current slot reported to the script (`cardano#current-slot`). */
  cardanoCurrentSlot?: bigint;
}

/** Result of `WasmClient.callCoordinationScript`. */
export interface CoordinationScriptCall {
  /** The transaction the call produced, unsigned and not submitted. */
  transaction: Transaction;
  /** The result of the script, of `ty.result`. */
  result: unknown;
}

/**
 * Ledger client running in this JS realm, as an instance of the
 * `starstream-ledger-client-web` Wasm module, which executes coordination
 * scripts locally, fetching contracts and outputs from a `Ledger`. Requires
 * JSPI (`WebAssembly.Suspending`).
 */
export class WasmClient {
  private exports!: Exports;
  private runtime!: RuntimeWeb;
  private callScript!: (
    ...args: Parameters<Exports["client_call"]>
  ) => Promise<number>;
  private error: string | undefined;
  private result: { transaction: Uint8Array; results: Uint8Array } | undefined;
  private queue: Promise<unknown> = Promise.resolve();

  private constructor(private readonly ledger: Ledger) {}

  /**
   * Instantiate the compiled `starstream-ledger-client-web` module, fetching
   * contracts and outputs from `ledger`.
   */
  static async instantiate(
    module: WebAssembly.Module,
    ledger: Ledger,
  ): Promise<WasmClient> {
    const client = new WasmClient(ledger);
    client.runtime = new RuntimeWeb();
    const instance = await WebAssembly.instantiate(module, {
      env: {
        ...client.runtime.imports,
        client_error: (ptr: number, len: number) => {
          client.error = decoder.decode(client.bytes(ptr, len));
        },
        client_result: (
          tx: number,
          txLen: number,
          results: number,
          resultsLen: number,
        ) => {
          client.result = {
            transaction: client.bytes(tx, txLen).slice(),
            results: client.bytes(results, resultsLen).slice(),
          };
        },
        client_get_contract: (digest: number, len: number, reply: number) => {
          const contract = decoder.decode(client.bytes(digest, len));
          void client.reply(reply, async () => {
            const wasm = await client.ledger.getContractWasm(contract);
            if (!wasm) {
              throw new Error("contract not found");
            }
            return wasm;
          });
        },
        client_get_output: (
          transaction: number,
          len: number,
          index: number,
          reply: number,
        ) => {
          const input = {
            transaction: decoder.decode(client.bytes(transaction, len)),
            index,
          };
          void client.reply(reply, async () => {
            const output = await client.ledger.getOutput(input);
            if (!output) {
              throw new Error("output not found");
            }
            const w = new Writer();
            encodeValue(w, TransactionOutputType, output as InputValue);
            return w.finish();
          });
        },
      },
    });
    client.exports = instance.exports as unknown as Exports;
    client.runtime.init(client.exports);
    client.callScript = promising(client.exports.client_call);
    return client;
  }

  /**
   * Call the coordination script `script` of the published contract with
   * `contract` digest with `args` of `ty.params` and return the transaction
   * it produces, without submitting it, and its result of `ty.result`. UTXO
   * parameters are given as `TransactionInput`s of type `TransactionInput`,
   * and UTXO results are the index of their output in the transaction, of
   * type `t.u32`.
   */
  async callCoordinationScript(
    contract: string,
    script: string,
    ty: FuncType,
    args: unknown[],
    options?: CoordinationScriptOptions,
  ): Promise<CoordinationScriptCall> {
    const params = ty.params ?? [];
    if (args.length !== params.length) {
      throw new Error(
        `expected ${params.length} argument(s), got ${args.length}`,
      );
    }
    const w = new Writer();
    params.forEach((ty, i) => encodeValue(w, ty, args[i] as InputValue));
    const encoded = w.finish();
    const call = this.queue.then(() =>
      this.call(contract, script, encoded, ty, options),
    );
    this.queue = call.catch(() => {});
    return call;
  }

  private async call(
    contract: string,
    script: string,
    args: Uint8Array,
    ty: FuncType,
    options?: CoordinationScriptOptions,
  ): Promise<CoordinationScriptCall> {
    const contractBytes = encoder.encode(contract);
    const scriptBytes = encoder.encode(script);
    const contractPtr = this.write(contractBytes);
    const scriptPtr = this.write(scriptBytes);
    const argsPtr = this.write(args);
    this.error = undefined;
    this.result = undefined;
    let code: number;
    try {
      code = await this.callScript(
        contractPtr,
        contractBytes.length,
        scriptPtr,
        scriptBytes.length,
        argsPtr,
        args.length,
        options?.cardanoBlockHeight ?? 0n,
        options?.cardanoCurrentSlot ?? 0n,
      );
    } catch (err) {
      throw this.error ? new Error(this.error, { cause: err }) : err;
    }
    if (code !== 0 || !this.result) {
      throw new Error(this.error ?? "coordination script call failed");
    }
    const { transaction, results } = this.result;
    return {
      transaction: decodeValue(
        new Reader(transaction),
        TransactionType,
      ) as Transaction,
      result: ty.result
        ? decodeValue(new Reader(results), ty.result)
        : undefined,
    };
  }

  private async reply(
    reply: number,
    f: () => Promise<Uint8Array>,
  ): Promise<void> {
    let data: Uint8Array;
    let ok = true;
    try {
      data = await f();
    } catch (err) {
      data = encoder.encode(err instanceof Error ? err.message : String(err));
      ok = false;
    }
    const ptr = this.write(data);
    if (ok) {
      this.exports.client_reply_ok(reply, ptr, data.length);
    } else {
      this.exports.client_reply_error(reply, ptr, data.length);
    }
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
