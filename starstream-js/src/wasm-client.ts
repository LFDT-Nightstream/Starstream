import {
  Transaction as TransactionType,
  TransactionOutput as TransactionOutputType,
  type Transaction,
  type TransactionInput,
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
  t,
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
  client_call_wave(
    contract: number,
    contractLen: number,
    script: number,
    scriptLen: number,
    args: number,
    argsLen: number,
    cardanoBlockHeight: bigint,
    cardanoCurrentSlot: bigint,
  ): number;
  client_call_utxo_method_wave(
    transaction: number,
    transactionLen: number,
    index: number,
    method: number,
    methodLen: number,
    args: number,
    argsLen: number,
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
export interface CoordinationScriptCall<T = unknown> {
  /** The transaction the call produced, unsigned and not submitted. */
  transaction: Transaction;
  /** The result of the script. */
  result: T;
}

type CallScript = (
  ...args: Parameters<Exports["client_call"]>
) => Promise<number>;

/**
 * Ledger client running in this JS realm, as an instance of the
 * `starstream-ledger-client-web` Wasm module, which executes coordination
 * scripts locally, fetching contracts and outputs from a `Ledger`. Requires
 * JSPI (`WebAssembly.Suspending`).
 */
export class WasmClient {
  private exports!: Exports;
  private runtime!: RuntimeWeb;
  private callScript!: CallScript;
  private callScriptWave!: CallScript;
  private callUtxoMethod!: (
    ...args: Parameters<Exports["client_call_utxo_method_wave"]>
  ) => Promise<number>;
  private error: string | undefined;
  private result: { transaction: Uint8Array; results: Uint8Array } | undefined;
  private value: Uint8Array | undefined;
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
        client_value: (ptr: number, len: number) => {
          client.value = client.bytes(ptr, len).slice();
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
          const input = client.input(transaction, len, index);
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
        client_get_utxo_wit: (
          transaction: number,
          len: number,
          index: number,
          reply: number,
        ) => {
          const input = client.input(transaction, len, index);
          void client.reply(reply, async () => {
            const wit = await client.ledger.getUtxoWitWasm(input);
            if (!wit) {
              throw new Error("UTXO not found");
            }
            return wit;
          });
        },
        client_call_utxo_method: (
          transaction: number,
          len: number,
          index: number,
          method: number,
          methodLen: number,
          params: number,
          paramsLen: number,
          reply: number,
        ) => {
          const input = client.input(transaction, len, index);
          const name = decoder.decode(client.bytes(method, methodLen));
          const encoded = client.bytes(params, paramsLen).slice();
          void client.reply(reply, () =>
            client.ledger.callUtxoMethodEncoded(input, name, encoded),
          );
        },
      },
    });
    client.exports = instance.exports as unknown as Exports;
    client.runtime.init(client.exports);
    client.callScript = promising(client.exports.client_call);
    client.callScriptWave = promising(client.exports.client_call_wave);
    client.callUtxoMethod = promising(
      client.exports.client_call_utxo_method_wave,
    );
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
    return this.enqueue(async () => {
      const { transaction, results } = await this.call(
        this.callScript,
        contract,
        script,
        w.finish(),
        options,
      );
      return {
        transaction,
        result: ty.result
          ? decodeValue(new Reader(results), ty.result)
          : undefined,
      };
    });
  }

  /**
   * Like `callCoordinationScript`, but with WAVE-encoded `args`, UTXO
   * parameters given as `[TRANSACTION]:INDEX` with an empty `TRANSACTION`
   * for genesis, and the results returned as a WAVE-encoded tuple.
   */
  async callCoordinationScriptWave(
    contract: string,
    script: string,
    args: string[],
    options?: CoordinationScriptOptions,
  ): Promise<CoordinationScriptCall<string>> {
    const w = new Writer();
    encodeValue(w, t.list(t.string), args);
    return this.enqueue(async () => {
      const { transaction, results } = await this.call(
        this.callScriptWave,
        contract,
        script,
        w.finish(),
        options,
      );
      return { transaction, result: decoder.decode(results) };
    });
  }

  /**
   * Call the method `name` of the UTXO created by `output` on the ledger with
   * WAVE-encoded `args`, discarding the resulting state, and return its
   * results as a WAVE-encoded tuple.
   */
  async callUtxoMethodWave(
    output: TransactionInput,
    name: string,
    args: string[],
  ): Promise<string> {
    const w = new Writer();
    encodeValue(w, t.list(t.string), args);
    const params = w.finish();
    return this.enqueue(async () => {
      const transaction = encoder.encode(output.transaction);
      const method = encoder.encode(name);
      const transactionPtr = this.write(transaction);
      const methodPtr = this.write(method);
      const paramsPtr = this.write(params);
      this.error = undefined;
      this.value = undefined;
      let code: number;
      try {
        code = await this.callUtxoMethod(
          transactionPtr,
          transaction.length,
          output.index,
          methodPtr,
          method.length,
          paramsPtr,
          params.length,
        );
      } catch (err) {
        throw this.error ? new Error(this.error, { cause: err }) : err;
      }
      if (code !== 0 || !this.value) {
        throw new Error(this.error ?? "UTXO method call failed");
      }
      return decoder.decode(this.value);
    });
  }

  private enqueue<T>(f: () => Promise<T>): Promise<T> {
    const call = this.queue.then(f);
    this.queue = call.catch(() => {});
    return call;
  }

  private input(
    transaction: number,
    len: number,
    index: number,
  ): TransactionInput {
    return {
      transaction: decoder.decode(this.bytes(transaction, len)),
      index,
    };
  }

  private async call(
    callScript: CallScript,
    contract: string,
    script: string,
    args: Uint8Array,
    options?: CoordinationScriptOptions,
  ): Promise<{ transaction: Transaction; results: Uint8Array }> {
    const contractBytes = encoder.encode(contract);
    const scriptBytes = encoder.encode(script);
    const contractPtr = this.write(contractBytes);
    const scriptPtr = this.write(scriptBytes);
    const argsPtr = this.write(args);
    this.error = undefined;
    this.result = undefined;
    let code: number;
    try {
      code = await callScript(
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
      results,
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
