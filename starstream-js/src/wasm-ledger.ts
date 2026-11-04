import {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
} from "./runtime-web.ts";

const { rawJSON } = JSON as unknown as { rawJSON(text: string): unknown };
const decoder = new TextDecoder();
const encoder = new TextEncoder();
const MAX_BODY_CHUNK = 1 << 16;

interface Exports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  ledger_alloc(len: number): number;
  ledger_import(wasm: number, len: number): number;
  ledger_start(config: number, len: number): number;
  ledger_poll(): number;
  ledger_handle(id: number, head: number, len: number): void;
  ledger_abort(id: number): void;
  ledger_body_data(read: number, data: number, len: number): void;
  ledger_body_end(read: number): void;
  ledger_body_error(read: number): void;
}

interface Pending {
  body: ReadableStreamDefaultReader<Uint8Array> | undefined;
  rest: Uint8Array | undefined;
  resolve: (res: Response) => void;
  reject: (err: unknown) => void;
  signal: AbortSignal;
  abort: () => void;
}

/** Coordination script call producing genesis outputs. */
export interface GenesisScript {
  /** Digest of the contract exporting the coordination script. */
  contract: string;
  /** Coordination script to call. */
  script: string;
  /** WAVE-encoded coordination script arguments. */
  args?: string[];
}

/** Definition of a ledger's genesis. */
export interface Genesis {
  /** Coordination script calls producing the genesis outputs. */
  scripts?: GenesisScript[];
}

/** Configuration of a `WasmLedger`. */
export interface WasmLedgerOptions {
  /** Network identifier. */
  network: string;
  /** Ed25519 public key of the admin account. */
  admin: Uint8Array;
  /** Genesis of the ledger, checked by the ledger as given. */
  genesis?: Genesis;
  /** Contracts the genesis scripts refer to. */
  imports?: Uint8Array[];
  /** Maximum amount of concurrent requests. */
  maxRequests?: number;
  /** Fuel available to each UTXO method call over `/rpc`. */
  fuel?: bigint;
  /** Time a request body may stall before the request fails, in whole seconds. */
  ioTimeout?: number;
  /** Cardano block height reported to contracts (`cardano#block-height`). */
  cardanoBlockHeight?: bigint;
  /** Cardano current slot reported to contracts (`cardano#current-slot`). */
  cardanoCurrentSlot?: bigint;
}

function latin1(bytes: Uint8Array): string {
  return Array.from(bytes, (b) => String.fromCharCode(b)).join("");
}

/**
 * Ledger running in this JS realm, as an instance of the
 * `starstream-ledger-web` Wasm module, without any network access, handling
 * requests concurrently. Requires JSPI (`WebAssembly.Suspending`).
 */
export class WasmLedger {
  private exports!: Exports;
  private runtime!: RuntimeWeb;
  private startLedger!: (config: number, len: number) => Promise<number>;
  private poll!: () => Promise<number>;
  private error: string | undefined;
  private failure: unknown;
  private requests = new Map<number, Pending>();
  private nextId = 0;
  private busy = false;
  private woken = false;

  private constructor() {}

  private async instantiate(module: WebAssembly.Module): Promise<void> {
    this.runtime = new RuntimeWeb((err) => this.fail(this.trapError(err)));
    const instance = await WebAssembly.instantiate(module, {
      env: {
        ...this.runtime.imports,
        ledger_error: (ptr: number, len: number) => {
          this.error = decoder.decode(this.bytes(ptr, len));
        },
        ledger_response: (
          id: number,
          status: number,
          head: number,
          headLen: number,
          body: number,
          bodyLen: number,
        ) => {
          const request = this.take(id);
          if (!request) {
            return;
          }
          try {
            request.resolve(
              new Response(
                bodyLen > 0 ? this.bytes(body, bodyLen).slice() : null,
                {
                  status,
                  headers: latin1(this.bytes(head, headLen))
                    .split("\n")
                    .filter((line) => line)
                    .map((line): [string, string] => {
                      const i = line.indexOf(": ");
                      return [line.slice(0, i), line.slice(i + 2)];
                    }),
                },
              ),
            );
          } catch (err) {
            request.reject(err);
          }
        },
        ledger_wake: () => {
          this.wake();
        },
        ledger_body_read: (id: number, read: number) => {
          void this.read(id, read);
        },
      },
    });
    this.exports = instance.exports as unknown as Exports;
    this.runtime.init(this.exports);
    this.startLedger = promising(this.exports.ledger_start);
    this.poll = promising(this.exports.ledger_poll);
  }

  /**
   * Start a ledger from the compiled `starstream-ledger-web` module. Each
   * ledger runs in its own instance of the module.
   */
  static async start(
    module: WebAssembly.Module,
    {
      network,
      admin,
      genesis,
      imports,
      maxRequests,
      fuel,
      ioTimeout,
      cardanoBlockHeight,
      cardanoCurrentSlot,
    }: WasmLedgerOptions,
  ): Promise<WasmLedger> {
    const ledger = new WasmLedger();
    await ledger.instantiate(module);
    ledger.busy = true;
    for (const wasm of imports ?? []) {
      const ptr = ledger.write(wasm);
      await ledger.call(async () =>
        ledger.exports.ledger_import(ptr, wasm.length),
      );
    }
    const config = encoder.encode(
      JSON.stringify({
        network,
        admin: Array.from(admin)
          .map((b) => b.toString(16).padStart(2, "0"))
          .join(""),
        genesis: genesis ?? {},
        max_requests: maxRequests,
        fuel: fuel === undefined ? undefined : rawJSON(String(fuel)),
        io_timeout: ioTimeout,
        cardano_block_height:
          cardanoBlockHeight === undefined
            ? undefined
            : rawJSON(String(cardanoBlockHeight)),
        cardano_current_slot:
          cardanoCurrentSlot === undefined
            ? undefined
            : rawJSON(String(cardanoCurrentSlot)),
      }),
    );
    const ptr = ledger.write(config);
    await ledger.call(() => ledger.startLedger(ptr, config.length));
    ledger.busy = false;
    if (ledger.woken) {
      ledger.wake();
    }
    return ledger;
  }

  /**
   * Handle a request to the HTTP API of the ledger, for use as the `fetch`
   * option of `Ledger`.
   */
  readonly fetch: typeof fetch = async (input, init) => {
    if (this.failure) {
      throw this.failure;
    }
    const request = new Request(input, init);
    const { signal } = request;
    signal.throwIfAborted();
    const { pathname, search } = new URL(request.url);
    let head = `${request.method} ${pathname}${search}\n`;
    request.headers.forEach((value, name) => {
      head += `${name}: ${value}\n`;
    });
    const id = this.nextId;
    this.nextId = (this.nextId + 1) | 0;
    const response = new Promise<Response>((resolve, reject) => {
      const abort = () => {
        this.take(id);
        reject(signal.reason);
        this.guard(() => this.exports.ledger_abort(id));
      };
      signal.addEventListener("abort", abort);
      this.requests.set(id, {
        body: request.body?.getReader(),
        rest: undefined,
        resolve,
        reject,
        signal,
        abort,
      });
    });
    const encoded = Uint8Array.from(head, (c) => c.charCodeAt(0));
    this.guard(() =>
      this.exports.ledger_handle(id, this.write(encoded), encoded.length),
    );
    return response;
  };

  /** Stop the ledger, failing pending and later requests. */
  dispose(): void {
    this.fail(new Error("ledger disposed"));
  }

  private take(id: number): Pending | undefined {
    const request = this.requests.get(id);
    if (request) {
      this.requests.delete(id);
      request.signal.removeEventListener("abort", request.abort);
      request.body?.cancel().catch(() => {});
    }
    return request;
  }

  private fail(err: unknown): void {
    if (this.failure) {
      return;
    }
    this.failure = err;
    this.runtime.dispose();
    for (const [id, request] of this.requests) {
      this.take(id);
      request.reject(err);
    }
  }

  private guard(f: () => void): void {
    if (this.failure) {
      return;
    }
    try {
      f();
    } catch (err) {
      this.fail(this.trapError(err));
    }
  }

  private trapError(cause: unknown): unknown {
    return this.error ? new Error(this.error, { cause }) : cause;
  }

  private wake(): void {
    if (this.failure) {
      return;
    }
    if (this.busy) {
      this.woken = true;
      return;
    }
    this.busy = true;
    queueMicrotask(() => void this.drive());
  }

  private async drive(): Promise<void> {
    try {
      while (!this.failure) {
        this.woken = false;
        await this.call(() => this.poll());
        if (!this.woken) {
          break;
        }
      }
    } catch (err) {
      this.fail(err);
      return;
    }
    this.busy = false;
  }

  private async read(id: number, read: number): Promise<void> {
    let chunk: ReadableStreamReadResult<Uint8Array> | undefined;
    try {
      const request = this.requests.get(id);
      chunk = request?.rest?.length
        ? { done: false, value: request.rest }
        : await (request?.body?.read() ?? { done: true, value: undefined });
    } catch (err) {
      this.take(id)?.reject(err);
      chunk = undefined;
    }
    if (
      chunk &&
      !chunk.done &&
      Object.prototype.toString.call(chunk.value) !== "[object Uint8Array]"
    ) {
      this.take(id)?.reject(
        new TypeError("request body chunk is not a `Uint8Array`"),
      );
    }
    const request = this.requests.get(id);
    this.guard(() => {
      if (!request || !chunk) {
        this.exports.ledger_body_error(read);
      } else if (chunk.done) {
        this.exports.ledger_body_end(read);
      } else {
        const data = chunk.value.subarray(0, MAX_BODY_CHUNK);
        request.rest = chunk.value.subarray(data.length);
        const ptr = this.write(data);
        this.exports.ledger_body_data(read, ptr, data.length);
      }
    });
  }

  private async call(f: () => Promise<number>): Promise<void> {
    this.error = undefined;
    let code: number;
    try {
      code = await f();
    } catch (err) {
      throw this.trapError(err);
    }
    if (code !== 0) {
      throw new Error(this.error ?? "ledger call failed");
    }
  }

  private bytes(ptr: number, len: number): Uint8Array<ArrayBuffer> {
    return new Uint8Array(this.exports.memory.buffer, ptr >>> 0, len);
  }

  private write(data: Uint8Array): number {
    const ptr = this.exports.ledger_alloc(data.length);
    this.bytes(ptr, data.length).set(data);
    return ptr;
  }
}
