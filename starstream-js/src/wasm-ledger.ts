import {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
} from "./runtime-web.ts";

interface Exports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  ledger_alloc(len: number): number;
  ledger_import(wasm: number, len: number): number;
  ledger_start(config: number, len: number): number;
  ledger_poll(): number;
  ledger_handle(id: number, head: number, len: number): void;
  ledger_body_data(read: number, data: number, len: number): void;
  ledger_body_end(read: number): void;
  ledger_body_error(read: number): void;
}

interface Pending {
  body: ReadableStreamDefaultReader<Uint8Array> | undefined;
  resolve: (res: Response) => void;
  reject: (err: unknown) => void;
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

/** Configuration of a `WasmLedger`. */
export interface WasmLedgerOptions {
  /** Network identifier. */
  network: string;
  /** Ed25519 public key of the admin account. */
  admin: Uint8Array;
  /** Coordination script calls producing the genesis outputs. */
  genesis?: GenesisScript[];
  /** Contracts the genesis scripts refer to. */
  imports?: Uint8Array[];
  /** Maximum amount of concurrent requests. */
  maxRequests?: number;
  /** Fuel available to each UTXO method call over `/rpc`. */
  fuel?: number;
  /** Time a request body may stall before the request fails, in seconds. */
  ioTimeout?: number;
  /** Cardano block height reported to contracts (`cardano#block-height`). */
  cardanoBlockHeight?: number;
  /** Cardano current slot reported to contracts (`cardano#current-slot`). */
  cardanoCurrentSlot?: number;
}

const decoder = new TextDecoder();
const encoder = new TextEncoder();

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
  private polling = false;
  private woken = false;

  private constructor() {}

  private async instantiate(module: WebAssembly.Module): Promise<void> {
    this.runtime = new RuntimeWeb();
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
          const request = this.requests.get(id)!;
          this.requests.delete(id);
          void request.body?.cancel();
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
          void this.read(this.requests.get(id)?.body, read);
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
        genesis: { scripts: genesis ?? [] },
        max_requests: maxRequests,
        fuel,
        io_timeout: ioTimeout,
        cardano_block_height: cardanoBlockHeight,
        cardano_current_slot: cardanoCurrentSlot,
      }),
    );
    const ptr = ledger.write(config);
    await ledger.call(() => ledger.startLedger(ptr, config.length));
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
    const { pathname, search } = new URL(request.url);
    let head = `${request.method} ${pathname}${search}\n`;
    request.headers.forEach((value, name) => {
      head += `${name}: ${value}\n`;
    });
    const id = this.nextId;
    this.nextId = (this.nextId + 1) | 0;
    const response = new Promise<Response>((resolve, reject) => {
      this.requests.set(id, {
        body: request.body?.getReader(),
        resolve,
        reject,
      });
    });
    const encoded = Uint8Array.from(head, (c) => c.charCodeAt(0));
    this.exports.ledger_handle(id, this.write(encoded), encoded.length);
    return response;
  };

  private wake(): void {
    if (this.polling) {
      this.woken = true;
      return;
    }
    this.polling = true;
    queueMicrotask(() => void this.drive());
  }

  private async drive(): Promise<void> {
    try {
      do {
        this.woken = false;
        await this.call(() => this.poll());
      } while (this.woken);
    } catch (err) {
      this.failure = err;
      this.runtime.dispose();
      for (const request of this.requests.values()) {
        void request.body?.cancel();
        request.reject(err);
      }
      this.requests.clear();
      return;
    }
    this.polling = false;
  }

  private async read(
    body: ReadableStreamDefaultReader<Uint8Array> | undefined,
    read: number,
  ): Promise<void> {
    let chunk: ReadableStreamReadResult<Uint8Array> | undefined;
    try {
      chunk = await (body?.read() ?? { done: true, value: undefined });
    } catch {
      chunk = undefined;
    }
    if (this.failure) {
      return;
    }
    if (!chunk) {
      this.exports.ledger_body_error(read);
      return;
    }
    if (chunk.done) {
      this.exports.ledger_body_end(read);
      return;
    }
    const ptr = this.write(chunk.value);
    this.exports.ledger_body_data(read, ptr, chunk.value.length);
  }

  private async call(f: () => Promise<number>): Promise<void> {
    this.error = undefined;
    let code: number;
    try {
      code = await f();
    } catch (err) {
      throw this.error ? new Error(this.error, { cause: err }) : err;
    }
    if (code !== 0) {
      throw new Error(this.error);
    }
  }

  private bytes(ptr: number, len: number): Uint8Array<ArrayBuffer> {
    return new Uint8Array(this.exports.memory.buffer, ptr, len);
  }

  private write(data: Uint8Array): number {
    const ptr = this.exports.ledger_alloc(data.length);
    this.bytes(ptr, data.length).set(data);
    return ptr;
  }
}
