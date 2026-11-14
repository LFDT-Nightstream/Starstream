import {
  ConsoleStdout,
  type Directory,
  OpenFile,
  File,
  PreopenDirectory,
  WASI,
  WASIProcExit,
  wasi as defs,
} from "@bjorn3/browser_wasi_shim";
import {
  promising,
  RuntimeWeb,
  suspending,
  type RuntimeWebExports,
} from "../../starstream-js/src/index";

const decoder = new TextDecoder();
const encoder = new TextEncoder();

interface Exports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  _start(): void;
}

/** Environment of a `starstream` command run by `runStarstream`. */
export interface StarstreamIo {
  /** Working directory, read and written in place. */
  dir: Directory;
  /** Environment variables. */
  env?: Record<string, string>;
  /** `fetch` the command sends HTTP requests with, by default the global one. */
  fetch?: typeof fetch;
  /** Write `data` to stdout. */
  stdout?(data: Uint8Array): void;
  /** Write `data` to stderr. */
  stderr?(data: Uint8Array): void;
}

/**
 * Run the `starstream` CLI, built for `wasm32-wasip1` as `module`, with the
 * arguments `args`, without the program name, in `io`, and return its exit
 * code. Each run gets its own instance. Requires JSPI.
 */
export async function runStarstream(
  module: WebAssembly.Module,
  args: string[],
  io: StarstreamIo,
): Promise<number> {
  const send = io.fetch ?? ((input, init) => fetch(input, init));
  const wasi = new WASI(
    ["starstream", ...args],
    Object.entries(io.env ?? {}).map(([name, value]) => `${name}=${value}`),
    [
      new OpenFile(new File([])),
      new ConsoleStdout((data) => io.stdout?.(data)),
      new ConsoleStdout((data) => io.stderr?.(data)),
      new PreopenDirectory("/", io.dir.contents),
    ],
    { debug: false },
  );
  const runtime = new RuntimeWeb();
  let exports!: Exports;
  const bytes = (ptr: number, len: number) =>
    new Uint8Array(exports.memory.buffer, ptr >>> 0, len);
  const view = () => new DataView(exports.memory.buffer);
  const str = (ptr: number) => {
    const mem = new Uint8Array(exports.memory.buffer);
    return decoder.decode(mem.subarray(ptr >>> 0, mem.indexOf(0, ptr >>> 0)));
  };
  let last = new Uint8Array();
  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: {
      ...wasi.wasiImport,
      path_readlink: (...args: number[]) => {
        const ret = wasi.wasiImport.path_readlink(...args);
        return ret === defs.ERRNO_NOTSUP ? defs.ERRNO_INVAL : ret;
      },
    },
    env: runtime.imports,
    starstream: {
      fetch: suspending(
        async (
          method: number,
          uri: number,
          headers: number,
          headersLen: number,
          body: number,
          bodyLen: number,
          len: number,
        ) => {
          const init: RequestInit = {
            method: str(method),
            headers: Array.from(
              { length: headersLen },
              (_, i): [string, string] => [
                str(view().getUint32((headers >>> 0) + 8 * i, true)),
                str(view().getUint32((headers >>> 0) + 8 * i + 4, true)),
              ],
            ),
            body: bodyLen > 0 ? bytes(body, bodyLen).slice() : undefined,
          };
          const target = str(uri);
          let status: number;
          try {
            const res = await send(target, init);
            last = new Uint8Array(await res.arrayBuffer());
            status = res.status;
          } catch (err) {
            last = encoder.encode(
              err instanceof Error ? err.message : String(err),
            );
            status = -1;
          }
          view().setUint32(len >>> 0, last.length, true);
          return status;
        },
      ),
      fetch_body: (dst: number) => {
        bytes(dst, last.length).set(last);
      },
    },
  });
  exports = instance.exports as unknown as Exports;
  runtime.init(exports);
  wasi.inst = { exports };
  try {
    await promising(exports._start)();
    return 0;
  } catch (err) {
    if (err instanceof WASIProcExit) {
      return err.code;
    }
    throw err;
  }
}
