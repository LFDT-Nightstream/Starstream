import starstreamSandboxWasm from "file-loader!../starstream_sandbox_web.wasm";
import { encode } from "cbor2";

export interface SandboxWorkerRequest {
  request_id: number;
  /** Absolute path -> contents of every `.star`/`.wasm` file. */
  files: Record<string, Uint8Array>;
  /** The workspace directory, scanned for `contract;` files. */
  root: string;
  /** The file compiled on its own when the workspace has no contracts. */
  entry: string;
}

/** One compiled contract. Outputs are missing if its compilation failed. */
export interface CompiledContract {
  /** Absolute path of the contract's entry file. */
  path: string;
  /** `path` relative to the workspace root, e.g. `main.star`. */
  name: string;
  wat?: string;
  coreWasm?: Uint8Array;
  wit?: string;
  componentWasm?: Uint8Array;
}

export type SandboxWorkerResponse = {
  request_id: number;
} & (
  | {
      type: "idle";
    }
  | {
      type: "log";
      level: number;
      target: string;
      body: string;
    }
  | {
      /**
       * Every contract the request compiled, in workspace order. Not sent if
       * the workspace failed to load or typecheck.
       */
      type: "contracts";
      contracts: CompiledContract[];
    }
);

// ----------------------------------------------------------------------------
// These interfaces should match `starstream-sandbox-web/src/lib.rs`.
interface SandboxInput {
  files: Record<string, Uint8Array>;
  root: string;
  entry: string;
}

interface SandboxWasmImports extends WebAssembly.ModuleImports {
  read_input(ptr: number, len: number): void;

  sandbox_log(
    level: number,
    target: number,
    target_len: number,
    body: number,
    body_len: number,
  ): void;
  set_contract(ptr: number, len: number): void;
  set_wat(ptr: number, len: number): void;
  set_core_wasm(ptr: number, len: number): void;
  set_wit(ptr: number, len: number): void;
  set_component_wasm(ptr: number, len: number): void;

  set_describe(ptr: number, len: number): void;
  set_call_result(ptr: number, len: number): void;
  set_storage(ptr: number, len: number): void;
  set_implemented(ptr: number, len: number): void;
  set_events(ptr: number, len: number): void;

  wasmtime_fiber_init(top: number, entry: number, arg0: number): void;
  wasmtime_fiber_switch(top: number): void;
  host_park(): void;
  host_unpark(): void;
}

interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  run(input_len: number): void;
}
// ----------------------------------------------------------------------------

let modulePromise: Promise<WebAssembly.WebAssemblyInstantiatedSource> | null =
  null;
export async function getWasmInstance(
  env: SandboxWasmImports,
): Promise<SandboxWasmExports> {
  if (modulePromise === null) {
    // First fetch gets instantiateStreaming privilege.
    modulePromise = WebAssembly.instantiateStreaming(
      fetch(starstreamSandboxWasm),
      { env },
    );
    const { instance } = await modulePromise;
    return instance.exports as unknown as SandboxWasmExports;
  } else {
    // Future fetches use synchronous instantiation of fetched module.
    const { module } = await modulePromise;
    return new WebAssembly.Instance(module, { env })
      .exports as unknown as SandboxWasmExports;
  }
}

function utf8(wasm: SandboxWasmExports, ptr: number, len: number): string {
  return new TextDecoder().decode(new Uint8Array(wasm.memory.buffer, ptr, len));
}

/** A copy of Wasm memory, since the buffer may be reused or grown. */
function bytes(wasm: SandboxWasmExports, ptr: number, len: number): Uint8Array {
  return new Uint8Array(wasm.memory.buffer, ptr, len).slice();
}

function send(r: SandboxWorkerResponse, opts?: WindowPostMessageOptions) {
  self.postMessage(r, opts);
}

self.onmessage = async function ({ data }: { data: SandboxWorkerRequest }) {
  const request_id = data.request_id;
  const input = encode({
    files: data.files,
    root: data.root,
    entry: data.entry,
  } satisfies SandboxInput);
  // Outputs arrive per contract, each after its `set_contract`.
  const contracts: CompiledContract[] = [];
  const current = () => contracts[contracts.length - 1];
  const wasm = await getWasmInstance({
    read_input(ptr, len) {
      new Uint8Array(wasm.memory.buffer, ptr, len).set(input);
    },
    sandbox_log(level, target, target_len, body, body_len) {
      send({
        request_id,
        type: "log",
        level,
        target: utf8(wasm, target, target_len),
        body: utf8(wasm, body, body_len),
      });
    },
    set_contract(ptr, len) {
      const path = utf8(wasm, ptr, len);
      const prefix = `${data.root}/`;
      const name = path.startsWith(prefix) ? path.slice(prefix.length) : path;
      contracts.push({ path, name });
    },
    set_wat(ptr, len) {
      current().wat = utf8(wasm, ptr, len);
    },
    set_core_wasm(ptr, len) {
      current().coreWasm = bytes(wasm, ptr, len);
    },
    set_wit(ptr, len) {
      current().wit = utf8(wasm, ptr, len);
    },
    set_component_wasm(ptr, len) {
      current().componentWasm = bytes(wasm, ptr, len);
    },
    // Only called by the Run tab's exports (`deploy`/`construct`/`call`/…),
    // which the run worker handles.
    set_describe() {},
    set_call_result() {},
    set_storage() {},
    set_implemented() {},
    set_events() {},
    // Fibers only run under the run worker's JSPI glue; compiling never
    // touches them.
    wasmtime_fiber_init() {},
    wasmtime_fiber_switch() {},
    host_park() {},
    host_unpark() {},
  });
  try {
    wasm.run(input.length);
  } catch (crash) {
    send({
      request_id,
      type: "log",
      level: 1,
      target: "sandbox",
      body: String(crash),
    });
  }
  if (contracts.length > 0) {
    send({ request_id, type: "contracts", contracts });
  }
  send({
    request_id,
    type: "idle",
  });
};
