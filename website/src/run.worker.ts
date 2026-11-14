// WebWorker hosting the sandbox "Run" environment. Unlike the compile worker,
// it keeps one Wasm instance alive so the wasmtime engine and deployed
// contracts persist across messages.
//
// Protocol (see `starstream-sandbox-web/src/lib.rs`): deploy a component, mint
// UTXOs via `[static]` constructors, then invoke `[method]`s on live handles.

import starstreamSandboxWasm from "file-loader!../starstream_sandbox_web.wasm";
import { encode } from "cbor2";
import {
  promising,
  RuntimeWeb,
  type RuntimeWebExports,
} from "../../starstream-js/src/runtime-web";

// ----------------------------------------------------------------------------
// The contract description produced by `describe` in
// `starstream-sandbox-web/src/lib.rs`.

/** A scalar type name (`u64`, `bool`, …) or `"json"` (entered as raw JSON). */
export type Kind = string;

export interface DescribeParam {
  name: string;
  kind: Kind;
}

/** A constructor or method; `export` is the WIT name, `label` its tail. */
export interface DescribeFunc {
  export: string;
  label: string;
  params: DescribeParam[];
}

export interface DescribeInstance {
  name: string;
  resource: string;
  constructors: DescribeFunc[];
  methods: DescribeFunc[];
  /** The `storage` record's fields, or `null` if the resource has none. */
  storage: DescribeParam[] | null;
}

export interface Describe {
  instances: DescribeInstance[];
}

export interface AbiEvent {
  instance: string;
  name: string;
  params: unknown[];
}
// ----------------------------------------------------------------------------

export type RunWorkerRequest = {
  request_id: number;
} & (
  | {
      type: "deploy";
      /** The contract's name, echoed back in the `deployed` response. */
      name: string;
      /** Sha256 hex digest of `component`, identifying the deployment. */
      digest: string;
      component: Uint8Array;
    }
  | {
      type: "construct";
      digest: string;
      instance: string;
      ctor: string;
      args: unknown[];
    }
  | {
      type: "call";
      digest: string;
      handle: number;
      method: string;
      /** One JSON value per parameter, excluding the `self` receiver. */
      args: unknown[];
    }
  | {
      type: "storageGet";
      digest: string;
      handle: number;
    }
  | {
      type: "implementedMethods";
      digest: string;
      handle: number;
    }
  | {
      type: "drop";
      digest: string;
      handle: number;
    }
  | {
      type: "setCardano";
      digest: string;
      blockHeight: number;
      currentSlot: number;
    }
);

export type RunWorkerResponse = {
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
      type: "deployed";
      name: string;
      digest: string;
      describe: Describe;
    }
  | {
      type: "deploy_failed";
      digest: string;
    }
  | {
      type: "constructed";
      digest: string;
      instance: string;
      handle: number;
      events: AbiEvent[];
    }
  | {
      type: "construct_failed";
      digest: string;
      instance: string;
    }
  | {
      type: "called";
      results: unknown[];
      events: AbiEvent[];
    }
  | {
      type: "storage";
      digest: string;
      handle: number;
      storage: Record<string, unknown>;
    }
  | {
      type: "implemented";
      digest: string;
      handle: number;
      methods: string[];
    }
  | {
      type: "dropped";
      digest: string;
      handle: number;
    }
);

// ----------------------------------------------------------------------------
// These interfaces should match `starstream-sandbox-web/src/lib.rs`.
interface SandboxWasmImports extends WebAssembly.ModuleImports {
  read_input(ptr: number, len: number): void;

  sandbox_log(
    level: number,
    target: number,
    target_len: number,
    body: number,
    body_len: number,
  ): void;
  set_contract(ptr: number, len: number, standalone: number): void;
  set_wat(ptr: number, len: number): void;
  set_core_wasm(ptr: number, len: number): void;
  set_wit(ptr: number, len: number): void;
  set_component_wasm(ptr: number, len: number): void;

  set_describe(ptr: number, len: number): void;
  set_call_result(ptr: number, len: number): void;
  set_storage(ptr: number, len: number): void;
  set_implemented(ptr: number, len: number): void;
  set_events(ptr: number, len: number): void;
}

interface SandboxWasmExports extends RuntimeWebExports {
  memory: WebAssembly.Memory;
  deploy(input_len: number): number;
  construct(input_len: number): number;
  call(input_len: number): number;
  storage_get(digest: number, handle: number): number;
  implemented_methods(digest: number, handle: number): number;
  drop_resource(digest: number, handle: number): number;
  set_cardano(
    digest: number,
    block_height: bigint,
    current_slot: bigint,
  ): number;
}

// The exports the worker actually calls. Exports that may run guest code are
// wrapped in `WebAssembly.promising` so the fibers underneath can suspend the
// activation; they resolve to the export's plain return value.
interface RunWasm {
  memory: WebAssembly.Memory;
  deploy(input_len: number): number;
  construct(input_len: number): Promise<number>;
  call(input_len: number): Promise<number>;
  storage_get(digest: number, handle: number): Promise<number>;
  implemented_methods(digest: number, handle: number): number;
  drop_resource(digest: number, handle: number): Promise<number>;
  set_cardano(
    digest: number,
    block_height: bigint,
    current_slot: bigint,
  ): number;
}
// ----------------------------------------------------------------------------

// State the Wasm imports below read and write; set per incoming message.
let input = new Uint8Array();
let request_id = 0;
let describeJson: Describe | undefined;
let callResult: unknown[] | undefined;
let storageJson: Record<string, unknown> | undefined;
let implementedJson: string[] | undefined;
let eventsJson: AbiEvent[] = [];

// The numeric digest each contract is known by to the Wasm, keyed by the
// sha256 hex digest computed by the page.
const digestNumbers = new Map<string, number>();

let wasm: RunWasm;
let wasmPromise: Promise<RunWasm> | null = null;
function getWasmInstance(): Promise<RunWasm> {
  if (wasmPromise) {
    return wasmPromise;
  }
  const runtime = new RuntimeWeb();
  wasmPromise = WebAssembly.instantiateStreaming(fetch(starstreamSandboxWasm), {
    env: {
      ...runtime.imports,
      read_input(ptr, len) {
        new Uint8Array(wasm.memory.buffer, ptr, len).set(input);
      },
      sandbox_log(level, target, target_len, body, body_len) {
        send({
          request_id,
          type: "log",
          level,
          target: utf8(target, target_len),
          body: utf8(body, body_len),
        });
      },
      set_contract() {},
      set_wat() {},
      set_core_wasm() {},
      set_wit() {},
      set_component_wasm() {},
      set_describe(ptr, len) {
        describeJson = JSON.parse(utf8(ptr, len)) as Describe;
      },
      set_call_result(ptr, len) {
        callResult = JSON.parse(utf8(ptr, len)) as unknown[];
      },
      set_storage(ptr, len) {
        storageJson = JSON.parse(utf8(ptr, len)) as Record<string, unknown>;
      },
      set_implemented(ptr, len) {
        implementedJson = JSON.parse(utf8(ptr, len)) as string[];
      },
      set_events(ptr, len) {
        eventsJson = JSON.parse(utf8(ptr, len)) as AbiEvent[];
      },
    } satisfies SandboxWasmImports,
  }).then(({ instance }) => {
    const exports = instance.exports as unknown as SandboxWasmExports;
    runtime.init(exports);
    wasm = {
      memory: exports.memory,
      deploy: exports.deploy,
      construct: promising(exports.construct),
      call: promising(exports.call),
      storage_get: promising(exports.storage_get),
      implemented_methods: exports.implemented_methods,
      drop_resource: promising(exports.drop_resource),
      set_cardano: exports.set_cardano,
    };
    return wasm;
  });
  wasmPromise.catch(() => {
    wasmPromise = null;
  });
  return wasmPromise;
}

const decoder = new TextDecoder();
const encoder = new TextEncoder();

function utf8(ptr: number, len: number): string {
  return decoder.decode(new Uint8Array(wasm.memory.buffer, ptr, len));
}

function send(r: RunWorkerResponse, opts?: WindowPostMessageOptions) {
  self.postMessage(r, opts);
}

// Look up the numeric digest the Wasm knows a deployment by, throwing if it was
// never deployed in this worker.
function digestNumber(digest: string): number {
  const n = digestNumbers.get(digest);
  if (n === undefined) {
    throw new Error(`no contract deployed with digest ${digest}`);
  }
  return n;
}

// Requests run strictly one after another: a guest call suspends the worker's
// activation at `await`s, and only one root activation may be in flight.
let chain: Promise<void> = Promise.resolve();
self.onmessage = function ({ data }: { data: RunWorkerRequest }) {
  chain = chain.then(() => serve(data)).catch(() => {});
};

async function serve(data: RunWorkerRequest) {
  request_id = data.request_id;
  try {
    const wasm = await getWasmInstance();
    if (data.type === "deploy") {
      // Assign the digest a number; re-deploying the same digest reuses it.
      let digest = digestNumbers.get(data.digest);
      if (digest === undefined) {
        digest = digestNumbers.size;
        digestNumbers.set(data.digest, digest);
      }
      // CBOR-encoded `DeployInput`.
      input = encode({ digest, wasm: data.component });
      describeJson = undefined;
      if (wasm.deploy(input.length) >= 0 && describeJson) {
        send({
          request_id,
          type: "deployed",
          name: data.name,
          digest: data.digest,
          describe: describeJson,
        });
      } else {
        // The error itself was already reported as a "log" response.
        send({ request_id, type: "deploy_failed", digest: data.digest });
      }
    } else if (data.type === "construct") {
      const digest = digestNumber(data.digest);
      // JSON-encoded `ConstructInput`.
      input = encoder.encode(
        JSON.stringify({
          digest,
          instance: data.instance,
          constructor: data.ctor,
          args: data.args,
        }),
      );
      eventsJson = [];
      const handle = await wasm.construct(input.length);
      if (handle >= 0) {
        send({
          request_id,
          type: "constructed",
          digest: data.digest,
          instance: data.instance,
          handle,
          events: eventsJson,
        });
      } else {
        send({
          request_id,
          type: "construct_failed",
          digest: data.digest,
          instance: data.instance,
        });
      }
    } else if (data.type === "call") {
      const digest = digestNumber(data.digest);
      // JSON-encoded `CallInput`.
      input = encoder.encode(
        JSON.stringify({
          digest,
          handle: data.handle,
          method: data.method,
          args: data.args,
        }),
      );
      callResult = undefined;
      eventsJson = [];
      if ((await wasm.call(input.length)) >= 0) {
        send({
          request_id,
          type: "called",
          results: callResult ?? [],
          events: eventsJson,
        });
      }
    } else if (data.type === "storageGet") {
      const digest = digestNumber(data.digest);
      storageJson = undefined;
      if ((await wasm.storage_get(digest, data.handle)) >= 0 && storageJson) {
        send({
          request_id,
          type: "storage",
          digest: data.digest,
          handle: data.handle,
          storage: storageJson,
        });
      }
    } else if (data.type === "implementedMethods") {
      const digest = digestNumber(data.digest);
      implementedJson = undefined;
      if (
        wasm.implemented_methods(digest, data.handle) >= 0 &&
        implementedJson
      ) {
        send({
          request_id,
          type: "implemented",
          digest: data.digest,
          handle: data.handle,
          methods: implementedJson,
        });
      }
    } else if (data.type === "drop") {
      const digest = digestNumber(data.digest);
      if ((await wasm.drop_resource(digest, data.handle)) >= 0) {
        send({
          request_id,
          type: "dropped",
          digest: data.digest,
          handle: data.handle,
        });
      }
    } else if (data.type === "setCardano") {
      const digest = digestNumber(data.digest);
      wasm.set_cardano(
        digest,
        BigInt(Math.trunc(data.blockHeight)),
        BigInt(Math.trunc(data.currentSlot)),
      );
    } else {
      data satisfies never;
    }
  } catch (crash) {
    send({
      request_id,
      type: "log",
      level: 1,
      target: "run",
      body: String(crash),
    });
    if (crash instanceof WebAssembly.RuntimeError) {
      // A trap leaves the runtime's state inconsistent (a Rust panic does not
      // unwind); start over with a fresh instance. Deployments are lost.
      wasmPromise = null;
      digestNumbers.clear();
      send({
        request_id,
        type: "log",
        level: 1,
        target: "run",
        body: "the runtime crashed and was restarted; deploy again to continue",
      });
    }
    if (data.type === "deploy") {
      send({ request_id, type: "deploy_failed", digest: data.digest });
    } else if (data.type === "construct") {
      send({
        request_id,
        type: "construct_failed",
        digest: data.digest,
        instance: data.instance,
      });
    }
  }
  send({ request_id, type: "idle" });
}
