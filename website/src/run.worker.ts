// WebWorker that hosts the sandbox "Run" execution environment.
// Unlike the compile worker, this worker keeps a single Wasm instance alive so
// the wasmtime engine and the deployed component persist across messages.

import starstreamSandboxWasm from "file-loader!../starstream_sandbox_web.wasm";

// ----------------------------------------------------------------------------
// Subset of wit-parser's JSON serialization of a resolved WIT document
// (`wasm-tools component wit --json`) that the Run tab consumes.

/** A primitive type name (e.g. `"s64"`) or an index into `WitResolve["types"]`. */
export type WitType = string | number;

export interface WitParam {
  name: string;
  type: WitType;
}

export interface WitFunction {
  name: string;
  params: WitParam[];
  result?: WitType | null;
}

export type WitWorldItem =
  | { function: WitFunction }
  | { interface: number }
  | { type: WitType };

export interface WitWorld {
  name: string;
  imports: Record<string, WitWorldItem>;
  exports: Record<string, WitWorldItem>;
}

export interface WitInterface {
  name: string | null;
  functions: Record<string, WitFunction>;
}

/** Kinds without payload (e.g. resources) serialize as plain strings. */
export type WitTypeDefKind =
  | string
  | {
      record?: { fields: WitParam[] };
      variant?: { cases: { name: string; type?: WitType | null }[] };
      enum?: { cases: { name: string }[] };
      flags?: { flags: { name: string }[] };
      tuple?: { types: WitType[] };
      option?: WitType;
      list?: WitType;
      result?: { ok: WitType | null; err: WitType | null };
      /** A type alias. */
      type?: WitType;
    };

export interface WitTypeDef {
  name: string | null;
  kind: WitTypeDefKind;
}

export interface WitResolve {
  worlds: WitWorld[];
  interfaces: WitInterface[];
  types: WitTypeDef[];
}
// ----------------------------------------------------------------------------

export type RunWorkerRequest = {
  request_id: number;
} & (
  | {
      type: "deploy";
      component: Uint8Array;
    }
  | {
      type: "call";
      /** Name of the exported function to call. */
      name: string;
      /** One WAVE-encoded value per function parameter. */
      args: string[];
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
      wit: WitResolve;
    }
  | {
      type: "called";
      /** The WAVE-encoded result, absent for functions with no result. */
      result: string | undefined;
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
  set_wat(ptr: number, len: number): void;
  set_core_wasm(ptr: number, len: number): void;
  set_wit(ptr: number, len: number): void;
  set_component_wasm(ptr: number, len: number): void;

  set_deployed_wit_json(ptr: number, len: number): void;
  set_call_result(ptr: number, len: number): void;
}

interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  deploy(input_len: number): number;
  call(input_len: number): number;
}
// ----------------------------------------------------------------------------

// State the Wasm imports below read and write; set per incoming message.
let input = new Uint8Array();
let request_id = 0;
let deployedWit: WitResolve | undefined;
let callResult: string | undefined;

let wasm: SandboxWasmExports;
let wasmPromise: Promise<SandboxWasmExports> | null = null;
function getWasmInstance(): Promise<SandboxWasmExports> {
  wasmPromise ??= WebAssembly.instantiateStreaming(
    fetch(starstreamSandboxWasm),
    {
      env: {
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
        set_wat() {},
        set_core_wasm() {},
        set_wit() {},
        set_component_wasm() {},
        set_deployed_wit_json(ptr, len) {
          deployedWit = JSON.parse(utf8(ptr, len)) as WitResolve;
        },
        set_call_result(ptr, len) {
          callResult = utf8(ptr, len);
        },
      } satisfies SandboxWasmImports,
    },
  ).then(({ instance }) => {
    wasm = instance.exports as unknown as SandboxWasmExports;
    return wasm;
  });
  return wasmPromise;
}

function utf8(ptr: number, len: number): string {
  return new TextDecoder().decode(new Uint8Array(wasm.memory.buffer, ptr, len));
}

function send(r: RunWorkerResponse, opts?: WindowPostMessageOptions) {
  self.postMessage(r, opts);
}

self.onmessage = async function ({ data }: { data: RunWorkerRequest }) {
  request_id = data.request_id;
  const wasm = await getWasmInstance();
  if (data.type === "deploy") {
    input = data.component;
    deployedWit = undefined;
    try {
      if (wasm.deploy(input.length) >= 0 && deployedWit) {
        send({
          request_id,
          type: "deployed",
          wit: deployedWit,
        });
      }
    } catch (crash) {
      send({
        request_id,
        type: "log",
        level: 1,
        target: "run",
        body: String(crash),
      });
    }
  } else if (data.type === "call") {
    input = new TextEncoder().encode(
      JSON.stringify({ name: data.name, args: data.args }),
    );
    callResult = undefined;
    try {
      if (wasm.call(input.length) >= 0) {
        send({
          request_id,
          type: "called",
          result: callResult,
        });
      }
    } catch (crash) {
      send({
        request_id,
        type: "log",
        level: 1,
        target: "run",
        body: String(crash),
      });
    }
  }
  send({
    request_id,
    type: "idle",
  });
};
