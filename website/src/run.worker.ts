// WebWorker that hosts the sandbox "Run" execution environment.
// Unlike the compile worker, this worker keeps a single Wasm instance alive so
// the wasmtime engine and the deployed component persist across messages.

import starstreamSandboxWasm from "file-loader!../starstream_sandbox_web.wasm";
import { encode } from "cbor2";

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
      /** Sha256 hex digest of `component`, identifying the deployment. */
      digest: string;
      component: Uint8Array;
    }
  | {
      type: "instantiate";
      /** Digest of the deployment to create an instance (UTXO) of. */
      digest: string;
    }
  | {
      type: "call";
      /** Digest of the deployment to call. */
      digest: string;
      /** Number of the instance to call, as reported by "instantiated". */
      instance: number;
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
      /** Digest identifying the deployment, as passed to "deploy". */
      digest: string;
      wit: WitResolve;
    }
  | {
      type: "instantiated";
      /** Digest of the deployment the instance belongs to. */
      digest: string;
      /** Number of the new instance, counted per deployment. */
      instance: number;
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
  instantiate(digest: number): number;
  call(input_len: number): number;
}
// ----------------------------------------------------------------------------

// State the Wasm imports below read and write; set per incoming message.
let input = new Uint8Array();
let request_id = 0;
let deployedWit: WitResolve | undefined;
let callResult: string | undefined;

// The numeric digest each contract is known by to the Wasm, keyed by the
// sha256 hex digest computed by the page.
const digestNumbers = new Map<string, number>();

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
    // Assign the digest a number; re-deploying the same digest reuses it.
    let digest = digestNumbers.get(data.digest);
    if (digest === undefined) {
      digest = digestNumbers.size;
      digestNumbers.set(data.digest, digest);
    }
    // CBOR-encoded `DeployInput`.
    input = encode({
      digest,
      wasm: data.component,
    });
    deployedWit = undefined;
    try {
      if (wasm.deploy(input.length) >= 0 && deployedWit) {
        send({
          request_id,
          type: "deployed",
          digest: data.digest,
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
  } else if (data.type === "instantiate") {
    const digest = digestNumbers.get(data.digest);
    try {
      if (digest === undefined) {
        throw new Error(`no contract deployed with digest ${data.digest}`);
      }
      const instance = wasm.instantiate(digest);
      if (instance >= 0) {
        send({
          request_id,
          type: "instantiated",
          digest: data.digest,
          instance,
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
    const digest = digestNumbers.get(data.digest);
    callResult = undefined;
    try {
      if (digest === undefined) {
        throw new Error(`no contract deployed with digest ${data.digest}`);
      }
      // JSON-encoded `CallInput`.
      input = new TextEncoder().encode(
        JSON.stringify({
          digest,
          instance: data.instance,
          function: data.name,
          args: data.args,
        }),
      );
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
