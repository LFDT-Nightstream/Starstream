import starstreamSandboxWasm from "file-loader!../starstream_sandbox_web.wasm";
import { encode } from "cbor2";

export interface SandboxWorkerRequest {
  request_id: number;
  code: string;
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
      type: "wat";
      wat: string;
    }
  | {
      type: "core_wasm";
      bytes: Uint8Array;
    }
  | {
      type: "wit";
      wit: string;
    }
  | {
      type: "component_wasm";
      bytes: Uint8Array;
    }
);

// ----------------------------------------------------------------------------
// These interfaces should match `starsteam_sandbox/src/main.rs`.
interface SandboxInput {
  code: string;
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
  set_wat(ptr: number, len: number): void;
  set_core_wasm(ptr: number, len: number): void;
  set_wit(ptr: number, len: number): void;
  set_component_wasm(ptr: number, len: number): void;
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

function send(r: SandboxWorkerResponse, opts?: WindowPostMessageOptions) {
  self.postMessage(r, opts);
}

self.onmessage = async function ({ data }: { data: SandboxWorkerRequest }) {
  const request_id = data.request_id;
  const input = encode({
    code: data.code,
  } satisfies SandboxInput);
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
    set_wat(ptr, len) {
      send({
        request_id,
        type: "wat",
        wat: utf8(wasm, ptr, len),
      });
    },
    set_core_wasm(ptr, len) {
      send({
        request_id,
        type: "core_wasm",
        bytes: new Uint8Array(wasm.memory.buffer, ptr, len),
      });
    },
    set_wit(ptr, len) {
      send({
        request_id,
        type: "wit",
        wit: utf8(wasm, ptr, len),
      });
    },
    set_component_wasm(ptr, len) {
      send({
        request_id,
        type: "component_wasm",
        bytes: new Uint8Array(wasm.memory.buffer, ptr, len),
      });
    },
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
  send({
    request_id,
    type: "idle",
  });
};
