import starstreamSandboxWasm from "file-loader!../../target/wasm32-unknown-unknown/release/starstream_sandbox.wasm";

export interface SandboxWorkerRequest {
  request_id: number;
  input: string;
  run: boolean;
  prove: boolean;
}

export type SandboxWorkerResponse = {
  request_id: number;
} & (
  | {
      compiler_log: string;
      warnings: number;
      errors: number;
    }
  | {
      ast: string;
    }
  | {
      wat: string;
    }
  | {
      run_log: string;
    }
  | {
      append_run_log: number;
      target: string;
      body: string;
    }
  | {
      idle: true;
    }
  | {
      sequence_diagram: string;
    }
);

interface SandboxWasmImports extends WebAssembly.ModuleImports {
  getrandom(ptr: number, len: number): void;

  read_input(ptr: number, len: number): void;
  set_compiler_log(
    ptr: number,
    len: number,
    warnings: number,
    errors: number
  ): void;
  set_ast(ptr: number, len: number): void;
  set_wat(ptr: number, len: number): void;
  set_run_log(ptr: number, len: number): void;
  append_run_log(
    level: number,
    target: number,
    target_len: number,
    body: number,
    body_len: number
  ): void;
  set_sequence_diagram(ptr: number, len: number): void;
  set_proof_file(ptr: number, len: number): void;
}

interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  run(input_len: number, run: boolean, prove: boolean): void;
}

let modulePromise: Promise<WebAssembly.WebAssemblyInstantiatedSource> | null =
  null;
export async function getWasmInstance(
  env: SandboxWasmImports
): Promise<SandboxWasmExports> {
  if (modulePromise === null) {
    // First fetch gets instantiateStreaming privilege.
    modulePromise = WebAssembly.instantiateStreaming(
      fetch(starstreamSandboxWasm),
      { env }
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

function send(r: SandboxWorkerResponse) {
  self.postMessage(r);
}

self.onmessage = async function ({ data }: { data: SandboxWorkerRequest }) {
  const request_id = data.request_id;
  const input = new TextEncoder().encode(data.input);
  const wasm = await getWasmInstance({
    getrandom(ptr, len) {
      crypto.getRandomValues(new Uint8Array(wasm.memory.buffer, ptr, len));
    },

    read_input(ptr, len) {
      new Uint8Array(wasm.memory.buffer, ptr, len).set(input);
    },
    set_compiler_log(ptr, len, warnings, errors) {
      send({
        request_id,
        compiler_log: utf8(wasm, ptr, len),
        warnings,
        errors,
      });
    },
    set_ast(ptr, len) {
      send({
        request_id,
        ast: utf8(wasm, ptr, len),
      });
    },
    set_wat(ptr, len) {
      send({
        request_id,
        wat: utf8(wasm, ptr, len),
      });
    },
    set_run_log(ptr, len) {
      send({
        request_id,
        run_log: utf8(wasm, ptr, len),
      });
    },
    append_run_log(level, target, target_len, body, body_len) {
      send({
        request_id,
        append_run_log: level,
        target: utf8(wasm, target, target_len),
        body: utf8(wasm, body, body_len),
      });
    },
    set_sequence_diagram(ptr, len) {
      send({
        request_id,
        sequence_diagram: utf8(wasm, ptr, len),
      });
    },
    set_proof_file(ptr, len) {
      console.log("set_proof_file", ptr, len);
    },
  });
  try {
    wasm.run(input.length, data.run, data.prove);
  } catch (crash) {
    send({
      request_id,
      append_run_log: 1,
      target: "sandbox",
      body: String(crash),
    });
  }
  send({
    request_id,
    idle: true,
  });
};
