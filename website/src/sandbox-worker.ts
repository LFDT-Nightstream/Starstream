import starstreamSandboxWasm from "file-loader!../../target/wasm32-unknown-unknown/release/starstream_sandbox.wasm";

export interface SandboxWasmImports extends WebAssembly.ModuleImports {
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
}

export interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  run(input_len: number, run: boolean): void;
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
