const decoder = new TextDecoder();
const encoder = new TextEncoder();

interface Exports {
  memory: WebAssembly.Memory;
  compiler_alloc(len: number): number;
  compiler_add(
    path: number,
    pathLen: number,
    data: number,
    dataLen: number,
  ): number;
  compiler_compile(entry: number, len: number): number;
}

/** Contract compiled by `WasmCompiler.compile`. */
export interface Compiled {
  /** The Wasm component of the contract. */
  component: Uint8Array;
  /** The warnings of the compilation, rendered as text. */
  diagnostics: string;
}

/**
 * Starstream compiler running in this JS realm, as an instance of the
 * `starstream-compiler-web` Wasm module.
 */
export class WasmCompiler {
  private exports!: Exports;
  private error: string | undefined;
  private compiled: Compiled | undefined;
  private failure: unknown;

  private constructor() {}

  /** Instantiate the compiled `starstream-compiler-web` module. */
  static async instantiate(module: WebAssembly.Module): Promise<WasmCompiler> {
    const compiler = new WasmCompiler();
    const instance = await WebAssembly.instantiate(module, {
      env: {
        compiler_error: (ptr: number, len: number) => {
          compiler.error = decoder.decode(compiler.bytes(ptr, len));
        },
        compiler_result: (
          component: number,
          componentLen: number,
          diagnostics: number,
          diagnosticsLen: number,
        ) => {
          compiler.compiled = {
            component: compiler.bytes(component, componentLen).slice(),
            diagnostics: decoder.decode(
              compiler.bytes(diagnostics, diagnosticsLen),
            ),
          };
        },
      },
    });
    compiler.exports = instance.exports as unknown as Exports;
    return compiler;
  }

  /**
   * Compile the contract at `entry` and the modules it imports from the
   * source `files`, by path, failing with the rendered errors.
   */
  compile(entry: string, files: Record<string, string | Uint8Array>): Compiled {
    if (this.failure) {
      throw this.failure;
    }
    this.error = undefined;
    this.compiled = undefined;
    try {
      for (const [path, contents] of Object.entries(files)) {
        const data =
          typeof contents === "string" ? encoder.encode(contents) : contents;
        this.call(() => {
          const pathData = encoder.encode(path);
          return this.exports.compiler_add(
            this.write(pathData),
            pathData.length,
            this.write(data),
            data.length,
          );
        });
      }
      const entryData = encoder.encode(entry);
      this.call(() =>
        this.exports.compiler_compile(this.write(entryData), entryData.length),
      );
    } catch (err) {
      if (err instanceof WebAssembly.RuntimeError) {
        this.failure = this.error ? new Error(this.error, { cause: err }) : err;
        throw this.failure;
      }
      throw err;
    }
    const compiled = this.compiled!;
    if (compiled.component.length === 0) {
      throw new Error(compiled.diagnostics || "compilation failed");
    }
    return compiled;
  }

  private call(f: () => number): void {
    if (f() !== 0) {
      throw new Error(this.error);
    }
  }

  private bytes(ptr: number, len: number): Uint8Array<ArrayBuffer> {
    return new Uint8Array(this.exports.memory.buffer, ptr >>> 0, len);
  }

  private write(data: Uint8Array): number {
    const ptr = this.exports.compiler_alloc(data.length);
    this.bytes(ptr, data.length).set(data);
    return ptr;
  }
}
