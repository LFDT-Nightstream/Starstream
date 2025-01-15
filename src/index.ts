// example.js
import binaryen from "binaryen";
import { readFile } from "fs/promises";

interface MemoryExports {
  memory: WebAssembly.Memory;
}

interface AsyncifyExports {
  asyncify_get_state(): number;
  asyncify_start_unwind(addr: number): void;
  asyncify_stop_unwind(): void;
  asyncify_start_rewind(addr: number): void;
  asyncify_stop_rewind(): void;
}

type UtxoExports = MemoryExports & AsyncifyExports;

function asyncify(blob: Uint8Array): Uint8Array {
  binaryen.setOptimizeLevel(4);
  binaryen.setPassArgument(
    "asyncify-imports",
    `env.${LoadedUtxo.utxoEnv.starstream_yield.name}`
  );

  const ir = binaryen.readBinary(blob);
  // BulkMemory is called for by AssemblyScript; stuff blows up w/o something else.
  ir.setFeatures(binaryen.Features.All);
  ir.runPasses(["asyncify"]);
  return ir.emitBinary();
}

enum AsyncifyState {
  NORMAL = 0,
  UNWIND = 1,
  REWIND = 2,
}

/** Where the unwind/rewind data structure will live. */
const STACK_START = 16;
const STACK_END = 1024;

function abort() {
  throw new Error("abort() called");
}

/** A UTXO that has a WebAssembly instance currently in memory. */
class LoadedUtxo {
  static utxoEnv = {
    abort,

    starstream_yield(this: LoadedUtxo, ...args: unknown[]) {
      const view = new Int32Array(this.exports.memory.buffer);
      if (this.exports.asyncify_get_state() == AsyncifyState.NORMAL) {
        this.#yielded = args;
        view[STACK_START >> 2] = STACK_START + 8;
        view[(STACK_START + 4) >> 2] = STACK_END;
        this.exports.asyncify_start_unwind(STACK_START);
      } else {
        this.exports.asyncify_stop_rewind();
      }
    },
  } as const;

  readonly instance: WebAssembly.Instance;
  readonly exports: UtxoExports;

  #entryPoint: Function;
  #yielded: unknown[] | null = null;
  #returned: unknown | null = null;

  constructor(
    module: WebAssembly.Module,
    entryPoint: string,
    memory?: Uint8Array
  ) {
    const env = Object.fromEntries(
      Object.entries(LoadedUtxo.utxoEnv).map(([k, v]) => [k, v.bind(this)])
    );
    this.instance = new WebAssembly.Instance(module, { env });
    // TODO: validate exports
    this.exports = this.instance.exports as unknown as UtxoExports;

    if (memory) {
      // memcpy saved memory on top
      new Uint8Array(this.exports.memory.buffer).set(memory);
    }

    this.#entryPoint = this.instance.exports[entryPoint] as Function;
  }

  #raw_resume() {
    const returned = this.#entryPoint();
    if (this.exports.asyncify_get_state() == AsyncifyState.NORMAL) {
      // Normal exit; it's spent.
      this.#yielded = null;
      this.#returned = returned;
      return false;
    }
    this.exports.asyncify_stop_unwind();
    return true;
  }

  start(): boolean {
    if (this.#returned !== null) {
      throw new Error("Cannot start() after exhaustion");
    }
    if (this.#yielded) {
      throw new Error("Cannot start() after yield");
    }
    return this.#raw_resume();
  }

  resume(resume_data?: Uint8Array): boolean {
    if (this.#returned !== null) {
      throw new Error("Cannot resume() after exhaustion");
    }
    if (!this.#yielded) {
      throw new Error("Cannot resume() before yield");
    }
    const [yield_arg, yield_arg_size, resume_arg, resume_arg_size] = this.#yielded;
    if (resume_arg_size !== (resume_data?.byteLength ?? 0)) {
      throw new Error("resume_arg size mismatch");
    } else if (resume_data) {
      new Uint8Array(this.exports.memory.buffer).set(resume_data, resume_arg as number);
    }
    this.exports.asyncify_start_rewind(STACK_START);
    return this.#raw_resume();
  }

  effect(name: string, ...args: unknown[]) {
    // TODO: enforce asyncify_get_state is NORMAL after this call
    return (this.instance.exports[name] as Function)(this.#yielded![0], ...args);
  }

  get yielded() {
    return this.#yielded;
  }

  get returned() {
    return this.#returned;
  }
}

class UtxoCode {
  constructor(public module: WebAssembly.Module) {}
}

class Utxo {
  #code: UtxoCode;
  #entryPoint: string;
  #loaded?: LoadedUtxo;

  constructor(code: UtxoCode, entryPoint: string) {
    this.#code = code;
    this.#entryPoint = entryPoint;
  }

  flush() {}

  load(): LoadedUtxo {
    return (this.#loaded ??= new LoadedUtxo(this.#code.module, this.#entryPoint));
  }

  isAlive(): boolean {
    return this.load().returned === null;
  }
}

class Universe {
  readonly utxoCode = new Map<string, UtxoCode>();
  readonly utxos = new Set<Utxo>();

  runTransaction(coordinationScript: WebAssembly.Module, main: string, inputs: unknown[] = []) {
    // We aren't suspending this, we want to run it to completion always, so
    // we don't need to asyncify it.
    const imports: WebAssembly.Imports = {
      env: {
        abort,
      },
    };

    // Prepare UTXO handles
    const utxos = new Map<number, Utxo>();
    function setUtxo(utxo: Utxo): number {
      // target range: [1, 1 << 30]
      const handle = Math.ceil((1 - Math.random()) * (1 << 30));
      utxos.set(handle, utxo);
      return handle;
    }
    function getUtxo(handle: number): Utxo {
      const utxo = utxos.get(handle);
      if (!utxo) {
        throw new Error(`Invalid UTXO handle: ${handle}; known: ${JSON.stringify(utxos)}`);
      }
      return utxo;
    }

    // Prepare inputs
    const inputs2 = [...inputs];
    for (let i = 0; i < inputs2.length; ++i) {
      const v = inputs2[i];
      if (v instanceof Utxo) {
        inputs2[i] = setUtxo(v);
      }
    }

    // Bind imports
    for (const entry of WebAssembly.Module.imports(coordinationScript)) {
      if (entry.module.startsWith("starstream:")) {
        const code = this.utxoCode.get(entry.module);
        if (!code) {
          throw new Error(`Unknown module: ${entry.module}`);
        }

        const module = (imports[entry.module] ??= {});

        if (entry.kind == "function") {
          if (entry.name.startsWith("starstream_status_")) {
            module[entry.name] = (utxo_handle: number) => {
              return getUtxo(utxo_handle).isAlive();
            };
          } else if (entry.name.startsWith("starstream_resume_")) {
            module[entry.name] = (utxo_handle: number, resume_arg: number, resume_arg_size: number) => {
              const slice = new Uint8Array(memory.buffer).slice(resume_arg, resume_arg + resume_arg_size);
              getUtxo(utxo_handle).load().resume(slice);
            };
          } else if (entry.name.startsWith("starstream_new_")) {
            module[entry.name] = () => {
              // TODO: allow passing arguments
              const utxo = new Utxo(code, entry.name);
              utxo.load().start();
              return setUtxo(utxo);
            };
          } else if (entry.name.startsWith("starstream_effect_")) {
            module[entry.name] = (utxo_handle: number, ...args: unknown[]) => {
              return getUtxo(utxo_handle).load().effect(entry.name, ...args);
            };
          }
        }
      }
    }

    // Run
    const instance = new WebAssembly.Instance(coordinationScript, imports);
    const memory = (instance.exports as unknown as MemoryExports).memory;
    (instance.exports[main] as Function)(...inputs2);

    // Update UTXO set
    for (const utxo of utxos.values()) {
      if (utxo.isAlive()) {
        this.utxos.add(utxo);
      } else {
        this.utxos.delete(utxo);
      }
    }
  }
}

const universe = new Universe();
console.log(universe);

universe.utxoCode.set(
  "starstream:example_contract",
  new UtxoCode(
    new WebAssembly.Module(
      asyncify(
        await readFile(
          "target/wasm32-unknown-unknown/debug/example_contract.wasm"
        )
      )
    )
  )
);
console.log(universe);

universe.runTransaction(
  new WebAssembly.Module(
    await readFile(
      "target/wasm32-unknown-unknown/debug/example_coordination.wasm"
    )
  ),
  "produce"
);
console.log(universe, universe.utxos.values().next().value?.load().effect("starstream_effect_MyMain_get_supply"));

universe.runTransaction(
  new WebAssembly.Module(
    await readFile(
      "target/wasm32-unknown-unknown/debug/example_coordination.wasm"
    )
  ),
  "consume",
  [
    universe.utxos.values().next().value
  ]
);
console.log(universe, universe.utxos.values().next().value?.load().effect("starstream_effect_MyMain_get_supply"));
