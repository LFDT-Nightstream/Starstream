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
    `env.${LiveUtxo.utxoEnv.starstream_yield.name}`
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

/** A UTXO that has a WebAssembly instance currently in memory. */
class LiveUtxo {
  static utxoEnv = {
    abort(this: LiveUtxo) {
      throw "abort() called";
    },

    log(this: LiveUtxo, ...args: unknown[]) {
      console.log(...args);
    },

    starstream_yield(this: LiveUtxo, ...args: unknown[]) {
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
      Object.entries(LiveUtxo.utxoEnv).map(([k, v]) => [k, v.bind(this)])
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

  resume() {
    if (this.#returned !== null) {
      throw new Error("Cannot resume() after exhaustion");
    }
    if (this.#yielded) {
      this.exports.asyncify_start_rewind(STACK_START);
    }
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

  effect(name: string, args: unknown[]) {
    return (this.instance.exports[name] as Function)(this.#yielded![0]);
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
  #live?: LiveUtxo;

  #module;
  #entryPoint;

  constructor(module: WebAssembly.Module, entryPoint: string) {
    this.#module = module;
    this.#entryPoint = entryPoint;
  }

  flush() {}

  load(): LiveUtxo {
    return (this.#live ??= new LiveUtxo(this.#module, this.#entryPoint));
  }
}

class Universe {
  readonly utxoCode = new Map<string, UtxoCode>();
  readonly utxos = new Set<Utxo>();

  runTransaction(coordinationScript: WebAssembly.Module, main: string) {
    // We aren't suspending this, we want to run it to completion always, so
    // we don't need to asyncify it.
    const imports: WebAssembly.Imports = {
      env: {
        abort() {
          throw "abort() called";
        },
      },
    };

    const utxos: Utxo[] = [];

    for (const entry of WebAssembly.Module.imports(coordinationScript)) {
      const code = this.utxoCode.get(entry.module);
      if (!code) {
        throw new Error(`Missing import: ${code}`);
      }

      console.log(entry);
      const module = (imports[entry.module] ??= {});
      let f: Function = (...args: unknown[]) => console.log(entry.name, args);
      if (entry.kind == "function") {
        switch (entry.name) {
          case "MyMain_status":
            f = (utxo_handle: number) => {
              return utxos[utxo_handle].load().returned === null;
            };
            break;
          case "MyMain_resume":
            f = (utxo_handle: number) => {
              utxos[utxo_handle].load().resume();
              console.log("after resume");
            };
            break;
          case "MyMain_main_new":
            f = () => {
              const utxo_handle = utxos.length;
              const utxo = new Utxo(code.module, entry.name);
              utxo.load().resume();
              utxos.push(utxo);
              return utxo_handle;
            };
            break;
          case "MyMain_effect_get_supply":
            f = (utxo_handle: number) => {
              const r = utxos[utxo_handle].load().effect(entry.name, []);
              console.log("effect[", entry.name, "] =", r);
              return r;
            };
            break;
        }
        module[entry.name] = f;
      }
    }
    console.log(
      WebAssembly.Module.customSections(coordinationScript, "starstream")
    );

    const instance = new WebAssembly.Instance(coordinationScript, imports);
    (instance.exports[main] as Function)();
  }
}

const universe = new Universe();
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

universe.runTransaction(
  new WebAssembly.Module(
    await readFile(
      "target/wasm32-unknown-unknown/debug/example_coordination.wasm"
    )
  ),
  "produce"
);
