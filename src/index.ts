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

interface MainExports {
  main(): void;
}

type UtxoExports = MemoryExports & AsyncifyExports & MainExports;

function asyncify(blob: Uint8Array): Uint8Array {
  binaryen.setOptimizeLevel(4);
  binaryen.setPassArgument("asyncify-imports", "env.ss_yield");

  const ir = binaryen.readBinary(blob);
  ir.setMemory(1, 1);
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

    ss_yield(this: LiveUtxo, ...args: unknown[]) {
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

  #yielded: unknown[];

  constructor(module: WebAssembly.Module, memory?: Uint8Array) {
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
  }

  test() {
    while (true) {
      const returned = this.exports.main();
      if (this.exports.asyncify_get_state() == AsyncifyState.NORMAL) {
        // Normal exit; it's spent.
        console.log("returned", returned);
        return returned;
      }
      this.exports.asyncify_stop_unwind();
      console.log("yielded", this.#yielded);
      this.exports.asyncify_start_rewind(STACK_START);
    }
  }
}

class Utxo {
  live?: LiveUtxo;
}

class Universe {
  readonly utxos = new Set<Utxo>();

  runTransaction(coordinationScript: WebAssembly.Module) {
    // We aren't suspending this, we want to run it to completion always, so
    // we don't need to asyncify it.
    const imports = WebAssembly.Module.imports(coordinationScript);
    const instance = new WebAssembly.Instance(coordinationScript, {
      env: {
        abort() {
          throw "abort() called";
        },
      },
      // utxo types
    });
    (instance.exports.main as Function)();
  }
}

// Get a WebAssembly binary and compile it to an instance.
const binary = asyncify(await readFile("build/release.wasm"));
const compiled = new WebAssembly.Module(binary);
const liveUtxo = new LiveUtxo(compiled);
liveUtxo.test();
