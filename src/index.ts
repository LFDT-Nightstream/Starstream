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

interface IndirectFunctionTableExports {
  __indirect_function_table: WebAssembly.Table,
}

type UtxoExports = MemoryExports & AsyncifyExports;

function asyncify(blob: Uint8Array): Uint8Array {
  binaryen.setOptimizeLevel(4);
  binaryen.setPassArgument(
    "asyncify-imports",
    [
      `env.${LoadedUtxo.utxoEnv.starstream_yield.name}`,
      `env.${LoadedUtxo.utxoEnv.starstream_effect_my_effect.name}`,
    ].join(),
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

function starstream_log(...args: unknown[]) {
  console.log('starstream_log', ...args);
}

const effectHandlers = new Map<string, Function>();

/** A UTXO that has a WebAssembly instance currently in memory. */
class LoadedUtxo {
  static utxoEnv = {
    abort,

    starstream_log,

    starstream_yield(
      this: LoadedUtxo,
      name: number,
      name_len: number,
      data: number,
      data_size: number,
      resume_arg: number,
      resume_arg_size: number,
    ) {
      const view = new Int32Array(this.#exports.memory.buffer);
      if (this.#exports.asyncify_get_state() == AsyncifyState.NORMAL) {
        this.#state = {
          state: "yielded",
          yielded: {
            type_name: new Uint8Array(this.#exports.memory.buffer, name, name_len),
            data: new Uint8Array(this.#exports.memory.buffer, data, data_size),
            resume_arg: new Uint8Array(this.#exports.memory.buffer, resume_arg, resume_arg_size),
          },
        };
        view[STACK_START >> 2] = STACK_START + 8;
        view[(STACK_START + 4) >> 2] = STACK_END;
        this.#exports.asyncify_start_unwind(STACK_START);
      } else {
        this.#exports.asyncify_stop_rewind();
      }
    },

    starstream_event_my_event(this: LoadedUtxo, ...args: unknown[]) {
      console.log('EVENT', ...args);
    },

    starstream_effect_my_effect(this: LoadedUtxo, ...args: unknown[]) {
      console.log('EFFECT', ...args);
      /*LoadedUtxo.utxoEnv.starstream_yield.call(this, ...args);
      this.#state = {
        state: "effect",
        effect: "my_effect",
        args,
      };*/
      let fn = effectHandlers.get("starstream_handle_MyMain_my_effect");
      if (fn) {
        fn(...args);
      } else {
        console.log(effectHandlers);
        throw new Error('missing handler');
      }
    },

    starstream_error_my_error(this: LoadedUtxo, ...args: unknown[]) {
      throw new Error('TX errored: ' + JSON.stringify(args));
    }
  } as const;

  readonly #instance: WebAssembly.Instance;
  readonly #exports: UtxoExports;

  #entryPoint: Function;
  #start_args: unknown[] | undefined;
  #state: {
    state: "not_started",
  } | {
    state: "yielded",
    yielded: {
      type_name: Uint8Array,
      data: Uint8Array,
      resume_arg: Uint8Array,
    },
  } | {
    state: "returned",
    value: unknown,
  } | {
    state: "errored",
    args: unknown[],
  } | {
    state: "effect",
    effect: string,
    args: unknown[],
  } = {
    state: "not_started"
  };

  constructor(
    module: WebAssembly.Module,
    entryPoint: string,
    memory?: Uint8Array
  ) {
    const env = Object.fromEntries(
      Object.entries(LoadedUtxo.utxoEnv).map(([k, v]) => [k, v.bind(this)])
    );
    this.#instance = new WebAssembly.Instance(module, { env });
    // TODO: validate exports
    this.#exports = this.#instance.exports as unknown as UtxoExports;

    if (memory) {
      // memcpy saved memory on top
      new Uint8Array(this.#exports.memory.buffer).set(memory);
    }

    this.#entryPoint = this.#instance.exports[entryPoint] as Function;
  }

  #raw_resume() {
    const returned = this.#entryPoint(...this.#start_args!);
    if (this.#exports.asyncify_get_state() == AsyncifyState.NORMAL) {
      // Normal exit; it's spent.
      this.#state = {
        state: "returned",
        value: returned,
      }
      return false;
    }
    this.#exports.asyncify_stop_unwind();
    return true;
  }

  start(...args: unknown[]): boolean {
    if (this.#state.state !== "not_started") {
      throw new Error("Cannot start() in state " + JSON.stringify(this.#state));
    }
    this.#start_args = args;
    return this.#raw_resume();
  }

  resume(resume_data?: Uint8Array): boolean {
    if (this.#state.state !== "yielded") {
      throw new Error("Cannot resume() in state " + JSON.stringify(this.#state));
    }
    if (this.#state.yielded.resume_arg.byteLength !== (resume_data?.byteLength ?? 0)) {
      throw new Error("resume_arg size mismatch");
    } else if (resume_data) {
      this.#state.yielded.resume_arg.set(resume_data);
    }
    this.#exports.asyncify_start_rewind(STACK_START);
    return this.#raw_resume();
  }

  query(name: string, ...args: unknown[]) {
    if (this.#state.state !== "yielded") {
      throw new Error("Cannot query() in state " + JSON.stringify(this.#state));
    }
    // TODO: enforce asyncify_get_state is NORMAL after this call
    return (this.#instance.exports[name] as Function)(this.#state.yielded.data.byteOffset, ...args);
  }

  isAlive(): boolean {
    return this.#state.state !== "returned";
  }

  debug() {
    if (this.#state.state === "yielded") {
      const result: Record<string, any> = {};
      const name = new TextDecoder().decode(this.#state.yielded.type_name);
      result.__type = name;
      const last_part = name.split("::").pop();
      const prefix = `starstream_query_${last_part}_`;
      for (var key of Object.keys(this.#instance.exports)) {
        if (key.startsWith(prefix)) {
          try {
            result[key.substring(prefix.length)] = this.query(key);
          } catch (e) {
            result[key.substring(prefix.length)] = e;
          }
        }
      }
      return result;
    } else {
      return { state: this.#state.state };
    }
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
    return this.load().isAlive();
  }

  debug() {
    return this.load().debug();
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
        starstream_log(...args: unknown[]) {
          console.log('starstream_log', ...args);
        }
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
            module[entry.name] = (...args: unknown[]) => {
              const utxo = new Utxo(code, entry.name);
              utxo.load().start(...args);
              return setUtxo(utxo);
            };
          } else if (entry.name.startsWith("starstream_query_")) {
            module[entry.name] = (utxo_handle: number, ...args: unknown[]) => {
              return getUtxo(utxo_handle).load().query(entry.name, ...args);
            };
          } else if (entry.name.startsWith("starstream_event_")) {
            module[entry.name] = (...args: unknown[]) => {
              console.log('EVENT', ...args);
            }
          } else if (entry.name.startsWith("starstream_handle_")) {
            module[entry.name] = (handler: number) => {
              console.log('HANDLER =', handler);
              if (handler == 0) {
                effectHandlers.delete(entry.name);
              } else {
                effectHandlers.set(entry.name, (...args: unknown[]) => {
                  indirect.get(handler)(...args);
                });
              }
            }
          }
        }
      }
    }

    // Run
    const instance = new WebAssembly.Instance(coordinationScript, imports);
    const indirect = (instance.exports as unknown as IndirectFunctionTableExports).__indirect_function_table;
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

  debug() {
    return [...this.utxos].map(u => u.debug());
  }
}

let n = 0;
const universe = new Universe();
console.log(++n, '--', universe.debug());

const exampleCoordination = new WebAssembly.Module(
  await readFile(
    "target/wasm32-unknown-unknown/debug/example_coordination.wasm"
  )
);

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
console.log(++n, '--', universe.debug());

universe.runTransaction(
  exampleCoordination,
  "produce"
);
console.log(++n, '--', universe.debug());

/*
universe.runTransaction(
  exampleCoordination,
  "consume",
  [
    universe.utxos.values().next().value
  ]
);
console.log(++n, '--', universe, universe.utxos.values().next().value?.load().query("starstream_query_MyMain_get_supply"));
*/

universe.runTransaction(
  exampleCoordination,
  "star_mint",
  [
    // PublicKey has no representation yet
    17n,
  ]
);
console.log(++n, '--', universe.debug());

universe.runTransaction(
  exampleCoordination,
  "star_mint",
  [
    20n,
  ]
);
console.log(++n, '--', universe.debug());

universe.runTransaction(
  exampleCoordination,
  "star_combine",
  [...universe.utxos.values()].splice(1),
);
console.log(++n, '--', universe.debug());
