interface Jspi {
  Suspending?: new (
    fn: (...args: never[]) => Promise<unknown>,
  ) => WebAssembly.ImportValue;
  promising<A extends unknown[], R>(
    fn: (...args: A) => R,
  ): (...args: A) => Promise<R>;
}

function jspi(): Jspi {
  return WebAssembly as unknown as Jspi;
}

/** Imports of a Wasm module built with `starstream-runtime-web`. */
export interface RuntimeWebImports extends WebAssembly.ModuleImports {
  wasmtime_fiber_init: WebAssembly.ImportValue;
  wasmtime_fiber_switch: WebAssembly.ImportValue;
  host_park: WebAssembly.ImportValue;
  host_unpark: WebAssembly.ImportValue;
  host_set_timeout: WebAssembly.ImportValue;
  host_clear_timeout: WebAssembly.ImportValue;
}

/**
 * Imports for an instance that never runs guest code, sleeps or blocks, so
 * never calls them. Unlike `RuntimeWeb`, they do not require JSPI.
 */
export const unusedImports: RuntimeWebImports = {
  wasmtime_fiber_init() {},
  wasmtime_fiber_switch() {},
  host_park() {},
  host_unpark() {},
  host_set_timeout() {},
  host_clear_timeout() {},
};

/** Exports of a Wasm module built with `starstream-runtime-web`. */
export interface RuntimeWebExports {
  __stack_pointer: WebAssembly.Global;
  starstream_fiber_enter(entry: number, arg0: number, top: number): void;
  starstream_timer_fire(timer: number): void;
}

interface Parked {
  sp: number;
  resolve: () => void;
  reject: (err: unknown) => void;
}

interface Fiber {
  entry: number;
  arg0: number;
  started: boolean;
  slot: Parked | undefined;
}

/** Wrap an export in `WebAssembly.promising`. */
export function promising<A extends unknown[], R>(
  fn: (...args: A) => R,
): (...args: A) => Promise<R> {
  return jspi().promising(fn);
}

/**
 * The embedder half of the imports of a Wasm module built with
 * `starstream-runtime-web`: Wasmtime's `custom-fiber` hooks and the
 * `block_on` parking hooks, implemented with JSPI, and timers.
 *
 * Each fiber's `slot` holds the parked other side of a switch to it: a switch
 * parks the caller, swaps the shadow-stack pointer and wakes the other side,
 * and JSPI switches the actual stacks. A fiber's shadow stack starts at
 * `top - 16`, as the top 16 bytes are reserved by the wasmtime-fiber stack
 * layout. Exports that may run guest code must be wrapped in `promising`, and
 * at most one call to them may be in flight: root activations share the main
 * shadow stack, which is only safe LIFO.
 */
export class RuntimeWeb {
  /** Imports to add to the `env` module of the instance. */
  readonly imports: RuntimeWebImports;
  private exports!: RuntimeWebExports;
  private enterFiber!: (
    entry: number,
    arg0: number,
    top: number,
  ) => Promise<void>;
  private readonly fibers = new Map<number, Fiber>();
  private readonly timers = new Map<number, ReturnType<typeof setTimeout>>();
  private parked: (() => void) | undefined;
  private wakePending = false;
  private disposed = false;

  /**
   * Throws if JSPI is unavailable. `onTrap` is called with the error once a
   * timer traps, after all timers are cancelled, and rethrows it by default.
   */
  constructor(
    onTrap: (err: unknown) => void = (err) => {
      throw err;
    },
  ) {
    const { Suspending } = jspi();
    if (typeof Suspending !== "function") {
      throw new Error(
        "running contracts requires JSPI (WebAssembly.Suspending): Chromium 137 or Node.js 24 or newer",
      );
    }
    this.imports = {
      wasmtime_fiber_init: (top: number, entry: number, arg0: number) => {
        this.fibers.set(top, { entry, arg0, started: false, slot: undefined });
      },
      wasmtime_fiber_switch: new Suspending((top: number) =>
        this.fiberSwitch(top),
      ),
      host_park: new Suspending(() => this.park()),
      host_unpark: () => {
        this.unpark();
      },
      host_set_timeout: (timer: number, ms: number) => {
        if (this.disposed) {
          return;
        }
        this.timers.set(
          timer,
          setTimeout(() => {
            this.timers.delete(timer);
            try {
              this.exports.starstream_timer_fire(timer);
            } catch (err) {
              this.dispose();
              onTrap(err);
            }
          }, ms),
        );
      },
      host_clear_timeout: (timer: number) => {
        clearTimeout(this.timers.get(timer));
        this.timers.delete(timer);
      },
    };
  }

  /** Attach to the instance created with `imports`. */
  init(exports: RuntimeWebExports): void {
    this.exports = exports;
    this.enterFiber = promising(exports.starstream_fiber_enter);
  }

  /** Cancel all timers and arm no new ones, e.g. once the instance trapped. */
  dispose(): void {
    this.disposed = true;
    for (const timeout of this.timers.values()) {
      clearTimeout(timeout);
    }
    this.timers.clear();
  }

  private fiberSwitch(top: number): Promise<void> {
    const fiber = this.fibers.get(top);
    if (!fiber) {
      throw new Error(`switch to unknown fiber ${top}`);
    }
    const sp = this.exports.__stack_pointer;
    return new Promise<void>((resolve, reject) => {
      const me: Parked = { sp: sp.value as number, resolve, reject };
      const other = fiber.slot;
      fiber.slot = me;
      if (fiber.started) {
        sp.value = other!.sp;
        other!.resolve();
        return;
      }
      fiber.started = true;
      sp.value = top - 16;
      const finish = (settle: (back: Parked) => void) => {
        const back = fiber.slot!;
        this.fibers.delete(top);
        sp.value = back.sp;
        settle(back);
      };
      this.enterFiber(fiber.entry, fiber.arg0, top).then(
        () => finish((back) => back.resolve()),
        (err: unknown) => finish((back) => back.reject(err)),
      );
    });
  }

  private park(): Promise<void> {
    if (this.wakePending) {
      this.wakePending = false;
      return Promise.resolve();
    }
    return new Promise<void>((resolve) => {
      this.parked = resolve;
    });
  }

  private unpark(): void {
    const resolve = this.parked;
    if (resolve) {
      this.parked = undefined;
      resolve();
    } else {
      this.wakePending = true;
    }
  }
}
