// JavaScript Promise Integration (JSPI), not yet in TypeScript's DOM lib.
// https://github.com/WebAssembly/js-promise-integration
declare namespace WebAssembly {
  // Extends `Function` so a `Suspending` is a valid `ImportValue`.
  class Suspending extends Function {
    constructor(fn: (...args: never[]) => unknown);
  }
  function promising<A extends unknown[], R>(
    fn: (...args: A) => R,
  ): (...args: A) => Promise<R>;
}
