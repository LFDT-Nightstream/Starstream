type ContractConstructor<T> = new (wasm: Uint8Array<ArrayBuffer>) => T;

function uint8ArrayToHexString(uint8Array: Uint8Array): string {
  return Array.prototype.map
    .call(uint8Array, (byte) => ("0" + byte.toString(16)).slice(-2))
    .join("");
}

/** Starstream contract Wasm file loaded in memory. */
export class Contract {
  private readonly module: Promise<WebAssembly.Module>;
  private readonly id: Promise<ArrayBuffer>;

  /** Load a contract from a Wasm module directly. */
  constructor(wasm: Uint8Array<ArrayBuffer>) {
    // TODO: use our Wasmtime variant as a library rather than browser's Wasm implementation
    this.module = WebAssembly.compile(wasm);

    this.id = crypto.subtle.digest("sha256", wasm);
  }

  // sha-256 of wasm
  async getId(): Promise<string> {
    return uint8ArrayToHexString(new Uint8Array(await this.id));
  }

  downcast<T extends Contract>(type: ContractConstructor<T>): T {
    throw new Error("todo");
  }

  // TODO: pass in real type somehow for runtime type checking.
  getCoordinationScript<TArgs extends unknown[], TImports, TReturn>(
    name: string,
  ): CoordinationScript<TArgs, TImports, TReturn> {
    throw new Error("todo");
  }
}

/**  */
export interface CoordinationScript<
  TArgs extends unknown[],
  TImports,
  TReturn,
> {
  readonly contract: Contract;
}

export function call<TArgs extends unknown[], TImports, TReturn>(
  script: CoordinationScript<TArgs, TImports, TReturn>,
  inputs: TArgs,
  imports: TImports,
): Promise<TReturn> {
  throw new Error("todo");
}
