// ----------------------------------------------------------------------------
// FFI imports
@external("env", "ss_yield")
declare function ss_yield(): void;

// ----------------------------------------------------------------------------
// Entry
let supply: u64 = 0;

interface Foo {
  bar: u8,
}

// exports are contract entry points that can be called freely?
export function createInfiniteMint(): void {
  while (true) {
    ++supply;
    ss_yield();
    /*
    desired interface: {
      resume(): void
    }
    */
  }
}

export function getSupply(): u64 {
  return supply;
}

export function main(): void {
  createInfiniteMint();
}
