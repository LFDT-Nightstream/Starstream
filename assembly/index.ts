// ----------------------------------------------------------------------------
// FFI imports
@external("env", "yield")
declare function emit(value: isize): void;

// ----------------------------------------------------------------------------
// Entry
class TokenContract {
  createInfiniteMint(frame: i32): void {
    for (let i: i32 = 0; i < frame; ++i) {
      emit(i);
      if (i > 5) {
        break;
      }
    }
  }
}

export function main(): i32 {
  new TokenContract().createInfiniteMint(5);
  return 17;
}
