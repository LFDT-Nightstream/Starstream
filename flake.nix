{
  description = "Starstream development environment";

  inputs.nixify.url = "github:rvolosatovs/nixify";

  outputs =
    { nixify, ... }:
    with nixify.lib;
    mkFlake {
      withDevShells =
        { devShells, pkgs, ... }:
        extendDerivations {
          buildInputs = [
            pkgs.binaryen
            pkgs.cargo-insta
            pkgs.emscripten
            pkgs.llvmPackages.bintools-unwrapped
            pkgs.llvmPackages.clang-unwrapped
            pkgs.nodejs_22
            pkgs.python3
            pkgs.rustup
            pkgs.wabt
            pkgs.wasm-bindgen-cli_0_2_105
            pkgs.wasm-tools
          ];

          # Apple's ar/ranlib silently drop non-Mach-O archive members, which
          # breaks `psm`'s wasm32.o static library on macOS: its rust_psm_*
          # symbols end up as `(import "env" ...)` in the final Wasm. Use
          # llvm-ar for wasm32 builds everywhere instead.
          env.AR_wasm32_unknown_unknown = "${pkgs.llvmPackages.bintools-unwrapped}/bin/llvm-ar";

          # Compile `psm`'s wasm32 shim with an unwrapped clang; the nix cc
          # wrapper injects host-specific flags that break wasm32 targets.
          env.CC_wasm32_unknown_unknown = "${pkgs.llvmPackages.clang-unwrapped}/bin/clang";
        } devShells;
    };
}
