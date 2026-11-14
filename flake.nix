{
  description = "Starstream development environment";

  inputs.nixify.url = "github:rvolosatovs/nixify";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixify, nixpkgs-unstable, ... }:
    with nixify.lib;
    mkFlake {
      overlays = [
        (final: prev: {
          pkgsUnstable = nixpkgs-unstable.legacyPackages.${final.stdenv.hostPlatform.system};
        })
      ];

      withDevShells =
        { devShells, pkgs, ... }:
        extendDerivations {
          buildInputs = [
            pkgs.binaryen
            pkgs.cargo-insta
            pkgs.emscripten
            pkgs.llvmPackages.bintools-unwrapped
            pkgs.llvmPackages.clang-unwrapped
            pkgs.nodejs_24
            pkgs.python3
            pkgs.rustup
            pkgs.wabt
            pkgs.pkgsUnstable.wasm-bindgen-cli_0_2_129
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
