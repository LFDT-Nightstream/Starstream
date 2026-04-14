# Contract registry

Starstream is a **UTXO-based blockchain** where **smart contracts are WebAssembly Components**. Unlike account-based blockchains (Ethereum), Starstream uses a UTXO (Unspent Transaction Output) model where each smart contract instance is an independent, immutable output. That means many UTXOs may exist as instances of the same underlying component definition. This registry serves as a way to deduplicate, and serve information about these definitions.

Notably the registry keeps track of
- The binary data (Wasm component)
- The interface definitions (WIT)

The ledger tracks
- which UTXO uses which registry entry
- a serial ID tracking that the UTXO is the nth occurrence of the contract hash onchain

## Design

### Dependency handling

While WIT files allow dependencies (through package-managers), Wasm Components do not embed the entire contents of each dependency inside their file. Rather, they only keep a subset of the dependencies that they use

For example, if we have the following

```wit
// foo.wit

world adder {
    export bar:pkg/add;
}
```
```wit
// bar.wit

interface add {
    add: func(x: u32, y: u32) -> u32;
}
interface sub {
    sub: func(x: u32, y: u32) -> u32;
}
```

the contents of a wasm component for `adder` does *not* contain `sub` in it (even if it's in the dependency) as it is not used (only `add` is used). Although this kind of minimization is tool-specific (i.e. `wac` and `wasm-tools`'s `wit-component` tools have different logic)

That means that if a smart contract has dependencies inlined, we cannot assume that is the *full* dependency (for any given wasm component, its inlined dependencies are likely only a subset of the full dependency sources). Therefore, we cannot infer dependency hashes just from the user's contract.

This means that, unless we required the user to pass the full dependencies to the node as well, we cannot infer the hash of the dependency (the user cannot provide the hash without the pre-image either, or they could lie). Therefore, we cannot rename inline dependencies to specify their hash.

⚠️ Careful: this means that two different contracts `foo`, `bar` that both depend on a dependency of the same name (ex: `dep`) cannot assume the implementations are compatible (no matter what semver says). This is because `dep` in foo and `dep` in `bar` could be two totally different pieces of code that happen to have the same name (you cannot deduplicate them!). This should not be an issue, as tools (like `wac`) do not deduplicate deep dependencies - and only deduplicate top-level exports/imports.

### Content addressing

The primary key for the registry is the contract hash, which is a hash of the Wasm component binary data. Recall that the component contains the type definitions inside its [binary representation](https://github.com/WebAssembly/component-model/blob/0b951b72b3f4019edf386a93585782587d221a0c/design/mvp/Binary.md), so the hash of the contract also includes the hash of its type.

This design avoids the fact that WIT files themselves are not content-addressed (imports instead use names like `use namespace:pkg`). While dependency management systems like `wit-deps` associate a hash to the textual representation of each dependency, in Starstream we cannot have external dependencies (to avoid withholding attacks, where a dependency no longer exists causing the program to be unusable).

Note: WIT can be binary-encoded by tools like `wit-parser`, but we leverage the textual representation to avoid breaking changes in the binary format causing breaking changes in the ledger.

WIT files *can* be made content-addressed as dependencies are imported using `semver` notation, and semver supports "build metadata" that can be used to inline the hash of the dependency (ex: `(1.0.0+wit-hash)`). However, [given the minification of dependencies](#dependency-handling) means the ledger cannot know the full dependency, the ledger cannot fully leverage this.

### Storage

The registry needs to be runnable in the browser. To accomplish this, the registry does not depend on filesystem access.

As the ledger may have millions of registry entries, we also do not force the entire registry to be in-memory.

Instead, there are multiple storage backend options depending on the target platform.

## Custom sections

The registry preserves custom sections in the Wasm component. If you want to remove sections for efficiency, it must be done before submitting to the registry. That means it can be used to
- preserve [comments](https://github.com/bytecodealliance/wasm-tools/blob/d4e317f22c3bace76cb3205003bcc34b4929037d/crates/wit-parser/src/metadata.rs#L70) and [names](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md#name-section)
- preserve information about the original language (ex: Starstream-specific information)

## Registry format support

### wasm-pkg-tools (OCI registry)

TODO

### wit-deps (package registry)

wit-deps generate a sha256 and sha512 of tar files containing the wit files (in text form, not binary). For wit-deps dependency, we need our RPC to be able to return a `.tar.gz` file that contains the WIT file. The WIT file should be in a `wit` folder at the root of the tar. Dependencies can be in a `wit/deps` folder if necessary.

### Single file

We allow querying the WIT for smart contracts in a way that also include all its dependencies inlined in a single file using the [nested package notation](https://github.com/WebAssembly/component-model/blob/41f6ee955dddd74e4b4d8967142bb8ea0110fe43/design/mvp/WIT.md#package-names). Note that only dependencies are in the nested package notation - the root package is not. This is to maximize tooling compatibility. The nested packages, as well as their contents, all preserve the ordering in which they appear inside the Wasm component.

## Validation rules

- Must be a component
- All imports must be of the `exportname` format. Notably, it means you cannot import from URL or paths (no `urlname`) to avoid withholding attacks.
- All exports have an implementation (i.e. [package format](https://github.com/WebAssembly/component-model/blob/0b951b72b3f4019edf386a93585782587d221a0c/design/mvp/WIT.md#package-format) is not allowed)
- No extraneous types allowed (including any top-level package name - i.e. should implicitly be `root:component`). This means you cannot bundle the `package format` and the implementation in the same file.
- Single root described by root imports & exports (i.e. should implicitly be a world called `root`)
- Properly formatted under `wasm-tools validate`

Note that the following do not cause a problem:
- Do not not block certain types of imports that may seem dangerous (ex: `wasi:io`), as they are simply not provided by the host runtime
- Do not limit memory size of components, as they only exist on disk (not in memory). Provers are the ones that have to allocate memory (not the ledger), so it's not a vulnerability. Memory may require to be serialized on-chain, but transactions pay for this in gas.
- Do not ensure that wasm components are always valid on all codepaths (ex: they may have bugs and may crash). This is not a problem from the ledger's perspective, as the ledger never is the one that runs them. It's up to developers to ensure their contracts work correctly, and up to users to only use contracts that do not have bugs.

## Endpoints

- get-wit
  input: contract hash
  supported formats:
    - dependencies excluded (just root)
    - dependencies included (dependencies use nested package notation)
- get-component
  input: contract hash
  supported format:
    - implementation
    - package format

The name of the package name of WIT files exported by this endpoint will use `starstream-contract:<sha256>`.

TODO: how to build wit-deps compatibility. Should make sure we do something that doesn't mess up OCI support

## Future features

- `get-wit` support for querying by the WIT hash of the dependencies-excluded view of the contract's WIT
- consider adding `hashname` as an import type, and the `unlocked-dep` branch of `depname`

## TODO

- deduplicate dependencies (would be really complex, given it's inlined in the wasm components and the way it's embedded matters for the contract hash)
- wac vs `wasm-compose` in `wasm-tools`
- `.dep-v0` to inline dependencies in a custom section?
  - ex: `r#"{"packages":[{"name":"adler","version":"0.2.3","source":"registry"}]}"#;`

### OCI questions

- wasm-pkg-tools support explicit `signing_key`
- how should you add to the registry? OCI (`PackagePublisher`)? Something else?

- oci format and/or custom section can be used to get the associated frontend?

- warg vs oci vs wkg? (warg is the deprecated one, right?)

```
Uses the standard OCI Distribution Spec. Packages are stored as OCI artifacts:
  - List versions: GET /v2/{repository}/tags/list
  - Pull manifest: GET /v2/{repository}/manifests/{tag}
  - Pull blob: GET /v2/{repository}/blobs/{digest}

  The WASM binary is stored as the first layer in an OCI manifest. Uses oci-wasm crate conventions.
```

```
2. Warg Registry

A purpose-built registry protocol for WASM packages with cryptographic signing and content addressing. See the https://github.com/bytecodealliance/registry crate.
```

```
Packages are WASM component format binaries created using wit_component::encode(). They can have embedded metadata (description, license, authors, etc.) via the wasm_metadata crate.
```

what is wasm-pkg-tools hash for a package
```
[[packages]]
name = "wasi:cli"
registry = "wasi.dev"

[[packages.versions]]
requirement = "=0.2.0"
version = "0.2.0"
digest = "sha256:e7e85458e11caf76554b724ebf4f113259decf0f3b1ee2e2930de096f72114a7"
```
