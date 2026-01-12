# Contract registry

This projoect keeps track of data for contract hashes. Notably,
- The binary data (Wasm component)
- The interface definitions (WIT)

This registry is content-addressed using the contract hash

## Design

The primary key for the registry is the contract hash, which is a hash of the Wasm component binary data. Recall that the component contains the type definitions inside its [binary representation](https://github.com/WebAssembly/component-model/blob/0b951b72b3f4019edf386a93585782587d221a0c/design/mvp/Binary.md), so the hash of the contract also includes the hash of its type.

The registry preserves custom sections in the Wasm component. If you want to remove sections for efficiency, it must be done before submitting to the registry. That means it can be used to
- preserve [comments](https://github.com/bytecodealliance/wasm-tools/blob/d4e317f22c3bace76cb3205003bcc34b4929037d/crates/wit-parser/src/metadata.rs#L70) and [names](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md#name-section)
- preserve information about the original language (ex: Starstream-specific information)

- import Starstream UTXOs via hash
  - imports [support](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions) using a hash name containing a content-hash of the bytes of a particular wasm implementation but not specifying location of the bytes.
- The ledger disallows any `urlname` in imports to prevent withholding attacks
- instance+package in the same file
- use hash as the root namespace instead w/ (1.0.0+build-metadata)
- minimization

## Registry format support

- (deprecated) warg: https://github.com/bytecodealliance/registry
- wasm-pkg-tools (OCI registry)
- wit-deps (package registry)

## Validation rules

- All imports must be of the `exportname` format. Notably, it means you cannot import from URL or paths (no `urlname`).
- All exports have an implementation (i.e. [package format](https://github.com/WebAssembly/component-model/blob/0b951b72b3f4019edf386a93585782587d221a0c/design/mvp/WIT.md#package-format) is not allowed)
- No extraneous types allowed (including any top-level package name - i.e. should implicitly be `root:component`)
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

## Future features

- `get-wit` support for querying by the WIT hash of the dependnecies-excluded view of the contract's WIT
- consider adding `hashname` as an import type


## TODO

- Do we have to reject components that have a top-level name? Probably best to unify to either always names + build hash OR generated name with hash

- Do we have to strip the package name before hashing the WIT, given it's removed when used as a top-level module?

- How does wit-deps calculate the hash? Binary representation? Text representation?

- Do we want to enforce `component-type` (includes `wit-component-encoding`) custom section?

```
(import "url=<https://mycdn.com/my-component.wasm>" (component ...))
(import "url=<./other-component.wasm>,integrity=<sha256-X9ArH3k...>" (component ...))
(import "locked-dep=<my-registry:sqlite@1.2.3>,integrity=<sha256-H8BRh8j...>" (component ...))
(import "unlocked-dep=<my-registry:imagemagick@{>=1.0.0}>" (instance ...))
(import "integrity=<sha256-Y3BsI4l...>" (component ...))
```

do we want to support unlocked-deps (the ledger fills the import for you)?
- `unlocked-dep=<my-registry:imagemagick@{>=1.0.0}>`
