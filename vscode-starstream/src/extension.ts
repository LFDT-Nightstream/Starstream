import tree_sitter_starstream_wasm from "file-loader!../../tree-sitter-starstream/tree-sitter-starstream.wasm";
import tree_sitter_wasm from "file-loader!../node_modules/web-tree-sitter/tree-sitter.wasm";
import highlights_scm from "raw-loader!../../tree-sitter-starstream/queries/highlights.scm";
import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/browser";
import { Parser } from "web-tree-sitter";
import { registerProvider } from "./tree-sitter-vscode";

export async function activate(context: vscode.ExtensionContext) {
  await Promise.all([
    activateTreeSitter(context),
    activateLanguageClient(context),
  ]);
}

// ----------------------------------------------------------------------------
// LSP client

async function activateLanguageClient(context: vscode.ExtensionContext) {
  const worker = new Worker(
    vscode.Uri.joinPath(
      context.extensionUri,
      "dist",
      "language-server.worker.js"
    ).toString()
  );
  worker.addEventListener("error", (event) => {
    vscode.window.showErrorMessage(`${event.error}`);
    console.error("Starstream worker error:", event.error);
  });
  worker.addEventListener("message", (event) => {
    vscode.window.showInformationMessage(JSON.stringify(event.data));
  });
  worker.postMessage({ derp: 17 });
  const lc = new LanguageClient(
    "starstream",
    "Starstream Language Server",
    {
      documentSelector: [{ language: "starstream" }],
    },
    worker
  );
  context.subscriptions.push(lc);
  context.subscriptions.push({
    dispose() {
      worker.terminate();
    },
  });
  vscode.window.showInformationMessage(`worker ok: ${worker}`);
  await lc.start();
}

// ----------------------------------------------------------------------------
// Tree-sitter highlighter

async function activateTreeSitter(context: vscode.ExtensionContext) {
  // NOTE: readFile returns some kind of evil Uint8Array that's missing methods, so we wrap it.
  const treeSitterWasmBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(context.extensionUri, "dist", tree_sitter_wasm)
    )
  );
  const treeSitterStarstreamWasmBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(
        context.extensionUri,
        "dist",
        tree_sitter_starstream_wasm
      )
    )
  );

  await Parser.init({
    async instantiateWasm(
      imports: WebAssembly.Imports,
      // NOTE: One spot in Emscripten's output has this in `(mod, inst)` order, which is a lie.
      cb: (inst: WebAssembly.Instance, mod: WebAssembly.Module) => void
    ) {
      try {
        const { module, instance } = await WebAssembly.instantiate(
          treeSitterWasmBytes,
          imports
        );
        cb(instance, module);
      } catch (e) {
        // Must manually catch since any error here gets swallowed otherwise.
        vscode.window.showErrorMessage(`${e}`);
      }
    },
  });

  // Use an embedded copy of https://marketplace.visualstudio.com/items?itemName=AlecGhost.tree-sitter-vscode
  // Until https://github.com/microsoft/vscode/issues/50140
  const provider = registerProvider([
    {
      lang: "starstream",
      parser: treeSitterStarstreamWasmBytes,
      highlights: highlights_scm,
      injectionOnly: false,
    },
  ]);
  context.subscriptions.push(provider);
}
