import tree_sitter_starstream_wasm from "file-loader!../../tree-sitter-starstream/tree-sitter-starstream.wasm";
import starstream_language_server_web_wasm from "file-loader!../build/starstream_language_server_web_bg.wasm";
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
  // Use readFile to get the worker.js contents, because passing the URL straight
  // to `new Worker` doesn't work properly in the browser where the URL is on
  // the non-fetchable `extension-file://` scheme.
  const workerJsBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(
        context.extensionUri,
        "dist",
        "language-server.worker.js"
      )
    )
  );
  const worker = new Worker(URL.createObjectURL(new Blob([workerJsBytes])), {
    name: "Starstream Language Server",
  });
  context.subscriptions.push({
    dispose() {
      worker.terminate();
    },
  });
  const lc = new LanguageClient(
    "starstream",
    "Starstream Language Server",
    {
      documentSelector: [{ language: "starstream" }],
    },
    worker
  );
  context.subscriptions.push(lc);

  // Set some extra error logging just in case...
  const output = lc.outputChannel;
  worker.addEventListener("error", (event) => {
    output.appendLine(`worker error: ${event.error}`);
    console.error("worker error:", event.error);
  });
  worker.addEventListener("messageerror", (event) => {
    output.appendLine(`worker messageerror: ${event.data}`);
    console.error("worker messageerror:", event.data);
  });

  // Send the Wasm bytes to the worker, wait for it to reply that it's loaded,
  // then start the language client.
  const wasmInitPromise = new Promise<void>((resolve, reject) => {
    function onInitReply(event: MessageEvent) {
      if (event.data) {
        reject(event.data);
      } else {
        resolve();
      }
      worker.removeEventListener("message", onInitReply);
    }
    worker.addEventListener("message", onInitReply);
  });
  const languageServerWasmBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(
        context.extensionUri,
        "dist",
        starstream_language_server_web_wasm
      )
    )
  );
  worker.postMessage(languageServerWasmBytes, [languageServerWasmBytes.buffer]);
  await wasmInitPromise;
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
