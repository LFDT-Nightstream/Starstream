import tree_sitter_starstream_wasm from "../../tree-sitter-starstream/tree-sitter-starstream.wasm?url";
import starstream_language_server_web_wasm from "../build/starstream_language_server_web_bg.wasm?url";
import tree_sitter_wasm from "../node_modules/web-tree-sitter/web-tree-sitter.wasm?url";
import highlights_scm from "../../tree-sitter-starstream/queries/highlights.scm?raw";
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
        "language-server.worker.js",
      ),
    ),
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
    worker,
    {
      documentSelector: [{ language: "starstream" }],
      // We push workspace files ourselves (see `syncWorkspaceFiles`), so the
      // server shouldn't also register its own file watcher.
      initializationOptions: { syncFiles: true },
    },
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
        starstream_language_server_web_wasm,
      ),
    ),
  );
  worker.postMessage(languageServerWasmBytes, [languageServerWasmBytes.buffer]);
  await wasmInitPromise;
  await lc.start();

  await syncWorkspaceFiles(context, lc);
}

// ----------------------------------------------------------------------------
// Workspace file sync
//
// The language server runs as Wasm in a worker and can't read the filesystem,
// so push it the workspace's source files (for resolving path imports) and
// keep them updated. Open documents are sent separately by the LSP client.

const SOURCE_GLOB = "**/*.{star,wasm}";

/** Must match the directories `walk` skips in `starstream-compiler/src/module_graph.rs`. */
function isSkippedName(name: string): boolean {
  return (
    name.startsWith(".") ||
    name === "target" ||
    name === "artifacts" ||
    name === "node_modules"
  );
}

/** Mirrors `SyncedFile` in `starstream-language-server/src/lib.rs`. */
interface SyncedFile {
  uri: string;
  text?: string;
  base64?: string;
}

async function syncWorkspaceFiles(
  context: vscode.ExtensionContext,
  lc: LanguageClient,
) {
  const send = (params: {
    replace?: boolean;
    files?: SyncedFile[];
    removed?: string[];
  }) => lc.sendNotification("starstream/syncFiles", params);

  // A failed sync shouldn't break the extension; log it and carry on.
  const logged =
    <A extends unknown[]>(f: (...args: A) => Promise<void>) =>
    (...args: A) =>
      f(...args).catch((e) =>
        lc.outputChannel.appendLine(`starstream/syncFiles failed: ${e}`),
      );

  const watcher = vscode.workspace.createFileSystemWatcher(SOURCE_GLOB);
  context.subscriptions.push(watcher);
  const update = logged(async (uri: vscode.Uri) => {
    if (uri.path.split("/").some(isSkippedName)) return;
    const file = await readSourceFile(uri);
    if (file) await send({ files: [file] });
  });
  watcher.onDidCreate(update);
  watcher.onDidChange(update);
  watcher.onDidDelete(
    logged((uri: vscode.Uri) => send({ removed: [uri.toString()] })),
  );

  const fullSync = logged(async () => {
    const uris: vscode.Uri[] = [];
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
      await collectSourceFiles(folder.uri, uris);
    }
    const files = await Promise.all(uris.map(readSourceFile));
    await send({
      replace: true,
      files: files.filter((f): f is SyncedFile => f !== undefined),
    });
  });
  context.subscriptions.push(
    vscode.workspace.onDidChangeWorkspaceFolders(fullSync),
  );
  await fullSync();
}

/** Recursively collect `.star`/`.wasm` files under `dir`. */
async function collectSourceFiles(dir: vscode.Uri, out: vscode.Uri[]) {
  let entries: [string, vscode.FileType][];
  try {
    entries = await vscode.workspace.fs.readDirectory(dir);
  } catch {
    return;
  }
  for (const [name, type] of entries) {
    if (isSkippedName(name)) continue;
    const uri = vscode.Uri.joinPath(dir, name);
    if (type & vscode.FileType.Directory) {
      await collectSourceFiles(uri, out);
    } else if (/\.(star|wasm)$/.test(name)) {
      out.push(uri);
    }
  }
}

async function readSourceFile(
  uri: vscode.Uri,
): Promise<SyncedFile | undefined> {
  let bytes: Uint8Array;
  try {
    bytes = await vscode.workspace.fs.readFile(uri);
  } catch {
    return undefined;
  }
  if (uri.path.endsWith(".wasm")) {
    let binary = "";
    for (let i = 0; i < bytes.length; i += 0x8000) {
      binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
    }
    return { uri: uri.toString(), base64: btoa(binary) };
  }
  return { uri: uri.toString(), text: new TextDecoder().decode(bytes) };
}

// ----------------------------------------------------------------------------
// Tree-sitter highlighter

async function activateTreeSitter(context: vscode.ExtensionContext) {
  // NOTE: readFile returns some kind of evil Uint8Array that's missing methods, so we wrap it.
  const treeSitterWasmBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(context.extensionUri, "dist", tree_sitter_wasm),
    ),
  );
  const treeSitterStarstreamWasmBytes = new Uint8Array(
    await vscode.workspace.fs.readFile(
      vscode.Uri.joinPath(
        context.extensionUri,
        "dist",
        tree_sitter_starstream_wasm,
      ),
    ),
  );

  await Parser.init({
    async instantiateWasm(
      imports: WebAssembly.Imports,
      // NOTE: One spot in Emscripten's output has this in `(mod, inst)` order, which is a lie.
      cb: (inst: WebAssembly.Instance, mod: WebAssembly.Module) => void,
    ) {
      try {
        const { module, instance } = await WebAssembly.instantiate(
          treeSitterWasmBytes,
          imports,
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
