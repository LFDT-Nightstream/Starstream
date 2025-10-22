import { Parser } from "web-tree-sitter";
import * as vscode from "vscode";
import tree_sitter_wasm from "file-loader!../node_modules/web-tree-sitter/tree-sitter.wasm";
import tree_sitter_starstream_wasm from "file-loader!../../tree-sitter-starstream/tree-sitter-starstream.wasm";
import highlights_scm from "file-loader!../../tree-sitter-starstream/queries/highlights.scm";
import { realpath } from "fs/promises";
import { registerProvider } from "./tree-sitter-vscode";

export async function activate(context: vscode.ExtensionContext) {
  await Promise.all([
    activateTreeSitter(context),
    activateLanguageClient(context),
  ]);
}

async function resolve(
  context: vscode.ExtensionContext,
  fname: string
): Promise<string> {
  return `${await realpath(context.extensionPath)}/dist/${fname}`;
}

// ----------------------------------------------------------------------------
// LSP client

async function activateLanguageClient(_context: vscode.ExtensionContext) {
  // TODO
}

// ----------------------------------------------------------------------------
// Tree-sitter highlighter

async function activateTreeSitter(context: vscode.ExtensionContext) {
  const fullTsWasm = await resolve(context, tree_sitter_wasm);
  await Parser.init({
    locateFile(name: string, _dir: string): string | null {
      if (name === "tree-sitter.wasm") {
        return fullTsWasm;
      }
      return null;
    },
  });

  // Use an embedded copy of https://marketplace.visualstudio.com/items?itemName=AlecGhost.tree-sitter-vscode
  // Until https://github.com/microsoft/vscode/issues/50140
  const provider = registerProvider([
    {
      lang: "starstream",
      parser: await resolve(context, tree_sitter_starstream_wasm),
      highlights: await resolve(context, highlights_scm),
      injectionOnly: false,
    },
  ]);
  context.subscriptions.push(provider);
}
