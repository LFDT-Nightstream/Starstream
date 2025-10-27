import { realpath } from "node:fs/promises";
import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";

export async function activate(context: vscode.ExtensionContext) {
  // TODO for publishing: bundle the LSP executable as part of the extension.
  // For now, we `cargo run` it assuming this extension is running out of its source directory.
  const lc = new LanguageClient(
    "starstream-lsp",
    {
      command: "cargo",
      args: ["run", "-p", "starstream_lsp", "--"],
      options: {
        cwd: `${await realpath(context.extensionPath)}/..`,
      },
      transport: TransportKind.stdio,
    },
    {
      documentSelector: [{ scheme: "file", language: "starstream" }],
    }
  );
  context.subscriptions.push(lc);
  await lc.start();
}
