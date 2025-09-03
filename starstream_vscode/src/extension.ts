import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";

export async function activate(context: vscode.ExtensionContext) {
  const lc = new LanguageClient(
    "starstream-lsp",
    {
      command: context.asAbsolutePath("../target/debug/starstream_lsp"),
      transport: TransportKind.stdio,
    },
    {
      documentSelector: [{ scheme: "file", language: "starstream" }],
    }
  );
  context.subscriptions.push(lc);
  await lc.start();
}
