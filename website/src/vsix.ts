import {
  ExtensionHostKind,
  registerExtension,
  RegisterExtensionResult,
} from "@codingame/monaco-vscode-api/extensions";
import { ZipReader, Uint8ArrayReader } from "@zip.js/zip.js";

export async function registerVsixExtension(
  vsixUrl: string
): Promise<RegisterExtensionResult> {
  const zipBuffer = await (await fetch(vsixUrl)).arrayBuffer();
  const zipReader = new ZipReader(
    new Uint8ArrayReader(new Uint8Array(zipBuffer))
  );

  const entries = await zipReader.getEntries();
  const manifestEntry = entries.find(
    (e) => e.filename === "extension/package.json"
  );
  if (!manifestEntry || manifestEntry.directory) {
    throw new Error("Missing package.json");
  }
  const manifest = JSON.parse(
    new TextDecoder().decode(await manifestEntry.arrayBuffer())
  );

  const ext = registerExtension(manifest, ExtensionHostKind.LocalProcess, {});
  for (const entry of entries) {
    const match = /^extension\/(.*)$/.exec(entry.filename);
    if (match && !entry.directory) {
      const data = await entry.arrayBuffer();
      try {
        ext.registerFileUrl(match[1], URL.createObjectURL(new Blob([data])));
      } catch (e) {
        // Generally caused by reregistering a file that already exists.
        console.warn(e.toString());
      }
    }
  }

  return ext;
}
