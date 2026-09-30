import getExplorerServiceOverride from "@codingame/monaco-vscode-explorer-service-override";
import {
  FileType,
  InMemoryFileSystemProvider,
  registerFileSystemOverlay,
} from "@codingame/monaco-vscode-files-service-override";
import { updateUserConfiguration } from "@codingame/monaco-vscode-configuration-service-override";
import getMarkersServiceOverride from "@codingame/monaco-vscode-markers-service-override";
import getSearchServiceOverride from "@codingame/monaco-vscode-search-service-override";
import * as monaco from "monaco-editor";
import {
  MonacoVscodeApiWrapper,
  type MonacoVscodeApiConfig,
} from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import { useEffect, useRef, useState } from "react";
import * as vscode from "vscode";
import "./starstream.vsix";

/** Root of the playground's in-memory workspace. */
const WORKSPACE = "/workspace";
/** Compiled when the active editor isn't a `.star` file. */
const DEFAULT_ENTRY = `${WORKSPACE}/main.star`;

const STARTER_FILES: Record<string, string> = {
  "main.star": `\
import { total } from "./math.star";

abi Score {
    fn plus_chips(chips: u64);
    fn plus_mult(mult: u64);
    fn mult_mult(mult_pct: u64);
    fn finish();
    event Finish(total: u64);
}

utxo ScoreProgress {
    storage {
        let mut chips: u64;
        let mut mult: u64;
    }

    main fn new() {
        yield(Score);
        emit Finish(total(chips, mult));
    }

    impl Score {
        fn plus_chips(pub chips2: u64) {
            chips = chips + chips2;
        }
        fn plus_mult(pub mult2: u64) {
            mult = mult + mult2;
        }
        fn mult_mult(pub mult_pct: u64) {
            mult = mult * mult_pct / 100;
        }
        fn finish() {
            resume;
        }
    }
}
`,
  "math.star": `\
/// Final score: chips times mult.
fn total(chips: u64, mult: u64) -> u64 {
    chips * mult
}
`,
};

/** A snapshot of the workspace, for the compiler. */
export interface WorkspaceSnapshot {
  /** Absolute path -> contents of every `.star`/`.wasm` file. */
  files: Record<string, Uint8Array>;
  /** The file to compile. */
  entry: string;
}

// ----------------------------------------------------------------------------
// Workspace filesystem

const fileSystem = new InMemoryFileSystemProvider();

async function seedWorkspace() {
  const encoder = new TextEncoder();
  const opts = {
    create: true,
    overwrite: true,
    unlock: false,
    atomic: false as const,
  };
  await fileSystem.mkdir(monaco.Uri.file(WORKSPACE));
  for (const [name, text] of Object.entries(STARTER_FILES)) {
    await fileSystem.writeFile(
      monaco.Uri.file(`${WORKSPACE}/${name}`),
      encoder.encode(text),
      opts,
    );
  }
}

/** Recursively collect `.star`/`.wasm` files under `dir`. */
async function collectFiles(
  dir: monaco.Uri,
  out: Record<string, Uint8Array>,
): Promise<void> {
  for (const [name, type] of await fileSystem.readdir(dir)) {
    const uri = monaco.Uri.joinPath(dir, name);
    if (type & FileType.Directory) {
      if (!name.startsWith(".")) await collectFiles(uri, out);
    } else if (/\.(star|wasm)$/.test(name)) {
      out[uri.path] = await fileSystem.readFile(uri);
    }
  }
}

/** Saved files, overlaid with the unsaved contents of open editors. */
async function snapshot(): Promise<WorkspaceSnapshot> {
  const files: Record<string, Uint8Array> = {};
  await collectFiles(monaco.Uri.file(WORKSPACE), files);
  const encoder = new TextEncoder();
  for (const model of monaco.editor.getModels()) {
    if (model.uri.scheme === "file" && model.uri.path in files) {
      files[model.uri.path] = encoder.encode(model.getValue());
    }
  }

  const active = vscode.window.activeTextEditor?.document.uri;
  const entry =
    active?.scheme === "file" && active.path.endsWith(".star")
      ? active.path
      : DEFAULT_ENTRY;
  return { files, entry };
}

// ----------------------------------------------------------------------------
// Workbench setup (once per page load)

function userConfiguration() {
  return JSON.stringify({
    "workbench.colorTheme": theme(),
    "workbench.startupEditor": "none",
    "workbench.layoutControl.enabled": false,
    "window.commandCenter": true,
    "breadcrumbs.enabled": false,
    "workbench.activityBar.location": "top",
    "editor.wordBasedSuggestions": "off",
    "editor.minimap.enabled": false,
    "editor.formatOnSave": true,
    "files.autoSave": "off",
  });
}

// The workbench can only be initialized once, into one container, so keep
// that container around and move it into whichever React host is mounted.
let workbenchContainer: HTMLDivElement | undefined;
let workbenchReady: Promise<void> | undefined;

function startWorkbench(): Promise<void> {
  if (workbenchReady) return workbenchReady;

  workbenchContainer = document.createElement("div");
  workbenchContainer.className = "sandbox-workbench";

  const config: MonacoVscodeApiConfig = {
    $type: "extended",
    viewsConfig: {
      $type: "WorkbenchService",
      htmlContainer: workbenchContainer,
    },
    serviceOverrides: {
      ...getExplorerServiceOverride(),
      ...getSearchServiceOverride(),
      ...getMarkersServiceOverride(),
    },
    workspaceConfig: {
      workspaceProvider: {
        trusted: true,
        workspace: { folderUri: monaco.Uri.file(WORKSPACE) },
        async open() {
          return false;
        },
      },
      defaultLayout: {
        editors: [{ uri: monaco.Uri.file(DEFAULT_ENTRY) }],
        force: true,
      },
    },
    userConfiguration: { json: userConfiguration() },
    monacoWorkerFactory: configureDefaultWorkerFactory,
  };

  workbenchReady = (async () => {
    // Must be in place before the workbench reads the workspace.
    registerFileSystemOverlay(1, fileSystem);
    await seedWorkspace();
    await new MonacoVscodeApiWrapper(config).start();
    startWatchingTheme();
  })();
  return workbenchReady;
}

// ----------------------------------------------------------------------------
// React component

export function Editor(props: {
  /** Called with a fresh snapshot whenever a file or the active editor changes. */
  onWorkspaceChanged?: (snapshot: WorkspaceSnapshot) => void;
}) {
  const hostRef = useRef<HTMLDivElement>(null);
  const propsRef = useRef(props);
  propsRef.current = props;
  const [error, setError] = useState<string>();

  useEffect(() => {
    let disposed = false;
    const disposables: { dispose(): void }[] = [];

    startWorkbench().then(
      () => {
        if (disposed || !hostRef.current || !workbenchContainer) return;
        hostRef.current.append(workbenchContainer);
        // The workbench lays itself out on window resize.
        window.dispatchEvent(new Event("resize"));

        // Recompile on edits, saves, file creation/deletion and editor switches.
        let timer: number | undefined;
        const changed = () => {
          window.clearTimeout(timer);
          timer = window.setTimeout(async () => {
            const snap = await snapshot();
            if (!disposed) propsRef.current.onWorkspaceChanged?.(snap);
          }, 100);
        };
        const watchModel = (model: monaco.editor.ITextModel) =>
          disposables.push(model.onDidChangeContent(changed));
        monaco.editor.getModels().forEach(watchModel);
        disposables.push(
          monaco.editor.onDidCreateModel((model) => {
            watchModel(model);
            changed();
          }),
          fileSystem.onDidChangeFile(changed),
          vscode.window.onDidChangeActiveTextEditor(changed),
          { dispose: () => window.clearTimeout(timer) },
        );
        changed();
      },
      (e) => {
        // The workbench can't be re-initialized on the same page, so this is
        // final until a reload.
        console.error("failed to start the editor", e);
        if (!disposed) setError(String(e));
      },
    );

    return () => {
      disposed = true;
      disposables.forEach((d) => d.dispose());
      workbenchContainer?.remove();
    };
  }, []);

  if (error) {
    return (
      <div className="padding--md">
        <p>The editor failed to start. Try reloading the page.</p>
        <pre>{error}</pre>
      </div>
    );
  }
  return <div ref={hostRef} style={{ width: "100%", height: "100%" }} />;
}

// ----------------------------------------------------------------------------
// Theme

function startWatchingTheme() {
  const mo = new MutationObserver((records) => {
    for (const each of records) {
      if (
        each.target === document.documentElement &&
        each.type === "attributes" &&
        each.attributeName === "data-theme"
      ) {
        updateUserConfiguration(userConfiguration());
      }
    }
  });
  mo.observe(document.documentElement, {
    attributes: true,
    attributeFilter: ["data-theme"],
  });
}

function theme() {
  return document.documentElement.getAttribute("data-theme") === "dark"
    ? "Default Dark Modern"
    : "Default Light Modern";
}
