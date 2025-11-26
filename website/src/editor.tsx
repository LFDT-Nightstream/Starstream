import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import * as monaco from "monaco-editor";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import "./starstream.vsix";

const code = `\
struct Token {
    amount: i64,
    price: i64,
}

script fn total_value(token: Token) -> i64 {
    token.amount * token.price
}
`;

// Doesn't seem to need to be global, but changes don't take effect, so it might as well be.
const vscodeApiConfig: MonacoVscodeApiConfig = {
  $type: "extended",
  viewsConfig: {
    $type: "EditorService",
  },
  userConfiguration: {
    json: JSON.stringify({
      "workbench.colorTheme": theme(),
      "editor.wordBasedSuggestions": "off",
      "editor.formatOnSave": true,
    }),
  },
  monacoWorkerFactory: configureDefaultWorkerFactory,
};

// MUST be global to avoid MonacoEditorReactComp double-initializing.
const editorAppConfig: EditorAppConfig = {
  codeResources: {
    modified: {
      text: code,
      uri: "/workspace/sandbox.star",
    },
  },
};

export function Editor(props: {
  onTextChanged?: (text: string) => void;
  onMarkersChange?: (markers: monaco.editor.IMarker[]) => void;
  onMount?: (editor: monaco.editor.ICodeEditor) => void;
}) {
  return (
    <MonacoEditorReactComp
      style={{ width: "100%", height: "100%" }}
      vscodeApiConfig={vscodeApiConfig}
      editorAppConfig={editorAppConfig}
      onTextChanged={(contents) =>
        props.onTextChanged
          ? props.onTextChanged(contents.modified ?? "")
          : null
      }
      onVscodeApiInitDone={(api) => {
        startWatchingTheme();
        if (props.onMarkersChange) {
          const updateMarkers = () => {
            // Get all markers. Since this is a sandbox, we don't need to filter strictly.
            // This avoids issues where the URI scheme might differ (e.g. file:// vs inmemory://).
            const markers = monaco.editor.getModelMarkers({});
            props.onMarkersChange?.(markers);
          };
          // Initial check
          updateMarkers();
          // Watch for changes
          monaco.editor.onDidChangeMarkers(() => {
            updateMarkers();
          });
        }

        // We need to get the editor instance.
        // MonacoEditorReactComp doesn't seem to expose onMount directly in the types used here?
        // But usually it does. Let's try to find the editor.
        // Since we are using monaco-languageclient wrapper, it might be different.
        // However, we can use monaco.editor.getEditors()[0] as a fallback if needed,
        // but let's try to see if we can grab it.
        // Actually, looking at the code, we don't have a direct ref.
        // Let's use a small timeout to grab the focused editor or just the first one.
        if (props.onMount) {
          // Wait for editor to be created
          setTimeout(() => {
            const editors = monaco.editor.getEditors();
            if (editors.length > 0) {
              props.onMount?.(editors[0]);
            }
          }, 100);
        }
      }}
    />
  );
}

function startWatchingTheme() {
  const mo = new MutationObserver((records) => {
    for (const each of records) {
      if (
        each.target === document.documentElement &&
        each.type === "attributes" &&
        each.attributeName === "data-theme"
      ) {
        monaco.editor.setTheme(theme());
      }
    }
  });
  mo.observe(document.documentElement, { attributes: true });
}

function theme() {
  return document.documentElement.getAttribute("data-theme") === "dark"
    ? "Default Dark Modern"
    : "Default Light Modern";
}
