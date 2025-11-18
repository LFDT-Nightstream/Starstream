import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import * as monaco from "monaco-editor";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import "./starstream.vsix";

const code = `\
script fn add(x: i64, y: i64) -> i64 {
    x + y
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

export function Editor(props: { onTextChanged?: (text: string) => void }) {
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
