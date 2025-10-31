import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import * as monaco from "monaco-editor";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import "./starstream.vsix";

const code = "let foo = 2 + 2;\nif (7 > 9) {\n    foo = 17;\n}\n";

export function Editor(props: { onTextChanged?: (text: string) => void }) {
  // NOTE: no hooks allowed here or MonacoEditorReactComp will double-init
  // and bad stuff will happen. Seems like a flaw in the MonacoEditorReactComp
  // implementation, but it's the best we've got.

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

  const editorAppConfig: EditorAppConfig = {
    codeResources: {
      modified: {
        text: code,
        uri: "/workspace/sandbox.star",
      },
    },
  };

  return (
    <MonacoEditorReactComp
      className="flex--grow"
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
