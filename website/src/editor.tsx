import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import { LanguageClientConfig } from "monaco-languageclient/lcwrapper";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import { useDocusaurusTheme } from "./hooks";
import "./starstream.vsix";

const code = "let foo = 2 + 2;\nif (7 > 9) {\n    foo = 17;\n}\n";

export function Editor() {
  // const theme =
  //   useDocusaurusTheme() === "dark"
  //     ? "Default Dark Modern"
  //     : "Default Light Modern";
  const theme = "Default Light Modern";

  const vscodeApiConfig: MonacoVscodeApiConfig = {
    $type: "extended",
    viewsConfig: {
      $type: "EditorService",
    },
    userConfiguration: {
      json: JSON.stringify({
        "workbench.colorTheme": theme,
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
    />
  );
}
