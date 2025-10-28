import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import tree_sitter_starstream_wasm from "file-loader!../../tree-sitter-starstream/tree-sitter-starstream.wasm";
import tree_sitter_wasm from "file-loader!../node_modules/web-tree-sitter/tree-sitter.wasm";
import highlights_scm from "file-loader!../../tree-sitter-starstream/queries/highlights.scm";
import { EditorAppConfig } from "monaco-languageclient/editorApp";
import { LanguageClientConfig } from "monaco-languageclient/lcwrapper";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import { useDocusaurusTheme } from "./hooks";

const languageId = "starstream";
const code = "var foo = 2 + 2;\nif (7 > 9) {\n    foo = 17;\n}\n";

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
      }),
    },
    monacoWorkerFactory: configureDefaultWorkerFactory,
  };

  const languageClientConfig: LanguageClientConfig = {
    languageId,
    connection: {
      options: {
        $type: "WorkerDirect",
        // NOTE: `new Worker(new URL(...))` is important to make Webpack do the right thing.
        worker: new Worker(new URL("./language-server.ts", import.meta.url), {
          type: "module",
        }),
      },
    },
    clientOptions: {},
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
      languageClientConfig={languageClientConfig}
    />
  );
}
