import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import Layout from "@theme/Layout";
//import type * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import {
  Dispatch,
  ReactNode,
  Ref,
  SetStateAction,
  useEffect,
  useRef,
  useState,
} from "react";
import { useDocusaurusTheme } from "../hooks";
import tree_sitter_wasm from "file-loader!../../node_modules/web-tree-sitter/tree-sitter.wasm";
import tree_sitter_starstream_wasm from "file-loader!../../../starstream-dsl/tree-sitter-starstream/tree-sitter-starstream.wasm";
import highlights_scm from "file-loader!../../starstream-dsl/tree-sitter-starstream/queries/highlights.scm";

import { configureDefaultWorkerFactory } from "monaco-languageclient/workerFactory";
import type { MonacoVscodeApiConfig } from "monaco-languageclient/vscodeApiWrapper";
import { MonacoEditorReactComp } from "@typefox/monaco-editor-react";
import { LanguageClientConfig } from "monaco-languageclient/lcwrapper";
import { EditorAppConfig } from "monaco-languageclient/editorApp";

const languageId = "starstream";
const code = "var foo = 2 + 2;\nif (7 > 9) {\n    foo = 17;\n}\n";

function Editor() {
  const theme =
    useDocusaurusTheme() === "dark"
      ? "Default Dark Modern"
      : "Default Light Modern";

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

  // const languageClientConfig: LanguageClientConfig = {
  //   languageId,
  //   connection: {
  //     options: {
  //       $type: "WorkerConfig",
  //     }
  //   }
  // }

  const editorAppConfig: EditorAppConfig = {
    codeResources: {
      modified: {
        text: code,
        uri: "file:///sandbox.star",
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

function Tabs(props: {
  current: string;
  setCurrent: Dispatch<SetStateAction<string>>;
  className?: string;
  style?: React.CSSProperties;
  tabs: { key: string; title?: ReactNode; body?: ReactNode }[];
  after?: ReactNode;
  right?: ReactNode;
}) {
  const { current, setCurrent } = props;
  useEffect(() => {
    setCurrent((current) => {
      if (!props.tabs.some((tab) => current === tab.key)) {
        current = props.tabs[0].key;
      }
      return current;
    });
  }, [current, JSON.stringify(props.tabs.map((tab) => tab.key))]);
  return (
    <div
      className={`flex--grow flex--column ${props.className}`}
      style={props.style}
    >
      <div className="sandbox-tabs__list">
        {props.tabs.map((tab) => (
          <button
            key={tab.key}
            type="button"
            onClick={() => setCurrent(tab.key)}
            className={`sandbox-tabs__button ${
              current === tab.key ? "sandbox-tabs__button--current" : ""
            }`}
          >
            {tab.title ?? tab.key}
          </button>
        ))}
        <div className="flex--grow">{props.after}</div>
        <div>{props.right}</div>
      </div>
      {/* Put all tabs in the DOM upfront to avoid unmounting/remounting the Monaco editor constantly. */}
      {props.tabs.map((tab) => (
        <div
          className="sandbox-tabs__area"
          key={tab.key}
          hidden={tab.key !== current}
        >
          {tab.body}
        </div>
      ))}
    </div>
  );
}

export function Sandbox() {
  const [inputTab, setInputTab] = useState("");
  const [inputLedgerState, setInputLedgerState] = useState({});

  const [outputTab, setOutputTab] = useState("");
  const [busy, setBusy] = useState(false);
  const [compilerLog, setCompilerLog] = useState({
    log: "",
    warnings: 0,
    errors: 0,
  });

  const startTime = useRef(0);
  const request_id = useRef(0);

  return (
    <div className="flex--grow row">
      <div className="col col--6 flex--column">
        <Tabs
          current={inputTab}
          setCurrent={setInputTab}
          tabs={[
            {
              key: "Editor",
              body: <Editor />,
            },
          ]}
        />
      </div>
      <div className="col col--6 flex--column">
        <Tabs
          current={outputTab}
          setCurrent={setOutputTab}
          tabs={[
            {
              key: "About",
              body: (
                <div className="margin--sm">
                  <h1>Starstream Sandbox</h1>
                </div>
              ),
            },
          ]}
        />
      </div>
    </div>
  );
}

export default function SandboxPage() {
  return (
    <Layout title="Sandbox">
      <div className="flex--grow margin-vert--md margin-horiz--md flex--column">
        <Sandbox />
      </div>
    </Layout>
  );
}
