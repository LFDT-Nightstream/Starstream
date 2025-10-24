import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import Layout from "@theme/Layout";
import type * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
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

// Monaco setup
if (ExecutionEnvironment.canUseDOM) {
  window.MonacoEnvironment = {
    getWorkerUrl(_moduleId: any, label: string): string {
      console.log("getWorkerUrl", _moduleId, label);
      return "";
    },
  };

  (async () => {
    const monaco = await import("monaco-editor/esm/vs/editor/editor.api.js");
    monaco.languages.register({
      id: "starstream",
      extensions: [".star"],
    });
  })();
}

function setRef<T>(ref: Ref<T> | undefined, value: T) {
  if (ref === null || ref === undefined) {
    // Nothing to do.
  } else if ("current" in ref) {
    ref.current = value;
  } else if (ref instanceof Function) {
    ref(value);
  }
}

function Editor(props: {
  ref?: Ref<monaco.editor.IStandaloneCodeEditor>;
  theme?: string;
}) {
  const div = useRef<HTMLDivElement>(null);
  useEffect(() => {
    let editor: monaco.editor.IStandaloneCodeEditor | undefined;
    (async () => {
      const monaco = await import("monaco-editor/esm/vs/editor/editor.api.js");
      editor = monaco.editor.create(div.current!, {
        automaticLayout: true,
        theme: props.theme,

        // value: await Object.values(examples)[0](),
        language: "starstream",
      });
      setRef(props.ref, editor);
    })();
    return () => editor?.dispose();
  }, []);

  useEffect(() => {
    (async () => {
      const monaco = await import("monaco-editor/esm/vs/editor/editor.api.js");
      if (props.theme) {
        monaco.editor.setTheme(props.theme);
      }
    })();
  }, [props.theme]);

  return <div className="flex--grow" ref={div} />;
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
  const editor = useRef<monaco.editor.IStandaloneCodeEditor>(null);
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

  const theme = useDocusaurusTheme();

  return (
    <div className="flex--grow row">
      <div className="col col--6 flex--column">
        <Tabs
          current={inputTab}
          setCurrent={setInputTab}
          tabs={[
            {
              key: "Editor",
              body: (
                <Editor
                  ref={editor}
                  theme={theme === "dark" ? "vs-dark" : "vs"}
                />
              ),
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
