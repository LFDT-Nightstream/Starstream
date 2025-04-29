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

if (ExecutionEnvironment.canUseDOM) {
  window.MonacoEnvironment = {
    getWorkerUrl(_moduleId: any, label: string): string {
      console.log("getWorkerUrl", _moduleId, label);
      return "";
    },
  };
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

function Editor(props: { ref?: Ref<monaco.editor.IStandaloneCodeEditor> }) {
  const div = useRef<HTMLDivElement>(null);
  //const editor = useRef<monaco.editor.IStandaloneCodeEditor>(null);
  useEffect(() => {
    let editor: monaco.editor.IStandaloneCodeEditor | undefined;
    (async () => {
      const monaco = await import("monaco-editor/esm/vs/editor/editor.api.js");
      editor = monaco.editor.create(div.current!, {
        value: "Hello, world!",
        language: "typescript",
      });
      setRef(props.ref, editor);
    })();
    return () => editor?.dispose();
  }, []);

  return <div style={{ flexGrow: 1 }} ref={div} />;
}

function Tabs(props: {
  current: string;
  setCurrent: Dispatch<SetStateAction<string>>;
  className?: string;
  style?: React.CSSProperties;
  tabs: { key: string; title?: ReactNode; body?: ReactNode }[];
  extra?: ReactNode;
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
      /* Ugly flex style stuff should be moved to a .css file later */
      style={Object.assign(
        { flexGrow: 1, display: "flex", flexDirection: "column" },
        props.style
      )}
      className={props.className}
    >
      <div
        style={{
          lineHeight: 0,
          display: "flex",
        }}
      >
        {props.tabs.map((tab) => (
          <button
            key={tab.key}
            type="button"
            onClick={() => setCurrent(tab.key)}
            style={{
              border: "1px solid lightgray",
              borderBottomColor: current === tab.key ? "white" : "lightgray",
              background: current === tab.key ? "none" : "",
              fontWeight: "bold",
              fontSize: "inherit",
              cursor: "pointer",
            }}
          >
            {tab.title ?? tab.key}
          </button>
        ))}
        <div style={{ borderBottom: "1px solid lightgray", flexGrow: 1 }}>
          {props.extra}
        </div>
      </div>
      <div
        style={{
          border: "1px solid lightgray",
          borderTop: "none",
          display: "flex",
          flexGrow: 1,
          flexBasis: 0,
          overflowY: "auto",
        }}
        key={current}
      >
        {props.tabs.find((tab) => current === tab.key)?.body}
      </div>
    </div>
  );
}

export function Sandbox() {
  const editor = useRef<monaco.editor.IStandaloneCodeEditor>(null);
  const [outputTab, setOutputTab] = useState("");
  const [compilerLog, setCompilerLog] = useState("");

  return (
    <div style={{ flexGrow: 1, display: "flex" }}>
      <div
        className="margin-horiz--md"
        style={{ flex: 1, display: "flex", flexDirection: "column" }}
      >
        <Tabs
          current="Contract Code"
          setCurrent={(_: string) => {}}
          tabs={[
            {
              key: "Contract Code",
              body: <Editor ref={editor} />,
            },
          ]}
          extra={
            <>
              <button
                type="button"
                onClick={() => {
                  setCompilerLog(editor.current?.getModel()?.getValue() ?? "");
                  setOutputTab("Compile log");
                }}
              >
                Fribitz
              </button>
            </>
          }
        />
      </div>
      <div
        className="margin-horiz--md"
        style={{
          flex: 1,
          overflowY: "auto",
          display: "flex",
          flexDirection: "column",
        }}
      >
        <Tabs
          current={outputTab}
          setCurrent={setOutputTab}
          tabs={[
            {
              key: "About",
              body: (
                <div className="margin-horiz--sm">
                  <h1>Starstream Sandbox</h1>
                </div>
              ),
            },
            {
              key: "Compile log",
              body: (
                <div className="margin-horiz--sm margin-vert--sm">
                  <pre>{compilerLog}</pre>
                </div>
              ),
            },
            {
              key: "AST",
            },
            {
              key: "WASM",
            },
            {
              key: "Run log",
            },
            {
              key: "Sequence diagram",
            },
            {
              key: "Ledger state",
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
      <div className="margin-vert--md" style={{ flexGrow: 1, display: "flex" }}>
        <Sandbox />
      </div>
    </Layout>
  );
}
