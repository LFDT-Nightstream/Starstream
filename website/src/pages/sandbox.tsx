import Layout from "@theme/Layout";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import {
  PropsWithChildren,
  ReactNode,
  Ref,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
} from "react";

window.MonacoEnvironment = {
  getWorkerUrl(_moduleId: any, label: string): string {
    console.log("getWorkerUrl", _moduleId, label);
    return "";
  },
};

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
    const editor = monaco.editor.create(div.current!, {
      value: "Hello, world!",
      language: "typescript",
    });
    setRef(props.ref, editor);
    return () => editor.dispose();
  }, []);

  return <div style={{ flexGrow: 1 }} ref={div} />;
}

function Tabs(props: { tabs: Record<string, ReactNode> }) {
  const [tab, setTab] = useState("");
  return (
    <div>
      <div>
        {Object.keys(props.tabs).map((k) => (
          <button type="button" onClick={() => setTab(k)}>
            {k}
          </button>
        ))}
      </div>
    </div>
  );
}

export function Sandbox() {
  const editor = useRef<monaco.editor.IStandaloneCodeEditor>(null);
  return (
    <div style={{ flexGrow: 1, display: "flex" }}>
      <div
        className="margin-horiz--md"
        style={{ flex: 1, display: "flex", flexDirection: "column" }}
      >
        <h4>Contract Code</h4>
        <Editor ref={editor} />
      </div>
      <div className="margin-horiz--md" style={{ flex: 1, overflowY: "auto" }}>
        <button
          type="button"
          onClick={() => {
            console.log(editor.current?.getModel()?.getValue());
          }}
        >
          Fribitz
        </button>
        <h4>AST</h4>
        <h4>WAT</h4>
        <h4>Output log (compiler and VM)</h4>
        <h4>Sequence diagram</h4>
        <h4>Final ledger state</h4>
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
