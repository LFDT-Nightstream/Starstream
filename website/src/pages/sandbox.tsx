import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
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

  (async () => {
    const monaco = await import("monaco-editor/esm/vs/editor/editor.api.js");
    monaco.languages.register({
      id: "starstream",
      extensions: [".star"],
    });
    monaco.languages.setMonarchTokensProvider("starstream", {
      keywords:
        "if|try|with|while|loop|yield|raise|fail|resume|return|utxo|script|token|abi|impl|main|storage|bind|unbind|fn|let|mut|true|false".split(
          "|"
        ),
      tokenizer: {
        root: [
          [
            // Lowercase identifiers and keywords
            /[a-z_$][\w$]*/,
            { cases: { "@keywords": "keyword", "@default": "identifier" } },
          ],
          [
            // Capital identifiers get highlighted like types
            /[A-Z][\w\$]*/,
            "type.identifier",
          ],
          [/\/\*.*?\*\//, "comment"],
          [/\/\/.*?$/, "comment"],
        ],
      },
    } satisfies monaco.languages.IMonarchLanguage);
  })();
}

interface SandboxWasmImports extends WebAssembly.ModuleImports {
  read_input(ptr: number, len: number): void;
  set_compiler_log(ptr: number, len: number): void;
  set_ast(ptr: number, len: number): void;
  set_wat(ptr: number, len: number): void;
}

interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  run(input_len: number): void;
}

let modulePromise: Promise<WebAssembly.WebAssemblyInstantiatedSource> | null =
  null;
async function getWasmInstance(
  env: SandboxWasmImports
): Promise<SandboxWasmExports> {
  if (modulePromise === null) {
    // First fetch gets instantiateStreaming privilege.
    modulePromise = WebAssembly.instantiateStreaming(
      fetch("../starstream_sandbox.wasm"),
      { env }
    );
    const { instance } = await modulePromise;
    return instance.exports as unknown as SandboxWasmExports;
  } else {
    // Future fetches use synchronous instantiation of fetched module.
    const { module } = await modulePromise;
    return new WebAssembly.Instance(module, { env })
      .exports as unknown as SandboxWasmExports;
  }
}

function useWasmInstance(
  env: SandboxWasmImports
): React.RefObject<SandboxWasmExports | null> {
  const exports = useRef<SandboxWasmExports>(null);
  useEffect(() => {
    getWasmInstance(env).then((x) => (exports.current = x));
  }, []);
  return exports;
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
        value: `token MyToken {

}
utxo MyUtxo {

}
script {
    fn example() {
        return;
    }
}`,
        language: "starstream",
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
  const [ast, setAst] = useState("");
  const [wat, setWat] = useState("");

  const input = useRef<Uint8Array>(null);
  const wasm = useWasmInstance({
    read_input(ptr, len) {
      console.log("read_input", ptr, len, "->", input.current);
      new Uint8Array(wasm.current!.memory.buffer, ptr, len).set(input.current!);
      console.log(new TextDecoder().decode(input.current!));
      console.log(
        new TextDecoder().decode(
          wasm.current!.memory.buffer.slice(ptr, ptr + len)
        )
      );
    },
    set_compiler_log(ptr, len) {
      console.log("set_compiler_log", ptr, len);
      setOutputTab("Compile log");
      setCompilerLog(
        new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        )
      );
    },
    set_ast(ptr, len) {
      console.log("set_ast", ptr, len);
      setOutputTab("AST");
      setAst(
        new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        )
      );
    },
    set_wat(ptr, len) {
      setOutputTab("WASM");
      setWat(
        new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        )
      );
    },
  });

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
                  input.current = new TextEncoder().encode(
                    editor.current?.getModel()?.getValue() ?? ""
                  );
                  wasm.current?.run(input.current.length);
                }}
              >
                Compile
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
                  <pre>
                    <AnsiHtml text={compilerLog} />
                  </pre>
                </div>
              ),
            },
            {
              key: "AST",
              body: (
                <div className="margin-horiz--sm margin-vert--sm">
                  <pre>{ast}</pre>
                </div>
              ),
            },
            {
              key: "WASM",
              body: (
                <div className="margin-horiz--sm margin-vert--sm">
                  <pre>{wat}</pre>
                </div>
              ),
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
