import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import type * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import {
  cache,
  CSSProperties,
  Dispatch,
  ReactNode,
  Ref,
  SetStateAction,
  useEffect,
  useRef,
  useState,
} from "react";
import examples from "../examples";
import { useDocusaurusTheme } from "../hooks";

import starstreamSandboxWasm from "file-loader!../../../target/wasm32-unknown-unknown/release/starstream_sandbox.wasm";

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
        "if|else|try|with|while|loop|yield|raise|fail|resume|return|utxo|script|token|abi|impl|main|storage|bind|unbind|fn|let|mut|true|false".split(
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
  getrandom(ptr: number, len: number): void;

  read_input(ptr: number, len: number): void;
  set_compiler_log(
    ptr: number,
    len: number,
    warnings: number,
    errors: number
  ): void;
  set_ast(ptr: number, len: number): void;
  set_wat(ptr: number, len: number): void;
  set_run_log(ptr: number, len: number): void;
}

interface SandboxWasmExports {
  memory: WebAssembly.Memory;
  run(input_len: number, run: boolean): void;
}

let modulePromise: Promise<WebAssembly.WebAssemblyInstantiatedSource> | null =
  null;
async function getWasmInstance(
  env: SandboxWasmImports
): Promise<SandboxWasmExports> {
  if (modulePromise === null) {
    // First fetch gets instantiateStreaming privilege.
    modulePromise = WebAssembly.instantiateStreaming(
      fetch(starstreamSandboxWasm),
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

        value: await Object.values(examples)[0](),
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
      <div className="sandbox-tabs__area" key={current}>
        {props.tabs.find((tab) => current === tab.key)?.body}
      </div>
    </div>
  );
}

function Builtins({ onChange }: { onChange: (code: string) => void }) {
  const [selectedOption, setSelectedOption] = useState("");

  const handleChange = async (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    setSelectedOption(value);

    examples[value]().then(onChange);
  };
  return (
    <div className="builtins-container">
      <select
        value={selectedOption}
        onChange={handleChange}
        className="builtins-select"
      >
        {Object.keys(examples).map((k) => (
          <option key={k} value={k}>
            {k}
          </option>
        ))}
      </select>
    </div>
  );
}

export function Sandbox() {
  const editor = useRef<monaco.editor.IStandaloneCodeEditor>(null);
  const [outputTab, setOutputTab] = useState("");
  const [compilerLog, setCompilerLog] = useState({
    log: "",
    warnings: 0,
    errors: 0,
  });
  const [ast, setAst] = useState("");
  const [wat, setWat] = useState("");
  const [runLog, setRunLog] = useState("");

  const input = useRef<Uint8Array>(null);
  const wasm = useWasmInstance({
    getrandom(ptr, len) {
      crypto.getRandomValues(
        new Uint8Array(wasm.current!.memory.buffer, ptr, len)
      );
    },

    read_input(ptr, len) {
      console.log("read_input", ptr, len);
      new Uint8Array(wasm.current!.memory.buffer, ptr, len).set(input.current!);
    },
    set_compiler_log(ptr, len, warnings, errors) {
      console.log("set_compiler_log", ptr, len);
      setOutputTab("Compile log");
      setCompilerLog({
        log: new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        ),
        warnings,
        errors,
      });
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
      console.log("set_wat", ptr, len);
      setOutputTab("Wasm");
      setWat(
        new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        )
      );
    },
    set_run_log(ptr, len) {
      console.log("set_run_log", ptr, len);
      setOutputTab("Run log");
      setRunLog(
        new TextDecoder().decode(
          new Uint8Array(wasm.current!.memory.buffer, ptr, len)
        )
      );
    },
  });

  const theme = useDocusaurusTheme();

  return (
    <div className="flex--grow row">
      <div className="col col--6 flex--column">
        <Tabs
          current="Contract Code"
          setCurrent={(_: string) => {}}
          tabs={[
            {
              key: "Contract Code",
              body: (
                <Editor
                  ref={editor}
                  theme={theme === "dark" ? "vs-dark" : "vs"}
                />
              ),
            },
          ]}
          after={
            <Builtins
              onChange={(value) => {
                editor.current?.setValue(value);
              }}
            />
          }
          right={
            <>
              <button
                type="button"
                onClick={() => {
                  input.current = new TextEncoder().encode(
                    editor.current?.getModel()?.getValue() ?? ""
                  );
                  wasm.current?.run(input.current.length, true);
                }}
              >
                Compile
              </button>
            </>
          }
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
            {
              key: "Compile log",
              title: (
                <span
                  style={
                    { "--ifm-badge-padding-vertical": "1px" } as CSSProperties
                  }
                >
                  Compile log
                  {compilerLog.warnings ? (
                    <>
                      {" "}
                      <span className="badge badge--warning">
                        {compilerLog.warnings}
                      </span>
                    </>
                  ) : null}
                  {compilerLog.errors ? (
                    <>
                      {" "}
                      <span className="badge badge--danger">
                        {compilerLog.errors}
                      </span>
                    </>
                  ) : null}
                </span>
              ),
              body: (
                <div className="margin--sm">
                  <pre style={{ whiteSpace: "pre-wrap" }}>
                    <AnsiHtml text={compilerLog.log} />
                  </pre>
                </div>
              ),
            },
            {
              key: "AST",
              body: (
                <div className="margin--sm">
                  <pre>{ast}</pre>
                </div>
              ),
            },
            {
              key: "Wasm",
              body: (
                <div className="margin--sm">
                  <pre>{wat}</pre>
                </div>
              ),
            },
            {
              key: "Run log",
              body: (
                <div className="margin--sm">
                  <pre style={{ whiteSpace: "pre-wrap" }}>
                    <AnsiHtml text={runLog} />
                  </pre>
                </div>
              ),
            },
            /*
            {
              key: "Sequence diagram",
            },
            {
              key: "Ledger state",
            },
            */
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
