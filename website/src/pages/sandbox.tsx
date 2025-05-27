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
import type {
  SandboxWorkerRequest,
  SandboxWorkerResponse,
} from "../sandbox.worker";

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

let worker: Worker;
function useSandboxWorker(onResponse: (r: SandboxWorkerResponse) => void): {
  request(r: SandboxWorkerRequest): void;
} {
  worker ??= new Worker(new URL("../sandbox.worker", import.meta.url));
  useEffect(() => {
    function onMessage({ data }: { data: SandboxWorkerResponse }) {
      onResponse(data);
    }
    worker.addEventListener("message", onMessage);
    return () => worker.removeEventListener("message", onMessage);
  }, []);
  return {
    request(r) {
      worker.postMessage(r);
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
  const [busy, setBusy] = useState(false);
  const [compilerLog, setCompilerLog] = useState({
    log: "",
    warnings: 0,
    errors: 0,
  });
  const [ast, setAst] = useState("");
  const [wat, setWat] = useState("");
  const [runLog, setRunLog] = useState("");

  const request_id = useRef(0);
  const worker = useSandboxWorker((response) => {
    if (response.request_id != request_id.current) {
      console.log("discarding response for old request", response);
      return;
    }

    if ("compiler_log" in response) {
      setOutputTab("Compile log");
      setCompilerLog({
        log: response.compiler_log,
        warnings: response.warnings,
        errors: response.errors,
      });
    } else if ("ast" in response) {
      setOutputTab("AST");
      setAst(response.ast);
    } else if ("wat" in response) {
      setOutputTab("Wasm");
      setWat(response.wat);
    } else if ("run_log" in response) {
      setOutputTab("Run log");
      setRunLog(response.run_log);
    } else if ("idle" in response) {
      setBusy(false);
    }
  });

  /*
  const input = useRef<Uint8Array>(null);
  con;
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
  */

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
              {busy ? <>Working... </> : null}
              <button
                type="button"
                onClick={() => {
                  request_id.current += 1;
                  setBusy(true);
                  worker.request({
                    request_id: request_id.current,
                    input: editor.current?.getModel()?.getValue() ?? "",
                    run: true,
                  });
                }}
              >
                Compile & Run
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
