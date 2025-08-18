import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment";
import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import type * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";
import {
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
import { MermaidDiagram } from "../mermaid";

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
        "if|else|try|with|while|loop|yield|raise|fail|resume|return|assert|utxo|script|token|abi|impl|main|storage|mint|burn|bind|unbind|fn|let|let mut|true|false|event|error|effect|typedef|const".split(
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

function useSandboxWorker(onResponse: (r: SandboxWorkerResponse) => void): {
  request(r: SandboxWorkerRequest): void;
  terminate(): void;
} {
  const worker = useRef<Worker>(null);
  useEffect(() => {
    worker.current = new Worker(new URL("../sandbox.worker", import.meta.url));
    return () => worker.current!.terminate();
  }, []);
  useEffect(() => {
    function onMessage({ data }: { data: SandboxWorkerResponse }) {
      onResponse(data);
    }
    worker.current!.addEventListener("message", onMessage);
    return () => worker.current!.removeEventListener("message", onMessage);
  }, [onResponse]);
  return {
    request(r) {
      worker.current!.postMessage(r);
    },
    terminate() {
      worker.current!.terminate();
      worker.current = new Worker(
        new URL("../sandbox.worker", import.meta.url)
      );
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

const LOG_PREFIX = ["", "ERROR", "WARN ", "INFO ", "DEBUG", "TRACE"];

function formatDuration(ms: number) {
  const s = Math.floor(ms / 1000);
  const parts = ["", "", "", "", s % 60, "s"];
  const m = Math.floor(s / 60);
  if (m > 0) {
    parts[3] = "m";
    parts[2] = m % 60;
  }
  const h = Math.floor(m / 60);
  if (h > 0) {
    parts[1] = "h";
    parts[0] = h;
  }
  return parts.join("");
}

interface RunLog {
  time: string;
  level: number;
  target: string;
  body: string;
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
  const [runLog, setRunLog] = useState<RunLog[]>([]);
  const [sequenceDiagram, setSequenceDiagram] = useState("");

  const [proofFile, setProofFile] = useState<ArrayBuffer | null>(null);
  const [proofFileUrl, setProofFileUrl] = useState<string | null>(null);
  useEffect(() => {
    if (proofFile) {
      const url = URL.createObjectURL(
        new File([proofFile], "proof.cbor", { type: "application/cbor" })
      );
      setProofFileUrl(url);
      return () => URL.revokeObjectURL(url);
    } else {
      setProofFileUrl(null);
    }
  }, [proofFile]);

  const [prove, setProve] = useState(false);

  const startTime = useRef(0);
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
      setRunLog([]);
    } else if ("append_run_log" in response) {
      setRunLog((l) => [
        ...l,
        {
          time: formatDuration(Date.now() - startTime.current),
          level: response.append_run_log,
          target: response.target.replaceAll("::", "\u200B::"),
          body: response.body,
        },
      ]);
    } else if ("idle" in response) {
      setBusy(false);
    } else if ("sequence_diagram" in response) {
      setSequenceDiagram(response.sequence_diagram);
    } else if ("set_proof_file" in response) {
      setProofFile(response.set_proof_file);
    } else {
      response satisfies never;
    }
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
            <div className="flex" style={{ gap: 8, alignItems: "baseline" }}>
              {busy ? (
                <>
                  Working...
                  <button
                    type="button"
                    onClick={() => {
                      worker.terminate();
                      setBusy(false);
                    }}
                  >
                    Stop
                  </button>
                </>
              ) : null}
              <label>
                <input
                  type="checkbox"
                  checked={prove}
                  onChange={(e) => setProve(e.currentTarget.checked)}
                />
                Prove
              </label>
              <button
                type="button"
                onClick={() => {
                  request_id.current += 1;
                  startTime.current = Date.now();
                  setBusy(true);
                  setProofFile(null);
                  worker.request({
                    request_id: request_id.current,
                    input: editor.current?.getModel()?.getValue() ?? "",
                    run: true,
                    prove,
                  });
                }}
              >
                Compile & Run
              </button>
            </div>
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
                  <pre>
                    <AnsiHtml text={wat} />
                  </pre>
                </div>
              ),
            },
            {
              key: "Run log",
              body: (
                <table className="margin--sm log-table">
                  <thead>
                    <tr>
                      <td></td>
                      <td>Level</td>
                      <td>Module</td>
                      <td>Message</td>
                    </tr>
                  </thead>
                  <tbody>
                    {runLog.map((l, i) => (
                      <tr key={i}>
                        <td className="log-table__time">{l.time}</td>
                        <td
                          className={`log-table__level log-table__level--${l.level}`}
                        >
                          {LOG_PREFIX[l.level]}
                        </td>
                        <td className="log-table__target">{l.target}</td>
                        <td className="log-table__body">{l.body}</td>
                      </tr>
                    ))}
                  </tbody>
                  <tfoot>
                    {proofFile && proofFileUrl ? (
                      <tr>
                        <td></td>
                        <td colSpan={3}>
                          <a
                            href={proofFileUrl}
                            download="starstream-proof.cbor"
                          >
                            Download proof file (CBOR, {proofFile.byteLength} B)
                          </a>
                        </td>
                      </tr>
                    ) : null}
                    <tr>
                      <td></td>
                      <td colSpan={3}>{busy ? "Working..." : "Done"}</td>
                    </tr>
                  </tfoot>
                </table>
              ),
            },
            {
              key: "Sequence diagram",
              body: <MermaidDiagram text={sequenceDiagram} />,
            },
            /*
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
