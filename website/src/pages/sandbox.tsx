import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import {
  ComponentProps,
  Dispatch,
  ReactNode,
  SetStateAction,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import type {
  SandboxWorkerRequest,
  SandboxWorkerResponse,
} from "../sandbox.worker";
import { useBlobUrl } from "../hooks";
import type * as monaco from "monaco-editor";

function useSandboxWorker(onResponse: (r: SandboxWorkerResponse) => void): {
  request(r: SandboxWorkerRequest): void;
  restart(): void;
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
    restart() {
      worker.current!.terminate();
      worker.current = new Worker(
        new URL("../sandbox.worker", import.meta.url),
      );
    },
  };
}

// Wrapper to load `../editor.tsx` only in the browser.
function Editor(props: ComponentProps<typeof import("../editor").Editor>) {
  const [editorModule, setEditorModule] =
    useState<typeof import("../editor")>();
  useEffect(() => {
    import("../editor").then(setEditorModule);
  }, []);
  return editorModule ? <editorModule.Editor {...props} /> : null;
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
      className={`flex--grow flex--column sandbox-tabs ${
        props.className ?? ""
      }`}
      style={props.style}
    >
      <div className="sandbox-tabs__header">
        {props.tabs.map((tab) => (
          <button
            key={tab.key}
            type="button"
            onClick={() => setCurrent(tab.key)}
            className={`sandbox-tabs__btn ${
              current === tab.key ? "sandbox-tabs__btn--active" : ""
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
          className="sandbox-tabs__content"
          key={tab.key}
          hidden={tab.key !== current}
          style={{
            display: tab.key === current ? "flex" : "none",
          }}
        >
          {tab.body}
        </div>
      ))}
    </div>
  );
}

function DiagnosticsList({
  markers,
  onDiagnosticClick,
}: {
  markers: monaco.editor.IMarker[];
  onDiagnosticClick: (marker: monaco.editor.IMarker) => void;
}) {
  if (markers.length === 0) {
    return (
      <div className="padding--md sandbox-diagnostics__empty">
        <div
          style={{
            width: 10,
            height: 10,
            borderRadius: "50%",
            background: "currentColor",
          }}
        />
        No problems found.
      </div>
    );
  }

  return (
    <div className="padding--md sandbox-diagnostics">
      {markers.map((marker, i) => (
        <div
          key={i}
          onClick={() => onDiagnosticClick(marker)}
          className={`sandbox-diagnostics__item ${
            marker.severity === 8
              ? "sandbox-diagnostics__item--error"
              : "sandbox-diagnostics__item--warning"
          }`}
        >
          <div className="sandbox-diagnostics__header">
            <span style={{ fontWeight: "bold", textTransform: "uppercase" }}>
              {marker.severity === 8 ? "Error" : "Warning"}
            </span>
            <span style={{ fontFamily: "monospace" }}>
              Ln {marker.startLineNumber}, Col {marker.startColumn}
            </span>
          </div>
          <div className="sandbox-diagnostics__message">{marker.message}</div>
        </div>
      ))}
    </div>
  );
}

export function Sandbox() {
  const [inputTab, setInputTab] = useState("Editor");
  const [outputTab, setOutputTab] = useState("Diagnostics");

  const [busy, setBusy] = useState(false);
  const [wat, setWat] = useState("");
  const [wit, setWit] = useState("");
  const [markers, setMarkers] = useState<monaco.editor.IMarker[]>([]);
  const [editorInstance, setEditorInstance] =
    useState<monaco.editor.ICodeEditor | null>(null);

  const [coreWasm, setCoreWasm] = useState<Uint8Array<ArrayBuffer>>();
  const [componentWasm, setComponentWasm] = useState<Uint8Array<ArrayBuffer>>();

  const coreWasmUrl = useBlobUrl(
    coreWasm,
    "starstream_sandbox.core.wasm",
    "application/wasm",
  );
  const componentWasmUrl = useBlobUrl(
    componentWasm,
    "starstream_sandbox.component.wasm",
    "application/wasm",
  );
  const witBlob = useMemo(() => new TextEncoder().encode(wit), [wit]);
  const witUrl = useBlobUrl(witBlob, "starstream_sandbox.wit", "text/plain");

  const request_id = useRef(0);
  const worker = useSandboxWorker((response) => {
    if (response.request_id !== request_id.current) {
      console.log("discarding response for old request", response);
      return;
    }

    if (response.type == "idle") {
      setBusy(false);
    } else if (response.type == "log") {
      // Ignored in favor of LSP diagnostics
    } else if (response.type == "wat") {
      setWat(response.wat);
    } else if (response.type == "core_wasm") {
      setCoreWasm(response.bytes);
      setComponentWasm(undefined); // In case of error.
      setWit("");
    } else if (response.type == "wit") {
      setWit(response.wit);
    } else if (response.type == "component_wasm") {
      setComponentWasm(response.bytes);
    } else {
      response satisfies never;
    }
  });

  const onTextChanged = useCallback((code: string) => {
    worker.request({ request_id: ++request_id.current, code });
  }, []);

  const onMarkersChange = useCallback((newMarkers: monaco.editor.IMarker[]) => {
    setMarkers(newMarkers);
  }, []);

  const onEditorMount = useCallback((editor: monaco.editor.ICodeEditor) => {
    setEditorInstance(editor);
  }, []);

  const onDiagnosticClick = useCallback(
    (marker: monaco.editor.IMarker) => {
      if (editorInstance) {
        editorInstance.revealLineInCenter(marker.startLineNumber);
        editorInstance.setPosition({
          lineNumber: marker.startLineNumber,
          column: marker.startColumn,
        });
        editorInstance.focus();
      }
    },
    [editorInstance],
  );

  return (
    <div className="flex--grow sandbox-container">
      <div className="sandbox-panel">
        <Tabs
          current={inputTab}
          setCurrent={setInputTab}
          tabs={[
            {
              key: "Editor",
              body: (
                <Editor
                  onTextChanged={onTextChanged}
                  onMarkersChange={onMarkersChange}
                  onMount={onEditorMount}
                />
              ),
            },
          ]}
          style={{ height: "100%" }}
        />
      </div>
      <div className="sandbox-panel">
        <Tabs
          current={outputTab}
          setCurrent={setOutputTab}
          style={{ height: "100%" }}
          tabs={[
            {
              key: "Diagnostics",
              body: (
                <DiagnosticsList
                  markers={markers}
                  onDiagnosticClick={onDiagnosticClick}
                />
              ),
            },
            {
              key: "WAT",
              body: (
                <div
                  className="padding--md"
                  style={{ height: "100%", overflow: "auto" }}
                >
                  <pre>
                    <AnsiHtml text={wat} />
                  </pre>
                </div>
              ),
            },
            {
              key: "WIT",
              body: (
                <div
                  className="padding--md"
                  style={{ height: "100%", overflow: "auto" }}
                >
                  {wit ? (
                    <pre>{wit}</pre>
                  ) : (
                    <div style={{ color: "var(--ifm-color-emphasis-500)" }}>
                      No WIT output available
                    </div>
                  )}
                </div>
              ),
            },
            {
              key: "Downloads",
              body: (
                <div className="padding--md">
                  <h3>Downloads</h3>
                  <ul style={{ listStyle: "none", padding: 0 }}>
                    {coreWasmUrl && (
                      <li style={{ marginBottom: 8 }}>
                        <a
                          href={coreWasmUrl}
                          download
                          className="button button--secondary button--block"
                        >
                          Download Core .wasm
                        </a>
                      </li>
                    )}
                    {componentWasmUrl && (
                      <li style={{ marginBottom: 8 }}>
                        <a
                          href={componentWasmUrl}
                          download
                          className="button button--secondary button--block"
                        >
                          Download Component .wasm
                        </a>
                      </li>
                    )}
                    {witUrl && (
                      <li style={{ marginBottom: 8 }}>
                        <a
                          href={witUrl}
                          download
                          className="button button--secondary button--block"
                        >
                          Download .wit
                        </a>
                      </li>
                    )}
                  </ul>
                  {!coreWasmUrl && !componentWasmUrl && !witUrl && (
                    <p>Compile the code to see download options.</p>
                  )}
                </div>
              ),
            },
            {
              key: "About",
              body: (
                <div className="padding--md">
                  <h1>Starstream Sandbox</h1>
                  <p>Tabs:</p>
                  <ul>
                    <li>
                      Diagnostics: Compiler errors and warnings from the editor.
                    </li>
                    <li>
                      Wasm: The output of the Starstream compiler targeting
                      WebAssembly. Updates live.
                    </li>
                  </ul>
                  <p>Keyboard shortcuts:</p>
                  <ul>
                    <li>Ctrl+S to format</li>
                  </ul>
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
    <Layout title="Sandbox" noFooter>
      <div
        className="flex--grow flex--column"
        style={{ height: "100%", overflow: "hidden" }}
      >
        <Sandbox />
      </div>
    </Layout>
  );
}
