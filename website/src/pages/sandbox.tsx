import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import {
  ComponentProps,
  CSSProperties,
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

const MARKER_SEVERITY_ERROR = 8;

function useSandboxWorker(onResponse: (r: SandboxWorkerResponse) => void): {
  request(r: SandboxWorkerRequest): void;
} {
  const worker = useRef<Worker | null>(null);

  useEffect(() => {
    worker.current = new Worker(new URL("../sandbox.worker", import.meta.url));
    return () => worker.current?.terminate();
  }, []);

  useEffect(() => {
    const current = worker.current;
    if (!current) return;

    function onMessage({ data }: { data: SandboxWorkerResponse }) {
      onResponse(data);
    }
    current.addEventListener("message", onMessage);
    return () => current.removeEventListener("message", onMessage);
  }, [onResponse]);

  return {
    request(r) {
      worker.current?.postMessage(r);
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
  style?: CSSProperties;
  tabs: { key: string; title?: ReactNode; body?: ReactNode }[];
  after?: ReactNode;
  right?: ReactNode;
}) {
  const { current, setCurrent, tabs } = props;

  useEffect(() => {
    setCurrent((prev) => {
      if (!tabs.some((tab) => tab.key === prev)) {
        return tabs[0]?.key ?? prev;
      }
      return prev;
    });
  }, [tabs, setCurrent]);

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
      {markers.map((marker) => {
        const key = `${marker.startLineNumber}:${marker.startColumn}:${marker.message}`;
        const isError = marker.severity === MARKER_SEVERITY_ERROR;
        return (
          <div
            key={key}
            onClick={() => onDiagnosticClick(marker)}
            className={`sandbox-diagnostics__item ${
              isError
                ? "sandbox-diagnostics__item--error"
                : "sandbox-diagnostics__item--warning"
            }`}
          >
            <div className="sandbox-diagnostics__header">
              <span style={{ fontWeight: "bold", textTransform: "uppercase" }}>
                {isError ? "Error" : "Warning"}
              </span>
              <span style={{ fontFamily: "monospace" }}>
                Ln {marker.startLineNumber}, Col {marker.startColumn}
              </span>
            </div>
            <div className="sandbox-diagnostics__message">{marker.message}</div>
          </div>
        );
      })}
    </div>
  );
}

export function Sandbox() {
  const [inputTab, setInputTab] = useState("Editor");
  const [outputTab, setOutputTab] = useState("About");

  const [wat, setWat] = useState("");
  const [wit, setWit] = useState("");
  const [markers, setMarkers] = useState<monaco.editor.IMarker[]>([]);
  const [editorInstance, setEditorInstance] =
    useState<monaco.editor.ICodeEditor | null>(null);

  const [coreWasm, setCoreWasm] = useState<Uint8Array>();
  const [componentWasm, setComponentWasm] = useState<Uint8Array>();

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
      // Idle response received
    } else if (response.type == "log") {
      console.log(
        ["", "Error", "Warn", "Info", "Debug", "Trace"][response.level],
        `[${response.target}]`,
        response.body,
      );
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
