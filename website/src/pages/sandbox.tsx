import Layout from "@theme/Layout";
import { AnsiHtml } from "fancy-ansi/react";
import {
  ComponentProps,
  CSSProperties,
  Dispatch,
  Fragment,
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
import type {
  RunWorkerRequest,
  RunWorkerResponse,
  AbiEvent,
  Describe,
  DescribeFunc,
  DescribeInstance,
} from "../run.worker";
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

function useRunWorker(onResponse: (r: RunWorkerResponse) => void): {
  request(r: RunWorkerRequest): void;
} {
  const worker = useRef<Worker | null>(null);

  useEffect(() => {
    worker.current = new Worker(new URL("../run.worker", import.meta.url));
    return () => worker.current?.terminate();
  }, []);

  useEffect(() => {
    const current = worker.current;
    if (!current) return;

    function onMessage({ data }: { data: RunWorkerResponse }) {
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

// ----------------------------------------------------------------------------
// Run tab: deploy a compiled component, mint UTXOs by calling `[static]`
// constructors (the only way to create one) and invoke methods on them.

const INT_KINDS = new Set(["s8", "u8", "s16", "u16", "s32", "u32", "s64", "u64"]);

// Parse a raw input string into the JSON value the runtime expects for `kind`.
// Starstream's only scalar types are integers and `bool`; everything compound
// (records, tuples, options/results and other enums) arrives as "json".
function readArg(kind: string, raw: string): unknown {
  if (kind === "bool") return raw === "true";
  // Blank integer fields default to 0 rather than rejecting the submit.
  if (INT_KINDS.has(kind)) {
    return Number(raw.trim() === "" ? "0" : raw);
  }
  // "json": records, tuples, options/results and other enums, as raw JSON.
  if (raw.trim() === "") throw new Error("missing JSON value");
  return JSON.parse(raw);
}

// A deployed component, identified by the sha256 hex digest of its Wasm.
interface RunDeployment {
  digest: string;
  describe: Describe;
  deployedAt: Date;
}

// A live UTXO handle, minted from a deployment's constructor.
interface RunUtxo {
  digest: string;
  /** Export name of the UTXO-owning instance. */
  instance: string;
  /** Handle number, as reported by the worker. */
  handle: number;
  /** Display label, e.g. `score-progress #0`. */
  label: string;
}

// One entry in a UTXO's call log: an emitted ABI event, a method return value,
// or a call error.
type LogItem =
  | { kind: "event"; instance: string; name: string; params: unknown[] }
  | { kind: "result"; label: string; text: string }
  | { kind: "error"; label: string; text: string };

// The key a UTXO's storage / implemented-methods / log are stored under.
function utxoKey(digest: string, handle: number): string {
  return `${digest}#${handle}`;
}

async function sha256Hex(bytes: Uint8Array): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", bytes as BufferSource);
  return [...new Uint8Array(digest)]
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

// A short, human label for an instance export id (`ns:pkg/iface@ver` → `iface`).
function shortInstance(name: string): string {
  const afterSlash = name.includes("/")
    ? name.slice(name.lastIndexOf("/") + 1)
    : name;
  return afterSlash.split("@")[0];
}

// The trailing segment of an export name (`[method]utxo.plus-chips` →
// `plus-chips`), used to label a call in the log.
function exportLabel(name: string): string {
  return name.split(".").pop() ?? name;
}

// A two-column grid that lines up the labels and inputs of the params inside.
const FIELD_GRID: CSSProperties = {
  display: "grid",
  gridTemplateColumns: "max-content 1fr",
  gap: 8,
  alignItems: "center",
  justifyItems: "start",
  marginTop: 8,
};

// Text inputs fill their grid cell and shrink with it on narrow layouts.
const INPUT_STYLE: CSSProperties = {
  width: "100%",
  minWidth: 0,
  boxSizing: "border-box",
};

// An input widget for one parameter, chosen by its scalar `kind`. Compound
// types arrive as "json" and get a raw-JSON textarea.
function KindInput({
  kind,
  value,
  onChange,
}: {
  kind: string;
  value: string;
  onChange: (value: string) => void;
}) {
  if (kind === "bool") {
    return (
      <input
        type="checkbox"
        checked={value === "true"}
        onChange={(e) => onChange(String(e.target.checked))}
      />
    );
  }
  if (kind === "json") {
    return (
      <textarea
        style={{ ...INPUT_STYLE, fontFamily: "monospace", minHeight: "3rem" }}
        placeholder="JSON value"
        value={value}
        onChange={(e) => onChange(e.target.value)}
      />
    );
  }
  // The only remaining scalar kinds are integers.
  return (
    <input
      type="number"
      step="1"
      style={INPUT_STYLE}
      placeholder={kind}
      value={value}
      onChange={(e) => onChange(e.target.value)}
      onFocus={(e) => e.target.select()}
    />
  );
}

// One callable function (constructor or method): a labelled row of parameter
// inputs and a submit button. Calls `onSubmit` with the JSON-decoded args.
function FuncForm({
  func,
  submitLabel,
  disabled,
  disabledReason,
  onSubmit,
}: {
  func: DescribeFunc;
  submitLabel: string;
  disabled?: boolean;
  disabledReason?: string;
  onSubmit: (args: unknown[]) => void;
}) {
  const [args, setArgs] = useState<Record<string, string>>({});
  const [error, setError] = useState<string>();
  const setArg = useCallback((name: string, value: string) => {
    setArgs((prev) => ({ ...prev, [name]: value }));
  }, []);

  return (
    <form
      className="run-func"
      onSubmit={(e) => {
        e.preventDefault();
        let values: unknown[];
        try {
          values = func.params.map((p) => readArg(p.kind, args[p.name] ?? ""));
        } catch (err) {
          setError(`Invalid input: ${(err as Error).message ?? err}`);
          return;
        }
        setError(undefined);
        onSubmit(values);
      }}
    >
      <div className="run-func__head">
        <span className="run-func__name">
          <code>{func.label}</code>
        </span>
        <button
          type="submit"
          className="button button--primary button--sm"
          disabled={disabled}
        >
          {submitLabel}
        </button>
      </div>
      {func.params.length > 0 && (
        <div style={FIELD_GRID}>
          {func.params.map((p) => (
            <Fragment key={p.name}>
              <span>
                {p.name}: <code>{p.kind}</code>
              </span>
              <KindInput
                kind={p.kind}
                value={args[p.name] ?? (INT_KINDS.has(p.kind) ? "0" : "")}
                onChange={(v) => setArg(p.name, v)}
              />
            </Fragment>
          ))}
        </div>
      )}
      {disabled && disabledReason && (
        <div className="run-func__hint">{disabledReason}</div>
      )}
      {error && <div className="run-func__error">{error}</div>}
    </form>
  );
}

// One entry of a UTXO's call log.
function LogLine({ item }: { item: LogItem }) {
  if (item.kind === "event") {
    const args = item.params.map((p) => JSON.stringify(p)).join(", ");
    return (
      <div className="run-log__item run-log__event">
        <span className="run-log__tag">event</span>
        <span className="run-log__sig">
          {item.instance}.{item.name}
        </span>
        <span className="run-log__detail">({args})</span>
      </div>
    );
  }
  return (
    <div
      className={`run-log__item ${
        item.kind === "error" ? "run-log__err" : "run-log__ok"
      }`}
    >
      <span className="run-log__tag">{item.kind === "error" ? "✗" : "→"}</span>
      <span className="run-log__sig">{item.label}</span>
      <span className="run-log__detail">{item.text}</span>
    </div>
  );
}

// One live UTXO: storage view, callable methods (only those declared via
// `implements-method` are enabled), a drop button and a call log.
function UtxoCard({
  utxo,
  instance,
  storage,
  implemented,
  log,
  onCall,
  onDrop,
}: {
  utxo: RunUtxo;
  instance: DescribeInstance;
  storage: Record<string, unknown> | undefined;
  implemented: string[] | undefined;
  log: LogItem[];
  onCall: (method: string, args: unknown[]) => void;
  onDrop: () => void;
}) {
  const implementedSet = useMemo(
    () => (implemented ? new Set(implemented) : null),
    [implemented],
  );

  // Keep the log pinned to the most recent entry as calls and events arrive.
  const logRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const el = logRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [log]);

  return (
    <div className="run-utxo">
      <div className="run-utxo__head">
        <span className="run-utxo__title">{utxo.label}</span>
        <button
          type="button"
          className="button button--secondary button--sm"
          onClick={onDrop}
        >
          Drop
        </button>
      </div>
      {instance.storage && (
        <div className="run-utxo__section">
          <span className="run-subhead">Storage</span>
          <pre className="run-utxo__storage">
            {storage ? JSON.stringify(storage, null, 2) : "…"}
          </pre>
        </div>
      )}
      {instance.methods.length > 0 && (
        <div className="run-utxo__section">
          <span className="run-subhead">Methods</span>
          {instance.methods.map((m) => {
            // `implementedSet` is null until the implements-method list loads;
            // keep methods disabled until then, since which are callable is
            // still unknown.
            const loading = implementedSet === null;
            const enabled = implementedSet ? implementedSet.has(m.export) : false;
            return (
              <FuncForm
                key={m.export}
                func={m}
                submitLabel="Call"
                disabled={!enabled}
                disabledReason={
                  loading
                    ? "Loading implemented methods…"
                    : enabled
                      ? undefined
                      : "Not declared via implements-method — not callable."
                }
                onSubmit={(args) => onCall(m.export, args)}
              />
            );
          })}
        </div>
      )}
      <div className="run-utxo__section">
        <span className="run-subhead">Log — events &amp; return values</span>
        {log.length === 0 ? (
          <div className="run-log__empty">No calls or events yet.</div>
        ) : (
          <div className="run-log" ref={logRef}>
            {log.map((item, i) => (
              <LogLine key={i} item={item} />
            ))}
          </div>
        )}
      </div>
    </div>
  );
}

// The Cardano context form for the selected deployment. Applies to UTXOs minted
// *after* a change.
function CardanoForm({
  digest,
  onSetCardano,
}: {
  digest: string;
  onSetCardano: (
    digest: string,
    blockHeight: number,
    currentSlot: number,
  ) => void;
}) {
  const [blockHeight, setBlockHeight] = useState("0");
  const [currentSlot, setCurrentSlot] = useState("0");

  // Reset the fields when the selected deployment changes.
  useEffect(() => {
    setBlockHeight("0");
    setCurrentSlot("0");
  }, [digest]);

  const apply = (bh: string, cs: string) =>
    onSetCardano(digest, Number(bh || "0"), Number(cs || "0"));

  return (
    <fieldset className="run-cardano">
      <legend>Cardano context</legend>
      <label>
        block height
        <input
          type="number"
          step="1"
          value={blockHeight}
          onChange={(e) => {
            setBlockHeight(e.target.value);
            apply(e.target.value, currentSlot);
          }}
        />
      </label>
      <label>
        current slot
        <input
          type="number"
          step="1"
          value={currentSlot}
          onChange={(e) => {
            setCurrentSlot(e.target.value);
            apply(blockHeight, e.target.value);
          }}
        />
      </label>
    </fieldset>
  );
}

function RunPanel({
  canDeploy,
  deployHint,
  deployErrors,
  onDeploy,
  deployments,
  onConstruct,
  utxos,
  utxoStorage,
  utxoImplemented,
  utxoLogs,
  onCall,
  onDrop,
  onSetCardano,
  log,
}: {
  canDeploy: boolean;
  /** Why deploying is unavailable, shown next to the disabled button. */
  deployHint: ReactNode | undefined;
  /** Errors of the last failed deploy, shown as a warning under the button. */
  deployErrors: string[];
  onDeploy: () => void;
  deployments: RunDeployment[];
  onConstruct: (
    digest: string,
    instance: string,
    constructor: string,
    args: unknown[],
  ) => void;
  utxos: RunUtxo[];
  utxoStorage: Record<string, Record<string, unknown>>;
  utxoImplemented: Record<string, string[]>;
  utxoLogs: Record<string, LogItem[]>;
  onCall: (
    digest: string,
    handle: number,
    method: string,
    args: unknown[],
  ) => void;
  onDrop: (digest: string, handle: number) => void;
  onSetCardano: (
    digest: string,
    blockHeight: number,
    currentSlot: number,
  ) => void;
  /** Log lines not attributable to a UTXO (deploy/construct). */
  log: string[];
}) {
  const [selectedDigest, setSelectedDigest] = useState("");

  // Deployments are ordered most recent first; select the newest one whenever a
  // deployment happens.
  useEffect(() => {
    setSelectedDigest(deployments[0]?.digest ?? "");
  }, [deployments]);

  const deployment = deployments.find((d) => d.digest === selectedDigest);

  return (
    <div className="padding--md" style={{ height: "100%", overflow: "auto" }}>
      <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
        <button
          type="button"
          className="button button--secondary"
          disabled={!canDeploy}
          onClick={onDeploy}
        >
          Deploy
        </button>
        {deployHint && <span>{deployHint}</span>}
      </div>
      {deployErrors.length > 0 && (
        <div
          className="alert alert--warning"
          role="alert"
          style={{ marginTop: 8 }}
        >
          <strong>Deployment failed.</strong>
          <pre style={{ margin: "8px 0 0", whiteSpace: "pre-wrap" }}>
            {deployErrors.join("\n")}
          </pre>
        </div>
      )}
      {deployments.length > 0 && (
        <div
          style={{
            marginTop: 16,
            display: "flex",
            gap: 16,
            alignItems: "flex-end",
            flexWrap: "wrap",
          }}
        >
          <label>
            Deployment{" "}
            <select
              value={selectedDigest}
              onChange={(e) => setSelectedDigest(e.target.value)}
            >
              {deployments.map((d) => (
                <option key={d.digest} value={d.digest}>
                  {d.digest.slice(0, 16)}… — deployed at{" "}
                  {d.deployedAt.toLocaleTimeString()}
                </option>
              ))}
            </select>
          </label>
          {selectedDigest && (
            <CardanoForm digest={selectedDigest} onSetCardano={onSetCardano} />
          )}
        </div>
      )}
      {deployment && deployment.describe.instances.length === 0 && (
        <p style={{ marginTop: 16 }}>No UTXO defined.</p>
      )}
      {deployment?.describe.instances.map((instance) => {
        const live = utxos.filter(
          (u) => u.digest === selectedDigest && u.instance === instance.name,
        );
        return (
          <div key={instance.name} className="run-instance">
            <h3 className="run-instance__title">
              <span className="run-badge">{instance.resource}</span>{" "}
              <code>{shortInstance(instance.name)}</code>
            </h3>
            <div className="run-construct">
              <span className="run-subhead">Construct</span>
              {instance.constructors.length === 0 ? (
                <p>This resource exposes no constructors.</p>
              ) : (
                instance.constructors.map((c) => (
                  <FuncForm
                    key={c.export}
                    func={c}
                    submitLabel="Create UTXO"
                    onSubmit={(args) =>
                      onConstruct(selectedDigest, instance.name, c.export, args)
                    }
                  />
                ))
              )}
            </div>
            <div className="run-live">
              <span className="run-subhead">Live UTXOs</span>
              {live.length === 0 ? (
                <div className="run-log__empty">
                  None yet — call a constructor above to mint one.
                </div>
              ) : (
                live.map((u) => (
                  <UtxoCard
                    key={u.handle}
                    utxo={u}
                    instance={instance}
                    storage={utxoStorage[utxoKey(u.digest, u.handle)]}
                    implemented={utxoImplemented[utxoKey(u.digest, u.handle)]}
                    log={utxoLogs[utxoKey(u.digest, u.handle)] ?? []}
                    onCall={(method, args) =>
                      onCall(u.digest, u.handle, method, args)
                    }
                    onDrop={() => onDrop(u.digest, u.handle)}
                  />
                ))
              )}
            </div>
          </div>
        );
      })}
      {log.length > 0 && <pre style={{ marginTop: 16 }}>{log.join("\n")}</pre>}
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

  const [deployments, setDeployments] = useState<RunDeployment[]>([]);
  const [utxos, setUtxos] = useState<RunUtxo[]>([]);
  // Per-UTXO storage, implemented methods and call log, keyed by `utxoKey`.
  const [utxoStorage, setUtxoStorage] = useState<
    Record<string, Record<string, unknown>>
  >({});
  const [utxoImplemented, setUtxoImplemented] = useState<
    Record<string, string[]>
  >({});
  const [utxoLogs, setUtxoLogs] = useState<Record<string, LogItem[]>>({});
  const [runLog, setRunLog] = useState<string[]>([]);

  // Monotonic per-(deployment, instance) counter for UTXO labels, kept across drops.
  const utxoCounters = useRef<Map<string, number>>(new Map());

  const appendUtxoLog = useCallback((key: string, item: LogItem) => {
    setUtxoLogs((prev) => ({ ...prev, [key]: [...(prev[key] ?? []), item] }));
  }, []);

  // The digest of the current compiler output, identifying its deployment.
  const [componentDigest, setComponentDigest] = useState<string>();
  useEffect(() => {
    setComponentDigest(undefined);
    if (!componentWasm) return;
    let cancelled = false;
    sha256Hex(componentWasm).then((digest) => {
      if (!cancelled) setComponentDigest(digest);
    });
    return () => {
      cancelled = true;
    };
  }, [componentWasm]);

  // The digest last deployed; Deploy is disabled until the Wasm digest changes.
  const [lastDeployedDigest, setLastDeployedDigest] = useState<string>();

  // Errors of the last failed deploy, shown next to the Deploy button.
  const [deployErrors, setDeployErrors] = useState<string[]>([]);
  useEffect(() => {
    setDeployErrors([]);
  }, [componentWasm]);

  // The in-flight deploy, to attribute its error logs to the deploy warning.
  const pendingDeploy = useRef<string | undefined>(undefined);

  // The in-flight method call, to attribute its logs/result to its UTXO.
  const pendingCall = useRef<
    { key: string; digest: string; handle: number; label: string } | undefined
  >(undefined);

  const run_request_id = useRef(0);
  const runWorker = useRunWorker((response) => {
    if (response.type == "idle") {
      // The request fully completed (its result responses, if any, arrived
      // before this). Stop attributing later logs to it, so a subsequent
      // operation that doesn't reset the refs (e.g. setCardano) can't be
      // mis-attributed to this one.
      pendingCall.current = undefined;
      pendingDeploy.current = undefined;
    } else if (response.type == "log") {
      const level = ["", "Error", "Warn", "Info", "Debug", "Trace"][
        response.level
      ];
      console.log(level, `[${response.target}]`, response.body);
      // Surface errors and warnings, attributed to the UTXO being called if
      // there is one, else to the deploy warning or the panel-level log.
      if (response.level <= 2) {
        const entry = `${level}: ${response.body}`;
        const pending = pendingCall.current;
        if (pending) {
          appendUtxoLog(pending.key, {
            kind: "error",
            label: pending.label,
            text: entry,
          });
        } else if (pendingDeploy.current !== undefined) {
          setDeployErrors((prev) => [...prev, entry]);
        } else {
          setRunLog((prev) => [...prev, entry]);
        }
      }
    } else if (response.type == "deployed") {
      pendingDeploy.current = undefined;
      const { digest, describe } = response;
      const deployment = { digest, describe, deployedAt: new Date() };
      // Most recent deployment first; a re-deploy moves its digest up front.
      setDeployments((prev) => [
        deployment,
        ...prev.filter((d) => d.digest !== digest),
      ]);
    } else if (response.type == "deploy_failed") {
      pendingDeploy.current = undefined;
      // The cause arrived as "log" responses; ensure the warning shows anyway.
      setDeployErrors((prev) => (prev.length > 0 ? prev : ["Unknown error."]));
      // Re-enable the Deploy button that onDeploy optimistically disabled.
      setLastDeployedDigest((prev) =>
        prev === response.digest ? undefined : prev,
      );
    } else if (response.type == "constructed") {
      const { digest, instance, handle, events } = response;
      const counterKey = `${digest}|${instance}`;
      const n = utxoCounters.current.get(counterKey) ?? 0;
      utxoCounters.current.set(counterKey, n + 1);
      const label = `${shortInstance(instance)} #${n}`;
      const key = utxoKey(digest, handle);
      setUtxos((prev) => [...prev, { digest, instance, handle, label }]);
      setUtxoLogs((prev) => ({
        ...prev,
        [key]: [
          { kind: "result", label: "constructed", text: "(minted)" },
          ...events.map(
            (e: AbiEvent): LogItem => ({
              kind: "event",
              instance: e.instance,
              name: e.name,
              params: e.params,
            }),
          ),
        ],
      }));
      // Fetch the new UTXO's initial storage and its callable methods.
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "storageGet",
        digest,
        handle,
      });
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "implementedMethods",
        digest,
        handle,
      });
    } else if (response.type == "construct_failed") {
      // The cause arrived as "log" responses (attributed to the panel log).
    } else if (response.type == "called") {
      const pending = pendingCall.current;
      if (pending) {
        const text =
          response.results.length > 0
            ? JSON.stringify(response.results)
            : "(no return value)";
        const items: LogItem[] = [
          { kind: "result", label: pending.label, text },
          ...response.events.map(
            (e: AbiEvent): LogItem => ({
              kind: "event",
              instance: e.instance,
              name: e.name,
              params: e.params,
            }),
          ),
        ];
        setUtxoLogs((prev) => ({
          ...prev,
          [pending.key]: [...(prev[pending.key] ?? []), ...items],
        }));
        // The call may have mutated storage — refresh the view.
        runWorker.request({
          request_id: ++run_request_id.current,
          type: "storageGet",
          digest: pending.digest,
          handle: pending.handle,
        });
      }
    } else if (response.type == "storage") {
      const key = utxoKey(response.digest, response.handle);
      setUtxoStorage((prev) => ({ ...prev, [key]: response.storage }));
    } else if (response.type == "implemented") {
      const key = utxoKey(response.digest, response.handle);
      setUtxoImplemented((prev) => ({ ...prev, [key]: response.methods }));
    } else if (response.type == "dropped") {
      const { digest, handle } = response;
      const key = utxoKey(digest, handle);
      setUtxos((prev) =>
        prev.filter((u) => !(u.digest === digest && u.handle === handle)),
      );
      const remove = <T,>(rec: Record<string, T>) => {
        if (!(key in rec)) return rec;
        const next = { ...rec };
        delete next[key];
        return next;
      };
      setUtxoStorage(remove);
      setUtxoImplemented(remove);
      setUtxoLogs(remove);
    } else {
      response satisfies never;
    }
  });

  const onDeploy = useCallback(() => {
    if (!componentWasm || !componentDigest) return;
    setLastDeployedDigest(componentDigest);
    pendingCall.current = undefined;
    pendingDeploy.current = componentDigest;
    setDeployErrors([]);
    setRunLog([]);
    runWorker.request({
      request_id: ++run_request_id.current,
      type: "deploy",
      digest: componentDigest,
      component: componentWasm,
    });
  }, [componentWasm, componentDigest]);

  const onConstruct = useCallback(
    (digest: string, instance: string, ctor: string, args: unknown[]) => {
      pendingCall.current = undefined;
      pendingDeploy.current = undefined;
      setRunLog([]);
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "construct",
        digest,
        instance,
        ctor,
        args,
      });
    },
    [],
  );

  const onCall = useCallback(
    (digest: string, handle: number, method: string, args: unknown[]) => {
      const key = utxoKey(digest, handle);
      const label = `${exportLabel(method)}(${args
        .map((a) => JSON.stringify(a))
        .join(", ")})`;
      pendingCall.current = { key, digest, handle, label };
      pendingDeploy.current = undefined;
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "call",
        digest,
        handle,
        method,
        args,
      });
    },
    [],
  );

  const onDrop = useCallback((digest: string, handle: number) => {
    pendingCall.current = undefined;
    pendingDeploy.current = undefined;
    runWorker.request({
      request_id: ++run_request_id.current,
      type: "drop",
      digest,
      handle,
    });
  }, []);

  const onSetCardano = useCallback(
    (digest: string, blockHeight: number, currentSlot: number) => {
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "setCardano",
        digest,
        blockHeight,
        currentSlot,
      });
    },
    [],
  );

  // Whether the current output is deployed, and whether a deploy is in flight.
  const deployed =
    componentDigest !== undefined &&
    deployments.some((d) => d.digest === componentDigest);
  const deploying =
    !deployed &&
    componentDigest !== undefined &&
    componentDigest === lastDeployedDigest;

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
                      Wasm/WAT: The output of the Starstream compiler targeting
                      WebAssembly. Updates live.
                    </li>
                    <li>
                      WIT: The WebAssembly interface that is generated by the
                      compiler. Updates live.
                    </li>
                    <li>
                      Downloads: Buttons for downloading the compiler outputs.
                    </li>
                    <li>
                      Run: Deploy the compiled component, mint UTXOs by calling a
                      constructor, and invoke methods on them.
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
            {
              key: "Run",
              body: (
                <RunPanel
                  canDeploy={
                    !deployed && !deploying && componentDigest !== undefined
                  }
                  deployHint={
                    !componentWasm ? (
                      "Compile the code to deploy it."
                    ) : deploying ? (
                      "Deploying…"
                    ) : deployed ? (
                      <>
                        Contract deployed as{" "}
                        <code style={{ overflowWrap: "anywhere" }}>
                          {componentDigest}
                        </code>
                      </>
                    ) : undefined
                  }
                  deployErrors={deployErrors}
                  onDeploy={onDeploy}
                  deployments={deployments}
                  onConstruct={onConstruct}
                  utxos={utxos}
                  utxoStorage={utxoStorage}
                  utxoImplemented={utxoImplemented}
                  utxoLogs={utxoLogs}
                  onCall={onCall}
                  onDrop={onDrop}
                  onSetCardano={onSetCardano}
                  log={runLog}
                />
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
