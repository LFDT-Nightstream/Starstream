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
import type {
  RunWorkerRequest,
  RunWorkerResponse,
  WitFunction,
  WitResolve,
  WitType,
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

const NUMERIC_TYPES = new Set([
  "s8",
  "u8",
  "s16",
  "u16",
  "s32",
  "u32",
  "s64",
  "u64",
  "f32",
  "f64",
]);

/// A deployed component, identified by the sha256 hex digest of its Wasm.
interface RunDeployment {
  digest: string;
  wit: WitResolve;
  deployedAt: Date;
}

/// A live instance (UTXO) of a deployment.
interface RunInstance {
  digest: string;
  instance: number;
}

// The key call logs are stored under, one per instance.
function instanceKey(digest: string, instance: number): string {
  return `${digest}#${instance}`;
}

async function sha256Hex(bytes: Uint8Array): Promise<string> {
  const digest = await crypto.subtle.digest("SHA-256", bytes as BufferSource);
  return [...new Uint8Array(digest)]
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

// The deployed component's callable functions: its worlds' top-level function
// exports. (Starstream components export functions at the world level.)
function exportedFunctions(wit: WitResolve): WitFunction[] {
  return wit.worlds.flatMap((world) =>
    Object.values(world.exports).flatMap((item) =>
      "function" in item ? [item.function] : [],
    ),
  );
}

// The WIT spelling of a type: named types by name, anonymous compounds spelled
// out, anything else by its kind.
function typeStr(wit: WitResolve, type: WitType): string {
  if (typeof type === "string") return type;
  const def = wit.types[type];
  if (def.name) return def.name;
  const kind = def.kind;
  if (typeof kind === "string") return kind;
  if (kind.type !== undefined) return typeStr(wit, kind.type);
  if (kind.list !== undefined) return `list<${typeStr(wit, kind.list)}>`;
  if (kind.option !== undefined) return `option<${typeStr(wit, kind.option)}>`;
  if (kind.tuple) {
    return `tuple<${kind.tuple.types.map((t) => typeStr(wit, t)).join(", ")}>`;
  }
  if (kind.result) {
    const { ok, err } = kind.result;
    if (ok === null && err === null) return "result";
    return `result<${ok === null ? "_" : typeStr(wit, ok)}, ${
      err === null ? "_" : typeStr(wit, err)
    }>`;
  }
  return Object.keys(kind)[0] ?? "unknown";
}

// The WAVE encoding of one argument, built from the flat form state. Inputs
// for free-form types (lists, options, tuples, ...) are passed through as the
// WAVE the user typed.
function waveArg(
  wit: WitResolve,
  type: WitType,
  path: string,
  args: Record<string, string>,
): string {
  const value = args[path] ?? "";
  if (typeof type === "number") {
    const kind = wit.types[type].kind;
    if (typeof kind !== "string") {
      if (kind.type !== undefined) return waveArg(wit, kind.type, path, args);
      if (kind.record) {
        const fields = kind.record.fields.map(
          (f) =>
            `${f.name}: ${waveArg(wit, f.type, `${path}.${f.name}`, args)}`,
        );
        return `{${fields.join(", ")}}`;
      }
      if (kind.enum) {
        return value || (kind.enum.cases[0]?.name ?? "");
      }
      if (kind.variant) {
        const cases = kind.variant.cases;
        const selected = cases.find((c) => c.name === value) ?? cases[0];
        if (!selected) return value;
        return selected.type != null
          ? `${selected.name}(${waveArg(wit, selected.type, `${path}.${selected.name}`, args)})`
          : selected.name;
      }
    }
    return value;
  }
  if (type === "bool") return value === "true" ? "true" : "false";
  if (NUMERIC_TYPES.has(type)) return value || "0";
  if (type === "char") return `'${value.replace(/[\\']/g, (c) => `\\${c}`)}'`;
  if (type === "string") {
    return `"${value.replace(/[\\"]/g, (c) => `\\${c}`)}"`;
  }
  return value;
}

// A two-column grid that lines up the labels and inputs of the Fields inside.
const FIELD_GRID: CSSProperties = {
  display: "grid",
  gridTemplateColumns: "max-content 1fr",
  gap: 8,
  alignItems: "center",
  justifyItems: "start",
};

// Text inputs fill their grid cell and shrink with it on narrow layouts.
const INPUT_STYLE: CSSProperties = {
  width: "100%",
  minWidth: 0,
  boxSizing: "border-box",
};

// A labeled input for one function parameter or record field, as one row (two
// cells) of a FIELD_GRID container. Argument values are kept in a flat map
// keyed by dotted path (e.g. "token.amount").
function Field({
  wit,
  name,
  type,
  path,
  args,
  setArg,
}: {
  wit: WitResolve;
  name: string;
  type: WitType;
  path: string;
  args: Record<string, string>;
  setArg: (path: string, value: string) => void;
}) {
  return (
    <>
      <span>
        {name}: <code>{typeStr(wit, type)}</code>
      </span>
      <ParamInput
        wit={wit}
        type={type}
        path={path}
        args={args}
        setArg={setArg}
      />
    </>
  );
}

function ParamInput({
  wit,
  type,
  path,
  args,
  setArg,
}: {
  wit: WitResolve;
  type: WitType;
  path: string;
  args: Record<string, string>;
  setArg: (path: string, value: string) => void;
}) {
  // Numeric inputs default to 0, matching the WAVE encoding fallback.
  const value =
    args[path] ??
    (typeof type === "string" && NUMERIC_TYPES.has(type) ? "0" : "");
  if (typeof type === "number") {
    const kind = wit.types[type].kind;
    if (typeof kind !== "string") {
      if (kind.type !== undefined) {
        // Alias: render as the underlying type.
        return (
          <ParamInput
            wit={wit}
            type={kind.type}
            path={path}
            args={args}
            setArg={setArg}
          />
        );
      }
      if (kind.record) {
        return (
          <fieldset style={{ ...FIELD_GRID, margin: 0, minWidth: 0 }}>
            {kind.record.fields.map((field) => (
              <Field
                key={field.name}
                wit={wit}
                name={field.name}
                type={field.type}
                path={`${path}.${field.name}`}
                args={args}
                setArg={setArg}
              />
            ))}
          </fieldset>
        );
      }
      if (kind.enum) {
        return (
          <select value={value} onChange={(e) => setArg(path, e.target.value)}>
            {kind.enum.cases.map((c) => (
              <option key={c.name} value={c.name}>
                {c.name}
              </option>
            ))}
          </select>
        );
      }
      if (kind.variant) {
        const cases = kind.variant.cases;
        const selected = cases.find((c) => c.name === value) ?? cases[0];
        // A single grid cell even when the selected case has a payload input.
        return (
          <span
            style={{ display: "inline-flex", gap: 8, alignItems: "center" }}
          >
            <select
              value={selected?.name ?? ""}
              onChange={(e) => setArg(path, e.target.value)}
            >
              {cases.map((c) => (
                <option key={c.name} value={c.name}>
                  {c.name}
                </option>
              ))}
            </select>
            {selected?.type != null && (
              <ParamInput
                wit={wit}
                type={selected.type}
                path={`${path}.${selected.name}`}
                args={args}
                setArg={setArg}
              />
            )}
          </span>
        );
      }
    }
    // Other compound types fall back to a free-form text input.
    return (
      <input
        type="text"
        style={INPUT_STYLE}
        value={value}
        onChange={(e) => setArg(path, e.target.value)}
        onFocus={(e) => e.target.select()}
      />
    );
  }
  if (type === "bool") {
    return (
      <input
        type="checkbox"
        checked={value === "true"}
        onChange={(e) => setArg(path, String(e.target.checked))}
      />
    );
  }
  return (
    <input
      type={NUMERIC_TYPES.has(type) ? "number" : "text"}
      style={INPUT_STYLE}
      value={value}
      onChange={(e) => setArg(path, e.target.value)}
      onFocus={(e) => e.target.select()}
    />
  );
}

// The invocation form of one instance (UTXO): a function selector and one
// input per parameter, labeled with the deployment digest and instance number.
function InstanceCard({
  wit,
  digest,
  instance,
  onCall,
  log,
}: {
  wit: WitResolve;
  digest: string;
  instance: number;
  onCall: (name: string, args: string[]) => void;
  log: string[];
}) {
  const [selected, setSelected] = useState("");
  const [args, setArgs] = useState<Record<string, string>>({});

  const exports = useMemo(() => exportedFunctions(wit), [wit]);

  useEffect(() => {
    setSelected(exports[0]?.name ?? "");
    setArgs({});
  }, [exports]);

  const setArg = useCallback((path: string, value: string) => {
    setArgs((prev) => ({ ...prev, [path]: value }));
  }, []);

  const func = exports.find((e) => e.name === selected);

  return (
    <div
      className="card"
      style={{
        marginTop: 16,
        border: "1px solid var(--ifm-color-emphasis-300)",
      }}
    >
      <div className="card__header">
        <h4 style={{ margin: 0, overflowWrap: "anywhere" }}>
          Instance #{instance} of <code>{digest}</code>
        </h4>
      </div>
      {/* Form on the left, the call log on the right; the log wraps below
          the form when the card is too narrow for both. */}
      <div
        className="card__body"
        style={{
          display: "flex",
          gap: 16,
          alignItems: "flex-start",
          flexWrap: "wrap",
        }}
      >
        <div style={{ flex: "1 1 280px", minWidth: 0 }}>
          {exports.length === 0 ? (
            <p>The deployed component exports no functions.</p>
          ) : (
            <>
              <label>
                Function{" "}
                <select
                  value={selected}
                  onChange={(e) => {
                    setSelected(e.target.value);
                    setArgs({});
                  }}
                >
                  {exports.map((e) => (
                    <option key={e.name} value={e.name}>
                      {e.name}
                    </option>
                  ))}
                </select>
              </label>
              {func && (
                <form
                  onSubmit={(e) => {
                    e.preventDefault();
                    onCall(
                      func.name,
                      func.params.map((p) =>
                        waveArg(wit, p.type, p.name, args),
                      ),
                    );
                  }}
                  style={{ ...FIELD_GRID, marginTop: 8 }}
                >
                  {func.params.map((param) => (
                    <Field
                      key={param.name}
                      wit={wit}
                      name={param.name}
                      type={param.type}
                      path={param.name}
                      args={args}
                      setArg={setArg}
                    />
                  ))}
                  <div style={{ gridColumn: "1 / -1" }}>
                    <button type="submit" className="button button--primary">
                      Call
                    </button>
                  </div>
                </form>
              )}
            </>
          )}
        </div>
        <pre
          style={{
            flex: "1 1 240px",
            minWidth: 0,
            margin: 0,
            height: 240,
            overflow: "auto",
          }}
        >
          {log.length > 0 ? (
            log.join("\n")
          ) : (
            <span style={{ color: "var(--ifm-color-emphasis-500)" }}>
              Contract instantiated.
            </span>
          )}
        </pre>
      </div>
    </div>
  );
}

function RunPanel({
  canDeploy,
  deployHint,
  deployErrors,
  onDeploy,
  deployments,
  onCreateInstance,
  instances,
  onCall,
  instanceLogs,
  log,
}: {
  canDeploy: boolean;
  /** Why deploying is unavailable, shown next to the disabled button. */
  deployHint: ReactNode | undefined;
  /** Errors of the last failed deploy, shown as a warning under the button. */
  deployErrors: string[];
  onDeploy: () => void;
  deployments: RunDeployment[];
  onCreateInstance: (digest: string) => void;
  instances: RunInstance[];
  onCall: (
    digest: string,
    instance: number,
    name: string,
    args: string[],
  ) => void;
  /** Call log lines of each instance, keyed by instanceKey. */
  instanceLogs: Record<string, string[]>;
  /** Log lines not attributable to an instance (deploy/instantiate). */
  log: string[];
}) {
  const [selectedDigest, setSelectedDigest] = useState("");

  // Deployments are ordered most recent first; select the newest one
  // whenever a deployment happens.
  useEffect(() => {
    setSelectedDigest(deployments[0]?.digest ?? "");
  }, [deployments]);

  const witByDigest = useMemo(
    () => new Map(deployments.map((d) => [d.digest, d.wit])),
    [deployments],
  );

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
            gap: 8,
            alignItems: "center",
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
          <button
            type="button"
            className="button button--secondary"
            disabled={!selectedDigest}
            onClick={() => onCreateInstance(selectedDigest)}
          >
            Create instance (UTXO)
          </button>
        </div>
      )}
      {instances.map(({ digest, instance }) => {
        const wit = witByDigest.get(digest);
        return (
          wit && (
            <InstanceCard
              key={instanceKey(digest, instance)}
              wit={wit}
              digest={digest}
              instance={instance}
              onCall={(name, args) => onCall(digest, instance, name, args)}
              log={instanceLogs[instanceKey(digest, instance)] ?? []}
            />
          )
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
  const [instances, setInstances] = useState<RunInstance[]>([]);
  const [runLog, setRunLog] = useState<string[]>([]);
  // Call log lines of each instance, keyed by instanceKey.
  const [instanceLogs, setInstanceLogs] = useState<Record<string, string[]>>(
    {},
  );
  const appendInstanceLog = useCallback((key: string, entry: string) => {
    setInstanceLogs((prev) => ({
      ...prev,
      [key]: [...(prev[key] ?? []), entry],
    }));
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

  // The digest the Deploy button was last pressed for: deploying is disabled
  // until the digest of the Wasm changes.
  const [lastDeployedDigest, setLastDeployedDigest] = useState<string>();

  // Errors of the last failed deploy, shown as a warning next to the Deploy
  // button. Cleared on the next deploy and when the compiler output changes.
  const [deployErrors, setDeployErrors] = useState<string[]>([]);
  useEffect(() => {
    setDeployErrors([]);
  }, [componentWasm]);

  // The in-flight deploy, to attribute its error logs to the deploy warning.
  const pendingDeploy = useRef<string | undefined>(undefined);

  // The in-flight call, to attribute its logs and result to its instance.
  const pendingCall = useRef<{ key: string; label: string } | undefined>(
    undefined,
  );

  const run_request_id = useRef(0);
  const runWorker = useRunWorker((response) => {
    if (response.request_id !== run_request_id.current) {
      console.log("discarding response for old request", response);

      return;
    }

    if (response.type == "idle") {
      // Idle response received
    } else if (response.type == "log") {
      const level = ["", "Error", "Warn", "Info", "Debug", "Trace"][
        response.level
      ];
      console.log(level, `[${response.target}]`, response.body);
      // Surface errors and warnings in the Run panel, attributed to the
      // instance being called if there is one.
      if (response.level <= 2) {
        const entry = `${level}: ${response.body}`;
        const pending = pendingCall.current;
        if (pending) {
          appendInstanceLog(pending.key, entry);
        } else if (pendingDeploy.current !== undefined) {
          setDeployErrors((prev) => [...prev, entry]);
        } else {
          setRunLog((prev) => [...prev, entry]);
        }
      }
    } else if (response.type == "deployed") {
      pendingDeploy.current = undefined;
      const { digest, wit } = response;
      const deployment = { digest, wit, deployedAt: new Date() };
      // Most recent deployment first; a re-deploy moves its digest up front.
      setDeployments((prev) => [
        deployment,
        ...prev.filter((d) => d.digest !== digest),
      ]);
    } else if (response.type == "deploy_failed") {
      pendingDeploy.current = undefined;
      // The cause arrived as "log" responses; make sure the warning shows
      // even if none did.
      setDeployErrors((prev) => (prev.length > 0 ? prev : ["Unknown error."]));
      // Re-enable the Deploy button that onDeploy optimistically disabled.
      setLastDeployedDigest((prev) =>
        prev === response.digest ? undefined : prev,
      );
    } else if (response.type == "instantiated") {
      // Most recent instance first, same as deployments.
      setInstances((prev) => [
        { digest: response.digest, instance: response.instance },
        ...prev,
      ]);
    } else if (response.type == "called") {
      const pending = pendingCall.current;
      if (pending) {
        appendInstanceLog(
          pending.key,
          `${pending.label} returned${
            response.result !== undefined ? ` ${response.result}` : ""
          }`,
        );
      }
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

  const onCreateInstance = useCallback((digest: string) => {
    pendingCall.current = undefined;
    pendingDeploy.current = undefined;
    setRunLog([]);
    runWorker.request({
      request_id: ++run_request_id.current,
      type: "instantiate",
      digest,
    });
  }, []);

  const onCall = useCallback(
    (digest: string, instance: number, name: string, args: string[]) => {
      const key = instanceKey(digest, instance);
      const label = `${name}(${args.join(", ")})`;
      pendingCall.current = { key, label };
      pendingDeploy.current = undefined;
      appendInstanceLog(key, `→ ${label}`);
      runWorker.request({
        request_id: ++run_request_id.current,
        type: "call",
        digest,
        instance,
        name,
        args,
      });
    },
    [appendInstanceLog],
  );

  // Whether the current compiler output is deployed (confirmed by the
  // worker), and whether a deploy of it is still in flight.
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
                      Run: Deploy the compiled component, create instances
                      (UTXOs) of it, and call them.
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
                  onCreateInstance={onCreateInstance}
                  instances={instances}
                  onCall={onCall}
                  instanceLogs={instanceLogs}
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
