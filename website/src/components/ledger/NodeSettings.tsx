import { useEffect, useState, type ReactNode } from "react";
import { DEFAULT_CONFIG, type NodeConfig } from "../../ledger-config";
import type { Func, NodeInfo } from "../../ledger.worker";
import type { Call } from "./Details";
import styles from "./styles.module.css";
import {
  initialDrafts,
  ParamFields,
  signature,
  toArgs,
  toWave,
  type Draft,
} from "./ValueInput";

function Copyable({ label, value }: { label: string; value?: string }) {
  return (
    <div className={styles.value}>
      <span>{label}</span>
      <code>{value ?? "-"}</code>
      <button
        className="button button--sm button--secondary"
        disabled={!value}
        onClick={() => void navigator.clipboard.writeText(value ?? "")}
      >
        Copy
      </button>
    </div>
  );
}

function Field({ label, children }: { label: string; children: ReactNode }) {
  return (
    <label className={styles.field}>
      <span>{label}</span>
      {children}
    </label>
  );
}

function TextField({
  label,
  value,
  onChange,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
}) {
  return (
    <Field label={label}>
      <input value={value} onChange={(e) => onChange(e.target.value)} />
    </Field>
  );
}

/** Coordination scripts of the component at a path, as loaded. */
type Scripts = { path: string } & (
  { funcs: Func[]; error?: undefined } | { funcs?: undefined; error: string }
);

/** Status and configuration of the ledger the page is connected to. */
export function NodeSettings({
  info,
  preset,
  files,
  call,
  onStart,
  onStop,
  onConnect,
  onDownload,
}: {
  info: NodeInfo | undefined;
  /** Changes to the configuration to suggest. */
  preset: Partial<NodeConfig> | undefined;
  files: string[];
  call: Call;
  onStart: (config: NodeConfig) => void;
  onStop: () => void;
  onConnect: (url: string) => void;
  onDownload: (path: string) => void;
}) {
  const [remote, setRemote] = useState(info?.remote ?? false);
  const [url, setUrl] = useState(info?.remote ? info.url : "");
  const [config, setConfig] = useState<NodeConfig>(
    info?.config ?? DEFAULT_CONFIG,
  );
  const [scripts, setScripts] = useState<Scripts>();
  const [form, setForm] = useState<{ func: Func; drafts: Draft[] }>();
  const [invalid, setInvalid] = useState<string>();
  useEffect(() => {
    if (info?.running && !info.error) {
      setRemote(info.remote);
      if (info.remote) {
        setUrl(info.url);
      } else {
        setConfig(info.config);
      }
    }
  }, [info]);
  useEffect(() => {
    if (preset) {
      setRemote(false);
      setConfig((config) => ({ ...config, ...preset }));
    }
  }, [preset]);
  const contract = config.genesisContract;
  useEffect(() => {
    if (!contract) {
      setScripts(undefined);
      return;
    }
    let current = true;
    call({ op: "fileScripts", path: contract }).then(
      (funcs) =>
        current && setScripts({ path: contract, funcs: funcs as Func[] }),
      (err: unknown) =>
        current &&
        setScripts({
          path: contract,
          error: err instanceof Error ? err.message : String(err),
        }),
    );
    return () => {
      current = false;
    };
  }, [call, contract]);
  const func =
    scripts?.path === contract
      ? scripts.funcs?.find(({ name }) => name === config.genesisScript)
      : undefined;
  const drafts = func
    ? form?.func === func
      ? form.drafts
      : initialDrafts(func)
    : [];
  const set =
    (key: keyof Omit<NodeConfig, "imports" | "genesisArgs">) =>
    (value: string) =>
      setConfig((config) => ({ ...config, [key]: value }));
  const running = info?.running ?? false;
  const wasm = files.filter((path) => path.endsWith(".wasm"));

  const start = () => {
    setInvalid(undefined);
    let genesisArgs: string[] = [];
    if (contract) {
      if (!func) {
        setInvalid("choose the genesis script to run");
        return;
      }
      try {
        const args = toArgs(func, drafts);
        genesisArgs = func.params.map(({ ty }, i) => toWave(ty, args[i]));
      } catch (err) {
        setInvalid((err as Error).message);
        return;
      }
    }
    onStart({ ...config, genesisArgs });
  };

  return (
    <div className={styles.node}>
      <div className={styles.segmented} role="tablist">
        {(
          [
            [false, "In-browser node"],
            [true, "Remote ledger"],
          ] as const
        ).map(([value, label]) => (
          <button
            key={label}
            role="tab"
            aria-selected={remote === value}
            className={remote === value ? styles.segmentActive : styles.segment}
            onClick={() => setRemote(value)}
          >
            {label}
          </button>
        ))}
      </div>

      {info?.error ? (
        <div className="alert alert--danger margin-vert--sm">{info.error}</div>
      ) : null}

      {remote ? (
        <form
          className={styles.section}
          onSubmit={(e) => {
            e.preventDefault();
            onConnect(url);
          }}
        >
          <Field label="Ledger URL">
            <input
              value={url}
              placeholder="http://localhost:3000/"
              onChange={(e) => setUrl(e.target.value)}
            />
          </Field>
          <div className={styles.actions}>
            <button
              className="button button--sm button--primary"
              disabled={!info || !url.trim()}
            >
              {running && info?.remote ? "Reconnect" : "Connect"}
            </button>
          </div>
          <p className={styles.hint}>
            The page sends its requests to the ledger from this browser, so the
            ledger must allow cross-origin requests, like{" "}
            <code>starstream node</code> does. Scripts run as the ledger admin
            sign with <code>admin.key</code>, which must then be the admin key
            of that ledger.
          </p>
        </form>
      ) : (
        <>
          <div className={styles.section}>
            <div className={styles.actions}>
              <button
                className="button button--sm button--primary"
                disabled={!info}
                onClick={start}
              >
                {running && !info?.remote ? "Restart" : "Start"}
              </button>
              <button
                className="button button--sm button--secondary"
                disabled={!running || info?.remote}
                onClick={onStop}
              >
                Stop
              </button>
              <button
                className="button button--sm button--secondary"
                disabled={!files.includes("admin.key")}
                onClick={() => onDownload("admin.key")}
              >
                Download admin key
              </button>
            </div>
            {invalid ? (
              <div className="alert alert--danger margin-top--sm">
                {invalid}
              </div>
            ) : null}
            <p className={styles.hint}>
              Starting the node starts a new, empty ledger. Pages of this site
              in this browser reach it at its URL while this page is open.
            </p>
          </div>

          <h4 className={styles.sectionTitle}>Genesis</h4>
          <div className={styles.section}>
            <Field label="Contract">
              <select
                value={contract}
                onChange={(e) =>
                  setConfig((config) => ({
                    ...config,
                    genesisContract: e.target.value,
                    genesisScript: "",
                  }))
                }
              >
                <option value="">none, start without UTXOs</option>
                {wasm.map((path) => (
                  <option key={path}>{path}</option>
                ))}
              </select>
            </Field>
            {contract ? (
              scripts?.path !== contract ? (
                <p className={styles.hint}>loading…</p>
              ) : scripts.error !== undefined ? (
                <p className={styles.error}>{scripts.error}</p>
              ) : (
                <>
                  <Field label="Coordination script">
                    <select
                      value={config.genesisScript}
                      onChange={(e) => set("genesisScript")(e.target.value)}
                    >
                      <option value="" disabled>
                        choose a script
                      </option>
                      {scripts.funcs.map((func) => (
                        <option key={func.name} value={func.name}>
                          {signature(func)}
                        </option>
                      ))}
                    </select>
                  </Field>
                  {func ? (
                    <div className={styles.invoke}>
                      <ParamFields
                        func={func}
                        utxos={[]}
                        drafts={drafts}
                        onChange={(drafts) => setForm({ func, drafts })}
                      />
                    </div>
                  ) : null}
                </>
              )
            ) : null}
            {wasm.length > (contract ? 1 : 0) ? (
              <fieldset className={styles.field}>
                <span>Other contracts to import</span>
                {wasm
                  .filter((path) => path !== contract)
                  .map((path) => (
                    <label key={path} className={styles.check}>
                      <input
                        type="checkbox"
                        checked={config.imports.includes(path)}
                        onChange={(e) =>
                          setConfig((config) => ({
                            ...config,
                            imports: e.target.checked
                              ? [...config.imports, path]
                              : config.imports.filter((p) => p !== path),
                          }))
                        }
                      />
                      {path}
                    </label>
                  ))}
              </fieldset>
            ) : null}
          </div>

          <details className={styles.advanced}>
            <summary>Advanced</summary>
            <div className={styles.grid}>
              <TextField
                label="Network"
                value={config.network}
                onChange={set("network")}
              />
              <TextField
                label="Fuel"
                value={config.fuel}
                onChange={set("fuel")}
              />
              <TextField
                label="Maximum requests"
                value={config.maxRequests}
                onChange={set("maxRequests")}
              />
              <TextField
                label="Cardano block height"
                value={config.cardanoBlockHeight}
                onChange={set("cardanoBlockHeight")}
              />
              <TextField
                label="Cardano current slot"
                value={config.cardanoCurrentSlot}
                onChange={set("cardanoCurrentSlot")}
              />
            </div>
          </details>
        </>
      )}

      <h4 className={styles.sectionTitle}>Connection</h4>
      <div className={styles.section}>
        <Copyable label="URL" value={running ? info?.url : undefined} />
        <Copyable label="Network" value={running ? info?.network : undefined} />
        {info?.adminPublicKey ? (
          <Copyable label="Admin key" value={info.adminPublicKey} />
        ) : null}
      </div>
    </div>
  );
}
