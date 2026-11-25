import { useEffect, useState } from "react";
import { DEFAULT_CONFIG, type NodeConfig } from "../../ledger-config";
import type { NodeInfo } from "../../ledger.worker";
import styles from "./styles.module.css";

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

function Field({
  label,
  value,
  placeholder,
  onChange,
}: {
  label: string;
  value: string;
  placeholder?: string;
  onChange: (value: string) => void;
}) {
  return (
    <label className={styles.field}>
      <span>{label}</span>
      <input
        value={value}
        placeholder={placeholder}
        onChange={(e) => onChange(e.target.value)}
      />
    </label>
  );
}

/** Status and configuration of the in-browser ledger node. */
export function NodeSettings({
  info,
  preset,
  files,
  onStart,
  onStop,
  onDownload,
}: {
  info: NodeInfo | undefined;
  /** Changes to the configuration to suggest. */
  preset: Partial<NodeConfig> | undefined;
  files: string[];
  onStart: (config: NodeConfig) => void;
  onStop: () => void;
  onDownload: (path: string) => void;
}) {
  const [config, setConfig] = useState<NodeConfig>(
    info?.config ?? DEFAULT_CONFIG,
  );
  useEffect(() => {
    if (info?.running && !info.error) {
      setConfig(info.config);
    }
  }, [info]);
  useEffect(() => {
    if (preset) {
      setConfig((config) => ({ ...config, ...preset }));
    }
  }, [preset]);
  const set = (key: keyof Omit<NodeConfig, "imports">) => (value: string) =>
    setConfig((config) => ({ ...config, [key]: value }));
  const running = info?.running ?? false;
  return (
    <div className={styles.node}>
      <label className={styles.toggle}>
        <input
          type="checkbox"
          checked={running}
          disabled={!info}
          onChange={(e) => (e.target.checked ? onStart(config) : onStop())}
        />{" "}
        {running ? "Running" : "Stopped"}
      </label>
      {info?.error ? (
        <div className="alert alert--danger margin-vert--sm">{info.error}</div>
      ) : null}
      <Copyable label="URL" value={running ? info?.url : undefined} />
      <Copyable label="Network" value={info?.network} />
      <Copyable label="Admin public key" value={info?.adminPublicKey} />
      <button
        className="button button--sm button--secondary margin-top--sm"
        disabled={!files.includes("admin.key")}
        onClick={() => onDownload("admin.key")}
      >
        Download admin.key
      </button>
      <p className={styles.hint}>
        Restarting the node starts a new, empty ledger. Pages of this site in
        this browser reach it at the URL above while this page is open.
      </p>

      <h4>Configuration</h4>
      <Field label="Network" value={config.network} onChange={set("network")} />
      <Field
        label="Admin public key"
        value={config.admin}
        placeholder="the one of admin.key"
        onChange={set("admin")}
      />
      <label className={styles.field}>
        <span>Genesis</span>
        <select
          value={config.genesis}
          onChange={(e) => set("genesis")(e.target.value)}
        >
          <option value="">none</option>
          {files
            .filter((path) => path.endsWith(".toml"))
            .map((path) => (
              <option key={path}>{path}</option>
            ))}
        </select>
      </label>
      <label className={styles.field}>
        <span>Genesis script</span>
        <select
          value={config.genesisContract}
          onChange={(e) => set("genesisContract")(e.target.value)}
        >
          <option value="">none</option>
          {files
            .filter((path) => path.endsWith(".wasm"))
            .map((path) => (
              <option key={path}>{path}</option>
            ))}
        </select>
      </label>
      {config.genesisContract ? (
        <>
          <Field
            label="Script"
            value={config.genesisScript}
            placeholder="name"
            onChange={set("genesisScript")}
          />
          <Field
            label="Arguments"
            value={config.genesisArgs}
            placeholder="WAVE arguments"
            onChange={set("genesisArgs")}
          />
        </>
      ) : null}
      <fieldset className={styles.field}>
        <span>Imports</span>
        {files
          .filter((path) => path.endsWith(".wasm"))
          .map((path) => (
            <label key={path}>
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
              />{" "}
              {path}
            </label>
          ))}
      </fieldset>
      <Field
        label="Fuel"
        value={config.fuel}
        placeholder="default"
        onChange={set("fuel")}
      />
      <Field
        label="Maximum requests"
        value={config.maxRequests}
        placeholder="default"
        onChange={set("maxRequests")}
      />
      <Field
        label="Cardano block height"
        value={config.cardanoBlockHeight}
        placeholder="0"
        onChange={set("cardanoBlockHeight")}
      />
      <Field
        label="Cardano current slot"
        value={config.cardanoCurrentSlot}
        placeholder="0"
        onChange={set("cardanoCurrentSlot")}
      />
      <button
        className="button button--sm button--primary margin-top--sm"
        disabled={!info}
        onClick={() => onStart(config)}
      >
        {running ? "Restart" : "Start"}
      </button>
    </div>
  );
}
