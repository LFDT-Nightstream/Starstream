import Layout from "@theme/Layout";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { useEffect, useRef, useState } from "react";
import { DEFAULT_CONFIG, type NodeConfig } from "../ledger-config";
import type {
  HttpResponse,
  LedgerState,
  LedgerWorkerRequest,
  LedgerWorkerResponse,
  NodeInfo,
} from "../ledger.worker";

const PROMPT = "\x1b[1;32mledger\x1b[0m$ ";
const STATE_INTERVAL = 2000;

function crlf(text: string): string {
  return text.replace(/\r?\n/g, "\r\n");
}

function unavailable(text: string): HttpResponse {
  const body = new TextEncoder().encode(text).buffer as ArrayBuffer;
  return { status: 503, headers: [], body };
}

function post(
  worker: Worker | undefined,
  request: LedgerWorkerRequest,
  transfer: Transferable[] = [],
): void {
  worker?.postMessage(request, transfer);
}

function upload(worker: Worker, done: (err?: unknown) => void): () => boolean {
  const input = document.createElement("input");
  input.type = "file";
  input.multiple = true;
  let active = true;
  const finish = (err?: unknown) => {
    if (active) {
      active = false;
      done(err);
    }
  };
  input.addEventListener("cancel", () => finish());
  input.addEventListener("change", async () => {
    try {
      const files = await Promise.all(
        Array.from(input.files ?? [], async (file) => ({
          path: file.name,
          data: new Uint8Array(await file.arrayBuffer()),
        })),
      );
      if (!active) {
        return;
      }
      active = false;
      post(
        worker,
        { type: "upload", files },
        files.map(({ data }) => data.buffer),
      );
    } catch (err) {
      finish(err);
    }
  });
  input.click();
  return () => {
    const cancelled = active;
    active = false;
    return cancelled;
  };
}

function download(path: string, data: Uint8Array<ArrayBuffer>): void {
  const url = URL.createObjectURL(new Blob([data]));
  const a = document.createElement("a");
  a.href = url;
  a.download = path.split("/").pop() ?? path;
  a.click();
  setTimeout(() => URL.revokeObjectURL(url), 40_000);
}

function Copyable({ label, value }: { label: string; value?: string }) {
  return (
    <div className="ledger-node__value">
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
    <label className="ledger-node__field">
      <span>{label}</span>
      <input
        value={value}
        placeholder={placeholder}
        onChange={(e) => onChange(e.target.value)}
      />
    </label>
  );
}

function NodePanel({
  info,
  state,
  files,
  onStart,
  onStop,
  onDownload,
}: {
  info: NodeInfo | undefined;
  state: LedgerState | undefined;
  files: string[];
  onStart: (config: NodeConfig) => void;
  onStop: () => void;
  onDownload: (path: string) => void;
}) {
  const [config, setConfig] = useState<NodeConfig>(DEFAULT_CONFIG);
  useEffect(() => {
    if (info?.running) {
      setConfig(info.config);
    }
  }, [info]);
  const set = (key: keyof Omit<NodeConfig, "imports">) => (value: string) =>
    setConfig((config) => ({ ...config, [key]: value }));
  const running = info?.running ?? false;
  return (
    <aside className="ledger-node">
      <div className="ledger-node__header">
        <h2>Ledger node</h2>
        <label>
          <input
            type="checkbox"
            checked={running}
            disabled={!info}
            onChange={(e) => (e.target.checked ? onStart(config) : onStop())}
          />{" "}
          {running ? "Running" : "Stopped"}
        </label>
      </div>
      {info?.error ? (
        <div className="alert alert--danger margin-bottom--sm">
          {info.error}
        </div>
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

      <details className="margin-top--md">
        <summary>Configuration</summary>
        <Field
          label="Network"
          value={config.network}
          onChange={set("network")}
        />
        <Field
          label="Admin public key"
          value={config.admin}
          placeholder="the one of admin.key"
          onChange={set("admin")}
        />
        <label className="ledger-node__field">
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
        <fieldset className="ledger-node__field">
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
      </details>

      <h3 className="margin-top--md">State</h3>
      {state ? (
        <>
          <p>
            Height {String(state.height)}, admin nonce{" "}
            {String(state.adminNonce)}
          </p>
          <h4>Genesis outputs</h4>
          {state.genesis.length ? (
            <ol start={0}>
              {state.genesis.map(({ contract, instance }, i) => (
                <li key={i}>
                  <code>{instance}</code> of <code>{contract}</code>
                </li>
              ))}
            </ol>
          ) : (
            <p>none</p>
          )}
          <h4>Blocks</h4>
          {state.blocks.length ? (
            <ul>
              {state.blocks.map(({ height, envelopes }) => (
                <li key={String(height)}>
                  #{String(height)}: {envelopes.length} envelope
                  {envelopes.length === 1 ? "" : "s"} (
                  {envelopes.reduce((a, b) => a + b, 0)} bytes)
                </li>
              ))}
            </ul>
          ) : (
            <p>none</p>
          )}
        </>
      ) : (
        <p>{running ? "loading" : "not running"}</p>
      )}
    </aside>
  );
}

export default function LedgerPage() {
  const container = useRef<HTMLDivElement>(null);
  const worker = useRef<Worker>(undefined);
  const serviceWorker = useBaseUrl("/ledger-sw.js");
  const [jspi, setJspi] = useState(true);
  const [info, setInfo] = useState<NodeInfo>();
  const [state, setState] = useState<LedgerState>();
  const [files, setFiles] = useState<string[]>([]);

  useEffect(() => setJspi("Suspending" in WebAssembly), []);

  useEffect(() => {
    if (!jspi) {
      return;
    }
    let disposed = false;
    let cleanup = () => {};

    void (async () => {
      const { init, FitAddon, Terminal } = await import("ghostty-web");
      await init();
      if (disposed || !container.current) {
        return;
      }
      const w = new Worker(new URL("../ledger.worker", import.meta.url));
      worker.current = w;
      const term = new Terminal({
        fontFamily: getComputedStyle(document.documentElement)
          .getPropertyValue("--ifm-font-family-monospace")
          .trim(),
        fontSize: 14,
        cursorBlink: true,
        theme: { background: "#1a1b26", foreground: "#a9b1d6" },
      });
      const fit = new FitAddon();
      term.loadAddon(fit);
      term.open(container.current);
      fit.fit();
      const onResize = () => fit.fit();
      window.addEventListener("resize", onResize);
      cleanup = () => {
        window.removeEventListener("resize", onResize);
        term.dispose();
        w.terminate();
        worker.current = undefined;
      };
      term.write(
        crlf(
          "Starstream ledger terminal. Type `help` for the available commands.\n",
        ),
      );
      term.focus();

      const history: string[] = [];
      let entry = 0;
      let line = "";
      let busy = false;
      let cancelling = false;
      let cancelUpload: (() => boolean) | undefined;

      const prompt = () => {
        busy = false;
        cancelling = false;
        cancelUpload = undefined;
        line = "";
        entry = history.length;
        term.write(PROMPT);
      };
      const replace = (next: string) => {
        term.write("\b \b".repeat(line.length) + next);
        line = next;
      };

      w.addEventListener(
        "message",
        ({ data }: { data: LedgerWorkerResponse }) => {
          switch (data.type) {
            case "stdout":
              term.write(crlf(data.text));
              return;
            case "stderr":
              term.write(`\x1b[2m${crlf(data.text)}\x1b[0m`);
              return;
            case "download":
              download(data.path, data.data as Uint8Array<ArrayBuffer>);
              return;
            case "node":
              setInfo(data.info);
              return;
            case "state":
              setState(data.state);
              return;
            case "files":
              setFiles(data.paths);
              return;
            case "done":
              prompt();
          }
        },
      );
      prompt();

      const submit = () => {
        term.write("\r\n");
        busy = true;
        const trimmed = line.trim();
        if (trimmed) {
          history.push(line);
        }
        if (trimmed === "clear") {
          term.clear();
          prompt();
        } else if (trimmed === "upload") {
          cancelUpload = upload(w, (err) => {
            if (err !== undefined) {
              term.write(
                `\x1b[2merror: ${err instanceof Error ? err.message : String(err)}\x1b[0m\r\n`,
              );
            }
            prompt();
          });
        } else {
          post(w, { type: "exec", line });
        }
      };

      const cancel = () => {
        if (cancelling) {
          return;
        }
        cancelling = true;
        term.write("^C\r\n");
        if (cancelUpload?.()) {
          prompt();
        } else {
          post(w, { type: "cancel" });
        }
      };

      term.onData((data) => {
        if (busy) {
          if (data.includes("\x03")) {
            cancel();
          }
          return;
        }
        if (data === "\x1b[A" || data === "\x1b[B") {
          entry = Math.max(
            0,
            Math.min(history.length, entry + (data === "\x1b[A" ? -1 : 1)),
          );
          replace(history[entry] ?? "");
          return;
        }
        if (data.startsWith("\x1b")) {
          return;
        }
        for (const c of data) {
          if (busy) {
            return;
          }
          if (c === "\r" || c === "\n") {
            submit();
          } else if (c === "\x7f" || c === "\b") {
            if (line) {
              line = line.slice(0, -1);
              term.write("\b \b");
            }
          } else if (c === "\x03") {
            term.write("^C\r\n");
            prompt();
          } else if (c === "\x0c") {
            term.clear();
            term.write(PROMPT + line);
          } else if (c >= " ") {
            line += c;
            term.write(c);
          }
        }
      });
    })();

    return () => {
      disposed = true;
      cleanup();
    };
  }, [jspi]);

  useEffect(() => {
    if (!jspi || !("serviceWorker" in navigator)) {
      return;
    }
    void navigator.serviceWorker.register(serviceWorker, { scope: "/" });
    const pending = new Set<MessagePort>();
    const respond = (port: MessagePort, response: HttpResponse) => {
      pending.delete(port);
      port.postMessage(response, [response.body]);
    };
    const onMessage = ({ data, ports: [port] }: MessageEvent) => {
      if (data?.type !== "ledger-request" || !port) {
        return;
      }
      if (!worker.current) {
        respond(port, unavailable("the ledger terminal is not ready"));
        return;
      }
      pending.add(port);
      const { port1, port2 } = new MessageChannel();
      port1.onmessage = ({ data }) => respond(port, data);
      post(
        worker.current,
        { type: "http", request: data.request, port: port2 },
        [port2],
      );
    };
    navigator.serviceWorker.addEventListener("message", onMessage);
    navigator.serviceWorker.startMessages();
    return () => {
      navigator.serviceWorker.removeEventListener("message", onMessage);
      for (const port of pending) {
        respond(port, unavailable("the ledger terminal was closed"));
      }
    };
  }, [jspi, serviceWorker]);

  useEffect(() => {
    if (!info?.running) {
      setState(undefined);
      return;
    }
    const refresh = () => post(worker.current, { type: "state" });
    refresh();
    const timer = setInterval(refresh, STATE_INTERVAL);
    return () => clearInterval(timer);
  }, [info]);

  return (
    <Layout
      title="Ledger terminal"
      description="Run a Starstream ledger in the browser"
      noFooter
    >
      {jspi ? (
        <div className="ledger">
          <div className="ledger-terminal" ref={container} />
          <NodePanel
            info={info}
            state={state}
            files={files}
            onStart={(config) =>
              post(worker.current, { type: "start", config })
            }
            onStop={() => post(worker.current, { type: "stop" })}
            onDownload={(path) =>
              post(worker.current, { type: "download", path })
            }
          />
        </div>
      ) : (
        <main className="container margin-vert--xl">
          <h1>Ledger terminal</h1>
          <p>
            This browser does not support JSPI (
            <code>WebAssembly.Suspending</code>), which the ledger terminal
            requires.
          </p>
        </main>
      )}
    </Layout>
  );
}
