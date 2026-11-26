import Layout from "@theme/Layout";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { useCallback, useEffect, useRef, useState } from "react";
import { Details, type Call } from "../components/ledger/Details";
import { NodeSettings } from "../components/ledger/NodeSettings";
import { Tables } from "../components/ledger/Tables";
import { Terminal } from "../components/ledger/Terminal";
import { amount, key, type Selection } from "../components/ledger/format";
import styles from "../components/ledger/styles.module.css";
import type { NodeConfig } from "../ledger-config";
import payToPublicKey from "file-loader!../../../examples/pay_to_public_key.star";
import score from "file-loader!../../../examples/score.star";
import type {
  HttpResponse,
  LedgerCall,
  LedgerWorkerRequest,
  LedgerWorkerResponse,
  NodeInfo,
} from "../ledger.worker";

const REFRESH_INTERVAL = 2000;
const WALLET_TOP_UP = 1_000_000n;
/** Examples of the repository that compile on their own, by file name. */
const EXAMPLES: Record<string, string> = {
  "score.star": score,
  "pay_to_public_key.star": payToPublicKey,
};

type ExplorerMessage = Extract<LedgerWorkerResponse, { type: "explorer" }>;
type Panel = "details" | "node";

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

function download(path: string, data: Uint8Array<ArrayBuffer>): void {
  const url = URL.createObjectURL(new Blob([data]));
  const a = document.createElement("a");
  a.href = url;
  a.download = path.split("/").pop() ?? path;
  a.click();
  setTimeout(() => URL.revokeObjectURL(url), 40_000);
}

function Explorer() {
  const serviceWorker = useBaseUrl("/ledger-sw.js");
  const [worker, setWorker] = useState<Worker>();
  const [info, setInfo] = useState<NodeInfo>();
  const [explorer, setExplorer] = useState<ExplorerMessage>();
  const [files, setFiles] = useState<string[]>([]);
  const [selection, setSelection] = useState<Selection>();
  const [panel, setPanel] = useState<Panel>("details");
  const [terminal, setTerminal] = useState(false);
  const [terminalUsed, setTerminalUsed] = useState(false);
  const [dragging, setDragging] = useState(0);
  const [publishing, setPublishing] = useState(false);
  const [error, setError] = useState<string>();
  const [preset, setPreset] = useState<Partial<NodeConfig>>();
  const replies = useRef(
    new Map<number, { resolve(v: unknown): void; reject(e: Error): void }>(),
  );
  const nextId = useRef(0);
  const picker = useRef<HTMLInputElement>(null);
  const keyPicker = useRef<HTMLInputElement>(null);

  useEffect(() => {
    const w = new Worker(new URL("../ledger.worker", import.meta.url));
    const onMessage = ({ data }: { data: LedgerWorkerResponse }) => {
      switch (data.type) {
        case "download":
          download(data.path, data.data as Uint8Array<ArrayBuffer>);
          return;
        case "node":
          setInfo(data.info);
          return;
        case "explorer":
          setExplorer(data);
          return;
        case "files":
          setFiles(data.paths);
          return;
        case "reply": {
          const reply = replies.current.get(data.id);
          replies.current.delete(data.id);
          if (data.error !== undefined) {
            reply?.reject(new Error(data.error));
          } else {
            reply?.resolve(data.result);
          }
          post(w, { type: "explore" });
        }
      }
    };
    w.addEventListener("message", onMessage);
    setWorker(w);
    return () => {
      w.terminate();
      for (const { reject } of replies.current.values()) {
        reject(new Error("the ledger page was closed"));
      }
      replies.current.clear();
    };
  }, []);

  const call: Call = useCallback(
    (request) =>
      new Promise((resolve, reject) => {
        if (!worker) {
          reject(new Error("the ledger is not ready"));
          return;
        }
        const id = nextId.current++;
        replies.current.set(id, { resolve, reject });
        post(worker, { type: "call", id, call: request });
      }),
    [worker],
  );

  const perform = (request: LedgerCall) => {
    call(request).catch((err: unknown) =>
      setError(err instanceof Error ? err.message : String(err)),
    );
  };

  useEffect(() => {
    if (!worker || !("serviceWorker" in navigator)) {
      return;
    }
    navigator.serviceWorker
      .register(serviceWorker, { scope: "/" })
      .catch((err: unknown) =>
        setError(`failed to register the service worker: ${String(err)}`),
      );
    const pending = new Set<MessagePort>();
    const respond = (port: MessagePort, response: HttpResponse) => {
      pending.delete(port);
      port.postMessage(response, [response.body]);
    };
    const onMessage = ({ data, ports: [port] }: MessageEvent) => {
      if (data?.type !== "ledger-request" || !port) {
        return;
      }
      port.postMessage({ ack: true });
      pending.add(port);
      const { port1, port2 } = new MessageChannel();
      port1.onmessage = ({ data }) => respond(port, data);
      post(worker, { type: "http", request: data.request, port: port2 }, [
        port2,
      ]);
    };
    navigator.serviceWorker.addEventListener("message", onMessage);
    navigator.serviceWorker.startMessages();
    return () => {
      navigator.serviceWorker.removeEventListener("message", onMessage);
      for (const port of pending) {
        respond(port, unavailable("the ledger page was closed"));
      }
    };
  }, [worker, serviceWorker]);

  useEffect(() => {
    if (!info?.running) {
      setExplorer(undefined);
      setSelection(undefined);
      return;
    }
    const refresh = () => post(worker, { type: "explore" });
    refresh();
    const timer = setInterval(refresh, REFRESH_INTERVAL);
    return () => clearInterval(timer);
  }, [info, worker]);

  const publish = async (list: FileList | File[]) => {
    const files = await Promise.all(
      Array.from(list, async (file) => ({
        name: file.name,
        data: new Uint8Array(await file.arrayBuffer()),
      })),
    );
    if (!files.length) {
      return;
    }
    setPublishing(true);
    setError(undefined);
    try {
      const digests = (await call({ op: "publish", files })) as string[];
      const last = digests.at(-1);
      if (last) {
        setSelection({ kind: "contract", digest: last });
        setPanel("details");
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setPublishing(false);
    }
  };

  const publishExample = async (name: string) => {
    try {
      const res = await fetch(EXAMPLES[name]);
      if (!res.ok) {
        throw new Error(`failed to fetch \`${name}\`: ${res.status}`);
      }
      const data = new Uint8Array(await res.arrayBuffer());
      await publish([new File([data], name)]);
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    }
  };

  const topUp = async () => {
    setError(undefined);
    try {
      await call({ op: "fund", amount: WALLET_TOP_UP });
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    }
  };

  const uploadKey = async (file: File) => {
    setError(undefined);
    try {
      await call({
        op: "wallet",
        key: new Uint8Array(await file.arrayBuffer()),
      });
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    }
  };

  const state = explorer?.state;
  const wallet = explorer?.wallet;
  const walletAccount = state?.accounts.find(({ key }) => key === wallet);
  const select = (s: Selection) => {
    setSelection(s);
    setPanel("details");
  };

  return (
    <div
      className={styles.page}
      onDragEnter={(e) => {
        if (e.dataTransfer.types.includes("Files")) {
          e.preventDefault();
          setDragging((n) => n + 1);
        }
      }}
      onDragOver={(e) => {
        if (e.dataTransfer.types.includes("Files")) {
          e.preventDefault();
          e.dataTransfer.dropEffect = "copy";
        }
      }}
      onDragLeave={() => setDragging((n) => Math.max(0, n - 1))}
      onDrop={(e) => {
        e.preventDefault();
        setDragging(0);
        void publish(e.dataTransfer.files);
      }}
    >
      <header className={styles.toolbar}>
        <h1>Ledger</h1>
        <button
          className={
            info?.running
              ? styles.statusRunning
              : info?.error
                ? styles.statusError
                : styles.status
          }
          title={info?.error ?? info?.url}
          onClick={() => setPanel("node")}
        >
          {info?.running
            ? `${info.remote ? new URL(info.url).host : "in-browser"} · ${info.network} · block ${state ? String(state.height) : "…"}`
            : info?.error
              ? "failed"
              : info
                ? "stopped"
                : "starting…"}
        </button>
        <span className={styles.spacer} />
        <span className={styles.wallet}>
          {wallet ? (
            <button
              className={styles.link}
              title={`Account ${wallet}, which signs what this page publishes and runs`}
              onClick={() => select({ kind: "account", key: wallet })}
            >
              Wallet <code>{key(wallet)}</code>
              <strong>{amount(walletAccount?.balance)}</strong>
            </button>
          ) : (
            <span className={styles.hint}>No wallet yet</span>
          )}
          {info?.remote ? (
            <>
              <button
                className="button button--sm button--secondary"
                disabled={!info.running}
                onClick={() => keyPicker.current?.click()}
                title="Use the secret key of a funded account of the remote ledger as your wallet"
              >
                Upload key
              </button>
              <input
                ref={keyPicker}
                type="file"
                hidden
                onChange={(e) => {
                  const file = e.target.files?.[0];
                  e.target.value = "";
                  if (file) {
                    void uploadKey(file);
                  }
                }}
              />
            </>
          ) : (
            <button
              className="button button--sm button--secondary"
              disabled={!info?.running}
              onClick={() => void topUp()}
              title={`The ledger admin credits ${amount(WALLET_TOP_UP)} to your wallet, creating it if needed`}
            >
              {wallet ? "Add funds" : "Create wallet"}
            </button>
          )}
        </span>
        <span className={styles.group}>
          <select
            className={styles.examples}
            value=""
            disabled={!info?.running || publishing}
            onChange={(e) => void publishExample(e.target.value)}
          >
            <option value="" disabled>
              Publish an example…
            </option>
            {Object.keys(EXAMPLES).map((name) => (
              <option key={name}>{name}</option>
            ))}
          </select>
          <button
            className="button button--sm button--primary"
            disabled={!info?.running || publishing}
            onClick={() => picker.current?.click()}
          >
            {publishing ? "Publishing…" : "Publish contract"}
          </button>
          <input
            ref={picker}
            type="file"
            accept=".star,.wasm"
            multiple
            hidden
            onChange={(e) => {
              const files = Array.from(e.target.files ?? []);
              e.target.value = "";
              void publish(files);
            }}
          />
        </span>
        <button
          className={`button button--sm ${terminal ? "button--primary" : "button--secondary"}`}
          aria-pressed={terminal}
          onClick={() => {
            setTerminal((open) => !open);
            setTerminalUsed(true);
          }}
        >
          Terminal
        </button>
      </header>

      {error ? (
        <div className={`alert alert--danger ${styles.banner}`}>
          <button
            className="clean-btn close"
            aria-label="Dismiss"
            onClick={() => setError(undefined)}
          >
            ×
          </button>
          <pre>{error}</pre>
        </div>
      ) : null}
      {explorer?.error ? (
        <div className={`alert alert--warning ${styles.banner}`}>
          failed to read the ledger: {explorer.error}
        </div>
      ) : null}

      <div className={styles.main}>
        <section className={styles.content}>
          {state ? (
            <Tables
              state={state}
              wallet={wallet}
              files={files}
              selection={selection}
              onSelect={select}
              onDownload={(path) => post(worker, { type: "download", path })}
            />
          ) : (
            <div className={styles.empty}>
              <p>
                {info?.running
                  ? "Reading the ledger…"
                  : info?.error
                    ? "The ledger failed to start, see the node settings."
                    : info
                      ? "The ledger is stopped, start it in the node settings."
                      : "Starting an in-browser ledger…"}
              </p>
            </div>
          )}
        </section>

        <aside className={styles.sidebar}>
          <div className={styles.tabs} role="tablist">
            {(
              [
                ["details", "Details"],
                ["node", "Node"],
              ] as [Panel, string][]
            ).map(([id, label]) => (
              <button
                key={id}
                role="tab"
                aria-selected={panel === id}
                className={panel === id ? styles.tabActive : styles.tab}
                onClick={() => setPanel(id)}
              >
                {label}
              </button>
            ))}
          </div>
          <div className={styles.panel}>
            {panel === "details" && state ? (
              <Details
                selection={selection}
                state={state}
                wallet={wallet}
                published={explorer?.published ?? {}}
                call={call}
                perform={perform}
                onGenesis={(genesisContract, genesisScript) => {
                  setPreset({ genesisContract, genesisScript });
                  setPanel("node");
                }}
                onSelect={select}
              />
            ) : null}
            <div hidden={panel !== "node"}>
              <NodeSettings
                info={info}
                preset={preset}
                files={files}
                call={call}
                onStart={(config: NodeConfig) => {
                  setPreset(undefined);
                  post(worker, { type: "start", config });
                }}
                onStop={() => post(worker, { type: "stop" })}
                onConnect={(url) => post(worker, { type: "connect", url })}
                onDownload={(path) => post(worker, { type: "download", path })}
              />
            </div>
          </div>
        </aside>
      </div>

      {worker && terminalUsed ? (
        <Terminal worker={worker} open={terminal} />
      ) : null}

      {dragging > 0 ? (
        <div className={styles.dropzone}>
          <p>
            Drop a Starstream source (<code>.star</code>) or a component (
            <code>.wasm</code>) to publish it
          </p>
        </div>
      ) : null}
    </div>
  );
}

export default function LedgerPage() {
  const [jspi, setJspi] = useState<boolean>();
  useEffect(() => setJspi("Suspending" in WebAssembly), []);
  return (
    <Layout
      title="Ledger"
      description="Explore a Starstream ledger running in the browser"
      noFooter
    >
      {jspi ? (
        <Explorer />
      ) : jspi === false ? (
        <main className="container margin-vert--xl">
          <h1>Ledger</h1>
          <p>
            This browser does not support JSPI (
            <code>WebAssembly.Suspending</code>), which the in-browser ledger
            requires.
          </p>
        </main>
      ) : null}
    </Layout>
  );
}
