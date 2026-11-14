import Layout from "@theme/Layout";
import { type FormEvent, useEffect, useRef, useState } from "react";
import type {
  LedgerWorkerRequest,
  LedgerWorkerResponse,
} from "../ledger.worker";

const PROMPT = "\x1b[1;32mledger\x1b[0m$ ";

function crlf(text: string): string {
  return text.replace(/\r?\n/g, "\r\n");
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
      worker.postMessage(
        { type: "upload", files } satisfies LedgerWorkerRequest,
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

type Source = { url?: string };

function Chooser({ onChoose }: { onChoose: (source: Source) => void }) {
  const [jspi, setJspi] = useState(true);
  const [url, setUrl] = useState("http://[::1]:9000/");
  useEffect(() => setJspi("Suspending" in WebAssembly), []);
  const connect = (e: FormEvent) => {
    e.preventDefault();
    onChoose({ url });
  };
  return (
    <main className="container margin-vert--xl ledger-chooser">
      <h1>Ledger</h1>
      <p>Choose the ledger the terminal talks to.</p>
      <button
        className="button button--primary button--lg"
        disabled={!jspi}
        onClick={() => onChoose({})}
      >
        In-browser ledger
      </button>
      {jspi ? null : (
        <p className="margin-top--sm">
          This browser does not support JSPI (
          <code>WebAssembly.Suspending</code>), which the in-browser ledger
          requires.
        </p>
      )}
      <form className="margin-top--lg" onSubmit={connect}>
        <label htmlFor="ledger-url">Ledger node URL</label>
        <div className="ledger-chooser__url">
          <input
            id="ledger-url"
            type="url"
            required
            value={url}
            onChange={(e) => setUrl(e.target.value)}
          />
          <button className="button button--secondary" type="submit">
            Connect
          </button>
        </div>
      </form>
    </main>
  );
}

export default function LedgerPage() {
  const container = useRef<HTMLDivElement>(null);
  const [source, setSource] = useState<Source>();

  useEffect(() => {
    if (!source) {
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
      const worker = new Worker(new URL("../ledger.worker", import.meta.url));
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
        worker.terminate();
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
      let busy = true;
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

      worker.addEventListener(
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
            case "done":
              prompt();
          }
        },
      );

      worker.postMessage({
        type: "start",
        url: source.url,
      } satisfies LedgerWorkerRequest);

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
          cancelUpload = upload(worker, (err) => {
            if (err !== undefined) {
              term.write(
                `\x1b[2merror: ${err instanceof Error ? err.message : String(err)}\x1b[0m\r\n`,
              );
            }
            prompt();
          });
        } else {
          worker.postMessage({
            type: "exec",
            line,
          } satisfies LedgerWorkerRequest);
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
          worker.postMessage({ type: "cancel" } satisfies LedgerWorkerRequest);
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
  }, [source]);

  return (
    <Layout
      title="Ledger terminal"
      description="Run a Starstream ledger in the browser"
      noFooter
    >
      {source ? (
        <div className="ledger-terminal" ref={container} />
      ) : (
        <Chooser onChoose={setSource} />
      )}
    </Layout>
  );
}
