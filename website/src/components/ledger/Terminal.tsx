import { useEffect, useRef } from "react";
import type {
  LedgerWorkerRequest,
  LedgerWorkerResponse,
} from "../../ledger.worker";
import styles from "./styles.module.css";

const PROMPT = "\x1b[1;32mledger\x1b[0m$ ";

function crlf(text: string): string {
  return text.replace(/\r?\n/g, "\r\n");
}

function post(
  worker: Worker,
  request: LedgerWorkerRequest,
  transfer: Transferable[] = [],
): void {
  worker.postMessage(request, transfer);
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

/** Shell running `starstream` and file commands in `worker`. */
export function Terminal({ worker, open }: { worker: Worker; open: boolean }) {
  const container = useRef<HTMLDivElement>(null);
  const fit = useRef<() => void>(undefined);

  useEffect(() => {
    if (open) {
      fit.current?.();
    }
  }, [open]);

  useEffect(() => {
    let disposed = false;
    let cleanup = () => {};

    void (async () => {
      const { init, FitAddon, Terminal } = await import("ghostty-web");
      await init();
      if (disposed || !container.current) {
        return;
      }
      const term = new Terminal({
        fontFamily: getComputedStyle(document.documentElement)
          .getPropertyValue("--ifm-font-family-monospace")
          .trim(),
        fontSize: 14,
        cursorBlink: true,
        theme: { background: "#1a1b26", foreground: "#a9b1d6" },
      });
      const addon = new FitAddon();
      term.loadAddon(addon);
      term.open(container.current);
      fit.current = () => {
        addon.fit();
        term.focus();
      };
      addon.fit();
      const onResize = () => addon.fit();
      window.addEventListener("resize", onResize);
      term.write(
        crlf(
          "Starstream ledger terminal. Type `help` for the available commands.\n",
        ),
      );

      const history: string[] = [];
      let entry = 0;
      let draft = "";
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

      const onMessage = ({ data }: { data: LedgerWorkerResponse }) => {
        switch (data.type) {
          case "stdout":
            term.write(crlf(data.text));
            return;
          case "stderr":
            term.write(`\x1b[2m${crlf(data.text)}\x1b[0m`);
            return;
          case "done":
            prompt();
        }
      };
      worker.addEventListener("message", onMessage);
      cleanup = () => {
        worker.removeEventListener("message", onMessage);
        window.removeEventListener("resize", onResize);
        fit.current = undefined;
        term.dispose();
      };
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
          cancelUpload = upload(worker, (err) => {
            if (err !== undefined) {
              term.write(
                `\x1b[2merror: ${err instanceof Error ? err.message : String(err)}\x1b[0m\r\n`,
              );
            }
            prompt();
          });
        } else {
          post(worker, { type: "exec", line });
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
          post(worker, { type: "cancel" });
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
          const next = Math.max(
            0,
            Math.min(history.length, entry + (data === "\x1b[A" ? -1 : 1)),
          );
          if (next !== entry) {
            if (entry === history.length) {
              draft = line;
            }
            entry = next;
            replace(history[entry] ?? draft);
          }
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
  }, [worker]);

  return (
    <div
      className={styles.terminal}
      ref={container}
      hidden={!open}
      aria-label="Ledger terminal"
    />
  );
}
