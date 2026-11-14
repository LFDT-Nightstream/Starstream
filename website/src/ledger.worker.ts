import starstreamWasm from "file-loader!../starstream.wasm";
import starstreamLedgerWasm from "file-loader!../starstream_ledger_web.wasm";
import { Directory, File, type Inode } from "@bjorn3/browser_wasi_shim";
import {
  type Genesis,
  Ledger,
  WasmLedger,
} from "../../starstream-js/src/index";
import { parse } from "smol-toml";
import { runStarstream } from "./starstream";
import { DEFAULT_CONFIG, type NodeConfig } from "./ledger-config";

/** Status of the in-browser ledger. */
export interface NodeInfo {
  running: boolean;
  /** URL the ledger is served at, through the service worker. */
  url: string;
  config: NodeConfig;
  network?: string;
  adminPublicKey?: string;
  error?: string;
}

/** Snapshot of the state of the in-browser ledger. */
export interface LedgerState {
  height: bigint;
  adminNonce: bigint;
  genesis: { contract: string; instance: string }[];
  /** The latest blocks, newest first, with the size of each envelope. */
  blocks: { height: bigint; envelopes: number[] }[];
}

/** Request forwarded from the service worker. */
export interface HttpRequest {
  method: string;
  url: string;
  headers: [string, string][];
  body: ArrayBuffer | undefined;
}

/** Response to a `HttpRequest`. */
export interface HttpResponse {
  status: number;
  headers: [string, string][];
  body: ArrayBuffer;
}

export type LedgerWorkerRequest =
  | { type: "exec"; line: string }
  | { type: "upload"; files: { path: string; data: Uint8Array }[] }
  | { type: "cancel" }
  | { type: "start"; config: NodeConfig }
  | { type: "stop" }
  | { type: "state" }
  | { type: "download"; path: string }
  | { type: "http"; request: HttpRequest; port: MessagePort };

export type LedgerWorkerResponse =
  | { type: "stdout" | "stderr"; text: string }
  | { type: "done" }
  | { type: "download"; path: string; data: Uint8Array }
  | { type: "node"; info: NodeInfo }
  | { type: "state"; state: LedgerState | undefined }
  | { type: "files"; paths: string[] };

/** Path the service worker serves the in-browser ledger under. */
export const LEDGER_PATH = "/ledger-node/";

const LEDGER_URL = new URL(LEDGER_PATH, self.location.origin).href;
const MAX_BLOCKS = 20;

function send(r: LedgerWorkerResponse, transfer?: Transferable[]): void {
  self.postMessage(r, { transfer });
}

function stdout(text: string): void {
  send({ type: "stdout", text });
}

function stderr(text: string): void {
  send({ type: "stderr", text: text.endsWith("\n") ? text : `${text}\n` });
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function errorMessage(err: unknown): string {
  if (!(err instanceof Error)) {
    return String(err);
  }
  return err.cause === undefined
    ? err.message
    : `${err.message}: ${errorMessage(err.cause)}`;
}

// ----------------------------------------------------------------------------
// Files

const root = new Directory([]);

function names(path: string): string[] {
  return path.split("/").filter((name) => name && name !== ".");
}

function lookup(path: string): Inode | undefined {
  let inode: Inode | undefined = root;
  for (const name of names(path)) {
    inode = inode instanceof Directory ? inode.contents.get(name) : undefined;
  }
  return inode;
}

function readFile(path: string): Uint8Array {
  const file = lookup(path);
  if (!(file instanceof File)) {
    throw new Error(`\`${path}\` not found, see \`ls\``);
  }
  return file.data;
}

function writeFile(path: string, data: Uint8Array): void {
  const parts = names(path);
  const name = parts.pop();
  if (!name) {
    throw new Error("missing file name");
  }
  let dir = root;
  for (const part of parts) {
    let child = dir.contents.get(part);
    if (!child) {
      child = new Directory([]);
      dir.contents.set(part, child);
    }
    if (!(child instanceof Directory)) {
      throw new Error(`\`${part}\` is not a directory`);
    }
    dir = child;
  }
  if (dir.contents.get(name) instanceof Directory) {
    throw new Error(`\`${path}\` is a directory`);
  }
  dir.contents.set(name, new File(data));
}

function removeFile(path: string): void {
  const parts = names(path);
  const name = parts.pop();
  const dir = lookup(parts.join("/"));
  if (!name || !(dir instanceof Directory) || !dir.contents.delete(name)) {
    throw new Error(`\`${path}\` not found, see \`ls\``);
  }
}

function files(dir = root, prefix = ""): [string, Uint8Array][] {
  return [...dir.contents]
    .sort(([a], [b]) => a.localeCompare(b))
    .flatMap(([name, inode]): [string, Uint8Array][] => {
      const path = `${prefix}${name}`;
      if (inode instanceof Directory) {
        return files(inode, `${path}/`);
      }
      return inode instanceof File ? [[path, inode.data]] : [];
    });
}

function sendFiles(): void {
  send({ type: "files", paths: files().map(([path]) => path) });
}

// ----------------------------------------------------------------------------
// Keys

const PKCS8_ED25519 = Uint8Array.of(
  0x30,
  0x2e,
  0x02,
  0x01,
  0x00,
  0x30,
  0x05,
  0x06,
  0x03,
  0x2b,
  0x65,
  0x70,
  0x04,
  0x22,
  0x04,
  0x20,
);

function hex(bytes: Uint8Array): string {
  return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

function unhex(s: string, len: number, what: string): Uint8Array {
  if (!new RegExp(`^[0-9a-fA-F]{${len * 2}}$`).test(s)) {
    throw new Error(`${what} is not a valid hex-encoded ${len} bytes`);
  }
  return Uint8Array.from(s.match(/../g)!, (b) => parseInt(b, 16));
}

async function publicKey(seed: Uint8Array): Promise<Uint8Array> {
  const pkcs8 = new Uint8Array(PKCS8_ED25519.length + seed.length);
  pkcs8.set(PKCS8_ED25519);
  pkcs8.set(seed, PKCS8_ED25519.length);
  const key = await crypto.subtle.importKey("pkcs8", pkcs8, "Ed25519", true, [
    "sign",
  ]);
  const { x } = await crypto.subtle.exportKey("jwk", key);
  return Uint8Array.from(atob(x!.replace(/-/g, "+").replace(/_/g, "/")), (c) =>
    c.charCodeAt(0),
  );
}

/** Get the public key of `admin.key`, generating it if missing. */
async function adminKey(): Promise<Uint8Array> {
  const file = lookup("admin.key");
  if (file instanceof File) {
    return publicKey(
      unhex(decoder.decode(file.data).trim(), 32, "key in `admin.key`"),
    );
  }
  const seed = crypto.getRandomValues(new Uint8Array(32));
  writeFile("admin.key", encoder.encode(`${hex(seed)}\n`));
  sendFiles();
  return publicKey(seed);
}

// ----------------------------------------------------------------------------
// Ledger

const modules = new Map<string, Promise<WebAssembly.Module>>();

function loadModule(url: string): Promise<WebAssembly.Module> {
  let module = modules.get(url);
  if (!module) {
    const loading = (async () => {
      const res = await fetch(url);
      if (!res.ok) {
        throw new Error(`failed to fetch \`${url}\`: ${res.status}`);
      }
      return WebAssembly.compile(await res.arrayBuffer());
    })();
    modules.set(url, loading);
    loading.catch(() => {
      if (modules.get(url) === loading) {
        modules.delete(url);
      }
    });
    module = loading;
  }
  return module;
}

function integer(s: string, what: string, bits: number, signed = false) {
  if (!(signed ? /^-?\d+$/ : /^\d+$/).test(s)) {
    throw new Error(`${what} is not a valid integer`);
  }
  const n = BigInt(s);
  if (n !== (signed ? BigInt.asIntN(bits, n) : BigInt.asUintN(bits, n))) {
    throw new Error(`${what} is out of range`);
  }
  return n;
}

function optional<T>(s: string, f: (s: string) => T): T | undefined {
  return s.trim() === "" ? undefined : f(s.trim());
}

let node: WasmLedger | undefined;
/** The genesis outputs and the envelope sizes of the blocks of `node`. */
let known: {
  genesis?: LedgerState["genesis"];
  blocks: Map<bigint, number[]>;
} = { blocks: new Map() };
let info: NodeInfo = {
  running: false,
  url: LEDGER_URL,
  config: DEFAULT_CONFIG,
};

function api(node: WasmLedger): Ledger {
  return new Ledger("http://ledger/", { fetch: node.fetch });
}

function stopNode(): void {
  node?.dispose();
  node = undefined;
  known = { blocks: new Map() };
  info = { ...info, running: false, error: undefined };
}

async function startNode(config: NodeConfig): Promise<void> {
  try {
    const admin = config.admin.trim()
      ? unhex(config.admin.trim(), 32, "admin key")
      : await adminKey();
    const next = await WasmLedger.start(
      await loadModule(starstreamLedgerWasm),
      {
        network: config.network.trim() || "dev",
        admin,
        genesis: optional(
          config.genesis,
          (path) => parse(decoder.decode(readFile(path))) as Genesis,
        ),
        imports: config.imports.map(readFile),
        fuel: optional(config.fuel, (s) => integer(s, "fuel", 64)),
        maxRequests: optional(config.maxRequests, (s) =>
          Number(integer(s, "maximum requests", 32)),
        ),
        cardanoBlockHeight: optional(config.cardanoBlockHeight, (s) =>
          integer(s, "Cardano block height", 64, true),
        ),
        cardanoCurrentSlot: optional(config.cardanoCurrentSlot, (s) =>
          integer(s, "Cardano current slot", 64, true),
        ),
      },
    );
    const network = await api(next)
      .getNetworkName()
      .catch((err: unknown) => {
        next.dispose();
        throw err;
      });
    stopNode();
    node = next;
    info = {
      running: true,
      url: LEDGER_URL,
      config,
      network,
      adminPublicKey: hex(admin),
    };
  } catch (err) {
    info = { ...info, error: errorMessage(err) };
  }
  send({ type: "node", info });
}

async function state(): Promise<LedgerState | undefined> {
  if (!node) {
    return undefined;
  }
  const ledger = api(node);
  const cache = known;
  const height = await ledger.getBlockHeight();
  const adminNonce = await ledger.getAdminNonce();
  cache.genesis ??= (await ledger.getGenesisOutputs()).map(
    ({ contract, instance }) => ({ contract, instance }),
  );
  const latest = [];
  for (let h = height; h > 0n && latest.length < MAX_BLOCKS; h--) {
    let envelopes = cache.blocks.get(h);
    if (!envelopes) {
      envelopes = ((await ledger.getBlock(h)) ?? []).map((e) => e.length);
      cache.blocks.set(h, envelopes);
    }
    latest.push({ height: h, envelopes });
  }
  return { height, adminNonce, genesis: cache.genesis, blocks: latest };
}

/** Send requests to `LEDGER_URL` to the in-browser ledger, others out. */
const ledgerFetch: typeof fetch = async (input, init) => {
  const request = new Request(input, init);
  if (!request.url.startsWith(LEDGER_URL)) {
    return fetch(request);
  }
  if (!node) {
    return new Response("the in-browser ledger is not running", {
      status: 503,
    });
  }
  return node.fetch(
    new Request(
      `http://ledger/${request.url.slice(LEDGER_URL.length)}`,
      request,
    ),
  );
};

async function serve(
  { method, url, headers, body }: HttpRequest,
  port: MessagePort,
): Promise<void> {
  let response: HttpResponse;
  try {
    const res = await ledgerFetch(url, { method, headers, body });
    const resHeaders: [string, string][] = [];
    res.headers.forEach((value, name) => resHeaders.push([name, value]));
    response = {
      status: res.status,
      headers: resHeaders,
      body: await res.arrayBuffer(),
    };
  } catch (err) {
    response = {
      status: 502,
      headers: [],
      body: encoder.encode(errorMessage(err)).buffer as ArrayBuffer,
    };
  }
  port.postMessage(response, [response.body]);
}

let controller = new AbortController();

function abortable(f: typeof fetch): typeof fetch {
  return (input, init) =>
    f(input, {
      ...init,
      signal: init?.signal
        ? AbortSignal.any([init.signal, controller.signal])
        : controller.signal,
    });
}

// ----------------------------------------------------------------------------
// Command line

const REDIRECT = Symbol(">");

function tokenize(line: string): (string | typeof REDIRECT)[] {
  const args: (string | typeof REDIRECT)[] = [];
  let arg: string | undefined;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (/\s/.test(c) || c === ">") {
      if (arg !== undefined) {
        args.push(arg);
        arg = undefined;
      }
      if (c === ">") {
        args.push(REDIRECT);
      }
    } else if (c === "'") {
      const end = line.indexOf("'", i + 1);
      if (end < 0) {
        throw new Error("unterminated `'`");
      }
      arg = (arg ?? "") + line.slice(i + 1, end);
      i = end;
    } else if (c === '"') {
      arg ??= "";
      for (i++; line[i] !== '"'; i++) {
        if (i >= line.length) {
          throw new Error('unterminated `"`');
        }
        if (line[i] === "\\" && ['"', "\\"].includes(line[i + 1])) {
          i++;
        }
        arg += line[i];
      }
    } else if (c === "\\" && i + 1 < line.length) {
      arg = (arg ?? "") + line[++i];
    } else {
      arg = (arg ?? "") + c;
    }
  }
  if (arg !== undefined) {
    args.push(arg);
  }
  return args;
}

const USAGE = `Commands:
  starstream ARGS...  run the Starstream CLI, e.g. \`starstream --help\`,
                      \`starstream wasm -c FILE --output-component OUT\` or
                      \`starstream ledger network name\`
  ls                  list files
  cat PATH            print a file
  rm PATH             remove a file
  upload              add files from this computer
  download PATH       save a file to this computer
  echo ARGS...        print ARGS
  COMMAND > PATH      write the output of COMMAND to PATH
  clear               clear the screen

\`starstream ledger\` talks to the in-browser ledger, configured in the panel,
unless given \`--url\`. Quote arguments with spaces or quotes, for example
\`'"text"'\`.
`;

class Exit extends Error {
  constructor(readonly code: number) {
    super(`exited with code ${code}`);
  }
}

function path(argv: string[]): string {
  if (argv.length !== 2) {
    throw new Error(`\`${argv[0]}\` takes exactly one path`);
  }
  return argv[1];
}

async function run(
  argv: string[],
  out: (data: Uint8Array) => void,
): Promise<void> {
  const [command, ...args] = argv;
  switch (command) {
    case "help":
      return out(encoder.encode(USAGE));
    case "echo":
      return out(encoder.encode(`${args.join(" ")}\n`));
    case "ls":
      return out(
        encoder.encode(
          files()
            .map(
              ([path, data]) =>
                `${String(data.length).padStart(10)}  ${path}\n`,
            )
            .join(""),
        ),
      );
    case "cat":
      return out(readFile(path(argv)));
    case "rm":
      return removeFile(path(argv));
    case "download": {
      const name = path(argv);
      const data = readFile(name).slice();
      return send({ type: "download", path: name, data }, [data.buffer]);
    }
    case "starstream": {
      const errors = new TextDecoder();
      const err = (text: string) => {
        if (text) {
          send({ type: "stderr", text });
        }
      };
      let code: number;
      try {
        code = await runStarstream(await loadModule(starstreamWasm), args, {
          dir: root,
          env: { STARSTREAM_LEDGER_URL: LEDGER_URL },
          fetch: abortable(ledgerFetch),
          stdout: out,
          stderr: (data) => err(errors.decode(data, { stream: true })),
        });
      } finally {
        err(errors.decode());
      }
      if (code !== 0) {
        throw new Exit(code);
      }
      return;
    }
  }
  throw new Error(`command not found: \`${command}\`, see \`help\``);
}

async function exec(line: string): Promise<void> {
  const tokens = tokenize(line);
  const i = tokens.indexOf(REDIRECT);
  const argv = tokens
    .slice(0, i < 0 ? undefined : i)
    .filter((token) => typeof token === "string");
  if (argv.length === 0) {
    return;
  }
  let redirect: string | undefined;
  if (i >= 0) {
    const path = tokens[i + 1];
    if (tokens.length !== i + 2 || typeof path !== "string") {
      throw new Error("`>` must be followed by a single path");
    }
    redirect = path;
    writeFile(redirect, new Uint8Array());
  }
  const chunks: Uint8Array[] = [];
  const text = new TextDecoder();
  let last = "\n";
  const show = (s: string) => {
    if (s) {
      last = s;
      stdout(s);
    }
  };
  try {
    await run(
      argv,
      redirect
        ? (data) => chunks.push(data.slice())
        : (data) => show(text.decode(data, { stream: true })),
    );
  } finally {
    if (redirect) {
      const data = new Uint8Array(
        chunks.reduce((len, chunk) => len + chunk.length, 0),
      );
      let offset = 0;
      for (const chunk of chunks) {
        data.set(chunk, offset);
        offset += chunk.length;
      }
      writeFile(redirect, data);
    } else {
      show(text.decode());
      if (!last.endsWith("\n")) {
        stdout("\n");
      }
    }
  }
}

async function handle(f: () => Promise<void>): Promise<void> {
  controller = new AbortController();
  try {
    await f();
  } catch (err) {
    if (!controller.signal.aborted && !(err instanceof Exit)) {
      stderr(`error: ${errorMessage(err)}`);
    }
  }
  sendFiles();
  send({ type: "done" });
}

let queue = Promise.resolve();
let nodeQueue = Promise.resolve();
let refreshing: Promise<LedgerState | undefined> | undefined;

function enqueue(f: () => Promise<void>): void {
  queue = queue.then(f);
}

function enqueueNode(f: () => Promise<void>): void {
  nodeQueue = nodeQueue.then(f);
}

self.addEventListener("message", ({ data }: { data: LedgerWorkerRequest }) => {
  switch (data.type) {
    case "cancel":
      controller.abort();
      return;
    case "http":
      void serve(data.request, data.port);
      return;
    case "download": {
      const file = lookup(data.path);
      if (file instanceof File) {
        const bytes = file.data.slice();
        send({ type: "download", path: data.path, data: bytes }, [
          bytes.buffer,
        ]);
      }
      return;
    }
    case "state":
      refreshing ??= state().finally(() => {
        refreshing = undefined;
      });
      void refreshing.then(
        (state) => send({ type: "state", state }),
        () => send({ type: "state", state: undefined }),
      );
      return;
    case "start":
      enqueueNode(() => startNode(data.config));
      return;
    case "stop":
      enqueueNode(async () => {
        stopNode();
        send({ type: "node", info });
      });
      return;
    case "upload":
      enqueue(() =>
        handle(async () => {
          for (const { path, data: bytes } of data.files) {
            writeFile(path, bytes);
            stderr(`added \`${path}\``);
          }
        }),
      );
      return;
    case "exec":
      enqueue(() => handle(() => exec(data.line)));
  }
});

enqueueNode(() => startNode(DEFAULT_CONFIG));
enqueue(() => nodeQueue);
