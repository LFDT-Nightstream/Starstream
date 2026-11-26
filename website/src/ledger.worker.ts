import starstreamWasm from "file-loader!../starstream.wasm";
import starstreamLedgerWasm from "file-loader!../starstream_ledger_web.wasm";
import starstreamLedgerClientWasm from "file-loader!../starstream_ledger_client_web.wasm";
import starstreamCompilerWasm from "file-loader!../starstream_compiler_web.wasm";
import { Directory, File, type Inode } from "@bjorn3/browser_wasi_shim";
import {
  digest,
  type FuncType,
  Ledger,
  type Signature,
  TransactionInput,
  type Type,
  WasmClient,
  WasmCompiler,
  WasmLedger,
} from "../../starstream-js/src/index";
import { runStarstream } from "./starstream";
import { DEFAULT_CONFIG, type NodeConfig } from "./ledger-config";
import { Explorer, type ExplorerState } from "./ledger-explorer";

/** Status of the ledger the page is connected to. */
export interface NodeInfo {
  running: boolean;
  /**
   * URL of the ledger, through the service worker for the in-browser one.
   */
  url: string;
  /** Whether the ledger is a remote one rather than the in-browser one. */
  remote: boolean;
  config: NodeConfig;
  network?: string;
  adminPublicKey?: string;
  error?: string;
}

/** Contract published from the page. */
export interface Published {
  /** Path of the component. */
  path: string;
}

/** Coordination script or UTXO method. */
export interface Func {
  name: string;
  /** Parameters, UTXO ones of type `TransactionInput`. */
  params: { name: string; ty: Type; utxo: boolean }[];
  result?: Type;
}

/** Key signing on behalf of the page. */
export type Signer = "wallet" | "admin";

/** Action of the page. */
export type LedgerCall =
  | { op: "publish"; files: { name: string; data: Uint8Array }[] }
  | { op: "publishFile"; path: string }
  | { op: "fund"; amount: bigint }
  | { op: "wallet"; key: Uint8Array }
  | {
      op: "script";
      contract: string;
      script: string;
      args: unknown[];
      signer: Signer;
    }
  | { op: "scripts"; contract: string }
  | { op: "fileScripts"; path: string }
  | { op: "utxoMethods"; contract: string; instance: string }
  | {
      op: "utxo";
      utxo: TransactionInput;
      method: string;
      ty: FuncType;
      args: unknown[];
    }
  | { op: "download"; contract: string };

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
  | { type: "connect"; url: string }
  | { type: "stop" }
  | { type: "explore" }
  | { type: "download"; path: string }
  | { type: "call"; id: number; call: LedgerCall }
  | { type: "http"; request: HttpRequest; port: MessagePort };

export type LedgerWorkerResponse =
  | { type: "stdout" | "stderr"; text: string }
  | { type: "done" }
  | { type: "download"; path: string; data: Uint8Array }
  | { type: "node"; info: NodeInfo }
  | {
      type: "explorer";
      state: ExplorerState | undefined;
      /** Hex-encoded public key of the wallet, if any. */
      wallet: string | undefined;
      /** Contracts published from the page, by digest. */
      published: Record<string, Published>;
      error?: string;
    }
  | { type: "files"; paths: string[] }
  | { type: "reply"; id: number; result?: unknown; error?: string };

/** Path the service worker serves the in-browser ledger under. */
export const LEDGER_PATH = "/ledger-node/";

const LEDGER_URL = new URL(LEDGER_PATH, self.location.origin).href;
/** Signing key of the account the page publishes contracts and calls scripts with. */
const WALLET = "wallet.key";
const ADMIN = "admin.key";
/** Amount the page funds a new wallet with. */
const WALLET_FUNDS = 1_000_000n;

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
  const file = lookup(ADMIN);
  if (file instanceof File) {
    return publicKey(
      unhex(decoder.decode(file.data).trim(), 32, "key in `admin.key`"),
    );
  }
  const seed = crypto.getRandomValues(new Uint8Array(32));
  writeFile(ADMIN, encoder.encode(`${hex(seed)}\n`));
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

/** The in-browser ledger, unless connected to a remote one. */
let node: WasmLedger | undefined;
let ledger: Ledger | undefined;
let client: WasmClient | undefined;
let explorer: Explorer | undefined;
let info: NodeInfo = {
  running: false,
  url: LEDGER_URL,
  remote: false,
  config: DEFAULT_CONFIG,
};

function stopNode(): void {
  node?.dispose();
  client?.dispose();
  node = undefined;
  ledger = undefined;
  client = undefined;
  explorer = undefined;
  info = {
    ...info,
    running: false,
    url: LEDGER_URL,
    remote: false,
    error: undefined,
  };
}

/**
 * Connect the page to the ledger at `url`, sending requests through `fetch`,
 * and run `stop` if that fails.
 */
async function connect(
  url: string,
  fetch: typeof globalThis.fetch,
  admin: Uint8Array | undefined,
  stop = () => {},
): Promise<void> {
  const nextLedger = new Ledger(url, { fetch });
  let nextClient: WasmClient;
  let network: string;
  try {
    nextClient = await WasmClient.instantiate(
      await loadModule(starstreamLedgerClientWasm),
      url,
      { fetch },
    );
    network = await nextLedger.getNetworkName();
  } catch (err) {
    stop();
    throw err;
  }
  stopNode();
  ledger = nextLedger;
  client = nextClient;
  explorer = new Explorer(nextLedger, network, admin && hex(admin));
  info = {
    ...info,
    running: true,
    network,
    adminPublicKey: admin && hex(admin),
    error: undefined,
  };
}

async function connectRemote(url: string): Promise<void> {
  try {
    const base = new URL(url.trim()).href;
    await connect(base, (input, init) => fetch(input, init), undefined);
    info = { ...info, url: base, remote: true };
  } catch (err) {
    info = { ...info, error: errorMessage(err) };
  }
  send({ type: "node", info });
}

async function startNode(config: NodeConfig): Promise<void> {
  try {
    const admin = await adminKey();
    const imports = config.imports.map(readFile);
    const scripts = [];
    if (config.genesisContract) {
      const wasm = readFile(config.genesisContract);
      if (!config.imports.includes(config.genesisContract)) {
        imports.push(wasm);
      }
      scripts.push({
        contract: await digest(wasm),
        script: config.genesisScript,
        args: config.genesisArgs,
      });
    }
    const next = await WasmLedger.start(
      await loadModule(starstreamLedgerWasm),
      {
        network: config.network.trim() || "dev",
        admin,
        genesis: { scripts },
        imports,
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
    await connect("http://ledger/", next.fetch, admin, () => next.dispose());
    node = next;
    info = { ...info, url: LEDGER_URL, remote: false, config };
  } catch (err) {
    info = { ...info, error: errorMessage(err) };
  }
  send({ type: "node", info });
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
  err: (text: string) => void,
  fetch: typeof globalThis.fetch,
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
      let code: number;
      try {
        code = await runStarstream(await loadModule(starstreamWasm), args, {
          dir: root,
          env: { STARSTREAM_LEDGER_URL: info.url },
          fetch,
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
      (text) => {
        if (text) {
          send({ type: "stderr", text });
        }
      },
      abortable(ledgerFetch),
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

/** `exec` requests received. */
let execs = 0;
/** The last `exec` request cancelled, which is skipped if it has not started. */
let cancelled = 0;

async function handle(f: () => Promise<void>, exec?: number): Promise<void> {
  controller = new AbortController();
  if (exec !== undefined && exec === cancelled) {
    controller.abort();
  }
  try {
    if (!controller.signal.aborted) {
      await f();
    }
  } catch (err) {
    if (!controller.signal.aborted && !(err instanceof Exit)) {
      stderr(`error: ${errorMessage(err)}`);
    }
  }
  sendFiles();
  send({ type: "done" });
}

// ----------------------------------------------------------------------------
// Page actions

type ExplorerMessage = Extract<LedgerWorkerResponse, { type: "explorer" }>;

const published: Record<string, Published> = {};

function running(): { ledger: Ledger; client: WasmClient } {
  if (!ledger || !client) {
    throw new Error("not connected to a ledger");
  }
  return { ledger, client };
}

async function describe(wasm: Uint8Array): Promise<Func[]> {
  const describer = await WasmClient.instantiate(
    await loadModule(starstreamLedgerClientWasm),
    "http://ledger/",
    { fetch: () => Promise.reject(new Error("offline")) },
  );
  const scripts = await describer.getCoordinationScripts(wasm);
  return scripts.map(func);
}

function secretKey(path: string): Uint8Array {
  const file = lookup(path);
  if (!(file instanceof File)) {
    throw new Error(`\`${path}\` is missing`);
  }
  return unhex(decoder.decode(file.data).trim(), 32, `key in \`${path}\``);
}

async function walletKey(): Promise<string | undefined> {
  if (!(lookup(WALLET) instanceof File)) {
    return undefined;
  }
  return hex(await publicKey(secretKey(WALLET)));
}

/**
 * Get the secret key of the wallet, generating it if missing on the
 * in-browser ledger.
 */
function walletSeed(): Uint8Array {
  if (!(lookup(WALLET) instanceof File)) {
    if (info.remote) {
      throw new Error(
        "upload the secret key of a funded account of the remote ledger as your wallet",
      );
    }
    const seed = crypto.getRandomValues(new Uint8Array(32));
    writeFile(WALLET, encoder.encode(`${hex(seed)}\n`));
    sendFiles();
  }
  return secretKey(WALLET);
}

/**
 * Get the secret key of the wallet, like `walletSeed`, funding its account on
 * the in-browser ledger if it has no balance.
 */
async function wallet(): Promise<Uint8Array> {
  const { ledger, client } = running();
  const seed = walletSeed();
  if (info.remote) {
    return seed;
  }
  const account = await publicKey(seed);
  const balance = await ledger.getAccountBalance(account);
  if (!balance) {
    await client.fund(secretKey(ADMIN), account, WALLET_FUNDS);
  }
  return seed;
}

async function compile(path: string): Promise<Uint8Array> {
  const compiler = await WasmCompiler.instantiate(
    await loadModule(starstreamCompilerWasm),
  );
  const sources = Object.fromEntries(
    files().filter(
      ([path]) => path.endsWith(".star") || path.endsWith(".wasm"),
    ),
  );
  const { component } = compiler.compile(path, sources);
  return component;
}

/**
 * Publish `files`, compiling Starstream sources first, and return their
 * digests. Files another of the sources imports are only written.
 */
async function publish(
  files: { name: string; data: Uint8Array }[],
): Promise<string[]> {
  const { ledger, client } = running();
  const named = files.map(({ name, data }) => ({
    path: name.split("/").pop()!,
    data,
  }));
  for (const { path } of named) {
    if (!path.endsWith(".star") && !path.endsWith(".wasm")) {
      throw new Error(
        `\`${path}\` is neither a Starstream source (.star) nor a component (.wasm)`,
      );
    }
  }
  const imported = new Set<string>();
  for (const { path, data } of named) {
    writeFile(path, data);
    if (path.endsWith(".star")) {
      for (const [, from] of decoder
        .decode(data)
        .matchAll(/from\s+"([^"]+)"/g)) {
        imported.add(from.split("/").pop()!);
      }
    }
  }
  sendFiles();
  const digests = [];
  for (const { path, data } of named.filter(
    ({ path }) => !imported.has(path),
  )) {
    let wasm = path;
    let component = data;
    if (path.endsWith(".star")) {
      wasm = `${path.slice(0, -".star".length)}.wasm`;
      component = await compile(path);
      writeFile(wasm, component);
      sendFiles();
    }
    let contract = await digest(component);
    if (!(await ledger.getContractEnvelope(contract))) {
      contract = await client.publishContract(await wallet(), component);
    }
    published[contract] = { path: wasm };
    digests.push(contract);
  }
  return digests;
}

function func({ name, params, result }: Signature): Func {
  return {
    name,
    params: params.map(({ name, ty }) => ({
      name,
      ty,
      utxo: ty === TransactionInput,
    })),
    result,
  };
}

async function perform(call: LedgerCall): Promise<unknown> {
  switch (call.op) {
    case "publish":
      return publish(call.files);
    case "publishFile":
      return publish([{ name: call.path, data: readFile(call.path) }]);
    case "fund": {
      const { client } = running();
      const account = await publicKey(walletSeed());
      return client.fund(secretKey(ADMIN), account, call.amount);
    }
    case "wallet":
      unhex(decoder.decode(call.key).trim(), 32, "wallet key");
      writeFile(WALLET, call.key);
      sendFiles();
      return undefined;
    case "script": {
      const { client } = running();
      const key = call.signer === "admin" ? secretKey(ADMIN) : await wallet();
      const { transaction, result } = await client.callCoordinationScript(
        call.contract,
        call.script,
        call.args,
        info.remote
          ? undefined
          : {
              cardanoBlockHeight: optional(
                info.config.cardanoBlockHeight,
                (s) => integer(s, "Cardano block height", 64, true),
              ),
              cardanoCurrentSlot: optional(
                info.config.cardanoCurrentSlot,
                (s) => integer(s, "Cardano current slot", 64, true),
              ),
            },
      );
      try {
        return {
          transaction: await client.submitTransaction(key, transaction),
          result,
        };
      } catch (err) {
        return { result, error: errorMessage(err) };
      }
    }
    case "scripts": {
      const scripts = await running().client.getCoordinationScripts(
        call.contract,
      );
      return scripts.map(func);
    }
    case "fileScripts":
      return describe(readFile(call.path));
    case "utxoMethods": {
      const methods = await running().client.getUtxoMethods(
        call.contract,
        call.instance,
      );
      return methods.map(func);
    }
    case "utxo":
      return running().ledger.callUtxoMethod(
        call.utxo,
        call.method,
        call.ty,
        call.args,
      );
    case "download": {
      const data = await running().ledger.getContractWasm(call.contract);
      if (!data) {
        throw new Error(`contract \`${call.contract}\` not found`);
      }
      const bytes = data.slice();
      send({ type: "download", path: `${call.contract}.wasm`, data: bytes }, [
        bytes.buffer,
      ]);
      return undefined;
    }
  }
}

function isWrite(call: LedgerCall): boolean {
  return (
    call.op === "publish" ||
    call.op === "publishFile" ||
    call.op === "fund" ||
    call.op === "wallet" ||
    call.op === "script"
  );
}

async function explore(): Promise<ExplorerMessage> {
  const message: ExplorerMessage = {
    type: "explorer",
    state: undefined,
    wallet: undefined,
    published,
  };
  const current = explorer;
  try {
    message.state = await current?.state();
    message.wallet = await walletKey();
  } catch (err) {
    message.error = errorMessage(err);
  }
  if (explorer !== current) {
    message.state = undefined;
    message.error = undefined;
  }
  return message;
}

function reply(id: number, f: () => Promise<unknown>): Promise<void> {
  return f().then(
    (result) => send({ type: "reply", id, result }),
    (err: unknown) => send({ type: "reply", id, error: errorMessage(err) }),
  );
}

let queue = Promise.resolve();
let nodeQueue = Promise.resolve();
let refreshing: Promise<ExplorerMessage> | undefined;

function enqueue(f: () => Promise<void>): void {
  queue = queue.then(f);
}

function enqueueNode(f: () => Promise<void>): void {
  nodeQueue = nodeQueue.then(f);
}

self.addEventListener("message", ({ data }: { data: LedgerWorkerRequest }) => {
  switch (data.type) {
    case "cancel":
      cancelled = execs;
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
    case "explore":
      refreshing ??= explore().finally(() => {
        refreshing = undefined;
      });
      void refreshing.then(send);
      return;
    case "call": {
      const { id, call } = data;
      if (isWrite(call)) {
        enqueue(() => reply(id, () => perform(call)));
      } else {
        void reply(id, () => perform(call));
      }
      return;
    }
    case "start":
      enqueueNode(() => startNode(data.config));
      return;
    case "connect":
      enqueueNode(() => connectRemote(data.url));
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
    case "exec": {
      const id = ++execs;
      enqueue(() => handle(() => exec(data.line), id));
    }
  }
});

enqueueNode(() => startNode(DEFAULT_CONFIG));
enqueue(() => nodeQueue);
