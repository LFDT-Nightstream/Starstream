import starstreamLedgerWasm from "file-loader!../starstream_ledger_web.wasm";
import starstreamLedgerClientWasm from "file-loader!../starstream_ledger_client_web.wasm";
import { parse as parseToml } from "smol-toml";
import {
  digest,
  type Genesis,
  Ledger,
  type Signer,
  type TransactionInput,
  WasmClient,
  WasmLedger,
} from "../../starstream-js/src/index";

export type LedgerWorkerRequest =
  | { type: "start"; url?: string }
  | { type: "exec"; line: string }
  | { type: "upload"; files: { path: string; data: Uint8Array }[] }
  | { type: "cancel" };

export type LedgerWorkerResponse =
  | { type: "stdout" | "stderr"; text: string }
  | { type: "done" }
  | { type: "download"; path: string; data: Uint8Array };

function send(r: LedgerWorkerResponse, transfer?: Transferable[]): void {
  self.postMessage(r, { transfer });
}

function stdout(text: string): void {
  send({ type: "stdout", text: text.endsWith("\n") ? text : `${text}\n` });
}

function stderr(text: string): void {
  send({ type: "stderr", text: text.endsWith("\n") ? text : `${text}\n` });
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const files = new Map<string, Uint8Array>();

const modules = new Map<string, Promise<WebAssembly.Module>>();
let node: WasmLedger | undefined;
let ledger: Ledger | undefined;
let client: Promise<WasmClient> | undefined;

async function compile(url: string): Promise<WebAssembly.Module> {
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(`failed to fetch \`${url}\`: ${res.status}`);
  }
  return WebAssembly.compile(await res.arrayBuffer());
}

function loadModule(url: string): Promise<WebAssembly.Module> {
  let module = modules.get(url);
  if (!module) {
    const loading = compile(url);
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

function currentLedger(): Ledger {
  if (!ledger) {
    throw new Error("no ledger, run `node` first");
  }
  return ledger;
}

function currentClient(): Promise<WasmClient> {
  const l = currentLedger();
  if (!client) {
    const loading = loadModule(starstreamLedgerClientWasm).then((module) =>
      WasmClient.instantiate(module, l),
    );
    client = loading;
    loading.catch(() => {
      if (client === loading) {
        client = undefined;
      }
    });
  }
  return client;
}

async function withClient<T>(f: (c: WasmClient) => Promise<T>): Promise<T> {
  const current = currentClient();
  const c = await current;
  try {
    return await f(c);
  } catch (err) {
    if (
      err instanceof WebAssembly.RuntimeError ||
      (err instanceof Error && err.cause instanceof WebAssembly.RuntimeError)
    ) {
      c.dispose();
      if (client === current) {
        client = undefined;
      }
    }
    throw err;
  }
}

function setLedger(next: Ledger, nextNode?: WasmLedger): void {
  node?.dispose();
  void client?.then(
    (c) => c.dispose(),
    () => {},
  );
  node = nextNode;
  ledger = next;
  client = undefined;
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
// Encoding

function hex(bytes: Uint8Array): string {
  return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

function unhex(s: string, len: number, what: string): Uint8Array {
  if (!new RegExp(`^[0-9a-fA-F]{${len * 2}}$`).test(s)) {
    throw new Error(`${what} is not a valid hex-encoded ${len} bytes`);
  }
  return Uint8Array.from(s.match(/../g)!, (b) => parseInt(b, 16));
}

function json(v: unknown): string {
  return JSON.stringify(
    v,
    (_, v) =>
      typeof v === "bigint"
        ? v.toString()
        : v instanceof Uint8Array
          ? hex(v)
          : v instanceof BigUint64Array
            ? Array.from(v, String)
            : v,
    2,
  );
}

function u64(s: string, what: string): bigint {
  if (!/^\d+$/.test(s)) {
    throw new Error(`${what} is not a valid unsigned integer`);
  }
  const n = BigInt(s);
  if (n !== BigInt.asUintN(64, n)) {
    throw new Error(`${what} is out of range`);
  }
  return n;
}

function i64(s: string, what: string): bigint {
  if (!/^-?\d+$/.test(s)) {
    throw new Error(`${what} is not a valid integer`);
  }
  const n = BigInt(s);
  if (n !== BigInt.asIntN(64, n)) {
    throw new Error(`${what} is out of range`);
  }
  return n;
}

function u32(s: string, what: string): number {
  const n = u64(s, what);
  if (n > 0xffffffffn) {
    throw new Error(`${what} is out of range`);
  }
  return Number(n);
}

// ----------------------------------------------------------------------------
// Keys

const PKCS8_ED25519 = unhex(
  "302e020100300506032b657004220420",
  16,
  "PKCS#8 prefix",
);

async function signer(path: string): Promise<Signer> {
  const seed = unhex(
    decoder.decode(readFile(path)).trim(),
    32,
    `key in \`${path}\``,
  );
  const der = new Uint8Array(PKCS8_ED25519.length + seed.length);
  der.set(PKCS8_ED25519);
  der.set(seed, PKCS8_ED25519.length);
  const key = await crypto.subtle.importKey(
    "pkcs8",
    der,
    { name: "Ed25519" },
    true,
    ["sign"],
  );
  const { x } = await crypto.subtle.exportKey("jwk", key);
  const publicKey = Uint8Array.from(
    atob(x!.replace(/-/g, "+").replace(/_/g, "/")),
    (c) => c.charCodeAt(0),
  );
  return {
    publicKey,
    sign: async (data) =>
      new Uint8Array(await crypto.subtle.sign({ name: "Ed25519" }, key, data)),
  };
}

async function adminKey(): Promise<Uint8Array> {
  if (!files.has("admin.key")) {
    const key = await generateKey();
    files.set("admin.key", encoder.encode(`${key.secret}\n`));
    stderr(`generated \`admin.key\` with public key ${key.public}`);
  }
  return (await signer("admin.key")).publicKey;
}

async function generateKey(): Promise<{ secret: string; public: string }> {
  const { privateKey, publicKey } = (await crypto.subtle.generateKey(
    { name: "Ed25519" },
    true,
    ["sign", "verify"],
  )) as CryptoKeyPair;
  const der = new Uint8Array(
    await crypto.subtle.exportKey("pkcs8", privateKey),
  );
  const raw = new Uint8Array(await crypto.subtle.exportKey("raw", publicKey));
  return { secret: hex(der.subarray(-32)), public: hex(raw) };
}

// ----------------------------------------------------------------------------
// Files

function readFile(path: string): Uint8Array {
  const data = files.get(path);
  if (!data) {
    throw new Error(`\`${path}\` not found, see \`ls\``);
  }
  return data;
}

// ----------------------------------------------------------------------------
// Command line

function tokenize(line: string): string[] {
  const args: string[] = [];
  let arg: string | undefined;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (/\s/.test(c)) {
      if (arg !== undefined) {
        args.push(arg);
        arg = undefined;
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

interface Parsed {
  flags: Map<string, string | string[] | true>;
  args: string[];
}

function parse(
  argv: string[],
  options: Record<string, "value" | "values" | "switch">,
): Parsed {
  const flags = new Map<string, string | string[] | true>();
  const args: string[] = [];
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--") {
      args.push(...argv.slice(i + 1));
      break;
    }
    if (!arg.startsWith("--")) {
      args.push(arg);
      continue;
    }
    const eq = arg.indexOf("=");
    const name = arg.slice(2, eq < 0 ? undefined : eq);
    const kind = options[name];
    if (!kind) {
      throw new Error(`unexpected argument \`${arg}\``);
    }
    if (kind === "switch") {
      flags.set(name, true);
      continue;
    }
    let value: string;
    if (eq >= 0) {
      value = arg.slice(eq + 1);
    } else if (i + 1 < argv.length) {
      value = argv[++i];
    } else {
      throw new Error(`\`${arg}\` requires a value`);
    }
    if (kind === "values") {
      const values = flags.get(name);
      flags.set(name, Array.isArray(values) ? [...values, value] : [value]);
    } else {
      flags.set(name, value);
    }
  }
  return { flags, args };
}

function flag(p: Parsed, name: string): string | undefined {
  const v = p.flags.get(name);
  return typeof v === "string" ? v : undefined;
}

function flagValues(p: Parsed, name: string): string[] {
  const v = p.flags.get(name);
  return Array.isArray(v) ? v : [];
}

function positional(p: Parsed, names: string[], rest = false): string[] {
  if (p.args.length < names.length) {
    throw new Error(`missing \`${names[p.args.length]}\``);
  }
  if (!rest && p.args.length > names.length) {
    throw new Error(`unexpected argument \`${p.args[names.length]}\``);
  }
  return p.args;
}

function utxoInput(p: Parsed, index: string): TransactionInput {
  return {
    transaction: flag(p, "transaction") ?? "",
    index: u32(index, "index"),
  };
}

const NONCE_OPTIONS = {
  key: "value",
  network: "value",
  nonce: "value",
} as const;

const USAGE = `Ledger commands, as in \`starstream ledger\`:
  account balance ACCOUNT
  account fund --key PATH [--network NAME] [--nonce N] ACCOUNT AMOUNT
  account nonce ACCOUNT
  admin nonce
  block get HEIGHT
  block height
  contract publish --key PATH [--network NAME] [--nonce N] WASM
  contract script call (--key PATH [--network NAME] | --simulate)
      [--cardano-block-height N] [--cardano-current-slot N]
      DIGEST SCRIPT [ARGS...]
  digest PATH
  genesis
  key generate
  network name
  transaction get DIGEST
  utxo call [--transaction DIGEST] INDEX METHOD [ARGS...]
  utxo wit [--transaction DIGEST] INDEX

Ledgers:
  node [--network NAME] [--admin KEY] [--fuel N] [--max-requests N]
      [--io-timeout SECONDS] [--cardano-block-height N]
      [--cardano-current-slot N] [--genesis PATH [--import PATH]...]
                  start a ledger in this browser tab, administered by
                  \`admin.key\` unless \`--admin\` is given, generating it if
                  missing

Files:
  ls              list files
  cat PATH        print a file
  rm PATH         remove a file
  upload          add files from this computer
  download PATH   save a file to this computer
  echo ARGS...    print ARGS
  COMMAND > PATH  write the output of COMMAND to PATH

  clear           clear the screen

ACCOUNT and KEY are hex-encoded Ed25519 public keys, key files hold a
hex-encoded Ed25519 secret key. Arguments are WAVE-encoded; UTXO script
arguments are \`[TRANSACTION]:INDEX\`, with an empty TRANSACTION for genesis.
Quote arguments with spaces or quotes, for example \`'"text"'\`.`;

async function run(argv: string[], out: (text: string) => void): Promise<void> {
  const [command, sub, ...rest] = argv;
  switch (command) {
    case "help":
      return out(USAGE);
    case "echo":
      return out(argv.slice(1).join(" "));
    case "ls":
      return out(
        [...files]
          .sort(([a], [b]) => a.localeCompare(b))
          .map(([path, data]) => `${String(data.length).padStart(10)}  ${path}`)
          .join("\n"),
      );
    case "cat":
      return out(
        decoder.decode(
          readFile(positional(parse(argv.slice(1), {}), ["PATH"])[0]),
        ),
      );
    case "rm": {
      const [path] = positional(parse(argv.slice(1), {}), ["PATH"]);
      readFile(path);
      files.delete(path);
      return;
    }
    case "download": {
      const [path] = positional(parse(argv.slice(1), {}), ["PATH"]);
      const data = readFile(path).slice();
      return send({ type: "download", path, data }, [data.buffer]);
    }
    case "digest": {
      const [path] = positional(parse(argv.slice(1), {}), ["PATH"]);
      return out(await digest(readFile(path)));
    }
    case "node": {
      const p = parse(argv.slice(1), {
        network: "value",
        admin: "value",
        fuel: "value",
        "max-requests": "value",
        "io-timeout": "value",
        "cardano-block-height": "value",
        "cardano-current-slot": "value",
        genesis: "value",
        import: "values",
      });
      positional(p, []);
      const genesis = flag(p, "genesis");
      const imports = flagValues(p, "import");
      if (imports.length > 0 && genesis === undefined) {
        throw new Error("`--import` requires `--genesis`");
      }
      const admin = flag(p, "admin");
      const fuel = flag(p, "fuel");
      const maxRequests = flag(p, "max-requests");
      const ioTimeout = flag(p, "io-timeout");
      const blockHeight = flag(p, "cardano-block-height");
      const currentSlot = flag(p, "cardano-current-slot");
      const module = await loadModule(starstreamLedgerWasm);
      const next = await WasmLedger.start(module, {
        network: flag(p, "network") ?? "dev",
        admin: admin ? unhex(admin, 32, "admin key") : await adminKey(),
        genesis:
          genesis === undefined
            ? undefined
            : (parseToml(decoder.decode(readFile(genesis))) as Genesis),
        imports: imports.map(readFile),
        fuel: fuel === undefined ? undefined : u64(fuel, "fuel"),
        maxRequests:
          maxRequests === undefined
            ? undefined
            : u32(maxRequests, "maximum requests"),
        ioTimeout:
          ioTimeout === undefined ? undefined : u32(ioTimeout, "I/O timeout"),
        cardanoBlockHeight:
          blockHeight === undefined
            ? undefined
            : i64(blockHeight, "Cardano block height"),
        cardanoCurrentSlot:
          currentSlot === undefined
            ? undefined
            : i64(currentSlot, "Cardano current slot"),
      });
      if (controller.signal.aborted) {
        next.dispose();
        return;
      }
      setLedger(
        new Ledger("http://ledger", { fetch: abortable(next.fetch) }),
        next,
      );
      return stderr(
        `started ledger \`${await currentLedger().getNetworkName()}\``,
      );
    }
    case "key":
      if (sub === "generate") {
        positional(parse(rest, {}), []);
        const key = await generateKey();
        stderr(`generated key pair with public key ${key.public}`);
        return out(key.secret);
      }
      break;
    case "network":
      if (sub === "name") {
        positional(parse(rest, {}), []);
        return out(await currentLedger().getNetworkName());
      }
      break;
    case "admin":
      if (sub === "nonce") {
        positional(parse(rest, {}), []);
        return out(String(await currentLedger().getAdminNonce()));
      }
      break;
    case "block":
      if (sub === "height") {
        positional(parse(rest, {}), []);
        return out(String(await currentLedger().getBlockHeight()));
      }
      if (sub === "get") {
        const [height] = positional(parse(rest, {}), ["HEIGHT"]);
        const envelopes = await currentLedger().getBlock(u64(height, "height"));
        if (!envelopes) {
          throw new Error(`block ${height} not found`);
        }
        return out(envelopes.map(hex).join("\n"));
      }
      break;
    case "genesis":
      positional(parse(argv.slice(1), {}), []);
      return out(json({ outputs: await currentLedger().getGenesisOutputs() }));
    case "transaction":
      if (sub === "get") {
        const [transaction] = positional(parse(rest, {}), ["DIGEST"]);
        const tx = await currentLedger().getTransaction(transaction);
        if (!tx) {
          throw new Error("transaction not found");
        }
        return out(json(tx));
      }
      break;
    case "account": {
      if (sub === "balance" || sub === "nonce") {
        const [account] = positional(parse(rest, {}), ["ACCOUNT"]);
        const key = unhex(account, 32, "account key");
        const v =
          sub === "balance"
            ? await currentLedger().getAccountBalance(key)
            : await currentLedger().getAccountNonce(key);
        if (v === undefined) {
          throw new Error(`account \`${account}\` not found`);
        }
        return out(String(v));
      }
      if (sub === "fund") {
        const p = parse(rest, NONCE_OPTIONS);
        const [account, amount] = positional(p, ["ACCOUNT", "AMOUNT"]);
        const nonce = flag(p, "nonce");
        return currentLedger().fund(
          await signer(requireKey(p)),
          unhex(account, 32, "account key"),
          u64(amount, "amount"),
          {
            network: flag(p, "network"),
            nonce: nonce === undefined ? undefined : u64(nonce, "nonce"),
          },
        );
      }
      break;
    }
    case "contract":
      if (sub === "publish") {
        const p = parse(rest, NONCE_OPTIONS);
        const [path] = positional(p, ["WASM"]);
        const nonce = flag(p, "nonce");
        return out(
          await currentLedger().publishContract(
            await signer(requireKey(p)),
            readFile(path),
            {
              network: flag(p, "network"),
              nonce: nonce === undefined ? undefined : u64(nonce, "nonce"),
            },
          ),
        );
      }
      if (sub === "script" && rest[0] === "call") {
        const p = parse(rest.slice(1), {
          key: "value",
          network: "value",
          simulate: "switch",
          "cardano-block-height": "value",
          "cardano-current-slot": "value",
        });
        const [contract, script, ...args] = positional(
          p,
          ["DIGEST", "SCRIPT"],
          true,
        );
        const simulate = p.flags.has("simulate");
        if (simulate && (p.flags.has("key") || p.flags.has("network"))) {
          throw new Error(
            "`--simulate` conflicts with `--key` and `--network`",
          );
        }
        const key = simulate ? undefined : await signer(requireKey(p));
        const blockHeight = flag(p, "cardano-block-height");
        const currentSlot = flag(p, "cardano-current-slot");
        const options = {
          cardanoBlockHeight:
            blockHeight === undefined
              ? undefined
              : i64(blockHeight, "Cardano block height"),
          cardanoCurrentSlot:
            currentSlot === undefined
              ? undefined
              : i64(currentSlot, "Cardano current slot"),
        };
        const call = await withClient((c) =>
          c.callCoordinationScriptWave(contract, script, args, options),
        );
        if (key) {
          const transaction = await currentLedger().submitTransaction(
            key,
            call.transaction,
            { network: flag(p, "network") },
          );
          stderr(`submitted transaction ${transaction}`);
        }
        return out(call.result);
      }
      break;
    case "utxo":
      if (sub === "wit") {
        const p = parse(rest, { transaction: "value" });
        const [index] = positional(p, ["INDEX"]);
        const wit = await currentLedger().getUtxoWit(utxoInput(p, index));
        if (wit === undefined) {
          throw new Error("UTXO not found");
        }
        return out(wit);
      }
      if (sub === "call") {
        const p = parse(rest, { transaction: "value" });
        const [index, method, ...args] = positional(
          p,
          ["INDEX", "METHOD"],
          true,
        );
        const input = utxoInput(p, index);
        return out(
          await withClient((c) => c.callUtxoMethodWave(input, method, args)),
        );
      }
      break;
    default:
      throw new Error(`command \`${command}\` not found, see \`help\``);
  }
  throw new Error(
    sub === undefined
      ? `\`${command}\` requires a subcommand, see \`help\``
      : `subcommand \`${command} ${sub}\` not found, see \`help\``,
  );
}

function requireKey(p: Parsed): string {
  const key = flag(p, "key");
  if (key === undefined) {
    throw new Error("`--key` is required");
  }
  return key;
}

async function exec(line: string): Promise<void> {
  let argv = tokenize(line);
  if (argv.length === 0) {
    return;
  }
  let redirect: string | undefined;
  const i = argv.indexOf(">");
  if (i >= 0) {
    if (i !== argv.length - 2) {
      throw new Error("`>` must be followed by a single path");
    }
    redirect = argv[i + 1];
    argv = argv.slice(0, i);
  }
  let output = "";
  await run(
    argv,
    redirect
      ? (text) => (output += text.endsWith("\n") ? text : `${text}\n`)
      : stdout,
  );
  if (redirect) {
    files.set(redirect, encoder.encode(output));
  }
}

function errorMessage(err: unknown): string {
  if (!(err instanceof Error)) {
    return String(err);
  }
  return err.cause === undefined
    ? err.message
    : `${err.message}: ${errorMessage(err.cause)}`;
}

async function start(url: string | undefined): Promise<void> {
  if (url !== undefined) {
    const next = new Ledger(url, {
      fetch: abortable((input, init) => fetch(input, init)),
    });
    const network = await next.getNetworkName();
    setLedger(next);
    return stderr(`connected to ledger \`${network}\` at ${url}`);
  }
  stderr("Starting a ledger in this tab...");
  await run(["node"], stdout);
}

async function handle(f: () => Promise<void>): Promise<void> {
  controller = new AbortController();
  try {
    await f();
  } catch (err) {
    if (!controller.signal.aborted) {
      stderr(`error: ${errorMessage(err)}`);
    }
  }
  send({ type: "done" });
}

let queue = Promise.resolve();

self.addEventListener("message", ({ data }: { data: LedgerWorkerRequest }) => {
  switch (data.type) {
    case "cancel":
      controller.abort();
      return;
    case "start":
      queue = queue.then(() => handle(() => start(data.url)));
      return;
    case "upload":
      queue = queue.then(() =>
        handle(async () => {
          for (const { path, data: bytes } of data.files) {
            files.set(path, bytes);
            stderr(`added \`${path}\``);
          }
        }),
      );
      return;
    case "exec":
      queue = queue.then(() => handle(() => exec(data.line)));
  }
});
