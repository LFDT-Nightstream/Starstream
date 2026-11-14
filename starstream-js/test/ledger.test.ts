import { execFile } from "node:child_process";
import {
  createHash,
  createPrivateKey,
  createPublicKey,
  sign,
  type KeyObject,
} from "node:crypto";
import { once } from "node:events";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import { type AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { Readable } from "node:stream";
import { join, resolve } from "node:path";
import { promisify } from "node:util";
import { afterAll, beforeAll, expect, test, vi } from "vitest";
import * as starstream from "../src/index.ts";

const root = resolve(import.meta.dirname, "../..");
const STARSTREAM = process.env.STARSTREAM!;
const STARSTREAM_LEDGER_WASM = process.env.STARSTREAM_LEDGER_WASM!;
const STARSTREAM_LEDGER_CLIENT_WASM =
  process.env.STARSTREAM_LEDGER_CLIENT_WASM!;

function ed25519PrivateKey(seed: Buffer): KeyObject {
  return createPrivateKey({
    key: Buffer.concat([
      Buffer.from("302e020100300506032b657004220420", "hex"),
      seed,
    ]),
    format: "der",
    type: "pkcs8",
  });
}

function ed25519PublicKey(seed: Buffer): Buffer {
  return Buffer.from(
    createPublicKey(ed25519PrivateKey(seed)).export({ format: "jwk" }).x!,
    "base64url",
  );
}

const ADMIN_SEED = createHash("sha256").update("admin").digest();
const ADMIN_PUBLIC_KEY = ed25519PublicKey(ADMIN_SEED);
const ADMIN: starstream.Signer = {
  publicKey: ADMIN_PUBLIC_KEY,
  sign: async (data) => sign(null, data, ed25519PrivateKey(ADMIN_SEED)),
};
const WASM_MAGIC = Uint8Array.of(0x00, 0x61, 0x73, 0x6d);

const run = promisify(execFile);

let dir: string;
let module: WebAssembly.Module;
let node: starstream.WasmLedger;
let server: Server;
let url: string;
let ledger: starstream.Ledger;
let wasm: Uint8Array;
let contract: string;

async function cli(...args: string[]): Promise<string> {
  const { stdout } = await run(STARSTREAM, ["ledger", "--url", url, ...args]);
  return stdout.trim();
}

beforeAll(async () => {
  dir = await mkdtemp(join(tmpdir(), "starstream-js-"));
  const sourcePath = join(dir, "score.star");
  await writeFile(
    sourcePath,
    `${await readFile(join(root, "examples/score.star"), "utf8")}\nscript fn update(prog: ScoreProgress) { prog.plus_chips(5); }\n`,
  );
  const wasmPath = join(dir, "score.wasm");
  await run(STARSTREAM, [
    "wasm",
    "-c",
    sourcePath,
    "--output-component",
    wasmPath,
  ]);
  wasm = new Uint8Array(await readFile(wasmPath));
  contract = await starstream.digest(wasm);

  await writeFile(join(dir, "admin.key"), ADMIN_SEED.toString("hex"));

  module = await WebAssembly.compile(await readFile(STARSTREAM_LEDGER_WASM));
  node = await starstream.WasmLedger.start(module, {
    network: "dev",
    admin: ADMIN_PUBLIC_KEY,
    genesis: { scripts: [{ contract, script: "example" }] },
    imports: [wasm],
  });
  ledger = new starstream.Ledger("http://ledger", { fetch: node.fetch });

  server = createServer(async (req, res) => {
    const hasBody = req.method !== "GET" && req.method !== "HEAD";
    try {
      const response = await node.fetch(`http://ledger${req.url}`, {
        method: req.method,
        headers: req.headers as HeadersInit,
        body: hasBody ? (Readable.toWeb(req) as ReadableStream) : undefined,
        duplex: "half",
      } as RequestInit);
      const body = Buffer.from(await response.arrayBuffer());
      res.writeHead(response.status, Object.fromEntries(response.headers));
      res.end(body);
    } catch (err) {
      res.writeHead(500);
      res.end(String(err));
    }
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const { port } = server.address() as AddressInfo;
  url = `http://127.0.0.1:${port}`;
}, 600_000);

afterAll(async () => {
  try {
    server?.close();
    node?.dispose();
  } finally {
    if (dir) {
      await rm(dir, { recursive: true, force: true });
    }
  }
});

test("genesis", async () => {
  expect(await ledger.getNetworkName()).toBe("dev");
  expect(await ledger.getBlock(0n)).toBeUndefined();

  const outputs = await ledger.getGenesisOutputs();
  expect(outputs).toHaveLength(1);
  const [output] = outputs;
  expect(output.contract).toBe(contract);
  expect(output.instance).toBe("score-progress");
  expect(output.methods).toHaveLength(6);
  expect(output.state).not.toHaveLength(0);

  expect(await ledger.getOutput({ transaction: "", index: 0 })).toEqual(output);
  expect(await ledger.getOutput({ transaction: "", index: 1 })).toBeUndefined();

  expect(await ledger.getWit()).toContain("get-name: func() -> string;");
  const witWasm = await ledger.getWitWasm();
  expect(witWasm.subarray(0, 4)).toEqual(WASM_MAGIC);
});

test("concurrent requests", async () => {
  const controller = new AbortController();
  const stalled = node.fetch("http://ledger/rpc", {
    method: "POST",
    headers: { "content-type": "application/x.wrpc" },
    body: new ReadableStream(),
    duplex: "half",
    signal: controller.signal,
  } as RequestInit);
  expect(await ledger.getNetworkName()).toBe("dev");
  controller.abort();
  await expect(stalled).rejects.toMatchObject({ name: "AbortError" });
});

test("request body", async () => {
  let request: Request | undefined;
  const client = new starstream.Ledger("http://ledger", {
    fetch: async (input, init) => {
      request = new Request(input, init);
      return node.fetch(request.clone());
    },
  });
  expect(await client.getNetworkName()).toBe("dev");
  const body = new Uint8Array(await request!.arrayBuffer());
  const post = (chunks: unknown[], length?: number, error?: Error) =>
    node.fetch("http://ledger/rpc", {
      method: "POST",
      headers: {
        "content-type": "application/x.wrpc",
        ...(length === undefined ? {} : { "content-length": String(length) }),
      },
      body: new ReadableStream({
        start(controller) {
          for (const chunk of chunks) {
            controller.enqueue(chunk);
          }
          if (error) {
            controller.error(error);
          } else {
            controller.close();
          }
        },
      }),
      duplex: "half",
    } as RequestInit);

  const exact = await post([body], body.length);
  expect(exact.status).toBe(200);
  const excess = await post([body, Uint8Array.of(0)], body.length);
  expect(excess.status).toBe(400);
  const large = await node.fetch("http://ledger/rpc", {
    method: "POST",
    headers: { "content-type": "application/cose" },
    body: new Uint8Array((1 << 24) + 1),
  });
  expect(large.status).toBe(413);
  await expect(post(["text"])).rejects.toThrow(TypeError);
  await expect(post([], undefined, new Error("body failed"))).rejects.toThrow(
    "body failed",
  );
});

test("options", async () => {
  const node = await starstream.WasmLedger.start(module, {
    network: "options",
    admin: ADMIN_PUBLIC_KEY,
    maxRequests: 1,
    fuel: 1n,
    ioTimeout: 1,
    cardanoBlockHeight: 1n,
    cardanoCurrentSlot: -1n,
  });
  const ledger = new starstream.Ledger("http://ledger", { fetch: node.fetch });
  expect(await ledger.getNetworkName()).toBe("options");
  node.dispose();
  await expect(ledger.getNetworkName()).rejects.toThrow("ledger disposed");
});

test("genesis failure", async () => {
  await expect(
    starstream.WasmLedger.start(module, {
      network: "dev",
      admin: ADMIN_PUBLIC_KEY,
      genesis: { scripts: [{ contract, script: "example" }] },
    }),
  ).rejects.toThrow(`contract \`${contract}\` not imported`);
  await expect(
    starstream.WasmLedger.start(module, {
      network: "dev",
      admin: ADMIN_PUBLIC_KEY,
      genesis: {
        scripts: [
          { contract, script: "example", arg: [] } as starstream.GenesisScript,
        ],
      },
    }),
  ).rejects.toThrow("unknown field `arg`");
});

test("base URL", () => {
  expect(() => new starstream.Ledger("http://localhost/?token=abc")).toThrow(
    "must not contain a query or fragment",
  );
  expect(() => new starstream.Ledger("http://localhost/#rpc")).toThrow(
    "must not contain a query or fragment",
  );
});

test("cancel the body of a missing UTXO WIT", async () => {
  const cancel = vi.fn();
  const client = new starstream.Ledger("http://localhost", {
    fetch: async () =>
      new Response(new ReadableStream({ cancel }), { status: 404 }),
  });
  expect(
    await client.getUtxoWit({ transaction: "", index: 0 }),
  ).toBeUndefined();
  expect(cancel).toHaveBeenCalledOnce();
});

test("missing", async () => {
  const missing = await starstream.digest(new Uint8Array());
  expect(await ledger.getContractWasm(missing)).toBeUndefined();
  expect(await ledger.getContractEnvelope(missing)).toBeUndefined();
  expect(await ledger.getTransaction(missing)).toBeUndefined();
  expect(await ledger.getTransactionEnvelope(missing)).toBeUndefined();
  expect(
    await ledger.getOutput({ transaction: missing, index: 0 }),
  ).toBeUndefined();
  expect(
    await ledger.getUtxoWit({ transaction: missing, index: 0 }),
  ).toBeUndefined();
  expect(
    await ledger.getUtxoWitWasm({ transaction: missing, index: 0 }),
  ).toBeUndefined();
  await expect(ledger.getTransaction("invalid")).rejects.toThrow(
    "invalid digest",
  );
  await expect(
    ledger.getContractWasm(`B${contract.slice(1).toUpperCase()}`),
  ).rejects.toThrow("is not canonical");

  const key = ed25519PublicKey(createHash("sha256").update("missing").digest());
  expect(await ledger.getAccountBalance(key)).toBeUndefined();
  expect(await ledger.getAccountNonce(key)).toBeUndefined();
  await expect(ledger.getAccountNonce(key.subarray(1))).rejects.toThrow(
    "invalid key",
  );
});

test("contract and UTXO method calls", async () => {
  const height = await ledger.getBlockHeight();
  const adminNonce = await ledger.getAdminNonce();
  const balance = (await ledger.getAccountBalance(ADMIN_PUBLIC_KEY)) ?? 0n;
  const nonce = (await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)) ?? 0n;
  await cli(
    "account",
    "fund",
    "--key",
    join(dir, "admin.key"),
    "--nonce",
    String(adminNonce + 1n),
    ADMIN_PUBLIC_KEY.toString("hex"),
    "1000000000",
  );
  expect(await ledger.getAdminNonce()).toBe(adminNonce + 1n);
  expect(await ledger.getAccountBalance(ADMIN_PUBLIC_KEY)).toBe(
    balance + 1000000000n,
  );
  expect(await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)).toBe(nonce);
  expect(
    await cli(
      "contract",
      "publish",
      "--key",
      join(dir, "admin.key"),
      "--nonce",
      String(nonce + 1n),
      join(dir, "score.wasm"),
    ),
  ).toBe(contract);
  expect(await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)).toBe(nonce + 1n);
  expect(await ledger.getBlockHeight()).toBe(height + 2n);
  expect(await ledger.getBlock(height + 1n)).toHaveLength(1);
  expect(await ledger.getBlock(height + 3n)).toBeUndefined();

  expect(await ledger.getContractWasm(contract)).toEqual(wasm);
  const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
  vi.stubGlobal("crypto", {});
  try {
    expect(await ledger.getContractWasm(contract)).toEqual(wasm);
    expect(warn).toHaveBeenCalledExactlyOnceWith(
      `\`crypto.subtle\` is unavailable, skipping validation of contract \`${contract}\``,
    );
  } finally {
    vi.unstubAllGlobals();
    warn.mockRestore();
  }
  const envelope = await ledger.getContractEnvelope(contract);
  expect(envelope).toBeInstanceOf(Uint8Array);
  expect(await ledger.getBlock(height + 2n)).toEqual([envelope]);

  const genesis = { transaction: "", index: 0 };
  expect(await ledger.getUtxoWit(genesis)).toContain(
    "get-chips: func(transaction: option<string>, index: u32) -> u64;",
  );
  const utxoWitWasm = await ledger.getUtxoWitWasm(genesis);
  expect(utxoWitWasm?.subarray(0, 4)).toEqual(WASM_MAGIC);
  expect(
    await ledger.callUtxoMethod(
      genesis,
      "get-chips",
      { result: starstream.t.u64 },
      [],
    ),
  ).toBe(42n);
  expect(
    await ledger.callUtxoMethod(
      genesis,
      "get-mult",
      { result: starstream.t.u64 },
      [],
    ),
  ).toBe(4n);
  expect(
    await ledger.callUtxoMethod(
      genesis,
      "plus-chips",
      { params: [starstream.t.u64] },
      [1n],
    ),
  ).toBeUndefined();
  expect(
    await ledger.callUtxoMethod(
      genesis,
      "get-chips",
      { result: starstream.t.u64 },
      [],
    ),
  ).toBe(42n);
  const methods = ["get-chips", "get-mult"];
  expect(
    await Promise.all(
      Array.from({ length: 32 }, (_, i) =>
        ledger.callUtxoMethod(
          genesis,
          methods[i % 2],
          { result: starstream.t.u64 },
          [],
        ),
      ),
    ),
  ).toEqual(Array.from({ length: 32 }, (_, i) => (i % 2 ? 4n : 42n)));
  await expect(
    ledger.callUtxoMethod(
      genesis,
      "plus-chips",
      { params: [starstream.t.u64] },
      [],
    ),
  ).rejects.toThrow("expected 1 argument(s), got 0");
  await expect(
    ledger.callUtxoMethod(genesis, "missing", {}, []),
  ).rejects.toThrow("status 404");

  const client = await starstream.WasmClient.instantiate(
    await WebAssembly.compile(await readFile(STARSTREAM_LEDGER_CLIENT_WASM)),
    ledger,
  );
  const call = await client.callCoordinationScript(
    contract,
    "update",
    { params: [starstream.TransactionInput] },
    [genesis],
  );
  expect(call.result).toBeUndefined();
  const transaction = await ledger.submitTransaction(ADMIN, call.transaction);
  await expect(
    ledger.submitTransaction(ADMIN, call.transaction),
  ).rejects.toThrow("failed to submit transaction");
  const tx = await ledger.getTransaction(transaction);
  expect(tx).toEqual(call.transaction);
  expect(tx?.inputs).toEqual([genesis]);
  expect(tx?.outputs).toHaveLength(1);
  expect(tx?.outputs[0].contract).toBe(contract);
  expect(tx?.outputs[0].instance).toBe("score-progress");
  const txEnvelope = await ledger.getTransactionEnvelope(transaction);
  expect(txEnvelope).toBeInstanceOf(Uint8Array);
  expect(await ledger.getBlockHeight()).toBe(height + 3n);
  expect(await ledger.getBlock(height + 3n)).toEqual([txEnvelope]);
  expect(await ledger.getOutput({ transaction, index: 0 })).toEqual(
    tx?.outputs[0],
  );
  expect(await ledger.getUtxoWit({ transaction, index: 0 })).toContain(
    "get-chips: func(transaction: option<string>, index: u32) -> u64;",
  );
  expect(
    await ledger.callUtxoMethod(
      { transaction, index: 0 },
      "get-chips",
      { result: starstream.t.u64 },
      [],
    ),
  ).toBe(47n);
}, 600_000);
