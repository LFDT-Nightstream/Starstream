import { execFile } from "node:child_process";
import { createHash, createPrivateKey, createPublicKey } from "node:crypto";
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
const STARSTREAM = process.env.STARSTREAM ?? join(root, "starstream");
const STARSTREAM_LEDGER_WASM =
  process.env.STARSTREAM_LEDGER_WASM ??
  join(
    root,
    "target/wasm32-unknown-unknown/release/starstream_ledger_web.wasm",
  );

function ed25519PublicKey(seed: Buffer): Buffer {
  return Buffer.from(
    createPublicKey(
      createPrivateKey({
        key: Buffer.concat([
          Buffer.from("302e020100300506032b657004220420", "hex"),
          seed,
        ]),
        format: "der",
        type: "pkcs8",
      }),
    ).export({ format: "jwk" }).x!,
    "base64url",
  );
}

const ADMIN_SEED = createHash("sha256").update("admin").digest();
const ADMIN_PUBLIC_KEY = ed25519PublicKey(ADMIN_SEED);
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
    genesis: [{ contract, script: "example" }],
    imports: [wasm],
  });
  ledger = new starstream.Ledger("http://ledger", { fetch: node.fetch });

  server = createServer(async (req, res) => {
    const hasBody = req.method !== "GET" && req.method !== "HEAD";
    const response = await node.fetch(`http://ledger${req.url}`, {
      method: req.method,
      headers: req.headers as HeadersInit,
      body: hasBody ? (Readable.toWeb(req) as ReadableStream) : undefined,
      duplex: "half",
    } as RequestInit);
    res.writeHead(response.status, Object.fromEntries(response.headers));
    res.end(Buffer.from(await response.arrayBuffer()));
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const { port } = server.address() as AddressInfo;
  url = `http://127.0.0.1:${port}`;
}, 600_000);

afterAll(async () => {
  try {
    server?.close();
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
  let close!: () => void;
  const stalled = node.fetch("http://ledger/rpc", {
    method: "POST",
    headers: { "content-type": "application/x.wrpc" },
    body: new ReadableStream({
      start(controller) {
        close = () => controller.close();
      },
    }),
    duplex: "half",
  } as RequestInit);
  expect(await ledger.getNetworkName()).toBe("dev");
  close();
  expect((await stalled).status).toBe(400);
});

test("options", async () => {
  const node = await starstream.WasmLedger.start(module, {
    network: "options",
    admin: ADMIN_PUBLIC_KEY,
    maxRequests: 1,
    fuel: 1,
    ioTimeout: 1,
    cardanoBlockHeight: 1,
    cardanoCurrentSlot: 1,
  });
  const ledger = new starstream.Ledger("http://ledger", { fetch: node.fetch });
  expect(await ledger.getNetworkName()).toBe("options");
});

test("genesis failure", async () => {
  await expect(
    starstream.WasmLedger.start(module, {
      network: "dev",
      admin: ADMIN_PUBLIC_KEY,
      genesis: [{ contract, script: "example" }],
    }),
  ).rejects.toThrow(`contract \`${contract}\` not imported`);
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

  const txPath = join(dir, "update.tx");
  expect(
    await cli(
      "contract",
      "script",
      "call",
      "--key",
      join(dir, "admin.key"),
      "--output-transaction",
      txPath,
      contract,
      "update",
      ":0",
    ),
  ).toBe("()");
  const transaction = await starstream.digest(await readFile(txPath));
  const tx = await ledger.getTransaction(transaction);
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
