import { createHash, createPrivateKey, createPublicKey } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import { afterAll, beforeAll, expect, test, vi } from "vitest";
import * as starstream from "../src/index.ts";

const root = resolve(import.meta.dirname, "../..");
const STARSTREAM_COMPILER_WASM = process.env.STARSTREAM_COMPILER_WASM!;
const STARSTREAM_LEDGER_WASM = process.env.STARSTREAM_LEDGER_WASM!;
const STARSTREAM_LEDGER_CLIENT_WASM =
  process.env.STARSTREAM_LEDGER_CLIENT_WASM!;

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

let compiler: starstream.WasmCompiler;
let module: WebAssembly.Module;
let node: starstream.WasmLedger;
let ledger: starstream.Ledger;
let wasm: Uint8Array;
let contract: string;

beforeAll(async () => {
  compiler = await starstream.WasmCompiler.instantiate(
    await WebAssembly.compile(await readFile(STARSTREAM_COMPILER_WASM)),
  );
  const source = await readFile(join(root, "examples/score.star"), "utf8");
  ({ component: wasm } = compiler.compile("score.star", {
    "score.star": `${source}\nscript fn update(prog: ScoreProgress) { prog.plus_chips(5); }\n\nstruct Item {\n    amount: i64,\n    price: i64,\n}\n\nscript fn total_value(item: Item) -> i64 {\n    item.amount * item.price\n}\n`,
  }));
  contract = await starstream.digest(wasm);

  module = await WebAssembly.compile(await readFile(STARSTREAM_LEDGER_WASM));
  node = await starstream.WasmLedger.start(module, {
    network: "dev",
    admin: ADMIN_PUBLIC_KEY,
    genesis: { scripts: [{ contract, script: "example" }] },
    imports: [wasm],
  });
  ledger = new starstream.Ledger("http://ledger", { fetch: node.fetch });
}, 600_000);

afterAll(() => {
  node?.dispose();
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
    headers: { "content-type": "application/vnd.bytecodealliance.wrpc" },
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
        "content-type": "application/vnd.bytecodealliance.wrpc",
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

test("compile failure", () => {
  expect(() =>
    compiler.compile("broken.star", { "broken.star": "script fn" }),
  ).toThrow("broken.star");
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

test("cancel an oversized error body", async () => {
  const cancel = vi.fn();
  const client = new starstream.Ledger("http://localhost", {
    fetch: async () =>
      new Response(
        new ReadableStream({
          pull(controller) {
            controller.enqueue(new Uint8Array(1 << 16));
          },
          cancel,
        }),
        { status: 500 },
      ),
  });
  await expect(client.getNetworkName()).rejects.toThrow(
    /^HTTP request failed with status 500$/,
  );
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
  let disposeOnFetch = false;
  const client = await starstream.WasmClient.instantiate(
    await WebAssembly.compile(await readFile(STARSTREAM_LEDGER_CLIENT_WASM)),
    "http://ledger",
    {
      fetch: (input, init) => {
        if (disposeOnFetch) {
          client.dispose();
        }
        return node.fetch(input, init);
      },
    },
  );
  await client.fund(ADMIN_SEED, ADMIN_PUBLIC_KEY, 1000000000n);
  expect(await ledger.getAdminNonce()).toBe(adminNonce + 1n);
  expect(await ledger.getAccountBalance(ADMIN_PUBLIC_KEY)).toBe(
    balance + 1000000000n,
  );
  expect(await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)).toBe(nonce);
  expect(await client.publishContract(ADMIN_SEED, wasm)).toBe(contract);
  await expect(client.publishContract(ADMIN_SEED, wasm)).rejects.toThrow(
    "failed to publish contract",
  );
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
    "get-chips: func() -> u64;",
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

  expect(await client.getCoordinationScriptType(contract, "update")).toEqual({
    params: [starstream.TransactionInput],
    result: undefined,
  });
  expect(
    await client.getCoordinationScriptType(contract, "total-value"),
  ).toEqual({
    params: [
      starstream.t.record([
        { name: "amount", ty: starstream.t.s64 },
        { name: "price", ty: starstream.t.s64 },
      ]),
    ],
    result: starstream.t.s64,
  });
  expect(
    await client.callCoordinationScript(contract, "total-value", [
      { amount: 2n, price: 3n },
    ]),
  ).toMatchObject({ result: 6n });
  const call = await client.callCoordinationScript(contract, "update", [
    genesis,
  ]);
  expect(call.result).toBeUndefined();
  const transaction = await client.submitTransaction(
    ADMIN_SEED,
    call.transaction,
  );
  await expect(
    client.submitTransaction(ADMIN_SEED, call.transaction),
  ).rejects.toThrow("failed to submit transaction");
  const tx = await ledger.getTransaction(transaction);
  expect(tx?.inputs).toEqual([genesis]);
  expect(tx?.nonces).toEqual([
    { key: new Uint8Array(ADMIN_PUBLIC_KEY), nonce: nonce + 2n },
  ]);
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
    "get-chips: func() -> u64;",
  );
  expect(
    await ledger.callUtxoMethod(
      { transaction, index: 0 },
      "get-chips",
      { result: starstream.t.u64 },
      [],
    ),
  ).toBe(47n);
  disposeOnFetch = true;
  await client.fund(ADMIN_SEED, ADMIN_PUBLIC_KEY, 1n);
  await expect(client.publishContract(ADMIN_SEED, wasm)).rejects.toThrow(
    "client disposed",
  );
}, 600_000);
