import { execFile, spawn, type ChildProcess } from "node:child_process";
import { createHash, createPrivateKey, createPublicKey } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { promisify } from "node:util";
import { afterAll, beforeAll, expect, test } from "vitest";
import * as starstream from "../src/index.ts";

const root = resolve(import.meta.dirname, "../..");
const STARSTREAM = process.env.STARSTREAM ?? join(root, "starstream");

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

const run = promisify(execFile);

let dir: string;
let node: ChildProcess;
let url: string;
let ledger: starstream.Ledger;
let wasm: Uint8Array<ArrayBuffer>;
let contract: string;

async function cli(...args: string[]): Promise<string> {
  const { stdout } = await run(STARSTREAM, ["ledger", "--url", url, ...args]);
  return stdout.trim();
}

beforeAll(async () => {
  dir = await mkdtemp(join(tmpdir(), "starstream-js-"));
  const wasmPath = join(dir, "score.wasm");
  await run(STARSTREAM, [
    "wasm",
    "-c",
    join(root, "examples/score.star"),
    "--output-component",
    wasmPath,
  ]);
  wasm = new Uint8Array(await readFile(wasmPath));
  contract = await starstream.digest(wasm);

  const genesisPath = join(dir, "genesis.toml");
  await writeFile(
    genesisPath,
    `[[scripts]]\ncontract = "${contract}"\nscript = "example"\n`,
  );
  await writeFile(join(dir, "admin.key"), ADMIN_SEED.toString("hex"));

  node = spawn(
    STARSTREAM,
    [
      "node",
      "--addr",
      "127.0.0.1:0",
      "--genesis",
      genesisPath,
      "--import",
      wasmPath,
    ],
    {
      env: { ...process.env, NO_COLOR: "1", STARSTREAM_LOG: "info" },
      stdio: ["ignore", "ignore", "pipe"],
    },
  );
  let log = "";
  node.stderr!.setEncoding("utf8");
  const addr = await new Promise<string>((resolve, reject) => {
    node.once("error", reject);
    node.once("close", (code, signal) => {
      reject(new Error(`ledger node exited with ${code ?? signal}:\n${log}`));
    });
    const onData = (chunk: string) => {
      log += chunk;
      const m = /ledger running addr=(\S+)/.exec(log);
      if (m) {
        node.stderr!.off("data", onData);
        resolve(m[1]);
      }
    };
    node.stderr!.on("data", onData);
  });
  url = `http://${addr}`;
  ledger = new starstream.RpcLedger(url);
}, 600_000);

afterAll(async () => {
  node?.kill();
  if (dir) {
    await rm(dir, { recursive: true, force: true });
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
  await expect(ledger.getTransaction("invalid")).rejects.toThrow(
    "invalid digest",
  );
  await expect(ledger.getContractWasm(missing.toUpperCase())).rejects.toThrow(
    "invalid digest",
  );

  const key = ed25519PublicKey(createHash("sha256").update("missing").digest());
  expect(await ledger.getAccountBalance(key)).toBeUndefined();
  expect(await ledger.getAccountNonce(key)).toBeUndefined();
  await expect(ledger.getAccountNonce(key.subarray(1))).rejects.toThrow(
    "invalid key",
  );
});

test("contract and UTXO method calls", async () => {
  expect(await ledger.getBlockHeight()).toBe(0n);
  expect(await ledger.getAdminNonce()).toBe(0n);
  expect(await ledger.getAccountBalance(ADMIN_PUBLIC_KEY)).toBeUndefined();
  await cli(
    "account",
    "fund",
    "--key",
    join(dir, "admin.key"),
    "--nonce",
    "1",
    ADMIN_PUBLIC_KEY.toString("hex"),
    "1000000000",
  );
  expect(await ledger.getAdminNonce()).toBe(1n);
  expect(await ledger.getAccountBalance(ADMIN_PUBLIC_KEY)).toBe(1000000000n);
  expect(await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)).toBe(0n);
  expect(
    await cli(
      "contract",
      "publish",
      "--key",
      join(dir, "admin.key"),
      "--nonce",
      "1",
      join(dir, "score.wasm"),
    ),
  ).toBe(contract);
  expect(await ledger.getAccountNonce(ADMIN_PUBLIC_KEY)).toBe(1n);
  expect(await ledger.getBlockHeight()).toBe(2n);
  expect(await ledger.getBlock(1n)).toHaveLength(1);
  expect(await ledger.getBlock(3n)).toBeUndefined();

  expect(await ledger.getContractWasm(contract)).toEqual(wasm);
  const envelope = await ledger.getContractEnvelope(contract);
  expect(envelope).toBeInstanceOf(Uint8Array);
  expect(await ledger.getBlock(2n)).toEqual([envelope]);

  const genesis = { transaction: "", index: 0 };
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
});
