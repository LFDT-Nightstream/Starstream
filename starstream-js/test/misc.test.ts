import { expect, test, vi } from "vitest";
import * as starstream from "../src";

test("derp", () => {
  expect(starstream.Contract).toBeDefined();
});

test("RpcLedger rejects a base URL with a query or fragment", () => {
  expect(() => new starstream.RpcLedger("http://localhost/?net=x")).toThrow(
    "must not contain a query",
  );
  expect(() => new starstream.RpcLedger("http://localhost/#x")).toThrow(
    "must not contain a query",
  );
});

test("RpcLedger rejects a non-wRPC response", async () => {
  const ledger = new starstream.RpcLedger("http://localhost", {
    fetch: async () =>
      new Response("<html></html>", {
        headers: { "content-type": "text/html" },
      }),
  });
  await expect(ledger.getBlockHeight()).rejects.toThrow(
    "returned content-type `text/html`",
  );
});

test("RpcLedger resolves a relative base URL against the location", async () => {
  const urls: string[] = [];
  vi.stubGlobal("location", new URL("http://localhost/app/index.html"));
  let ledger: starstream.RpcLedger;
  try {
    ledger = new starstream.RpcLedger("ledger/", {
      fetch: async (input) => {
        urls.push(String(input));
        return new Response(null, { status: 503 });
      },
    });
  } finally {
    vi.unstubAllGlobals();
  }
  await expect(ledger.getBlockHeight()).rejects.toThrow("status 503");
  expect(urls).toEqual(["http://localhost/app/ledger/rpc"]);
});
