import { expect, test } from "vitest";
import { digest } from "../src/index.ts";

test("digest of a view on a `SharedArrayBuffer`", async () => {
  const want = await digest(new Uint8Array(8));
  const shared = new SharedArrayBuffer(8);
  expect(await digest(new Uint8Array(shared))).toBe(want);
  expect(await digest(Buffer.from(shared))).toBe(want);
});
