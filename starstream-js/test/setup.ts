import { execFileSync } from "node:child_process";
import { resolve } from "node:path";

const root = resolve(import.meta.dirname, "../..");

interface Artifact {
  reason: string;
  filenames?: string[];
}

function build(...args: string[]): Artifact[] {
  const stdout = execFileSync(
    "cargo",
    ["build", "--message-format=json-render-diagnostics", ...args],
    {
      cwd: root,
      encoding: "utf8",
      maxBuffer: Infinity,
      stdio: ["ignore", "pipe", "inherit"],
    },
  );
  return stdout
    .split("\n")
    .filter((line) => line)
    .map((line) => JSON.parse(line) as Artifact)
    .filter(({ reason }) => reason === "compiler-artifact");
}

export default function setup(): void {
  if (!process.env.STARSTREAM_COMPILER_WASM) {
    const wasm = build(
      "-p",
      "starstream-compiler-web",
      "--release",
      "--target",
      "wasm32-unknown-unknown",
    )
      .flatMap(({ filenames }) => filenames ?? [])
      .find((file) => file.endsWith("starstream_compiler_web.wasm"));
    process.env.STARSTREAM_COMPILER_WASM = wasm!;
  }
  if (!process.env.STARSTREAM_LEDGER_WASM) {
    const wasm = build(
      "-p",
      "starstream-ledger-web",
      "--release",
      "--target",
      "wasm32-unknown-unknown",
    )
      .flatMap(({ filenames }) => filenames ?? [])
      .find((file) => file.endsWith("starstream_ledger_web.wasm"));
    process.env.STARSTREAM_LEDGER_WASM = wasm!;
  }
  if (!process.env.STARSTREAM_LEDGER_CLIENT_WASM) {
    const wasm = build(
      "-p",
      "starstream-ledger-client-web",
      "--release",
      "--target",
      "wasm32-unknown-unknown",
    )
      .flatMap(({ filenames }) => filenames ?? [])
      .find((file) => file.endsWith("starstream_ledger_client_web.wasm"));
    process.env.STARSTREAM_LEDGER_CLIENT_WASM = wasm!;
  }
}
