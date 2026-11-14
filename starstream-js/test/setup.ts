import { execFileSync } from "node:child_process";
import { resolve } from "node:path";

const root = resolve(import.meta.dirname, "../..");

interface Artifact {
  reason: string;
  target?: { name: string };
  executable?: string | null;
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
  if (!process.env.STARSTREAM) {
    const artifact = build("-p", "starstream-cli").find(
      ({ target, executable }) => target?.name === "starstream" && executable,
    );
    process.env.STARSTREAM = artifact!.executable!;
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
