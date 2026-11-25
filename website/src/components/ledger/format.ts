import type { ExplorerState, UtxoInfo } from "../../ledger-explorer";

/** What the explorer shows the details of. */
export type Selection =
  | { kind: "block"; height: bigint }
  | { kind: "fund"; height: bigint; index: number }
  | { kind: "contract"; digest: string }
  | { kind: "transaction"; digest: string }
  | { kind: "utxo"; transaction: string; index: number }
  | { kind: "account"; key: string };

export function same(a: Selection | undefined, b: Selection): boolean {
  return (
    a !== undefined &&
    a.kind === b.kind &&
    Object.entries(b).every(
      ([key, value]) => (a as Record<string, unknown>)[key] === value,
    )
  );
}

/** Abbreviate a multibase digest, whose first 4 characters are constant. */
export function digest(d: string): string {
  return d.length > 16 ? `${d.slice(4, 10)}…${d.slice(-4)}` : d;
}

/** Abbreviate a hex-encoded key. */
export function key(k: string): string {
  return k.length > 12 ? `${k.slice(0, 8)}…` : k;
}

export function utxoId({
  transaction,
  index,
}: {
  transaction: string;
  index: number;
}): string {
  return `${transaction}:${index}`;
}

export function utxoLabel(utxo: { transaction: string; index: number }) {
  return utxo.transaction
    ? `${digest(utxo.transaction)}:${utxo.index}`
    : `genesis:${utxo.index}`;
}

export function amount(n: bigint | undefined): string {
  return n === undefined ? "-" : n.toLocaleString("en-US");
}

/** Color of the contract with `digest`, the same on every render. */
export function color(digest: string): string {
  let h = 0;
  for (const c of digest) {
    h = (h * 31 + c.charCodeAt(0)) >>> 0;
  }
  return `hsl(${h % 360} 65% 52%)`;
}

export function liveUtxos(state: ExplorerState): UtxoInfo[] {
  return state.utxos.filter(({ spentBy }) => !spentBy);
}

export interface WitFunc {
  name: string;
  params: string;
  result?: string;
}

const FUNC = String.raw`(%?[a-z][a-z0-9-]*): func\(([^)]*)\)(?:\s*->\s*([^;{]+))?;`;

function funcs(wit: string, pattern: RegExp): WitFunc[] {
  return Array.from(wit.matchAll(pattern), ([, name, params, result]) => ({
    name: name.replace(/^%/, ""),
    params,
    result: result?.trim(),
  }));
}

/** List the functions of a WIT interface, without parsing it fully. */
export function witFuncs(wit: string): WitFunc[] {
  return funcs(wit, new RegExp(`^\\s*${FUNC}`, "gm"));
}

/** List the functions a WIT world exports, the coordination scripts of a contract. */
export function witExports(wit: string): WitFunc[] {
  return funcs(wit, new RegExp(`^\\s*export ${FUNC}`, "gm"));
}
