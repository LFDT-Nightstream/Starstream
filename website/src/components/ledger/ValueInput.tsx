import { useState, type ReactNode } from "react";
import type { Type } from "../../../../starstream-js/src/index";
import type { UtxoInfo } from "../../ledger-explorer";
import type { Func } from "../../ledger.worker";
import { utxoId, utxoLabel } from "./format";
import styles from "./styles.module.css";

/** What a form holds for a value of a type, before converting it. */
export type Draft = unknown;

const INTEGERS: Record<string, [bigint, bigint]> = {
  s8: [-(1n << 7n), (1n << 7n) - 1n],
  u8: [0n, (1n << 8n) - 1n],
  s16: [-(1n << 15n), (1n << 15n) - 1n],
  u16: [0n, (1n << 16n) - 1n],
  s32: [-(1n << 31n), (1n << 31n) - 1n],
  u32: [0n, (1n << 32n) - 1n],
  s64: [-(1n << 63n), (1n << 63n) - 1n],
  u64: [0n, (1n << 64n) - 1n],
};

const WAVE_KEYWORDS = new Set([
  "true",
  "false",
  "some",
  "none",
  "ok",
  "err",
  "inf",
  "nan",
]);

export function typeName(ty: Type): string {
  switch (ty.kind) {
    case "list":
      return `list<${typeName(ty.elem)}>`;
    case "option":
      return `option<${typeName(ty.some)}>`;
    case "result":
      return `result<${ty.ok ? typeName(ty.ok) : "_"}, ${ty.err ? typeName(ty.err) : "_"}>`;
    case "tuple":
      return `tuple<${ty.elems.map(typeName).join(", ")}>`;
    default:
      return ty.kind;
  }
}

function initial(ty: Type): Draft {
  switch (ty.kind) {
    case "bool":
      return false;
    case "list":
      return [];
    case "option":
      return { some: false, value: initial(ty.some) };
    case "result":
      return { ok: true, value: ty.ok ? initial(ty.ok) : undefined };
    case "tuple":
      return ty.elems.map(initial);
    case "record":
      return ty.fields.map(({ ty }) => initial(ty));
    case "variant":
      return { case: 0, value: ty.cases[0]?.ty && initial(ty.cases[0].ty) };
    case "enum":
      return ty.names[0] ?? "";
    case "flags":
      return ty.flags.map(() => false);
    case "char":
    case "string":
      return "";
    default:
      return "0";
  }
}

function toValue(ty: Type, draft: Draft, what: string): unknown {
  const d = draft as never;
  switch (ty.kind) {
    case "bool":
      return d;
    case "s8":
    case "u8":
    case "s16":
    case "u16":
    case "s32":
    case "u32":
    case "s64":
    case "u64": {
      const s = (d as string).trim();
      if (!/^-?\d+$/.test(s)) {
        throw new Error(`${what} is not an integer`);
      }
      const n = BigInt(s);
      const [min, max] = INTEGERS[ty.kind];
      if (n < min || n > max) {
        throw new Error(`${what} is out of the range of \`${ty.kind}\``);
      }
      return ty.kind === "s64" || ty.kind === "u64" ? n : Number(n);
    }
    case "f32":
    case "f64": {
      const n = Number((d as string).trim());
      if ((d as string).trim() === "" || Number.isNaN(n)) {
        throw new Error(`${what} is not a number`);
      }
      return n;
    }
    case "char":
      if ([...(d as string)].length !== 1) {
        throw new Error(`${what} is not a single character`);
      }
      return d;
    case "string":
    case "enum":
      return d;
    case "list":
      return (d as Draft[]).map((item, i) =>
        toValue(ty.elem, item, `${what}[${i}]`),
      );
    case "option": {
      const { some, value } = d as { some: boolean; value: Draft };
      const nested = ty.some.kind === "option";
      if (!some) {
        return nested ? { kind: "none" } : null;
      }
      const v = toValue(ty.some, value, what);
      return nested ? { kind: "some", value: v } : v;
    }
    case "result": {
      const { ok, value } = d as { ok: boolean; value: Draft };
      const payload = ok ? ty.ok : ty.err;
      return {
        kind: ok ? "ok" : "error",
        value: payload ? toValue(payload, value, what) : undefined,
      };
    }
    case "tuple":
      return ty.elems.map((ty, i) =>
        toValue(ty, (d as Draft[])[i], `${what}.${i}`),
      );
    case "record":
      return Object.fromEntries(
        ty.fields.map(({ name, key, ty }, i) => [
          key,
          toValue(ty, (d as Draft[])[i], `${what}.${name}`),
        ]),
      );
    case "variant": {
      const { case: i, value } = d as { case: number; value: Draft };
      const c = ty.cases[i];
      return {
        kind: c.name,
        value: c.ty ? toValue(c.ty, value, what) : undefined,
      };
    }
    case "flags":
      return Object.fromEntries(
        ty.flags.map(({ key }, i) => [key, (d as boolean[])[i]]),
      );
  }
  throw new Error(`${what} is of the unsupported type \`${ty.kind}\``);
}

function waveString(s: string, quote: string): string {
  let out = quote;
  for (const c of s) {
    const code = c.codePointAt(0)!;
    if (c === "\\" || c === quote) {
      out += `\\${c}`;
    } else if (c === "\n") {
      out += "\\n";
    } else if (c === "\r") {
      out += "\\r";
    } else if (c === "\t") {
      out += "\\t";
    } else if (code < 0x20 || code === 0x7f) {
      out += `\\u{${code.toString(16)}}`;
    } else {
      out += c;
    }
  }
  return out + quote;
}

function waveCase(name: string): string {
  return WAVE_KEYWORDS.has(name) ? `%${name}` : name;
}

/** Encode `v`, a value of `ty` as returned by `toArgs`, in WAVE. */
export function toWave(ty: Type, v: unknown): string {
  const d = v as never;
  switch (ty.kind) {
    case "bool":
    case "s8":
    case "u8":
    case "s16":
    case "u16":
    case "s32":
    case "u32":
    case "s64":
    case "u64":
      return String(d);
    case "f32":
    case "f64": {
      const n = d as number;
      if (Number.isNaN(n)) {
        return "nan";
      }
      return n === Infinity ? "inf" : n === -Infinity ? "-inf" : String(n);
    }
    case "char":
      return waveString(d, "'");
    case "string":
      return waveString(d, '"');
    case "enum":
      return waveCase(d);
    case "list":
      return `[${(d as unknown[]).map((v) => toWave(ty.elem, v)).join(", ")}]`;
    case "tuple":
      return `(${ty.elems.map((ty, i) => toWave(ty, (d as unknown[])[i])).join(", ")})`;
    case "option": {
      if (ty.some.kind === "option") {
        const { kind, value } = d as { kind: string; value?: unknown };
        return kind === "none" ? "none" : `some(${toWave(ty.some, value)})`;
      }
      return d === null ? "none" : `some(${toWave(ty.some, d)})`;
    }
    case "result": {
      const { kind, value } = d as { kind: string; value?: unknown };
      const payload = kind === "ok" ? ty.ok : ty.err;
      const name = kind === "ok" ? "ok" : "err";
      return payload ? `${name}(${toWave(payload, value)})` : name;
    }
    case "record":
      if (ty.fields.length === 0) {
        return "{:}";
      }
      return `{${ty.fields
        .map(
          ({ name, key, ty }) =>
            `${name}: ${toWave(ty, (d as Record<string, unknown>)[key])}`,
        )
        .join(", ")}}`;
    case "variant": {
      const { kind, value } = d as { kind: string; value?: unknown };
      const payload = ty.cases.find(({ name }) => name === kind)?.ty;
      const name = waveCase(kind);
      return payload ? `${name}(${toWave(payload, value)})` : name;
    }
    case "flags":
      return `{${ty.flags
        .filter(({ key }) => (d as Record<string, boolean>)[key])
        .map(({ name }) => name)
        .join(", ")}}`;
  }
  throw new Error(`values of type \`${ty.kind}\` have no WAVE encoding`);
}

function utxoValue(draft: Draft, what: string) {
  const id = draft as string;
  const i = id.lastIndexOf(":");
  if (i < 0) {
    throw new Error(`select a UTXO for ${what}`);
  }
  return { transaction: id.slice(0, i), index: Number(id.slice(i + 1)) };
}

function Input({
  ty,
  draft,
  onChange,
}: {
  ty: Type;
  draft: Draft;
  onChange: (draft: Draft) => void;
}) {
  const d = draft as never;
  switch (ty.kind) {
    case "bool":
      return (
        <input
          type="checkbox"
          checked={d}
          onChange={(e) => onChange(e.target.checked)}
        />
      );
    case "s8":
    case "u8":
    case "s16":
    case "u16":
    case "s32":
    case "u32":
    case "s64":
    case "u64":
    case "f32":
    case "f64":
      return (
        <input
          inputMode={ty.kind.startsWith("f") ? "decimal" : "numeric"}
          value={d}
          onChange={(e) => onChange(e.target.value)}
        />
      );
    case "char":
    case "string":
      return <input value={d} onChange={(e) => onChange(e.target.value)} />;
    case "enum":
      return (
        <select value={d} onChange={(e) => onChange(e.target.value)}>
          {ty.names.map((name) => (
            <option key={name}>{name}</option>
          ))}
        </select>
      );
    case "flags":
      return (
        <div className={styles.nested}>
          {ty.flags.map(({ name }, i) => (
            <label key={name}>
              <input
                type="checkbox"
                checked={(d as boolean[])[i]}
                onChange={(e) =>
                  onChange(
                    (d as boolean[]).map((v, j) =>
                      i === j ? e.target.checked : v,
                    ),
                  )
                }
              />{" "}
              {name}
            </label>
          ))}
        </div>
      );
    case "list": {
      const items = d as Draft[];
      return (
        <div className={styles.nested}>
          {items.map((item, i) => (
            <div key={i} className={styles.item}>
              <Input
                ty={ty.elem}
                draft={item}
                onChange={(v) =>
                  onChange(items.map((old, j) => (i === j ? v : old)))
                }
              />
              <button
                type="button"
                className="button button--sm button--secondary"
                onClick={() => onChange(items.filter((_, j) => i !== j))}
              >
                Remove
              </button>
            </div>
          ))}
          <button
            type="button"
            className="button button--sm button--secondary"
            onClick={() => onChange([...items, initial(ty.elem)])}
          >
            Add {typeName(ty.elem)}
          </button>
        </div>
      );
    }
    case "option": {
      const { some, value } = d as { some: boolean; value: Draft };
      return (
        <div className={styles.nested}>
          <label>
            <input
              type="checkbox"
              checked={some}
              onChange={(e) => onChange({ some: e.target.checked, value })}
            />{" "}
            some
          </label>
          {some ? (
            <Input
              ty={ty.some}
              draft={value}
              onChange={(value) => onChange({ some, value })}
            />
          ) : null}
        </div>
      );
    }
    case "result": {
      const { ok, value } = d as { ok: boolean; value: Draft };
      const payload = ok ? ty.ok : ty.err;
      return (
        <div className={styles.nested}>
          <select
            value={ok ? "ok" : "error"}
            onChange={(e) => {
              const ok = e.target.value === "ok";
              const payload = ok ? ty.ok : ty.err;
              onChange({ ok, value: payload ? initial(payload) : undefined });
            }}
          >
            <option>ok</option>
            <option>error</option>
          </select>
          {payload ? (
            <Input
              ty={payload}
              draft={value}
              onChange={(value) => onChange({ ok, value })}
            />
          ) : null}
        </div>
      );
    }
    case "tuple":
    case "record": {
      const items = d as Draft[];
      const fields =
        ty.kind === "tuple"
          ? ty.elems.map((ty, i) => ({ name: String(i), ty }))
          : ty.fields;
      return (
        <div className={styles.nested}>
          {fields.map(({ name, ty }, i) => (
            <Field key={name} name={name} ty={ty}>
              <Input
                ty={ty}
                draft={items[i]}
                onChange={(v) =>
                  onChange(items.map((old, j) => (i === j ? v : old)))
                }
              />
            </Field>
          ))}
        </div>
      );
    }
    case "variant": {
      const { case: i, value } = d as { case: number; value: Draft };
      const payload = ty.cases[i]?.ty;
      return (
        <div className={styles.nested}>
          <select
            value={i}
            onChange={(e) => {
              const i = Number(e.target.value);
              const payload = ty.cases[i].ty;
              onChange({ case: i, value: payload && initial(payload) });
            }}
          >
            {ty.cases.map(({ name }, i) => (
              <option key={name} value={i}>
                {name}
              </option>
            ))}
          </select>
          {payload ? (
            <Input
              ty={payload}
              draft={value}
              onChange={(value) => onChange({ case: i, value })}
            />
          ) : null}
        </div>
      );
    }
  }
  return <span className={styles.hint}>unsupported type</span>;
}

function Field({
  name,
  ty,
  children,
}: {
  name: string;
  ty: Type | "utxo";
  children: ReactNode;
}) {
  return (
    <div className={styles.param}>
      <span>
        {name}{" "}
        <span className={styles.hint}>
          {ty === "utxo" ? "UTXO" : typeName(ty)}
        </span>
      </span>
      {children}
    </div>
  );
}

export function signature({ name, params, result }: Func): string {
  const list = params
    .map(({ name, ty, utxo }) => `${name}: ${utxo ? "utxo" : typeName(ty)}`)
    .join(", ");
  return `${name}(${list})${result ? ` -> ${typeName(result)}` : ""}`;
}

/** Render `v`, a decoded wRPC value, roughly like WAVE. */
export function formatValue(v: unknown): string {
  if (v === undefined || v === null) {
    return "none";
  }
  if (typeof v === "string") {
    return JSON.stringify(v);
  }
  if (typeof v !== "object") {
    return String(v);
  }
  if (ArrayBuffer.isView(v)) {
    return `[${Array.from(v as unknown as ArrayLike<number>).join(", ")}]`;
  }
  if (Array.isArray(v)) {
    return `[${v.map(formatValue).join(", ")}]`;
  }
  if (v instanceof Map) {
    return `{${[...v].map(([k, v]) => `${formatValue(k)}: ${formatValue(v)}`).join(", ")}}`;
  }
  const o = v as Record<string, unknown>;
  if (
    typeof o.kind === "string" &&
    Object.keys(o).every((k) => k === "kind" || k === "value")
  ) {
    return "value" in o ? `${o.kind}(${formatValue(o.value)})` : o.kind;
  }
  return `{${Object.entries(o)
    .map(([k, v]) => `${k}: ${formatValue(v)}`)
    .join(", ")}}`;
}

/** Drafts of the arguments of `func` a form starts with. */
export function initialDrafts(func: Func): Draft[] {
  return func.params.map(({ ty, utxo }) => (utxo ? "" : initial(ty)));
}

/** Convert `drafts` to arguments of `func`, failing if one is not valid. */
export function toArgs(func: Func, drafts: Draft[]): unknown[] {
  return func.params.map(({ name, ty, utxo }, i) =>
    utxo
      ? utxoValue(drafts[i], `\`${name}\``)
      : toValue(ty, drafts[i], `\`${name}\``),
  );
}

/** Fields of the parameters of `func`. */
export function ParamFields({
  func,
  utxos,
  drafts,
  onChange,
}: {
  func: Func;
  /** UTXOs to choose UTXO arguments from. */
  utxos: UtxoInfo[];
  drafts: Draft[];
  onChange: (drafts: Draft[]) => void;
}) {
  return func.params.map(({ name, ty, utxo }, i) => {
    const set = (v: Draft) =>
      onChange(drafts.map((old, j) => (i === j ? v : old)));
    return (
      <Field key={name} name={name} ty={utxo ? "utxo" : ty}>
        {utxo ? (
          <select
            value={drafts[i] as string}
            onChange={(e) => set(e.target.value)}
          >
            <option value="">select a live UTXO</option>
            {utxos.map((u) => (
              <option key={utxoId(u)} value={utxoId(u)}>
                {utxoLabel(u)} {u.instance}
              </option>
            ))}
          </select>
        ) : (
          <Input ty={ty} draft={drafts[i]} onChange={set} />
        )}
      </Field>
    );
  });
}

/** Form calling `func` with arguments of its parameter types. */
export function CallForm({
  func,
  utxos,
  label = "Call",
  run,
}: {
  func: Func;
  /** UTXOs to choose UTXO arguments from. */
  utxos: UtxoInfo[];
  label?: string;
  run: (args: unknown[]) => Promise<string>;
}) {
  const [drafts, setDrafts] = useState<Draft[]>(() => initialDrafts(func));
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState<{ ok: boolean; text: string }>();
  return (
    <form
      className={styles.invoke}
      onSubmit={(e) => {
        e.preventDefault();
        setResult(undefined);
        let args: unknown[];
        try {
          args = toArgs(func, drafts);
        } catch (err) {
          setResult({ ok: false, text: (err as Error).message });
          return;
        }
        setBusy(true);
        void run(args)
          .then(
            (text) => setResult({ ok: true, text }),
            (err: unknown) =>
              setResult({
                ok: false,
                text: err instanceof Error ? err.message : String(err),
              }),
          )
          .finally(() => setBusy(false));
      }}
    >
      <ParamFields
        func={func}
        utxos={utxos}
        drafts={drafts}
        onChange={setDrafts}
      />
      <button className="button button--sm button--primary" disabled={busy}>
        {busy ? "Running…" : label}
      </button>
      {result ? (
        <pre className={result.ok ? styles.result : styles.error}>
          {result.text}
        </pre>
      ) : null}
    </form>
  );
}
