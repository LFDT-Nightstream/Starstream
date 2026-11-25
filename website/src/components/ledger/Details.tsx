import { useEffect, useState, type ReactNode } from "react";
import type { ExplorerState, UtxoInfo } from "../../ledger-explorer";
import type { LedgerCall, Published } from "../../ledger.worker";
import {
  amount,
  color,
  digest,
  key,
  utxoLabel,
  witExports,
  witFuncs,
  type Selection,
} from "./format";
import styles from "./styles.module.css";

export type Call = (call: LedgerCall) => Promise<unknown>;

interface Context {
  state: ExplorerState;
  wallet: string | undefined;
  published: Record<string, Published>;
  /** Paths of the files of the terminal. */
  files: string[];
  call: Call;
  /** Run `call`, reporting its failure. */
  perform: (call: LedgerCall) => void;
  /** Set up a new ledger running `script` of the component at `path` at genesis. */
  onGenesis: (path: string, script: string) => void;
  onSelect: (selection: Selection) => void;
}

function Link({
  to,
  onSelect,
  children,
}: {
  to: Selection;
  onSelect: (selection: Selection) => void;
  children: ReactNode;
}) {
  return (
    <button className={styles.link} onClick={() => onSelect(to)}>
      {children}
    </button>
  );
}

export function Swatch({ contract }: { contract: string }) {
  return (
    <span className={styles.swatch} style={{ background: color(contract) }} />
  );
}

function Field({ label, children }: { label: string; children: ReactNode }) {
  return (
    <>
      <dt>{label}</dt>
      <dd>{children}</dd>
    </>
  );
}

function Copy({ value }: { value: string }) {
  return (
    <span className={styles.copy}>
      <code>{value}</code>
      <button
        className="button button--sm button--secondary"
        onClick={() => void navigator.clipboard.writeText(value)}
      >
        Copy
      </button>
    </span>
  );
}

function UtxoLink({ utxo, ctx }: { utxo: UtxoInfo; ctx: Context }) {
  return (
    <Link
      to={{ kind: "utxo", transaction: utxo.transaction, index: utxo.index }}
      onSelect={ctx.onSelect}
    >
      <Swatch contract={utxo.contract} />
      {utxoLabel(utxo)} {utxo.instance}
      {utxo.spentBy ? " (spent)" : ""}
    </Link>
  );
}

/** Form running a call that takes arguments as typed in the terminal. */
function Invoke({
  label,
  placeholder,
  disabled,
  run,
}: {
  label: string;
  placeholder?: string;
  disabled?: boolean;
  run: (args: string) => Promise<unknown>;
}) {
  const [args, setArgs] = useState("");
  const [busy, setBusy] = useState(false);
  const [result, setResult] = useState<{ ok: boolean; text: string }>();
  return (
    <form
      className={styles.invoke}
      onSubmit={(e) => {
        e.preventDefault();
        setBusy(true);
        setResult(undefined);
        void run(args)
          .then(
            (out) => setResult({ ok: true, text: String(out ?? "").trim() }),
            (err: unknown) =>
              setResult({
                ok: false,
                text: err instanceof Error ? err.message : String(err),
              }),
          )
          .finally(() => setBusy(false));
      }}
    >
      <input
        value={args}
        placeholder={placeholder ?? 'WAVE arguments, e.g. 1 "text"'}
        onChange={(e) => setArgs(e.target.value)}
      />
      <button
        className="button button--sm button--primary"
        disabled={disabled || busy}
      >
        {label}
      </button>
      {result ? (
        <pre className={result.ok ? styles.result : styles.error}>
          {result.text || "()"}
        </pre>
      ) : null}
    </form>
  );
}

function ContractDetails({ d, ctx }: { d: string; ctx: Context }) {
  const contract = ctx.state.contracts.find(({ digest }) => digest === d);
  const [script, setScript] = useState("");
  const [signer, setSigner] = useState("user.key");
  if (!contract) {
    return <p>Contract not found.</p>;
  }
  const utxos = ctx.state.utxos.filter(({ contract }) => contract === d);
  const published = ctx.published[d];
  const wit = published?.wit;
  const scripts = wit ? witExports(wit) : [];
  const params = scripts.find(({ name }) => name === script)?.params;
  return (
    <>
      <h3>
        <Swatch contract={d} />
        Contract
      </h3>
      <dl className={styles.fields}>
        <Field label="Digest">
          <Copy value={d} />
        </Field>
        <Field label="Published">
          {contract.height === undefined ? (
            "not published on this ledger"
          ) : (
            <Link
              to={{ kind: "block", height: contract.height }}
              onSelect={ctx.onSelect}
            >
              block {String(contract.height)}
            </Link>
          )}
        </Field>
        {contract.publisher ? (
          <Field label="Publisher">
            <Link
              to={{ kind: "account", key: contract.publisher }}
              onSelect={ctx.onSelect}
            >
              {key(contract.publisher)}
            </Link>
          </Field>
        ) : null}
        {contract.size !== undefined ? (
          <Field label="Size">{amount(BigInt(contract.size))} bytes</Field>
        ) : null}
      </dl>
      {contract.height !== undefined ? (
        <button
          className="button button--sm button--secondary margin-bottom--md"
          onClick={() => ctx.perform({ op: "download", contract: d })}
        >
          Download component
        </button>
      ) : null}

      <h4>Call a coordination script</h4>
      <p className={styles.hint}>
        Runs locally and submits the transaction. UTXO arguments are{" "}
        <code>TRANSACTION:INDEX</code>, with an empty <code>TRANSACTION</code>{" "}
        for genesis outputs, which only <code>admin.key</code> may spend.
      </p>
      <label className={styles.inline}>
        <span>Sign with</span>
        <select value={signer} onChange={(e) => setSigner(e.target.value)}>
          {[...new Set(["user.key", ...ctx.files])]
            .filter((path) => path.endsWith(".key"))
            .map((path) => (
              <option key={path}>{path}</option>
            ))}
        </select>
      </label>
      <label className={styles.inline}>
        <span>Script</span>
        <input
          list={`scripts-${d}`}
          value={script}
          placeholder="name"
          onChange={(e) => setScript(e.target.value)}
        />
        <datalist id={`scripts-${d}`}>
          {scripts.map(({ name }) => (
            <option key={name} value={name} />
          ))}
        </datalist>
      </label>
      <Invoke
        label="Call"
        disabled={!script}
        placeholder={
          params === undefined ? undefined : params || "no arguments"
        }
        run={(args) =>
          ctx.call({ op: "script", contract: d, script, args, key: signer })
        }
      />
      {published ? (
        <p className={styles.hint}>
          A transaction needs an input, so scripts creating the first UTXOs of a
          contract run at genesis.{" "}
          <button
            className={styles.link}
            onClick={() => ctx.onGenesis(published.path, script)}
          >
            Start a new ledger running{" "}
            {script ? <code>{script}</code> : "a script"} at genesis
          </button>
        </p>
      ) : null}

      <h4>UTXOs ({utxos.length})</h4>
      <ul className={styles.list}>
        {utxos.map((utxo) => (
          <li key={`${utxo.transaction}:${utxo.index}`}>
            <UtxoLink utxo={utxo} ctx={ctx} />
          </li>
        ))}
      </ul>
      {wit ? (
        <details>
          <summary>WIT</summary>
          <pre className={styles.code}>{wit}</pre>
        </details>
      ) : null}
    </>
  );
}

function UtxoDetails({
  transaction,
  index,
  ctx,
}: {
  transaction: string;
  index: number;
  ctx: Context;
}) {
  const utxo = ctx.state.utxos.find(
    (u) => u.transaction === transaction && u.index === index,
  );
  const [methods, setMethods] = useState<{ wit?: string; error?: string }>();
  const spent = utxo?.spentBy;
  const published =
    utxo &&
    ctx.state.contracts.find(({ digest }) => digest === utxo.contract)
      ?.height !== undefined;
  useEffect(() => {
    setMethods(undefined);
    if (spent !== undefined) {
      return;
    }
    let active = true;
    ctx.call({ op: "utxoWit", utxo: { transaction, index } }).then(
      (wit) => active && setMethods({ wit: wit as string | undefined }),
      (err: unknown) =>
        active &&
        setMethods({ error: err instanceof Error ? err.message : String(err) }),
    );
    return () => {
      active = false;
    };
  }, [transaction, index, spent, published]);
  if (!utxo) {
    return <p>UTXO not found.</p>;
  }
  return (
    <>
      <h3>
        <Swatch contract={utxo.contract} />
        UTXO {utxoLabel(utxo)}
      </h3>
      <dl className={styles.fields}>
        <Field label="Status">
          {utxo.spentBy ? (
            <>
              spent by{" "}
              <Link
                to={{ kind: "transaction", digest: utxo.spentBy }}
                onSelect={ctx.onSelect}
              >
                tx {digest(utxo.spentBy)}
              </Link>
            </>
          ) : (
            <span className={styles.live}>live</span>
          )}
        </Field>
        <Field label="Contract">
          <Link
            to={{ kind: "contract", digest: utxo.contract }}
            onSelect={ctx.onSelect}
          >
            {digest(utxo.contract)}
          </Link>
        </Field>
        <Field label="Instance">
          <code>{utxo.instance}</code>
        </Field>
        <Field label="Created by">
          {utxo.transaction ? (
            <Link
              to={{ kind: "transaction", digest: utxo.transaction }}
              onSelect={ctx.onSelect}
            >
              tx {digest(utxo.transaction)}
            </Link>
          ) : (
            "genesis"
          )}
        </Field>
        <Field label="Input reference">
          <Copy value={`${utxo.transaction}:${utxo.index}`} />
        </Field>
        <Field label="Storage">{utxo.storage} bytes</Field>
      </dl>
      {utxo.spentBy ? null : (
        <>
          <h4>Methods</h4>
          <p className={styles.hint}>
            Calls discard the resulting state, so they only read the UTXO.
          </p>
          {!methods ? (
            <p>loading</p>
          ) : methods.error ? (
            <p className={styles.error}>{methods.error}</p>
          ) : methods.wit === undefined ? (
            <p className={styles.hint}>
              The contract of this UTXO is not published on this ledger, so the
              ledger cannot describe its methods.{" "}
              {ctx.published[utxo.contract] ? (
                <button
                  className={styles.link}
                  onClick={() =>
                    ctx.perform({
                      op: "publishFile",
                      path: ctx.published[utxo.contract].path,
                    })
                  }
                >
                  Publish {ctx.published[utxo.contract].path}
                </button>
              ) : null}
            </p>
          ) : (
            witFuncs(methods.wit).map(({ name, params, result }) => (
              <div key={name} className={styles.method}>
                <code>
                  {name}({params}){result ? ` -> ${result}` : ""}
                </code>
                <Invoke
                  label="Call"
                  placeholder={params ? undefined : "no arguments"}
                  run={(args) =>
                    ctx.call({
                      op: "utxo",
                      utxo: { transaction, index },
                      method: name,
                      args,
                    })
                  }
                />
              </div>
            ))
          )}
        </>
      )}
    </>
  );
}

function TransactionDetails({ d, ctx }: { d: string; ctx: Context }) {
  const tx = ctx.state.transactions.find(({ digest }) => digest === d);
  if (!tx) {
    return <p>Transaction not found.</p>;
  }
  const outputs = ctx.state.utxos.filter(
    ({ transaction }) => transaction === d,
  );
  return (
    <>
      <h3>Transaction</h3>
      <dl className={styles.fields}>
        <Field label="Digest">
          <Copy value={d} />
        </Field>
        <Field label="Block">
          <Link
            to={{ kind: "block", height: tx.height }}
            onSelect={ctx.onSelect}
          >
            {String(tx.height)}
          </Link>
        </Field>
        <Field label="Signed by">
          {tx.signers.map((k) => (
            <Link
              key={k}
              to={{ kind: "account", key: k }}
              onSelect={ctx.onSelect}
            >
              {key(k)}
            </Link>
          ))}
        </Field>
        <Field label="Proof">
          {tx.proof ? `${amount(BigInt(tx.proof))} bytes` : "none"}
        </Field>
      </dl>
      <h4>Inputs ({tx.inputs.length})</h4>
      <ul className={styles.list}>
        {tx.inputs.map((input) => {
          const utxo = ctx.state.utxos.find(
            (u) =>
              u.transaction === input.transaction && u.index === input.index,
          );
          return (
            <li key={`${input.transaction}:${input.index}`}>
              {utxo ? <UtxoLink utxo={utxo} ctx={ctx} /> : utxoLabel(input)}
            </li>
          );
        })}
      </ul>
      <h4>Outputs ({outputs.length})</h4>
      <ul className={styles.list}>
        {outputs.map((utxo) => (
          <li key={utxo.index}>
            <UtxoLink utxo={utxo} ctx={ctx} />
          </li>
        ))}
      </ul>
      <h4>Events ({tx.events.length})</h4>
      <ul className={styles.list}>
        {tx.events.map(({ abiName, name, params }, i) => (
          <li key={i}>
            <code>{name}</code> ({abiName}, {params} bytes)
          </li>
        ))}
      </ul>
    </>
  );
}

function AccountDetails({ k, ctx }: { k: string; ctx: Context }) {
  const account = ctx.state.accounts.find(({ key }) => key === k);
  if (!account) {
    return <p>Account not found.</p>;
  }
  const published = ctx.state.contracts.filter(
    ({ publisher }) => publisher === k,
  );
  return (
    <>
      <h3>Account{k === ctx.wallet ? " (user.key)" : ""}</h3>
      <dl className={styles.fields}>
        <Field label="Public key">
          <Copy value={k} />
        </Field>
        <Field label="Balance">{amount(account.balance)}</Field>
        <Field label="Funded">{amount(account.funded)}</Field>
        <Field label="Nonce">{amount(account.nonce)}</Field>
      </dl>
      <h4>Published contracts ({published.length})</h4>
      <ul className={styles.list}>
        {published.map(({ digest: d }) => (
          <li key={d}>
            <Link to={{ kind: "contract", digest: d }} onSelect={ctx.onSelect}>
              <Swatch contract={d} />
              {digest(d)}
            </Link>
          </li>
        ))}
      </ul>
    </>
  );
}

function BlockDetails({ height, ctx }: { height: bigint; ctx: Context }) {
  if (height === 0n) {
    const outputs = ctx.state.utxos.filter(({ transaction }) => !transaction);
    return (
      <>
        <h3>Genesis</h3>
        <h4>Outputs ({outputs.length})</h4>
        <ul className={styles.list}>
          {outputs.map((utxo) => (
            <li key={utxo.index}>
              <UtxoLink utxo={utxo} ctx={ctx} />
            </li>
          ))}
        </ul>
      </>
    );
  }
  const block = ctx.state.blocks.find((b) => b.height === height);
  if (!block) {
    return <p>Block not found.</p>;
  }
  return (
    <>
      <h3>Block {String(height)}</h3>
      <ul className={styles.list}>
        {block.envelopes.map((e, index) => (
          <li key={index}>
            {e.kind === "fund" ? (
              <Link
                to={{ kind: "fund", height, index }}
                onSelect={ctx.onSelect}
              >
                fund {key(e.account)} with {amount(e.amount)}
              </Link>
            ) : e.kind === "publish" ? (
              <Link
                to={{ kind: "contract", digest: e.digest }}
                onSelect={ctx.onSelect}
              >
                <Swatch contract={e.digest} />
                publish {digest(e.digest)}
              </Link>
            ) : (
              <Link
                to={{ kind: "transaction", digest: e.digest }}
                onSelect={ctx.onSelect}
              >
                tx {digest(e.digest)}
              </Link>
            )}{" "}
            <span className={styles.hint}>{e.size} bytes</span>
          </li>
        ))}
      </ul>
    </>
  );
}

function FundDetails({
  height,
  index,
  ctx,
}: {
  height: bigint;
  index: number;
  ctx: Context;
}) {
  const e = ctx.state.blocks.find((b) => b.height === height)?.envelopes[index];
  if (e?.kind !== "fund") {
    return <p>Envelope not found.</p>;
  }
  return (
    <>
      <h3>Fund</h3>
      <dl className={styles.fields}>
        <Field label="Account">
          <Link
            to={{ kind: "account", key: e.account }}
            onSelect={ctx.onSelect}
          >
            {key(e.account)}
          </Link>
        </Field>
        <Field label="Amount">{amount(e.amount)}</Field>
        <Field label="Admin nonce">{amount(e.nonce)}</Field>
        <Field label="Block">
          <Link to={{ kind: "block", height }} onSelect={ctx.onSelect}>
            {String(height)}
          </Link>
        </Field>
        <Field label="Digest">
          <Copy value={e.digest} />
        </Field>
      </dl>
    </>
  );
}

export function Details({
  selection,
  ...ctx
}: Context & { selection: Selection | undefined }) {
  if (!selection) {
    return (
      <div className={styles.empty}>
        <p>
          Select a block, transaction, UTXO or contract in the graph to see its
          details and interact with it.
        </p>
        <p>
          Drop a Starstream source (<code>.star</code>) or a compiled component
          (<code>.wasm</code>) anywhere on this page to publish it.
        </p>
      </div>
    );
  }
  switch (selection.kind) {
    case "contract":
      return (
        <ContractDetails
          key={selection.digest}
          d={selection.digest}
          ctx={ctx}
        />
      );
    case "utxo":
      return (
        <UtxoDetails
          key={`${selection.transaction}:${selection.index}`}
          transaction={selection.transaction}
          index={selection.index}
          ctx={ctx}
        />
      );
    case "transaction":
      return <TransactionDetails d={selection.digest} ctx={ctx} />;
    case "account":
      return <AccountDetails k={selection.key} ctx={ctx} />;
    case "block":
      return <BlockDetails height={selection.height} ctx={ctx} />;
    case "fund":
      return (
        <FundDetails
          height={selection.height}
          index={selection.index}
          ctx={ctx}
        />
      );
  }
}
