import { useEffect, useState, type ReactNode } from "react";
import type { ExplorerState, UtxoInfo } from "../../ledger-explorer";
import type { Func, LedgerCall, Published, Signer } from "../../ledger.worker";
import {
  amount,
  color,
  digest,
  key,
  liveUtxos,
  utxoLabel,
  type Selection,
} from "./format";
import styles from "./styles.module.css";
import { CallForm, formatValue, signature } from "./ValueInput";

export type Call = (call: LedgerCall) => Promise<unknown>;

interface Context {
  state: ExplorerState;
  wallet: string | undefined;
  published: Record<string, Published>;
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

/** Load the functions `load` lists while `active`, again when `deps` change. */
function useFuncs(
  active: boolean,
  load: () => Promise<unknown>,
  deps: unknown[],
): { funcs?: Func[]; error?: string } | undefined {
  const [funcs, setFuncs] = useState<{ funcs?: Func[]; error?: string }>();
  useEffect(() => {
    setFuncs(undefined);
    if (!active) {
      return;
    }
    let current = true;
    load().then(
      (funcs) => current && setFuncs({ funcs: funcs as Func[] }),
      (err: unknown) =>
        current &&
        setFuncs({ error: err instanceof Error ? err.message : String(err) }),
    );
    return () => {
      current = false;
    };
  }, [active, ...deps]);
  return funcs;
}

function Funcs({
  loaded,
  children,
}: {
  loaded: { funcs?: Func[]; error?: string } | undefined;
  children: (func: Func) => ReactNode;
}) {
  if (!loaded) {
    return <p className={styles.hint}>loading…</p>;
  }
  if (loaded.error) {
    return <p className={styles.error}>{loaded.error}</p>;
  }
  if (!loaded.funcs?.length) {
    return <p className={styles.hint}>none</p>;
  }
  return (
    <>
      {loaded.funcs.map((func) => (
        <details key={func.name} className={styles.method}>
          <summary>
            <code>{signature(func)}</code>
          </summary>
          {children(func)}
        </details>
      ))}
    </>
  );
}

function ContractDetails({ d, ctx }: { d: string; ctx: Context }) {
  const contract = ctx.state.contracts.find(({ digest }) => digest === d);
  const [signer, setSigner] = useState<Signer>("wallet");
  const onLedger = contract?.height !== undefined;
  const scripts = useFuncs(
    onLedger,
    () => ctx.call({ op: "scripts", contract: d }),
    [d],
  );
  if (!contract) {
    return <p>Contract not found.</p>;
  }
  const utxos = ctx.state.utxos.filter(({ contract }) => contract === d);
  const published = ctx.published[d];
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
              {contract.publisher === ctx.wallet
                ? "your wallet"
                : key(contract.publisher)}
            </Link>
          </Field>
        ) : null}
        {contract.size !== undefined ? (
          <Field label="Size">{amount(BigInt(contract.size))} bytes</Field>
        ) : null}
      </dl>
      {onLedger ? (
        <button
          className="button button--sm button--secondary margin-bottom--md"
          onClick={() => ctx.perform({ op: "download", contract: d })}
        >
          Download component
        </button>
      ) : null}

      <h4>Coordination scripts</h4>
      {onLedger ? (
        <>
          <p className={styles.hint}>
            Scripts run in this browser and submit the transaction they build.
            Genesis UTXOs may only be spent by the ledger admin.
          </p>
          <label className={styles.inline}>
            <span>Sign with</span>
            <select
              value={signer}
              onChange={(e) => setSigner(e.target.value as Signer)}
            >
              <option value="wallet">your wallet</option>
              <option value="admin">the ledger admin</option>
            </select>
          </label>
          <Funcs loaded={scripts}>
            {(func) => (
              <>
                <CallForm
                  func={func}
                  utxos={liveUtxos(ctx.state)}
                  label="Run"
                  run={async (args) => {
                    const { transaction, result, error } = (await ctx.call({
                      op: "script",
                      contract: d,
                      script: func.name,
                      args,
                      signer,
                    })) as {
                      transaction?: string;
                      result: unknown;
                      error?: string;
                    };
                    return [
                      ...(func.result
                        ? [`returned ${formatValue(result)}`]
                        : []),
                      transaction
                        ? `submitted tx ${digest(transaction)}`
                        : `not submitted: ${error}`,
                    ].join("\n");
                  }}
                />
                {published ? (
                  <p className={styles.hint}>
                    <button
                      className={styles.link}
                      onClick={() => ctx.onGenesis(published.path, func.name)}
                    >
                      Start a new ledger running it at genesis
                    </button>
                  </p>
                ) : null}
              </>
            )}
          </Funcs>
        </>
      ) : (
        <p className={styles.hint}>Publish the contract to list its scripts.</p>
      )}

      <h4>UTXOs ({utxos.length})</h4>
      <ul className={styles.list}>
        {utxos.map((utxo) => (
          <li key={`${utxo.transaction}:${utxo.index}`}>
            <UtxoLink utxo={utxo} ctx={ctx} />
          </li>
        ))}
      </ul>
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
  const published =
    utxo &&
    ctx.state.contracts.find(({ digest }) => digest === utxo.contract)
      ?.height !== undefined;
  const methods = useFuncs(
    !!utxo && !utxo.spentBy && !!published,
    () =>
      ctx.call({
        op: "utxoMethods",
        contract: utxo!.contract,
        instance: utxo!.instance,
      }),
    [utxo?.contract, utxo?.instance],
  );
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
          {!published ? (
            <p className={styles.hint}>
              The contract of this UTXO is not published on this ledger, so its
              methods are unknown.{" "}
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
            <Funcs loaded={methods}>
              {(func) => (
                <CallForm
                  func={func}
                  utxos={liveUtxos(ctx.state)}
                  run={async (args) => {
                    const result = await ctx.call({
                      op: "utxo",
                      utxo: { transaction, index },
                      method: func.name,
                      ty: {
                        params: func.params.map(({ ty }) => ty),
                        result: func.result,
                      },
                      args,
                    });
                    return func.result ? formatValue(result) : "done";
                  }}
                />
              )}
            </Funcs>
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
      <h3>{k === ctx.wallet ? "Your wallet" : "Account"}</h3>
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
