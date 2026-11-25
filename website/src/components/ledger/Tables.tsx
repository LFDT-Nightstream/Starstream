import { useState, type ReactNode } from "react";
import type { ExplorerState } from "../../ledger-explorer";
import { Swatch } from "./Details";
import { Graph } from "./Graph";
import { amount, digest, key, same, utxoLabel, type Selection } from "./format";
import styles from "./styles.module.css";

type Tab =
  "blocks" | "transactions" | "utxos" | "contracts" | "accounts" | "files";

function Row({
  to,
  selection,
  onSelect,
  children,
}: {
  to: Selection;
  selection: Selection | undefined;
  onSelect: (selection: Selection) => void;
  children: ReactNode;
}) {
  return (
    <tr
      className={same(selection, to) ? styles.selectedRow : undefined}
      onClick={() => onSelect(to)}
    >
      {children}
    </tr>
  );
}

/** Views of everything on the ledger, and of the files of the terminal. */
export function Tables({
  state,
  wallet,
  files,
  selection,
  onSelect,
  onDownload,
}: {
  state: ExplorerState;
  wallet: string | undefined;
  files: string[];
  selection: Selection | undefined;
  onSelect: (selection: Selection) => void;
  onDownload: (path: string) => void;
}) {
  const [tab, setTab] = useState<Tab>("blocks");
  const [spent, setSpent] = useState(false);
  const live = state.utxos.filter(({ spentBy }) => !spentBy).length;
  const utxos = spent
    ? state.utxos
    : state.utxos.filter(({ spentBy }) => !spentBy);
  const tabs: [Tab, string][] = [
    ["blocks", `Blocks (${state.height})`],
    ["transactions", `Transactions (${state.transactions.length})`],
    ["utxos", `UTXOs (${live} live)`],
    ["contracts", `Contracts (${state.contracts.length})`],
    ["accounts", `Accounts (${state.accounts.length})`],
    ["files", `Files (${files.length})`],
  ];
  const row = { selection, onSelect };
  return (
    <section className={styles.tables}>
      <div className={styles.tabs} role="tablist">
        {tabs.map(([id, label]) => (
          <button
            key={id}
            role="tab"
            aria-selected={tab === id}
            className={tab === id ? styles.tabActive : styles.tab}
            onClick={() => setTab(id)}
          >
            {label}
          </button>
        ))}
        {tab === "utxos" ? (
          <label className={styles.toggle}>
            <input
              type="checkbox"
              checked={spent}
              onChange={(e) => setSpent(e.target.checked)}
            />{" "}
            show spent
          </label>
        ) : null}
      </div>
      {tab === "blocks" ? (
        <Graph state={state} selection={selection} onSelect={onSelect} />
      ) : (
        <div className={styles.tableScroll}>
          <table className={styles.table}>
            {tab === "contracts" ? (
              <>
                <thead>
                  <tr>
                    <th>Contract</th>
                    <th>Published</th>
                    <th>Publisher</th>
                    <th>Size</th>
                    <th>Live UTXOs</th>
                  </tr>
                </thead>
                <tbody>
                  {state.contracts.map((c) => (
                    <Row
                      key={c.digest}
                      to={{ kind: "contract", digest: c.digest }}
                      {...row}
                    >
                      <td>
                        <Swatch contract={c.digest} />
                        <code>{digest(c.digest)}</code>
                      </td>
                      <td>
                        {c.height === undefined
                          ? "not published"
                          : `block ${c.height}`}
                      </td>
                      <td>{c.publisher ? key(c.publisher) : "-"}</td>
                      <td>{c.size === undefined ? "-" : `${c.size} B`}</td>
                      <td>
                        {
                          state.utxos.filter(
                            (u) => u.contract === c.digest && !u.spentBy,
                          ).length
                        }
                      </td>
                    </Row>
                  ))}
                </tbody>
              </>
            ) : tab === "utxos" ? (
              <>
                <thead>
                  <tr>
                    <th>UTXO</th>
                    <th>Instance</th>
                    <th>Contract</th>
                    <th>Storage</th>
                    <th>Status</th>
                  </tr>
                </thead>
                <tbody>
                  {utxos.map((u) => (
                    <Row
                      key={`${u.transaction}:${u.index}`}
                      to={{
                        kind: "utxo",
                        transaction: u.transaction,
                        index: u.index,
                      }}
                      {...row}
                    >
                      <td>
                        <code>{utxoLabel(u)}</code>
                      </td>
                      <td>{u.instance}</td>
                      <td>
                        <Swatch contract={u.contract} />
                        <code>{digest(u.contract)}</code>
                      </td>
                      <td>{u.storage} B</td>
                      <td>
                        {u.spentBy ? (
                          "spent"
                        ) : (
                          <span className={styles.live}>live</span>
                        )}
                      </td>
                    </Row>
                  ))}
                </tbody>
              </>
            ) : tab === "transactions" ? (
              <>
                <thead>
                  <tr>
                    <th>Transaction</th>
                    <th>Block</th>
                    <th>Inputs</th>
                    <th>Outputs</th>
                    <th>Events</th>
                  </tr>
                </thead>
                <tbody>
                  {[...state.transactions].reverse().map((tx) => (
                    <Row
                      key={tx.digest}
                      to={{ kind: "transaction", digest: tx.digest }}
                      {...row}
                    >
                      <td>
                        <code>{digest(tx.digest)}</code>
                      </td>
                      <td>{String(tx.height)}</td>
                      <td>{tx.inputs.length}</td>
                      <td>{tx.outputs}</td>
                      <td>{tx.events.length}</td>
                    </Row>
                  ))}
                </tbody>
              </>
            ) : tab === "accounts" ? (
              <>
                <thead>
                  <tr>
                    <th>Account</th>
                    <th>Balance</th>
                    <th>Nonce</th>
                  </tr>
                </thead>
                <tbody>
                  {state.accounts.map((a) => (
                    <Row
                      key={a.key}
                      to={{ kind: "account", key: a.key }}
                      {...row}
                    >
                      <td>
                        <code>{key(a.key)}</code>
                        {a.key === wallet ? " (your wallet)" : ""}
                      </td>
                      <td>{amount(a.balance)}</td>
                      <td>{amount(a.nonce)}</td>
                    </Row>
                  ))}
                </tbody>
              </>
            ) : (
              <tbody>
                {files.map((path) => (
                  <tr key={path}>
                    <td>
                      <code>{path}</code>
                    </td>
                    <td className={styles.right}>
                      <button
                        className="button button--sm button--secondary"
                        onClick={() => onDownload(path)}
                      >
                        Download
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            )}
          </table>
        </div>
      )}
    </section>
  );
}
