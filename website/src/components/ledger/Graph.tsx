import { useEffect, useRef, type ReactNode } from "react";
import type {
  EnvelopeInfo,
  ExplorerState,
  UtxoInfo,
} from "../../ledger-explorer";
import {
  amount,
  color,
  digest,
  key,
  same,
  utxoId,
  type Selection,
} from "./format";
import styles from "./styles.module.css";

const COLUMN = 210;
const WIDTH = 170;
const LEFT = 24;
const TOP = 44;
const GAP = 12;
const HEAD = 26;
const ROW = 22;
const PILL = 30;

interface Point {
  x: number;
  y: number;
}

interface Layout {
  nodes: ReactNode[];
  height: number;
}

function Port({ utxo, at }: { utxo: UtxoInfo; at: Point }) {
  const fill = color(utxo.contract);
  return (
    <circle
      className={utxo.spentBy ? styles.portSpent : styles.port}
      cx={at.x}
      cy={at.y}
      r={5}
      style={{ stroke: fill, fill: utxo.spentBy ? undefined : fill }}
    />
  );
}

/**
 * Flow of the ledger: a column per block, after one for the genesis outputs,
 * with each UTXO linked to the transaction spending it.
 */
export function Graph({
  state,
  selection,
  onSelect,
}: {
  state: ExplorerState;
  selection: Selection | undefined;
  onSelect: (selection: Selection) => void;
}) {
  const scroller = useRef<HTMLDivElement>(null);
  const columns = state.blocks.length + 1;
  useEffect(() => {
    const el = scroller.current;
    if (el) {
      el.scrollLeft = el.scrollWidth;
    }
  }, [columns]);

  const outputs = new Map<string, Point>();
  const inputs = new Map<string, Point>();
  const byTransaction = new Map<string, UtxoInfo[]>();
  for (const utxo of state.utxos) {
    const list = byTransaction.get(utxo.transaction) ?? [];
    list.push(utxo);
    byTransaction.set(utxo.transaction, list);
  }
  const selected = (s: Selection) =>
    same(selection, s) ? styles.selected : "";

  const rows = (utxos: UtxoInfo[], x: number, y: number): ReactNode[] =>
    utxos.map((utxo, i) => {
      const cy = y + i * ROW + ROW / 2;
      const at = { x: x + WIDTH, y: cy };
      outputs.set(utxoId(utxo), at);
      const s: Selection = {
        kind: "utxo",
        transaction: utxo.transaction,
        index: utxo.index,
      };
      return (
        <g
          key={utxoId(utxo)}
          className={`${styles.row} ${selected(s)}`}
          onClick={(e) => {
            e.stopPropagation();
            onSelect(s);
          }}
        >
          <title>
            {`UTXO ${utxo.index}: ${utxo.instance} of contract ${utxo.contract}${utxo.spentBy ? `, spent by ${utxo.spentBy}` : ""}`}
          </title>
          <rect x={x + 1} y={y + i * ROW} width={WIDTH - 2} height={ROW} />
          <text x={x + 10} y={cy + 4}>
            {`${utxo.index} ${utxo.instance}`}
          </text>
          <Port utxo={utxo} at={at} />
        </g>
      );
    });

  const card = (
    x: number,
    y: number,
    title: string,
    tooltip: string,
    s: Selection,
    utxos: UtxoInfo[],
    className: string,
  ): Layout => {
    const height = HEAD + utxos.length * ROW + (utxos.length ? 6 : 0);
    return {
      height,
      nodes: [
        <g
          key={`card-${title}-${y}`}
          className={`${styles.card} ${className} ${selected(s)}`}
          onClick={() => onSelect(s)}
        >
          <title>{tooltip}</title>
          <rect x={x} y={y} width={WIDTH} height={height} rx={8} />
          <text className={styles.cardTitle} x={x + 10} y={y + 17}>
            {title}
          </text>
        </g>,
        ...rows(utxos, x, y + HEAD),
      ],
    };
  };

  const envelope = (
    e: EnvelopeInfo,
    height: bigint,
    index: number,
    x: number,
    y: number,
  ): Layout => {
    switch (e.kind) {
      case "transaction": {
        const layout = card(
          x,
          y,
          `tx ${digest(e.digest)}`,
          `Transaction ${e.digest}`,
          { kind: "transaction", digest: e.digest },
          byTransaction.get(e.digest) ?? [],
          "",
        );
        inputs.set(e.digest, { x, y: y + HEAD / 2 });
        return layout;
      }
      case "publish": {
        const s: Selection = { kind: "contract", digest: e.digest };
        return {
          height: PILL,
          nodes: [
            <g
              key={`publish-${e.digest}`}
              className={`${styles.pill} ${selected(s)}`}
              onClick={() => onSelect(s)}
            >
              <title>{`Published contract ${e.digest}`}</title>
              <rect x={x} y={y} width={WIDTH} height={PILL} rx={PILL / 2} />
              <rect
                x={x + 8}
                y={y + 8}
                width={14}
                height={14}
                rx={3}
                style={{ fill: color(e.digest) }}
              />
              <text x={x + 30} y={y + 19}>
                {`publish ${digest(e.digest)}`}
              </text>
            </g>,
          ],
        };
      }
      case "fund": {
        const s: Selection = { kind: "fund", height, index };
        return {
          height: PILL,
          nodes: [
            <g
              key={`fund-${height}-${index}`}
              className={`${styles.pill} ${styles.fund} ${selected(s)}`}
              onClick={() => onSelect(s)}
            >
              <title>{`Funded ${e.account} with ${amount(e.amount)}`}</title>
              <rect x={x} y={y} width={WIDTH} height={PILL} rx={PILL / 2} />
              <text x={x + 12} y={y + 19}>
                {`+${amount(e.amount)} → ${key(e.account)}`}
              </text>
            </g>,
          ],
        };
      }
    }
  };

  const nodes: ReactNode[] = [];
  let height = 0;
  const column = (
    i: number,
    label: string,
    s: Selection | undefined,
    items: ((x: number, y: number) => Layout)[],
  ) => {
    const x = LEFT + i * COLUMN;
    nodes.push(
      <text
        key={`label-${i}`}
        className={`${styles.columnLabel} ${s ? styles.clickable : ""}`}
        x={x}
        y={24}
        onClick={s && (() => onSelect(s))}
      >
        {label}
      </text>,
    );
    let y = TOP;
    for (const item of items) {
      const layout = item(x, y);
      nodes.push(...layout.nodes);
      y += layout.height + GAP;
    }
    height = Math.max(height, y);
  };

  const genesis = byTransaction.get("") ?? [];
  column(
    0,
    "Genesis",
    undefined,
    genesis.length
      ? [
          (x, y) =>
            card(
              x,
              y,
              "genesis outputs",
              "Outputs of the genesis block",
              { kind: "block", height: 0n },
              genesis,
              styles.genesis,
            ),
        ]
      : [],
  );
  state.blocks.forEach(({ height, envelopes }, i) =>
    column(
      i + 1,
      `Block ${height}`,
      { kind: "block", height },
      envelopes.map((e, index) => (x, y) => envelope(e, height, index, x, y)),
    ),
  );

  const edges: ReactNode[] = [];
  for (const tx of state.transactions) {
    const to = inputs.get(tx.digest);
    tx.inputs.forEach((input) => {
      const from = outputs.get(utxoId(input));
      const utxo = state.utxos.find(
        (u) => u.transaction === input.transaction && u.index === input.index,
      );
      if (!from || !to || !utxo) {
        return;
      }
      const bend = Math.max(40, (to.x - from.x) / 2);
      edges.push(
        <path
          key={`${utxoId(input)}->${tx.digest}`}
          className={styles.edge}
          d={`M ${from.x} ${from.y} C ${from.x + bend} ${from.y}, ${to.x - bend} ${to.y}, ${to.x} ${to.y}`}
          style={{ stroke: color(utxo.contract) }}
        />,
      );
    });
  }

  return (
    <div className={styles.graph} ref={scroller}>
      <svg
        width={LEFT * 2 + columns * COLUMN - (COLUMN - WIDTH)}
        height={Math.max(height, 120)}
        role="img"
        aria-label="Blocks, transactions and UTXOs of the ledger"
      >
        <g>{edges}</g>
        <g>{nodes}</g>
      </svg>
      {columns === 1 && !genesis.length ? (
        <p className={styles.graphEmpty}>
          The ledger is empty. Drop a Starstream source (<code>.star</code>) or
          a component (<code>.wasm</code>) here to publish it.
        </p>
      ) : null}
    </div>
  );
}
