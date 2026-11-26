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
  utxoLabel,
  type Selection,
} from "./format";
import styles from "./styles.module.css";

const WIDTH = 200;
const LABEL = 80;
const LEFT = 16;
const TOP = 16;
const GAP_X = 64;
const GAP_Y = 32;
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
 * Flow of the ledger: a row per block, after one for the genesis outputs,
 * with each UTXO linked to the input of the transaction spending it.
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
  const blocks = state.blocks.length;
  useEffect(() => {
    const el = scroller.current;
    if (el) {
      el.scrollTop = el.scrollHeight;
    }
  }, [blocks]);

  const utxos = new Map(state.utxos.map((utxo) => [utxoId(utxo), utxo]));
  const outputs = new Map<string, Point>();
  const edges: [string, Point][] = [];
  const byTransaction = new Map<string, UtxoInfo[]>();
  for (const utxo of state.utxos) {
    const list = byTransaction.get(utxo.transaction) ?? [];
    list.push(utxo);
    byTransaction.set(utxo.transaction, list);
  }
  const selected = (s: Selection) =>
    same(selection, s) ? styles.selected : "";

  const row = (
    utxo: UtxoInfo,
    x: number,
    y: number,
    text: string,
    tooltip: string,
  ): ReactNode => {
    const s: Selection = {
      kind: "utxo",
      transaction: utxo.transaction,
      index: utxo.index,
    };
    return (
      <g
        key={`row-${x}-${y}`}
        className={`${styles.row} ${selected(s)}`}
        onClick={(e) => {
          e.stopPropagation();
          onSelect(s);
        }}
      >
        <title>{tooltip}</title>
        <rect x={x + 1} y={y} width={WIDTH - 2} height={ROW} />
        <text x={x + 10} y={y + ROW / 2 + 4}>
          {text}
        </text>
        <Port utxo={utxo} at={{ x: x + WIDTH, y: y + ROW / 2 }} />
      </g>
    );
  };

  const card = (
    x: number,
    y: number,
    title: string,
    tooltip: string,
    s: Selection,
    inputs: UtxoInfo[],
    created: UtxoInfo[],
    className: string,
  ): Layout => {
    const rows = inputs.length + created.length;
    const height = HEAD + rows * ROW + (rows ? 6 : 0);
    const nodes: ReactNode[] = [
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
    ];
    let top = y + HEAD;
    for (const utxo of inputs) {
      edges.push([utxoId(utxo), { x: x + WIDTH, y: top + ROW / 2 }]);
      nodes.push(
        row(
          utxo,
          x,
          top,
          `← ${utxoLabel(utxo)} ${utxo.instance}`,
          `Spends UTXO ${utxoLabel(utxo)}: ${utxo.instance} of contract ${utxo.contract}`,
        ),
      );
      top += ROW;
    }
    for (const utxo of created) {
      outputs.set(utxoId(utxo), { x: x + WIDTH, y: top + ROW / 2 });
      nodes.push(
        row(
          utxo,
          x,
          top,
          `→ ${utxo.index} ${utxo.instance}`,
          `Creates UTXO ${utxo.index}: ${utxo.instance} of contract ${utxo.contract}${utxo.spentBy ? `, spent by ${utxo.spentBy}` : ""}`,
        ),
      );
      top += ROW;
    }
    return { height, nodes };
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
        const tx = state.transactions.find(({ digest }) => digest === e.digest);
        return card(
          x,
          y,
          `tx ${digest(e.digest)}`,
          `Transaction ${e.digest}`,
          { kind: "transaction", digest: e.digest },
          (tx?.inputs ?? []).flatMap((input) => utxos.get(utxoId(input)) ?? []),
          byTransaction.get(e.digest) ?? [],
          "",
        );
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
  let width = 0;
  let y = TOP;
  const block = (
    label: string,
    s: Selection | undefined,
    items: ((x: number, y: number) => Layout)[],
  ) => {
    nodes.push(
      <text
        key={`label-${label}`}
        className={`${styles.columnLabel} ${s ? styles.clickable : ""}`}
        x={LEFT}
        y={y + 19}
        onClick={s && (() => onSelect(s))}
      >
        {label}
      </text>,
    );
    let x = LEFT + LABEL;
    let height = PILL;
    for (const item of items) {
      const layout = item(x, y);
      nodes.push(...layout.nodes);
      height = Math.max(height, layout.height);
      x += WIDTH + GAP_X;
    }
    width = Math.max(width, x);
    y += height + GAP_Y;
  };

  const genesis = byTransaction.get("") ?? [];
  if (genesis.length) {
    block("Genesis", { kind: "block", height: 0n }, [
      (x, y) =>
        card(
          x,
          y,
          "genesis outputs",
          "Outputs of the genesis block",
          { kind: "block", height: 0n },
          [],
          genesis,
          styles.genesis,
        ),
    ]);
  }
  state.blocks.forEach(({ height, envelopes }) =>
    block(
      `Block ${height}`,
      { kind: "block", height },
      envelopes.map((e, index) => (x, y) => envelope(e, height, index, x, y)),
    ),
  );

  const paths = edges.flatMap(([id, to]) => {
    const from = outputs.get(id);
    const utxo = utxos.get(id);
    if (!from || !utxo) {
      return [];
    }
    const bend = 16 + Math.min(GAP_X - 24, Math.abs(to.y - from.y) / 8);
    return [
      <path
        key={`${id}->${to.x},${to.y}`}
        className={styles.edge}
        d={`M ${from.x} ${from.y} C ${from.x + bend} ${from.y}, ${to.x + bend} ${to.y}, ${to.x} ${to.y}`}
        style={{ stroke: color(utxo.contract) }}
      />,
    ];
  });

  return (
    <div className={styles.graph} ref={scroller}>
      <svg
        width={Math.max(width, LEFT + LABEL + WIDTH + GAP_X)}
        height={Math.max(y, 120)}
        role="img"
        aria-label="Blocks, transactions and UTXOs of the ledger"
      >
        <g>{paths}</g>
        <g>{nodes}</g>
      </svg>
      {!blocks && !genesis.length ? (
        <p className={styles.graphEmpty}>
          <span>
            The ledger is empty. Drop a Starstream source (<code>.star</code>)
            or a component (<code>.wasm</code>) here, or pick an example, to
            publish it.
          </span>
        </p>
      ) : null}
    </div>
  );
}
