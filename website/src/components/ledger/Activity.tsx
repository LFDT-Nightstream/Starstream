import type { Activity as Entry } from "../../ledger.worker";
import styles from "./styles.module.css";

/** The `starstream` commands the page ran, newest first. */
export function Activity({ entries }: { entries: Entry[] }) {
  if (!entries.length) {
    return (
      <p className={styles.empty}>
        Actions on this page run <code>starstream</code> commands, which are
        listed here so you can repeat them in the terminal.
      </p>
    );
  }
  return (
    <ol className={styles.activity}>
      {entries.map(({ id, command, status, output }) => (
        <li key={id} className={styles[status]}>
          <code className={styles.command}>$ {command}</code>
          {output.trim() ? (
            <details open={status === "error"}>
              <summary>output</summary>
              <pre className={styles.code}>{output.trim()}</pre>
            </details>
          ) : null}
        </li>
      ))}
    </ol>
  );
}
