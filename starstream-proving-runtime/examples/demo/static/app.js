const $ = (selector) => document.querySelector(selector);
const htmlCache = new Map();
function renderHtml(selector, html) {
  if (htmlCache.get(selector) === html) return;
  $(selector).innerHTML = html;
  htmlCache.set(selector, html);
}
const esc = (value) =>
  String(value ?? "—").replace(
    /[&<>"']/g,
    (c) =>
      ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[
        c
      ],
  );
const duration = (ms) =>
  ms == null
    ? "—"
    : ms < 1000
      ? `${ms.toFixed(0)} ms`
      : `${(ms / 1000).toFixed(2)} s`;
const rootText = (root) => root?.join(" ") ?? "—";
const shortRoot = (root) => (root ? `${root[0]}…${root[3].slice(-8)}` : "");
const statementOutputs = (outputs, otherOutputs) => outputs?.length
  ? outputs.map((output) => {
      const other = otherOutputs?.find((candidate) => candidate.utxo === output.utxo);
      const fields = output.fields?.map((field) => {
        const otherValue = other?.fields?.find((candidate) => candidate.name === field.name)?.value;
        const changed = field.value !== otherValue;
        const value = field.value.replace(/^U64\((\d+)\)$/, "$1");
        return `<span class="statement-field ${changed ? "changed" : ""}">${esc(field.name)} <strong>${esc(value)}</strong></span>`;
      }).join("") || "";
      const rootChanged = rootText(output.storage_root) !== rootText(other?.storage_root);
      const methodsChanged = JSON.stringify(output.methods) !== JSON.stringify(other?.methods);
      return `<div class="statement-output"><strong>UTXO ${esc(output.utxo)}</strong><div class="statement-fields">${fields || "No storage fields"}</div><div class="statement-meta"><span class="${rootChanged ? "changed" : ""}">storage <code title="${esc(rootText(output.storage_root))}">${esc(shortRoot(output.storage_root))}</code></span><span class="${methodsChanged ? "changed" : ""}">${output.methods?.length ?? 0} methods</span></div></div>`;
    }).join("")
  : '<div class="statement-output">No output UTXOs</div>';
const spinner = '<span class="spinner" aria-hidden="true"></span>';
const highlight = (source) => esc(source).replace(
  /\b(abi|utxo|storage|let|mut|main|fn|pub|yield|impl|script|while|for|if|else|return|u64|u32|bool|component|module|func|import|export|memory|type|param|result|call|local|global)\b|\b(\d+)\b/g,
  (match, keyword) => `<span class="${keyword ? "kw" : "num"}">${match}</span>`,
);
let examples = [],
  state = { busy: false, proofs: [] },
  execution = null,
  loadedRun = null,
  loadedRevision = null;
let sourceTab = 0,
  mode = "captured",
  posting = false,
  connected = false;
let pollTimer = null;
let dialogRows = [],
  dialogTrace = null,
  interleavingRows = [],
  editingRow = null;
const drafts = new Map();
const selectedExample = () => $("#example").value;
const sourceDrafts = () => {
  const example = examples.find((item) => item.id === selectedExample());
  if (!example) return [];
  if (!drafts.has(example.id)) drafts.set(example.id, example.sources.map((file) => file.source));
  return drafts.get(example.id);
};
const combinedSource = () => {
  const files = sourceDrafts();
  return [...files.slice(1), files[0]].join("\n");
};
const current = () =>
  execution &&
  execution.example === selectedExample() &&
  loadedRun === state.run_id;
const busy = () => state.busy || posting;
const sourceFiles = () => {
  const example = examples.find((item) => item.id === selectedExample());
  const sources = (example?.sources || []).map((file, i) => ({...file, source: sourceDrafts()[i], kind: "star"}));
  return [...sources, ...(current() ? execution.generated || [] : [])];
};

async function api(path, body) {
  const response = await fetch(
    path,
    body
      ? {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            "X-Starstream-Demo": "1",
          },
          body: JSON.stringify(body),
        }
      : {},
  );
  const result = await response.json();
  if (!response.ok) throw new Error(result.error || `HTTP ${response.status}`);
  return result;
}
function showError(message) {
  $("#error").hidden = !message;
  $("#error").textContent = message || "";
}

function renderSources() {
  const files = sourceFiles();
  if (!files.length) return;
  sourceTab = Math.min(sourceTab, files.length - 1);
  $("#source-tabs").innerHTML = files
    .map(
      (file, i) =>
        `<button role="tab" aria-selected="${i === sourceTab}" data-source="${i}">${esc(file.name)}</button>`,
    )
    .join("");
  const file = files[sourceTab];
  const editable = file.kind === "star";
  $("#source-mode").textContent = editable ? "EDITABLE SOURCE" : "GENERATED WASM";
  $("#source").hidden = editable;
  $("#source-editor-wrap").hidden = !editable;
  if (editable) {
    $("#source-editor").value = file.source;
    $("#source-highlight").innerHTML = highlight(file.source) + "\n";
  }
  const lines = file.source.split("\n");
  $("#source").innerHTML = lines
    .slice(0, 180)
    .map((line, i) => {
      const highlighted = esc(line).replace(
        /\b(abi|utxo|storage|let|mut|main|fn|pub|yield|impl|script|while|u64|component|module|func|import|export|memory|type|param|result|call|local|global)\b|\b(\d+)\b/g,
        (match, keyword) =>
          `<span class="${keyword ? "kw" : "num"}">${match}</span>`,
      );
      return `<span class="source-line"><span class="line-number">${i + 1}</span>${highlighted || " "}</span>`;
    })
    .join("") +
    (lines.length > 180
      ? `<span class="source-truncated">Showing 180 of ${lines.length} lines · Expand to inspect the full file</span>`
      : "");
}

function eventDetails(row) {
  const fields = Object.entries(row.payload).filter(
    ([name]) => name !== "event" && !(name === "method" && row.method_name),
  );
  return (
    fields
      .map(
        ([name, value]) =>
          `${name}: ${Array.isArray(value) ? (value.length === 4 && value.slice(1).every((v) => v === 0) ? String(value[0]) : value.join(", ")) : JSON.stringify(value)}`,
      )
      .join(" · ") || "—"
  );
}
function traceRowSummary(row, type) {
  if (type === "events") return `
    <strong>${esc(row.kind)}</strong>
    ${row.method_name ? `<span class="trace-row-meta">method ${esc(row.method_name)}</span>` : ""}
    <span class="trace-row-detail">${esc(eventDetails(row))}</span>`;
  if (type === "normalized") return `
    <strong>${esc(row.kind)}</strong>
    <span class="trace-row-meta">pc ${esc(row.pc)} · ${esc(row.slot || row.opcode)}</span>
    ${row.gather_value == null ? "" : `<span class="trace-row-detail">gathered word ${esc(row.gather_value)}</span>`}`;
  const stack = row.stack.length ? row.stack.slice(-4).join(" · ") : "empty";
  return `<strong>${esc(row.opcode)}</strong>
    <span class="trace-row-meta">pc ${esc(row.pc)} · function ${esc(row.function)}</span>
    <span class="trace-row-detail">stack ${esc(row.stack.length > 4 ? `… · ${stack}` : stack)}</span>`;
}
function table(rows, type, origin, compact = false) {
  if (compact) return `<table class="trace-detail-table"><tbody>${rows.map((row, i) => `
    <tr tabindex="0" data-row="${i}" data-origin="${origin}" aria-label="Inspect row ${esc(row.index)} ${esc(row.opcode || row.kind)}">
      <td class="index">${esc(row.index)}</td><td class="trace-row-content">${traceRowSummary(row, type)}</td>
    </tr>`).join("")}</tbody></table>`;
  const headings =
    type === "events"
      ? ["#", ...(origin === "interleaving" ? ["source"] : []), "event", "committed blocks", "payload"]
      : type === "normalized"
        ? ["#", "row", "pc", "source", "gathered word"]
      : ["#", "opcode", "pc", "function", "operand stack"];
  return `<table><thead><tr>${headings.map((h) => `<th>${h}</th>`).join("")}</tr></thead><tbody>${rows
    .map((row, i) => {
      const cells =
        type === "events"
          ? [
              row.index,
              row.kind + (row.method_name ? ` · ${row.method_name}` : ""),
              row.blocks.length || "host only",
              eventDetails(row),
            ]
          : type === "normalized"
            ? [row.index, row.kind, row.pc, row.slot || row.opcode, row.gather_value ?? "—"]
          : [
              row.index,
              row.opcode,
              row.pc,
              row.function,
              row.stack.join(" · ") || "—",
            ];
      const source = origin === "interleaving"
        ? `<td><span class="event-source ${row.source === "Coord" ? "source-coord" : row.source === "UTXO 0" ? "source-utxo-0" : row.source === "UTXO 1" ? "source-utxo-1" : "source-transaction"}">${esc(row.source || "Transaction")}</span></td>`
        : "";
      return `<tr tabindex="0" data-row="${i}" data-origin="${origin}" aria-label="Inspect row ${esc(row.index)} ${type === "normalized" ? esc(row.kind) : esc(row.opcode || row.kind)}"><td class="index">${esc(cells[0])}</td>${source}${cells.slice(1).map((cell, j) => `<td class="${j === 0 ? "opcode" : ""}">${esc(cell)}</td>`).join("")}</tr>`;
    })
    .join("")}</tbody></table>`;
}
function renderTracePanels() {
  htmlCache.clear();
  $("#trace-panels").innerHTML = current()
    ? execution.traces.map((trace, i) => `
      <article class="panel trace-panel">
        <div class="trace-panel-head">
          <div><h3>${esc(trace.label)}</h3><small>${trace[mode].length} ${mode === "captured" ? "instructions" : mode === "normalized" ? "proof rows" : "host events"}</small></div>
          <div class="trace-head-actions"><button data-lock="${i}" data-locked="${Boolean(execution.locked?.[i])}" class="${execution.locked?.[i] ? "locked" : ""}">${execution.locked?.[i] ? "Locked" : "Lock"}</button><button data-expand-trace="${i}" class="text-button">Expand ↗</button></div>
        </div>
        <div class="trace-scroll" data-trace-scroll="${i}">${table(trace[mode], mode, `trace-${i}`, true)}</div>
        <div class="trace-panel-foot" id="trace-status-${i}"></div>
      </article>`).join("")
    : '<div class="panel empty wide">Program traces will appear after execution.</div>';
  document
    .querySelectorAll("[data-mode]")
    .forEach((button) =>
      button.classList.toggle("selected", button.dataset.mode === mode),
    );
}

function markFailingTransactionStep() {
  const failureStep = state.transaction_check?.failure_step;
  document.querySelectorAll('#interleaving [data-row], #dialog-table [data-row]').forEach((row) => {
    const isInterleaving = row.dataset.origin === "interleaving" ||
      (row.dataset.origin === "dialog" && dialogTrace == null);
    row.classList.toggle("failure-row", isInterleaving && failureStep != null && Number(row.dataset.row) === failureStep);
  });
}

function renderExecution() {
  renderTracePanels();
  const active = current();
  $("#expand-interleaving").disabled = !active || !execution.interleaving_ready;
  if (!active) {
    $("#execution-stats").textContent =
      "Run the script to capture its instructions.";
    $("#interleaving").innerHTML =
      '<div class="empty">The scheduled host events will appear here.</div>';
    $("#outputs").innerHTML =
      '<div class="empty">Storage is read from each surviving UTXO.</div>';
    $("#commitment").textContent = "Awaiting execution";
    $("#interleaving-count").textContent =
      "Calls, returns, and storage exports";
    return;
  }
  const captured = execution.traces.reduce(
    (n, trace) => n + trace.captured.length,
    0,
  );
  $("#execution-stats").textContent =
    `${execution.traces.length} programs · ${captured} instructions · ${duration(execution.elapsed_ms)}`;
  interleavingRows = execution.interleaving;
  $("#interleaving").innerHTML = execution.interleaving_ready
    ? table(interleavingRows, "events", "interleaving")
    : '<div class="empty">Press Interleave to combine the selected programs’ host events.</div>';
  $("#interleaving-count").textContent = !execution.interleaving_ready
    ? "No transaction trace yet."
    : `${interleavingRows.length} transaction steps${execution.interleaving_stale ? " · built before the current trace roots" : " · ready for circuit check"}`;
  $("#outputs").innerHTML = !execution.interleaving_ready
    ? '<div class="empty">Transaction outputs appear after interleaving.</div>'
    : execution.outputs
    .map((output) => {
      const primary = output.fields.find((field) => field.name === "count" || field.name === "reading");
      const value = primary ? `${primary.name} = ${primary.value.replace(/^U64\((\d+)\)$/, "$1")}` : "Storage exported";
      return `<div class="output-row"><div class="output-title"><h3>UTXO ${esc(output.utxo)}</h3><span class="output-value">${esc(value)}</span></div><div class="root" title="${esc(rootText(output.storage_root))}">storage ${esc(shortRoot(output.storage_root))}</div><details><summary>Exported storage & registered methods</summary><pre>${esc(output.fields.map((field) => `${field.name}: ${field.value}`).join("\n"))}\n\nStorage commitment\n${esc(rootText(output.storage_root))}\n\nMethod identities\n${esc(output.methods.join("\n"))}</pre></details></div>`;
    })
    .join("");
  $("#commitment").textContent = execution.interleaving_ready
    ? rootText(execution.commitment) : "Awaiting interleaving";
}

function renderState() {
  const active = current(),
    blocked = busy() || !connected;
  $("#run").disabled = blocked;
  $("#interleave").disabled = blocked || !active;
  $("#interleave").innerHTML = state.busy && state.phase === "Interleaving host events"
    ? `${spinner}Interleaving…` : "Interleave";
  $("#statement-notice").hidden = !active || !execution.statement_differs;
  $("#example").disabled = blocked;
  $("#run").innerHTML =
    state.busy && state.phase === "Compiling and tracing"
      ? `${spinner}Running…`
      : "Run";
  $("#run-status").textContent = state.busy
    ? `${state.phase}${state.active != null ? ` · ${state.proofs[state.active]?.label}` : ""}…`
    : active
      ? `Executed in ${duration(execution.elapsed_ms)} · storage exported`
      : "Compiles and executes locally. No proof yet.";
  const complete =
    active &&
    state.proofs.length &&
    state.proofs.every((proof) => proof.status === "Verified");
  $("#prove-all").disabled = blocked || !active || !execution.interleaving_ready || complete;
  $("#prove-all").innerHTML =
    state.busy && state.active != null
      ? `${spinner}Working sequentially…`
      : complete
        ? "All proofs verified ✓"
        : "Prove all sequentially <span>→</span>";
  $("#verify").disabled = blocked || !complete;
  document.querySelectorAll("[data-lock]").forEach((button) => { button.disabled = blocked; });
  $("#statement-choice").hidden = !active;
  $("#statement-choice").classList.toggle("is-different", Boolean(active && execution.statement_differs));
  renderHtml("#statement-choice", !active ? "" : execution.statement_differs ? `
    <div class="statement-choice-heading"><strong>Transaction statements differ</strong><span>CHECK + PROOF USE THE HELD STATEMENT</span></div>
    <div class="statement-compare">
      <div class="statement-version held"><strong>Held statement · selected</strong>${statementOutputs(execution.outputs, execution.latest_outputs)}<details class="statement-raw"><summary>Full held statement</summary><pre>${esc(execution.statement_json)}</pre></details></div>
      <div class="statement-version latest"><strong>Latest run · available to use</strong>${statementOutputs(execution.latest_outputs, execution.outputs)}<details class="statement-raw"><summary>Full latest statement</summary><pre>${esc(execution.latest_statement_json)}</pre></details></div>
    </div>
    <div class="statement-choice-action"><span>Changed values are highlighted. Switching statements does not rerun the traces or interleaving.</span><button data-update-statement class="primary" ${blocked ? "disabled" : ""}>Switch to latest statement →</button></div>
  ` : `<div class="statement-choice-heading"><strong>Statement matches latest run</strong><span>NO STATEMENT CHANGE</span></div><details class="statement-raw"><summary>Inspect statement</summary><pre>${esc(execution.statement_json)}</pre></details>`);
  $("#check-transaction").disabled = blocked || !active || !execution.interleaving_ready;
  $("#check-transaction").innerHTML = state.busy && state.phase === "Checking transaction"
    ? `${spinner}Checking transaction…` : "Check transaction →";
  $("#verify").innerHTML =
    state.busy && state.phase === "Verifying transaction"
      ? `${spinner}Verifying…`
      : "Verify transaction →";
  if (active) execution.traces.forEach((trace, i) => {
    const proof = state.proofs[i];
    const check = state.trace_checks?.[i];
    const working = state.busy && state.active === i;
    const done = proof.status === "Verified";
    const checkText = !check ? "Circuit not checked"
      : check.accepted ? `Circuit check passed · ${duration(check.elapsed_ms)}`
      : `Circuit check failed: ${esc(check.reason)}`;
    renderHtml(`#trace-status-${i}`, `
      <div class="trace-root-line"><span>TRACE ROOT</span><code>${esc(rootText(trace.root))}</code></div>
      <div class="trace-check ${!check ? "pending" : !check.accepted ? "rejected" : ""}">${checkText}</div>
      ${proof.preprocessing_ms == null && proof.proving_ms == null && proof.verification_ms == null ? "" : `<div class="trace-timings">${[
        proof.preprocessing_ms == null ? "" : `Preprocess ${duration(proof.preprocessing_ms)}`,
        proof.proving_ms == null ? "" : `Prove ${duration(proof.proving_ms)}`,
        proof.verification_ms == null ? "" : `Verify ${duration(proof.verification_ms)}`,
      ].filter(Boolean).join(" · ")}</div>`}
      <div class="trace-action-row">
        <button data-check="${i}" ${blocked ? "disabled" : ""}>${working && state.phase === "Checking Wasm trace" ? `${spinner}Checking…` : "Check trace"}</button>
        <button data-prove="${i}" ${blocked || done ? "disabled" : ""}>${working ? `${spinner}${esc(state.phase)}…` : done ? "Proof verified ✓" : "Prove & verify"}</button>
      </div>`);
  });
  const interleavingIndex = active ? execution.traces.length : 0;
  const interleavingProof = active ? state.proofs[interleavingIndex] : null;
  const working = active && state.busy && state.active === interleavingIndex;
  const done = interleavingProof?.status === "Verified";
  const needsWasm = active && !state.proofs.slice(0, interleavingIndex).every((proof) => proof.status === "Verified");
  renderHtml("#interleaving-proof", active ? `
    <article class="panel proof-card ${working ? "active" : done ? "complete" : ""}">
      <div class="proof-top"><h3>Interleaving proof</h3><span class="${done ? "done" : ""}">${working ? spinner : ""}${esc(done ? "Verified ✓" : interleavingProof.status)}</span></div>
      <div class="proof-time">${interleavingProof.proving_ms == null ? "—" : (interleavingProof.proving_ms / 1000).toFixed(2)} <small>${interleavingProof.proving_ms == null ? "" : "s"}</small></div>
      <div class="proof-sub">${working ? `${esc(state.phase)} · ${duration(Math.max(0, Date.now() - state.phase_started_ms))}` : "proving time"}</div>
      <div class="timings"><div><span>PREPROCESS</span><strong>${duration(interleavingProof.preprocessing_ms)}</strong></div><div><span>VERIFY</span><strong>${duration(interleavingProof.verification_ms)}</strong></div></div>
      <button data-prove="${interleavingIndex}" ${blocked || done || needsWasm || !execution.interleaving_ready ? "disabled" : ""}>${done ? "Proof verified" : needsWasm ? "Waiting for Wasm proofs" : "Prove & verify"}</button>
      <div class="proof-root">Binds the authenticated program roots</div>
    </article>` : "");
  renderHtml("#commitment-inputs", active && execution.interleaving_ready ? `
    <div class="commitment-input-heading">COMMITMENT INPUTS</div>
    ${execution.traces.map((trace, i) => `<div class="commitment-input"><span>${esc(trace.label)} · ${state.proofs[i]?.status === "Verified" ? "proven" : "captured"}</span><code title="${esc(rootText(trace.root))}">${esc(shortRoot(trace.root))}</code></div>`).join("")}
    <div class="commitment-input"><span>Selected statement</span><span>outputs · storage · ABI</span></div>` : "");
  const result = active ? state.verification : null;
  $("#verification").hidden = !result;
  if (result) {
    renderHtml("#verification", `<strong>✓ Complete proof verified</strong><p>${esc(result.description)} · ${duration(result.elapsed_ms)}</p><pre>COMMITMENT ${esc(rootText(result.commitment))}</pre>`);
  }
  const transactionCheck = active ? state.transaction_check : null;
  $("#transaction-check-result").hidden = !transactionCheck;
  if (transactionCheck) {
    $("#transaction-check-result").classList.toggle("rejected", !transactionCheck.accepted);
    const failure = transactionCheck.failure_step == null ? null : execution.interleaving?.[transactionCheck.failure_step];
    renderHtml("#transaction-check-result", `<strong>${transactionCheck.accepted ? "✓ Transaction circuit accepts selected traces" : "↛ Transaction circuit rejects selected traces"}</strong><p>${transactionCheck.accepted ? "Selected traces and the selected transaction statement agree." : esc(transactionCheck.reason)}</p>${failure ? `<button data-jump-failure>View failing step ${esc(failure.index)} · ${esc(failure.kind)} ↑</button>` : ""}<small>${transactionCheck.authenticated_roots}/${transactionCheck.total_roots} roots independently proven · ${duration(transactionCheck.elapsed_ms)} · circuit check</small>`);
  }
  markFailingTransactionStep();
}

async function refresh() {
  state = await api("/api/state");
  connected = true;
  $("#connection").textContent = "Native runtime · local";
  if (
    state.run_id !== loadedRun || state.revision !== loadedRevision ||
    (!execution && state.proofs.length) ||
    (execution && !state.proofs.length)
  ) {
    const payload = await api("/api/execution");
    const newRun = payload.run_id !== loadedRun;
    loadedRun = payload.run_id;
    loadedRevision = payload.revision;
    execution = payload.execution;
    if (newRun) {
      sourceTab = 0;
    }
    if (execution) {
      $("#example").value = execution.example;
    }
    renderSources();
    $("#source").scrollTop = 0;
    $("#source-editor").scrollTop = 0;
    renderExecution();
  }
  showError(state.error);
  renderState();
}
async function command(body) {
  if (busy()) return;
  posting = true;
  showError(null);
  renderState();
  try {
    await api("/api/command", body);
    await refresh();
  } catch (error) {
    showError(error.message);
  } finally {
    posting = false;
    renderState();
    schedulePoll();
  }
}
function inspect(rows, index, traceIndex) {
  const row = rows[index];
  if (!row) return;
  $("#row-title").textContent = `Row ${row.index} · ${traceIndex != null && mode === "normalized" ? row.kind : row.opcode || row.kind}`;
  $("#row-json").value = JSON.stringify(row, null, 2);
  editingRow = mode === "normalized" && traceIndex != null
    ? {target: traceIndex, row: row.index} : null;
  $("#row-json").readOnly = !editingRow;
  $("#save-row").hidden = !editingRow;
  $("#row-dialog").showModal();
}
function expand(interleaving, traceIndex = null) {
  if (!current()) return;
  const trace = traceIndex == null ? null : execution.traces[traceIndex];
  dialogTrace = traceIndex;
  dialogRows = interleaving ? execution.interleaving : trace[mode];
  $("#dialog-title").textContent = interleaving
    ? `Interleaving · ${dialogRows.length} steps`
    : `${trace.label} · ${mode === "captured" ? "Wasm instructions" : mode === "normalized" ? "Proof rows" : "Host events"} · ${dialogRows.length} rows`;
  $("#dialog-table").innerHTML = table(
    dialogRows,
    interleaving ? "events" : mode,
    "dialog",
  );
  markFailingTransactionStep();
  $("#trace-dialog").showModal();
}
$("#run").onclick = () => command(current()
  ? {action: "rerun", run_id: state.run_id, source: combinedSource()}
  : {action: "run", example: selectedExample(), source: combinedSource()});
$("#source-editor").oninput = () => {
  sourceDrafts()[sourceTab] = $("#source-editor").value;
  $("#source-highlight").innerHTML = highlight($("#source-editor").value) + "\n";
};
$("#source-editor").onscroll = () => {
  $("#source-highlight").scrollTop = $("#source-editor").scrollTop;
  $("#source-highlight").scrollLeft = $("#source-editor").scrollLeft;
};
$("#interleave").onclick = () => command({action: "interleave", run_id: state.run_id});
$("#prove-all").onclick = () =>
  command({ action: "prove_all", run_id: state.run_id });
$("#verify").onclick = () =>
  command({
    action: "verify",
    run_id: state.run_id,
  });
$("#check-transaction").onclick = () =>
  command({ action: "check_transaction", run_id: state.run_id });
$("#save-row").onclick = () => {
  if (!editingRow) return;
  const raw = $("#row-json").value;
  try { JSON.parse(raw); } catch (error) { showError(`Invalid row JSON: ${error.message}`); return; }
  $("#row-dialog").close();
  command({ action: "edit_row", run_id: state.run_id, ...editingRow, raw });
};
$("#example").onchange = () => {
  sourceTab = 0;
  renderSources();
  renderExecution();
  renderState();
};
$("#expand-interleaving").onclick = () => expand(true);
$("#expand-source").onclick = () => {
  const file = sourceFiles()[sourceTab];
  if (!file) return;
  $("#source-dialog-title").textContent = file.name;
  $("#source-full").textContent = file.source;
  $("#source-full").scrollTop = 0;
  $("#source-dialog").showModal();
};
document.addEventListener("click", (event) => {
  const target = event.target.closest("button, [data-row]");
  if (!target) return;
  if (target.dataset.source != null) {
    sourceTab = Number(target.dataset.source);
    renderSources();
    $("#source").scrollTop = 0;
  }
  if (target.dataset.mode) {
    mode = target.dataset.mode;
    renderTracePanels();
    renderState();
  }
  if (target.dataset.expandTrace != null)
    expand(false, Number(target.dataset.expandTrace));
  if (target.dataset.prove != null)
    command({
      action: "prove",
      run_id: state.run_id,
      target: Number(target.dataset.prove),
    });
  if (target.dataset.check != null)
    command({ action: "check_trace", run_id: state.run_id, target: Number(target.dataset.check) });
  if (target.dataset.lock != null)
    command({ action: "set_trace_lock", run_id: state.run_id, target: Number(target.dataset.lock), locked: target.dataset.locked !== "true" });
  if (target.hasAttribute("data-update-statement"))
    command({ action: "update_statement", run_id: state.run_id });
  if (target.hasAttribute("data-jump-failure")) {
    const row = document.querySelector(`#interleaving [data-row="${state.transaction_check?.failure_step}"]`);
    row?.scrollIntoView({ behavior: "smooth", block: "center" });
    row?.focus({ preventScroll: true });
  }
  if (target.dataset.close)
    document.getElementById(target.dataset.close).close();
  if (target.dataset.row != null)
    inspect(target.dataset.origin === "dialog" ? dialogRows
      : target.dataset.origin === "interleaving" ? interleavingRows
      : execution.traces[Number(target.dataset.origin.slice(6))][mode],
      Number(target.dataset.row),
      target.dataset.origin === "dialog" ? dialogTrace
        : target.dataset.origin === "interleaving" ? null
        : Number(target.dataset.origin.slice(6)));
});
document.addEventListener("keydown", (event) => {
  if (
    (event.key === "Enter" || event.key === " ") &&
    event.target.matches("[data-row]")
  ) {
    event.preventDefault();
    event.target.click();
  }
  if (
    ["ArrowLeft", "ArrowRight", "Home", "End"].includes(event.key) &&
    event.target.matches('[role="tab"]')
  ) {
    event.preventDefault();
    const tabs = [...event.target.parentElement.children];
    const next =
      event.key === "Home"
        ? 0
        : event.key === "End"
          ? tabs.length - 1
          : (tabs.indexOf(event.target) +
              (event.key === "ArrowRight" ? 1 : -1) +
              tabs.length) %
            tabs.length;
    tabs[next].click();
    tabs[next].focus();
  }
});
function schedulePoll() {
  if (pollTimer || (!state.busy && connected)) return;
  pollTimer = setTimeout(async () => {
    pollTimer = null;
    try {
      await refresh();
    } catch (error) {
      connected = false;
      $("#connection").textContent = "Server disconnected";
      showError(`Cannot reach the local server: ${error.message}`);
      renderState();
    }
    schedulePoll();
  }, 800);
}
(async () => {
  try {
    examples = (await api("/api/examples")).examples;
    renderSources();
    await refresh();
    schedulePoll();
  } catch (error) {
    showError(error.message);
  }
})();
