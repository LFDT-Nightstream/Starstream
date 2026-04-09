const CONTRACT_IDS = ["utxo-a", "utxo-b", "coord"];

const state = {
  snapshot: null,
  activeContract: "utxo-a",
  busy: new Set(),
  monacoReady: false,
  editors: {},
  contractViews: {
    "utxo-a": "source",
    "utxo-b": "source",
    coord: "source",
  },
  editorFiles: {
    "utxo-a": { files: [], activeFileId: null },
    "utxo-b": { files: [], activeFileId: null },
    coord: { files: [], activeFileId: null },
  },
};

const elements = {
  semanticStatus: document.getElementById("semantic-status"),
  floatingTooltip: document.getElementById("floating-tooltip"),
  proofOutput: document.getElementById("proof-output"),
  proofPublicStatement: document.getElementById("proof-public-statement"),
  proofTransactionDebug: document.getElementById("proof-transaction-debug"),
  ledgerBeforeDetails: document.getElementById("ledger-before-details"),
  ledgerAfterDetails: document.getElementById("ledger-after-details"),
  ledgerBeforeDebug: document.getElementById("ledger-before-debug"),
  ledgerAfterDebug: document.getElementById("ledger-after-debug"),
  interleavedTrace: document.getElementById("interleaved-trace"),
  traceCoord: document.getElementById("trace-coord"),
  traceUtxoA: document.getElementById("trace-utxo-a"),
  traceUtxoB: document.getElementById("trace-utxo-b"),
  traceBoxCoord: document.getElementById("trace-box-coord"),
  traceBoxUtxoA: document.getElementById("trace-box-utxo-a"),
  traceBoxUtxoB: document.getElementById("trace-box-utxo-b"),
  interleavedBox: document.getElementById("interleaved-box"),
  currentEdge: document.getElementById("current-edge"),
  stepButton: document.getElementById("step"),
  foldButton: document.getElementById("fold"),
  applyTransactionButton: document.getElementById("apply-transaction"),
  traceButton: document.getElementById("trace"),
  initialStateUtxoA: document.getElementById("initial-state-utxo-a"),
  initialStateUtxoB: document.getElementById("initial-state-utxo-b"),
  applyInitialStates: document.getElementById("apply-initial-states"),
  initialStateStatus: document.getElementById("initial-state-status"),
  contractEditors: {
    "utxo-a": document.getElementById("contract-editor-utxo-a"),
    "utxo-b": document.getElementById("contract-editor-utxo-b"),
    coord: document.getElementById("contract-editor-coord"),
  },
  contractWat: {
    "utxo-a": document.getElementById("contract-wat-utxo-a"),
    "utxo-b": document.getElementById("contract-wat-utxo-b"),
    coord: document.getElementById("contract-wat-coord"),
  },
  contractFileTabs: {
    "utxo-a": document.getElementById("contract-files-utxo-a"),
    "utxo-b": document.getElementById("contract-files-utxo-b"),
    coord: document.getElementById("contract-files-coord"),
  },
  contractViewTabs: {
    "utxo-a": document.getElementById("contract-view-tabs-utxo-a"),
    "utxo-b": document.getElementById("contract-view-tabs-utxo-b"),
    coord: document.getElementById("contract-view-tabs-coord"),
  },
  contractStatuses: {
    "utxo-a": document.getElementById("contract-status-utxo-a"),
    "utxo-b": document.getElementById("contract-status-utxo-b"),
    coord: document.getElementById("contract-status-coord"),
  },
  contractErrors: {
    "utxo-a": document.getElementById("contract-error-utxo-a"),
    "utxo-b": document.getElementById("contract-error-utxo-b"),
    coord: document.getElementById("contract-error-coord"),
  },
  applyContracts: document.getElementById("apply-contracts"),
  contractTabs: [...document.querySelectorAll("#contract-tabs .tab")],
  contractPanels: [...document.querySelectorAll("[data-contract-panel]")],
};

function setBadge(node, text, kind = "muted") {
  node.textContent = text;
  node.className = `pill ${kind === "ok" ? "pill-ok" : kind === "error" ? "pill-error" : "pill-muted"}`;
}

async function applyContractsNow() {
  return runAction("Compiling contracts", async () => {
    for (const contractId of CONTRACT_IDS) {
      const entry = state.editorFiles[contractId];
      syncActiveEditor(contractId);
      for (const file of entry.files) {
        await request(`/api/contracts/${contractId}/editor-files/${encodeURIComponent(file.id)}`, {
          method: "PUT",
          body: JSON.stringify({ source: file.source }),
        });
        clearDraft(contractId, file.id);
      }
    }
    const compile = await request("/api/compile", { method: "POST" });
    await loadEditorFiles();
    return {
      ok: compile.ok,
      message: compile.ok ? "compiled all contract sources" : compile.message,
    };
  });
}

async function request(path, options = {}) {
  const response = await fetch(path, {
    headers: {
      "Content-Type": "application/json",
      ...(options.headers || {}),
    },
    ...options,
  });

  if (!response.ok) {
    const text = await response.text();
    throw new Error(text || `Request failed: ${response.status}`);
  }

  const contentType = response.headers.get("content-type") || "";
  if (contentType.includes("application/json")) {
    return response.json();
  }

  return response.text();
}

function renderViewTabs() {
  for (const tab of elements.contractTabs) {
    tab.classList.toggle("active", tab.dataset.contract === state.activeContract);
  }
  for (const panel of elements.contractPanels) {
    panel.classList.toggle(
      "active-contract",
      panel.dataset.contractPanel === state.activeContract,
    );
  }

  for (const node of Object.values(elements.contractEditors)) {
    node.classList.add("active");
  }
  for (const node of Object.values(elements.contractFileTabs)) {
    node.classList.add("active");
  }
  for (const node of Object.values(elements.contractWat)) {
    node.classList.remove("active");
  }
  layoutEditors();
}

function compileResultFor(contractId) {
  return state.snapshot?.compile?.results?.find((result) => result.contract === contractId) || null;
}

function draftStorageKey(contractId, fileId) {
  return `starstream-demo:draft:${contractId}:${fileId}`;
}

function clearAllDrafts() {
  try {
    const prefix = "starstream-demo:draft:";
    const keys = [];
    for (let index = 0; index < window.localStorage.length; index += 1) {
      const key = window.localStorage.key(index);
      if (key && key.startsWith(prefix)) {
        keys.push(key);
      }
    }
    for (const key of keys) {
      window.localStorage.removeItem(key);
    }
  } catch {
    // ignore storage failures
  }
}

function readDraft(contractId, fileId) {
  try {
    return window.localStorage.getItem(draftStorageKey(contractId, fileId));
  } catch {
    return null;
  }
}

function writeDraft(contractId, fileId, source) {
  try {
    window.localStorage.setItem(draftStorageKey(contractId, fileId), source);
  } catch {
    // ignore storage failures
  }
}

function clearDraft(contractId, fileId) {
  try {
    window.localStorage.removeItem(draftStorageKey(contractId, fileId));
  } catch {
    // ignore storage failures
  }
}

function syncActiveEditor(contractId) {
  const editor = state.editors[contractId];
  const activeFile = activeEditorFile(contractId);
  if (!editor || !activeFile) {
    return;
  }

  const value = editor.getValue();
  activeFile.source = value;
  writeDraft(contractId, activeFile.id, value);
}

function renderContractPanel() {
  if (!state.snapshot) {
    return;
  }

  const inputsUninitialized =
    elements.initialStateUtxoA.value === "" && elements.initialStateUtxoB.value === "";
  if (inputsUninitialized || !initialStatesAreDirty()) {
    elements.initialStateUtxoA.value = state.snapshot.initial_states.utxo_a;
    elements.initialStateUtxoB.value = state.snapshot.initial_states.utxo_b;
  }
  renderInitialStateStatus();

  renderEditorTabs();

  for (const contractId of CONTRACT_IDS) {
    const result = compileResultFor(contractId);
    const statusNode = elements.contractStatuses[contractId];
    const errorNode = elements.contractErrors[contractId];
    const watNode = elements.contractWat[contractId];

    if (!result) {
      setBadge(statusNode, "Not compiled", "muted");
      watNode.textContent = "No compiled WAT yet.";
      watNode.classList.add("empty");
      errorNode.textContent = "No compile errors.";
      errorNode.classList.add("empty");
      continue;
    }

    setBadge(statusNode, result.ok ? "Compiled" : "Compile failed", result.ok ? "ok" : "error");
    const watText = result.artifact?.wat_pretty || "";
    watNode.textContent = watText || "No compiled WAT yet.";
    watNode.classList.toggle("empty", !watText);
    if (result.error) {
      errorNode.textContent = result.error;
      errorNode.classList.remove("empty");
    } else {
      errorNode.textContent = "No compile errors.";
      errorNode.classList.add("empty");
    }
  }
}

function renderExecution() {
  if (!state.snapshot) {
    return;
  }

  const execution = state.snapshot.execution;
  renderTraceList(elements.interleavedTrace, execution.interleaved_trace, execution.active_process);

  const tracesByProcess = Object.fromEntries(
    execution.per_process_traces.map((trace) => [trace.process, trace]),
  );

  renderTraceBox(elements.traceCoord, tracesByProcess["coord"] || emptyTrace("coord"));
  renderTraceBox(elements.traceUtxoA, tracesByProcess["utxo-a"] || emptyTrace("utxo-a"));
  renderTraceBox(elements.traceUtxoB, tracesByProcess["utxo-b"] || emptyTrace("utxo-b"));

  elements.traceBoxCoord.classList.toggle("active-process", execution.active_process === "coord");
  elements.traceBoxUtxoA.classList.toggle("active-process", execution.active_process === "utxo-a");
  elements.traceBoxUtxoB.classList.toggle("active-process", execution.active_process === "utxo-b");
  elements.interleavedBox.classList.toggle("active-process", Boolean(execution.active_process));
  elements.currentEdge.textContent = execution.highlighted_edge || "No active edge";
  elements.currentEdge.classList.toggle("active", Boolean(execution.highlighted_edge));
  const isFinished =
    execution.total_steps > 0 && execution.step_index >= execution.total_steps;
  elements.stepButton.disabled = isFinished;
  elements.foldButton.disabled = !isFinished || execution.interleaved_trace.length === 0;
  elements.applyTransactionButton.disabled = !state.snapshot?.proof_public_statement;

  if (execution.semantics_ok === true) {
    setBadge(
      elements.semanticStatus,
      execution.semantics_message || "Semantics ok",
      "ok",
    );
  } else if (execution.semantics_ok === false) {
    setBadge(
      elements.semanticStatus,
      execution.semantics_message || "Semantics failed",
      "error",
    );
  } else {
    setBadge(elements.semanticStatus, "Semantics unchecked", "muted");
  }
}

function initialStatesAreDirty() {
  if (!state.snapshot) {
    return false;
  }

  return (
    Number(elements.initialStateUtxoA.value || 0) !== state.snapshot.initial_states.utxo_a ||
    Number(elements.initialStateUtxoB.value || 0) !== state.snapshot.initial_states.utxo_b
  );
}

function renderInitialStateStatus() {
  const dirty = initialStatesAreDirty();
  elements.applyInitialStates.classList.toggle("pending", dirty);
  setBadge(
    elements.initialStateStatus,
    dirty ? "Pending changes" : "Applied",
    dirty ? "error" : "ok",
  );
}

function emptyTrace(process) {
  return { process, effects: [], processed: 0, commitment: null };
}

function renderTraceBox(node, trace) {
  const progress = Math.min(trace.processed || 0, trace.effects.length || 0);
  const total = trace.effects.length || 0;
  node.innerHTML = "";

  const meta = document.createElement("div");
  meta.className = "trace-meta";

  const commitment = document.createElement("div");
  commitment.className = "trace-commitment";
  const commitmentText = document.createElement("span");
  commitmentText.textContent = trace.commitment || "No commitment yet";
  const commitmentInfo = document.createElement("span");
  commitmentInfo.className = "trace-commitment-info";
  commitmentInfo.textContent = "?";
  commitmentInfo.dataset.tooltip =
    "A binding commitment to the trace, that both the wasm and the interleaving proof get bound to";
  commitment.appendChild(commitmentText);
  commitment.appendChild(commitmentInfo);
  meta.appendChild(commitment);

  const counts = document.createElement("div");
  counts.className = "trace-counts";
  counts.textContent = `${progress} / ${total} consumed`;
  meta.appendChild(counts);

  const progressBar = document.createElement("div");
  progressBar.className = "trace-progress";
  const fill = document.createElement("div");
  fill.className = "trace-progress-fill";
  fill.style.width = total ? `${(progress / total) * 100}%` : "0%";
  progressBar.appendChild(fill);

  const list = document.createElement("div");
  list.className = "trace-list";
  renderTraceList(
    list,
    trace.effects.map((effect, index) => ({
      process: null,
      effect,
      processed: index < progress,
      cursor: progress > 0 && index === progress - 1,
    })),
    null,
  );

  node.appendChild(meta);
  node.appendChild(progressBar);
  node.appendChild(list);
}

function renderTraceList(node, items, activeProcess) {
  node.innerHTML = "";
  node.classList.toggle("empty", items.length === 0);

  if (!items.length) {
    const empty = document.createElement("div");
    empty.className = "trace-empty";
    empty.textContent = "No events yet.";
    node.appendChild(empty);
    return;
  }

  for (const item of items) {
    const row = document.createElement("div");
    row.className = "trace-item";
    if (item.processed) {
      row.classList.add("processed");
    }
    if (item.cursor) {
      row.classList.add("cursor");
    }
    if (item.process && item.process === activeProcess) {
      row.classList.add("active");
    }

    if (item.process) {
      const label = document.createElement("span");
      label.className = "trace-item-process";
      label.textContent = item.process;
      row.appendChild(label);
    }

    const text = document.createElement("span");
    text.className = "trace-item-effect";
    text.textContent = item.effect;
    row.appendChild(text);
    node.appendChild(row);
  }

  if (activeProcess && node === elements.interleavedTrace) {
    node.scrollTop = node.scrollHeight;
  }
}

function renderDiagnostics() {
  if (!elements.proofOutput) {
    return;
  }
  const proof = state.snapshot?.proof_output;
  const publicStatement = state.snapshot?.proof_public_statement;
  const transactionDebug = state.snapshot?.proof_transaction_debug;
  const ledgerBeforeDebug = state.snapshot?.ledger_before_debug;
  const ledgerAfterDebug = state.snapshot?.ledger_after_debug;
  elements.proofOutput.textContent = proof || "No proof yet.";
  elements.proofOutput.classList.toggle("empty", !proof);
  elements.proofPublicStatement.textContent =
    publicStatement || "No public statement yet.";
  elements.proofPublicStatement.classList.toggle("empty", !publicStatement);
  elements.proofTransactionDebug.textContent =
    transactionDebug || "No transaction structure yet.";
  elements.proofTransactionDebug.classList.toggle("empty", !transactionDebug);
  elements.ledgerBeforeDebug.textContent =
    ledgerBeforeDebug || "No ledger snapshot yet.";
  elements.ledgerBeforeDebug.classList.toggle("empty", !ledgerBeforeDebug);
  elements.ledgerAfterDebug.textContent =
    ledgerAfterDebug || "No ledger snapshot yet.";
  elements.ledgerAfterDebug.classList.toggle("empty", !ledgerAfterDebug);
}

function renderBusyStates() {
  const tracing = state.busy.has("Tracing");
  const folding = state.busy.has("Folding");
  const applyingTransaction = state.busy.has("Applying transaction");
  elements.traceButton.classList.toggle("loading", tracing);
  elements.traceButton.disabled = tracing;
  elements.foldButton.classList.toggle("loading", folding);
  if (folding) {
    elements.foldButton.disabled = true;
  }
  elements.applyTransactionButton.classList.toggle("loading", applyingTransaction);
  if (applyingTransaction) {
    elements.applyTransactionButton.disabled = true;
  }
}

function activeEditorFile(contractId) {
  const entry = state.editorFiles[contractId];
  if (!entry) return null;
  return entry.files.find((file) => file.id === entry.activeFileId) || entry.files[0] || null;
}

function editorLanguage(language) {
  if (language === "wit") return "plaintext";
  if (language === "wat") return "wat";
  return language;
}

function ensureEditor(contractId) {
  if (state.editors[contractId] || !state.monacoReady) {
    return state.editors[contractId] || null;
  }
  const editor = window.monaco.editor.create(elements.contractEditors[contractId], {
    value: "",
    language: "rust",
    automaticLayout: true,
    minimap: { enabled: false },
    fontFamily: "IBM Plex Mono, SFMono-Regular, monospace",
    fontSize: 13,
    lineHeight: 20,
    theme: "vs",
    scrollBeyondLastLine: false,
  });
  editor.onDidChangeModelContent(() => {
    const activeFile = activeEditorFile(contractId);
    if (!activeFile) {
      return;
    }
    const value = editor.getValue();
    activeFile.source = value;
    writeDraft(contractId, activeFile.id, value);
  });
  editor.addCommand(window.monaco.KeyMod.CtrlCmd | window.monaco.KeyCode.KeyS, () => {
    applyContractsNow();
  });
  state.editors[contractId] = editor;
  return editor;
}

function layoutEditors() {
  for (const contractId of CONTRACT_IDS) {
    state.editors[contractId]?.layout();
  }
}

function renderEditorTabs() {
  for (const contractId of CONTRACT_IDS) {
    const tabsNode = elements.contractFileTabs[contractId];
    const viewTabsNode = elements.contractViewTabs[contractId];
    const editor = ensureEditor(contractId);
    const entry = state.editorFiles[contractId];
    const currentView = state.contractViews[contractId] || "source";
    tabsNode.innerHTML = "";
    viewTabsNode.innerHTML = "";

    if (!state.monacoReady) {
      elements.contractEditors[contractId].textContent =
        "Monaco failed to load. Check network access and reload.";
      continue;
    }

    for (const view of [
      { id: "source", label: "Source" },
      { id: "wat", label: "WAT" },
    ]) {
      const button = document.createElement("button");
      button.className = "contract-file-tab";
      button.textContent = view.label;
      button.classList.toggle("active", view.id === currentView);
      button.addEventListener("click", () => {
        if (view.id === currentView) {
          return;
        }
        syncActiveEditor(contractId);
        state.contractViews[contractId] = view.id;
        renderEditorTabs();
      });
      viewTabsNode.appendChild(button);
    }

    const showSource = currentView === "source";
    elements.contractEditors[contractId].classList.toggle("hidden", !showSource);
    elements.contractFileTabs[contractId].classList.toggle("hidden", !showSource);
    elements.contractWat[contractId].classList.toggle("active", !showSource);

    for (const file of entry.files) {
      const button = document.createElement("button");
      button.className = "contract-file-tab";
      button.textContent = file.label;
      button.classList.toggle("active", file.id === entry.activeFileId);
      button.addEventListener("click", () => {
        syncActiveEditor(contractId);
        entry.activeFileId = file.id;
        renderEditorTabs();
      });
      tabsNode.appendChild(button);
    }

    const activeFile = activeEditorFile(contractId);
    if (!activeFile || !editor) continue;
    if (!showSource) continue;
    const model = editor.getModel();
    if (!model || model.__fileId !== activeFile.id) {
      const newModel = window.monaco.editor.createModel(
        activeFile.source,
        editorLanguage(activeFile.language),
      );
      newModel.__fileId = activeFile.id;
      editor.setModel(newModel);
    } else if (model.getValue() !== activeFile.source) {
      model.setValue(activeFile.source);
    }
  }
  layoutEditors();
}

async function loadEditorFiles() {
  for (const contractId of CONTRACT_IDS) {
    const previousActiveFileId = state.editorFiles[contractId]?.activeFileId || null;
    const files = await request(`/api/contracts/${contractId}/editor-files`);
    for (const file of files) {
      const draft = readDraft(contractId, file.id);
      if (draft !== null) {
        file.source = draft;
      }
    }
    const nextActiveFileId =
      files.find((file) => file.id === previousActiveFileId)?.id || files[0]?.id || null;
    state.editorFiles[contractId] = {
      files,
      activeFileId: nextActiveFileId,
    };
  }
}

async function initMonaco() {
  if (!window.require) {
    return;
  }
  await new Promise((resolve) => {
    window.require.config({
      paths: {
        vs: "https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/min/vs",
      },
    });
    window.require(["vs/editor/editor.main"], () => resolve());
  });
  state.monacoReady = true;
}

function hideTooltip() {
  elements.floatingTooltip.classList.remove("visible");
  elements.floatingTooltip.setAttribute("aria-hidden", "true");
}

function showTooltip(text, x, y) {
  elements.floatingTooltip.textContent = text;
  elements.floatingTooltip.style.left = `${x + 14}px`;
  elements.floatingTooltip.style.top = `${y - 10}px`;
  elements.floatingTooltip.classList.add("visible");
  elements.floatingTooltip.setAttribute("aria-hidden", "false");
}

function render() {
  renderViewTabs();
  renderContractPanel();
  renderExecution();
  renderDiagnostics();
  renderBusyStates();
}

async function refreshSnapshot() {
  const snapshot = await request("/api/snapshot");
  state.snapshot = snapshot;
  if (!state.editorFiles.coord.files.length) {
    await loadEditorFiles();
  }
  render();
}

async function runAction(label, fn) {
  try {
    state.busy.add(label);
    renderBusyStates();
    const result = await fn();
    await refreshSnapshot();
  } catch (error) {
    console.error(error);
  } finally {
    state.busy.delete(label);
    renderBusyStates();
  }
}

function bindEvents() {
  document.getElementById("trace").addEventListener("click", () => {
    runAction("Tracing", () => request("/api/trace", { method: "POST" }));
  });

  document.getElementById("reset-execution").addEventListener("click", () => {
    runAction("Resetting interleave", () => request("/api/execution/reset", { method: "POST" }));
  });

  document.getElementById("step").addEventListener("click", () => {
    runAction("Stepping", () => request("/api/execution/step", { method: "POST" }));
  });

  elements.foldButton.addEventListener("click", () => {
    runAction("Folding", () => request("/api/fold", { method: "POST" }));
  });

  elements.applyTransactionButton.addEventListener("click", () => {
    runAction("Applying transaction", async () => {
      const response = await request("/api/apply-transaction", { method: "POST" });
      if (response?.ok) {
        elements.ledgerBeforeDetails.open = true;
        elements.ledgerAfterDetails.open = true;
      }
      return response;
    });
  });

  elements.applyContracts.addEventListener("click", () => {
    applyContractsNow();
  });

  elements.applyInitialStates.addEventListener("click", () => {
    runAction("Applying initial state", () =>
      request("/api/initial-states", {
        method: "PUT",
        body: JSON.stringify({
          utxo_a: Number(elements.initialStateUtxoA.value || 0),
          utxo_b: Number(elements.initialStateUtxoB.value || 0),
        }),
      }),
    );
  });

  for (const input of [elements.initialStateUtxoA, elements.initialStateUtxoB]) {
    input.addEventListener("input", () => {
      renderInitialStateStatus();
    });
  }

  for (const tab of elements.contractTabs) {
    tab.addEventListener("click", () => {
      syncActiveEditor(state.activeContract);
      state.activeContract = tab.dataset.contract;
      render();
    });
  }

  document.addEventListener("mouseover", (event) => {
    const target = event.target.closest(".trace-commitment-info");
    if (!target) {
      return;
    }
    showTooltip(target.dataset.tooltip || "", event.clientX, event.clientY);
  });

  document.addEventListener("mousemove", (event) => {
    const target = event.target.closest(".trace-commitment-info");
    if (!target || !elements.floatingTooltip.classList.contains("visible")) {
      return;
    }
    showTooltip(target.dataset.tooltip || "", event.clientX, event.clientY);
  });

  document.addEventListener("mouseout", (event) => {
    if (event.target.closest(".trace-commitment-info")) {
      hideTooltip();
    }
  });

  document.addEventListener("scroll", hideTooltip, true);
  window.addEventListener("resize", layoutEditors);
}

async function boot() {
  bindEvents();
  try {
    clearAllDrafts();
    await initMonaco();
    await request("/api/compile", { method: "POST" });
    await loadEditorFiles();
    await refreshSnapshot();
  } catch (error) {
    console.error(error);
  }
}

boot();
