//! Build a topologically sorted graph of `.star` files.
//!
//! Two entry points:
//!   - [`load_from_entry`] for the single-file flow (`starstream wasm -c <file>`).
//!     The given file is treated as a contract regardless of header; its
//!     transitive imports populate the graph.
//!   - [`load_workspace`] for the scan-based commands (`check`, `docs`,
//!     `build`) and the language server. Walks a directory for every
//!     `.star` file, follows imports out of the scan dir as needed, and
//!     enforces the cross-contract guard on every edge.
//!
//! In both modes the resulting [`ModuleGraph`] holds *all* modules once
//! (deduped by canonical path), the complete topological order, and a list
//! of nodes that declare `contract;` — those become codegen entries.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use starstream_types::{
    DUMMY_SPAN, FileSystem, Span,
    ast::{Definition, ImportSource, Program},
};

use crate::parser::{self, ParseError};

/// Stable identifier for a module within a `ModuleGraph`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ModuleId(pub u32);

impl ModuleId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// One parsed `.star` file.
pub struct Module {
    pub id: ModuleId,
    /// Canonical absolute path on disk.
    pub abs_path: PathBuf,
    pub source: Arc<str>,
    pub program: Program,
}

impl Module {
    /// True if this module's top-level definitions include `contract;`.
    pub fn declares_contract(&self) -> bool {
        self.program
            .definitions
            .iter()
            .any(|d| matches!(d.node, Definition::Contract))
    }
}

/// A resolved `import { ... } from "..."` edge from one module to another.
#[derive(Clone, Debug)]
pub struct PathImport {
    /// Index of the import statement in the importer's `program.definitions`.
    pub def_index: usize,
    /// The module the path resolves to.
    pub target: ModuleId,
    /// Span of the path string literal in the importer's source.
    pub span: Span,
}

pub struct ModuleGraph {
    modules: Vec<Module>,
    topo_order: Vec<ModuleId>,
    edges: HashMap<u32, Vec<PathImport>>,
    /// Codegen entries — nodes whose source declares `contract;`. The
    /// single-file `load_from_entry` always populates this with exactly the
    /// entry file (even if the file doesn't actually declare `contract;`).
    contract_entries: Vec<ModuleId>,
}

impl ModuleGraph {
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }
    pub fn module(&self, id: ModuleId) -> &Module {
        &self.modules[id.index()]
    }
    pub fn topo_order(&self) -> &[ModuleId] {
        &self.topo_order
    }
    pub fn edges_of(&self, id: ModuleId) -> &[PathImport] {
        self.edges.get(&id.0).map(|v| v.as_slice()).unwrap_or(&[])
    }
    pub fn contract_entries(&self) -> &[ModuleId] {
        &self.contract_entries
    }
    /// Look up a module by its canonical absolute path.
    pub fn find_by_path(&self, abs_path: &Path) -> Option<ModuleId> {
        self.modules
            .iter()
            .find(|m| m.abs_path == abs_path)
            .map(|m| m.id)
    }

    /// IDs of every module reachable from `start` via outgoing path-import
    /// edges (inclusive). Used by `compile_contract` to walk a contract's
    /// subgraph without dragging in unrelated modules.
    pub fn reachable_from(&self, start: ModuleId) -> Vec<ModuleId> {
        let mut visited = vec![false; self.modules.len()];
        let mut order = Vec::new();
        let mut stack = vec![start];
        while let Some(id) = stack.pop() {
            if visited[id.index()] {
                continue;
            }
            visited[id.index()] = true;
            order.push(id);
            for edge in self.edges_of(id) {
                if !visited[edge.target.index()] {
                    stack.push(edge.target);
                }
            }
        }
        order
    }
}

#[derive(Debug)]
pub enum ModuleGraphError {
    /// The entry file itself couldn't be read.
    EntryIo {
        path: PathBuf,
        error: std::io::Error,
    },
    /// A path import targets a file we couldn't read.
    ImportIo {
        path: PathBuf,
        importer: ModuleId,
        span: Span,
        error: std::io::Error,
    },
    /// Path import isn't relative (must start with `./` or `../`).
    NonRelativePath {
        path: String,
        importer: ModuleId,
        span: Span,
    },
    /// Path import doesn't end with `.star`.
    NotStarExtension {
        path: String,
        importer: ModuleId,
        span: Span,
    },
    /// An edge in the graph points at a file that also declares `contract;`.
    /// Cross-contract imports aren't supported yet.
    CrossContractImport {
        importer: ModuleId,
        importer_path: PathBuf,
        target: ModuleId,
        target_path: PathBuf,
        span: Span,
    },
    /// Topo sort found a cycle.
    Cycle {
        chain: Vec<(ModuleId, PathBuf, Span)>,
    },
    /// One or more modules failed to parse.
    ParseFailed {
        failures: Vec<(ModuleId, Vec<ParseError>)>,
    },
}

/// Build a graph rooted at `entry` for the single-file `wasm -c` flow.
///
/// The entry is always treated as a contract — its `contract_entries` list
/// is just `[entry]` regardless of whether the file declares `contract;`.
/// Any *imported* helper that declares `contract;` triggers the
/// cross-contract guard.
pub fn load_from_entry(entry: &Path, fs: &mut FileSystem) -> Result<ModuleGraph, ModuleGraphError> {
    let canonical_entry =
        std::fs::canonicalize(entry).map_err(|error| ModuleGraphError::EntryIo {
            path: entry.to_path_buf(),
            error,
        })?;

    let mut builder = Builder::new(fs);
    let entry_id =
        builder
            .parse_module(&canonical_entry)
            .map_err(|error| ModuleGraphError::EntryIo {
                path: entry.to_path_buf(),
                error,
            })?;

    // Resolve imports transitively.
    let mut next = 0;
    while next < builder.modules.len() {
        builder.resolve_imports(ModuleId(next as u32))?;
        next += 1;
    }

    builder.validate_cross_contract(Some(entry_id))?;

    if !builder.parse_failures.is_empty() {
        return Err(ModuleGraphError::ParseFailed {
            failures: builder.parse_failures,
        });
    }

    let topo_order = builder.topo_order(&[entry_id])?;

    Ok(ModuleGraph {
        modules: builder.modules,
        topo_order,
        edges: builder.edges,
        contract_entries: vec![entry_id],
    })
}

/// Build a workspace graph by recursively scanning `scan_dir` for `.star`
/// files, then resolving every path import they declare (which may pull in
/// files outside `scan_dir`).
///
/// All scanned + transitively-imported modules end up in the graph. Every
/// file that declares `contract;` becomes a codegen entry. The
/// cross-contract guard is enforced per edge: any import edge whose target
/// declares `contract;` is rejected.
pub fn load_workspace(
    scan_dir: &Path,
    fs: &mut FileSystem,
) -> Result<ModuleGraph, ModuleGraphError> {
    let mut builder = Builder::new(fs);

    // Seed the graph with every `.star` file under scan_dir.
    let star_files = collect_star_files(scan_dir);
    for path in &star_files {
        let canonical = std::fs::canonicalize(path).map_err(|error| ModuleGraphError::EntryIo {
            path: path.clone(),
            error,
        })?;
        builder
            .parse_module(&canonical)
            .map_err(|error| ModuleGraphError::EntryIo {
                path: path.clone(),
                error,
            })?;
    }

    // Resolve imports transitively. This may add nodes outside scan_dir.
    let mut next = 0;
    while next < builder.modules.len() {
        builder.resolve_imports(ModuleId(next as u32))?;
        next += 1;
    }

    builder.validate_cross_contract(None)?;

    if !builder.parse_failures.is_empty() {
        return Err(ModuleGraphError::ParseFailed {
            failures: builder.parse_failures,
        });
    }

    // Contract entries = every node that declares `contract;`.
    let contract_entries: Vec<ModuleId> = builder
        .modules
        .iter()
        .filter(|m| m.declares_contract())
        .map(|m| m.id)
        .collect();

    // Topo sort starting from contract entries first (so the meaningful
    // codegen roots get walked first), then sweep in any unreached loose
    // nodes so every module ends up in the order.
    let topo_order = builder.topo_order(&contract_entries)?;

    Ok(ModuleGraph {
        modules: builder.modules,
        topo_order,
        edges: builder.edges,
        contract_entries,
    })
}

fn collect_star_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(dir, &mut |path| {
        if path.extension().map(|e| e == "star").unwrap_or(false) {
            out.push(path.to_path_buf());
        }
    });
    out
}

fn walk(dir: &Path, visit: &mut dyn FnMut(&Path)) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        // Skip dot-dirs (.git, .vscode, ...), `target/`, `artifacts/`.
        if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.starts_with('.') || n == "target" || n == "artifacts")
            .unwrap_or(false)
        {
            continue;
        }
        if path.is_dir() {
            walk(&path, visit);
        } else if path.is_file() {
            visit(&path);
        }
    }
}

struct Builder<'a> {
    modules: Vec<Module>,
    edges: HashMap<u32, Vec<PathImport>>,
    by_path: HashMap<PathBuf, ModuleId>,
    parse_failures: Vec<(ModuleId, Vec<ParseError>)>,
    fs: &'a mut FileSystem,
}

impl<'a> Builder<'a> {
    fn new(fs: &'a mut FileSystem) -> Self {
        Self {
            modules: Vec::new(),
            edges: HashMap::new(),
            by_path: HashMap::new(),
            parse_failures: Vec::new(),
            fs,
        }
    }

    fn parse_module(&mut self, abs_path: &Path) -> std::io::Result<ModuleId> {
        if let Some(id) = self.by_path.get(abs_path) {
            return Ok(*id);
        }

        let source = self.fs.read_to_string(abs_path)?;
        let parse_output = parser::parse_program(&source);

        let id = ModuleId(self.modules.len() as u32);
        let program = parse_output.program.unwrap_or_else(|| Program {
            shebang: None,
            definitions: Vec::new(),
        });

        self.modules.push(Module {
            id,
            abs_path: abs_path.to_path_buf(),
            source: Arc::from(source.into_boxed_str()),
            program,
        });
        self.by_path.insert(abs_path.to_path_buf(), id);

        if !parse_output.errors.is_empty() {
            self.parse_failures.push((id, parse_output.errors));
        }

        Ok(id)
    }

    fn resolve_imports(&mut self, id: ModuleId) -> Result<(), ModuleGraphError> {
        let importer_dir = self.modules[id.index()]
            .abs_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));

        let raw_imports: Vec<(usize, String, Span)> = self.modules[id.index()]
            .program
            .definitions
            .iter()
            .enumerate()
            .filter_map(|(idx, def)| match &def.node {
                Definition::Import(import) => match &import.from {
                    ImportSource::Path(path) => Some((idx, path.value.clone(), path.span)),
                    _ => None,
                },
                _ => None,
            })
            .collect();

        let mut resolved = Vec::with_capacity(raw_imports.len());
        for (def_index, raw_path, span) in raw_imports {
            if !is_relative_path(&raw_path) {
                return Err(ModuleGraphError::NonRelativePath {
                    path: raw_path,
                    importer: id,
                    span,
                });
            }
            if !raw_path.ends_with(".star") {
                return Err(ModuleGraphError::NotStarExtension {
                    path: raw_path,
                    importer: id,
                    span,
                });
            }

            let candidate = importer_dir.join(&raw_path);
            let canonical =
                std::fs::canonicalize(&candidate).map_err(|error| ModuleGraphError::ImportIo {
                    path: candidate.clone(),
                    importer: id,
                    span,
                    error,
                })?;

            let target =
                self.parse_module(&canonical)
                    .map_err(|error| ModuleGraphError::ImportIo {
                        path: canonical.clone(),
                        importer: id,
                        span,
                        error,
                    })?;

            resolved.push(PathImport {
                def_index,
                target,
                span,
            });
        }

        if !resolved.is_empty() {
            self.edges.insert(id.0, resolved);
        }
        Ok(())
    }

    /// Reject any edge whose target declares `contract;`.
    ///
    /// `allow_target` is set in the single-file flow: when callers point at
    /// a contract file via `wasm -c <contract.star>` the entry file itself
    /// is allowed to declare `contract;`, but other files pulled in through
    /// imports still can't.
    fn validate_cross_contract(
        &self,
        allow_target: Option<ModuleId>,
    ) -> Result<(), ModuleGraphError> {
        for (importer_raw, edges) in &self.edges {
            let importer = ModuleId(*importer_raw);
            for edge in edges {
                if Some(edge.target) == allow_target {
                    continue;
                }
                if self.modules[edge.target.index()].declares_contract() {
                    return Err(ModuleGraphError::CrossContractImport {
                        importer,
                        importer_path: self.modules[importer.index()].abs_path.clone(),
                        target: edge.target,
                        target_path: self.modules[edge.target.index()].abs_path.clone(),
                        span: edge.span,
                    });
                }
            }
        }
        Ok(())
    }

    /// Iterative DFS that returns a topological order over **every** module
    /// in the graph (dependencies first), seeded from `seeds` and then
    /// sweeping in any modules not yet visited. Reports cycles with the
    /// import spans that closed them.
    fn topo_order(&self, seeds: &[ModuleId]) -> Result<Vec<ModuleId>, ModuleGraphError> {
        let n = self.modules.len();
        let mut color = vec![DfsColor::White; n];
        let mut order = Vec::with_capacity(n);

        for seed in seeds
            .iter()
            .copied()
            .chain((0..n).map(|i| ModuleId(i as u32)))
        {
            if color[seed.index()] != DfsColor::White {
                continue;
            }
            self.dfs_visit(seed, &mut color, &mut order)?;
        }

        Ok(order)
    }

    fn dfs_visit(
        &self,
        start: ModuleId,
        color: &mut [DfsColor],
        order: &mut Vec<ModuleId>,
    ) -> Result<(), ModuleGraphError> {
        let mut stack: Vec<(ModuleId, usize)> = Vec::new();
        let mut path_stack: Vec<(ModuleId, Span)> = Vec::new();

        stack.push((start, 0));
        color[start.index()] = DfsColor::Gray;
        path_stack.push((start, DUMMY_SPAN));

        while let Some(&(node, child_idx)) = stack.last() {
            let edges = self.edges.get(&node.0).map(|v| v.as_slice()).unwrap_or(&[]);

            if child_idx >= edges.len() {
                color[node.index()] = DfsColor::Black;
                order.push(node);
                stack.pop();
                path_stack.pop();
                continue;
            }

            let edge = &edges[child_idx];
            let last = stack.len() - 1;
            stack[last].1 = child_idx + 1;

            match color[edge.target.index()] {
                DfsColor::White => {
                    color[edge.target.index()] = DfsColor::Gray;
                    path_stack.push((edge.target, edge.span));
                    stack.push((edge.target, 0));
                }
                DfsColor::Gray => {
                    let mut chain: Vec<(ModuleId, PathBuf, Span)> = Vec::new();
                    let mut found = false;
                    for &(mid, span) in &path_stack {
                        if mid == edge.target {
                            found = true;
                        }
                        if found {
                            chain.push((mid, self.modules[mid.index()].abs_path.clone(), span));
                        }
                    }
                    chain.push((
                        edge.target,
                        self.modules[edge.target.index()].abs_path.clone(),
                        edge.span,
                    ));
                    return Err(ModuleGraphError::Cycle { chain });
                }
                DfsColor::Black => {}
            }
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum DfsColor {
    White,
    Gray,
    Black,
}

fn is_relative_path(s: &str) -> bool {
    s.starts_with("./") || s.starts_with("../")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;

    fn tmp_dir(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "starstream-modgraph-{}-{}",
            name,
            std::process::id()
        ));
        let _ = fs::remove_dir_all(&path);
        fs::create_dir_all(&path).unwrap();
        path
    }

    fn write_file(dir: &Path, name: &str, contents: &str) -> PathBuf {
        let path = dir.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        let mut f = fs::File::create(&path).unwrap();
        f.write_all(contents.as_bytes()).unwrap();
        path
    }

    #[test]
    fn single_file_simple_chain() {
        let dir = tmp_dir("simple");
        write_file(
            &dir,
            "helpers/math.star",
            "fn add(a: i64, b: i64) -> i64 { a + b }\n",
        );
        let entry = write_file(
            &dir,
            "entry.star",
            "contract;\nimport { add } from \"./helpers/math.star\";\nfn main() { }\n",
        );

        let mut fs = FileSystem::new();
        let graph = load_from_entry(&entry, &mut fs).unwrap();
        assert_eq!(graph.modules().len(), 2);
        assert_eq!(graph.contract_entries().len(), 1);
    }

    #[test]
    fn single_file_cross_contract_rejected() {
        let dir = tmp_dir("xc-single");
        write_file(&dir, "other.star", "contract;\nfn other() { }\n");
        let entry = write_file(
            &dir,
            "main.star",
            "contract;\nimport { other } from \"./other.star\";\nfn main() { }\n",
        );

        let mut fs = FileSystem::new();
        match load_from_entry(&entry, &mut fs) {
            Err(ModuleGraphError::CrossContractImport { .. }) => {}
            other => panic!("expected CrossContractImport, got {:?}", other.map(|_| ())),
        }
    }

    #[test]
    fn workspace_deduped_shared_helper() {
        let dir = tmp_dir("ws-shared");
        write_file(&dir, "helper.star", "fn util() -> i64 { 9 }\n");
        write_file(
            &dir,
            "a.star",
            "contract;\nimport { util } from \"./helper.star\";\nscript fn run() -> i64 { util() }\n",
        );
        write_file(
            &dir,
            "b.star",
            "contract;\nimport { util } from \"./helper.star\";\nscript fn run() -> i64 { util() }\n",
        );

        let mut fs = FileSystem::new();
        let graph = load_workspace(&dir, &mut fs).unwrap();
        // helper + a + b = 3 nodes total; helper is shared.
        assert_eq!(graph.modules().len(), 3);
        assert_eq!(graph.contract_entries().len(), 2);
    }

    #[test]
    fn workspace_orphan_helper_loose_node() {
        let dir = tmp_dir("ws-orphan");
        write_file(&dir, "orphan.star", "fn unused() { }\n");
        write_file(&dir, "main.star", "contract;\nfn main() { }\n");

        let mut fs = FileSystem::new();
        let graph = load_workspace(&dir, &mut fs).unwrap();
        assert_eq!(graph.modules().len(), 2);
        assert_eq!(graph.contract_entries().len(), 1);
        // orphan.star is in the graph but has no contract;
        assert!(
            graph
                .modules()
                .iter()
                .any(|m| m.abs_path.ends_with("orphan.star") && !m.declares_contract())
        );
    }

    #[test]
    fn workspace_cross_contract_rejected() {
        let dir = tmp_dir("ws-xc");
        write_file(&dir, "other.star", "contract;\nfn other() { }\n");
        write_file(
            &dir,
            "main.star",
            "contract;\nimport { other } from \"./other.star\";\nfn main() { }\n",
        );

        let mut fs = FileSystem::new();
        match load_workspace(&dir, &mut fs) {
            Err(ModuleGraphError::CrossContractImport { .. }) => {}
            other => panic!("expected CrossContractImport, got {:?}", other.map(|_| ())),
        }
    }

    #[test]
    fn workspace_cycle_detected() {
        let dir = tmp_dir("ws-cycle");
        // Two helpers that import each other; a third file is the contract
        // that pulls them in. The cycle is between the helpers.
        write_file(
            &dir,
            "a.star",
            "import { b } from \"./b.star\";\nfn a() { }\n",
        );
        write_file(
            &dir,
            "b.star",
            "import { a } from \"./a.star\";\nfn b() { }\n",
        );
        write_file(
            &dir,
            "main.star",
            "contract;\nimport { a } from \"./a.star\";\nfn main() { }\n",
        );

        let mut fs = FileSystem::new();
        match load_workspace(&dir, &mut fs) {
            Err(ModuleGraphError::Cycle { chain }) => assert!(chain.len() >= 2),
            other => panic!("expected cycle, got {:?}", other.map(|_| ())),
        }
    }

    #[test]
    fn reachable_from_walks_subgraph() {
        let dir = tmp_dir("reach");
        write_file(&dir, "shared.star", "fn shared() { }\n");
        let a = write_file(
            &dir,
            "a.star",
            "contract;\nimport { shared } from \"./shared.star\";\nfn a() { }\n",
        );
        write_file(&dir, "b.star", "contract;\nfn b() { }\n");

        let mut fs = FileSystem::new();
        let graph = load_workspace(&dir, &mut fs).unwrap();
        let a_id = graph
            .find_by_path(&std::fs::canonicalize(&a).unwrap())
            .unwrap();
        let reachable = graph.reachable_from(a_id);
        // a + shared = 2; b is unreachable from a.
        assert_eq!(reachable.len(), 2);
    }
}
