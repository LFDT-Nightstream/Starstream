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

use miette::{Diagnostic, NamedSource};
use starstream_types::{
    DUMMY_SPAN, FileSystem, Span,
    ast::{Definition, ImportSource, Program},
};

use crate::parser::{self, ParseError};

/// Stable identifier for a module within a `ModuleGraph`.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ModuleId(pub u32);

impl ModuleId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Debug for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Always put on one line, ignoring alternate formatting.
        write!(f, "ModuleId({})", self.0)
    }
}

/// One parsed `.star` file.
pub struct Module {
    pub id: ModuleId,
    /// Canonical absolute path on disk.
    pub abs_path: PathBuf,
    pub source: Arc<str>,
    pub contents: ModuleContents,
}

pub enum ModuleContents {
    Empty,
    Starstream(Program),
    Wasm(Vec<u8>),
}

impl Module {
    /// True if this module's top-level definitions include `contract;`.
    pub fn declares_contract(&self) -> bool {
        matches!(&self.contents, ModuleContents::Starstream(program) if program
            .definitions
            .iter()
            .any(|d| matches!(d.node, Definition::Contract)))
    }

    pub fn to_named_source(&self) -> NamedSource<Arc<str>> {
        NamedSource::new(self.abs_path.to_string_lossy(), self.source.clone())
            .with_language("starstream")
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

    pub fn source(&self, id: ModuleId) -> NamedSource<Arc<str>> {
        self.modules[id.0 as usize].to_named_source()
    }
}

impl std::fmt::Debug for ModuleGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleGraph")
            .field("modules.len()", &self.modules.len())
            .field("topo_order", &self.topo_order)
            .field("edges", &self.edges)
            .field("contract_entries", &self.contract_entries)
            .finish()
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
    /// Path import has unknown extension.
    UnknownExtension {
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
    Parse {
        source: NamedSource<Arc<str>>,
        error: ParseError,
    },
}

impl std::fmt::Display for ModuleGraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleGraphError::EntryIo { path, error } => {
                write!(f, "error: failed to read `{}`: {}", path.display(), error)
            }
            ModuleGraphError::ImportIo { path, error, .. } => {
                write!(
                    f,
                    "error: failed to resolve path import `{}`: {}",
                    path.display(),
                    error
                )
            }
            ModuleGraphError::NonRelativePath { path, .. } => {
                write!(
                    f,
                    "error: path import `{path}` must start with `./` or `../`"
                )
            }
            ModuleGraphError::UnknownExtension { path, .. } => {
                write!(
                    f,
                    "error: path import `{path}` has unknown extension, expecting `.star`"
                )
            }
            ModuleGraphError::CrossContractImport {
                importer_path,
                target_path,
                ..
            } => {
                write!(
                    f,
                    "error: cross-contract calls not supported yet: `{}` imports `{}`, which also declares `contract;`",
                    importer_path.display(),
                    target_path.display()
                )
            }
            ModuleGraphError::Cycle { chain } => {
                write!(f, "error: cyclic path import detected:")?;
                for (_id, p, _span) in chain {
                    write!(f, "\n  - {}", p.display())?;
                }
                Ok(())
            }
            ModuleGraphError::Parse { .. } => write!(f, "parse error"),
        }
    }
}

impl std::error::Error for ModuleGraphError {}

impl Diagnostic for ModuleGraphError {
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        match self {
            ModuleGraphError::Parse { error, .. } => Some(error),
            _ => None,
        }
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        match self {
            ModuleGraphError::Parse { source, .. } => Some(source),
            _ => None,
        }
    }
}

/// Build a graph rooted at `entry` for the single-file `wasm -c` flow.
///
/// The entry point is treated as a contract even if it doesn't start with a
/// `contract;` item.
pub fn load_from_entry(
    entry: &Path,
    fs: &mut FileSystem,
) -> Result<ModuleGraph, Vec<ModuleGraphError>> {
    let canonical_entry = std::fs::canonicalize(entry).map_err(|error| {
        vec![ModuleGraphError::EntryIo {
            path: entry.to_path_buf(),
            error,
        }]
    })?;

    let mut builder = Builder::new(fs);
    let entry_id = builder.parse_module(&canonical_entry).map_err(|error| {
        vec![ModuleGraphError::EntryIo {
            path: entry.to_path_buf(),
            error,
        }]
    })?;
    builder.finish(Some(entry_id))
}

/// Build a workspace graph by recursively scanning `scan_dir` for `.star`
/// files, then resolving every path import they declare (which may pull in
/// files outside `scan_dir`).
///
/// Every scanned file is included in the graph.
/// `.star` files declaring `contract;` become codegen entry points.
pub fn load_workspace(
    scan_dir: &Path,
    fs: &mut FileSystem,
) -> Result<ModuleGraph, Vec<ModuleGraphError>> {
    let mut builder = Builder::new(fs);

    // Seed the graph with every `.star` file under scan_dir.
    let star_files = collect_star_files(scan_dir);
    for path in &star_files {
        match std::fs::canonicalize(path) {
            Err(error) => {
                builder.errors.push(ModuleGraphError::EntryIo {
                    path: path.clone(),
                    error,
                });
            }
            Ok(canonical) => {
                if let Err(error) = builder.parse_module(&canonical) {
                    builder.errors.push(ModuleGraphError::EntryIo {
                        path: path.clone(),
                        error,
                    });
                }
            }
        }
    }

    builder.finish(None)
}

fn collect_star_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(dir, &mut |path| {
        if path.extension().is_some_and(|e| e == "star") {
            out.push(path.to_path_buf());
        }
    });
    out
}

fn walk(dir: &Path, visit: &mut impl FnMut(&Path)) {
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
    errors: Vec<ModuleGraphError>,
    fs: &'a mut FileSystem,
}

impl<'a> Builder<'a> {
    fn new(fs: &'a mut FileSystem) -> Self {
        Self {
            modules: Vec::new(),
            edges: HashMap::new(),
            by_path: HashMap::new(),
            errors: Vec::new(),
            fs,
        }
    }

    fn finish(
        mut self,
        force_entry: Option<ModuleId>,
    ) -> Result<ModuleGraph, Vec<ModuleGraphError>> {
        // Resolve imports transitively.
        let mut next = 0;
        while next < self.modules.len() {
            self.resolve_imports(ModuleId(next as u32));
            next += 1;
        }

        if let Err(err) = self.validate_cross_contract(force_entry) {
            self.errors.push(err);
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        // Contract entries = every node that declares `contract;`.
        let contract_entries: Vec<ModuleId> = match force_entry {
            Some(entry) => vec![entry],
            None => self
                .modules
                .iter()
                .filter(|m| m.declares_contract())
                .map(|m| m.id)
                .collect(),
        };

        // Topo sort starting from contract entries first (so the meaningful
        // codegen roots get walked first), then sweep in any unreached loose
        // nodes so every module ends up in the order.
        let topo_order = self.topo_order(&contract_entries).map_err(|e| vec![e])?;

        Ok(ModuleGraph {
            modules: self.modules,
            topo_order,
            edges: self.edges,
            contract_entries,
        })
    }

    fn parse_module(&mut self, abs_path: &Path) -> std::io::Result<ModuleId> {
        if let Some(&id) = self.by_path.get(abs_path) {
            return Ok(id);
        }
        let idx = self.modules.len();
        let id = ModuleId(idx as u32);
        self.by_path.insert(abs_path.to_path_buf(), id);
        self.modules.push(Module {
            id,
            abs_path: abs_path.to_path_buf(),
            source: Default::default(),
            contents: ModuleContents::Empty,
        });

        match abs_path.extension().and_then(|x| x.to_str()) {
            Some("star") => {
                self.parse_star_module(abs_path, idx)?;
            }
            Some("wasm") => {
                self.parse_wasm_module(abs_path, idx)?;
            }
            _ => {}
        }

        Ok(id)
    }

    fn parse_star_module(&mut self, abs_path: &Path, idx: usize) -> std::io::Result<()> {
        let source = self.fs.read_to_string(abs_path)?;
        let parse_output = parser::parse_program(&source);
        let source = Arc::<str>::from(source);
        self.modules[idx].source = source.clone();
        if let Some(program) = parse_output.program {
            self.modules[idx].contents = ModuleContents::Starstream(program);
        }
        self.errors.extend(
            parse_output
                .errors
                .into_iter()
                .map(|error| ModuleGraphError::Parse {
                    source: NamedSource::new(abs_path.to_string_lossy(), source.clone()),
                    error,
                }),
        );
        Ok(())
    }

    fn parse_wasm_module(&mut self, abs_path: &Path, idx: usize) -> std::io::Result<()> {
        let source = self.fs.read(abs_path)?;
        // NOTE: currently assumes that imported .wasm files cannot themselves
        // contain relevant imports. If that changes, they need to be parsed
        // here so those imports can be resolved.
        self.modules[idx].contents = ModuleContents::Wasm(source);
        Ok(())
    }

    fn resolve_imports(&mut self, id: ModuleId) {
        let importer_dir = self.modules[id.index()]
            .abs_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));

        let ModuleContents::Starstream(program) = &self.modules[id.index()].contents else {
            return;
        };

        let raw_imports: Vec<(usize, String, Span)> = program
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
                self.errors.push(ModuleGraphError::NonRelativePath {
                    path: raw_path,
                    importer: id,
                    span,
                });
                continue;
            }
            let candidate = importer_dir.join(&raw_path);
            let abs_path = match std::fs::canonicalize(&candidate) {
                Ok(c) => c,
                Err(error) => {
                    self.errors.push(ModuleGraphError::ImportIo {
                        path: candidate.clone(),
                        importer: id,
                        span,
                        error,
                    });
                    continue;
                }
            };
            match self.parse_module(&abs_path) {
                Ok(target) => {
                    resolved.push(PathImport {
                        def_index,
                        target,
                        span,
                    });
                }
                Err(error) => {
                    self.errors.push(ModuleGraphError::ImportIo {
                        path: abs_path.to_owned(),
                        importer: id,
                        span,
                        error,
                    });
                }
            }
        }

        if !resolved.is_empty() {
            self.edges.insert(id.0, resolved);
        }
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
        match load_from_entry(&entry, &mut fs)
            .err()
            .unwrap_or_default()
            .as_slice()
        {
            [ModuleGraphError::CrossContractImport { .. }] => {}
            other => panic!("expected CrossContractImport, got {:?}", other),
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
        match load_workspace(&dir, &mut fs)
            .err()
            .unwrap_or_default()
            .as_slice()
        {
            [ModuleGraphError::CrossContractImport { .. }] => {}
            other => panic!("expected CrossContractImport, got {:?}", other),
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
        match load_workspace(&dir, &mut fs)
            .err()
            .unwrap_or_default()
            .as_slice()
        {
            [ModuleGraphError::Cycle { chain }] => assert!(chain.len() >= 2),
            other => panic!("expected cycle, got {:?}", other),
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
