use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

use nexc_ast::{ImportDecl, ImportKind, Item, SourceFile, Visibility};
use nexc_diag::{Diagnostic, DiagnosticSink, Severity};

#[derive(Debug)]
pub struct ModuleGraph {
    pub imports: HashMap<String, Vec<String>>,
    pub declarations: HashMap<String, SourceFile>,
    pub dependency_order: Vec<String>,
    pub lib_names: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct LibDependency {
    pub name: String,
    pub root: PathBuf,
    /// Path to a native dynamic library (.dll / .so) if specified in project.toml.
    pub native_lib: Option<PathBuf>,
    /// Semver version string (e.g. "0.1.0") when resolved from the global cache.
    pub version: Option<String>,
    /// GitHub source in "user/repo" format, recorded for reinstallation.
    pub git: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ProjectLayout {
    pub root: PathBuf,
    pub src_dir: PathBuf,
    pub libs: Vec<LibDependency>,
}

pub fn build_module_graph(files: &[SourceFile], sink: &mut DiagnosticSink) -> ModuleGraph {
    build_module_graph_with_libs(files, &HashSet::new(), sink)
}

pub fn build_module_graph_with_libs(
    files: &[SourceFile],
    lib_names: &HashSet<String>,
    sink: &mut DiagnosticSink,
) -> ModuleGraph {
    let mut graph = ModuleGraph {
        imports: HashMap::new(),
        declarations: HashMap::new(),
        dependency_order: Vec::new(),
        lib_names: lib_names.clone(),
    };

    for file in files {
        let module = canonical_module_name(&file.path);
        if graph
            .declarations
            .insert(module.clone(), file.clone())
            .is_some()
        {
            sink.push(Diagnostic {
                id: "module_duplicate".to_string(),
                severity: Severity::Error,
                span: Some(file.span),
                file: Some(PathBuf::from(file.path.clone())),
                message: format!("duplicate module declaration `{module}`"),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }

    for (module, file) in &graph.declarations {
        let mut deps = Vec::new();
        for item in &file.items {
            if let Item::Import(ImportDecl { path, .. }) = item {
                deps.push(path.join("."));
            }
        }
        graph.imports.insert(module.clone(), deps);
    }

    validate_imports(&graph, sink);
    detect_cycles(&graph, sink);
    graph.dependency_order = topological_order(&graph);
    graph
}

/// Merge partial class declarations within a single source file.
/// All `partial class Foo` items with the same name are combined into one `ClassDecl`.
pub fn merge_partial_classes(file: &mut SourceFile, _sink: &mut DiagnosticSink) {
    use std::collections::BTreeMap;
    let mut partial_indices: BTreeMap<String, Vec<usize>> = BTreeMap::new();

    for (i, item) in file.items.iter().enumerate() {
        if let Item::Class(cls) = item {
            if cls.is_partial {
                partial_indices.entry(cls.name.clone()).or_default().push(i);
            }
        }
    }

    let mut remove_set: HashSet<usize> = HashSet::new();
    for (name, indices) in &partial_indices {
        if indices.len() < 2 {
            continue;
        }
        let keep = indices[0];
        for &merge_idx in &indices[1..] {
            let donor = if let Item::Class(c) = &file.items[merge_idx] {
                c.clone()
            } else {
                continue;
            };
            if let Item::Class(target) = &mut file.items[keep] {
                for bs in &donor.base_specs {
                    if !target.base_specs.iter().any(|b| b.name == bs.name) {
                        target.base_specs.push(bs.clone());
                    }
                }
                target.fields.extend(donor.fields);
                target.methods.extend(donor.methods);
                if matches!(target.visibility, Visibility::Internal)
                    && matches!(donor.visibility, Visibility::Public)
                {
                    target.visibility = Visibility::Public;
                }
            }
            remove_set.insert(merge_idx);
        }
        if partial_indices[name].len() >= 2 {
            if let Item::Class(c) = &mut file.items[keep] {
                c.is_partial = false;
            }
        }
    }

    if !remove_set.is_empty() {
        let mut idx = 0;
        file.items.retain(|_| {
            let keep = !remove_set.contains(&idx);
            idx += 1;
            keep
        });
    }
}

pub fn resolve_module(file: &SourceFile, graph: &ModuleGraph) -> SourceFile {
    let mut resolved = file.clone();
    for item in &mut resolved.items {
        if let Item::Import(import) = item {
            let dep = import.path.join(".");
            import.synthetic = !(is_external_module(&dep, &graph.lib_names) || graph.declarations.contains_key(&dep));
        }
    }
    resolved
}

pub fn build_symbol_table(file: &SourceFile, sink: &mut DiagnosticSink) -> HashMap<String, Visibility> {
    let mut symbols = HashMap::new();
    for item in &file.items {
        let Some(name) = item_name(item) else {
            continue;
        };
        let vis = item_visibility(item);
        if symbols.insert(name.clone(), vis).is_some() {
            sink.push(Diagnostic {
                id: "resolve_duplicate_symbol".to_string(),
                severity: Severity::Error,
                span: None,
                file: Some(PathBuf::from(file.path.clone())),
                message: format!("duplicate declaration `{name}`"),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }
    symbols
}

pub fn enforce_visibility(file: &SourceFile, sink: &mut DiagnosticSink) {
    for item in &file.items {
        if let Visibility::Public = item_visibility(item) {
            if let Some(name) = item_name(item) {
                if name.starts_with('_') {
                    sink.push(Diagnostic {
                        id: "visibility_internal_name".to_string(),
                        severity: Severity::Warning,
                        span: None,
                        file: Some(file.path.clone().into()),
                        message: format!("public declaration `{name}` begins with '_'"),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                }
            }
        }
    }
}

pub fn discover_project_layout(root: &Path, sink: &mut DiagnosticSink) -> ProjectLayout {
    let project_toml = root.join("project.toml");
    if !project_toml.exists() {
        sink.push(Diagnostic {
            id: "project_missing".to_string(),
            severity: Severity::Error,
            span: None,
            file: Some(project_toml.clone()),
            message: "missing project.toml".to_string(),
            notes: Vec::new(),
            suggestions: vec!["create project.toml at package root".to_string()],
        });
        return ProjectLayout {
            root: root.to_path_buf(),
            src_dir: root.join("src"),
            libs: Vec::new(),
        };
    }

    let text = fs::read_to_string(&project_toml).unwrap_or_default();
    let src_rel = parse_src_setting(&text).unwrap_or_else(|| "src".to_string());
    let src_dir = root.join(&src_rel);
    if !src_dir.exists() {
        sink.push(Diagnostic {
            id: "project_src_missing".to_string(),
            severity: Severity::Error,
            span: None,
            file: Some(src_dir.clone()),
            message: "source directory does not exist".to_string(),
            notes: Vec::new(),
            suggestions: vec!["create the configured source directory".to_string()],
        });
    }

    let libs = parse_libs_section(&text, root, sink);

    ProjectLayout {
        root: root.to_path_buf(),
        src_dir,
        libs,
    }
}

pub fn discover_project_modules(root: &Path, sink: &mut DiagnosticSink) -> HashMap<String, PathBuf> {
    let layout = discover_project_layout(root, sink);
    let mut modules = HashMap::new();
    if !layout.src_dir.exists() {
        return modules;
    }

    let mut files = Vec::new();
    collect_au_files(&layout.src_dir, &mut files, sink);
    for file in files {
        let Some(module) = module_name_from_source_path(&layout.src_dir, &file) else {
            continue;
        };
        if modules.insert(module.clone(), file.clone()).is_some() {
            sink.push(Diagnostic {
                id: "module_duplicate_path".to_string(),
                severity: Severity::Error,
                span: None,
                file: Some(file),
                message: format!("duplicate module path `{module}`"),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }

    for lib in &layout.libs {
        let lib_modules = discover_lib_modules(lib, sink);
        for (name, path) in lib_modules {
            if modules.insert(name.clone(), path.clone()).is_some() {
                sink.push(Diagnostic {
                    id: "module_duplicate_path".to_string(),
                    severity: Severity::Error,
                    span: None,
                    file: Some(path),
                    message: format!("duplicate module path `{name}` (from library `{}`)", lib.name),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
        }
    }

    modules
}

/// Discover all `.nex` files in `base_dir` and its subdirectories.
/// Module names are derived relative to `base_dir` (no `src/` or `project.toml` required).
///
/// The file at `exclude_path` (if provided) is skipped — typically the main source file.
pub fn discover_sibling_modules(
    base_dir: &Path,
    exclude_path: Option<&Path>,
    sink: &mut DiagnosticSink,
) -> HashMap<String, PathBuf> {
    let mut modules = HashMap::new();
    if !base_dir.exists() {
        return modules;
    }

    let mut files = Vec::new();
    collect_au_files(base_dir, &mut files, sink);

    let exclude_canonical = exclude_path.and_then(|p| fs::canonicalize(p).ok());

    for file in files {
        if let Some(ref exc) = exclude_canonical {
            if let Ok(file_canonical) = fs::canonicalize(&file) {
                if file_canonical == *exc {
                    continue;
                }
            }
        }

        let Some(module) = module_name_from_source_path(base_dir, &file) else {
            continue;
        };
        if modules.insert(module.clone(), file.clone()).is_some() {
            sink.push(Diagnostic {
                id: "module_duplicate_path".to_string(),
                severity: Severity::Error,
                span: None,
                file: Some(file),
                message: format!("duplicate module path `{module}`"),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }

    modules
}

/// Returns the library names declared in the project at `root`.
pub fn discover_lib_names(root: &Path, sink: &mut DiagnosticSink) -> Vec<String> {
    let layout = discover_project_layout(root, sink);
    layout.libs.iter().map(|l| l.name.clone()).collect()
}

/// Discover native library paths (.dll / .so) from project dependencies.
pub fn discover_native_libs(root: &Path, sink: &mut DiagnosticSink) -> Vec<PathBuf> {
    let layout = discover_project_layout(root, sink);
    layout
        .libs
        .iter()
        .filter_map(|l| l.native_lib.clone())
        .collect()
}

pub fn discover_lib_modules(
    lib: &LibDependency,
    sink: &mut DiagnosticSink,
) -> HashMap<String, PathBuf> {
    let lib_layout = discover_project_layout(&lib.root, sink);
    let mut modules = HashMap::new();
    if !lib_layout.src_dir.exists() {
        return modules;
    }

    let mut files = Vec::new();
    collect_au_files(&lib_layout.src_dir, &mut files, sink);
    for file in files {
        let Some(local_name) = module_name_from_source_path(&lib_layout.src_dir, &file) else {
            continue;
        };
        let prefixed = format!("{}.{}", lib.name, local_name);
        if modules.insert(prefixed.clone(), file.clone()).is_some() {
            sink.push(Diagnostic {
                id: "module_duplicate_path".to_string(),
                severity: Severity::Error,
                span: None,
                file: Some(file),
                message: format!("duplicate module path `{prefixed}` in library `{}`", lib.name),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }
    modules
}

fn parse_src_setting(toml: &str) -> Option<String> {
    for line in toml.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with("src") {
            continue;
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        if key.trim() != "src" {
            continue;
        }
        let value = value.trim().trim_matches('"').trim_matches('\'');
        if !value.is_empty() {
            return Some(value.to_string());
        }
    }
    None
}

/// Return the global library cache directory: `~/.nex/libs/`.
pub fn nex_libs_dir() -> PathBuf {
    let home = if cfg!(windows) {
        std::env::var("USERPROFILE").unwrap_or_else(|_| {
            std::env::var("HOME").unwrap_or_else(|_| ".".to_string())
        })
    } else {
        std::env::var("HOME").unwrap_or_else(|_| ".".to_string())
    };
    PathBuf::from(home).join(".nex").join("libs")
}

/// Resolve a library from the global cache by name and version.
/// Returns the cached library root path if it exists.
pub fn resolve_from_global_cache(name: &str, version: &str) -> Option<PathBuf> {
    let cache_dir = nex_libs_dir().join(name).join(version);
    if cache_dir.exists() && cache_dir.join("project.toml").exists() {
        Some(cache_dir)
    } else {
        None
    }
}

/// Auto-resolve a library by name alone (no project.toml required).
///
/// Search order:
///   1. Global cache `~/.nex/libs/<name>/` — picks the latest version dir.
///   2. `libs/<name>/` relative to `search_root` (common workspace layout).
///
/// Returns a `LibDependency` ready for `discover_lib_modules`.
pub fn resolve_lib_auto(name: &str, search_root: Option<&Path>) -> Option<LibDependency> {
    // 1. Global cache
    let cache_root = nex_libs_dir().join(name);
    if cache_root.is_dir() {
        if let Some((version_dir, version)) = latest_cached_version(&cache_root) {
            let native_lib = resolve_native_lib(&version_dir);
            return Some(LibDependency {
                name: name.to_string(),
                root: version_dir,
                native_lib,
                version: Some(version),
                git: None,
            });
        }
    }

    // 2. Walk up from search_root looking for libs/<name>/ in each ancestor.
    if let Some(root) = search_root {
        let mut dir = root.to_path_buf();
        loop {
            let local = dir.join("libs").join(name);
            if local.is_dir() && local.join("project.toml").exists() {
                let native_lib = resolve_native_lib(&local);
                return Some(LibDependency {
                    name: name.to_string(),
                    root: local,
                    native_lib,
                    version: None,
                    git: None,
                });
            }
            if !dir.pop() {
                break;
            }
        }
    }

    None
}

/// Scan `<cache_root>/` for version subdirectories and return the one
/// with the highest directory name (reverse-sorted, so newest first).
fn latest_cached_version(cache_root: &Path) -> Option<(PathBuf, String)> {
    let mut versions: Vec<(String, PathBuf)> = Vec::new();
    let entries = fs::read_dir(cache_root).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() && path.join("project.toml").exists() {
            let name = entry.file_name().to_string_lossy().to_string();
            versions.push((name, path));
        }
    }
    // Sort descending so that "1.0.0" > "0.9.0", "latest" sorts high too.
    versions.sort_by(|a, b| b.0.cmp(&a.0));
    versions.into_iter().next().map(|(v, p)| (p, v))
}

fn parse_libs_section(toml: &str, project_root: &Path, sink: &mut DiagnosticSink) -> Vec<LibDependency> {
    let mut libs = Vec::new();
    let mut in_libs = false;

    for line in toml.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_libs = trimmed == "[libs]";
            continue;
        }
        if !in_libs || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        let name = key.trim().to_string();
        let value = value.trim();

        // Determine which format this entry uses.
        if let Some(inner) = value.strip_prefix('{').and_then(|v| v.strip_suffix('}')) {
            // Inline table: { path = "..." } or { git = "...", version = "..." }
            let fields: HashMap<String, String> = inner
                .split(',')
                .filter_map(|part| {
                    let (k, v) = part.split_once('=')?;
                    Some((
                        k.trim().to_string(),
                        v.trim().trim_matches('"').trim_matches('\'').to_string(),
                    ))
                })
                .collect();

            if let Some(path_str) = fields.get("path") {
                // Local path dependency (existing behavior)
                let lib_root = project_root.join(path_str);
                if !lib_root.exists() {
                    sink.push(Diagnostic {
                        id: "lib_path_missing".to_string(),
                        severity: Severity::Error,
                        span: None,
                        file: Some(lib_root.clone()),
                        message: format!("library `{name}` path does not exist: {}", lib_root.display()),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                    continue;
                }
                let native_lib = resolve_native_lib(&lib_root);
                libs.push(LibDependency {
                    name,
                    root: lib_root,
                    native_lib,
                    version: fields.get("version").cloned(),
                    git: fields.get("git").cloned(),
                });
            } else if let Some(git_source) = fields.get("git") {
                // Git dependency: resolve from global cache
                let version = fields.get("version").cloned().unwrap_or_else(|| "latest".to_string());
                match resolve_from_global_cache(&name, &version) {
                    Some(lib_root) => {
                        let native_lib = resolve_native_lib(&lib_root);
                        libs.push(LibDependency {
                            name,
                            root: lib_root,
                            native_lib,
                            version: Some(version),
                            git: Some(git_source.clone()),
                        });
                    }
                    None => {
                        sink.push(Diagnostic {
                            id: "lib_not_installed".to_string(),
                            severity: Severity::Error,
                            span: None,
                            file: None,
                            message: format!(
                                "library `{name}` (git: {git_source}, version: {version}) is not installed. Run: nex install {git_source}:{version}"
                            ),
                            notes: Vec::new(),
                            suggestions: Vec::new(),
                        });
                    }
                }
            } else {
                sink.push(Diagnostic {
                    id: "lib_invalid_entry".to_string(),
                    severity: Severity::Error,
                    span: None,
                    file: None,
                    message: format!("invalid [libs] entry for `{name}`: expected {{ path = \"...\" }} or {{ git = \"...\", version = \"...\" }}"),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
        } else {
            // Plain string value — could be a version or a local path.
            let v = value.trim_matches('"').trim_matches('\'');
            if v.is_empty() {
                continue;
            }

            // Heuristic: if it looks like a semver (starts with digit), treat as version.
            // Otherwise treat as a local path (backwards compat).
            let is_version = v.chars().next().map_or(false, |c| c.is_ascii_digit());

            if is_version {
                // Version string: resolve from global cache
                match resolve_from_global_cache(&name, v) {
                    Some(lib_root) => {
                        let native_lib = resolve_native_lib(&lib_root);
                        libs.push(LibDependency {
                            name,
                            root: lib_root,
                            native_lib,
                            version: Some(v.to_string()),
                            git: None,
                        });
                    }
                    None => {
                        sink.push(Diagnostic {
                            id: "lib_not_installed".to_string(),
                            severity: Severity::Error,
                            span: None,
                            file: None,
                            message: format!(
                                "library `{name}` version {v} is not installed. Run: nex install <user/repo>:{v}"
                            ),
                            notes: Vec::new(),
                            suggestions: Vec::new(),
                        });
                    }
                }
            } else {
                // Local path (backwards compat)
                let lib_root = project_root.join(v);
                if !lib_root.exists() {
                    sink.push(Diagnostic {
                        id: "lib_path_missing".to_string(),
                        severity: Severity::Error,
                        span: None,
                        file: Some(lib_root.clone()),
                        message: format!("library `{name}` path does not exist: {}", lib_root.display()),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                    continue;
                }
                let native_lib = resolve_native_lib(&lib_root);
                libs.push(LibDependency {
                    name,
                    root: lib_root,
                    native_lib,
                    version: None,
                    git: None,
                });
            }
        }
    }

    libs
}

/// Resolve a library's dynamic library path.
///
/// First checks for a Nex-compiled shared library (named after the lib's
/// `name` field in project.toml, e.g. `torch.dll`).  Falls back to the
/// legacy `native = "..."` field for Rust-compiled native DLLs.
fn resolve_native_lib(lib_root: &Path) -> Option<PathBuf> {
    let toml_path = lib_root.join("project.toml");
    let toml_text = fs::read_to_string(&toml_path).ok()?;

    // Try 1: `native = "..."` field — Rust-compiled native DLLs with runtime
    // primitives.  These take priority because the JIT needs the actual
    // native implementations (e.g. nex_torch_native.dll), not the
    // Nex-compiled wrapper DLL (e.g. torch.dll).
    if let Some(native_name) = parse_native_field(&toml_text) {
        let candidates = if cfg!(target_os = "windows") {
            vec![lib_root.join(format!("{native_name}.dll"))]
        } else if cfg!(target_os = "macos") {
            vec![
                lib_root.join(format!("lib{native_name}.dylib")),
                lib_root.join(format!("lib{native_name}.so")),
            ]
        } else {
            vec![lib_root.join(format!("lib{native_name}.so"))]
        };

        for path in candidates {
            if path.exists() {
                return Some(path);
            }
        }

        // DLL not found yet — return the expected path for error messages.
        if cfg!(target_os = "windows") {
            return Some(lib_root.join(format!("{native_name}.dll")));
        } else if cfg!(target_os = "macos") {
            return Some(lib_root.join(format!("lib{native_name}.dylib")));
        } else {
            return Some(lib_root.join(format!("lib{native_name}.so")));
        }
    }

    // Try 2: Nex-compiled lib DLL (name from `name` field).
    // Used for pure-Nex libraries that have no native runtime dependency.
    if let Some(lib_name) = parse_name_field(&toml_text) {
        let candidates = if cfg!(target_os = "windows") {
            vec![lib_root.join(format!("{lib_name}.dll"))]
        } else if cfg!(target_os = "macos") {
            vec![
                lib_root.join(format!("lib{lib_name}.dylib")),
                lib_root.join(format!("lib{lib_name}.so")),
            ]
        } else {
            vec![lib_root.join(format!("lib{lib_name}.so"))]
        };

        for path in &candidates {
            if path.exists() {
                return Some(path.clone());
            }
        }
    }

    None
}

/// Parse the `name = "..."` field from the top-level of a project.toml string.
fn parse_name_field(toml: &str) -> Option<String> {
    for line in toml.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            break; // entered a section
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        if key.trim() == "name" {
            let v = value.trim().trim_matches('"').trim_matches('\'');
            if !v.is_empty() {
                return Some(v.to_string());
            }
        }
    }
    None
}

/// Parse the `native = "..."` field from a project.toml string.
fn parse_native_field(toml: &str) -> Option<String> {
    let mut in_section = false;
    for line in toml.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            // We're entering a section — `native` must be at the top level
            in_section = true;
            continue;
        }
        if in_section {
            continue;
        }
        if !trimmed.starts_with("native") {
            continue;
        }
        let Some((key, value)) = trimmed.split_once('=') else {
            continue;
        };
        if key.trim() != "native" {
            continue;
        }
        let value = value.trim().trim_matches('"').trim_matches('\'');
        if !value.is_empty() {
            return Some(value.to_string());
        }
    }
    None
}

fn collect_au_files(dir: &Path, out: &mut Vec<PathBuf>, sink: &mut DiagnosticSink) {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(err) => {
            sink.push(Diagnostic {
                id: "project_read_dir".to_string(),
                severity: Severity::Error,
                span: None,
                file: Some(dir.to_path_buf()),
                message: err.to_string(),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
            return;
        }
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_au_files(&path, out, sink);
            continue;
        }
        if path
            .extension()
            .is_some_and(|ext| ext.to_string_lossy().eq_ignore_ascii_case("nex"))
        {
            out.push(path);
        }
    }
}

fn module_name_from_source_path(src_dir: &Path, source: &Path) -> Option<String> {
    let rel = source.strip_prefix(src_dir).ok()?;
    let mut parts = rel
        .iter()
        .map(|part| part.to_string_lossy().to_string())
        .collect::<Vec<_>>();
    let last = parts.last_mut()?;
    if !last.ends_with(".nex") {
        return None;
    }
    *last = last.trim_end_matches(".nex").to_string();
    if last.is_empty() {
        return None;
    }
    Some(parts.join("."))
}

fn canonical_module_name(path: &str) -> String {
    let normalized = path.replace('\\', "/");
    let without_ext = normalized.trim_end_matches(".nex");

    let src_relative = if let Some(index) = without_ext.rfind("/src/") {
        &without_ext[index + 5..]
    } else if let Some(rest) = without_ext.strip_prefix("src/") {
        rest
    } else {
        without_ext
    };

    let trimmed = src_relative.trim_matches('/');
    if trimmed.is_empty() {
        "module".to_string()
    } else {
        trimmed.replace('/', ".")
    }
}

fn validate_imports(graph: &ModuleGraph, sink: &mut DiagnosticSink) {
    for (module, file) in &graph.declarations {
        for item in &file.items {
            if let Item::Import(import) = item {
                validate_import_decl(module, file, import, graph, sink);
            }
        }
    }
}

fn validate_import_decl(
    module: &str,
    file: &SourceFile,
    import: &ImportDecl,
    graph: &ModuleGraph,
    sink: &mut DiagnosticSink,
) {
    let dep = import.path.join(".");
    if dep.is_empty() {
        sink.push(Diagnostic {
            id: "import_invalid_path".to_string(),
            severity: Severity::Error,
            span: Some(import.span),
            file: Some(PathBuf::from(file.path.clone())),
            message: "invalid import path".to_string(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
        return;
    }
    if is_external_module(&dep, &graph.lib_names) {
        return;
    }

    let Some(target_file) = graph.declarations.get(&dep) else {
        sink.push(Diagnostic {
            id: "module_import_missing".to_string(),
            severity: Severity::Error,
            span: Some(import.span),
            file: Some(PathBuf::from(file.path.clone())),
            message: format!("module `{module}` imports missing module `{dep}`"),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
        return;
    };

    if let ImportKind::From(names) = &import.kind {
        let exported = exported_symbols(target_file);
        for name in names {
            match exported.get(name) {
                Some(Visibility::Public) => {}
                Some(Visibility::Internal) => sink.push(Diagnostic {
                    id: "import_symbol_private".to_string(),
                    severity: Severity::Error,
                    span: Some(import.span),
                    file: Some(PathBuf::from(file.path.clone())),
                    message: format!("symbol `{name}` in module `{dep}` is not public"),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                }),
                None => sink.push(Diagnostic {
                    id: "import_symbol_missing".to_string(),
                    severity: Severity::Error,
                    span: Some(import.span),
                    file: Some(PathBuf::from(file.path.clone())),
                    message: format!("symbol `{name}` not found in module `{dep}`"),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                }),
            }
        }
    }
}

fn exported_symbols(file: &SourceFile) -> HashMap<String, Visibility> {
    let mut out = HashMap::new();
    for item in &file.items {
        if let Some(name) = item_name(item) {
            out.insert(name, item_visibility(item));
        }
    }
    out
}

fn is_std_module(module: &str) -> bool {
    module == "std" || module.starts_with("std.")
}

fn is_external_module(module: &str, lib_names: &HashSet<String>) -> bool {
    if is_std_module(module) {
        return true;
    }
    let prefix = module.split('.').next().unwrap_or("");
    lib_names.contains(prefix)
}

fn item_visibility(item: &Item) -> Visibility {
    match item {
        Item::Import(_) => Visibility::Internal,
        Item::Function(f) => {
            if f.is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            }
        }
        Item::Class(c) => c.visibility,
        Item::Interface(i) => i.visibility,
        Item::Struct(s) => s.visibility,
        Item::Enum(e) => e.visibility,
        Item::Variable(v) => v.visibility,
        Item::Using(_) => Visibility::Internal,
        Item::Statement(_) => Visibility::Internal,
    }
}

fn item_name(item: &Item) -> Option<String> {
    match item {
        Item::Import(_) => None,
        Item::Function(f) => Some(f.name.clone()),
        Item::Class(c) => Some(c.name.clone()),
        Item::Interface(i) => Some(i.name.clone()),
        Item::Struct(s) => Some(s.name.clone()),
        Item::Enum(e) => Some(e.name.clone()),
        Item::Variable(v) => Some(v.name.clone()),
        Item::Using(_) | Item::Statement(_) => None,
    }
}

fn topological_order(graph: &ModuleGraph) -> Vec<String> {
    let mut indeg: HashMap<String, usize> = HashMap::new();
    for module in graph.declarations.keys() {
        indeg.insert(module.clone(), 0);
    }

    for deps in graph.imports.values() {
        for dep in deps {
            if is_external_module(dep, &graph.lib_names) || !graph.declarations.contains_key(dep) {
                continue;
            }
            *indeg.entry(dep.clone()).or_insert(0) += 1;
        }
    }

    let mut queue = VecDeque::new();
    for (module, degree) in &indeg {
        if *degree == 0 {
            queue.push_back(module.clone());
        }
    }

    let mut order = Vec::new();
    while let Some(module) = queue.pop_front() {
        order.push(module.clone());
        if let Some(deps) = graph.imports.get(&module) {
            for dep in deps {
                if is_external_module(dep, &graph.lib_names) || !graph.declarations.contains_key(dep) {
                    continue;
                }
                if let Some(degree) = indeg.get_mut(dep) {
                    if *degree > 0 {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push_back(dep.clone());
                        }
                    }
                }
            }
        }
    }

    order
}

fn detect_cycles(graph: &ModuleGraph, sink: &mut DiagnosticSink) {
    let mut states: HashMap<String, u8> = HashMap::new();
    let mut stack: Vec<String> = Vec::new();
    let mut reported = HashSet::new();

    for module in graph.declarations.keys() {
        if states.get(module).copied().unwrap_or(0) == 0 {
            detect_cycles_dfs(module, graph, &mut states, &mut stack, &mut reported, sink);
        }
    }
}

fn detect_cycles_dfs(
    module: &str,
    graph: &ModuleGraph,
    states: &mut HashMap<String, u8>,
    stack: &mut Vec<String>,
    reported: &mut HashSet<String>,
    sink: &mut DiagnosticSink,
) {
    states.insert(module.to_string(), 1);
    stack.push(module.to_string());

    if let Some(deps) = graph.imports.get(module) {
        for dep in deps {
            if is_external_module(dep, &graph.lib_names) || !graph.declarations.contains_key(dep) {
                continue;
            }
            match states.get(dep).copied().unwrap_or(0) {
                0 => {
                    detect_cycles_dfs(dep, graph, states, stack, reported, sink);
                }
                1 => {
                    if let Some(start) = stack.iter().position(|name| name == dep) {
                        let mut cycle = stack[start..].to_vec();
                        cycle.push(dep.clone());
                        let trace = cycle.join(" -> ");
                        if reported.insert(trace.clone()) {
                            sink.push(Diagnostic {
                                id: "module_cycle".to_string(),
                                severity: Severity::Error,
                                span: None,
                                file: graph
                                    .declarations
                                    .get(module)
                                    .map(|file| PathBuf::from(file.path.clone())),
                                message: format!("module import cycle detected: {trace}"),
                                notes: vec![trace],
                                suggestions: Vec::new(),
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }

    stack.pop();
    states.insert(module.to_string(), 2);
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexc_ast::*;
    use nexc_diag::Span;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn source(path: &str, items: Vec<Item>) -> SourceFile {
        SourceFile {
            path: path.to_string(),
            span: span(),
            items,
        }
    }

    fn import_module(path: &str) -> Item {
        Item::Import(ImportDecl {
            path: path.split('.').map(str::to_string).collect(),
            alias: None,
            kind: ImportKind::Module,
            span: span(),
            visibility: Visibility::Internal,
            synthetic: false,
        })
    }

    fn import_from(path: &str, names: &[&str]) -> Item {
        Item::Import(ImportDecl {
            path: path.split('.').map(str::to_string).collect(),
            alias: None,
            kind: ImportKind::From(names.iter().map(|n| (*n).to_string()).collect()),
            span: span(),
            visibility: Visibility::Internal,
            synthetic: false,
        })
    }

    fn class_item(name: &str, vis: Visibility) -> Item {
        Item::Class(ClassDecl {
            name: name.to_string(),
            is_partial: false,
            visibility: vis,
            type_params: Vec::new(),
            base_specs: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            attributes: Vec::new(),
            span: span(),
        })
    }

    #[test]
    fn reports_missing_import_module() {
        let mut sink = DiagnosticSink::new();
        let files = vec![source("src/a.nex", vec![import_module("missing.mod")])];
        let _ = build_module_graph(&files, &mut sink);
        assert!(sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "module_import_missing"));
    }

    #[test]
    fn validates_from_import_symbol_visibility() {
        let mut sink = DiagnosticSink::new();
        let dep = source(
            "src/dep.nex",
            vec![
                class_item("PublicType", Visibility::Public),
                class_item("HiddenType", Visibility::Internal),
            ],
        );
        let user = source(
            "src/user.nex",
            vec![import_from("dep", &["PublicType", "HiddenType", "MissingType"])],
        );
        let _ = build_module_graph(&[dep, user], &mut sink);

        assert!(sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "import_symbol_private"));
        assert!(sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "import_symbol_missing"));
    }

    #[test]
    fn reports_module_cycle_with_trace() {
        let mut sink = DiagnosticSink::new();
        let a = source("src/a.nex", vec![import_module("b")]);
        let b = source("src/b.nex", vec![import_module("a")]);
        let _ = build_module_graph(&[a, b], &mut sink);

        let cycle = sink
            .diagnostics()
            .iter()
            .find(|d| d.id == "module_cycle")
            .expect("expected cycle diagnostic");
        assert!(cycle.message.contains("->"));
    }

    #[test]
    fn discovers_modules_from_project_layout() {
        let mut sink = DiagnosticSink::new();
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("nex_resolve_{nonce}"));
        let src = root.join("src").join("net");
        fs::create_dir_all(&src).unwrap();
        fs::write(root.join("project.toml"), "name = \"demo\"\n").unwrap();
        fs::write(src.join("http.nex"), "def x() -> Unit { return }").unwrap();

        let modules = discover_project_modules(&root, &mut sink);
        assert!(sink.is_empty(), "{:?}", sink.diagnostics());
        assert!(modules.contains_key("net.http"));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn discovers_sibling_modules_from_directory() {
        let mut sink = DiagnosticSink::new();
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("nex_sibling_{nonce}"));
        let net_dir = root.join("net");
        fs::create_dir_all(&net_dir).unwrap();
        fs::write(root.join("main.nex"), "def main() -> Unit { return }").unwrap();
        fs::write(root.join("utils.nex"), "def helper() -> Unit { return }").unwrap();
        fs::write(net_dir.join("http.nex"), "def fetch() -> Unit { return }").unwrap();

        let main_path = root.join("main.nex");
        let modules = discover_sibling_modules(&root, Some(&main_path), &mut sink);

        assert!(sink.is_empty(), "{:?}", sink.diagnostics());
        assert!(!modules.contains_key("main"), "should exclude the main file");
        assert!(modules.contains_key("utils"), "should find utils.nex");
        assert!(modules.contains_key("net.http"), "should find net/http.nex");
        assert_eq!(modules.len(), 2);

        let _ = fs::remove_dir_all(&root);
    }
}
