use nexc_ast as ast;
use nexc_diag::{Diagnostic, DiagnosticSink, Severity, SourceMap};
use nexc_lex::Token;
use nexc_layout::ClassLayout;
use nexc_meta::{NexMeta, read_meta_if_exists, write_meta};
use nexc_parse::Parser;
use nexc_resolve::{build_module_graph_with_libs, build_symbol_table, enforce_visibility, merge_partial_classes, resolve_module};
use nexc_type::TypedModule;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

pub use nexc_ir::IrModule;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputKind {
    Executable,
    SharedLib,
}

#[derive(Debug, Clone)]
pub struct CompileOptions {
    pub source_path: String,
    pub emit_metadata: bool,
    pub output_dir: Option<PathBuf>,
    pub lib_names: HashSet<String>,
    /// Paths to native dynamic libraries (.dll / .so) to load for JIT.
    pub native_libs: Vec<PathBuf>,
    pub output_kind: OutputKind,
    /// Canonical module name (e.g. "nex3d.color") used to namespace exported
    /// symbols so that identically-named functions in different libraries
    /// don't collide at link time.  When `Some`, all top-level function and
    /// class/struct method definitions are prefixed with this name.
    pub module_prefix: Option<String>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            source_path: String::from("<stdin>"),
            emit_metadata: false,
            output_dir: None,
            lib_names: HashSet::new(),
            native_libs: Vec::new(),
            output_kind: OutputKind::Executable,
            module_prefix: None,
        }
    }
}

#[derive(Debug)]
pub struct CompileResult {
    pub diagnostics: Vec<Diagnostic>,
    pub source_map: SourceMap,
    pub typed: Option<TypedModule>,
    pub ir: Option<IrModule>,
    pub layout: Vec<ClassLayout>,
    pub object: Option<Vec<u8>>,
    pub metadata: Option<NexMeta>,
    pub executable: Option<PathBuf>,
}

#[derive(Debug)]
pub struct ModuleGraphPlan {
    pub source_tokens: Vec<Token>,
    pub source_ast: ast::SourceFile,
    pub imports: Vec<ast::ImportDecl>,
    pub layouts: Vec<ClassLayout>,
}

/// Full compilation pipeline: lex -> parse -> type check -> lower -> codegen.
/// Produces object file bytes in `result.object`.
pub fn compile_module(source: &str, options: CompileOptions) -> CompileResult {
    let mut sink = DiagnosticSink::new();
    let mut source_map = SourceMap::new();
    source_map.add_file(Some(&options.source_path), source);
    let mut stage_tokens = Vec::new();
    let mut stage_ast = ast::SourceFile::default();
    let mut stage_graph: Vec<ast::ImportDecl> = Vec::new();
    let mut stage_layouts = Vec::new();
    let typed: Option<TypedModule>;
    let mut ir = None;
    let mut object = None;
    let mut metadata = None;

    run_lexer(source, &options.source_path, &mut sink, &mut stage_tokens);
    run_parser(source, &options.source_path, &stage_tokens, &mut sink, &mut stage_ast);
    run_nexui_pass(&options.source_path, &mut stage_ast, &mut sink);
    merge_partial_classes(&mut stage_ast, &mut sink);
    run_module_graph(&stage_ast, &options.lib_names, &mut sink, &mut stage_graph);
    let mut typed_mod = run_declare_types(&stage_ast, &mut sink);
    run_validate_inheritance(&stage_ast, &mut typed_mod, &mut sink);
    run_layout(&typed_mod, &mut sink, &mut stage_layouts);
    run_typecheck(&mut typed_mod, &mut sink);
    let lowered = run_lower(&typed_mod, &mut sink, &stage_layouts, &mut ir, options.module_prefix.as_deref());

    if !sink.has_errors() {
        run_codegen(&lowered, &mut sink, &mut object, options.output_kind);
    }

    typed = Some(typed_mod);

    if options.emit_metadata && !sink.has_errors() {
        let meta = NexMeta {
            module: options.source_path.clone(),
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            signatures: Vec::new(),
            exported_symbols: Vec::new(),
            layout_hashes: stage_layouts.iter().map(|x| (x.class_name.clone(), x.hash())).collect(),
            dependency_hashes: Vec::new(),
            source_hash: format!("{:x}", murmur_hash(source)),
            warnings: sink
                .diagnostics()
                .iter()
                .filter(|d| matches!(d.severity, Severity::Warning))
                .count() as u32,
        };
        let path = PathBuf::from(format!("{}.nexmeta", options.source_path));
        if let Err(diag) = write_meta(&meta, &path) {
            sink.push(diag);
        } else {
            metadata = Some(meta);
        }
    }

    if !options.emit_metadata {
        if let Some(prev) = read_previous_meta_if_imported(&options.source_path, &mut sink) {
            metadata = Some(prev);
        }
    }

    CompileResult {
        diagnostics: sink.diagnostics().to_vec(),
        source_map,
        typed,
        ir,
        layout: stage_layouts,
        object,
        metadata,
        executable: None,
    }
}

/// AOT compilation: compile + produce object bytes.
/// The object bytes in `result.object` are a real native object file.
pub fn compile_to_native(source: &str, options: CompileOptions) -> CompileResult {
    compile_module(source, options)
}

/// Link object file bytes into a native executable using the system linker.
pub fn link_native(result: &mut CompileResult, out_dir: &Path, stem: &str) -> Result<PathBuf, String> {
    let object_bytes = match &result.object {
        Some(b) => b.clone(),
        None => return Err("no object code generated".into()),
    };

    std::fs::create_dir_all(out_dir)
        .map_err(|e| format!("cannot create output directory: {e}"))?;

    let obj_ext = if cfg!(windows) { "obj" } else { "o" };
    let obj_path = out_dir.join(format!("{stem}.{obj_ext}"));
    std::fs::write(&obj_path, &object_bytes)
        .map_err(|e| format!("cannot write {}: {e}", obj_path.display()))?;

    let exe_name = if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.to_string()
    };
    let exe_path = out_dir.join(&exe_name);

    let runtime_lib = find_runtime_library();

    let linker = find_linker()
        .ok_or_else(|| "no linker found. Install a C toolchain (gcc, clang, or MSVC Build Tools).".to_string())?;

    let output = invoke_linker(&linker, &obj_path, &exe_path, runtime_lib.as_deref())?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!("linker failed (exit {}):\n{stdout}{stderr}", output.status));
    }

    result.executable = Some(exe_path.clone());
    Ok(exe_path)
}

/// Link object file bytes into a shared library (DLL / .so / .dylib).
pub fn link_shared_lib(result: &mut CompileResult, out_dir: &Path, stem: &str) -> Result<PathBuf, String> {
    let object_bytes = match &result.object {
        Some(b) => b.clone(),
        None => return Err("no object code generated".into()),
    };

    std::fs::create_dir_all(out_dir)
        .map_err(|e| format!("cannot create output directory: {e}"))?;

    let obj_ext = if cfg!(windows) { "obj" } else { "o" };
    let obj_path = out_dir.join(format!("{stem}.{obj_ext}"));
    std::fs::write(&obj_path, &object_bytes)
        .map_err(|e| format!("cannot write {}: {e}", obj_path.display()))?;

    let lib_name = if cfg!(windows) {
        format!("{stem}.dll")
    } else if cfg!(target_os = "macos") {
        format!("lib{stem}.dylib")
    } else {
        format!("lib{stem}.so")
    };
    let lib_path = out_dir.join(&lib_name);

    let linker = find_linker()
        .ok_or_else(|| "no linker found. Install a C toolchain (gcc, clang, or MSVC Build Tools).".to_string())?;

    let output = invoke_linker_shared(&linker, &obj_path, &lib_path, None)?;

    if !output.status.success() && !lib_path.exists() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!("linker failed (exit {}):\n{stdout}{stderr}", output.status));
    }

    result.executable = Some(lib_path.clone());
    Ok(lib_path)
}

/// Link multiple object files into a shared library (DLL / .so / .dylib).
pub fn link_shared_lib_multi(obj_paths: &[PathBuf], out_dir: &Path, stem: &str) -> Result<PathBuf, String> {
    std::fs::create_dir_all(out_dir)
        .map_err(|e| format!("cannot create output directory: {e}"))?;

    let lib_name = if cfg!(windows) {
        format!("{stem}.dll")
    } else if cfg!(target_os = "macos") {
        format!("lib{stem}.dylib")
    } else {
        format!("lib{stem}.so")
    };
    let lib_path = out_dir.join(&lib_name);

    // Nex lib DLLs do NOT link against nex_runtime — they contain only compiled
    // Nex functions.  Runtime symbols are resolved when the host app loads the
    // DLL (the host has the runtime linked in).
    let linker = find_linker()
        .ok_or_else(|| "no linker found. Install a C toolchain (gcc, clang, or MSVC Build Tools).".to_string())?;

    let output = invoke_linker_multi_shared(&linker, obj_paths, &lib_path, None)?;

    // MSVC with /FORCE returns non-zero even when it successfully
    // produces the DLL.  Check whether the output file was created instead of
    // relying solely on the exit code.
    if !output.status.success() && !lib_path.exists() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!("linker failed (exit {}):\n{stdout}{stderr}", output.status));
    }

    Ok(lib_path)
}

/// Link multiple object files into a single native executable.
pub fn link_native_multi(obj_paths: &[PathBuf], out_dir: &Path, stem: &str) -> Result<PathBuf, String> {
    std::fs::create_dir_all(out_dir)
        .map_err(|e| format!("cannot create output directory: {e}"))?;

    let exe_name = if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.to_string()
    };
    let exe_path = out_dir.join(&exe_name);

    let runtime_lib = find_runtime_library();
    let linker = find_linker()
        .ok_or_else(|| "no linker found. Install a C toolchain (gcc, clang, or MSVC Build Tools).".to_string())?;

    let output = invoke_linker_multi(&linker, obj_paths, &exe_path, runtime_lib.as_deref())?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!("linker failed (exit {}):\n{stdout}{stderr}", output.status));
    }

    Ok(exe_path)
}

/// JIT execution: compile source and execute `main` in-memory.
pub fn jit_run(source: &str, options: CompileOptions, args: &[String]) -> Result<i32, String> {
    let native_libs = options.native_libs.clone();
    let result = compile_module(source, options);

    if result.diagnostics.iter().any(|d| matches!(d.severity, Severity::Error)) {
        for d in &result.diagnostics {
            eprint!("{}", d.render(&result.source_map));
        }
        return Err("compilation failed".into());
    }

    for d in &result.diagnostics {
        if matches!(d.severity, Severity::Warning) {
            eprint!("{}", d.render(&result.source_map));
        }
    }

    let ir = match &result.ir {
        Some(m) => m,
        None => return Err("no IR produced".into()),
    };

    nexc_codegen_cranelift::jit_execute(ir, args, &native_libs)
}

/// JIT execution with multiple source modules.
///
/// Each entry in `sources` is `(source_text, CompileOptions)`.  All modules
/// are compiled independently and their IR is merged into a single module
/// before JIT execution.  The last module is expected to contain `main`.
pub fn jit_run_multi(
    sources: &[(&str, CompileOptions)],
    args: &[String],
    native_libs: &[PathBuf],
) -> Result<i32, String> {
    use std::collections::HashMap;

    let mut merged = IrModule {
        name: "merged".to_string(),
        functions: Vec::new(),
        types: HashMap::new(),
        layouts: HashMap::new(),
        globals: Vec::new(),
    };
    let mut had_error = false;

    for (source, options) in sources {
        let result = compile_module(source, options.clone());

        for d in &result.diagnostics {
            if matches!(d.severity, Severity::Error) {
                eprint!("{}", d.render(&result.source_map));
                had_error = true;
            } else if matches!(d.severity, Severity::Warning) {
                eprint!("{}", d.render(&result.source_map));
            }
        }

        if let Some(ir) = &result.ir {
            for func in &ir.functions {
                if !merged.functions.iter().any(|f| f.name == func.name) {
                    merged.functions.push(func.clone());
                }
            }
            merged.types.extend(ir.types.clone());
            merged.layouts.extend(ir.layouts.clone());
            for g in &ir.globals {
                if !merged.globals.contains(g) {
                    merged.globals.push(g.clone());
                }
            }
        }
    }

    if had_error {
        return Err("compilation failed".into());
    }

    if merged.functions.is_empty() {
        return Err("no IR produced".into());
    }

    // Resolve cross-module call targets.  When module prefixes are active,
    // a caller might reference a bare name (e.g. `COLOR_WHITE`) while the
    // definition is `nex3d::color::COLOR_WHITE`.  Scan Call instructions
    // and resolve bare targets to the qualified name found in the merged IR.
    resolve_cross_module_calls(&mut merged);

    nexc_codegen_cranelift::jit_execute(&merged, args, native_libs)
}

// ---------------------------------------------------------------------------
// Pipeline stages
// ---------------------------------------------------------------------------

fn run_lexer(
    source: &str,
    file: &str,
    sink: &mut DiagnosticSink,
    out_tokens: &mut Vec<Token>,
) {
    let raw = nexc_lex::lex(source, Some(file.to_string()), sink);
    let tokens = nexc_lex::asi_normalize(&raw);
    out_tokens.extend(tokens);
}

fn run_parser(
    _source: &str,
    file: &str,
    tokens: &[Token],
    sink: &mut DiagnosticSink,
    out_ast: &mut ast::SourceFile,
) {
    let mut parser = Parser::new(tokens, file.to_string());
    let parsed = parser.parse();
    parser
        .diagnostics()
        .iter()
        .cloned()
        .for_each(|d| sink.push(d));
    *out_ast = parsed;
}

/// Discover `.nexui` files in the same directory as the source file, inject
/// their generated partial classes into the AST, and also parse matching
/// `.nex` code-behind files so the partial class merge has both halves.
fn run_nexui_pass(
    source_path: &str,
    ast: &mut ast::SourceFile,
    sink: &mut DiagnosticSink,
) {
    let src_path = PathBuf::from(source_path);
    let parent = match src_path.parent() {
        Some(p) => p,
        None => return,
    };

    if !parent.exists() {
        return;
    }

    let entries = match std::fs::read_dir(parent) {
        Ok(e) => e,
        Err(_) => return,
    };

    let src_canon = std::fs::canonicalize(&src_path).unwrap_or_else(|_| src_path.clone());

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("nexui") {
            if let Ok(content) = std::fs::read_to_string(&path) {
                // Canonicalize to an absolute path so that resource imports resolve
                // correctly regardless of the process's working directory.
                let abs_path = std::fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
                let file_str = abs_path.to_string_lossy().to_string();
                if let Some(mut doc) = nexui_parse::parse_nexui(&content, &file_str, sink) {
                    // Pure stylesheet files (ResourceDictionary root) are not lowered to classes.
                    if doc.root.tag == "ResourceDictionary" {
                        continue;
                    }

                    // Resolve and merge external stylesheet imports from <Window.Resources>.
                    // Imported styles are merged before local styles so local definitions win.
                    // doc.file_path is already an absolute path (canonicalized above).
                    let doc_dir = PathBuf::from(&doc.file_path)
                        .parent()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_default();
                    let mut imported_styles: Vec<nexui_parse::UIStyle> = Vec::new();
                    for import in &doc.resource_imports {
                        let import_path = doc_dir.join(&import.source);
                        match std::fs::read_to_string(&import_path) {
                            Ok(import_content) => {
                                let import_str = import_path.to_string_lossy().to_string();
                                if let Some(import_doc) =
                                    nexui_parse::parse_nexui(&import_content, &import_str, sink)
                                {
                                    imported_styles.extend(import_doc.styles);
                                }
                            }
                            Err(_) => {
                                sink.push(Diagnostic {
                                    id: "nexui_driver".into(),
                                    severity: Severity::Error,
                                    span: None,
                                    file: Some(PathBuf::from(&doc.file_path)),
                                    message: format!(
                                        "cannot read resource file '{}'",
                                        import_path.display()
                                    ),
                                    notes: Vec::new(),
                                    suggestions: Vec::new(),
                                });
                            }
                        }
                    }
                    // Prepend imported styles so that local styles (already in doc.styles) win.
                    imported_styles.extend(doc.styles);
                    doc.styles = imported_styles;

                    let partial_class = nexui_lower::lower_document(&doc, sink);
                    ast.items.push(ast::Item::Class(partial_class));
                }
            }

            // Also parse the matching code-behind .nex file (e.g. Showcase.nex for Showcase.nexui)
            let code_behind = path.with_extension("nex");
            let cb_canon = std::fs::canonicalize(&code_behind).unwrap_or_default();
            if code_behind.exists() && cb_canon != src_canon {
                if let Ok(cb_source) = std::fs::read_to_string(&code_behind) {
                    let cb_file = code_behind.to_string_lossy().to_string();
                    let mut cb_tokens = Vec::new();
                    run_lexer(&cb_source, &cb_file, sink, &mut cb_tokens);
                    let mut cb_ast = ast::SourceFile::default();
                    run_parser(&cb_source, &cb_file, &cb_tokens, sink, &mut cb_ast);
                    for item in cb_ast.items {
                        ast.items.push(item);
                    }
                }
            }
        }
    }
}

fn run_module_graph(
    file: &ast::SourceFile,
    lib_names: &HashSet<String>,
    sink: &mut DiagnosticSink,
    out_imports: &mut Vec<ast::ImportDecl>,
) {
    let graph = build_module_graph_with_libs(std::slice::from_ref(file), lib_names, sink);
    let resolved = resolve_module(file, &graph);
    let _ = build_symbol_table(&resolved, sink);
    enforce_visibility(&resolved, sink);
    for item in &resolved.items {
        if let ast::Item::Import(decl) = item {
            out_imports.push(decl.clone());
        }
    }
}

fn run_declare_types(file: &ast::SourceFile, sink: &mut DiagnosticSink) -> TypedModule {
    nexc_type::declare_types(file, sink)
}

fn run_validate_inheritance(_file: &ast::SourceFile, typed: &mut TypedModule, _sink: &mut DiagnosticSink) {
    nexc_type::validate_inheritance(typed, _sink);
}

fn run_layout(typed: &TypedModule, _sink: &mut DiagnosticSink, out_layout: &mut Vec<ClassLayout>) {
    *out_layout = nexc_layout::compute_layouts(typed);
}

fn run_typecheck(typed: &mut TypedModule, sink: &mut DiagnosticSink) {
    nexc_type::check_bodies(typed, sink);
}

fn run_lower(
    typed: &TypedModule,
    sink: &mut DiagnosticSink,
    layouts: &[ClassLayout],
    out_ir: &mut Option<IrModule>,
    module_prefix: Option<&str>,
) -> IrModule {
    let lowered = nexc_ir::lower_typed_module_with_prefix(typed, layouts, sink, module_prefix);
    *out_ir = Some(lowered.clone());
    lowered
}

fn run_codegen(ir: &IrModule, sink: &mut DiagnosticSink, out_object: &mut Option<Vec<u8>>, output_kind: OutputKind) {
    let gen_result = match output_kind {
        OutputKind::SharedLib => nexc_codegen_cranelift::generate_shared_object(ir),
        OutputKind::Executable => nexc_codegen_cranelift::generate_object(ir),
    };
    match gen_result {
        Ok(bytes) => *out_object = Some(bytes),
        Err(msg) => {
            let (span, file) = extract_function_location(&msg, ir);
            sink.push(Diagnostic {
                id: "codegen_failure".to_string(),
                severity: Severity::Error,
                span,
                file,
                message: msg,
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }
    }
}

fn extract_function_location(
    error_msg: &str,
    ir: &IrModule,
) -> (Option<nexc_diag::Span>, Option<PathBuf>) {
    for func in &ir.functions {
        let prefixes = [
            format!("define {}: ", func.name),
            format!("declare {}: ", func.name),
        ];
        if prefixes.iter().any(|p| error_msg.starts_with(p)) {
            return (func.span, func.file.clone());
        }
    }
    (None, None)
}

/// Resolve unqualified Call targets in the merged IR to their module-prefixed
/// names.  For example, a call to `draw_line` is resolved to
/// `nex3d::draw::draw_line` if the merged IR contains such a function.
fn resolve_cross_module_calls(ir: &mut IrModule) {
    use std::collections::HashMap;

    // Build a map: bare suffix → fully-qualified name.  For `nex3d::color::COLOR_WHITE`,
    // the suffix `COLOR_WHITE` maps to the full name.  Method names like
    // `nex3d::color::Foo::bar` have suffix `Foo::bar` AND `bar`.
    let func_names: Vec<String> = ir.functions.iter().map(|f| f.name.clone()).collect();
    let mut suffix_map: HashMap<String, String> = HashMap::new();
    for name in &func_names {
        // If the name contains `::`, the last component (and multi-component
        // suffixes) are potential bare call targets.
        if let Some(pos) = name.rfind("::") {
            let bare = &name[pos + 2..];
            suffix_map.entry(bare.to_string()).or_insert_with(|| name.clone());
        }
        // Also try the portion after the first `::` (e.g. `Color::init` from
        // `nex3d::color::Color::init`).
        if let Some(first_sep) = name.find("::") {
            let rest = &name[first_sep + 2..];
            if rest != name {
                suffix_map.entry(rest.to_string()).or_insert_with(|| name.clone());
            }
        }
    }

    let func_set: std::collections::HashSet<&str> =
        func_names.iter().map(|s| s.as_str()).collect();

    for func in &mut ir.functions {
        for block in &mut func.blocks {
            for inst in &mut block.instructions {
                if let nexc_ir::IrInstruction::Call { target, .. } = inst {
                    // Skip targets that already resolve directly.
                    if func_set.contains(target.as_str()) {
                        continue;
                    }
                    // Try to resolve via suffix map.
                    if let Some(qualified) = suffix_map.get(target.as_str()) {
                        *target = qualified.clone();
                    }
                }
            }
        }
    }
}

fn read_previous_meta_if_imported(path: &str, sink: &mut DiagnosticSink) -> Option<NexMeta> {
    let source = PathBuf::from(format!("{}.nexmeta", path));
    match read_meta_if_exists(&source) {
        Ok(Some(meta)) => Some(meta),
        Ok(None) => None,
        Err(diag) => {
            sink.push(diag);
            None
        }
    }
}

fn murmur_hash(input: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    input.hash(&mut hasher);
    hasher.finish()
}

pub fn stage_order() -> [&'static str; 10] {
    [
        "Lexer + ASI",
        "Parser -> AST",
        "Module graph",
        "Resolve + visibility",
        "Declare types",
        "Inheritance DAG validation",
        "Layout engine + vtables",
        "Typecheck bodies",
        "Lower to IR",
        "Native codegen + link",
    ]
}

// ---------------------------------------------------------------------------
// Linker – invokes the system linker to produce an executable from .o/.obj
// ---------------------------------------------------------------------------

enum Linker {
    Cc(String),
    Msvc { vcvarsall: PathBuf },
}

fn find_linker() -> Option<Linker> {
    let candidates = if cfg!(windows) {
        vec!["gcc", "clang", "cc", "cl"]
    } else {
        vec!["cc", "gcc", "clang"]
    };

    for cmd in candidates {
        let probe = if cmd == "cl" {
            Command::new(cmd).arg("/?").output()
        } else {
            Command::new(cmd).arg("--version").output()
        };
        if let Ok(out) = probe {
            if out.status.success() || cmd == "cl" {
                return Some(Linker::Cc(cmd.to_string()));
            }
        }
    }

    if cfg!(windows) {
        if let Some(vcvarsall) = find_msvc_vcvarsall() {
            return Some(Linker::Msvc { vcvarsall });
        }
    }

    None
}

fn find_msvc_vcvarsall() -> Option<PathBuf> {
    let vswhere = PathBuf::from(r"C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe");
    if !vswhere.exists() {
        return None;
    }

    let output = Command::new(&vswhere)
        .args(["-latest", "-property", "installationPath"])
        .output()
        .ok()?;

    let vs_path = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if vs_path.is_empty() {
        return None;
    }

    let vcvarsall = PathBuf::from(&vs_path).join(r"VC\Auxiliary\Build\vcvarsall.bat");
    if vcvarsall.exists() {
        Some(vcvarsall)
    } else {
        None
    }
}

fn find_runtime_library() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let exe_dir = exe.parent()?;

    let lib_name = if cfg!(windows) {
        "nex_runtime.lib"
    } else {
        "libnex_runtime.a"
    };

    // 1. Check next to executable (installed toolchain).
    let candidate = exe_dir.join(lib_name);
    if candidate.exists() {
        return Some(candidate);
    }

    // 2. Check in ../lib/
    let candidate = exe_dir.join("../lib").join(lib_name);
    if candidate.exists() {
        return Some(candidate);
    }

    // 3. Development mode: scan deps/ for the hashed staticlib.
    let deps_dir = exe_dir.join("deps");
    if deps_dir.is_dir() {
        let prefix = if cfg!(windows) {
            "nex_runtime-"
        } else {
            "libnex_runtime-"
        };
        let ext = if cfg!(windows) { "lib" } else { "a" };
        if let Ok(entries) = std::fs::read_dir(&deps_dir) {
            for entry in entries.flatten() {
                let name = entry.file_name();
                let name_str = name.to_string_lossy();
                if name_str.starts_with(prefix) && name_str.ends_with(ext)
                    && !name_str.ends_with(".rlib")
                {
                    return Some(entry.path());
                }
            }
        }
    }

    None
}

fn invoke_linker(
    linker: &Linker,
    obj_path: &Path,
    exe_path: &Path,
    runtime_lib: Option<&Path>,
) -> Result<std::process::Output, String> {
    match linker {
        Linker::Cc(cmd) if cmd == "cl" => {
            let mut args: Vec<String> = vec![
                obj_path.to_string_lossy().into_owned(),
                format!("/Fe:{}", exe_path.to_string_lossy()),
                "/nologo".into(),
            ];
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            // Link with the CRT and Windows system libraries needed by the
            // Rust runtime embedded in the staticlib.
            args.extend([
                "/link".into(),
                "ws2_32.lib".into(),
                "advapi32.lib".into(),
                "userenv.lib".into(),
                "ntdll.lib".into(),
                "bcrypt.lib".into(),
                "kernel32.lib".into(),
                "msvcrt.lib".into(),
                "/DEFAULTLIB:libcmt".into(),
            ]);
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run cl: {e}"))
        }
        Linker::Cc(cmd) => {
            let mut args: Vec<String> = vec![
                obj_path.to_string_lossy().into_owned(),
                "-o".into(),
                exe_path.to_string_lossy().into_owned(),
            ];
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run {cmd}: {e}"))
        }
        Linker::Msvc { vcvarsall } => {
            let arch = if cfg!(target_arch = "x86_64") { "x64" }
                       else if cfg!(target_arch = "x86") { "x86" }
                       else { "x64" };

            let obj_dir = obj_path.parent().unwrap_or(Path::new("."));
            let bat_path = obj_dir.join("_nex_link.bat");

            let rt_arg = runtime_lib
                .map(|p| format!(" \"{}\"", p.display()))
                .unwrap_or_default();

            let win_libs = if cfg!(windows) {
                " ws2_32.lib advapi32.lib userenv.lib ntdll.lib bcrypt.lib kernel32.lib user32.lib gdi32.lib d3dcompiler_47.lib ole32.lib oleaut32.lib dwmapi.lib dxgi.lib d3d11.lib dxguid.lib"
            } else {
                ""
            };

            let bat_content = format!(
                "@echo off\r\ncall \"{}\" {} >nul 2>&1\r\nif errorlevel 1 exit /b 1\r\nlink /nologo /OUT:\"{}\" \"{}\"{}{}\r\n",
                vcvarsall.display(),
                arch,
                exe_path.display(),
                obj_path.display(),
                rt_arg,
                win_libs,
            );
            std::fs::write(&bat_path, &bat_content)
                .map_err(|e| format!("cannot write link script: {e}"))?;

            let result = Command::new("cmd")
                .args(["/C", &bat_path.to_string_lossy()])
                .output()
                .map_err(|e| format!("failed to invoke MSVC linker: {e}"));

            let _ = std::fs::remove_file(&bat_path);
            result
        }
    }
}

fn invoke_linker_multi(
    linker: &Linker,
    obj_paths: &[PathBuf],
    exe_path: &Path,
    runtime_lib: Option<&Path>,
) -> Result<std::process::Output, String> {
    match linker {
        Linker::Cc(cmd) if cmd == "cl" => {
            let mut args: Vec<String> = obj_paths
                .iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect();
            args.push(format!("/Fe:{}", exe_path.to_string_lossy()));
            args.push("/nologo".into());
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            args.extend([
                "/link".into(),
                "ws2_32.lib".into(),
                "advapi32.lib".into(),
                "userenv.lib".into(),
                "ntdll.lib".into(),
                "bcrypt.lib".into(),
                "kernel32.lib".into(),
                "msvcrt.lib".into(),
                "/DEFAULTLIB:libcmt".into(),
            ]);
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run cl: {e}"))
        }
        Linker::Cc(cmd) => {
            let mut args: Vec<String> = obj_paths
                .iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect();
            args.push("-o".into());
            args.push(exe_path.to_string_lossy().into_owned());
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run {cmd}: {e}"))
        }
        Linker::Msvc { vcvarsall } => {
            let arch = if cfg!(target_arch = "x86_64") { "x64" }
                       else if cfg!(target_arch = "x86") { "x86" }
                       else { "x64" };

            let out_dir = exe_path.parent().unwrap_or(Path::new("."));
            let bat_path = out_dir.join("_nex_link.bat");

            let obj_args: String = obj_paths
                .iter()
                .map(|p| format!(" \"{}\"", p.display()))
                .collect();
            let rt_arg = runtime_lib
                .map(|p| format!(" \"{}\"", p.display()))
                .unwrap_or_default();

            let win_libs = if cfg!(windows) {
                " ws2_32.lib advapi32.lib userenv.lib ntdll.lib bcrypt.lib kernel32.lib user32.lib gdi32.lib d3dcompiler_47.lib ole32.lib oleaut32.lib dwmapi.lib dxgi.lib d3d11.lib dxguid.lib"
            } else {
                ""
            };

            let bat_content = format!(
                "@echo off\r\ncall \"{}\" {} >nul 2>&1\r\nif errorlevel 1 exit /b 1\r\nlink /nologo /OUT:\"{}\"{}{}{}\r\n",
                vcvarsall.display(),
                arch,
                exe_path.display(),
                obj_args,
                rt_arg,
                win_libs,
            );
            std::fs::write(&bat_path, &bat_content)
                .map_err(|e| format!("cannot write link script: {e}"))?;

            let result = Command::new("cmd")
                .args(["/C", &bat_path.to_string_lossy()])
                .output()
                .map_err(|e| format!("failed to invoke MSVC linker: {e}"));

            let _ = std::fs::remove_file(&bat_path);
            result
        }
    }
}

// ---------------------------------------------------------------------------
// Linker – shared library variants (-shared / /DLL)
// ---------------------------------------------------------------------------

fn invoke_linker_shared(
    linker: &Linker,
    obj_path: &Path,
    lib_path: &Path,
    runtime_lib: Option<&Path>,
) -> Result<std::process::Output, String> {
    match linker {
        Linker::Cc(cmd) if cmd == "cl" => {
            let mut args: Vec<String> = vec![
                obj_path.to_string_lossy().into_owned(),
                format!("/Fe:{}", lib_path.to_string_lossy()),
                "/nologo".into(),
                "/LD".into(),
            ];
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            args.extend([
                "/link".into(),
                "/DLL".into(),
                "/NOENTRY".into(),
                "/NODEFAULTLIB".into(),
                "/FORCE".into(),
            ]);
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run cl: {e}"))
        }
        Linker::Cc(cmd) => {
            let mut args: Vec<String> = vec![
                "-shared".into(),
                "-nostdlib".into(),
                "-Wl,--allow-shlib-undefined".into(),
                "-Wl,--no-as-needed".into(),
                obj_path.to_string_lossy().into_owned(),
                "-o".into(),
                lib_path.to_string_lossy().into_owned(),
            ];
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run {cmd}: {e}"))
        }
        Linker::Msvc { vcvarsall } => {
            let arch = if cfg!(target_arch = "x86_64") { "x64" }
                       else if cfg!(target_arch = "x86") { "x86" }
                       else { "x64" };

            let obj_dir = obj_path.parent().unwrap_or(Path::new("."));
            let bat_path = obj_dir.join("_nex_link_dll.bat");

            let rt_arg = runtime_lib
                .map(|p| format!(" \"{}\"", p.display()))
                .unwrap_or_default();

            let bat_content = format!(
                "@echo off\r\ncall \"{}\" {} >nul 2>&1\r\nif errorlevel 1 exit /b 1\r\nlink /nologo /DLL /NOENTRY /NODEFAULTLIB /FORCE /OUT:\"{}\" \"{}\"{}\r\n",
                vcvarsall.display(),
                arch,
                lib_path.display(),
                obj_path.display(),
                rt_arg,
            );
            std::fs::write(&bat_path, &bat_content)
                .map_err(|e| format!("cannot write link script: {e}"))?;

            let result = Command::new("cmd")
                .args(["/C", &bat_path.to_string_lossy()])
                .output()
                .map_err(|e| format!("failed to invoke MSVC linker: {e}"));

            let _ = std::fs::remove_file(&bat_path);
            result
        }
    }
}

fn invoke_linker_multi_shared(
    linker: &Linker,
    obj_paths: &[PathBuf],
    lib_path: &Path,
    runtime_lib: Option<&Path>,
) -> Result<std::process::Output, String> {
    match linker {
        Linker::Cc(cmd) if cmd == "cl" => {
            let mut args: Vec<String> = obj_paths
                .iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect();
            args.push(format!("/Fe:{}", lib_path.to_string_lossy()));
            args.push("/nologo".into());
            args.push("/LD".into());
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            args.extend([
                "/link".into(),
                "/DLL".into(),
                "/NOENTRY".into(),
                "/NODEFAULTLIB".into(),
                "/FORCE".into(),
            ]);
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run cl: {e}"))
        }
        Linker::Cc(cmd) => {
            let mut args: Vec<String> = vec![
                "-shared".into(),
                "-nostdlib".into(),
                "-Wl,--allow-shlib-undefined".into(),
                "-Wl,--no-as-needed".into(),
            ];
            args.extend(obj_paths.iter().map(|p| p.to_string_lossy().into_owned()));
            args.push("-o".into());
            args.push(lib_path.to_string_lossy().into_owned());
            if let Some(rt) = runtime_lib {
                args.push(rt.to_string_lossy().into_owned());
            }
            Command::new(cmd)
                .args(&args)
                .output()
                .map_err(|e| format!("failed to run {cmd}: {e}"))
        }
        Linker::Msvc { vcvarsall } => {
            let arch = if cfg!(target_arch = "x86_64") { "x64" }
                       else if cfg!(target_arch = "x86") { "x86" }
                       else { "x64" };

            let out_dir = lib_path.parent().unwrap_or(Path::new("."));
            let bat_path = out_dir.join("_nex_link_dll.bat");

            let obj_args: String = obj_paths
                .iter()
                .map(|p| format!(" \"{}\"", p.display()))
                .collect();
            let rt_arg = runtime_lib
                .map(|p| format!(" \"{}\"", p.display()))
                .unwrap_or_default();

            let bat_content = format!(
                "@echo off\r\ncall \"{}\" {} >nul 2>&1\r\nif errorlevel 1 exit /b 1\r\nlink /nologo /DLL /NOENTRY /NODEFAULTLIB /FORCE /OUT:\"{}\"{}{}\r\n",
                vcvarsall.display(),
                arch,
                lib_path.display(),
                obj_args,
                rt_arg,
            );
            std::fs::write(&bat_path, &bat_content)
                .map_err(|e| format!("cannot write link script: {e}"))?;

            let result = Command::new("cmd")
                .args(["/C", &bat_path.to_string_lossy()])
                .output()
                .map_err(|e| format!("failed to invoke MSVC linker: {e}"));

            let _ = std::fs::remove_file(&bat_path);
            result
        }
    }
}
