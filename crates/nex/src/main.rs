use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use nexc_diag::{Diagnostic, Severity, SourceMap};
use nexc_driver::{compile_module, compile_to_native, link_native, link_native_multi, link_shared_lib, link_shared_lib_multi, jit_run, jit_run_multi, CompileOptions, OutputKind};
use nexc_fmt::format_source;
use nexc_resolve::{discover_project_modules, discover_lib_names, discover_native_libs, discover_sibling_modules, nex_libs_dir, resolve_from_global_cache};

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        usage();
        return;
    }

    // Shorthand: `nex file.nex [args...]` -> JIT run
    if args[0].ends_with(".nex") {
        let source_path = PathBuf::from(args.remove(0));
        let code = run_jit(&source_path, &args);
        process::exit(code);
    }

    let command = args.remove(0);
    match command.as_str() {
        "build" => {
            let is_lib = args.iter().any(|a| a == "--lib");
            args.retain(|a| a != "--lib");

            let target = args.pop().unwrap_or_else(|| ".".to_string());
            let target_path = PathBuf::from(&target);

            let code = if is_lib {
                let root = if target_path.is_dir() {
                    target_path
                } else {
                    PathBuf::from(".")
                };
                build_lib_project(&root)
            } else if target_path.is_file() {
                build_single_module(&target_path)
            } else {
                let root = if target_path.is_dir() {
                    target_path
                } else {
                    PathBuf::from(".")
                };
                build_project_modules(&root)
            };
            if code != 0 {
                process::exit(code);
            }
        }
        "run" => {
            let mut program_args: Vec<String> = Vec::new();
            let mut source_arg = ".".to_string();

            // Split args at "--": before is for nex, after is for the program
            if let Some(sep) = args.iter().position(|a| a == "--") {
                program_args = args.split_off(sep + 1);
                args.pop(); // remove the "--"
            }
            if let Some(a) = args.pop() {
                source_arg = a;
            }

            let target_path = PathBuf::from(&source_arg);
            let source_path = if target_path.is_file() {
                target_path
            } else {
                let root = if target_path.is_dir() {
                    target_path
                } else {
                    PathBuf::from(".")
                };
                root.join("src").join("main.nex")
            };

            let code = run_jit(&source_path, &program_args);
            process::exit(code);
        }
        "fmt" => {
            let source = args
                .pop()
                .unwrap_or_else(|| "src/main.nex".to_string());
            let text = fs::read_to_string(&source).unwrap_or_default();
            println!("{}", format_source(&text));
        }
        "lint" => {
            if let Some(file) = args.pop() {
                let text = match fs::read_to_string(&file) {
                    Ok(s) => s,
                    Err(err) => {
                        eprintln!("nex lint: cannot read {file}: {err}");
                        return;
                    }
                };
                let result = compile_module(&text, CompileOptions {
                    source_path: file.clone(),
                    emit_metadata: false,
                    output_dir: None,
                    lib_names: HashSet::new(),
                    ..Default::default()
                });
                let default_file = nexc_ast::SourceFile::default();
                let ast_file = result.typed.as_ref().map(|t| &t.file).unwrap_or(&default_file);
                let lint_diags = nexc_lint::lint_all(ast_file);
                let all_diags: Vec<_> = result.diagnostics.iter()
                    .chain(lint_diags.iter())
                    .cloned()
                    .collect::<Vec<_>>();
                if all_diags.is_empty() {
                    println!("no lint issues in {file}");
                } else {
                    print_diagnostics(&all_diags, &result.source_map);
                }
            } else {
                eprintln!("nex lint: no file specified");
            }
        }
        "repl" => {
            nex_repl::run_repl();
        }
        "test" => {
            eprintln!("nex test: not implemented yet");
        }
        "install" => {
            if args.is_empty() {
                // No args: build-and-install toolchain (legacy behavior)
                let code = install_toolchain();
                if code != 0 {
                    process::exit(code);
                }
            } else {
                // nex install <user/repo[:version]> ...
                for spec in &args {
                    let code = cmd_install_lib(spec);
                    if code != 0 {
                        process::exit(code);
                    }
                }
            }
        }
        "uninstall" | "remove" => {
            if args.is_empty() {
                eprintln!("Usage: nex uninstall <lib-name>");
                process::exit(1);
            }
            for name in &args {
                let code = cmd_uninstall_lib(name);
                if code != 0 {
                    process::exit(code);
                }
            }
        }
        "list" | "ls" => {
            cmd_list_libs();
        }
        "new" => {
            let is_lib = args.iter().any(|a| a == "--lib");
            args.retain(|a| a != "--lib");
            let name = match args.pop() {
                Some(n) => n,
                None => {
                    eprintln!("nex new: missing project name");
                    eprintln!("Usage: nex new <name> [--lib]");
                    process::exit(1);
                }
            };
            let code = new_project(&name, is_lib);
            if code != 0 {
                process::exit(code);
            }
        }
        "clean" => {
            let dir = PathBuf::from("build");
            if dir.exists() {
                if let Err(e) = fs::remove_dir_all(&dir) {
                    eprintln!("nex clean: {e}");
                } else {
                    println!("cleaned build/");
                }
            } else {
                println!("nothing to clean");
            }
        }
        _ => {
            usage();
        }
    }
}

fn usage() {
    println!("Nex compiler toolchain\n");
    println!("Usage: nex <command> [args]\n");
    println!("Commands:");
    println!("  new <name> [--lib]          Create a new project (--lib for a library)");
    println!("  build [path]                Compile to a native executable (AOT)");
    println!("  build --lib [path]          Compile a library to a shared library (DLL)");
    println!("  run [path] [-- args]        JIT compile and execute");
    println!("  <file>.nex [args]           Shorthand for: nex run <file>.nex -- [args]");
    println!("  install <user/repo[:ver]>   Install a library from GitHub");
    println!("  install                     Build release binaries and install to nex/bin/");
    println!("  uninstall <name>            Remove a library from project.toml");
    println!("  list                        List project dependencies");
    println!("  fmt <file>                  Format source code");
    println!("  lint <file>                 Run linter");
    println!("  repl                        Interactive REPL");
    println!("  clean                       Remove build artifacts");
}

fn find_project_root(source_path: &Path) -> Option<PathBuf> {
    let mut dir = if source_path.is_file() {
        source_path.parent()?.to_path_buf()
    } else {
        source_path.to_path_buf()
    };
    loop {
        if dir.join("project.toml").exists() {
            // Canonicalize so that relative lib paths in project.toml
            // resolve correctly regardless of how the source path was given.
            return std::fs::canonicalize(&dir).ok().or(Some(dir));
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn lib_names_for_source(source_path: &Path) -> HashSet<String> {
    let Some(root) = find_project_root(source_path) else {
        return HashSet::new();
    };
    let mut sink = nexc_diag::DiagnosticSink::new();
    let names = discover_lib_names(&root, &mut sink);
    names.into_iter().collect()
}

fn build_single_module(source_path: &PathBuf) -> i32 {
    let source_path_text = source_path.to_string_lossy().to_string();
    let source_text = match fs::read_to_string(source_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("nex build: cannot read {}: {err}", source_path.display());
            return 1;
        }
    };

    let out_dir = PathBuf::from("build");
    let stem = source_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module")
        .to_string();

    let lib_names = lib_names_for_source(source_path);

    let mut result = compile_to_native(
        &source_text,
        CompileOptions {
            source_path: source_path_text.clone(),
            emit_metadata: true,
            output_dir: Some(out_dir.clone()),
            lib_names,
            ..Default::default()
        },
    );

    if has_errors(&result.diagnostics) {
        print_diagnostics(&result.diagnostics, &result.source_map);
        return 1;
    }

    print_warnings(&result.diagnostics, &result.source_map);

    match link_native(&mut result, &out_dir, &stem) {
        Ok(exe) => {
            println!("compiled {} -> {}", source_path_text, exe.display());
        }
        Err(e) => {
            eprintln!("nex build: link failed: {e}");
            return 1;
        }
    }

    0
}

fn build_project_modules(root: &PathBuf) -> i32 {
    let mut sink = nexc_diag::DiagnosticSink::new();
    let lib_names: HashSet<String> = discover_lib_names(root, &mut sink).into_iter().collect();

    let mut sink2 = nexc_diag::DiagnosticSink::new();
    let modules = discover_project_modules(root, &mut sink2);
    if !sink2.is_empty() {
        for d in sink2.diagnostics() {
            eprintln!("{d:?}");
        }
        return 1;
    }

    if modules.is_empty() {
        let main_au = root.join("src").join("main.nex");
        if main_au.is_file() {
            return build_single_module(&main_au);
        }
        eprintln!("nex build: no modules found under {}", root.join("src").display());
        return 1;
    }

    // Single-module shortcut: if there's only a main.nex and no library modules,
    // use the simpler single-module build path.
    let has_lib_modules = modules.keys().any(|k| lib_names.iter().any(|lib| k.starts_with(lib)));
    if !has_lib_modules {
        if let Some(main_path) = modules.iter()
            .find(|(_, p)| p.ends_with("main.nex"))
            .map(|(_, p)| p.clone())
        {
            return build_single_module(&main_path);
        }
    }

    let out_dir = root.join("build");
    let obj_ext = if cfg!(windows) { "obj" } else { "o" };
    let mut had_error = false;
    let mut obj_paths: Vec<PathBuf> = Vec::new();

    for (module, path) in &modules {
        let source_text = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(err) => {
                eprintln!("nex build: cannot read {}: {err}", path.display());
                had_error = true;
                continue;
            }
        };

        let source_path = path.to_string_lossy().to_string();
        let stem = module.replace('.', "_");

        let result = compile_to_native(
            &source_text,
            CompileOptions {
                source_path: source_path.clone(),
                emit_metadata: true,
                output_dir: Some(out_dir.clone()),
                lib_names: lib_names.clone(),
                ..Default::default()
            },
        );

        if has_errors(&result.diagnostics) {
            had_error = true;
            print_diagnostics(&result.diagnostics, &result.source_map);
            continue;
        }

        print_warnings(&result.diagnostics, &result.source_map);

        if let Some(object_bytes) = &result.object {
            let obj_path = out_dir.join(format!("{stem}.{obj_ext}"));
            if let Err(e) = fs::create_dir_all(&out_dir) {
                eprintln!("nex build: cannot create output directory: {e}");
                had_error = true;
                continue;
            }
            if let Err(e) = fs::write(&obj_path, object_bytes) {
                eprintln!("nex build: cannot write {}: {e}", obj_path.display());
                had_error = true;
                continue;
            }
            println!("  compiled {module}");
            obj_paths.push(obj_path);
        }
    }

    if had_error {
        return 1;
    }

    if obj_paths.is_empty() {
        eprintln!("nex build: no object files produced");
        return 1;
    }

    let exe_stem = modules.keys()
        .find(|k| k.ends_with("main") || *k == "main")
        .map(|k| k.replace('.', "_"))
        .unwrap_or_else(|| "output".to_string());

    match link_native_multi(&obj_paths, &out_dir, &exe_stem) {
        Ok(exe) => {
            println!("linked -> {}", exe.display());
        }
        Err(e) => {
            eprintln!("nex build: link failed: {e}");
            return 1;
        }
    }

    0
}

/// Build a Nex lib project into a shared library (DLL / .so / .dylib).
fn build_lib_project(root: &PathBuf) -> i32 {
    let toml_path = root.join("project.toml");
    let toml_text = match fs::read_to_string(&toml_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("nex build --lib: cannot read {}: {e}", toml_path.display());
            return 1;
        }
    };

    let lib_name = match parse_toml_name(&toml_text) {
        Some(n) => n,
        None => {
            eprintln!("nex build --lib: no `name` field in {}", toml_path.display());
            return 1;
        }
    };

    // Discover .nex sources under the lib's src/ directory
    let src_dir = root.join("src");
    if !src_dir.is_dir() {
        eprintln!("nex build --lib: no src/ directory in {}", root.display());
        return 1;
    }

    let mut nex_files = Vec::new();
    collect_nex_files(&src_dir, &mut nex_files);

    if nex_files.is_empty() {
        eprintln!("nex build --lib: no .nex files found under {}", src_dir.display());
        return 1;
    }

    let out_dir = root.join("build");
    let obj_ext = if cfg!(windows) { "obj" } else { "o" };
    let mut had_error = false;
    let mut obj_paths: Vec<PathBuf> = Vec::new();

    for path in &nex_files {
        let source_text = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(err) => {
                eprintln!("nex build --lib: cannot read {}: {err}", path.display());
                had_error = true;
                continue;
            }
        };

        let source_path = path.to_string_lossy().to_string();

        // Derive module name: <lib_name>.<relative_path_without_ext>
        let rel = path.strip_prefix(&src_dir).unwrap_or(path);
        let module = format!(
            "{}.{}",
            lib_name,
            rel.with_extension("")
                .to_string_lossy()
                .replace(std::path::MAIN_SEPARATOR, ".")
                .replace('/', ".")
        );
        let stem = module.replace('.', "_");

        let result = compile_to_native(
            &source_text,
            CompileOptions {
                source_path: source_path.clone(),
                emit_metadata: false,
                output_dir: Some(out_dir.clone()),
                output_kind: OutputKind::SharedLib,
                ..Default::default()
            },
        );

        if has_errors(&result.diagnostics) {
            had_error = true;
            print_diagnostics(&result.diagnostics, &result.source_map);
            continue;
        }

        print_warnings(&result.diagnostics, &result.source_map);

        if let Some(object_bytes) = &result.object {
            let obj_path = out_dir.join(format!("{stem}.{obj_ext}"));
            if let Err(e) = fs::create_dir_all(&out_dir) {
                eprintln!("nex build --lib: cannot create output directory: {e}");
                had_error = true;
                continue;
            }
            if let Err(e) = fs::write(&obj_path, object_bytes) {
                eprintln!("nex build --lib: cannot write {}: {e}", obj_path.display());
                had_error = true;
                continue;
            }
            println!("  compiled {module}");
            obj_paths.push(obj_path);
        }
    }

    if had_error {
        return 1;
    }

    if obj_paths.is_empty() {
        eprintln!("nex build --lib: no object files produced");
        return 1;
    }

    // Link into a shared library placed in the lib root
    match link_shared_lib_multi(&obj_paths, root, &lib_name) {
        Ok(lib_path) => {
            println!("linked -> {}", lib_path.display());
        }
        Err(e) => {
            eprintln!("nex build --lib: link failed: {e}");
            return 1;
        }
    }

    0
}

/// Parse `name = "..."` from the top-level of a project.toml string.
fn parse_toml_name(toml: &str) -> Option<String> {
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

/// Recursively collect all .nex files under a directory.
fn collect_nex_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_nex_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("nex") {
            out.push(path);
        }
    }
}

fn run_jit(source_path: &PathBuf, args: &[String]) -> i32 {
    let source_text = match fs::read_to_string(source_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("nex run: cannot read {}: {err}", source_path.display());
            return 1;
        }
    };

    // Step 1: Discover project libs and native DLLs (if project.toml exists).
    let project_root = find_project_root(source_path);
    let mut sink = nexc_diag::DiagnosticSink::new();
    let lib_names: HashSet<String> = project_root.as_ref()
        .map(|root| discover_lib_names(root, &mut sink).into_iter().collect())
        .unwrap_or_default();

    // Collect native library paths from project dependencies.
    let native_libs: Vec<PathBuf> = project_root.as_ref()
        .map(|root| discover_native_libs(root, &mut sink))
        .unwrap_or_default();

    for d in sink.diagnostics() {
        if matches!(d.severity, Severity::Error) {
            eprintln!("{d:?}");
        }
    }

    // Step 2: Discover sibling .nex files next to the source file.
    let base_dir = source_path.parent()
        .map(|p| if p.as_os_str().is_empty() { Path::new(".") } else { p })
        .unwrap_or(Path::new("."));
    let canonicalized_base = fs::canonicalize(base_dir).unwrap_or_else(|_| base_dir.to_path_buf());

    let mut sibling_sink = nexc_diag::DiagnosticSink::new();
    let sibling_modules = discover_sibling_modules(
        &canonicalized_base,
        Some(source_path),
        &mut sibling_sink,
    );

    for d in sibling_sink.diagnostics() {
        if matches!(d.severity, Severity::Error) {
            eprintln!("{d:?}");
        }
    }

    // Step 3: Discover lib modules from project.toml.
    let mut lib_modules: std::collections::HashMap<String, PathBuf> = std::collections::HashMap::new();
    if !lib_names.is_empty() {
        if let Some(ref root) = project_root {
            let mut project_sink = nexc_diag::DiagnosticSink::new();
            let project_modules = discover_project_modules(root, &mut project_sink);
            for (name, path) in project_modules {
                let is_lib = lib_names.iter().any(|lib| name.starts_with(lib));
                if is_lib {
                    lib_modules.insert(name, path);
                }
            }
        }
    }

    // Step 4: Merge all discovered modules (libs take precedence).
    let mut all_modules: std::collections::HashMap<String, PathBuf> = std::collections::HashMap::new();
    all_modules.extend(sibling_modules);
    all_modules.extend(lib_modules);

    // Step 5: If no extra modules, use single-module fast path.
    if all_modules.is_empty() {
        let options = CompileOptions {
            source_path: source_path.to_string_lossy().to_string(),
            emit_metadata: false,
            output_dir: None,
            lib_names,
            native_libs,
        };
        return match jit_run(&source_text, options, args) {
            Ok(code) => code,
            Err(e) => {
                eprintln!("nex run: {e}");
                1
            }
        };
    }

    // Step 6: Multi-module JIT — compile all discovered modules + main.
    // Build the set of known module prefixes so the import validator
    // treats sibling imports as known (not "missing module" errors).
    let original_lib_names = lib_names;
    let mut all_known_names = original_lib_names.clone();
    for name in all_modules.keys() {
        let root_prefix = name.split('.').next().unwrap_or(name);
        all_known_names.insert(root_prefix.to_string());
    }

    // Read and collect module sources, using relative paths for siblings
    // so canonical_module_name() derives correct module names.
    let mut sibling_texts: Vec<(String, String)> = Vec::new();
    let mut lib_texts: Vec<(String, String)> = Vec::new();

    for (module_name, path) in &all_modules {
        let text = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(err) => {
                eprintln!("nex run: cannot read {}: {err}", path.display());
                return 1;
            }
        };

        let is_lib = original_lib_names.iter().any(|lib| module_name.starts_with(lib));

        if is_lib {
            lib_texts.push((text, path.to_string_lossy().to_string()));
        } else {
            // Sibling module: use path relative to base_dir.
            let relative = path.strip_prefix(&canonicalized_base)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| path.to_string_lossy().to_string());
            sibling_texts.push((text, relative));
        }
    }

    let mut compile_sources: Vec<(&str, CompileOptions)> = Vec::new();
    for (text, path) in &sibling_texts {
        compile_sources.push((text.as_str(), CompileOptions {
            source_path: path.clone(),
            emit_metadata: false,
            output_dir: None,
            lib_names: all_known_names.clone(),
            native_libs: Vec::new(),
        }));
    }
    for (text, path) in &lib_texts {
        compile_sources.push((text.as_str(), CompileOptions {
            source_path: path.clone(),
            emit_metadata: false,
            output_dir: None,
            lib_names: all_known_names.clone(),
            native_libs: Vec::new(),
        }));
    }

    // Main module last, with path relative to base_dir.
    let main_relative = source_path.strip_prefix(&canonicalized_base)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| source_path.to_string_lossy().to_string());

    compile_sources.push((&source_text, CompileOptions {
        source_path: main_relative,
        emit_metadata: false,
        output_dir: None,
        lib_names: all_known_names,
        native_libs: Vec::new(),
    }));

    match jit_run_multi(&compile_sources, args, &native_libs) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("nex run: {e}");
            1
        }
    }
}


fn has_errors(diagnostics: &[Diagnostic]) -> bool {
    diagnostics.iter().any(|d| matches!(d.severity, Severity::Error))
}

fn print_diagnostics(diagnostics: &[Diagnostic], source_map: &SourceMap) {
    for d in diagnostics {
        eprint!("{}", d.render(source_map));
    }
}

fn print_warnings(diagnostics: &[Diagnostic], source_map: &SourceMap) {
    for d in diagnostics {
        if matches!(d.severity, Severity::Warning) {
            eprint!("{}", d.render(source_map));
        }
    }
}

// ---------------------------------------------------------------------------
// nex new
// ---------------------------------------------------------------------------

fn new_project(name: &str, is_lib: bool) -> i32 {
    let root = PathBuf::from(name);
    if root.exists() {
        eprintln!("nex new: directory `{name}` already exists");
        return 1;
    }

    let src_dir = root.join("src");
    if let Err(e) = fs::create_dir_all(&src_dir) {
        eprintln!("nex new: cannot create directories: {e}");
        return 1;
    }

    let project_toml = if is_lib {
        format!(
            "name = \"{name}\"\nversion = \"0.1.0\"\n"
        )
    } else {
        format!(
            "name = \"{name}\"\nversion = \"0.1.0\"\nentry = \"src/main.nex\"\n"
        )
    };

    if let Err(e) = fs::write(root.join("project.toml"), &project_toml) {
        eprintln!("nex new: cannot write project.toml: {e}");
        return 1;
    }

    let source_file = if is_lib {
        (
            src_dir.join("lib.nex"),
            concat!(
                "public def hello() -> String {\n",
                "    return \"hello from library\"\n",
                "}\n",
            ).to_string(),
        )
    } else {
        (
            src_dir.join("main.nex"),
            concat!(
                "def main() -> Unit {\n",
                "    println(\"hello, world\")\n",
                "    return\n",
                "}\n",
            ).to_string(),
        )
    };

    if let Err(e) = fs::write(&source_file.0, &source_file.1) {
        eprintln!("nex new: cannot write {}: {e}", source_file.0.display());
        return 1;
    }

    let kind = if is_lib { "library" } else { "application" };
    println!("created {kind} `{name}`\n");
    println!("  {}/", name);
    println!("    project.toml");
    println!("    src/");
    println!("      {}", source_file.0.file_name().unwrap().to_string_lossy());

    if !is_lib {
        println!("\nrun it with:  cd {name} && nex run");
    }

    0
}

// ---------------------------------------------------------------------------
// Package management: install / uninstall / list
// ---------------------------------------------------------------------------

/// Parse a GitHub install specifier into (owner, repo, version).
/// Formats:
///   user/repo          -> (user, repo, None)
///   user/repo:1.0.0    -> (user, repo, Some("1.0.0"))
///   user/repo:v1.0.0   -> (user, repo, Some("v1.0.0"))
fn parse_install_spec(spec: &str) -> Result<(String, String, Option<String>), String> {
    let (slug, version) = if let Some((s, v)) = spec.split_once(':') {
        (s, Some(v.to_string()))
    } else {
        (spec, None)
    };

    let parts: Vec<&str> = slug.split('/').collect();
    if parts.len() != 2 || parts[0].is_empty() || parts[1].is_empty() {
        return Err(format!(
            "invalid specifier `{spec}`. Expected format: user/repo or user/repo:version"
        ));
    }

    Ok((parts[0].to_string(), parts[1].to_string(), version))
}

/// Query available tags from a GitHub repo via `git ls-remote --tags`.
/// Returns tags sorted in reverse semver order (newest first).
fn query_github_tags(owner: &str, repo: &str) -> Result<Vec<String>, String> {
    let url = format!("https://github.com/{owner}/{repo}.git");
    let output = process::Command::new("git")
        .args(["ls-remote", "--tags", "--sort=-v:refname", &url])
        .output()
        .map_err(|e| format!("failed to run git: {e}. Is git installed?"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("git ls-remote failed: {stderr}"));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut tags = Vec::new();
    for line in stdout.lines() {
        // Format: <hash>\trefs/tags/<name>
        if let Some(refname) = line.split('\t').nth(1) {
            let tag = refname
                .strip_prefix("refs/tags/")
                .unwrap_or(refname);
            // Skip annotated tag dereferences (^{})
            if tag.ends_with("^{}") {
                continue;
            }
            tags.push(tag.to_string());
        }
    }

    Ok(tags)
}

/// Clone a specific tag from GitHub into a temp directory, then move
/// the source (without .git) into the global cache.
fn download_lib_source(
    owner: &str,
    repo: &str,
    tag: &str,
    cache_dir: &Path,
) -> Result<(), String> {
    let url = format!("https://github.com/{owner}/{repo}.git");

    // Clone to a temp dir first
    let temp_dir = cache_dir.parent().unwrap_or(cache_dir).join(".nex_download_tmp");
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir)
            .map_err(|e| format!("cannot clean temp dir: {e}"))?;
    }

    println!("  cloning {owner}/{repo} @ {tag} ...");
    let status = process::Command::new("git")
        .args([
            "clone",
            "--depth", "1",
            "--branch", tag,
            &url,
            &temp_dir.to_string_lossy(),
        ])
        .stdout(process::Stdio::null())
        .stderr(process::Stdio::piped())
        .status()
        .map_err(|e| format!("failed to run git clone: {e}"))?;

    if !status.success() {
        let _ = fs::remove_dir_all(&temp_dir);
        return Err(format!("git clone failed for {owner}/{repo} @ {tag}"));
    }

    // Remove .git directory to save space
    let git_dir = temp_dir.join(".git");
    if git_dir.exists() {
        let _ = fs::remove_dir_all(&git_dir);
    }

    // Move to final cache location
    if cache_dir.exists() {
        fs::remove_dir_all(cache_dir)
            .map_err(|e| format!("cannot clear cache dir: {e}"))?;
    }
    if let Some(parent) = cache_dir.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| format!("cannot create cache dir: {e}"))?;
    }
    fs::rename(&temp_dir, cache_dir)
        .or_else(|_| {
            // rename may fail across filesystems, fall back to copy
            copy_dir_recursive(&temp_dir, cache_dir)?;
            fs::remove_dir_all(&temp_dir)
                .map_err(|e| format!("cannot clean temp dir: {e}"))
        })
        .map_err(|e| format!("cannot move to cache: {e}"))?;

    Ok(())
}

/// Try to download a native DLL from GitHub release assets.
fn download_native_dll(
    owner: &str,
    repo: &str,
    tag: &str,
    native_name: &str,
    dest_dir: &Path,
) -> Result<bool, String> {
    // Determine platform-specific asset name patterns to look for.
    let candidates: Vec<String> = if cfg!(target_os = "windows") {
        vec![
            format!("{native_name}.dll"),
            format!("{native_name}-windows.dll"),
            format!("{native_name}-win64.dll"),
        ]
    } else if cfg!(target_os = "macos") {
        vec![
            format!("lib{native_name}.dylib"),
            format!("{native_name}-macos.dylib"),
            format!("{native_name}-darwin.dylib"),
        ]
    } else {
        vec![
            format!("lib{native_name}.so"),
            format!("{native_name}-linux.so"),
        ]
    };

    // Query release assets via GitHub API using curl
    let api_url = format!(
        "https://api.github.com/repos/{owner}/{repo}/releases/tags/{tag}"
    );
    let output = process::Command::new("curl")
        .args(["-sL", "-H", "Accept: application/vnd.github+json", &api_url])
        .output()
        .map_err(|e| format!("failed to run curl: {e}"))?;

    if !output.status.success() {
        // No release for this tag — not an error, just no DLL available
        return Ok(false);
    }

    let body = String::from_utf8_lossy(&output.stdout);
    let json: serde_json::Value = match serde_json::from_str(&body) {
        Ok(v) => v,
        Err(_) => return Ok(false), // Not valid JSON / no release
    };

    let assets = match json.get("assets").and_then(|a| a.as_array()) {
        Some(a) => a,
        None => return Ok(false),
    };

    // Look for a matching asset
    for asset in assets {
        let asset_name = asset
            .get("name")
            .and_then(|n| n.as_str())
            .unwrap_or("");
        let download_url = asset
            .get("browser_download_url")
            .and_then(|u| u.as_str())
            .unwrap_or("");

        if candidates.iter().any(|c| c == asset_name) && !download_url.is_empty() {
            let dest_path = dest_dir.join(asset_name);
            println!("  downloading native DLL: {asset_name} ...");
            let status = process::Command::new("curl")
                .args(["-sL", "-o", &dest_path.to_string_lossy(), download_url])
                .status()
                .map_err(|e| format!("curl download failed: {e}"))?;

            if status.success() && dest_path.exists() {
                println!("  saved to {}", dest_path.display());
                return Ok(true);
            }
        }
    }

    Ok(false)
}

/// Read the `native` field from a project.toml file.
fn read_native_field(project_toml: &Path) -> Option<String> {
    let text = fs::read_to_string(project_toml).ok()?;
    let mut in_section = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_section = true;
            continue;
        }
        if in_section {
            continue;
        }
        if let Some((key, value)) = trimmed.split_once('=') {
            if key.trim() == "native" {
                let v = value.trim().trim_matches('"').trim_matches('\'');
                if !v.is_empty() {
                    return Some(v.to_string());
                }
            }
        }
    }
    None
}

/// Read the `name` field from a project.toml file.
fn read_project_name(project_toml: &Path) -> Option<String> {
    let text = fs::read_to_string(project_toml).ok()?;
    let mut in_section = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_section = true;
            continue;
        }
        if in_section {
            continue;
        }
        if let Some((key, value)) = trimmed.split_once('=') {
            if key.trim() == "name" {
                let v = value.trim().trim_matches('"').trim_matches('\'');
                if !v.is_empty() {
                    return Some(v.to_string());
                }
            }
        }
    }
    None
}

/// Ensure the consuming project's `project.toml` has the given lib entry.
fn add_lib_to_project_toml(project_toml: &Path, name: &str, version: &str, git_source: &str) -> Result<(), String> {
    let text = fs::read_to_string(project_toml)
        .map_err(|e| format!("cannot read {}: {e}", project_toml.display()))?;

    // Check if library is already listed
    let mut in_libs = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_libs = trimmed == "[libs]";
            continue;
        }
        if !in_libs { continue; }
        if let Some((key, _)) = trimmed.split_once('=') {
            if key.trim() == name {
                // Already present — update the version
                let old_line = line;
                let new_entry = format!("{name} = {{ git = \"{git_source}\", version = \"{version}\" }}");
                let updated = text.replace(old_line, &new_entry);
                fs::write(project_toml, &updated)
                    .map_err(|e| format!("cannot write {}: {e}", project_toml.display()))?;
                return Ok(());
            }
        }
    }

    // Not found — append entry
    let entry = format!("{name} = {{ git = \"{git_source}\", version = \"{version}\" }}");

    if text.contains("[libs]") {
        // Find the [libs] section and append after it
        let mut result = String::new();
        let mut inserted = false;
        let mut in_libs_section = false;
        for line in text.lines() {
            result.push_str(line);
            result.push('\n');
            let trimmed = line.trim();
            if trimmed == "[libs]" {
                in_libs_section = true;
                continue;
            }
            if in_libs_section && !inserted {
                // Find the right place to insert (after last entry in [libs])
                if trimmed.starts_with('[') || trimmed.is_empty() {
                    // We've moved past the libs section or hit a blank line
                    // Insert before this line
                }
                // Keep going until we see the next section or EOF
                if trimmed.starts_with('[') && trimmed != "[libs]" {
                    // Insert before this section header
                    let last_newline = result.len() - line.len() - 1;
                    result.insert_str(last_newline, &format!("{entry}\n"));
                    inserted = true;
                    in_libs_section = false;
                }
            }
        }
        if !inserted {
            // Append at end of file (libs section is last)
            result.push_str(&entry);
            result.push('\n');
        }
        fs::write(project_toml, &result)
            .map_err(|e| format!("cannot write {}: {e}", project_toml.display()))?;
    } else {
        // No [libs] section exists — add one
        let mut result = text.clone();
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result.push_str(&format!("\n[libs]\n{entry}\n"));
        fs::write(project_toml, &result)
            .map_err(|e| format!("cannot write {}: {e}", project_toml.display()))?;
    }

    Ok(())
}

/// `nex install user/repo[:version]`
fn cmd_install_lib(spec: &str) -> i32 {
    let (owner, repo, requested_version) = match parse_install_spec(spec) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("nex install: {e}");
            return 1;
        }
    };

    // Ensure we're in a project
    let project_toml = PathBuf::from("project.toml");
    if !project_toml.exists() {
        eprintln!("nex install: no project.toml found in current directory");
        eprintln!("hint: run `nex new <name>` first, or cd into a project directory");
        return 1;
    }

    // Determine the tag to use
    let tag = match &requested_version {
        Some(v) => v.clone(),
        None => {
            println!("querying tags for {owner}/{repo} ...");
            match query_github_tags(&owner, &repo) {
                Ok(tags) if !tags.is_empty() => {
                    let latest = tags[0].clone();
                    println!("  latest tag: {latest}");
                    latest
                }
                Ok(_) => {
                    eprintln!("nex install: no tags found for {owner}/{repo}");
                    eprintln!("hint: specify a branch or tag with {owner}/{repo}:main");
                    return 1;
                }
                Err(e) => {
                    eprintln!("nex install: {e}");
                    return 1;
                }
            }
        }
    };

    // Derive a clean version string (strip leading 'v' for cache dir naming)
    let version = tag.strip_prefix('v').unwrap_or(&tag).to_string();
    let git_source = format!("{owner}/{repo}");

    // Check global cache first
    let cache_dir = nex_libs_dir().join(&repo).join(&version);
    if resolve_from_global_cache(&repo, &version).is_some() {
        println!("{repo} v{version} is already cached");
    } else {
        // Download source
        fs::create_dir_all(cache_dir.parent().unwrap_or(&cache_dir))
            .unwrap_or_default();

        if let Err(e) = download_lib_source(&owner, &repo, &tag, &cache_dir) {
            eprintln!("nex install: {e}");
            return 1;
        }
        println!("  cached at {}", cache_dir.display());
    }

    // Read the library name from its project.toml (may differ from repo name)
    let lib_project_toml = cache_dir.join("project.toml");
    let lib_name = if lib_project_toml.exists() {
        read_project_name(&lib_project_toml).unwrap_or_else(|| repo.clone())
    } else {
        eprintln!("warning: no project.toml found in downloaded library");
        repo.clone()
    };

    // Check for native DLL
    if let Some(native_name) = read_native_field(&lib_project_toml) {
        // Check if DLL already exists in cache
        let dll_exists = if cfg!(target_os = "windows") {
            cache_dir.join(format!("{native_name}.dll")).exists()
        } else if cfg!(target_os = "macos") {
            cache_dir.join(format!("lib{native_name}.dylib")).exists()
                || cache_dir.join(format!("lib{native_name}.so")).exists()
        } else {
            cache_dir.join(format!("lib{native_name}.so")).exists()
        };

        if !dll_exists {
            println!("  library requires native DLL: {native_name}");
            match download_native_dll(&owner, &repo, &tag, &native_name, &cache_dir) {
                Ok(true) => println!("  native DLL downloaded successfully"),
                Ok(false) => {
                    println!("  note: no prebuilt native DLL found in GitHub release assets");
                    println!("  you may need to build it manually: cargo build -p {native_name} --release");
                }
                Err(e) => {
                    println!("  warning: failed to download native DLL: {e}");
                }
            }
        }
    }

    // Update project.toml
    if let Err(e) = add_lib_to_project_toml(&project_toml, &lib_name, &version, &git_source) {
        eprintln!("nex install: {e}");
        return 1;
    }

    println!("\ninstalled {lib_name} v{version} from {git_source}");
    0
}

/// `nex uninstall <name>`
fn cmd_uninstall_lib(name: &str) -> i32 {
    let project_toml = PathBuf::from("project.toml");
    if !project_toml.exists() {
        eprintln!("nex uninstall: no project.toml found in current directory");
        return 1;
    }

    let text = match fs::read_to_string(&project_toml) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("nex uninstall: cannot read project.toml: {e}");
            return 1;
        }
    };

    let mut result = String::new();
    let mut in_libs = false;
    let mut found = false;

    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_libs = trimmed == "[libs]";
        }
        if in_libs {
            if let Some((key, _)) = trimmed.split_once('=') {
                if key.trim() == name {
                    found = true;
                    continue; // Skip this line
                }
            }
        }
        result.push_str(line);
        result.push('\n');
    }

    if !found {
        eprintln!("nex uninstall: `{name}` not found in [libs]");
        return 1;
    }

    if let Err(e) = fs::write(&project_toml, &result) {
        eprintln!("nex uninstall: cannot write project.toml: {e}");
        return 1;
    }

    println!("removed `{name}` from project.toml");
    println!("note: cached files in ~/.nex/libs/ are kept (shared by other projects)");
    0
}

/// `nex list`
fn cmd_list_libs() {
    let project_toml = PathBuf::from("project.toml");
    if !project_toml.exists() {
        eprintln!("nex list: no project.toml found in current directory");
        process::exit(1);
    }

    let text = match fs::read_to_string(&project_toml) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("nex list: cannot read project.toml: {e}");
            process::exit(1);
        }
    };

    let mut in_libs = false;
    let mut count = 0;

    println!("dependencies:");
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_libs = trimmed == "[libs]";
            continue;
        }
        if !in_libs || trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        if let Some((key, value)) = trimmed.split_once('=') {
            let name = key.trim();
            let value = value.trim();

            // Determine source type
            if let Some(inner) = value.strip_prefix('{').and_then(|v| v.strip_suffix('}')) {
                let fields: HashMap<&str, &str> = inner
                    .split(',')
                    .filter_map(|part| {
                        let (k, v) = part.split_once('=')?;
                        Some((k.trim(), v.trim().trim_matches('"').trim_matches('\'')))
                    })
                    .collect();

                if let Some(path) = fields.get("path") {
                    print!("  {name}");
                    if let Some(ver) = fields.get("version") {
                        print!(" v{ver}");
                    }
                    println!("  (path: {path})");
                } else if let Some(git) = fields.get("git") {
                    print!("  {name}");
                    if let Some(ver) = fields.get("version") {
                        print!(" v{ver}");
                    }
                    println!("  (git: {git})");
                } else {
                    println!("  {name}  {value}");
                }
            } else {
                let v = value.trim_matches('"').trim_matches('\'');
                let is_version = v.chars().next().map_or(false, |c| c.is_ascii_digit());
                if is_version {
                    println!("  {name} v{v}  (cached)");
                } else {
                    println!("  {name}  (path: {v})");
                }
            }
            count += 1;
        }
    }

    if count == 0 {
        println!("  (none)");
    }
}

// ---------------------------------------------------------------------------
// Toolchain install (legacy `nex install` with no arguments)
// ---------------------------------------------------------------------------

fn install_toolchain() -> i32 {
    let workspace_root = match find_workspace_root() {
        Some(r) => r,
        None => {
            eprintln!("nex install: cannot locate workspace root (Cargo.toml)");
            return 1;
        }
    };

    let nex_dir = workspace_root.join("nex");
    let bin_dir = nex_dir.join("bin");
    let config_path = nex_dir.join("config").join("nex.toml");

    let new_version = env!("CARGO_PKG_VERSION").to_string();

    // 1. Build release binaries
    println!("building release binaries...");
    let status = process::Command::new("cargo")
        .args(["build", "--release", "-p", "nex", "-p", "nexc", "-p", "nex_lsp"])
        .current_dir(&workspace_root)
        .status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => {
            eprintln!("nex install: cargo build failed (exit {})", s);
            return 1;
        }
        Err(e) => {
            eprintln!("nex install: cannot run cargo: {e}");
            return 1;
        }
    }

    let release_dir = workspace_root.join("target").join("release");

    let exe_ext = if cfg!(windows) { ".exe" } else { "" };
    let binaries: Vec<(&str, String)> = vec![
        ("nex", format!("nex{exe_ext}")),
        ("nexc", format!("nexc{exe_ext}")),
        ("nex-lsp", format!("nex-lsp{exe_ext}")),
    ];

    // Verify all binaries exist
    for (_, filename) in &binaries {
        let src = release_dir.join(filename);
        if !src.exists() {
            eprintln!("nex install: expected binary not found: {}", src.display());
            return 1;
        }
    }

    // 2. Back up existing bin/ if it has files
    if bin_dir.is_dir() {
        let old_version = read_toml_version(&config_path).unwrap_or_else(|| "unknown".into());
        let backup_dir = nex_dir.join(&old_version).join("bin");

        let has_files = fs::read_dir(&bin_dir)
            .map(|rd| rd.count() > 0)
            .unwrap_or(false);

        if has_files {
            println!("backing up nex/bin/ -> nex/{old_version}/bin/");
            if let Err(e) = copy_dir_recursive(&bin_dir, &backup_dir) {
                eprintln!("nex install: backup failed: {e}");
                return 1;
            }
        }
    }

    // 3. Copy new binaries into nex/bin/
    if let Err(e) = fs::create_dir_all(&bin_dir) {
        eprintln!("nex install: cannot create nex/bin/: {e}");
        return 1;
    }

    for (label, filename) in &binaries {
        let src = release_dir.join(filename);
        let dst = bin_dir.join(filename);
        print!("  {label} ... ");
        match fs::copy(&src, &dst) {
            Ok(bytes) => println!("ok ({} KB)", bytes / 1024),
            Err(e) => {
                println!("FAILED");
                eprintln!("nex install: cannot copy {}: {e}", src.display());
                return 1;
            }
        }
    }

    // 4. Update version in nex/config/nex.toml
    if config_path.exists() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            let updated = update_toml_version(&contents, &new_version);
            if let Err(e) = fs::write(&config_path, &updated) {
                eprintln!("nex install: cannot update config: {e}");
            }
        }
    }

    println!("\ninstalled Nex v{new_version} to {}", bin_dir.display());
    println!("add this to your PATH:  {}", bin_dir.display());
    0
}

fn find_workspace_root() -> Option<PathBuf> {
    // Walk up from the current exe or cwd looking for Cargo.toml with [workspace]
    let start = env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
        .unwrap_or_else(|| PathBuf::from("."));

    for dir in [start.as_path(), Path::new(".")].iter() {
        let mut current = dir.to_path_buf();
        if !current.is_absolute() {
            current = env::current_dir().unwrap_or_default().join(&current);
        }
        loop {
            let candidate = current.join("Cargo.toml");
            if candidate.exists() {
                if let Ok(text) = fs::read_to_string(&candidate) {
                    if text.contains("[workspace]") {
                        return Some(current);
                    }
                }
            }
            if !current.pop() {
                break;
            }
        }
    }
    None
}

fn read_toml_version(path: &Path) -> Option<String> {
    let text = fs::read_to_string(path).ok()?;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("version") && trimmed.contains('=') {
            let val = trimmed.split('=').nth(1)?.trim().trim_matches('"');
            return Some(val.to_string());
        }
    }
    None
}

fn update_toml_version(contents: &str, new_version: &str) -> String {
    let mut out = String::new();
    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("version") && trimmed.contains('=') {
            out.push_str(&format!("version = \"{new_version}\"\n"));
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<(), String> {
    fs::create_dir_all(dst).map_err(|e| format!("mkdir {}: {e}", dst.display()))?;
    let entries = fs::read_dir(src).map_err(|e| format!("read {}: {e}", src.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("entry: {e}"))?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)
                .map_err(|e| format!("copy {} -> {}: {e}", src_path.display(), dst_path.display()))?;
        }
    }
    Ok(())
}
