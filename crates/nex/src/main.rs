use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use nexc_diag::{Diagnostic, Severity, SourceMap};
use nexc_driver::{compile_module, compile_to_native, link_native, link_native_multi, jit_run, CompileOptions};
use nexc_fmt::format_source;
use nexc_resolve::{discover_project_modules, discover_lib_names};

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        usage();
        return;
    }

    // Shorthand: `aur file.nex [args...]` -> JIT run
    if args[0].ends_with(".nex") {
        let source_path = PathBuf::from(args.remove(0));
        let code = run_jit(&source_path, &args);
        process::exit(code);
    }

    let command = args.remove(0);
    match command.as_str() {
        "build" => {
            let target = args.pop().unwrap_or_else(|| ".".to_string());
            let target_path = PathBuf::from(&target);

            let code = if target_path.is_file() {
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

            // Split args at "--": before is for aur, after is for the program
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
                        eprintln!("aur lint: cannot read {file}: {err}");
                        return;
                    }
                };
                let result = compile_module(&text, CompileOptions {
                    source_path: file.clone(),
                    emit_metadata: false,
                    output_dir: None,
                    lib_names: HashSet::new(),
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
                eprintln!("aur lint: no file specified");
            }
        }
        "repl" => {
            nex_repl::run_repl();
        }
        "test" => {
            eprintln!("aur test: not implemented yet");
        }
        "install" => {
            let code = install_toolchain();
            if code != 0 {
                process::exit(code);
            }
        }
        "new" => {
            let is_lib = args.iter().any(|a| a == "--lib");
            args.retain(|a| a != "--lib");
            let name = match args.pop() {
                Some(n) => n,
                None => {
                    eprintln!("aur new: missing project name");
                    eprintln!("Usage: aur new <name> [--lib]");
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
                    eprintln!("aur clean: {e}");
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
    println!("Usage: aur <command> [args]\n");
    println!("Commands:");
    println!("  new <name> [--lib]    Create a new project (--lib for a library)");
    println!("  build [path]          Compile to a native executable (AOT)");
    println!("  run [path] [-- args]  JIT compile and execute");
    println!("  <file>.nex [args]     Shorthand for: aur run <file>.nex -- [args]");
    println!("  install               Build release binaries and install to nex/bin/");
    println!("  fmt <file>            Format source code");
    println!("  lint <file>           Run linter");
    println!("  repl                  Interactive REPL");
    println!("  clean                 Remove build artifacts");
}

fn find_project_root(source_path: &Path) -> Option<PathBuf> {
    let mut dir = if source_path.is_file() {
        source_path.parent()?.to_path_buf()
    } else {
        source_path.to_path_buf()
    };
    loop {
        if dir.join("project.toml").exists() {
            return Some(dir);
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
            eprintln!("aur build: cannot read {}: {err}", source_path.display());
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
            eprintln!("aur build: link failed: {e}");
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
        eprintln!("aur build: no modules found under {}", root.join("src").display());
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
                eprintln!("aur build: cannot read {}: {err}", path.display());
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
                eprintln!("aur build: cannot create output directory: {e}");
                had_error = true;
                continue;
            }
            if let Err(e) = fs::write(&obj_path, object_bytes) {
                eprintln!("aur build: cannot write {}: {e}", obj_path.display());
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
        eprintln!("aur build: no object files produced");
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
            eprintln!("aur build: link failed: {e}");
            return 1;
        }
    }

    0
}

fn run_jit(source_path: &PathBuf, args: &[String]) -> i32 {
    let source_path_text = source_path.to_string_lossy().to_string();
    let source_text = match fs::read_to_string(source_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("aur run: cannot read {}: {err}", source_path.display());
            return 1;
        }
    };

    let lib_names = lib_names_for_source(source_path);

    let options = CompileOptions {
        source_path: source_path_text,
        emit_metadata: false,
        output_dir: None,
        lib_names,
    };

    match jit_run(&source_text, options, args) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("aur run: {e}");
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
// aur new
// ---------------------------------------------------------------------------

fn new_project(name: &str, is_lib: bool) -> i32 {
    let root = PathBuf::from(name);
    if root.exists() {
        eprintln!("aur new: directory `{name}` already exists");
        return 1;
    }

    let src_dir = root.join("src");
    if let Err(e) = fs::create_dir_all(&src_dir) {
        eprintln!("aur new: cannot create directories: {e}");
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
        eprintln!("aur new: cannot write project.toml: {e}");
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
        eprintln!("aur new: cannot write {}: {e}", source_file.0.display());
        return 1;
    }

    let kind = if is_lib { "library" } else { "application" };
    println!("created {kind} `{name}`\n");
    println!("  {}/", name);
    println!("    project.toml");
    println!("    src/");
    println!("      {}", source_file.0.file_name().unwrap().to_string_lossy());

    if !is_lib {
        println!("\nrun it with:  cd {name} && aur run");
    }

    0
}

// ---------------------------------------------------------------------------
// aur install
// ---------------------------------------------------------------------------

fn install_toolchain() -> i32 {
    let workspace_root = match find_workspace_root() {
        Some(r) => r,
        None => {
            eprintln!("aur install: cannot locate workspace root (Cargo.toml)");
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
            eprintln!("aur install: cargo build failed (exit {})", s);
            return 1;
        }
        Err(e) => {
            eprintln!("aur install: cannot run cargo: {e}");
            return 1;
        }
    }

    let release_dir = workspace_root.join("target").join("release");

    let exe_ext = if cfg!(windows) { ".exe" } else { "" };
    let binaries: Vec<(&str, String)> = vec![
        ("aur", format!("aur{exe_ext}")),
        ("aurc", format!("aurc{exe_ext}")),
        ("aur-lsp", format!("aur-lsp{exe_ext}")),
    ];

    // Verify all binaries exist
    for (_, filename) in &binaries {
        let src = release_dir.join(filename);
        if !src.exists() {
            eprintln!("aur install: expected binary not found: {}", src.display());
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
                eprintln!("aur install: backup failed: {e}");
                return 1;
            }
        }
    }

    // 3. Copy new binaries into nex/bin/
    if let Err(e) = fs::create_dir_all(&bin_dir) {
        eprintln!("aur install: cannot create nex/bin/: {e}");
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
                eprintln!("aur install: cannot copy {}: {e}", src.display());
                return 1;
            }
        }
    }

    // 4. Update version in nex/config/nex.toml
    if config_path.exists() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            let updated = update_toml_version(&contents, &new_version);
            if let Err(e) = fs::write(&config_path, &updated) {
                eprintln!("aur install: cannot update config: {e}");
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
