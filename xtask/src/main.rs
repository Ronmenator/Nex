use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

const BINARIES: &[&str] = &["nex", "nexc", "nex-lsp"];

fn project_root() -> PathBuf {
    for start in workspace_root_search_starts() {
        if let Some(root) = find_workspace_root(&start) {
            return root;
        }
    }

    panic!(
        "failed to locate workspace root; expected a directory containing Cargo.toml and xtask/Cargo.toml"
    );
}

fn workspace_root_search_starts() -> Vec<PathBuf> {
    let mut starts = Vec::new();

    // Prefer runtime paths first so binaries still work if the workspace was moved.
    if let Ok(cwd) = env::current_dir() {
        starts.push(cwd);
    }

    // Keep compile-time manifest path as a fallback.
    starts.push(PathBuf::from(env!("CARGO_MANIFEST_DIR")));

    // Running from target/<profile>/xtask(.exe): walk ancestors to find workspace root.
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            starts.push(exe_dir.to_path_buf());
        }
    }

    starts
}

fn find_workspace_root(start: &Path) -> Option<PathBuf> {
    for dir in start.ancestors() {
        if dir.join("Cargo.toml").is_file() && dir.join("xtask").join("Cargo.toml").is_file() {
            return Some(dir.to_path_buf());
        }
    }
    None
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("deploy") => deploy(&args[1..]),
        Some("release") => release(&args[1..]),
        Some("package-libs") => package_libs_for_registry(),
        Some(cmd) => {
            eprintln!("error: unknown xtask command `{cmd}`");
            print_usage();
            process::exit(1);
        }
        None => {
            print_usage();
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!("usage: cargo xtask <command> [options]");
    eprintln!();
    eprintln!("commands:");
    eprintln!("  deploy         Build and deploy locally");
    eprintln!("  release        Package and publish a GitHub release");
    eprintln!("  package-libs   Package libs into docs/libs/ for registry");
    eprintln!();
    eprintln!("deploy options:");
    eprintln!("  --patch   bump patch version (default): 0.1.0 -> 0.1.1");
    eprintln!("  --minor   bump minor version:           0.1.0 -> 0.2.0");
    eprintln!("  --major   bump major version:           0.1.0 -> 1.0.0");
    eprintln!("  --release build in release mode");
    eprintln!();
    eprintln!("release options:");
    eprintln!("  --draft   create as draft release");
}

#[derive(Clone, Copy)]
enum Bump {
    Patch,
    Minor,
    Major,
}

fn deploy(args: &[String]) {
    let release = args.iter().any(|a| a == "--release");
    let bump = if args.iter().any(|a| a == "--major") {
        Bump::Major
    } else if args.iter().any(|a| a == "--minor") {
        Bump::Minor
    } else {
        Bump::Patch
    };

    let root = project_root();
    let nex_dir = root.join("nex");
    let live_bin = nex_dir.join("bin");
    let config_dir = nex_dir.join("config");
    let toolchain_config = config_dir.join("nex.toml");

    // 1. Read the current version from nex/config/nex.toml
    let config_text = fs::read_to_string(&toolchain_config).unwrap_or_else(|e| {
        panic!("failed to read {}: {e}", toolchain_config.display());
    });
    let current = parse_toolchain_version(&config_text)
        .expect("failed to parse version from nex/config/nex.toml");
    let next = bump_version(&current, bump);

    println!("version: {current} -> {next}");
    println!();

    // 2. Archive current nex/bin/ into nex/<current_version>/bin/
    let archive_dir = nex_dir.join(&current);
    if live_bin.exists() {
        let archive_bin = archive_dir.join("bin");
        fs::create_dir_all(&archive_bin).expect("failed to create archive bin dir");
        copy_dir_files(&live_bin, &archive_bin);
        println!("archived  nex/bin/    -> nex/{current}/bin/");
    }

    // Archive configs alongside the version snapshot
    if config_dir.exists() {
        let archive_config = archive_dir.join("config");
        fs::create_dir_all(&archive_config).expect("failed to create archive config dir");
        copy_dir_files(&config_dir, &archive_config);
        println!("archived  nex/config/ -> nex/{current}/config/");
    }

    println!();

    // 3. Bump version in all relevant files
    let version_files = [
        toolchain_config.clone(),  // nex/config/nex.toml
        root.join("Cargo.toml"),   // workspace version
        root.join("project.toml"), // nex package root
    ];
    for path in &version_files {
        if update_version_in_file(path, &current, &next) {
            println!(
                "bumped    {}",
                path.strip_prefix(&root).unwrap_or(path).display()
            );
        }
    }

    let vscode_pkg = root.join("vscode-extension").join("package.json");
    if update_json_version_in_file(&vscode_pkg, &next) {
        println!(
            "bumped    {}",
            vscode_pkg
                .strip_prefix(&root)
                .unwrap_or(&vscode_pkg)
                .display()
        );
    }

    println!();

    // 4. Build
    let profile = if release { "release" } else { "debug" };
    println!("building  ({profile})...");
    let mut cmd = Command::new("cargo");
    cmd.args(["build", "--workspace", "--exclude", "xtask"])
        .current_dir(&root);
    if release {
        cmd.arg("--release");
    }

    // Auto-detect libtorch if LIBTORCH is not already set.
    // Check common locations and system environment variables (setx values
    // aren't visible in the current session, so read from the registry).
    let has_libtorch = env::var("LIBTORCH").is_ok();
    if !has_libtorch {
        if let Some(libtorch_path) = detect_libtorch() {
            println!("detected  LIBTORCH={}", libtorch_path.display());
            cmd.env("LIBTORCH", &libtorch_path);
        }
    }
    // Ensure CXX11 ABI flag is set for torch-sys build.
    if env::var("LIBTORCH_CXX11_ABI").is_err() {
        cmd.env("LIBTORCH_CXX11_ABI", "0");
    }
    // torch-sys requires Python torch to determine ABI — check if available.
    // If not, skip nex_torch_native to avoid blocking the entire build.
    let python_torch_ok = Command::new("python")
        .args(["-c", "import torch"])
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if !python_torch_ok {
        println!("skipping  nex_torch_native (Python torch not found for ABI detection)");
        cmd.args(["--exclude", "nex_torch_native"]);
    }

    let status = cmd.status().expect("failed to run cargo build");
    if !status.success() {
        eprintln!("cargo build failed");
        process::exit(1);
    }

    println!();

    // 5. Deploy new binaries to nex/bin/
    let target_dir = root.join("target").join(profile);
    let ext = env::consts::EXE_SUFFIX;
    let mut failed = Vec::new();

    fs::create_dir_all(&live_bin).expect("failed to create nex/bin/");

    for name in BINARIES {
        let filename = format!("{name}{ext}");
        let src = target_dir.join(&filename);
        if !src.exists() {
            eprintln!("warning: {filename} not found in target/{profile}, skipping");
            continue;
        }
        let dst = live_bin.join(&filename);
        match fs::copy(&src, &dst) {
            Ok(_) => println!("deployed  {filename} -> nex/bin/{filename}"),
            Err(_) => {
                // File is locked by a running process (e.g. nex-lsp via Cursor).
                // Kill it repeatedly (Cursor may auto-restart) and retry the copy.
                let bare_name = name.replace('-', "_");
                let mut copied = false;
                for attempt in 0..5 {
                    kill_process(&filename);
                    kill_process(&bare_name);
                    if attempt == 0 {
                        println!("killed    {filename} (was locked)");
                    }
                    std::thread::sleep(std::time::Duration::from_millis(
                        300 * (attempt + 1) as u64,
                    ));
                    if fs::copy(&src, &dst).is_ok() {
                        println!("deployed  {filename} -> nex/bin/{filename}");
                        copied = true;
                        break;
                    }
                }
                if !copied {
                    eprintln!(
                        "FAILED    {filename} -> nex/bin/{filename} (still locked after retries)"
                    );
                    failed.push(filename);
                }
            }
        }
    }

    // 6. Deploy native DLLs into their corresponding libs/ directories
    deploy_native_libs(&root, &target_dir);

    // 7. Ensure configs are in place (they stay in nex/config/, already updated)
    println!("configs   nex/config/nex.toml (v{next})");

    // 8. Symlink vscode-extension into VS Code and Cursor extension directories
    let ext_source = root.join("vscode-extension");
    if ext_source.exists() {
        symlink_extension(&ext_source, "nex-language");
    }

    println!();

    if failed.is_empty() {
        println!("deployed v{next} to nex/bin/ ({profile})");
        println!("archived v{current} in nex/{current}/");
    } else {
        eprintln!("deployed with errors. locked files: {}", failed.join(", "));
        eprintln!("hint: close any process using those files (e.g. nex-lsp via VS Code) and retry");
        process::exit(1);
    }
}

// ---------------------------------------------------------------------------
// Release packaging
// ---------------------------------------------------------------------------

fn release(args: &[String]) {
    let draft = args.iter().any(|a| a == "--draft");

    let root = project_root();
    let nex_dir = root.join("nex");
    let toolchain_config = nex_dir.join("config").join("nex.toml");

    // 1. Read version
    let config_text = fs::read_to_string(&toolchain_config).unwrap_or_else(|e| {
        panic!("failed to read {}: {e}", toolchain_config.display());
    });
    let version = parse_toolchain_version(&config_text)
        .expect("failed to parse version from nex/config/nex.toml");

    let tag = format!("v{version}");
    let archive_name = format!("nex-{tag}-windows-x86_64");
    let staging_root = root.join("target").join("release-staging");
    let staging = staging_root.join(&archive_name);

    println!("packaging {tag}...");
    println!();

    // 2. Clean & create staging directory
    if staging.exists() {
        fs::remove_dir_all(&staging).expect("failed to clean staging directory");
    }

    // 3. Stage bin/
    let staged_bin = staging.join("bin");
    fs::create_dir_all(&staged_bin).expect("failed to create staging bin/");
    let live_bin = nex_dir.join("bin");
    if !live_bin.exists() {
        eprintln!("error: nex/bin/ does not exist. Run `cargo xtask deploy` first.");
        process::exit(1);
    }
    for entry in fs::read_dir(&live_bin).expect("failed to read nex/bin/").flatten() {
        let path = entry.path();
        if path.is_file() {
            let name = entry.file_name();
            fs::copy(&path, staged_bin.join(&name)).unwrap_or_else(|e| {
                panic!("failed to copy {}: {e}", path.display());
            });
            println!("staged    bin/{}", name.to_string_lossy());
        }
    }

    // 4. Stage libs/ (*.dll, *.nex, project.toml — skip build/ dirs)
    let libs_dir = root.join("libs");
    if libs_dir.exists() {
        stage_libs(&libs_dir, &staging.join("libs"));
    }

    // 5. Stage config/
    let staged_config = staging.join("config");
    fs::create_dir_all(&staged_config).expect("failed to create staging config/");
    let config_src = nex_dir.join("config").join("nex.toml");
    fs::copy(&config_src, staged_config.join("nex.toml")).unwrap_or_else(|e| {
        panic!("failed to copy nex.toml: {e}");
    });
    println!("staged    config/nex.toml");

    println!();

    // 6. Create zip archive
    let zip_path = staging_root.join(format!("{archive_name}.zip"));
    if zip_path.exists() {
        fs::remove_file(&zip_path).ok();
    }

    println!("creating  {archive_name}.zip ...");

    #[cfg(windows)]
    {
        let status = Command::new("powershell")
            .args([
                "-NoProfile",
                "-Command",
                &format!(
                    "Compress-Archive -Path '{}\\*' -DestinationPath '{}' -Force",
                    staging.to_string_lossy().replace('/', "\\"),
                    zip_path.to_string_lossy().replace('/', "\\"),
                ),
            ])
            .status()
            .expect("failed to run powershell Compress-Archive");
        if !status.success() {
            eprintln!("failed to create zip archive");
            process::exit(1);
        }
    }

    #[cfg(not(windows))]
    {
        let status = Command::new("zip")
            .args(["-r", &zip_path.to_string_lossy(), &archive_name])
            .current_dir(&staging_root)
            .status()
            .expect("failed to run zip");
        if !status.success() {
            eprintln!("failed to create zip archive");
            process::exit(1);
        }
    }

    let zip_size = fs::metadata(&zip_path).map(|m| m.len()).unwrap_or(0);
    println!(
        "created   {} ({:.1} MB)",
        zip_path
            .strip_prefix(&root)
            .unwrap_or(&zip_path)
            .display(),
        zip_size as f64 / (1024.0 * 1024.0)
    );
    println!();

    // 7. Build VS Code extension (.vsix)
    let vscode_dir = root.join("vscode-extension");
    let vsix_path = if vscode_dir.join("package.json").exists() {
        println!("building  VS Code extension...");
        let vsce_status = Command::new("npx")
            .args(["@vscode/vsce", "package", "--no-git-tag-version", "--no-update-package-json"])
            .current_dir(&vscode_dir)
            .status();
        match vsce_status {
            Ok(s) if s.success() => {
                let vsix_name = format!("nex-language-{version}.vsix");
                let vsix = vscode_dir.join(&vsix_name);
                if vsix.exists() {
                    println!("created   {vsix_name}");
                    Some(vsix)
                } else {
                    eprintln!("warning:  vsix not found at expected path, skipping");
                    None
                }
            }
            Ok(_) => {
                eprintln!("warning:  vsce package failed, release will not include vsix");
                None
            }
            Err(e) => {
                eprintln!("warning:  failed to run npx @vscode/vsce: {e}");
                eprintln!("hint:     npm install -g @vscode/vsce");
                None
            }
        }
    } else {
        None
    };

    // 7b. Package libs for registry (docs/libs/)
    println!("packaging libs for registry...");
    package_libs_for_registry();
    println!();

    // 8. Create GitHub release via gh CLI
    println!("publishing GitHub release {tag}...");

    let mut gh_args = vec![
        "release".to_string(),
        "create".to_string(),
        tag.clone(),
        zip_path.to_string_lossy().to_string(),
    ];
    if let Some(ref vsix) = vsix_path {
        gh_args.push(vsix.to_string_lossy().to_string());
    }
    gh_args.extend([
        "--title".to_string(),
        format!("Nex {tag}"),
        "--generate-notes".to_string(),
    ]);
    if draft {
        gh_args.push("--draft".to_string());
    }

    let status = Command::new("gh")
        .args(&gh_args)
        .current_dir(&root)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("failed to run `gh`: {e}");
            eprintln!("hint: install GitHub CLI: https://cli.github.com/");
            process::exit(1);
        });

    if !status.success() {
        eprintln!("gh release create failed");
        process::exit(1);
    }

    // 9. Clean up staging
    let _ = fs::remove_dir_all(&staging);

    println!();
    let draft_label = if draft { " (draft)" } else { "" };
    println!("released {tag}{draft_label}");
    println!("install:  irm https://raw.githubusercontent.com/Ronmenator/Nex/master/install.ps1 | iex");
}

// ---------------------------------------------------------------------------
// Library registry packaging
// ---------------------------------------------------------------------------

#[derive(serde::Serialize)]
struct Registry {
    version: u32,
    updated: String,
    nex_version: String,
    base_url: String,
    libraries: Vec<RegistryLib>,
}

#[derive(serde::Serialize)]
struct RegistryLib {
    name: String,
    version: String,
    description: String,
    has_native: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    native_name: Option<String>,
    modules: Vec<String>,
    platforms: HashMap<String, String>,
}

fn package_libs_for_registry() {
    let root = project_root();
    let libs_dir = root.join("libs");
    let docs_libs_dir = root.join("docs").join("libs");
    let toolchain_config = root.join("nex").join("config").join("nex.toml");

    let config_text = fs::read_to_string(&toolchain_config).unwrap_or_default();
    let nex_version =
        parse_toolchain_version(&config_text).unwrap_or_else(|| "0.0.0".to_string());

    fs::create_dir_all(&docs_libs_dir).expect("failed to create docs/libs/");

    let mut registry_libs = Vec::new();

    let mut lib_dirs: Vec<_> = fs::read_dir(&libs_dir)
        .expect("failed to read libs/")
        .flatten()
        .filter(|e| e.path().is_dir())
        .collect();
    lib_dirs.sort_by_key(|e| e.file_name());

    for entry in &lib_dirs {
        let lib_root = entry.path();
        let toml_path = lib_root.join("project.toml");
        let Ok(toml_text) = fs::read_to_string(&toml_path) else {
            continue;
        };

        let lib_name = parse_name_field(&toml_text).unwrap_or_default();
        if lib_name.is_empty() {
            continue;
        }
        let lib_version = parse_version_field(&toml_text).unwrap_or_else(|| "0.1.0".to_string());
        let native_name = parse_native_field(&toml_text);
        let has_native = native_name.is_some();
        let modules = enumerate_nex_modules(&lib_root.join("src"));
        let description = lib_description(&lib_name);

        let platform = if has_native { "windows-x86_64" } else { "any" };
        let zip_name = format!("{lib_name}-{lib_version}-{platform}.zip");
        let zip_path = docs_libs_dir.join(&zip_name);

        create_lib_zip(&lib_root, &zip_path, &lib_name, &native_name);

        let zip_size = fs::metadata(&zip_path).map(|m| m.len()).unwrap_or(0);
        println!(
            "packaged  {zip_name} ({:.1} KB)",
            zip_size as f64 / 1024.0
        );

        let mut platforms = HashMap::new();
        platforms.insert(platform.to_string(), zip_name);

        registry_libs.push(RegistryLib {
            name: lib_name,
            version: lib_version,
            description,
            has_native,
            native_name,
            modules,
            platforms,
        });
    }

    let registry = Registry {
        version: 1,
        updated: utc_now_iso(),
        nex_version,
        base_url: "https://ronmenator.github.io/Nex/libs".to_string(),
        libraries: registry_libs,
    };

    let json = serde_json::to_string_pretty(&registry).expect("failed to serialize registry");
    let registry_path = docs_libs_dir.join("registry.json");
    fs::write(&registry_path, &json).expect("failed to write registry.json");
    println!("wrote     docs/libs/registry.json");
}

fn create_lib_zip(lib_root: &Path, zip_path: &Path, lib_name: &str, native_name: &Option<String>) {
    let temp_dir = env::temp_dir().join(format!("nex-pkg-{lib_name}"));
    let staging = temp_dir.join(lib_name);
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&staging).expect("failed to create temp staging dir");

    // Copy project.toml
    let _ = fs::copy(lib_root.join("project.toml"), staging.join("project.toml"));

    // Copy src/*.nex
    let src_dir = lib_root.join("src");
    if src_dir.is_dir() {
        let dst_src = staging.join("src");
        fs::create_dir_all(&dst_src).ok();
        for file in fs::read_dir(&src_dir).into_iter().flatten().flatten() {
            let path = file.path();
            if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("nex") {
                fs::copy(&path, dst_src.join(file.file_name())).ok();
            }
        }
    }

    // Copy native DLLs (only nex_* / nex3d_* prefixed)
    if let Some(ref _native) = native_name {
        for file in fs::read_dir(lib_root).into_iter().flatten().flatten() {
            let path = file.path();
            if !path.is_file() {
                continue;
            }
            let name = file.file_name();
            let name_str = name.to_string_lossy();
            let is_native = name_str.ends_with(".dll")
                || name_str.ends_with(".so")
                || name_str.ends_with(".dylib");
            if is_native && (name_str.starts_with("nex_") || name_str.starts_with("nex3d_")) {
                fs::copy(&path, staging.join(&*name)).ok();
            }
        }
    }

    // Create zip
    if zip_path.exists() {
        fs::remove_file(zip_path).ok();
    }

    #[cfg(windows)]
    {
        let status = Command::new("powershell")
            .args([
                "-NoProfile",
                "-Command",
                &format!(
                    "Compress-Archive -Path '{}\\*' -DestinationPath '{}' -Force",
                    staging.to_string_lossy().replace('/', "\\"),
                    zip_path.to_string_lossy().replace('/', "\\"),
                ),
            ])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .expect("failed to run Compress-Archive");
        if !status.success() {
            eprintln!("warning: failed to create zip for {lib_name}");
        }
    }

    #[cfg(not(windows))]
    {
        let status = Command::new("zip")
            .args(["-r", &zip_path.to_string_lossy(), lib_name])
            .current_dir(&temp_dir)
            .stdout(std::process::Stdio::null())
            .status()
            .expect("failed to run zip");
        if !status.success() {
            eprintln!("warning: failed to create zip for {lib_name}");
        }
    }

    let _ = fs::remove_dir_all(&temp_dir);
}

fn enumerate_nex_modules(src_dir: &Path) -> Vec<String> {
    let mut modules = Vec::new();
    if let Ok(entries) = fs::read_dir(src_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("nex") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    modules.push(stem.to_string());
                }
            }
        }
    }
    modules.sort();
    modules
}

fn lib_description(name: &str) -> String {
    match name {
        "crypto" => "Cryptography: SHA, AES-256-GCM, Ed25519, HMAC, PBKDF2, Argon2, Base64.",
        "http" => "HTTP client and server.",
        "json" => "Type-safe JSON serialization and deserialization.",
        "net" => "TLS client and WebSocket server.",
        "nex3d" => "3D game engine: rendering, audio, input, physics, sprites.",
        "nex_ui" => "Native desktop UI: widgets, canvas, dialogs, styling, events.",
        "regex" => "Regular expression pattern matching.",
        "torch" => "Machine learning via libtorch: tensors, neural networks, training.",
        _ => "",
    }
    .to_string()
}

fn parse_name_field(toml: &str) -> Option<String> {
    let mut in_section = false;
    for line in toml.lines() {
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

fn parse_version_field(toml: &str) -> Option<String> {
    let mut in_section = false;
    for line in toml.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_section = true;
            continue;
        }
        if in_section {
            continue;
        }
        if let Some((key, value)) = trimmed.split_once('=') {
            if key.trim() == "version" {
                let v = value.trim().trim_matches('"').trim_matches('\'');
                if !v.is_empty() {
                    return Some(v.to_string());
                }
            }
        }
    }
    None
}

fn utc_now_iso() -> String {
    #[cfg(windows)]
    {
        let output = Command::new("powershell")
            .args([
                "-NoProfile",
                "-Command",
                "(Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')",
            ])
            .output();
        if let Ok(o) = output {
            let s = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if !s.is_empty() {
                return s;
            }
        }
    }
    #[cfg(not(windows))]
    {
        let output = Command::new("date")
            .args(["-u", "+%Y-%m-%dT%H:%M:%SZ"])
            .output();
        if let Ok(o) = output {
            let s = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if !s.is_empty() {
                return s;
            }
        }
    }
    "1970-01-01T00:00:00Z".to_string()
}

/// Copy lib directories into staging, including only .dll, .nex, and project.toml files.
fn stage_libs(libs_src: &Path, libs_dst: &Path) {
    let Ok(entries) = fs::read_dir(libs_src) else {
        return;
    };

    for entry in entries.flatten() {
        let src_lib = entry.path();
        if !src_lib.is_dir() {
            continue;
        }
        let lib_name = entry.file_name();
        let dst_lib = libs_dst.join(&lib_name);
        fs::create_dir_all(&dst_lib).expect("failed to create lib staging dir");

        // Copy top-level files (.dll, project.toml)
        // Only include Nex-built native DLLs (nex_* / nex3d_*) — skip third-party
        // redistributable DLLs (e.g. libtorch CUDA libs) that users install separately.
        for file in fs::read_dir(&src_lib).into_iter().flatten().flatten() {
            let path = file.path();
            if !path.is_file() {
                continue;
            }
            let name = file.file_name();
            let name_str = name.to_string_lossy();
            let is_native_lib = name_str.ends_with(".dll")
                || name_str.ends_with(".so")
                || name_str.ends_with(".dylib");
            if is_native_lib {
                // Only include DLLs built by Nex (nex_*, nex3d_*) — skip vendor libs
                if !name_str.starts_with("nex_") && !name_str.starts_with("nex3d_") {
                    continue;
                }
            }
            if is_native_lib || name_str == "project.toml" {
                fs::copy(&path, dst_lib.join(&name)).ok();
            }
        }

        // Copy src/*.nex
        let src_subdir = src_lib.join("src");
        if src_subdir.is_dir() {
            let dst_src = dst_lib.join("src");
            fs::create_dir_all(&dst_src).ok();
            for file in fs::read_dir(&src_subdir).into_iter().flatten().flatten() {
                let path = file.path();
                if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("nex") {
                    fs::copy(&path, dst_src.join(file.file_name())).ok();
                }
            }
        }

        println!("staged    libs/{}/", lib_name.to_string_lossy());
    }
}

// ---------------------------------------------------------------------------
// Native library deployment
// ---------------------------------------------------------------------------

/// Scan `libs/` for directories with a `project.toml` containing a `native` field,
/// then copy the corresponding built DLL/so/dylib from `target_dir` into the lib root.
fn deploy_native_libs(root: &Path, target_dir: &Path) {
    let libs_dir = root.join("libs");
    let Ok(entries) = fs::read_dir(&libs_dir) else {
        return;
    };

    let (dll_prefix, dll_ext) = if cfg!(target_os = "windows") {
        ("", "dll")
    } else if cfg!(target_os = "macos") {
        ("lib", "dylib")
    } else {
        ("lib", "so")
    };

    for entry in entries.flatten() {
        let lib_root = entry.path();
        if !lib_root.is_dir() {
            continue;
        }
        let toml_path = lib_root.join("project.toml");
        let Ok(toml_text) = fs::read_to_string(&toml_path) else {
            continue;
        };
        let Some(native_name) = parse_native_field(&toml_text) else {
            continue;
        };

        let filename = format!("{dll_prefix}{native_name}.{dll_ext}");
        let src = target_dir.join(&filename);
        if !src.exists() {
            continue;
        }

        let dst = lib_root.join(&filename);
        let lib_name = entry.file_name();
        match fs::copy(&src, &dst) {
            Ok(_) => println!(
                "deployed  {filename} -> libs/{}/{}",
                lib_name.to_string_lossy(),
                filename,
            ),
            Err(e) => eprintln!(
                "warning:  failed to copy {filename} to libs/{}/: {e}",
                lib_name.to_string_lossy(),
            ),
        }
    }
}

/// Parse the `native = "..."` field from a project.toml string (top-level only).
fn parse_native_field(toml: &str) -> Option<String> {
    let mut in_section = false;
    for line in toml.lines() {
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

// ---------------------------------------------------------------------------
// Version helpers
// ---------------------------------------------------------------------------

fn parse_toolchain_version(text: &str) -> Option<String> {
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("version") && trimmed.contains('=') {
            let val = trimmed.split('=').nth(1)?;
            return Some(val.trim().trim_matches('"').to_string());
        }
    }
    None
}

fn bump_version(version: &str, bump: Bump) -> String {
    let parts: Vec<u32> = version.split('.').filter_map(|s| s.parse().ok()).collect();
    let (major, minor, patch) = (
        parts.first().copied().unwrap_or(0),
        parts.get(1).copied().unwrap_or(0),
        parts.get(2).copied().unwrap_or(0),
    );
    match bump {
        Bump::Major => format!("{}.0.0", major + 1),
        Bump::Minor => format!("{}.{}.0", major, minor + 1),
        Bump::Patch => format!("{}.{}.{}", major, minor, patch + 1),
    }
}

/// Replace `version = "<old>"` with `version = "<new>"` in a TOML file.
/// Returns true if the file was modified.
fn update_version_in_file(path: &Path, old: &str, new: &str) -> bool {
    let Ok(text) = fs::read_to_string(path) else {
        return false;
    };
    let needle = format!("version = \"{old}\"");
    let replacement = format!("version = \"{new}\"");
    let updated = text.replace(&needle, &replacement);
    if updated != text {
        fs::write(path, &updated).unwrap_or_else(|e| {
            eprintln!("warning: could not write {}: {e}", path.display());
        });
        return true;
    }
    false
}

/// Replace `"version": "<anything>"` with `"version": "<new>"` in a JSON file.
/// Returns true if the file was modified.
fn update_json_version_in_file(path: &Path, new: &str) -> bool {
    let Ok(text) = fs::read_to_string(path) else {
        return false;
    };
    let mut updated = String::with_capacity(text.len());
    let mut changed = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("\"version\"") && trimmed.contains(':') {
            let indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
            let has_comma = trimmed.ends_with(',');
            let comma = if has_comma { "," } else { "" };
            updated.push_str(&format!("{indent}\"version\": \"{new}\"{comma}\n"));
            changed = true;
        } else {
            updated.push_str(line);
            updated.push('\n');
        }
    }
    if !text.ends_with('\n') {
        updated.pop();
    }
    if changed {
        fs::write(path, &updated).unwrap_or_else(|e| {
            eprintln!("warning: could not write {}: {e}", path.display());
        });
    }
    changed
}

// ---------------------------------------------------------------------------
// Process helpers
// ---------------------------------------------------------------------------

/// Try to kill a process by image name. Returns true if the kill command ran.
fn kill_process(image_name: &str) -> bool {
    if cfg!(windows) {
        Command::new("taskkill")
            .args(["/F", "/IM", image_name])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    } else {
        Command::new("pkill")
            .args(["-f", image_name])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }
}

// ---------------------------------------------------------------------------
// Extension symlink helpers
// ---------------------------------------------------------------------------

fn symlink_extension(ext_source: &Path, ext_name: &str) {
    let home = match env::var("USERPROFILE").or_else(|_| env::var("HOME")) {
        Ok(h) => PathBuf::from(h),
        Err(_) => {
            eprintln!("warning: could not determine home directory, skipping extension symlinks");
            return;
        }
    };

    let targets = [
        home.join(".vscode").join("extensions").join(ext_name),
        home.join(".cursor").join("extensions").join(ext_name),
    ];

    for link_path in &targets {
        if link_path.exists() {
            let label = link_path.strip_prefix(&home).unwrap_or(link_path);
            println!(
                "symlink   ~/{} (already exists)",
                label.display().to_string().replace('\\', "/")
            );
            continue;
        }

        if let Some(parent) = link_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        let ok = create_dir_symlink(ext_source, link_path);
        let label = link_path.strip_prefix(&home).unwrap_or(link_path);
        if ok {
            println!(
                "symlink   ~/{}",
                label.display().to_string().replace('\\', "/")
            );
        } else {
            eprintln!(
                "warning:  failed to symlink ~/{}",
                label.display().to_string().replace('\\', "/")
            );
        }
    }
}

fn create_dir_symlink(original: &Path, link: &Path) -> bool {
    #[cfg(windows)]
    {
        // Try junction first (no admin required), fall back to symlink
        Command::new("cmd")
            .args([
                "/C",
                "mklink",
                "/J",
                &link.to_string_lossy(),
                &original.to_string_lossy(),
            ])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }
    #[cfg(not(windows))]
    {
        std::os::unix::fs::symlink(original, link).is_ok()
    }
}

// ---------------------------------------------------------------------------
// Libtorch auto-detection
// ---------------------------------------------------------------------------

/// Try to find a libtorch installation. Checks:
/// 1. Common install locations (F:\libtorch, D:\libtorch, C:\libtorch)
/// 2. User/system environment variables (reads registry on Windows for setx values)
fn detect_libtorch() -> Option<PathBuf> {
    // Check common paths first.
    let common_paths = [
        PathBuf::from("F:\\libtorch"),
        PathBuf::from("D:\\libtorch"),
        PathBuf::from("C:\\libtorch"),
    ];
    for p in &common_paths {
        if p.join("lib").is_dir() && p.join("include").is_dir() {
            return Some(p.clone());
        }
    }

    // On Windows, read LIBTORCH from the registry (captures setx values
    // that aren't visible in the current process environment).
    #[cfg(target_os = "windows")]
    {
        let output = Command::new("reg")
            .args(["query", "HKCU\\Environment", "/v", "LIBTORCH"])
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .output()
            .ok()?;
        if output.status.success() {
            let text = String::from_utf8_lossy(&output.stdout);
            // Format: "    LIBTORCH    REG_SZ    D:\libtorch"
            for line in text.lines() {
                if line.contains("LIBTORCH") && line.contains("REG_SZ") {
                    if let Some(val) = line.split("REG_SZ").nth(1) {
                        let path = PathBuf::from(val.trim());
                        if path.join("lib").is_dir() {
                            return Some(path);
                        }
                    }
                }
            }
        }
    }

    None
}

// ---------------------------------------------------------------------------
// File helpers
// ---------------------------------------------------------------------------

fn copy_dir_files(src: &Path, dst: &Path) {
    let Ok(entries) = fs::read_dir(src) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() {
            let dest = dst.join(entry.file_name());
            if let Err(e) = fs::copy(&path, &dest) {
                eprintln!("warning: failed to copy {}: {e}", path.display());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock went backwards")
            .as_nanos();
        env::temp_dir().join(format!("nex-xtask-{name}-{}-{nanos}", process::id()))
    }

    #[test]
    fn finds_workspace_root_from_nested_path() {
        let root = unique_temp_dir("find-root");
        let nested = root.join("xtask").join("src");
        fs::create_dir_all(&nested).expect("create nested dir");
        fs::write(root.join("Cargo.toml"), "[workspace]\n").expect("write Cargo.toml");
        fs::write(
            root.join("xtask").join("Cargo.toml"),
            "[package]\nname = \"xtask\"\nversion = \"0.1.0\"\n",
        )
        .expect("write xtask Cargo.toml");

        let found = find_workspace_root(&nested).expect("workspace root not found");
        assert_eq!(found, root);

        let _ = fs::remove_dir_all(found);
    }

    #[test]
    fn returns_none_without_workspace_markers() {
        let root = unique_temp_dir("no-markers");
        fs::create_dir_all(&root).expect("create root");
        let nested = root.join("foo").join("bar");
        fs::create_dir_all(&nested).expect("create nested");

        assert!(find_workspace_root(&nested).is_none());

        let _ = fs::remove_dir_all(root);
    }
}
