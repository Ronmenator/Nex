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
    eprintln!("usage: cargo xtask deploy [--release] [--major | --minor | --patch]");
    eprintln!();
    eprintln!("  --patch   bump patch version (default): 0.1.0 -> 0.1.1");
    eprintln!("  --minor   bump minor version:           0.1.0 -> 0.2.0");
    eprintln!("  --major   bump major version:           0.1.0 -> 1.0.0");
    eprintln!("  --release build in release mode");
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
    cmd.args(["build", "--features", "nex/ui,nexc/ui"])
        .current_dir(&root);
    if release {
        cmd.arg("--release");
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
                // File is locked by a running process (e.g. aur-lsp via Cursor).
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

    // 6. Ensure configs are in place (they stay in nex/config/, already updated)
    println!("configs   nex/config/nex.toml (v{next})");

    // 7. Symlink vscode-extension into VS Code and Cursor extension directories
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
