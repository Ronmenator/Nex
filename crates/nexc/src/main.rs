use std::env;
use std::fs;

use nexc_driver::{compile_module, CompileOptions};

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        eprintln!("Usage: nexc <input>.nex [--emit-metadata]");
        return;
    }

    let emit_metadata = args.iter().any(|a| a == "--emit-metadata");
    args.retain(|a| a != "--emit-metadata");

    if args.len() != 1 {
        eprintln!("Usage: nexc <input>.nex [--emit-metadata]");
        return;
    }

    let source_path = args.remove(0);
    let source_text = match fs::read_to_string(&source_path) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("nexc: cannot read {}: {err}", source_path);
            return;
        }
    };

    let options = CompileOptions {
        source_path: source_path.clone(),
        emit_metadata,
        ..Default::default()
    };
    let result = compile_module(&source_text, options);

    if result.diagnostics.iter().any(|d| d.is_error()) {
        for d in &result.diagnostics {
            eprint!("{}", d.render(&result.source_map));
        }
        std::process::exit(1);
    }

    if !result.diagnostics.is_empty() {
        for d in &result.diagnostics {
            eprint!("{}", d.render(&result.source_map));
        }
    }

    if let Some(bytes) = &result.object {
        let ext = if cfg!(windows) { "obj" } else { "o" };
        let object_path = format!("{source_path}.{ext}");
        if let Err(err) = fs::write(&object_path, bytes) {
            eprintln!("nexc: cannot write {}: {err}", object_path);
            std::process::exit(1);
        }
        println!("wrote object file: {} ({} bytes)", object_path, bytes.len());
    } else {
        println!("ok: frontend checks passed for {}", source_path);
    }

    if emit_metadata {
        println!("wrote metadata: {}.nexmeta", source_path);
    }
}
