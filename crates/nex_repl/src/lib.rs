use std::io::{self, BufRead, Write};

use nexc_codegen_llvm_jit::{compile_repl_snippet, compile_to_jit_module, init_jit_session, JitSession};
use nexc_driver::{compile_module, CompileOptions};

#[derive(Debug)]
pub struct ReplSession {
    pub session: JitSession,
    pub module_counter: u32,
}

impl ReplSession {
    pub fn new() -> Self {
        Self {
            session: init_jit_session(),
            module_counter: 0,
        }
    }

    pub fn eval(&mut self, source: &str) -> Result<String, String> {
        self.module_counter += 1;
        let module_name = format!("repl${:04}", self.module_counter);

        let result = compile_module(source, CompileOptions {
            source_path: module_name.clone(),
            emit_metadata: false,
            ..Default::default()
        });

        let errors: Vec<_> = result.diagnostics.iter()
            .filter(|d| d.is_error())
            .collect();

        if !errors.is_empty() {
            let msgs: Vec<String> = errors.iter().map(|d| d.message.clone()).collect();
            return Err(msgs.join("\n"));
        }

        for d in &result.diagnostics {
            if !d.is_error() {
                eprintln!("warning: {}", d.message);
            }
        }

        if let Some(ir) = &result.ir {
            match compile_to_jit_module(ir, &mut self.session) {
                Ok(fn_name) => Ok(format!("compiled {fn_name} [{module_name}]")),
                Err(e) => Err(e),
            }
        } else {
            match compile_repl_snippet(&mut self.session, source) {
                Ok(r) => Ok(format!("{} [{module_name}]", r.value_text)),
                Err(e) => Err(e),
            }
        }
    }
}

pub fn run_repl() {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut session = ReplSession::new();
    let mut reader = stdin.lock();
    let mut writer = stdout.lock();

    let _ = writeln!(writer, "Nex REPL v0.1.0");
    let _ = writeln!(writer, "Type expressions or statements. Type :quit to exit.");
    let _ = writer.flush();

    loop {
        let _ = write!(writer, "aur> ");
        let _ = writer.flush();

        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => {}
            Err(_) => break,
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        match trimmed {
            ":quit" | ":exit" | ":q" => break,
            ":help" | ":h" => {
                let _ = writeln!(writer, "Commands:");
                let _ = writeln!(writer, "  :quit    Exit the REPL");
                let _ = writeln!(writer, "  :help    Show this help");
                let _ = writeln!(writer, "  :reset   Reset the session");
                let _ = writer.flush();
                continue;
            }
            ":reset" => {
                session = ReplSession::new();
                let _ = writeln!(writer, "Session reset.");
                let _ = writer.flush();
                continue;
            }
            _ => {}
        }

        match session.eval(trimmed) {
            Ok(result) => {
                let _ = writeln!(writer, "{result}");
            }
            Err(err) => {
                let _ = writeln!(writer, "error: {err}");
            }
        }
        let _ = writer.flush();
    }

    let _ = writeln!(writer, "Goodbye.");
    let _ = writer.flush();
}
