use nexc_diag::Diagnostic;
use nexc_ir::IrModule;

#[derive(Debug, Default)]
pub struct JitSession {
    pub counter: usize,
    pub functions: Vec<String>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct JitResult {
    pub value_text: String,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn init_jit_session() -> JitSession {
    JitSession::default()
}

pub fn compile_repl_snippet(_session: &mut JitSession, source: &str) -> Result<JitResult, String> {
    if source.trim().is_empty() {
        return Err("empty REPL snippet".to_string());
    }
    Ok(JitResult {
        value_text: format!("executed {}", source),
        diagnostics: Vec::new(),
    })
}

pub fn compile_to_jit_module(
    ir: &IrModule,
    session: &mut JitSession,
) -> Result<String, String> {
    session.counter += 1;
    let fn_name = format!("repl_entry${:04}", session.counter);
    session.functions.push(fn_name.clone());
    if ir.functions.is_empty() {
        return Err("cannot JIT compile empty module".to_string());
    }
    Ok(fn_name)
}

