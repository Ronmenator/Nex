use std::collections::HashMap;
use std::io::{self, BufRead, Write};

use nexc_ast::SourceFile;
use nexc_diag::{Diagnostic, DiagnosticSink};
// nexc_driver available for full compilation if needed
use nexc_lex::{asi_normalize, lex};
use nexc_lint::lint_all;
use nexc_parse::Parser;

#[derive(Debug, Clone)]
pub struct LspContext {
    pub module_cache: HashMap<String, CachedModule>,
}

#[derive(Debug, Clone)]
pub struct CachedModule {
    pub source: String,
    pub ast: SourceFile,
    pub diagnostics: Vec<Diagnostic>,
}

impl Default for LspContext {
    fn default() -> Self {
        Self { module_cache: HashMap::new() }
    }
}

impl LspContext {
    pub fn update_file(&mut self, uri: &str, source: &str) {
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some(uri.into()), &mut sink);
        let tokens = asi_normalize(&tokens);
        let mut parser = Parser::new(&tokens, uri.into());
        let ast = parser.parse();

        let mut diags: Vec<Diagnostic> = sink.into_vec();
        diags.extend(parser.diagnostics().to_vec());
        diags.extend(lint_all(&ast));

        self.module_cache.insert(uri.into(), CachedModule {
            source: source.into(),
            ast,
            diagnostics: diags,
        });
    }

    pub fn diagnostics_for(&self, uri: &str) -> Vec<Diagnostic> {
        self.module_cache.get(uri)
            .map(|m| m.diagnostics.clone())
            .unwrap_or_default()
    }
}

pub fn diagnostics_for_source(source: &str, file: &str) -> Vec<Diagnostic> {
    let mut sink = DiagnosticSink::new();
    let tokens = lex(source, Some(file.into()), &mut sink);
    let tokens = asi_normalize(&tokens);
    let mut parser = Parser::new(&tokens, file.into());
    let ast = parser.parse();
    let mut diags = sink.into_vec();
    diags.extend(parser.diagnostics().to_vec());
    diags.extend(lint_all(&ast));
    diags
}

pub fn goto_definition(source: &str, name: &str) -> Option<(String, usize)> {
    let mut sink = DiagnosticSink::new();
    let tokens = lex(source, None, &mut sink);
    let tokens = asi_normalize(&tokens);
    let mut parser = Parser::new(&tokens, "<goto>".into());
    let ast = parser.parse();

    for item in &ast.items {
        let item_name = match item {
            nexc_ast::Item::Function(f) => Some((&f.name, f.span)),
            nexc_ast::Item::Class(c) => Some((&c.name, c.span)),
            nexc_ast::Item::Interface(i) => Some((&i.name, i.span)),
            nexc_ast::Item::Struct(s) => Some((&s.name, s.span)),
            _ => None,
        };
        if let Some((n, span)) = item_name {
            if n == name {
                return Some(("<goto>".into(), span.lo));
            }
        }
    }
    None
}

pub fn hover_type(source: &str, offset: usize) -> Option<String> {
    let mut sink = DiagnosticSink::new();
    let tokens = lex(source, None, &mut sink);

    for token in &tokens {
        if token.span.lo <= offset && offset < token.span.hi {
            match &token.kind {
                nexc_lex::TokenKind::IntLiteral => return Some("Int".into()),
                nexc_lex::TokenKind::FloatLiteral => return Some("Double".into()),
                nexc_lex::TokenKind::StringLiteral => return Some("String".into()),
                nexc_lex::TokenKind::BooleanLiteral => return Some("Bool".into()),
                nexc_lex::TokenKind::CharLiteral => return Some("Char".into()),
                nexc_lex::TokenKind::NullLiteral => return Some("Null".into()),
                nexc_lex::TokenKind::Identifier => return Some(token.lexeme.clone()),
                _ => return Some(format!("{:?}", token.kind)),
            }
        }
    }
    None
}

pub fn run_lsp_stdio() {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut ctx = LspContext::default();
    let mut reader = stdin.lock();
    let mut writer = stdout.lock();

    loop {
        let content_length = match read_header(&mut reader) {
            Some(len) => len,
            None => break,
        };

        let mut body = vec![0u8; content_length];
        if io::Read::read_exact(&mut reader, &mut body).is_err() {
            break;
        }

        let body_str = match String::from_utf8(body) {
            Ok(s) => s,
            Err(_) => continue,
        };

        if let Some(response) = handle_message(&mut ctx, &body_str) {
            let resp_bytes = response.as_bytes();
            let header = format!("Content-Length: {}\r\n\r\n", resp_bytes.len());
            let _ = writer.write_all(header.as_bytes());
            let _ = writer.write_all(resp_bytes);
            let _ = writer.flush();
        }
    }
}

fn read_header(reader: &mut impl BufRead) -> Option<usize> {
    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some(value) = trimmed.strip_prefix("Content-Length: ") {
            content_length = value.parse().unwrap_or(0);
        }
    }
    if content_length == 0 { None } else { Some(content_length) }
}

fn handle_message(ctx: &mut LspContext, body: &str) -> Option<String> {
    let json: serde_json::Value = serde_json::from_str(body).ok()?;
    let method = json.get("method")?.as_str()?;
    let id = json.get("id");

    match method {
        "initialize" => {
            let result = serde_json::json!({
                "capabilities": {
                    "textDocumentSync": 1,
                    "hoverProvider": true,
                    "definitionProvider": true,
                }
            });
            Some(make_response(id, result))
        }
        "initialized" => None,
        "shutdown" => Some(make_response(id, serde_json::Value::Null)),
        "exit" => std::process::exit(0),
        "textDocument/didOpen" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let text = doc.get("text")?.as_str()?;
            ctx.update_file(uri, text);
            let diags = ctx.diagnostics_for(uri);
            Some(publish_diagnostics(uri, &diags))
        }
        "textDocument/didChange" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let changes = params.get("contentChanges")?.as_array()?;
            if let Some(change) = changes.last() {
                if let Some(text) = change.get("text").and_then(|t| t.as_str()) {
                    ctx.update_file(uri, text);
                    let diags = ctx.diagnostics_for(uri);
                    return Some(publish_diagnostics(uri, &diags));
                }
            }
            None
        }
        "textDocument/hover" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let pos = params.get("position")?;
            let _line = pos.get("line")?.as_u64()?;
            let _char = pos.get("character")?.as_u64()?;

            if let Some(cached) = ctx.module_cache.get(uri) {
                let offset = _char as usize;
                if let Some(ty) = hover_type(&cached.source, offset) {
                    let result = serde_json::json!({
                        "contents": { "kind": "plaintext", "value": ty }
                    });
                    return Some(make_response(id, result));
                }
            }
            Some(make_response(id, serde_json::Value::Null))
        }
        "textDocument/definition" => {
            Some(make_response(id, serde_json::Value::Null))
        }
        _ => None,
    }
}

fn make_response(id: Option<&serde_json::Value>, result: serde_json::Value) -> String {
    let resp = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id.cloned().unwrap_or(serde_json::Value::Null),
        "result": result,
    });
    resp.to_string()
}

fn publish_diagnostics(uri: &str, diags: &[Diagnostic]) -> String {
    let items: Vec<serde_json::Value> = diags.iter().map(|d| {
        let severity = match d.severity {
            nexc_diag::Severity::Error => 1,
            nexc_diag::Severity::Warning => 2,
            nexc_diag::Severity::Note => 3,
            nexc_diag::Severity::Help => 4,
        };
        let (start_line, start_char) = (0u32, d.span.map(|s| s.lo as u32).unwrap_or(0));
        let (end_line, end_char) = (0u32, d.span.map(|s| s.hi as u32).unwrap_or(0));
        serde_json::json!({
            "range": {
                "start": { "line": start_line, "character": start_char },
                "end": { "line": end_line, "character": end_char },
            },
            "severity": severity,
            "source": "nex",
            "message": d.message,
            "code": d.id,
        })
    }).collect();

    let notif = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": {
            "uri": uri,
            "diagnostics": items,
        }
    });
    notif.to_string()
}
