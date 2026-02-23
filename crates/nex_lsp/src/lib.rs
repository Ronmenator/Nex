use std::collections::HashMap;
use std::io::{self, BufRead, Write};

use nexc_ast::SourceFile;
use nexc_diag::{Diagnostic, DiagnosticSink};
use nexc_lex::{asi_normalize, lex};
use nexc_lint::lint_all;
use nexc_parse::Parser;
use nexui_parse::parse_nexui;

// ---------------------------------------------------------------------------
// Nex module cache
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct LspContext {
    pub module_cache: HashMap<String, CachedModule>,
    pub nexui_cache: HashMap<String, CachedNexui>,
}

#[derive(Debug, Clone)]
pub struct CachedModule {
    pub source: String,
    pub ast: SourceFile,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct CachedNexui {
    pub source: String,
    pub diagnostics: Vec<Diagnostic>,
}

impl Default for LspContext {
    fn default() -> Self {
        Self {
            module_cache: HashMap::new(),
            nexui_cache: HashMap::new(),
        }
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

    pub fn update_nexui_file(&mut self, uri: &str, source: &str) {
        let mut sink = DiagnosticSink::new();
        let _ = parse_nexui(source, uri, &mut sink);
        let diags = sink.into_vec();
        self.nexui_cache.insert(uri.into(), CachedNexui {
            source: source.into(),
            diagnostics: diags,
        });
    }

    pub fn diagnostics_for(&self, uri: &str) -> Vec<Diagnostic> {
        if let Some(m) = self.module_cache.get(uri) {
            return m.diagnostics.clone();
        }
        if let Some(m) = self.nexui_cache.get(uri) {
            return m.diagnostics.clone();
        }
        Vec::new()
    }
}

// ---------------------------------------------------------------------------
// Offset helpers
// ---------------------------------------------------------------------------

/// Convert a (line, character) LSP position to a byte offset in `source`.
fn line_char_to_offset(source: &str, line: usize, character: usize) -> usize {
    let mut cur_line = 0usize;
    let mut cur_char = 0usize;
    for (i, ch) in source.char_indices() {
        if cur_line == line && cur_char == character {
            return i;
        }
        if ch == '\n' {
            cur_line += 1;
            cur_char = 0;
        } else {
            cur_char += 1;
        }
    }
    source.len()
}

/// Convert a byte offset to an LSP (line, character) position.
fn offset_to_line_char(source: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Extract the identifier word that spans or precedes `offset`.
fn word_at_offset(source: &str, offset: usize) -> &str {
    let bytes = source.as_bytes();
    let safe_offset = offset.min(bytes.len());
    let start = (0..safe_offset)
        .rev()
        .find(|&i| {
            let b = bytes[i];
            !b.is_ascii_alphanumeric() && b != b'_'
        })
        .map(|i| i + 1)
        .unwrap_or(0);
    let end = (safe_offset..bytes.len())
        .find(|&i| {
            let b = bytes[i];
            !b.is_ascii_alphanumeric() && b != b'_'
        })
        .unwrap_or(bytes.len());
    &source[start..end]
}

// ---------------------------------------------------------------------------
// Analysis helpers
// ---------------------------------------------------------------------------

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
            return match &token.kind {
                nexc_lex::TokenKind::IntLiteral => Some("Int".into()),
                nexc_lex::TokenKind::FloatLiteral => Some("Double".into()),
                nexc_lex::TokenKind::StringLiteral => Some("String".into()),
                nexc_lex::TokenKind::BooleanLiteral => Some("Bool".into()),
                nexc_lex::TokenKind::CharLiteral => Some("Char".into()),
                nexc_lex::TokenKind::NullLiteral => Some("Null".into()),
                nexc_lex::TokenKind::Identifier => Some(token.lexeme.clone()),
                _ => None,
            };
        }
    }
    None
}

/// Collect top-level symbol names (functions, classes, structs, interfaces) from an AST.
fn collect_symbols(ast: &SourceFile) -> Vec<String> {
    let mut names = Vec::new();
    for item in &ast.items {
        let name = match item {
            nexc_ast::Item::Function(f) => Some(f.name.clone()),
            nexc_ast::Item::Class(c) => Some(c.name.clone()),
            nexc_ast::Item::Interface(i) => Some(i.name.clone()),
            nexc_ast::Item::Struct(s) => Some(s.name.clone()),
            _ => None,
        };
        if let Some(n) = name {
            names.push(n);
        }
    }
    names
}

// ---------------------------------------------------------------------------
// Completion item lists
// ---------------------------------------------------------------------------

const KEYWORDS: &[&str] = &[
    "if", "else", "while", "for", "try", "catch", "finally", "throw",
    "return", "break", "continue", "using", "class", "interface", "struct",
    "def", "var", "public", "shared", "virtual", "override", "static",
    "alias", "import", "from", "as", "self",
];

const TYPES: &[&str] = &[
    "Bool", "Byte", "Int", "Int64", "Float", "Double", "Char", "String",
    "Unit", "Var", "Object", "Disposable",
];

const UI_FUNCTIONS: &[&str] = &[
    "ui_app_create", "ui_app_set_backend", "ui_app_set_root", "ui_app_run",
    "ui_app_quit", "ui_app_destroy", "ui_is_running", "ui_app_render",
    "ui_render", "ui_poll_event", "ui_event_type", "ui_event_widget",
    "ui_text", "ui_button", "ui_text_input", "ui_image", "ui_checkbox",
    "ui_slider", "ui_row", "ui_column", "ui_stack", "ui_scroll", "ui_grid",
    "ui_canvas", "ui_add_child", "ui_remove_child", "ui_set_id", "ui_get_id",
    "ui_set_text", "ui_get_text", "ui_set_visible", "ui_set_enabled",
    "ui_get_value_float", "ui_set_value_float",
    "ui_set_width", "ui_set_height", "ui_set_min_width", "ui_set_min_height",
    "ui_set_max_width", "ui_set_max_height",
    "ui_set_padding", "ui_set_padding_all", "ui_set_margin", "ui_set_margin_all",
    "ui_set_bg_color", "ui_set_fg_color", "ui_set_font_size",
    "ui_set_border", "ui_set_border_radius", "ui_set_border_width", "ui_set_border_color",
    "ui_set_flex_grow", "ui_set_flex_shrink",
    "ui_set_align_self", "ui_set_h_align", "ui_set_v_align",
    "ui_set_justify_content", "ui_set_align_items",
    "ui_set_gap", "ui_set_checked",
    "ui_on_click", "ui_on_change", "ui_on_hover", "ui_on_key",
    "ui_canvas_fill_rect", "ui_canvas_stroke_rect", "ui_canvas_fill_circle",
    "ui_canvas_draw_line", "ui_canvas_draw_text", "ui_canvas_clear",
    "ui_dialog_message", "ui_dialog_confirm", "ui_dialog_open_file", "ui_dialog_save_file",
];

const NEXUI_ELEMENTS: &[&str] = &[
    "Window", "Widget", "Row", "Column", "Stack", "Scroll", "Grid",
    "Text", "Button", "TextInput", "Image", "Checkbox", "Slider", "Canvas",
    "WidgetSwitcher", "Include",
];

const NEXUI_ATTRIBUTES: &[&str] = &[
    "Width", "Height", "MinWidth", "MinHeight", "MaxWidth", "MaxHeight",
    "Padding", "Margin", "FlexGrow", "FlexShrink", "Gap",
    "AlignItems", "JustifyContent", "AlignSelf",
    "HorizontalAlignment", "VerticalAlignment",
    "BgColor", "FgColor", "FontSize",
    "BorderWidth", "BorderColor", "BorderRadius",
    "Visible", "Enabled",
    "Text", "Label", "Placeholder", "Source", "Title", "Columns",
    "Min", "Max", "Value", "Checked",
    "Style", "Extends", "Widget", "Active",
    "Click", "Change", "Hover", "Key",
    "xmlns",
];

// ---------------------------------------------------------------------------
// LSP server entry point
// ---------------------------------------------------------------------------

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

fn is_nexui_uri(uri: &str) -> bool {
    uri.ends_with(".nexui")
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
                    "completionProvider": {
                        "triggerCharacters": [".", " ", "<", "\""]
                    }
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
            if is_nexui_uri(uri) {
                ctx.update_nexui_file(uri, text);
            } else {
                ctx.update_file(uri, text);
            }
            let diags = ctx.diagnostics_for(uri);
            let source = if is_nexui_uri(uri) {
                ctx.nexui_cache.get(uri).map(|c| c.source.clone()).unwrap_or_default()
            } else {
                ctx.module_cache.get(uri).map(|c| c.source.clone()).unwrap_or_default()
            };
            Some(publish_diagnostics(uri, &diags, &source))
        }

        "textDocument/didChange" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let changes = params.get("contentChanges")?.as_array()?;
            if let Some(change) = changes.last() {
                if let Some(text) = change.get("text").and_then(|t| t.as_str()) {
                    if is_nexui_uri(uri) {
                        ctx.update_nexui_file(uri, text);
                    } else {
                        ctx.update_file(uri, text);
                    }
                    let diags = ctx.diagnostics_for(uri);
                    let source = if is_nexui_uri(uri) {
                        ctx.nexui_cache.get(uri).map(|c| c.source.clone()).unwrap_or_default()
                    } else {
                        ctx.module_cache.get(uri).map(|c| c.source.clone()).unwrap_or_default()
                    };
                    return Some(publish_diagnostics(uri, &diags, &source));
                }
            }
            None
        }

        "textDocument/hover" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let pos = params.get("position")?;
            let line = pos.get("line")?.as_u64()? as usize;
            let character = pos.get("character")?.as_u64()? as usize;

            if let Some(cached) = ctx.module_cache.get(uri) {
                let offset = line_char_to_offset(&cached.source, line, character);
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
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let pos = params.get("position")?;
            let line = pos.get("line")?.as_u64()? as usize;
            let character = pos.get("character")?.as_u64()? as usize;

            if let Some(cached) = ctx.module_cache.get(uri) {
                let offset = line_char_to_offset(&cached.source, line, character);
                let name = word_at_offset(&cached.source, offset);
                if !name.is_empty() {
                    if let Some((_file, def_offset)) = goto_definition(&cached.source, name) {
                        let (def_line, def_char) = offset_to_line_char(&cached.source, def_offset);
                        let result = serde_json::json!({
                            "uri": uri,
                            "range": {
                                "start": { "line": def_line, "character": def_char },
                                "end": { "line": def_line, "character": def_char }
                            }
                        });
                        return Some(make_response(id, result));
                    }
                }
            }
            Some(make_response(id, serde_json::Value::Null))
        }

        "textDocument/completion" => {
            let params = json.get("params")?;
            let doc = params.get("textDocument")?;
            let uri = doc.get("uri")?.as_str()?;
            let pos = params.get("position")?;
            let line = pos.get("line")?.as_u64()? as usize;
            let character = pos.get("character")?.as_u64()? as usize;

            let items = if is_nexui_uri(uri) {
                nexui_completions(uri, ctx, line, character)
            } else {
                nex_completions(uri, ctx, line, character)
            };

            Some(make_response(id, serde_json::json!({ "isIncomplete": false, "items": items })))
        }

        _ => None,
    }
}

fn nex_completions(
    uri: &str,
    ctx: &LspContext,
    line: usize,
    character: usize,
) -> Vec<serde_json::Value> {
    let mut items: Vec<serde_json::Value> = Vec::new();

    // File symbols first (highest priority)
    if let Some(cached) = ctx.module_cache.get(uri) {
        let offset = line_char_to_offset(&cached.source, line, character);
        let prefix = word_at_offset(&cached.source, offset);
        for sym in collect_symbols(&cached.ast) {
            if sym.starts_with(prefix) {
                items.push(completion_item(&sym, 3 /* Function */, None));
            }
        }
    }

    // Keywords
    for &kw in KEYWORDS {
        items.push(completion_item(kw, 14 /* Keyword */, None));
    }

    // Types
    for &ty in TYPES {
        items.push(completion_item(ty, 7 /* Class */, None));
    }

    // UI stdlib functions
    for &func in UI_FUNCTIONS {
        items.push(completion_item(func, 3 /* Function */, Some("std.ui")));
    }

    items
}

fn nexui_completions(
    uri: &str,
    ctx: &LspContext,
    line: usize,
    character: usize,
) -> Vec<serde_json::Value> {
    let mut items: Vec<serde_json::Value> = Vec::new();

    // Determine context: are we inside a tag (attribute) or between tags (element)?
    let in_attr = if let Some(cached) = ctx.nexui_cache.get(uri) {
        let offset = line_char_to_offset(&cached.source, line, character);
        let before = &cached.source[..offset.min(cached.source.len())];
        // Heuristic: if we're inside an open tag (after < but before >) suggest attributes
        let last_open = before.rfind('<');
        let last_close = before.rfind('>');
        match (last_open, last_close) {
            (Some(o), Some(c)) => o > c,
            (Some(_), None) => true,
            _ => false,
        }
    } else {
        false
    };

    if in_attr {
        for &attr in NEXUI_ATTRIBUTES {
            items.push(completion_item(attr, 10 /* Property */, None));
        }
    } else {
        for &elem in NEXUI_ELEMENTS {
            items.push(completion_item(elem, 7 /* Class */, None));
        }
        for &attr in NEXUI_ATTRIBUTES {
            items.push(completion_item(attr, 10 /* Property */, None));
        }
    }

    items
}

fn completion_item(label: &str, kind: u32, detail: Option<&str>) -> serde_json::Value {
    let mut item = serde_json::json!({
        "label": label,
        "kind": kind,
    });
    if let Some(d) = detail {
        item["detail"] = serde_json::Value::String(d.to_string());
    }
    item
}

fn make_response(id: Option<&serde_json::Value>, result: serde_json::Value) -> String {
    let resp = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id.cloned().unwrap_or(serde_json::Value::Null),
        "result": result,
    });
    resp.to_string()
}

fn publish_diagnostics(uri: &str, diags: &[Diagnostic], source: &str) -> String {
    let items: Vec<serde_json::Value> = diags.iter().map(|d| {
        let severity = match d.severity {
            nexc_diag::Severity::Error => 1,
            nexc_diag::Severity::Warning => 2,
            nexc_diag::Severity::Note => 3,
            nexc_diag::Severity::Help => 4,
        };
        let (start_line, start_char) = d.span
            .map(|s| offset_to_line_char(source, s.lo))
            .unwrap_or((0, 0));
        let (end_line, end_char) = d.span
            .map(|s| offset_to_line_char(source, s.hi))
            .unwrap_or((start_line, start_char));
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
