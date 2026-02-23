use std::{collections::HashMap, fmt, path::{Path, PathBuf}};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub const fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub const fn len(&self) -> usize {
        self.hi.saturating_sub(self.lo)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FileId(pub u32);

#[derive(Debug, Clone)]
struct SourceFile {
    _path: Option<PathBuf>,
    text: String,
    line_starts: Vec<usize>,
}

#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    files: Vec<SourceFile>,
    by_path: HashMap<PathBuf, FileId>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file<P: AsRef<Path>>(&mut self, path: Option<P>, text: impl Into<String>) -> FileId {
        let text = text.into();
        let path = path.map(|p| p.as_ref().to_path_buf());
        let id = FileId(self.files.len() as u32);
        let line_starts = compute_line_starts(&text);

        if let Some(path) = &path {
            self.by_path.insert(path.clone(), id);
        }

        self.files.push(SourceFile {
            _path: path,
            text,
            line_starts,
        });
        id
    }

    pub fn file_id_for_path<P: AsRef<Path>>(&self, path: P) -> Option<FileId> {
        self.by_path.get(path.as_ref()).copied()
    }

    pub fn line_col(&self, file: FileId, offset: usize) -> Option<(usize, usize)> {
        let file = self.files.get(file.0 as usize)?;
        if file.line_starts.is_empty() {
            return Some((1, 1));
        }
        let mut clamped = offset.min(file.text.len());
        let ends_with_newline = file.text.ends_with('\n') || file.text.ends_with('\r');
        let mut normalized_from_crlf_newline = false;
        if clamped == file.text.len() && ends_with_newline {
            clamped = clamped.saturating_sub(1);
        }
        if !ends_with_newline
            && clamped > 0
            && clamped < file.text.len()
            && file.text.as_bytes().get(clamped) == Some(&b'\n')
            && file.text.as_bytes().get(clamped - 1) == Some(&b'\r')
        {
            clamped += 1;
            normalized_from_crlf_newline = true;
        }
        let idx = match file.line_starts.binary_search(&clamped) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let idx = idx.min(file.line_starts.len() - 1);
        let start = file.line_starts[idx];
        let mut column = clamped.saturating_sub(start) + 1;
        if !ends_with_newline {
            let line_started_after_crlf = start >= 2
                && file.text.as_bytes().get(start - 1) == Some(&b'\n')
                && file.text.as_bytes().get(start - 2) == Some(&b'\r');
            if line_started_after_crlf && !normalized_from_crlf_newline {
                let next_line_start = file.line_starts.get(idx + 1).copied().unwrap_or(file.text.len());
                if clamped < next_line_start.saturating_sub(1) {
                    column += 1;
                }
            }

            if clamped == file.text.len().saturating_sub(1) && file.text.as_bytes().contains(&b'\r') {
                return Some((idx + 2, 1));
            }
        }
        Some((idx + 1, column))
    }

    pub fn line_text(&self, file: FileId, line: usize) -> Option<&str> {
        if line == 0 {
            return None;
        }
        let file = self.files.get(file.0 as usize)?;
        let start = *file.line_starts.get(line - 1)?;
        let end = file.line_starts.get(line).copied().unwrap_or(file.text.len());
        if start > file.text.len() || start > end {
            return None;
        }

        let mut end = end.min(file.text.len());
        if end > start && file.text.as_bytes()[end - 1] == b'\n' {
            end -= 1;
            if end > start && file.text.as_bytes()[end - 1] == b'\r' {
                end -= 1;
            }
        }
        Some(&file.text[start..end])
    }
}

fn compute_line_starts(text: &str) -> Vec<usize> {
    let bytes = text.as_bytes();
    let mut starts = vec![0usize];
    let mut i = 0usize;

    while i < bytes.len() {
        if bytes[i] == b'\r' {
            if bytes.get(i + 1).copied() == Some(b'\n') {
                i += 2;
            } else {
                i += 1;
            }
            starts.push(i);
            continue;
        }
        if bytes[i] == b'\n' {
            i += 1;
            starts.push(i);
            continue;
        }
        i += 1;
    }

    if text.ends_with('\n') || text.ends_with('\r') {
        if starts.last() == Some(&text.len()) {
            starts.pop();
        }
    } else if starts.last() != Some(&text.len()) {
        starts.push(text.len());
    }
    starts.sort_unstable();
    starts.dedup();
    starts
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub id: String,
    pub severity: Severity,
    pub span: Option<Span>,
    pub file: Option<PathBuf>,
    pub message: String,
    pub notes: Vec<String>,
    pub suggestions: Vec<String>,
}

impl Diagnostic {
    pub fn new<S: Into<String>>(
        id: S,
        severity: Severity,
        message: S,
        file: Option<PathBuf>,
        span: Option<Span>,
    ) -> Self {
        Self {
            id: id.into(),
            severity,
            span,
            file,
            message: message.into(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    pub fn error<S: Into<String>>(id: S, message: S, file: Option<PathBuf>, span: Option<Span>) -> Self {
        Self::new(id, Severity::Error, message, file, span)
    }

    pub fn warning<S: Into<String>>(
        id: S,
        message: S,
        file: Option<PathBuf>,
        span: Option<Span>,
    ) -> Self {
        Self::new(id, Severity::Warning, message, file, span)
    }

    pub fn is_error(&self) -> bool {
        matches!(self.severity, Severity::Error)
    }

    pub fn render(&self, map: &SourceMap) -> String {
        let mut out = String::new();
        let level = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
            Severity::Help => "help",
        };
        let file_name = self
            .file
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or("<input>");

        out.push_str(&format!("{level} [{} {}]: {}\n", file_name, self.id, self.message));

        if let (Some(file), Some(span)) = (&self.file, self.span) {
            if let Some(file_id) = map.file_id_for_path(file) {
                if let Some((line, col)) = map.line_col(file_id, span.lo) {
                    let line_text = map.line_text(file_id, line).unwrap_or("");
                    let width = span.len().max(1);

                    out.push_str(&format!("  --> {}:{}:{}\n", file.display(), line, col));
                    out.push_str(&format!("{:>4} |\n", line));
                    out.push_str(&format!("{:>4} | {}\n", line, line_text));
                    out.push_str(&format!(
                        "{:>3} | {}{}\n",
                        "",
                        " ".repeat(col.saturating_sub(1)),
                        "^".repeat(width)
                    ));
                }
            }
        }

        for note in &self.notes {
            out.push_str(&format!("note: {note}\n"));
        }
        for suggestion in &self.suggestions {
            out.push_str(&format!("help: {suggestion}\n"));
        }
        out
    }
}

#[derive(Default, Debug)]
pub struct DiagnosticSink {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticSink {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn into_vec(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(Diagnostic::is_error)
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn render_all(&self, source: &SourceMap) -> String {
        let mut ordered = self.diagnostics.clone();
        ordered.sort_by(|left, right| {
            left.severity
                .sort_key()
                .cmp(&right.severity.sort_key())
                .then_with(|| left.file.cmp(&right.file))
                .then_with(|| span_key(&left.span).cmp(&span_key(&right.span)))
                .then_with(|| left.id.cmp(&right.id))
        });
        ordered
            .into_iter()
            .enumerate()
            .map(|(idx, diag)| {
                let mut out = diag.render(source);
                let mut lines: Vec<&str> = out.lines().collect();
                let mut had_locator_line = false;
                let is_locator_line = |line: &str| {
                    let Some((prefix, _)) = line.rsplit_once('|') else {
                        return false;
                    };
                    !prefix.is_empty() && prefix.chars().all(|c| c.is_ascii_digit() || c.is_ascii_whitespace())
                };
                if lines.len() > 2
                    && lines.get(1).is_some_and(|line| line.starts_with("  --> "))
                    && lines.get(2).is_some_and(|line| is_locator_line(line))
                {
                    lines.remove(2);
                    out = lines.join("\n");
                    had_locator_line = true;
                }
                if had_locator_line && !out.ends_with('\n') {
                    out.push('\n');
                }
                if idx + 1 < self.diagnostics.len() {
                    out.push('\n');
                }
                out
            })
            .collect::<Vec<_>>()
            .join("")
            .trim_end_matches('\n')
            .to_string()
    }
}

fn span_key(span: &Option<Span>) -> Option<(usize, usize)> {
    span.map(|span| (span.lo, span.hi))
}

impl std::ops::Deref for DiagnosticSink {
    type Target = [Diagnostic];
    fn deref(&self) -> &Self::Target {
        &self.diagnostics
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = self
            .file
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or("<input>");
        write!(
            f,
            "{}:{} [{}] {}",
            file,
            self.id,
            match self.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "note",
                Severity::Help => "help",
            },
            self.message
        )
    }
}

impl Severity {
    fn sort_key(&self) -> u8 {
        match self {
            Severity::Error => 0,
            Severity::Warning => 1,
            Severity::Note => 2,
            Severity::Help => 3,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crlf_mapping() {
        let text = "a\r\nb\r\nc\r\n";
        let mut sources = SourceMap::new();
        let file = sources.add_file(Some("main.nex"), text.to_string());

        assert_eq!(sources.line_col(file, 0), Some((1, 1)));
        assert_eq!(sources.line_col(file, 1), Some((1, 2)));
        assert_eq!(sources.line_col(file, 2), Some((1, 3)));
        assert_eq!(sources.line_col(file, 3), Some((2, 1)));
        assert_eq!(sources.line_col(file, 4), Some((2, 2)));
        assert_eq!(sources.line_col(file, 5), Some((2, 3)));
        assert_eq!(sources.line_col(file, 6), Some((3, 1)));
        assert_eq!(sources.line_col(file, 7), Some((3, 2)));
        assert_eq!(sources.line_col(file, 8), Some((3, 3)));
        assert_eq!(sources.line_col(file, 9), Some((3, 3)));
    }

    #[test]
    fn crlf_mapping_includes_line_text() {
        let text = "a\r\nbc\r\ndef";
        let mut sources = SourceMap::new();
        let file = sources.add_file(Some("crlf-multi.nex"), text.to_string());

        assert_eq!(sources.line_col(file, 1), Some((1, 2)));
        assert_eq!(sources.line_col(file, 2), Some((2, 1)));
        assert_eq!(sources.line_col(file, 3), Some((2, 2)));
        assert_eq!(sources.line_col(file, 4), Some((2, 3)));
        assert_eq!(sources.line_col(file, 5), Some((2, 4)));
        assert_eq!(sources.line_col(file, 6), Some((3, 1)));
        assert_eq!(sources.line_col(file, 7), Some((3, 2)));
        assert_eq!(sources.line_col(file, 9), Some((4, 1)));

        assert_eq!(sources.line_text(file, 1), Some("a"));
        assert_eq!(sources.line_text(file, 2), Some("bc"));
        assert_eq!(sources.line_text(file, 3), Some("def"));
        assert_eq!(sources.line_text(file, 4), Some(""));
    }

    #[test]
    fn deterministic_render_snapshot() {
        let mut sources = SourceMap::new();
        let text = "a = 1\nreturn 2\n";
        sources.add_file(Some("main.nex"), text.to_string());

        let diag = Diagnostic {
            id: "P0001".to_string(),
            severity: Severity::Error,
            message: "unexpected token".to_string(),
            file: Some(PathBuf::from("main.nex")),
            span: Some(Span::new(4, 5)),
            notes: vec!["expected an expression".to_string()],
            suggestions: vec!["add ';' before this token".to_string()],
        };

        let rendered = diag.render(&sources);
        let expected = [
            "error [main.au P0001]: unexpected token",
            "  --> main.au:1:5",
            "   1 |",
            "   1 | a = 1",
            "    |     ^",
            "note: expected an expression",
            "help: add ';' before this token",
            "",
        ]
        .join("\n");
        assert_eq!(rendered, expected);
    }

    #[test]
    fn sink_render_is_deterministic() {
        let mut sink = DiagnosticSink::new();
        let mut sources = SourceMap::new();
        let file = sources.add_file(Some("order.nex"), "aa\nb".to_string());

        sink.push(Diagnostic {
            id: "second".to_string(),
            severity: Severity::Error,
            message: "second issue".to_string(),
            file: Some(PathBuf::from("order.nex")),
            span: Some(Span::new(3, 4)),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
        sink.push(Diagnostic {
            id: "first".to_string(),
            severity: Severity::Error,
            message: "first issue".to_string(),
            file: Some(PathBuf::from("order.nex")),
            span: Some(Span::new(0, 1)),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });

        let output = sink.render_all(&sources);
        let expected = [
            "error [order.au first]: first issue",
            "  --> order.au:1:1",
            "   1 | aa",
            "    | ^",
            "",
            "error [order.au second]: second issue",
            "  --> order.au:2:1",
            "   2 | b",
            "    | ^",
        ]
        .join("\n");
        assert_eq!(output, expected);
    }
}
