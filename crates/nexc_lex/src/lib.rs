use std::path::PathBuf;

use nexc_diag::{Diagnostic, DiagnosticSink, Severity, Span};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Eof,
    Identifier,
    IntLiteral,
    FloatLiteral,
    CharLiteral,
    StringLiteral,
    BooleanLiteral,
    NullLiteral,
    TypeBool,
    TypeByte,
    TypeShort,
    TypeUShort,
    TypeInt,
    TypeUInt,
    TypeLong,
    TypeULong,
    TypeInt8,
    TypeInt16,
    TypeInt32,
    TypeInt64,
    TypeUInt8,
    TypeUInt16,
    TypeUInt32,
    TypeUInt64,
    TypeFloat,
    TypeFloat32,
    TypeFloat64,
    TypeDouble,
    TypeChar,
    TypeString,
    TypeUnit,
    TypeVar,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    DotDot,
    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    SyntheticSemicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    Eq,
    EqEq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    AndAnd,
    OrOr,
    Bang,
    Amp,
    Pipe,
    Caret,
    Tilde,
    Shl,
    Shr,
    AmpEq,
    PipeEq,
    CaretEq,
    ShlEq,
    ShrEq,
    Question,
    Arrow,
    Return,
    Break,
    Continue,
    Public,
    Var,
    Using,
    Shared,
    Virtual,
    Override,
    Static,
    Operator,
    Class,
    Struct,
    Interface,
    Enum,
    Async,
    Await,
    Match,
    InterpolatedString,
    Def,
    If,
    Else,
    While,
    For,
    Try,
    Catch,
    Finally,
    Throw,
    From,
    As,
    Import,
    Newline,
    Unknown(char),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, lo: usize, hi: usize, lexeme: String) -> Self {
        Self {
            kind,
            span: Span::new(lo, hi),
            lexeme,
        }
    }
}

pub fn lex(source: &str, file: Option<String>, sink: &mut DiagnosticSink) -> Vec<Token> {
    let mut lexer = Lexer::new(source, file);
    lexer.scan(sink)
}

pub fn should_insert_terminator(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Identifier
            | TokenKind::IntLiteral
            | TokenKind::FloatLiteral
            | TokenKind::StringLiteral
            | TokenKind::BooleanLiteral
            | TokenKind::NullLiteral
            | TokenKind::CharLiteral
            | TokenKind::InterpolatedString
            | TokenKind::RParen
            | TokenKind::RBracket
            | TokenKind::RBrace
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Public
    )
}

/// Returns `true` if `kind` is a binary/infix operator that can continue an
/// expression from the previous line.  When a newline separates a terminable
/// token and one of these operators the ASI pass must **not** insert a
/// semicolon so the multi-line expression stays intact.
///
/// Only operators that are **unambiguously binary** (never valid as unary
/// prefix) are included.  `+` and `-` are excluded because they can be
/// unary and starting a new statement with `-expr` is a valid pattern.
fn is_continuation_operator(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::EqEq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq
            | TokenKind::Dot
            | TokenKind::Pipe
            | TokenKind::Amp
            | TokenKind::Caret
    )
}

pub fn asi_normalize(tokens: &[Token]) -> Vec<Token> {
    let mut output = Vec::new();
    let mut paren_depth: isize = 0;
    let mut bracket_depth: isize = 0;
    let mut prev_significant: Option<&TokenKind> = None;

    let len = tokens.len();
    let mut i = 0;
    while i < len {
        let token = &tokens[i];

        match token.kind {
            TokenKind::LParen => paren_depth += 1,
            TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LBracket => bracket_depth += 1,
            TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
            _ => {}
        }

        if matches!(token.kind, TokenKind::Newline) {
            if matches!(prev_significant, Some(kind) if should_insert_terminator(kind))
                && paren_depth == 0
                && bracket_depth == 0
            {
                // Look ahead past any further newlines to find the next
                // significant token.  If it is a continuation operator the
                // expression spans multiple lines and we must NOT insert a
                // semicolon.
                let mut next_significant: Option<&TokenKind> = None;
                for j in (i + 1)..len {
                    if !matches!(tokens[j].kind, TokenKind::Newline) {
                        next_significant = Some(&tokens[j].kind);
                        break;
                    }
                }

                if !matches!(next_significant, Some(k) if is_continuation_operator(k)) {
                    output.push(Token::new(
                        TokenKind::SyntheticSemicolon,
                        token.span.lo,
                        token.span.hi,
                        ";".to_string(),
                    ));
                }
            }
            i += 1;
            continue;
        }

        match token.kind {
            TokenKind::Semicolon | TokenKind::SyntheticSemicolon => {
                prev_significant = None;
            }
            TokenKind::Eof => {
                prev_significant = None;
            }
            _ => {
                prev_significant = Some(&token.kind);
            }
        }

        output.push(token.clone());
        if matches!(token.kind, TokenKind::Eof) {
            break;
        }
        i += 1;
    }

    if !matches!(output.last().map(|token| &token.kind), Some(TokenKind::Eof)) {
        let end = tokens.last().map(|token| token.span.hi).unwrap_or(0);
        output.push(Token::new(TokenKind::Eof, end, end, String::new()));
    }

    output
}

#[derive(Debug)]
struct Lexer {
    source: Vec<u8>,
    pos: usize,
    len: usize,
    file: Option<String>,
}

impl Lexer {
    fn new(source: &str, file: Option<String>) -> Self {
        Self {
            source: source.as_bytes().to_vec(),
            len: source.len(),
            pos: 0,
            file,
        }
    }

    fn scan(&mut self, sink: &mut DiagnosticSink) -> Vec<Token> {
        let mut tokens = Vec::new();

        while self.pos < self.len {
            let ch = self.source[self.pos];

            match ch {
                b' ' | b'\t' | b'\x0c' | b'\x0b' => {
                    self.pos += 1;
                }
                b'\r' | b'\n' => {
                    self.emit_newline(&mut tokens);
                }
                b'$' if self.pos + 1 < self.len && self.source[self.pos + 1] == b'"' => {
                    tokens.push(self.scan_interpolated_string(sink));
                }
                b'"' => {
                    tokens.push(self.scan_string(sink));
                }
                b'\'' => {
                    tokens.push(self.scan_char());
                }
                b'/' => {
                    if self.matches(b"//") {
                        self.skip_line_comment();
                        continue;
                    }
                    if self.matches(b"/*") {
                        self.scan_block_comment(sink, &mut tokens);
                        continue;
                    }
                    tokens.push(self.scan_multi_or_single(
                        b"/=",
                        TokenKind::Slash,
                        TokenKind::SlashEq,
                    ));
                }
                b'+' => {
                    tokens.push(self.scan_multi_or_single(
                        b"+=",
                        TokenKind::Plus,
                        TokenKind::PlusEq,
                    ));
                }
                b'-' => {
                    if self.matches(b"->") {
                        let start = self.pos;
                        self.pos += 2;
                        tokens.push(Token::new(
                            TokenKind::Arrow,
                            start,
                            self.pos,
                            "->".to_string(),
                        ));
                    } else {
                        tokens.push(self.scan_multi_or_single(
                            b"-=",
                            TokenKind::Minus,
                            TokenKind::MinusEq,
                        ));
                    }
                }
                b'*' => {
                    tokens.push(self.scan_multi_or_single(
                        b"*=",
                        TokenKind::Star,
                        TokenKind::StarEq,
                    ));
                }
                b'%' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::Percent, start, self.pos, "%".to_string()));
                }
                b'{' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::LBrace, start, self.pos, "{".to_string()));
                }
                b'}' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::RBrace, start, self.pos, "}".to_string()));
                }
                b'(' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::LParen, start, self.pos, "(".to_string()));
                }
                b')' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::RParen, start, self.pos, ")".to_string()));
                }
                b'[' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::LBracket, start, self.pos, "[".to_string()));
                }
                b']' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::RBracket, start, self.pos, "]".to_string()));
                }
                b'.' => {
                    let start = self.pos;
                    if self.matches(b"::") {
                        self.pos += 2;
                        tokens.push(Token::new(
                            TokenKind::DoubleColon,
                            start,
                            self.pos,
                            "::".to_string(),
                        ));
                    } else if self.matches(b"..") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::DotDot, start, self.pos, "..".to_string()));
                    } else {
                        self.pos += 1;
                        tokens.push(Token::new(TokenKind::Dot, start, self.pos, ".".to_string()));
                    }
                }
                b',' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::Comma, start, self.pos, ",".to_string()));
                }
                b':' => {
                    let start = self.pos;
                    if self.matches(b"::") {
                        self.pos += 2;
                        tokens.push(Token::new(
                            TokenKind::DoubleColon,
                            start,
                            self.pos,
                            "::".to_string(),
                        ));
                    } else {
                        self.pos += 1;
                        tokens.push(Token::new(TokenKind::Colon, start, self.pos, ":".to_string()));
                    }
                }
                b';' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::Semicolon, start, self.pos, ";".to_string()));
                }
                b'=' => {
                    tokens.push(self.scan_multi_or_single(
                        b"==",
                        TokenKind::Eq,
                        TokenKind::EqEq,
                    ));
                }
                b'!' => {
                    tokens.push(self.scan_multi_or_single(
                        b"!=",
                        TokenKind::Bang,
                        TokenKind::NotEq,
                    ));
                }
                b'<' => {
                    let start = self.pos;
                    if self.matches(b"<<=") {
                        self.pos += 3;
                        tokens.push(Token::new(TokenKind::ShlEq, start, self.pos, "<<=".to_string()));
                    } else if self.matches(b"<<") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::Shl, start, self.pos, "<<".to_string()));
                    } else {
                        tokens.push(self.scan_multi_or_single(
                            b"<=",
                            TokenKind::Lt,
                            TokenKind::LtEq,
                        ));
                    }
                }
                b'>' => {
                    let start = self.pos;
                    if self.matches(b">>=") {
                        self.pos += 3;
                        tokens.push(Token::new(TokenKind::ShrEq, start, self.pos, ">>=".to_string()));
                    } else if self.matches(b">>") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::Shr, start, self.pos, ">>".to_string()));
                    } else {
                        tokens.push(self.scan_multi_or_single(
                            b">=",
                            TokenKind::Gt,
                            TokenKind::GtEq,
                        ));
                    }
                }
                b'&' => {
                    let start = self.pos;
                    if self.matches(b"&&") {
                        self.pos += 2;
                        tokens.push(Token::new(
                            TokenKind::AndAnd,
                            start,
                            self.pos,
                            "&&".to_string(),
                        ));
                    } else if self.matches(b"&=") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::AmpEq, start, self.pos, "&=".to_string()));
                    } else {
                        self.pos += 1;
                        tokens.push(Token::new(TokenKind::Amp, start, self.pos, "&".to_string()));
                    }
                }
                b'|' => {
                    let start = self.pos;
                    if self.matches(b"||") {
                        self.pos += 2;
                        tokens.push(Token::new(
                            TokenKind::OrOr,
                            start,
                            self.pos,
                            "||".to_string(),
                        ));
                    } else if self.matches(b"|=") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::PipeEq, start, self.pos, "|=".to_string()));
                    } else {
                        self.pos += 1;
                        tokens.push(Token::new(TokenKind::Pipe, start, self.pos, "|".to_string()));
                    }
                }
                b'^' => {
                    let start = self.pos;
                    if self.matches(b"^=") {
                        self.pos += 2;
                        tokens.push(Token::new(TokenKind::CaretEq, start, self.pos, "^=".to_string()));
                    } else {
                        self.pos += 1;
                        tokens.push(Token::new(TokenKind::Caret, start, self.pos, "^".to_string()));
                    }
                }
                b'~' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(TokenKind::Tilde, start, self.pos, "~".to_string()));
                }
                b'?' => {
                    let start = self.pos;
                    self.pos += 1;
                    tokens.push(Token::new(
                        TokenKind::Question,
                        start,
                        self.pos,
                        "?".to_string(),
                    ));
                }
                _ if is_identifier_start(ch) => {
                    tokens.push(self.scan_identifier());
                }
                _ if ch.is_ascii_digit() => {
                    tokens.push(self.scan_number());
                }
                _ => {
                    let start = self.pos;
                    self.pos += 1;
                    let token = Token::new(
                        TokenKind::Unknown(ch as char),
                        start,
                        self.pos,
                        String::from_utf8(vec![ch]).unwrap_or_else(|_| "?".to_string()),
                    );
                    sink.push(Diagnostic {
                        id: "lex_invalid_char".to_string(),
                        severity: Severity::Error,
                        span: Some(token.span),
                        file: self.file.clone().map(PathBuf::from),
                        message: format!("invalid character `{}`", ch as char),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                    tokens.push(token);
                }
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.len, self.len),
            lexeme: String::new(),
        });

        tokens
    }

    fn emit_newline(&mut self, out: &mut Vec<Token>) {
        let start = self.pos;
        if self.source[self.pos] == b'\r' && self.source.get(self.pos + 1) == Some(&b'\n') {
            self.pos += 2;
            out.push(Token::new(
                TokenKind::Newline,
                start,
                self.pos,
                "\r\n".to_string(),
            ));
        } else {
            self.pos += 1;
            let ch = self.source[start];
            out.push(Token::new(
                TokenKind::Newline,
                start,
                self.pos,
                if ch == b'\r' {
                    "\r".to_string()
                } else {
                    "\n".to_string()
                },
            ));
        }
    }

    fn skip_line_comment(&mut self) {
        self.pos += 2;
        while self.pos < self.len {
            if self.source[self.pos] == b'\r' || self.source[self.pos] == b'\n' {
                break;
            }
            self.pos += 1;
        }
    }

    fn scan_block_comment(&mut self, sink: &mut DiagnosticSink, out: &mut Vec<Token>) {
        self.pos += 2;
        while self.pos < self.len {
            if self.source[self.pos] == b'\r' && self.source.get(self.pos + 1) == Some(&b'\n') {
                out.push(Token::new(
                    TokenKind::Newline,
                    self.pos,
                    self.pos + 2,
                    "\r\n".to_string(),
                ));
                self.pos += 2;
                continue;
            }

            if self.source[self.pos] == b'\n' {
                out.push(Token::new(TokenKind::Newline, self.pos, self.pos + 1, "\n".to_string()));
                self.pos += 1;
                continue;
            }

            if self.source[self.pos] == b'\r' {
                out.push(Token::new(TokenKind::Newline, self.pos, self.pos + 1, "\r".to_string()));
                self.pos += 1;
                continue;
            }

            if self.source[self.pos] == b'*' && self.source.get(self.pos + 1) == Some(&b'/') {
                self.pos += 2;
                return;
            }

            self.pos += 1;
        }

        sink.push(Diagnostic {
            id: "lex_block_comment_unterminated".to_string(),
            severity: Severity::Error,
            span: Some(Span::new(self.pos, self.len)),
            file: self.file.clone().map(PathBuf::from),
            message: "unterminated block comment".to_string(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
    }

    fn scan_identifier(&mut self) -> Token {
        let start = self.pos;
        self.pos += 1;
        while self.pos < self.len && is_identifier_continue(self.source[self.pos]) {
            self.pos += 1;
        }

        let lexeme = std::str::from_utf8(&self.source[start..self.pos])
            .unwrap_or("")
            .to_string();

        Token::new(token_kind_for_identifier(&lexeme), start, self.pos, lexeme)
    }

    fn scan_number(&mut self) -> Token {
        let start = self.pos;

        // Check for hex literal: 0x or 0X
        if self.pos + 1 < self.len
            && self.source[self.pos] == b'0'
            && matches!(self.source[self.pos + 1], b'x' | b'X')
        {
            self.pos += 2; // skip '0x'
            while self.pos < self.len && self.source[self.pos].is_ascii_hexdigit() {
                self.pos += 1;
            }
            // Optional integer suffix
            if self.pos < self.len && matches!(self.source[self.pos], b'i' | b'I' | b'u' | b'U' | b'l' | b'L') {
                self.pos += 1;
            }
            let lexeme = std::str::from_utf8(&self.source[start..self.pos])
                .unwrap_or("")
                .to_string();
            return Token::new(TokenKind::IntLiteral, start, self.pos, lexeme);
        }

        while self.pos < self.len && self.source[self.pos].is_ascii_digit() {
            self.pos += 1;
        }

        let mut is_float = false;

        if self.pos < self.len && self.source[self.pos] == b'.' {
            if self.source.get(self.pos + 1).is_some_and(|next| next.is_ascii_digit()) {
                self.pos += 1;
                is_float = true;
                while self.pos < self.len && self.source[self.pos].is_ascii_digit() {
                    self.pos += 1;
                }
            }
        }

        if !is_float && self.pos < self.len && matches!(self.source[self.pos], b'e' | b'E') {
            self.pos += 1;
            is_float = true;
            if self.pos < self.len && matches!(self.source[self.pos], b'+' | b'-') {
                self.pos += 1;
            }
            while self.pos < self.len && self.source[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
        }

        if is_float && self.pos < self.len {
            if matches!(self.source[self.pos], b'f' | b'F' | b'd' | b'D') {
                self.pos += 1;
            }
        } else if self.pos < self.len && matches!(self.source[self.pos], b'i' | b'I' | b'u' | b'U' | b'l' | b'L' | b'f' | b'F' | b'd' | b'D') {
            self.pos += 1;
            is_float = matches!(
                self.source[self.pos.saturating_sub(1)],
                b'f' | b'F' | b'd' | b'D'
            );
        }

        let lexeme = std::str::from_utf8(&self.source[start..self.pos])
            .unwrap_or("")
            .to_string();
        Token::new(
            if is_float {
                TokenKind::FloatLiteral
            } else {
                TokenKind::IntLiteral
            },
            start,
            self.pos,
            lexeme,
        )
    }

    fn scan_char(&mut self) -> Token {
        let start = self.pos;
        self.pos += 1;
        let mut escaped = false;

        while self.pos < self.len {
            let ch = self.source[self.pos];
            if escaped {
                escaped = false;
                self.pos += 1;
                continue;
            }
            if ch == b'\\' {
                escaped = true;
                self.pos += 1;
                continue;
            }
            if ch == b'\'' {
                self.pos += 1;
                break;
            }
            if ch == b'\r' || ch == b'\n' {
                break;
            }
            self.pos += 1;
        }

        if self.pos > self.len {
            self.pos = self.len;
        }

        if self.pos >= self.len || self.source.get(self.pos - 1).copied() != Some(b'\'') {
            // Unterminated char literal; advance to EOF if we have not already done so.
            self.pos = self.len;
        }

        Token::new(
            TokenKind::CharLiteral,
            start,
            self.pos.min(self.len),
            std::str::from_utf8(&self.source[start..self.pos.min(self.len)])
                .unwrap_or("")
                .to_string(),
        )
    }

    fn scan_string(&mut self, sink: &mut DiagnosticSink) -> Token {
        let start = self.pos;
        self.pos += 1;
        let mut escaped = false;
        let mut reported_newline = false;

        while self.pos < self.len {
            let ch = self.source[self.pos];

            if escaped {
                escaped = false;
                self.pos += 1;
                continue;
            }

            if ch == b'\\' {
                escaped = true;
                self.pos += 1;
                continue;
            }

            if ch == b'"' {
                self.pos += 1;
                break;
            }

            if ch == b'\r' || ch == b'\n' {
                if !reported_newline {
                    sink.push(Diagnostic {
                        id: "lex_string_newline".to_string(),
                        severity: Severity::Error,
                        span: Some(Span::new(start, self.pos)),
                        file: self.file.clone().map(PathBuf::from),
                        message: "newline in string literal".to_string(),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                    reported_newline = true;
                }
                if ch == b'\r' && self.source.get(self.pos + 1).copied() == Some(b'\n') {
                    self.pos += 2;
                } else {
                    self.pos += 1;
                }
                continue;
            }

            self.pos += 1;
        }

        if self.pos <= start || self.source.get(self.pos - 1).copied() != Some(b'"') {
            sink.push(Diagnostic {
                id: "lex_string_unterminated".to_string(),
                severity: Severity::Error,
                span: Some(Span::new(start, self.len)),
                file: self.file.clone().map(PathBuf::from),
                message: "unterminated string literal".to_string(),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }

        Token::new(
            TokenKind::StringLiteral,
            start,
            self.pos.min(self.len),
            std::str::from_utf8(&self.source[start..self.pos.min(self.len)])
                .unwrap_or("")
                .to_string(),
        )
    }

    fn scan_interpolated_string(&mut self, sink: &mut DiagnosticSink) -> Token {
        let start = self.pos;
        self.pos += 2; // skip '$"'
        let mut brace_depth: u32 = 0;
        let mut escaped = false;
        let mut in_inner_string = false;

        while self.pos < self.len {
            let ch = self.source[self.pos];

            if escaped {
                escaped = false;
                self.pos += 1;
                continue;
            }

            if ch == b'\\' {
                escaped = true;
                self.pos += 1;
                continue;
            }

            // Inside a nested string within {expr}, skip until closing quote
            if in_inner_string {
                if ch == b'"' {
                    in_inner_string = false;
                }
                self.pos += 1;
                continue;
            }

            if brace_depth > 0 && ch == b'"' {
                in_inner_string = true;
                self.pos += 1;
                continue;
            }

            if ch == b'{' {
                brace_depth += 1;
                self.pos += 1;
                continue;
            }

            if ch == b'}' {
                brace_depth = brace_depth.saturating_sub(1);
                self.pos += 1;
                continue;
            }

            if ch == b'"' && brace_depth == 0 {
                self.pos += 1; // consume closing "
                break;
            }

            if ch == b'\r' || ch == b'\n' {
                sink.push(Diagnostic {
                    id: "lex_interp_string_newline".to_string(),
                    severity: Severity::Error,
                    span: Some(Span::new(start, self.pos)),
                    file: self.file.clone().map(PathBuf::from),
                    message: "newline in interpolated string literal".to_string(),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
                break;
            }

            self.pos += 1;
        }

        if self.pos <= start + 2 || self.source.get(self.pos - 1).copied() != Some(b'"') {
            sink.push(Diagnostic {
                id: "lex_interp_string_unterminated".to_string(),
                severity: Severity::Error,
                span: Some(Span::new(start, self.len)),
                file: self.file.clone().map(PathBuf::from),
                message: "unterminated interpolated string literal".to_string(),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }

        Token::new(
            TokenKind::InterpolatedString,
            start,
            self.pos.min(self.len),
            std::str::from_utf8(&self.source[start..self.pos.min(self.len)])
                .unwrap_or("")
                .to_string(),
        )
    }

    fn scan_multi_or_single(
        &mut self,
        tail: &[u8],
        single: TokenKind,
        multi: TokenKind,
    ) -> Token {
        let start = self.pos;
        let (kind, end) = if self.matches(tail) {
            (multi, start + 2)
        } else {
            (single, start + 1)
        };
        self.pos = end;
        Token::new(
            kind,
            start,
            self.pos,
            std::str::from_utf8(&self.source[start..self.pos])
                .unwrap_or("")
                .to_string(),
        )
    }

    fn matches(&self, prefix: &[u8]) -> bool {
        if self.pos + prefix.len() > self.len {
            return false;
        }
        &self.source[self.pos..self.pos + prefix.len()] == prefix
    }
}

fn is_identifier_start(ch: u8) -> bool {
    ch.is_ascii_alphabetic() || ch == b'_'
}

fn is_identifier_continue(ch: u8) -> bool {
    is_identifier_start(ch) || ch.is_ascii_digit()
}

fn token_kind_for_identifier(name: &str) -> TokenKind {
    match name {
        "return" => TokenKind::Return,
        "break" => TokenKind::Break,
        "continue" => TokenKind::Continue,
        "public" => TokenKind::Public,
        "var" => TokenKind::Var,
        "using" => TokenKind::Using,
        "shared" => TokenKind::Shared,
        "virtual" => TokenKind::Virtual,
        "override" => TokenKind::Override,
        "static" => TokenKind::Static,
        "operator" => TokenKind::Operator,
        "class" => TokenKind::Class,
        "struct" => TokenKind::Struct,
        "interface" => TokenKind::Interface,
        "enum" => TokenKind::Enum,
        "async" => TokenKind::Async,
        "await" => TokenKind::Await,
        "match" => TokenKind::Match,
        "def" => TokenKind::Def,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "while" => TokenKind::While,
        "for" => TokenKind::For,
        "try" => TokenKind::Try,
        "catch" => TokenKind::Catch,
        "finally" => TokenKind::Finally,
        "throw" => TokenKind::Throw,
        "from" => TokenKind::From,
        "as" => TokenKind::As,
        "import" => TokenKind::Import,
        "Bool" => TokenKind::TypeBool,
        "Byte" => TokenKind::TypeByte,
        "Short" => TokenKind::TypeShort,
        "UShort" => TokenKind::TypeUShort,
        "Int" => TokenKind::TypeInt,
        "UInt" => TokenKind::TypeUInt,
        "Long" => TokenKind::TypeLong,
        "ULong" => TokenKind::TypeULong,
        "Int8" => TokenKind::TypeInt8,
        "Int16" => TokenKind::TypeInt16,
        "Int32" => TokenKind::TypeInt32,
        "Int64" => TokenKind::TypeInt64,
        "UInt8" => TokenKind::TypeUInt8,
        "UInt16" => TokenKind::TypeUInt16,
        "UInt32" => TokenKind::TypeUInt32,
        "UInt64" => TokenKind::TypeUInt64,
        "Float" => TokenKind::TypeFloat,
        "Float32" => TokenKind::TypeFloat32,
        "Float64" => TokenKind::TypeFloat64,
        "Double" => TokenKind::TypeDouble,
        "Char" => TokenKind::TypeChar,
        "String" => TokenKind::TypeString,
        "Unit" => TokenKind::TypeUnit,
        "Var" => TokenKind::TypeVar,
        "true" | "false" => TokenKind::BooleanLiteral,
        "null" => TokenKind::NullLiteral,
        _ => TokenKind::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kind_and_span(tokens: &[Token]) -> Vec<String> {
        tokens
            .iter()
            .map(|token| {
                format!(
                    "{kind}:{lo}..{hi}",
                    kind = format!("{:?}", token.kind),
                    lo = token.span.lo,
                    hi = token.span.hi
                )
            })
            .collect()
    }

    fn kinds(tokens: &[Token]) -> Vec<String> {
        tokens
            .iter()
            .map(|token| format!("{:?}", token.kind))
            .collect()
    }

    #[test]
    fn lex_conformance_tokens_and_spans() {
        let source = "a=1//x\ntrue false null";
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("input.nex".to_string()), &mut sink);
        assert!(sink.is_empty());

        let expected = vec![
            "Identifier:0..1",
            "Eq:1..2",
            "IntLiteral:2..3",
            "Newline:6..7",
            "BooleanLiteral:7..11",
            "BooleanLiteral:12..17",
            "NullLiteral:18..22",
            "Eof:22..22",
        ];

        assert_eq!(kind_and_span(&tokens), expected);
    }

    #[test]
    fn lex_preserves_newline_from_block_comment() {
        let source = "a=1/*x\n*/b=2\n";
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("input.nex".to_string()), &mut sink);
        assert!(sink.is_empty());

        let expected = vec![
            "Identifier:0..1",
            "Eq:1..2",
            "IntLiteral:2..3",
            "Newline:6..7",
            "Identifier:9..10",
            "Eq:10..11",
            "IntLiteral:11..12",
            "Newline:12..13",
            "Eof:13..13",
        ];

        assert_eq!(kind_and_span(&tokens), expected);
    }

    #[test]
    fn lex_asi_snapshot_no_insert_after_infix_operator() {
        let source = "a +\n b";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected = vec![
            "Identifier".to_string(),
            "Plus".to_string(),
            "Identifier".to_string(),
            "Eof".to_string(),
        ];

        assert_eq!(kinds(&tokens), expected);
    }

    #[test]
    fn lex_asi_snapshot_inserts_after_return_and_literals() {
        let source = "x = 1 +\ny = 2\nreturn\nz";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected: Vec<String> = vec![
            "Identifier",
            "Eq",
            "IntLiteral",
            "Plus",
            "Identifier",
            "Eq",
            "IntLiteral",
            "SyntheticSemicolon",
            "Return",
            "SyntheticSemicolon",
            "Identifier",
            "Eof",
        ]
        .into_iter()
        .map(str::to_string)
        .collect();

        assert_eq!(kinds(&tokens), expected);
    }

    #[test]
    fn lex_numeric_suffix_and_char_literals() {
        let source = "x=1i 2u 3.14f 4d '\\n' 'a'";
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("input.nex".to_string()), &mut sink);
        assert!(sink.is_empty());

        let expected = vec![
            "Identifier:0..1",
            "Eq:1..2",
            "IntLiteral:2..4",
            "IntLiteral:5..7",
            "FloatLiteral:8..13",
            "FloatLiteral:14..16",
            "CharLiteral:17..21",
            "CharLiteral:22..25",
            "Eof:25..25",
        ];

        assert_eq!(kind_and_span(&tokens), expected);
    }

    #[test]
    fn lex_normalizes_asi_and_preserves_crlf_newlines() {
        let source = "a=1\r\nb=2\r\nc=(d+e)\r\nf=(g\r\n)\r\n";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected = vec![
            "Identifier:0..1",
            "Eq:1..2",
            "IntLiteral:2..3",
            "SyntheticSemicolon:3..5",
            "Identifier:5..6",
            "Eq:6..7",
            "IntLiteral:7..8",
            "SyntheticSemicolon:8..10",
            "Identifier:10..11",
            "Eq:11..12",
            "LParen:12..13",
            "Identifier:13..14",
            "Plus:14..15",
            "Identifier:15..16",
            "RParen:16..17",
            "SyntheticSemicolon:17..19",
            "Identifier:19..20",
            "Eq:20..21",
            "LParen:21..22",
            "Identifier:22..23",
            "RParen:25..26",
            "SyntheticSemicolon:26..28",
            "Eof:28..28",
        ];

        assert_eq!(kind_and_span(&tokens), expected);
    }

    #[test]
    fn unknown_token_emits_diagnostic() {
        let source = "a=1@\n";
        let mut sink = DiagnosticSink::new();
        let _ = lex(source, Some("input.nex".to_string()), &mut sink);
        assert_eq!(sink.diagnostics().len(), 1);
        assert_eq!(sink.diagnostics()[0].id, "lex_invalid_char");
    }

    #[test]
    fn lex_recognizes_struct_static_operator_keywords() {
        let source = "struct static operator";
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("input.nex".to_string()), &mut sink);
        assert!(sink.is_empty());

        let expected = vec![
            "Struct:0..6",
            "Static:7..13",
            "Operator:14..22",
            "Eof:22..22",
        ];
        assert_eq!(kind_and_span(&tokens), expected);
    }

    #[test]
    fn asi_suppresses_semicolon_before_continuation_operator() {
        // Multi-line && expression: no semicolon should be inserted before &&
        let source = "a > 5\n    && b < 10";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected: Vec<String> = vec![
            "Identifier", // a
            "Gt",         // >
            "IntLiteral", // 5
            "AndAnd",     // && (no semicolon before this!)
            "Identifier", // b
            "Lt",         // <
            "IntLiteral", // 10
            "Eof",
        ]
        .into_iter()
        .map(str::to_string)
        .collect();

        assert_eq!(kinds(&tokens), expected);
    }

    #[test]
    fn asi_suppresses_semicolon_before_oror() {
        let source = "x == 1\n    || y == 2";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected: Vec<String> = vec![
            "Identifier", "EqEq", "IntLiteral",
            "OrOr",
            "Identifier", "EqEq", "IntLiteral",
            "Eof",
        ]
        .into_iter()
        .map(str::to_string)
        .collect();

        assert_eq!(kinds(&tokens), expected);
    }

    #[test]
    fn asi_still_inserts_semicolon_between_statements() {
        // Two separate statements — semicolon should still be inserted
        let source = "x = 1\ny = 2";
        let mut sink = DiagnosticSink::new();
        let tokens = asi_normalize(&lex(source, Some("input.nex".to_string()), &mut sink));

        let expected: Vec<String> = vec![
            "Identifier", "Eq", "IntLiteral",
            "SyntheticSemicolon",
            "Identifier", "Eq", "IntLiteral",
            "Eof",
        ]
        .into_iter()
        .map(str::to_string)
        .collect();

        assert_eq!(kinds(&tokens), expected);
    }
}
