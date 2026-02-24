use std::path::PathBuf;

use nexc_ast::*;
use nexc_diag::{Diagnostic, Severity, Span};
use nexc_lex::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    file: String,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    pub fn new(tokens: &[Token], file: String) -> Self {
        Self {
            tokens: tokens.to_vec(),
            pos: 0,
            file,
            diagnostics: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> SourceFile {
        let source_end = self.tokens.last().map(|token| token.span.hi).unwrap_or(0);
        let mut file = SourceFile {
            path: self.file.clone(),
            span: Span::new(0, source_end),
            items: Vec::new(),
        };

        while !self.is_eof() {
            self.skip_terminators();
            if self.is_eof() {
                break;
            }

            match self.peek_kind() {
                Some(TokenKind::Import) => {
                    file.items.push(Item::Import(self.parse_import(false, false)));
                }
                Some(TokenKind::From) => {
                    file.items.push(Item::Import(self.parse_import(true, false)));
                }
                Some(TokenKind::Class) => {
                    file.items.push(Item::Class(self.parse_class(false, false)));
                }
                Some(TokenKind::Struct) => {
                    file.items.push(Item::Struct(self.parse_struct(false)));
                }
                Some(TokenKind::Interface) => {
                    file.items.push(Item::Interface(self.parse_interface(false)));
                }
                Some(TokenKind::Def)
                | Some(TokenKind::Virtual)
                | Some(TokenKind::Override)
                | Some(TokenKind::Static) => {
                    file.items.push(Item::Function(self.parse_function(false, false)));
                }
                Some(TokenKind::Var) => {
                    file.items.push(Item::Variable(self.parse_var_decl(false)));
                }
                Some(TokenKind::Using) => {
                    file.items.push(Item::Using(self.parse_using()));
                }
                Some(TokenKind::Public) => {
                    self.advance();
                    let partial = self.is_contextual_keyword("partial");
                    if partial {
                        self.advance(); // consume `partial`
                    }
                    match self.peek_kind() {
                        Some(TokenKind::Class) => {
                            file.items.push(Item::Class(self.parse_class(true, partial)));
                        }
                        Some(TokenKind::Struct) => {
                            file.items.push(Item::Struct(self.parse_struct(true)));
                        }
                        Some(TokenKind::Interface) => {
                            file.items.push(Item::Interface(self.parse_interface(true)));
                        }
                        Some(TokenKind::Def)
                        | Some(TokenKind::Virtual)
                        | Some(TokenKind::Override)
                        | Some(TokenKind::Static) => {
                            file.items.push(Item::Function(self.parse_function(true, false)));
                        }
                        Some(TokenKind::Var) => {
                            file.items.push(Item::Variable(self.parse_var_decl(true)));
                        }
                        Some(TokenKind::Import) => {
                            file.items.push(Item::Import(self.parse_import(false, true)));
                        }
                        Some(TokenKind::From) => {
                            file.items.push(Item::Import(self.parse_import(true, true)));
                        }
                        Some(_) | None => {
                            self.push_error("expected declaration after `public`");
                            self.recover_to_item_boundary();
                        }
                    }
                }
                Some(TokenKind::Identifier) if self.is_contextual_keyword("partial") && self.peek_nth_kind(1) == Some(TokenKind::Class) => {
                    self.advance(); // consume `partial`
                    file.items.push(Item::Class(self.parse_class(false, true)));
                }
                Some(_) => {
                    let statement = self.parse_statement();
                    file.items.push(Item::Statement(statement));
                }
                None => break,
            }
        }

        file
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    fn parse_import(&mut self, from_import: bool, is_public: bool) -> ImportDecl {
        let span = self.current_span();
        if from_import {
            self.advance();

            let path = self.parse_path("from import source");
            if !self.consume_if(&TokenKind::Import) {
                self.push_error("expected `import` after source path");
            }

            let mut names = Vec::new();
            if self.is_identifier_start() {
                if let Some(name) = self.consume_identifier() {
                    names.push(name);
                    while self.consume_if(&TokenKind::Comma) {
                        if let Some(name) = self.consume_identifier() {
                            names.push(name);
                        } else {
                            self.push_error("expected imported symbol");
                            self.recover_to_item_boundary();
                            break;
                        }
                    }
                }
            } else {
                self.push_error("expected imported symbol");
            }

            self.consume_stmt_terminator();
            return ImportDecl {
                path,
                alias: None,
                kind: ImportKind::From(names),
                span,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Internal
                },
                synthetic: false,
            };
        }

        self.advance();
        let path = self.parse_path("import module path");
        let alias = if self.consume_if(&TokenKind::As) {
            self.consume_identifier()
        } else {
            None
        };
        self.consume_stmt_terminator();

        ImportDecl {
            path,
            alias,
            kind: ImportKind::Module,
            span,
            visibility: if is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            },
            synthetic: false,
        }
    }

    fn parse_class(&mut self, is_public: bool, is_partial: bool) -> ClassDecl {
        let span = self.current_span();
        self.advance(); // consume `class` keyword
        let name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected class name");
            "Class".to_string()
        });
        let type_params = self.parse_type_params();
        let base_specs = self.parse_base_specs();

        if !self.consume_if(&TokenKind::LBrace) {
            self.push_error("expected '{' after class name");
            self.recover_to_item_boundary();
            return ClassDecl {
                name,
                is_partial,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Internal
                },
                type_params,
                base_specs,
                fields: Vec::new(),
                methods: Vec::new(),
                span,
            };
        }

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.is_eof() {
            self.skip_terminators();
            if self.consume_if(&TokenKind::RBrace) {
                break;
            }

            let mut visibility = Visibility::Internal;
            if self.consume_if(&TokenKind::Public) {
                visibility = Visibility::Public;
            }

            if matches!(
                self.peek_kind(),
                Some(TokenKind::Def)
                    | Some(TokenKind::Virtual)
                    | Some(TokenKind::Override)
                    | Some(TokenKind::Static)
            ) {
                methods.push(self.parse_function(matches!(visibility, Visibility::Public), false));
                continue;
            }

            if self.consume_if(&TokenKind::Var) {
                fields.push(self.parse_class_field(visibility));
                continue;
            }

            if self.is_identifier_start() {
                fields.push(self.parse_class_field(visibility));
                continue;
            }

            self.push_error("unexpected token in class body");
            self.recover_to_block_boundary();
        }

        ClassDecl {
            name,
            is_partial,
            visibility: if is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            },
            type_params,
            base_specs,
            fields,
            methods,
            span,
        }
    }

    fn parse_interface(&mut self, is_public: bool) -> InterfaceDecl {
        let span = self.current_span();
        self.advance();
        let name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected interface name");
            "Interface".to_string()
        });
        let type_params = self.parse_type_params();

        if !self.consume_if(&TokenKind::LBrace) {
            self.push_error("expected '{' after interface name");
            self.recover_to_item_boundary();
            return InterfaceDecl {
                name,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Internal
                },
                type_params,
                methods: Vec::new(),
                span,
            };
        }

        let mut methods = Vec::new();
        while !self.is_eof() {
            self.skip_terminators();
            if self.consume_if(&TokenKind::RBrace) {
                break;
            }

            let mut visibility = Visibility::Internal;
            if self.consume_if(&TokenKind::Public) {
                visibility = Visibility::Public;
            }

            if matches!(
                self.peek_kind(),
                Some(TokenKind::Def)
                    | Some(TokenKind::Virtual)
                    | Some(TokenKind::Override)
                    | Some(TokenKind::Static)
            ) {
                methods.push(self.parse_function(matches!(visibility, Visibility::Public), true));
                self.consume_stmt_terminator();
                continue;
            }

            self.push_error("expected interface method");
            self.recover_to_block_boundary();
        }

        InterfaceDecl {
            name,
            visibility: if is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            },
            type_params,
            methods,
            span,
        }
    }

    fn parse_struct(&mut self, is_public: bool) -> StructDecl {
        let span = self.current_span();
        self.advance();
        let name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected struct name");
            "Struct".to_string()
        });
        let type_params = self.parse_type_params();
        let interfaces = self.parse_struct_interfaces();

        if !self.consume_if(&TokenKind::LBrace) {
            self.push_error("expected '{' after struct name");
            self.recover_to_item_boundary();
            return StructDecl {
                name,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Internal
                },
                type_params,
                interfaces,
                fields: Vec::new(),
                methods: Vec::new(),
                span,
            };
        }

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.is_eof() {
            self.skip_terminators();
            if self.consume_if(&TokenKind::RBrace) {
                break;
            }

            let mut visibility = Visibility::Internal;
            if self.consume_if(&TokenKind::Public) {
                visibility = Visibility::Public;
            }

            if matches!(
                self.peek_kind(),
                Some(TokenKind::Def)
                    | Some(TokenKind::Virtual)
                    | Some(TokenKind::Override)
                    | Some(TokenKind::Static)
            ) {
                methods.push(self.parse_function(matches!(visibility, Visibility::Public), false));
                continue;
            }

            if self.consume_if(&TokenKind::Var) {
                fields.push(self.parse_class_field(visibility));
                continue;
            }

            if self.is_identifier_start() {
                fields.push(self.parse_class_field(visibility));
                continue;
            }

            self.push_error("unexpected token in struct body");
            self.recover_to_block_boundary();
        }

        StructDecl {
            name,
            visibility: if is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            },
            type_params,
            interfaces,
            fields,
            methods,
            span,
        }
    }

    fn parse_function(&mut self, is_public: bool, signature_only: bool) -> FunctionDecl {
        let span = self.current_span();
        let mut is_virtual = false;
        let mut is_override = false;
        let mut is_static = false;

        loop {
            match self.peek_kind() {
                Some(TokenKind::Virtual) => {
                    self.advance();
                    is_virtual = true;
                }
                Some(TokenKind::Override) => {
                    self.advance();
                    is_override = true;
                }
                Some(TokenKind::Static) => {
                    self.advance();
                    is_static = true;
                }
                _ => break,
            }
        }
        if !self.consume_if(&TokenKind::Def) {
            self.push_error("expected `def`");
        }

        let (name, operator) = if self.consume_if(&TokenKind::Operator) {
            let op = self.parse_operator_symbol();
            (format!("operator{op}"), Some(op))
        } else {
            (
                self.consume_identifier().unwrap_or_else(|| {
                    self.push_error("expected function name");
                    "function".to_string()
                }),
                None,
            )
        };
        let type_params = self.parse_type_params();

        let params = if self.consume_if(&TokenKind::LParen) {
            let params = self.parse_parameters();
            let _ = self.consume_if(&TokenKind::RParen);
            params
        } else {
            self.push_error("expected '(' after function name");
            Vec::new()
        };

        let return_type = if self.consume_if(&TokenKind::Arrow) {
            self.parse_type_expr()
        } else {
            None
        };

        let body = if self.consume_if(&TokenKind::LBrace) {
            Some(Expr::Block(self.parse_block()))
        } else if signature_only {
            self.consume_stmt_terminator();
            None
        } else {
            self.push_error("expected function body");
            None
        };

        FunctionDecl {
            name,
            type_params,
            params,
            return_type,
            is_public,
            is_virtual,
            is_override,
            is_static,
            operator,
            body,
            span,
            attributes: Vec::new(),
        }
    }

    fn parse_var_decl(&mut self, is_public: bool) -> VarDecl {
        let span = self.current_span();
        self.advance();
        let name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected variable name");
            "value".to_string()
        });
        let explicit_type = if self.consume_if(&TokenKind::Colon) {
            self.parse_type_expr()
        } else {
            None
        };
        let inferred_type = explicit_type.clone();
        let initializer = if self.consume_if(&TokenKind::Eq) {
            Some(self.parse_expression())
        } else {
            None
        };
        self.consume_stmt_terminator();

        VarDecl {
            name,
            inferred_type,
            explicit_type,
            initializer,
            is_dynamic: false,
            visibility: if is_public {
                Visibility::Public
            } else {
                Visibility::Internal
            },
            attributes: Vec::new(),
            span,
        }
    }

    fn parse_class_field(&mut self, visibility: Visibility) -> FieldDecl {
        let span = self.current_span();
        let name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected field name");
            "field".to_string()
        });
        let ty = if self.consume_if(&TokenKind::Colon) {
            self.parse_type_expr()
        } else {
            None
        };
        let initializer = if self.consume_if(&TokenKind::Eq) {
            Some(self.parse_expression())
        } else {
            None
        };
        self.consume_stmt_terminator();
        FieldDecl {
            name,
            ty,
            initializer,
            visibility,
            span,
        }
    }

    fn parse_using(&mut self) -> UsingDecl {
        let span = self.current_span();
        self.advance();
        let _ = self.consume_if(&TokenKind::LParen);
        let variable_name = self.consume_identifier().unwrap_or_else(|| {
            self.push_error("expected resource name in using");
            "resource".to_string()
        });

        if !self.consume_if(&TokenKind::Eq) {
            self.push_error("expected '=' in using");
        }

        let expr = if self.is_statement_boundary() {
            self.push_error("expected expression in using");
            Expr::Unsupported {
                raw: "missing using expression".to_string(),
                span,
            }
        } else {
            self.parse_expression()
        };

        let _ = self.consume_if(&TokenKind::RParen);
        let body = if self.consume_if(&TokenKind::LBrace) {
            self.parse_block()
        } else {
            self.push_error("expected block after using");
            self.recover_to_statement_boundary();
            Block {
                statements: Vec::new(),
                span,
            }
        };

        self.consume_stmt_terminator();
        UsingDecl {
            variable_name,
            expr,
            body,
            span,
        }
    }

    fn parse_statement(&mut self) -> Stmt {
        match self.peek_kind() {
            Some(TokenKind::Return) => self.parse_return(),
            Some(TokenKind::Throw) => self.parse_throw(),
            Some(TokenKind::If) => Stmt::If(self.parse_if()),
            Some(TokenKind::While) => Stmt::While(self.parse_while()),
            Some(TokenKind::For) => Stmt::For(self.parse_for()),
            Some(TokenKind::Using) => Stmt::Using(self.parse_using()),
            Some(TokenKind::Try) => Stmt::Try(self.parse_try()),
            Some(TokenKind::Var) => Stmt::VarDecl(self.parse_var_decl(false)),
            Some(TokenKind::Continue) => {
                let span = self.current_span();
                self.advance();
                self.consume_stmt_terminator();
                Stmt::Continue(span)
            }
            Some(TokenKind::Break) => {
                let span = self.current_span();
                self.advance();
                self.consume_stmt_terminator();
                Stmt::Break(span)
            }
            Some(TokenKind::LBrace) => {
                self.advance();
                Stmt::Block(self.parse_block())
            }
            Some(_) => {
                let expr = self.parse_expression();
                self.consume_stmt_terminator();
                Stmt::Expr(expr)
            }
            None => Stmt::Expr(Expr::Unsupported {
                raw: "eof".to_string(),
                span: self.current_span(),
            }),
        }
    }

    fn parse_return(&mut self) -> Stmt {
        let span = self.current_span();
        self.advance();
        let value = if self.is_statement_boundary() {
            None
        } else {
            Some(self.parse_expression())
        };
        self.consume_stmt_terminator();
        Stmt::Return(value, span)
    }

    fn parse_throw(&mut self) -> Stmt {
        let span = self.current_span();
        self.advance();
        let value = if self.is_statement_boundary() {
            self.push_error("expected expression after throw");
            Expr::Unsupported {
                raw: "missing throw expression".to_string(),
                span,
            }
        } else {
            self.parse_expression()
        };
        self.consume_stmt_terminator();
        Stmt::Throw(value, span)
    }

    fn parse_if(&mut self) -> IfStmt {
        let span = self.current_span();
        self.advance();
        if !self.consume_if(&TokenKind::LParen) {
            self.push_error("expected '(' after if");
        }

        let condition = if self.consume_if(&TokenKind::RParen) {
            self.push_error("empty if condition");
            Expr::Unsupported {
                raw: "empty condition".to_string(),
                span,
            }
        } else {
            let condition = self.parse_expression();
            if !self.consume_if(&TokenKind::RParen) {
                self.push_error("expected ')' after if condition");
            }
            condition
        };

        let then_branch = if self.consume_if(&TokenKind::LBrace) {
            Stmt::Block(self.parse_block())
        } else {
            self.push_error("expected '{' for if body");
            self.recover_to_statement_boundary();
            Stmt::Block(Block {
                statements: Vec::new(),
                span,
            })
        };

        let else_branch = if self.consume_if(&TokenKind::Else) {
            let branch = if self.peek_kind() == Some(TokenKind::If) {
                Stmt::If(self.parse_if())
            } else if self.consume_if(&TokenKind::LBrace) {
                Stmt::Block(self.parse_block())
            } else {
                self.push_error("expected '{' or 'if' after else");
                self.recover_to_statement_boundary();
                Stmt::Block(Block {
                    statements: Vec::new(),
                    span,
                })
            };
            Some(Box::new(branch))
        } else {
            None
        };

        IfStmt {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
            span,
        }
    }

    fn parse_while(&mut self) -> WhileStmt {
        let span = self.current_span();
        self.advance();

        if !self.consume_if(&TokenKind::LParen) {
            self.push_error("expected '(' after while");
        }

        let condition = if self.consume_if(&TokenKind::RParen) {
            self.push_error("empty while condition");
            Expr::Unsupported {
                raw: "empty while condition".to_string(),
                span,
            }
        } else {
            let cond = self.parse_expression();
            if !self.consume_if(&TokenKind::RParen) {
                self.push_error("expected ')' after while condition");
            }
            cond
        };

        let body = if self.consume_if(&TokenKind::LBrace) {
            Stmt::Block(self.parse_block())
        } else {
            self.push_error("expected '{' for while body");
            self.recover_to_statement_boundary();
            Stmt::Block(Block {
                statements: Vec::new(),
                span,
            })
        };

        WhileStmt {
            condition,
            body: Box::new(body),
            span,
        }
    }

    fn parse_for(&mut self) -> ForStmt {
        let span = self.current_span();
        self.advance();
        if !self.consume_if(&TokenKind::LParen) {
            self.push_error("expected '(' after for");
        }

        let init = if matches!(
            self.peek_kind(),
            Some(TokenKind::Semicolon | TokenKind::SyntheticSemicolon)
        ) {
            None
        } else if matches!(self.peek_kind(), Some(TokenKind::RParen)) {
            None
        } else {
            Some(self.parse_expression())
        };
        if !(self.consume_if(&TokenKind::Semicolon)
            || self.consume_if(&TokenKind::SyntheticSemicolon))
        {
            self.push_error("expected ';' after for initializer");
        }

        let condition = if matches!(
            self.peek_kind(),
            Some(TokenKind::Semicolon | TokenKind::SyntheticSemicolon)
        ) {
            None
        } else if matches!(self.peek_kind(), Some(TokenKind::RParen)) {
            None
        } else {
            Some(self.parse_expression())
        };
        if !(self.consume_if(&TokenKind::Semicolon)
            || self.consume_if(&TokenKind::SyntheticSemicolon))
        {
            self.push_error("expected ';' after for condition");
        }

        let step = if matches!(self.peek_kind(), Some(TokenKind::RParen)) {
            None
        } else {
            Some(self.parse_expression())
        };
        if !self.consume_if(&TokenKind::RParen) {
            self.push_error("expected ')' after for clauses");
        }

        let body = if self.consume_if(&TokenKind::LBrace) {
            Stmt::Block(self.parse_block())
        } else {
            self.push_error("expected '{' for for body");
            self.recover_to_statement_boundary();
            Stmt::Block(Block {
                statements: Vec::new(),
                span,
            })
        };

        ForStmt {
            init,
            condition,
            step,
            body: Box::new(body),
            span,
        }
    }

    fn parse_try(&mut self) -> TryStmt {
        let start = self.current_span();
        self.advance();
        let body = if self.consume_if(&TokenKind::LBrace) {
            self.parse_block()
        } else {
            self.push_error("expected block after try");
            self.recover_to_block_boundary();
            Block {
                statements: Vec::new(),
                span: start,
            }
        };

        let mut catches = Vec::new();
        while self.consume_if(&TokenKind::Catch) {
            let catch_span = self.current_span();
            let mut variable_name = None;
            let mut variable_type = None;
            if self.consume_if(&TokenKind::LParen) {
                variable_name = self.consume_identifier();
                if self.consume_if(&TokenKind::Colon) {
                    variable_type = self.parse_type_expr();
                }
                let _ = self.consume_if(&TokenKind::RParen);
            } else {
                self.push_error("expected '(' in catch");
            }

            let body = if self.consume_if(&TokenKind::LBrace) {
                self.parse_block()
            } else {
                self.push_error("expected block in catch");
                self.recover_to_statement_boundary();
                Block {
                    statements: Vec::new(),
                    span: catch_span,
                }
            };

            catches.push(CatchClause {
                variable_name,
                variable_type,
                body,
                span: catch_span,
            });
        }

        let finally = if self.consume_if(&TokenKind::Finally) {
            if self.consume_if(&TokenKind::LBrace) {
                Some(self.parse_block())
            } else {
                self.push_error("expected block after finally");
                None
            }
        } else {
            None
        };

        TryStmt {
            body,
            catches,
            finally,
            span: start,
        }
    }

    fn parse_block(&mut self) -> Block {
        let span = self.current_span();
        let mut statements = Vec::new();
        let mut terminated = false;

        while !self.is_eof() {
            self.skip_terminators();
            if self.consume_if(&TokenKind::RBrace) {
                terminated = true;
                break;
            }
            statements.push(self.parse_statement());
            self.skip_terminators();
        }

        if !terminated {
            self.push_error("unterminated block");
        }

        Block { statements, span }
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_expression_bp(1)
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Expr {
        let mut lhs = self.parse_prefix_expr();

        loop {
            let infix = match self.peek_kind() {
                Some(TokenKind::Eq) => Some((1, 1, PrattInfix::Assign(AssignOp::Assign))),
                Some(TokenKind::PlusEq) => Some((1, 1, PrattInfix::Assign(AssignOp::AddAssign))),
                Some(TokenKind::MinusEq) => Some((1, 1, PrattInfix::Assign(AssignOp::SubAssign))),
                Some(TokenKind::StarEq) => Some((1, 1, PrattInfix::Assign(AssignOp::MulAssign))),
                Some(TokenKind::SlashEq) => Some((1, 1, PrattInfix::Assign(AssignOp::DivAssign))),
                Some(TokenKind::OrOr) => Some((2, 3, PrattInfix::Binary(BinaryOp::Or))),
                Some(TokenKind::AndAnd) => Some((4, 5, PrattInfix::Binary(BinaryOp::And))),
                Some(TokenKind::EqEq) => Some((6, 7, PrattInfix::Binary(BinaryOp::EqEq))),
                Some(TokenKind::NotEq) => Some((6, 7, PrattInfix::Binary(BinaryOp::NotEq))),
                Some(TokenKind::Lt) => Some((8, 9, PrattInfix::Binary(BinaryOp::Lt))),
                Some(TokenKind::LtEq) => Some((8, 9, PrattInfix::Binary(BinaryOp::LtEq))),
                Some(TokenKind::Gt) => Some((8, 9, PrattInfix::Binary(BinaryOp::Gt))),
                Some(TokenKind::GtEq) => Some((8, 9, PrattInfix::Binary(BinaryOp::GtEq))),
                Some(TokenKind::Plus) => Some((10, 11, PrattInfix::Binary(BinaryOp::Add))),
                Some(TokenKind::Minus) => Some((10, 11, PrattInfix::Binary(BinaryOp::Sub))),
                Some(TokenKind::Star) => Some((12, 13, PrattInfix::Binary(BinaryOp::Mul))),
                Some(TokenKind::Slash) => Some((12, 13, PrattInfix::Binary(BinaryOp::Div))),
                Some(TokenKind::Percent) => Some((12, 13, PrattInfix::Binary(BinaryOp::Mod))),
                Some(TokenKind::LParen) => Some((16, 17, PrattInfix::Call)),
                Some(TokenKind::Dot) => Some((16, 17, PrattInfix::Member)),
                Some(TokenKind::DoubleColon) => Some((16, 17, PrattInfix::QualifiedMember)),
                _ => None,
            };

            let Some((l_bp, r_bp, infix)) = infix else {
                break;
            };
            if l_bp < min_bp {
                break;
            }

            match infix {
                PrattInfix::Assign(op) => {
                    let _ = self.advance();
                    let rhs = self.parse_expression_bp(r_bp);
                    let span = span_between(&lhs, &rhs);
                    lhs = Expr::Assign {
                        target: Box::new(lhs),
                        value: Box::new(rhs),
                        op,
                        span,
                    };
                }
                PrattInfix::Binary(op) => {
                    self.advance();
                    let rhs = self.parse_expression_bp(r_bp);
                    let span = span_between(&lhs, &rhs);
                    lhs = Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span,
                    };
                }
                PrattInfix::Call => {
                    let callee = Box::new(lhs);
                    let callee_span = expr_span(&callee);
                    self.advance();
                    let args = self.parse_expr_list(TokenKind::RParen);
                    // parse_expr_list already consumed the closing ')'.
                    // Use the position just before current to get the span end.
                    let end = self.tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|token| token.span.hi)
                        .unwrap_or(callee_span.hi);
                    let span = Span::new(callee_span.lo, end);
                    lhs = Expr::Call {
                        callee,
                        args,
                        span,
                    };
                }
                PrattInfix::Member => {
                    self.advance();
                    let receiver = expr_span(&lhs);
                    let name = self.consume_identifier().unwrap_or_else(|| {
                        self.push_error("expected member name");
                        "member".to_string()
                    });
                    let member_span = self
                        .tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|token| token.span)
                        .unwrap_or(receiver);
                    lhs = Expr::MemberAccess {
                        receiver: Box::new(lhs),
                        name,
                        span: Span::new(receiver.lo, member_span.hi),
                        qualifier: None,
                    };
                }
                PrattInfix::QualifiedMember => {
                    self.advance();
                    let receiver = expr_span(&lhs);
                    let qualifier = expr_identifier_name(&lhs);
                    let name = self.consume_identifier().unwrap_or_else(|| {
                        self.push_error("expected qualified member name");
                        "member".to_string()
                    });
                    let member_span = self
                        .tokens
                        .get(self.pos.saturating_sub(1))
                        .map(|token| token.span)
                        .unwrap_or(receiver);
                    lhs = Expr::MemberAccess {
                        receiver: Box::new(lhs),
                        name,
                        span: Span::new(receiver.lo, member_span.hi),
                        qualifier,
                    };
                }
            }
        }

        lhs
    }

    fn parse_prefix_expr(&mut self) -> Expr {
        match self.peek_kind() {
            Some(TokenKind::Bang) => {
                let op = self.advance();
                let value = self.parse_expression_bp(18);
                let value_span = expr_span(&value);
                Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(value),
                    span: Span::new(op.span.lo, value_span.hi),
                }
            }
            Some(TokenKind::Minus) => {
                let op = self.advance();
                let value = self.parse_expression_bp(18);
                let value_span = expr_span(&value);
                Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(value),
                    span: Span::new(op.span.lo, value_span.hi),
                }
            }
            Some(TokenKind::Plus) => {
                self.advance();
                self.parse_expression_bp(18)
            }
            Some(_) => self.parse_primary(),
            None => Expr::Unsupported {
                raw: "eof".to_string(),
                span: self.current_span(),
            },
        }
    }

    fn parse_expr_list(&mut self, terminator: TokenKind) -> Vec<Expr> {
        let mut out = Vec::new();
        if self.consume_if(&terminator) {
            return out;
        }

        while !self.is_eof() {
            out.push(self.parse_expression());
            if self.consume_if(&TokenKind::Comma) {
                continue;
            }
            if self.consume_if(&terminator) {
                break;
            }
            if !self.is_statement_boundary() {
                self.push_error("expected ',' or ')' in expression list");
                self.recover_to_statement_boundary();
                break;
            }
            break;
        }

        out
    }

    fn parse_primary(&mut self) -> Expr {
        match self.peek_kind() {
            Some(TokenKind::Identifier) => {
                let token = self.advance();
                Expr::Identifier {
                    name: token.lexeme,
                    span: token.span,
                }
            }
            Some(TokenKind::IntLiteral) => {
                let token = self.advance();
                let value = self.parse_int_literal(&token.lexeme);
                Expr::Literal {
                    value: Literal::Int(value),
                    span: token.span,
                }
            }
            Some(TokenKind::FloatLiteral) => {
                let token = self.advance();
                let value = self.parse_float_literal(&token.lexeme);
                Expr::Literal {
                    value: Literal::Float(value),
                    span: token.span,
                }
            }
            Some(TokenKind::CharLiteral) => {
                let token = self.advance();
                let value = match self.parse_char_literal(&token.lexeme) {
                    Some(value) => value,
                    None => {
                        self.push_error("invalid char literal");
                        '\0'
                    }
                };
                Expr::Literal {
                    value: Literal::Char(value),
                    span: token.span,
                }
            }
            Some(TokenKind::StringLiteral) => {
                let token = self.advance();
                let raw = token.lexeme;
                let stripped = if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
                    raw[1..raw.len() - 1].to_string()
                } else {
                    raw
                };
                Expr::Literal {
                    value: Literal::String(stripped),
                    span: token.span,
                }
            }
            Some(TokenKind::BooleanLiteral) => {
                let token = self.advance();
                Expr::Literal {
                    value: Literal::Bool(token.lexeme == "true"),
                    span: token.span,
                }
            }
            Some(TokenKind::NullLiteral) => {
                let token = self.advance();
                Expr::Literal {
                    value: Literal::Null,
                    span: token.span,
                }
            }
            Some(TokenKind::LParen) => {
                self.advance();
                let expr = self.parse_expression_bp(1);
                let _ = self.consume_if(&TokenKind::RParen);
                expr
            }
            Some(TokenKind::LBrace) => {
                self.advance();
                Expr::Block(self.parse_block())
            }
            Some(tok) => {
                let token = self.advance();
                self.push_error(&format!("unexpected token in expression: {tok:?}"));
                Expr::Unsupported {
                    raw: token.lexeme,
                    span: token.span,
                }
            }
            None => Expr::Unsupported {
                raw: "eof".to_string(),
                span: self.current_span(),
            },
        }
    }

    fn parse_type_expr(&mut self) -> Option<TypeExpr> {
        let start = self.current_span();
        let first = self.consume_type_name();
        if first.is_none() {
            self.push_error("expected type name");
            return None;
        }

        let mut name = first.unwrap();
        while self.consume_if(&TokenKind::Dot) {
            if let Some(next) = self.consume_type_name() {
                name.push('.');
                name.push_str(&next);
            } else {
                self.push_error("expected qualified type name");
                return Some(TypeExpr {
                    span: start,
                    kind: resolve_type_name_kind(&name),
                });
            }
        }

        if self.consume_if(&TokenKind::LBracket) {
            let mut args = Vec::new();
            if !self.consume_if(&TokenKind::RBracket) {
                while !self.is_eof() {
                    if let Some(arg) = self.parse_type_expr() {
                        args.push(arg);
                    } else {
                        break;
                    }
                    if self.consume_if(&TokenKind::Comma) {
                        continue;
                    }
                    if self.consume_if(&TokenKind::RBracket) {
                        break;
                    }
                    self.push_error("expected ',' or ']' in type argument list");
                    break;
                }
            }
            let rendered = args
                .iter()
                .map(render_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            name = format!("{name}[{rendered}]");
        }

        let base = TypeExpr {
            span: start,
            kind: resolve_type_name_kind(&name),
        };
        if self.consume_if(&TokenKind::Question) {
            Some(TypeExpr {
                span: start,
                kind: TypeExprKind::Nullable(Box::new(base)),
            })
        } else {
            Some(base)
        }
    }

    fn parse_int_literal(&self, text: &str) -> i64 {
        let cleaned = text.trim_end_matches(|ch| matches!(ch, 'i' | 'I' | 'u' | 'U' | 'l' | 'L'));
        if let Some(hex) = cleaned.strip_prefix("0x").or_else(|| cleaned.strip_prefix("0X")) {
            i64::from_str_radix(hex, 16).unwrap_or(0)
        } else {
            cleaned.parse::<i64>().unwrap_or(0)
        }
    }

    fn parse_float_literal(&self, text: &str) -> f64 {
        let cleaned = text.trim_end_matches(|ch| matches!(ch, 'f' | 'F' | 'd' | 'D'));
        cleaned.parse::<f64>().unwrap_or(0.0)
    }

    fn parse_char_literal(&self, text: &str) -> Option<char> {
        let inner = text.trim_start_matches('\'').trim_end_matches('\'');
        if inner.is_empty() || inner.len() > 2 && !inner.starts_with('\\') {
            return None;
        }
        let mut chars = inner.chars();
        let first = chars.next()?;
        if first != '\\' {
            if chars.next().is_none() {
                return Some(first);
            }
            return None;
        }

        let escaped = chars.next()?;
        let value = match escaped {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            '0' => '\0',
            'b' => '\x08',
            'x' => {
                let first = chars.next()?;
                let second = chars.next()?;
                u8::from_str_radix(&format!("{first}{second}"), 16)
                    .ok()
                    .map(char::from)?
            }
            _ => return None,
        };
        if chars.next().is_some() {
            None
        } else {
            Some(value)
        }
    }

    fn consume_type_name(&mut self) -> Option<String> {
        match self.peek_kind() {
            Some(TokenKind::Identifier) => Some(self.advance().lexeme),
            Some(TokenKind::TypeBool)
            | Some(TokenKind::TypeByte)
            | Some(TokenKind::TypeShort)
            | Some(TokenKind::TypeUShort)
            | Some(TokenKind::TypeInt)
            | Some(TokenKind::TypeUInt)
            | Some(TokenKind::TypeLong)
            | Some(TokenKind::TypeULong)
            | Some(TokenKind::TypeInt8)
            | Some(TokenKind::TypeInt16)
            | Some(TokenKind::TypeInt32)
            | Some(TokenKind::TypeInt64)
            | Some(TokenKind::TypeUInt8)
            | Some(TokenKind::TypeUInt16)
            | Some(TokenKind::TypeUInt32)
            | Some(TokenKind::TypeUInt64)
            | Some(TokenKind::TypeFloat)
            | Some(TokenKind::TypeFloat32)
            | Some(TokenKind::TypeFloat64)
            | Some(TokenKind::TypeDouble)
            | Some(TokenKind::TypeChar)
            | Some(TokenKind::TypeString)
            | Some(TokenKind::TypeUnit)
            | Some(TokenKind::TypeVar) => Some(self.advance().lexeme),
            Some(_) => None,
            None => None,
        }
    }

    fn parse_parameters(&mut self) -> Vec<ParamDecl> {
        let mut params = Vec::new();
        if self.consume_if(&TokenKind::RParen) {
            return params;
        }

        while !self.is_eof() {
            let span = self.current_span();
            let name = self.consume_identifier().unwrap_or_else(|| {
                self.push_error("expected parameter name");
                "param".to_string()
            });
            let type_hint = if self.consume_if(&TokenKind::Colon) {
                self.parse_type_expr()
            } else {
                None
            };
            params.push(ParamDecl {
                name,
                type_hint,
                span,
            });
            if self.consume_if(&TokenKind::Comma) {
                continue;
            }
            break;
        }

        params
    }

    fn parse_type_params(&mut self) -> Vec<String> {
        if !self.consume_if(&TokenKind::LBracket) {
            return Vec::new();
        }

        let mut params = Vec::new();
        if self.consume_if(&TokenKind::RBracket) {
            return params;
        }

        while !self.is_eof() {
            if let Some(param) = self.consume_identifier() {
                params.push(param);
            } else {
                self.push_error("expected type parameter name");
                break;
            }

            if self.consume_if(&TokenKind::Comma) {
                continue;
            }
            if self.consume_if(&TokenKind::RBracket) {
                break;
            }
            self.push_error("expected ',' or ']' in type parameter list");
            break;
        }

        params
    }

    fn parse_struct_interfaces(&mut self) -> Vec<String> {
        if !self.consume_if(&TokenKind::Colon) {
            return Vec::new();
        }

        let mut interfaces = Vec::new();
        while !self.is_eof() {
            interfaces.push(self.parse_path("struct interface").join("."));
            if self.consume_if(&TokenKind::Comma) {
                continue;
            }
            break;
        }
        interfaces
    }

    fn parse_operator_symbol(&mut self) -> String {
        let symbol = match self.peek_kind() {
            Some(TokenKind::Plus) => "+",
            Some(TokenKind::Minus) => "-",
            Some(TokenKind::Star) => "*",
            Some(TokenKind::Slash) => "/",
            Some(TokenKind::EqEq) => "==",
            Some(TokenKind::NotEq) => "!=",
            Some(TokenKind::Lt) => "<",
            Some(TokenKind::LtEq) => "<=",
            Some(TokenKind::Gt) => ">",
            Some(TokenKind::GtEq) => ">=",
            Some(TokenKind::Bang) => "!",
            _ => {
                self.push_error("expected operator symbol after `operator`");
                "?"
            }
        };
        if symbol != "?" {
            self.advance();
        }
        symbol.to_string()
    }

    fn parse_base_specs(&mut self) -> Vec<BaseSpec> {
        if !self.consume_if(&TokenKind::Colon) {
            return Vec::new();
        }

        let mut specs = Vec::new();
        while !self.is_eof() {
            let start = self.current_span();
            let shared = self.consume_if(&TokenKind::Shared);
            let name = self.parse_path("base name").join(".");
            let ctor_args = if self.consume_if(&TokenKind::LParen) {
                let args = self.parse_expr_list(TokenKind::RParen);
                let _ = self.consume_if(&TokenKind::RParen);
                args
            } else {
                Vec::new()
            };
            specs.push(BaseSpec {
                name,
                shared,
                ctor_args,
                span: start,
            });
            if self.consume_if(&TokenKind::Comma) {
                continue;
            }
            break;
        }
        specs
    }

    fn parse_path(&mut self, context: &str) -> Vec<String> {
        let mut path = Vec::new();
        let first = self.consume_identifier();
        if first.is_none() {
            self.push_error(&format!("expected {context}"));
            return path;
        }

        path.push(first.unwrap());
        while self.consume_if(&TokenKind::Dot) {
            if let Some(seg) = self.consume_identifier() {
                path.push(seg);
            } else {
                self.push_error(&format!("expected {context} segment"));
                break;
            }
        }

        path
    }

    fn is_identifier_start(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Identifier))
    }

    fn is_contextual_keyword(&self, kw: &str) -> bool {
        self.peek()
            .map(|t| t.kind == TokenKind::Identifier && t.lexeme == kw)
            .unwrap_or(false)
    }

    fn peek_nth_kind(&self, n: usize) -> Option<TokenKind> {
        self.tokens.get(self.pos + n).map(|t| t.kind.clone())
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|token| token.kind.clone())
    }

    fn consume_if(&mut self, kind: &TokenKind) -> bool {
        if matches!(self.peek().map(|token| &token.kind), Some(token_kind) if token_kind == kind) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Token {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.pos += 1;
            token
        } else {
            Token {
                kind: TokenKind::Eof,
                span: self.current_span(),
                lexeme: String::new(),
            }
        }
    }

    fn current_span(&self) -> Span {
        self.peek()
            .map(|token| token.span)
            .or_else(|| self.tokens.last().map(|token| token.span))
            .unwrap_or_else(|| Span::new(0, 0))
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Eof) | None)
    }

    fn consume_identifier(&mut self) -> Option<String> {
        if self.is_identifier_start() {
            Some(self.advance().lexeme)
        } else {
            None
        }
    }

    fn consume_stmt_terminator(&mut self) -> bool {
        self.skip_terminators();
        if self.consume_if(&TokenKind::Newline)
            || self.consume_if(&TokenKind::Semicolon)
            || self.consume_if(&TokenKind::SyntheticSemicolon)
        {
            return true;
        }
        false
    }

    fn skip_terminators(&mut self) {
        while matches!(
            self.peek_kind(),
            Some(TokenKind::Newline | TokenKind::Semicolon | TokenKind::SyntheticSemicolon)
        ) {
            self.pos += 1;
        }
    }

    fn is_statement_boundary(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(
                TokenKind::Semicolon
                    | TokenKind::SyntheticSemicolon
                    | TokenKind::Newline
                    | TokenKind::RParen
                    | TokenKind::RBracket
                    | TokenKind::RBrace
                    | TokenKind::Comma
                    | TokenKind::Eof
                    | TokenKind::Else
                    | TokenKind::Catch
                    | TokenKind::Finally
            )
        )
    }

    fn is_statement_start(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(
                TokenKind::Return
                    | TokenKind::Throw
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::For
                    | TokenKind::Using
                    | TokenKind::Try
                    | TokenKind::Continue
                    | TokenKind::Break
                    | TokenKind::LBrace
                    | TokenKind::Var
            )
        )
    }

    fn recover_to_item_boundary(&mut self) {
        self.skip_terminators();
        while !self.is_eof() {
            if matches!(
                self.peek_kind(),
                Some(TokenKind::Class)
                    | Some(TokenKind::Struct)
                    | Some(TokenKind::Interface)
                    | Some(TokenKind::Def)
                    | Some(TokenKind::Virtual)
                    | Some(TokenKind::Override)
                    | Some(TokenKind::Static)
                    | Some(TokenKind::Var)
                    | Some(TokenKind::Import)
                    | Some(TokenKind::From)
                    | Some(TokenKind::Using)
                    | Some(TokenKind::Public)
                    | Some(TokenKind::If)
                    | Some(TokenKind::While)
                    | Some(TokenKind::For)
                    | Some(TokenKind::Try)
                    | Some(TokenKind::RBrace)
                    | Some(TokenKind::Eof)
            ) {
                break;
            }
            self.advance();
            self.skip_terminators();
        }
    }

    fn recover_to_block_boundary(&mut self) {
        self.skip_terminators();
        while !self.is_eof() {
            if matches!(self.peek_kind(), Some(TokenKind::RBrace) | Some(TokenKind::Eof)) {
                break;
            }
            self.advance();
            self.skip_terminators();
        }
    }

    fn recover_to_statement_boundary(&mut self) {
        self.skip_terminators();
        while !self.is_eof() {
            if self.is_statement_boundary() || self.is_statement_start() {
                break;
            }
            self.advance();
            self.skip_terminators();
        }
        if matches!(self.peek_kind(), Some(TokenKind::Semicolon | TokenKind::SyntheticSemicolon)) {
            self.advance();
        }
        self.skip_terminators();
    }

    fn push_error(&mut self, message: &str) {
        self.diagnostics.push(Diagnostic {
            id: "parse_error".to_string(),
            severity: Severity::Error,
            span: Some(self.current_span()),
            file: Some(PathBuf::from(self.file.clone())),
            message: message.to_string(),
            notes: Vec::new(),
            suggestions: Vec::new(),
        });
    }
}

fn span_between(left: &Expr, right: &Expr) -> Span {
    let l = expr_span(left);
    let r = expr_span(right);
    Span::new(l.lo.min(r.lo), l.hi.max(r.hi))
}

    #[derive(Debug, Clone)]
enum PrattInfix {
    Assign(AssignOp),
    Binary(BinaryOp),
    Call,
    Member,
    QualifiedMember,
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Identifier { span, .. }
        | Expr::Literal { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Unary { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Call { span, .. }
        | Expr::MemberAccess { span, .. }
        | Expr::Unsupported { span, .. } => *span,
        Expr::Block(Block { span, .. }) => *span,
    }
}

fn expr_identifier_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Identifier { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn resolve_type_name_kind(name: &str) -> TypeExprKind {
    match name {
        "Var" => TypeExprKind::Var,
        "Unit" => TypeExprKind::Unit,
        _ => TypeExprKind::Named(name.to_string()),
    }
}

fn render_type_expr(ty: &TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Named(name) => name.clone(),
        TypeExprKind::Var => "Var".to_string(),
        TypeExprKind::Unit => "Unit".to_string(),
        TypeExprKind::Nullable(inner) => format!("{}?", render_type_expr(inner)),
        TypeExprKind::Function(params, result) => {
            let params = params
                .iter()
                .map(render_type_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({params}) -> {}", render_type_expr(result))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexc_diag::DiagnosticSink;
    use nexc_lex::{asi_normalize, lex};

    fn parse_snapshot(source: &str, expect_no_errors: bool) -> (SourceFile, Vec<Diagnostic>) {
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("snapshot.nex".to_string()), &mut sink);
        let tokens = asi_normalize(&tokens);
        assert!(
            !expect_no_errors || sink.is_empty(),
            "lexer errors: {:?}",
            sink.diagnostics()
        );
        let mut parser = Parser::new(&tokens, "snapshot.nex".to_string());
        let file = parser.parse();
        let mut diagnostics = sink.into_vec();
        diagnostics.extend(parser.diagnostics().to_vec());
        (file, diagnostics)
    }

    fn summarize_items(file: &SourceFile) -> String {
        file
            .items
            .iter()
            .map(summarize_item)
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn summarize_item(item: &Item) -> String {
        match item {
            Item::Import(import) => {
                let kind = match &import.kind {
                    ImportKind::Module => format!("Module {}", import.path.join(".")),
                    ImportKind::From(names) => {
                        let names = if names.is_empty() {
                            "none".to_string()
                        } else {
                            summarize_names(names)
                        };
                        format!("From({}) names={}", import.path.join("."), names)
                    }
                };
                format!("Import {kind}")
            }
            Item::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|param| param.name.as_str())
                    .collect::<Vec<_>>()
                    .join(",");
                format!(
                    "Function {} vis={} params={} return={}",
                    function.name,
                    vis_name(function.is_public),
                    params,
                    function
                        .return_type
                        .as_ref()
                        .map(ty_to_string)
                        .unwrap_or_else(|| "None".to_string())
                )
            }
            Item::Class(class) => {
                let bases = if class.base_specs.is_empty() {
                    "none".to_string()
                } else {
                    class
                        .base_specs
                        .iter()
                        .map(|base| base.name.as_str())
                        .collect::<Vec<_>>()
                        .join(",")
                };
                format!(
                    "Class {} vis={} bases={} fields={} methods={}",
                    class.name,
                    vis_name(matches!(class.visibility, Visibility::Public)),
                    bases,
                    class.fields.len(),
                    class.methods.len()
                )
            }
            Item::Interface(interface) => format!(
                "Interface {} vis={} methods={}",
                interface.name,
                vis_name(matches!(interface.visibility, Visibility::Public)),
                interface.methods.len()
            ),
            Item::Struct(strukt) => {
                let interfaces = if strukt.interfaces.is_empty() {
                    "none".to_string()
                } else {
                    strukt.interfaces.join(",")
                };
                format!(
                    "Struct {} vis={} interfaces={} fields={} methods={}",
                    strukt.name,
                    vis_name(matches!(strukt.visibility, Visibility::Public)),
                    interfaces,
                    strukt.fields.len(),
                    strukt.methods.len()
                )
            }
            Item::Variable(var) => {
                format!("Variable {} vis={}", var.name, vis_name(matches!(var.visibility, Visibility::Public)))
            }
            Item::Using(using) => {
                format!(
                    "Using {} body_stmts={}",
                    using.variable_name,
                    using.body.statements.len()
                )
            }
            Item::Statement(statement) => format!("Statement {}", summarize_stmt(statement)),
        }
    }

    fn summarize_stmt(statement: &Stmt) -> String {
        match statement {
            Stmt::Expr(expr) => format!("Expr {}", summarize_expr(expr)),
            Stmt::Return(value, _) => {
                let value = value
                    .as_ref()
                    .map(summarize_expr)
                    .unwrap_or_else(|| "none".to_string());
                format!("Return {value}")
            }
            Stmt::Throw(value, _) => format!("Throw {}", summarize_expr(value)),
            Stmt::VarDecl(var) => format!("VarDecl {}", var.name),
            Stmt::Using(_) => "Using".to_string(),
            Stmt::If(_) => "If".to_string(),
            Stmt::While(_) => "While".to_string(),
            Stmt::For(_) => "For".to_string(),
            Stmt::Try(try_stmt) => format!(
                "Try body={} catches={} finally={}",
                try_stmt.body.statements.len(),
                try_stmt.catches.len(),
                if try_stmt.finally.is_some() { "yes" } else { "no" }
            ),
            Stmt::Block(block) => format!("Block {}", block.statements.len()),
            Stmt::Continue(_) => "Continue".to_string(),
            Stmt::Break(_) => "Break".to_string(),
        }
    }

    fn summarize_expr(expr: &Expr) -> String {
        match expr {
            Expr::Identifier { name, .. } => format!("Ident({name})"),
            Expr::Literal { value, .. } => format!("Literal({value:?})"),
            Expr::Binary { op, lhs, rhs, .. } => {
                format!(
                    "({} {} {})",
                    summarize_expr(lhs),
                    binary_to_string(op),
                    summarize_expr(rhs)
                )
            }
            Expr::Unary { op, expr, .. } => format!("({}{})", unary_to_string(op), summarize_expr(expr)),
            Expr::Assign {
                target,
                value,
                op,
                ..
            } => format!(
                "({} {} {})",
                summarize_expr(target),
                assign_to_string(op),
                summarize_expr(value)
            ),
            Expr::Call { callee, args, .. } => {
                let args = args
                    .iter()
                    .map(summarize_expr)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Call {}({})", summarize_expr(callee), args)
            }
            Expr::MemberAccess {
                receiver,
                name,
                qualifier,
                ..
            } => {
                if let Some(qualifier) = qualifier {
                    format!("{}::{}", qualifier, name)
                } else {
                    format!("{}.{}", summarize_expr(receiver), name)
                }
            }
            Expr::Block(block) => format!("Block({})", block.statements.len()),
            Expr::Unsupported { raw, .. } => format!("Unsupported({raw})"),
        }
    }

    fn summarize_names(names: &[String]) -> String {
        names.join(",")
    }

    fn vis_name(is_public: bool) -> &'static str {
        if is_public {
            "public"
        } else {
            "internal"
        }
    }

    fn binary_to_string(op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Or => "||",
            BinaryOp::And => "&&",
            BinaryOp::EqEq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::LtEq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::GtEq => ">=",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
        }
    }

    fn unary_to_string(op: &UnaryOp) -> &'static str {
        match op {
            UnaryOp::Not => "!",
            UnaryOp::Neg => "-",
        }
    }

    fn assign_to_string(op: &AssignOp) -> &'static str {
        match op {
            AssignOp::Assign => "=",
            AssignOp::AddAssign => "+=",
            AssignOp::SubAssign => "-=",
            AssignOp::MulAssign => "*=",
            AssignOp::DivAssign => "/=",
        }
    }

    fn ty_to_string(ty: &TypeExpr) -> String {
        match &ty.kind {
            TypeExprKind::Named(name) => name.clone(),
            TypeExprKind::Var => "Var".to_string(),
            TypeExprKind::Unit => "Unit".to_string(),
            TypeExprKind::Nullable(inner) => format!("{}?", ty_to_string(inner)),
            TypeExprKind::Function(params, result) => {
                let params = params
                    .iter()
                    .map(ty_to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) -> {}", params, ty_to_string(result))
            }
        }
    }

    #[test]
    fn parse_conformance_snapshot() {
        let source = r#"
import net.io as io
from util import Reader, Writer

public class Demo : SharedBase, Parent {
    value: Int
    public def init(seed: Int) -> Int {
        return seed + 1
    }
}

interface IService {
    def dispose() -> Unit
}

using (resource = make_resource()) {
    if (resource > 0) {
        return 1
    } else {
        return 0
    }
}

public def build(count: Int) -> Int {
    var total = count + 1
    return total
}
"#;
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let snapshot = summarize_items(&file);
        let expected = [
            "Import Module net.io",
            "Import From(util) names=Reader,Writer",
            "Class Demo vis=public bases=SharedBase,Parent fields=1 methods=1",
            "Interface IService vis=internal methods=1",
            "Using resource body_stmts=1",
            "Function build vis=public params=count return=Int",
        ]
        .join("\n");
        assert_eq!(snapshot, expected);
    }

    #[test]
    fn parse_expression_precedence_snapshot() {
        let source = "def calc(a: Int, b: Int) -> Int { return A::add(a + 2 * 3, b - 1) == 7 && true }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = function.body.as_ref().unwrap();
        let ret = match body {
            Expr::Block(block) => &block.statements[0],
            _ => panic!("expected block"),
        };
        let expr = match ret {
            Stmt::Return(Some(expr), _) => expr,
            other => panic!("expected return, got {other:?}"),
        };
        let snapshot = summarize_expr(expr);
        assert!(snapshot.contains("&&"));
        assert!(snapshot.contains("=="));
        assert!(snapshot.contains("::add"));
        assert!(snapshot.contains("Literal(Int(7))"));
        assert!(snapshot.contains("Call A::add"));
    }

    #[test]
    fn parse_expression_assignment_snapshot() {
        let source =
            "def calc() -> Unit { a = b = 1; x += y * 2; return a == b && true }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };

        let first = match &body[0] {
            Stmt::Expr(expr) => expr,
            other => panic!("expected expression statement, got {other:?}"),
        };
        assert!(matches!(first, Expr::Assign { op: AssignOp::Assign, .. }));
        let second = match &body[1] {
            Stmt::Expr(Expr::Assign { value, .. }) => value,
            other => panic!("expected compound assignment, got {other:?}"),
        };
        assert!(matches!(
            &**second,
            Expr::Binary {
                op: BinaryOp::Mul,
                ..
            }
        ));
    }

    #[test]
    fn parse_literals_snapshot() {
        let source = "def lit() -> Unit { '\\n'; 3.14f; 10d }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = function.body.as_ref().unwrap();
        let block = match body {
            Expr::Block(block) => block,
            _ => panic!("expected block"),
        };
        let first = match &block.statements[0] {
            Stmt::Expr(Expr::Literal {
                value: Literal::Char(_),
                ..
            }) => Some(()),
            _ => None,
        };
        assert!(first.is_some(), "expected char literal expression");

        let second = match &block.statements[1] {
            Stmt::Expr(Expr::Literal {
                value: Literal::Float(_),
                ..
            }) => Some(()),
            _ => None,
        };
        assert!(second.is_some(), "expected float literal expression");
        let third = match &block.statements[2] {
            Stmt::Expr(Expr::Literal {
                value: Literal::Float(_),
                ..
            }) => Some(()),
            _ => None,
        };
        assert!(third.is_some(), "expected second float literal expression");
    }

    #[test]
    fn parse_if_without_else() {
        let source = "def cond() -> Unit { if (true) { return } }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = function.body.as_ref().unwrap();
        let stmt = match body {
            Expr::Block(block) => &block.statements[0],
            _ => panic!("expected block"),
        };
        match stmt {
            Stmt::If(if_stmt) => assert!(if_stmt.else_branch.is_none()),
            other => panic!("expected if statement, got {other:?}"),
        }
    }

    #[test]
    fn parse_try_catch_finally_snapshot() {
        let source = r#"
def risky() -> Int {
    try {
        return 0
    } catch (err: Error) {
        return 1
    } finally {
        return 2
    }
}
"#;
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = function.body.as_ref().unwrap();
        let stmt = match body {
            Expr::Block(block) => &block.statements[0],
            _ => panic!("expected block"),
        };
        assert_eq!(summarize_stmt(stmt), "Try body=1 catches=1 finally=yes");
    }

    #[test]
    fn parse_struct_generic_and_operator_snapshot() {
        let source = r#"
public struct Vec2[T] : Equatable {
    x: T
    y: T

    public static def operator+(a: Vec2[T], b: Vec2[T]) -> Vec2[T] {
        return a
    }
}
"#;
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let snapshot = summarize_items(&file);
        let expected = "Struct Vec2 vis=public interfaces=Equatable fields=2 methods=1";
        assert_eq!(snapshot, expected);
    }

    #[test]
    fn parse_type_expr_with_generic_arguments() {
        let source = "def use_list(values: List[Int], map: Map[String, Int]) -> Unit { return }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(
            ty_to_string(function.params[0].type_hint.as_ref().unwrap()),
            "List[Int]"
        );
        assert_eq!(
            ty_to_string(function.params[1].type_hint.as_ref().unwrap()),
            "Map[String, Int]"
        );
    }

    #[test]
    fn parse_throw_statement() {
        let source = "def fail() -> Unit { throw \"boom\" }";
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(matches!(body[0], Stmt::Throw(_, _)));
    }

    #[test]
    fn parse_while_and_for_statements() {
        let source = r#"
def loopers() -> Unit {
    while (true) { break }
    for (i = 0; i < 10; i = i + 1) { continue }
}
"#;
        let (file, diagnostics) = parse_snapshot(source, true);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(matches!(body[0], Stmt::While(_)));
        assert!(matches!(body[1], Stmt::For(_)));
    }

    #[test]
    fn parse_error_recovery() {
        let source = "from util import\nvar x = 1";
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());
        let snapshot = summarize_items(&file);
        let expected = ["Import From(util) names=none", "Variable x vis=internal"].join("\n");
        assert_eq!(snapshot, expected);
    }

    #[test]
    fn parse_error_recovery_function_missing_body() {
        let source = r#"
def broken() -> Unit
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());
        assert!(matches!(&file.items[0], Item::Function(function) if function.name == "broken"));
        assert!(matches!(&file.items[1], Item::Function(function) if function.name == "good"));
    }

    #[test]
    fn parse_error_recovery_public_without_decl() {
        let source = r#"
public
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());
        let good_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
        assert!(!good_fn.is_public);
    }

    #[test]
    fn parse_error_recovery_missing_class_name() {
        let source = "class { value: Int }\ndef good() -> Unit { return }";
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());
        let snapshot = summarize_items(&file);
        let expected = [
            "Class Class vis=internal bases=none fields=1 methods=0",
            "Function good vis=internal params= return=Unit",
        ]
        .join("\n");
        assert_eq!(snapshot, expected);
    }

    #[test]
    fn parse_error_recovery_if_without_parentheses() {
        let source = r#"
def bad() -> Unit {
    if x > 0 { return 1 } else { return 0 }
    return 2
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert_eq!(bad_body.len(), 2);
        assert_eq!(summarize_stmt(&bad_body[0]), "If");
        assert_eq!(summarize_stmt(&bad_body[1]), "Return Literal(Int(2))");

        let good_fn = match &file.items[1] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
    }

    #[test]
    fn parse_error_recovery_using_missing_binding_expression() {
        let source = r#"
def bad() -> Unit {
    using (resource) {
        return 1
    }
    return 2
}
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert_eq!(summarize_stmt(&body[0]), "Using");
        assert_eq!(summarize_stmt(&body[1]), "Return Literal(Int(2))");
    }

    #[test]
    fn parse_error_recovery_try_without_catch_signature() {
        let source = r#"
def bad() -> Unit {
    try {
        return 1
    } catch {
        return 0
    } finally
    return 2
}
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        let try_stmt = match &body[0] {
            Stmt::Try(try_stmt) => try_stmt,
            item => panic!("expected try statement, got {item:?}"),
        };
        assert_eq!(try_stmt.catches.len(), 1);
        assert!(try_stmt.finally.is_none());
        assert_eq!(summarize_stmt(&body[1]), "Return Literal(Int(2))");
    }

    #[test]
    fn parse_error_recovery_expression_list_arguments() {
        let source = r#"
def bad() -> Unit {
    emit(1 2)
    return 3
}
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let function = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let body = match function.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        let call = match &body[0] {
            Stmt::Expr(Expr::Call { args, .. }) => args,
            item => panic!("expected call expression, got {item:?}"),
        };
        assert_eq!(call.len(), 1);
        assert_eq!(summarize_stmt(&body[1]), "Return Literal(Int(3))");
    }

    #[test]
    fn parse_error_recovery_if_missing_else_block() {
        let source = r#"
def bad() -> Unit {
    if (true) {
        return 1
    } else return 2
    return 3
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(!bad_body.is_empty());
        assert!(bad_body.iter().any(|stmt| matches!(stmt, Stmt::If(_))));
        assert!(bad_body.iter().any(|stmt| matches!(
            stmt,
            Stmt::Return(Some(Expr::Literal { .. }), _)
                | Stmt::Return(None, _)
        )));

        let good_fn = match &file.items[1] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
    }

    #[test]
    fn parse_error_recovery_if_missing_braced_body() {
        let source = r#"
def bad() -> Unit {
    if (true)
        return 1
    return 2
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(bad_body.len() >= 2);
        assert!(bad_body.iter().any(|stmt| matches!(stmt, Stmt::If(_))));
        assert!(bad_body
            .iter()
            .any(|stmt| matches!(stmt, Stmt::Return(Some(Expr::Literal { .. }), _))));

        let good_fn = match &file.items[1] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
    }

    #[test]
    fn parse_error_recovery_try_missing_catch_block() {
        let source = r#"
def bad() -> Unit {
    try {
        return 1
    } catch (err) return 2
    return 3
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(bad_body.iter().any(|stmt| matches!(stmt, Stmt::Try(_))));
        assert!(bad_body.iter().any(|stmt| matches!(
            stmt,
            Stmt::Return(Some(Expr::Literal { .. }), _)
                | Stmt::Return(None, _)
        )));

        let good_fn = match &file.items[1] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
    }

    #[test]
    fn parse_error_recovery_call_missing_rparen() {
        let source = r#"
def bad() -> Unit {
    emit(1, 2
    return 3
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(!bad_body.is_empty());
        let mut has_two_arg_call = false;
        for stmt in bad_body {
            if let Stmt::Expr(Expr::Call { args, .. }) = stmt {
                has_two_arg_call = args.len() == 2;
                if has_two_arg_call {
                    break;
                }
            }
        }
        assert!(has_two_arg_call);
        assert!(bad_body.iter().any(|stmt| matches!(
            stmt,
            Stmt::Return(Some(Expr::Literal { .. }), _)
                | Stmt::Return(None, _)
        )));
    }

    #[test]
    fn parse_error_recovery_try_catch_missing_body_block() {
        let source = r#"
def bad() -> Unit {
    try {
        return 1
    } catch (err) return 2
    return 3
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert!(bad_body.iter().any(|stmt| matches!(stmt, Stmt::Try(_))));
        assert!(bad_body
            .iter()
            .any(|stmt| matches!(stmt, Stmt::Return(Some(Expr::Literal { .. }), _))));

        let good_fn = match &file.items[1] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        assert_eq!(good_fn.name, "good");
    }

    #[test]
    fn parse_error_recovery_using_missing_resource_initializer() {
        let source = r#"
def bad() -> Unit {
    using (resource = ) {
        return 1
    }
    return 2
}
def good() -> Unit { return }
"#;
        let (file, diagnostics) = parse_snapshot(source, false);
        assert!(!diagnostics.is_empty());

        let bad_fn = match &file.items[0] {
            Item::Function(function) => function,
            item => panic!("expected function, got {item:?}"),
        };
        let bad_body = match bad_fn.body.as_ref().unwrap() {
            Expr::Block(block) => &block.statements,
            _ => panic!("expected block"),
        };
        assert_eq!(summarize_stmt(&bad_body[0]), "Using");
        assert_eq!(summarize_stmt(&bad_body[1]), "Return Literal(Int(2))");
    }
}
