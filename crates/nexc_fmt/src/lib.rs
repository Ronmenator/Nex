use nexc_diag::DiagnosticSink;
use nexc_lex::{asi_normalize, lex};
use nexc_parse::Parser;

pub fn format_source(source: &str) -> String {
    let mut sink = DiagnosticSink::new();
    let tokens = lex(source, None, &mut sink);
    let tokens = asi_normalize(&tokens);
    let mut parser = Parser::new(&tokens, "<fmt>".into());
    let file = parser.parse();

    if sink.has_errors() || !parser.diagnostics().is_empty() {
        let normalized = source.replace("\r\n", "\n");
        return normalized.trim_end().to_string() + "\n";
    }

    let mut printer = Printer::new();
    printer.print_file(&file);
    printer.output
}

pub fn format_with_diagnostics(source: &str, sink: &mut DiagnosticSink) -> String {
    let _ = lex(source, None, sink);
    format_source(source)
}

struct Printer {
    output: String,
    indent: usize,
}

impl Printer {
    fn new() -> Self {
        Self { output: String::new(), indent: 0 }
    }

    fn line(&mut self, text: &str) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(text);
        self.output.push('\n');
    }

    fn blank(&mut self) {
        self.output.push('\n');
    }

    fn print_file(&mut self, file: &nexc_ast::SourceFile) {
        let mut first = true;
        for item in &file.items {
            if !first {
                self.blank();
            }
            first = false;
            self.print_item(item);
        }
    }

    fn print_item(&mut self, item: &nexc_ast::Item) {
        use nexc_ast::Item;
        match item {
            Item::Import(imp) => self.print_import(imp),
            Item::Function(f) => self.print_function(f, false),
            Item::Class(c) => self.print_class(c),
            Item::Interface(i) => self.print_interface(i),
            Item::Struct(s) => self.print_struct(s),
            Item::Variable(v) => self.print_var_decl(v),
            Item::Using(u) => self.print_using(u),
            Item::Statement(s) => self.print_stmt(s),
        }
    }

    fn print_import(&mut self, imp: &nexc_ast::ImportDecl) {
        let path_str = imp.path.join(".");
        let vis = visibility_prefix(imp.visibility);
        match &imp.kind {
            nexc_ast::ImportKind::Module => {
                if let Some(alias) = &imp.alias {
                    self.line(&format!("{vis}import {path_str} as {alias}"));
                } else {
                    self.line(&format!("{vis}import {path_str}"));
                }
            }
            nexc_ast::ImportKind::From(names) => {
                self.line(&format!("{vis}from {path_str} import {}", names.join(", ")));
            }
        }
    }

    fn print_function(&mut self, f: &nexc_ast::FunctionDecl, is_method: bool) {
        let mut prefix = String::new();
        if !is_method && f.is_public {
            prefix.push_str("public ");
        }
        if f.is_static { prefix.push_str("static "); }
        if f.is_virtual { prefix.push_str("virtual "); }
        if f.is_override { prefix.push_str("override "); }

        let name = if let Some(op) = &f.operator {
            format!("operator{op}")
        } else {
            f.name.clone()
        };

        let type_params = if f.type_params.is_empty() {
            String::new()
        } else {
            format!("[{}]", f.type_params.join(", "))
        };

        let params: Vec<String> = f.params.iter().map(|p| {
            if let Some(ty) = &p.type_hint {
                format!("{}: {}", p.name, format_type_expr(ty))
            } else {
                p.name.clone()
            }
        }).collect();

        let ret = f.return_type.as_ref()
            .map(|t| format!(" -> {}", format_type_expr(t)))
            .unwrap_or_default();

        if let Some(body) = &f.body {
            self.line(&format!("{prefix}def {name}{type_params}({}) {ret} {{", params.join(", ")));
            self.indent += 1;
            if let nexc_ast::Expr::Block(block) = body {
                for stmt in &block.statements {
                    self.print_stmt(stmt);
                }
            }
            self.indent -= 1;
            self.line("}");
        } else {
            self.line(&format!("{prefix}def {name}{type_params}({}){ret}", params.join(", ")));
        }
    }

    fn print_class(&mut self, c: &nexc_ast::ClassDecl) {
        let vis = visibility_prefix(c.visibility);
        let type_params = if c.type_params.is_empty() {
            String::new()
        } else {
            format!("[{}]", c.type_params.join(", "))
        };
        let bases = if c.base_specs.is_empty() {
            String::new()
        } else {
            let bs: Vec<String> = c.base_specs.iter().map(|b| {
                let prefix = if b.shared { "shared " } else { "" };
                format!("{prefix}{}", b.name)
            }).collect();
            format!(" : {}", bs.join(", "))
        };

        self.line(&format!("{vis}class {}{type_params}{bases} {{", c.name));
        self.indent += 1;
        for field in &c.fields {
            self.print_field(field);
        }
        if !c.fields.is_empty() && !c.methods.is_empty() {
            self.blank();
        }
        for method in &c.methods {
            self.print_function(method, true);
        }
        self.indent -= 1;
        self.line("}");
    }

    fn print_interface(&mut self, i: &nexc_ast::InterfaceDecl) {
        let vis = visibility_prefix(i.visibility);
        let type_params = if i.type_params.is_empty() {
            String::new()
        } else {
            format!("[{}]", i.type_params.join(", "))
        };
        self.line(&format!("{vis}interface {}{type_params} {{", i.name));
        self.indent += 1;
        for method in &i.methods {
            self.print_function(method, true);
        }
        self.indent -= 1;
        self.line("}");
    }

    fn print_struct(&mut self, s: &nexc_ast::StructDecl) {
        let vis = visibility_prefix(s.visibility);
        let type_params = if s.type_params.is_empty() {
            String::new()
        } else {
            format!("[{}]", s.type_params.join(", "))
        };
        let ifaces = if s.interfaces.is_empty() {
            String::new()
        } else {
            format!(" : {}", s.interfaces.join(", "))
        };
        self.line(&format!("{vis}struct {}{type_params}{ifaces} {{", s.name));
        self.indent += 1;
        for field in &s.fields {
            self.print_field(field);
        }
        if !s.fields.is_empty() && !s.methods.is_empty() {
            self.blank();
        }
        for method in &s.methods {
            self.print_function(method, true);
        }
        self.indent -= 1;
        self.line("}");
    }

    fn print_field(&mut self, f: &nexc_ast::FieldDecl) {
        let vis = visibility_prefix(f.visibility);
        if let Some(ty) = &f.ty {
            self.line(&format!("{vis}{}: {}", f.name, format_type_expr(ty)));
        } else {
            self.line(&format!("{vis}{}", f.name));
        }
    }

    fn print_var_decl(&mut self, v: &nexc_ast::VarDecl) {
        let vis = visibility_prefix(v.visibility);
        if v.is_dynamic {
            self.line(&format!("{vis}var {}", v.name));
        } else if let Some(ty) = &v.explicit_type {
            self.line(&format!("{vis}{}: {}", v.name, format_type_expr(ty)));
        } else {
            self.line(&format!("{vis}{}", v.name));
        }
    }

    fn print_using(&mut self, u: &nexc_ast::UsingDecl) {
        self.line(&format!("using ({} = {}) {{", u.variable_name, format_expr(&u.expr)));
        self.indent += 1;
        for stmt in &u.body.statements {
            self.print_stmt(stmt);
        }
        self.indent -= 1;
        self.line("}");
    }

    fn print_stmt(&mut self, stmt: &nexc_ast::Stmt) {
        use nexc_ast::Stmt;
        match stmt {
            Stmt::Expr(expr) => self.line(&format_expr(expr)),
            Stmt::Return(Some(expr), _) => self.line(&format!("return {}", format_expr(expr))),
            Stmt::Return(None, _) => self.line("return"),
            Stmt::Throw(expr, _) => self.line(&format!("throw {}", format_expr(expr))),
            Stmt::VarDecl(v) => self.print_var_decl(v),
            Stmt::Using(u) => self.print_using(u),
            Stmt::If(if_stmt) => {
                self.line(&format!("if ({}) {{", format_expr(&if_stmt.condition)));
                self.indent += 1;
                self.print_stmt(&if_stmt.then_branch);
                self.indent -= 1;
                if let Some(else_branch) = &if_stmt.else_branch {
                    self.line("} else {");
                    self.indent += 1;
                    self.print_stmt(else_branch);
                    self.indent -= 1;
                }
                self.line("}");
            }
            Stmt::While(w) => {
                self.line(&format!("while ({}) {{", format_expr(&w.condition)));
                self.indent += 1;
                self.print_stmt(&w.body);
                self.indent -= 1;
                self.line("}");
            }
            Stmt::For(f) => {
                let init = f.init.as_ref().map(format_expr).unwrap_or_default();
                let cond = f.condition.as_ref().map(format_expr).unwrap_or_default();
                let step = f.step.as_ref().map(format_expr).unwrap_or_default();
                self.line(&format!("for ({init}; {cond}; {step}) {{"));
                self.indent += 1;
                self.print_stmt(&f.body);
                self.indent -= 1;
                self.line("}");
            }
            Stmt::Try(t) => {
                self.line("try {");
                self.indent += 1;
                for s in &t.body.statements {
                    self.print_stmt(s);
                }
                self.indent -= 1;
                for catch in &t.catches {
                    let param = match (&catch.variable_name, &catch.variable_type) {
                        (Some(n), Some(t)) => format!("{n}: {}", format_type_expr(t)),
                        (Some(n), None) => n.clone(),
                        _ => String::new(),
                    };
                    self.line(&format!("}} catch ({param}) {{"));
                    self.indent += 1;
                    for s in &catch.body.statements {
                        self.print_stmt(s);
                    }
                    self.indent -= 1;
                }
                if let Some(finally) = &t.finally {
                    self.line("} finally {");
                    self.indent += 1;
                    for s in &finally.statements {
                        self.print_stmt(s);
                    }
                    self.indent -= 1;
                }
                self.line("}");
            }
            Stmt::Block(block) => {
                for s in &block.statements {
                    self.print_stmt(s);
                }
            }
            Stmt::Continue(_) => self.line("continue"),
            Stmt::Break(_) => self.line("break"),
        }
    }
}

fn format_expr(expr: &nexc_ast::Expr) -> String {
    match expr {
        nexc_ast::Expr::Identifier { name, .. } => name.clone(),
        nexc_ast::Expr::Literal { value, .. } => match value {
            nexc_ast::Literal::Int(v) => v.to_string(),
            nexc_ast::Literal::Float(v) => format!("{v}"),
            nexc_ast::Literal::Char(c) => format!("'{c}'"),
            nexc_ast::Literal::Bool(b) => b.to_string(),
            nexc_ast::Literal::String(s) => format!("\"{s}\""),
            nexc_ast::Literal::Null => "null".into(),
        },
        nexc_ast::Expr::Binary { op, lhs, rhs, .. } => {
            let op_str = match op {
                nexc_ast::BinaryOp::Add => "+",
                nexc_ast::BinaryOp::Sub => "-",
                nexc_ast::BinaryOp::Mul => "*",
                nexc_ast::BinaryOp::Div => "/",
                nexc_ast::BinaryOp::Mod => "%",
                nexc_ast::BinaryOp::EqEq => "==",
                nexc_ast::BinaryOp::NotEq => "!=",
                nexc_ast::BinaryOp::Lt => "<",
                nexc_ast::BinaryOp::LtEq => "<=",
                nexc_ast::BinaryOp::Gt => ">",
                nexc_ast::BinaryOp::GtEq => ">=",
                nexc_ast::BinaryOp::And => "&&",
                nexc_ast::BinaryOp::Or => "||",
            };
            format!("{} {op_str} {}", format_expr(lhs), format_expr(rhs))
        }
        nexc_ast::Expr::Unary { op, expr, .. } => {
            let op_str = match op {
                nexc_ast::UnaryOp::Not => "!",
                nexc_ast::UnaryOp::Neg => "-",
            };
            format!("{op_str}{}", format_expr(expr))
        }
        nexc_ast::Expr::Assign { target, value, op, .. } => {
            let op_str = match op {
                nexc_ast::AssignOp::Assign => "=",
                nexc_ast::AssignOp::AddAssign => "+=",
                nexc_ast::AssignOp::SubAssign => "-=",
                nexc_ast::AssignOp::MulAssign => "*=",
                nexc_ast::AssignOp::DivAssign => "/=",
            };
            format!("{} {op_str} {}", format_expr(target), format_expr(value))
        }
        nexc_ast::Expr::Call { callee, args, .. } => {
            let arg_strs: Vec<String> = args.iter().map(format_expr).collect();
            format!("{}({})", format_expr(callee), arg_strs.join(", "))
        }
        nexc_ast::Expr::MemberAccess { receiver, name, qualifier, .. } => {
            if let Some(q) = qualifier {
                format!("{}::{}({})", q, name, format_expr(receiver))
            } else {
                format!("{}.{}", format_expr(receiver), name)
            }
        }
        nexc_ast::Expr::Block(_) => "{ ... }".into(),
        nexc_ast::Expr::Unsupported { raw, .. } => raw.clone(),
    }
}

fn format_type_expr(ty: &nexc_ast::TypeExpr) -> String {
    match &ty.kind {
        nexc_ast::TypeExprKind::Named(n) => n.clone(),
        nexc_ast::TypeExprKind::Var => "Var".into(),
        nexc_ast::TypeExprKind::Unit => "Unit".into(),
        nexc_ast::TypeExprKind::Nullable(inner) => format!("{}?", format_type_expr(inner)),
        nexc_ast::TypeExprKind::Function(params, ret) => {
            let ps: Vec<String> = params.iter().map(format_type_expr).collect();
            format!("({}) -> {}", ps.join(", "), format_type_expr(ret))
        }
    }
}

fn visibility_prefix(vis: nexc_ast::Visibility) -> &'static str {
    match vis {
        nexc_ast::Visibility::Public => "public ",
        nexc_ast::Visibility::Internal => "",
    }
}
