use std::collections::HashSet;

use nexc_ast::*;
use nexc_diag::{Diagnostic, Severity, Span};

pub fn lint_all(file: &SourceFile) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    out.extend(lint_public_api(file));
    out.extend(lint_unused_imports(file));
    out.extend(lint_unreachable(file));
    out.extend(lint_ambiguous_member(file));
    out
}

pub fn lint_public_api(file: &SourceFile) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    for item in &file.items {
        match item {
            Item::Function(f) if f.is_public => {
                if has_dynamic_return_or_arg(f) && !has_annotation(&f.attributes, "allow_var_api") {
                    out.push(Diagnostic {
                        id: "lint_public_var_api".into(),
                        severity: Severity::Warning,
                        span: Some(f.span),
                        file: Some(file.path.clone().into()),
                        message: format!("public function `{}` uses Var in signature", f.name),
                        notes: Vec::new(),
                        suggestions: vec!["add @allow_var_api annotation".into()],
                    });
                }
            }
            Item::Variable(var) if is_public_dynamic_var(var) => {
                out.push(Diagnostic {
                    id: "lint_public_var_api".into(),
                    severity: Severity::Warning,
                    span: Some(var.span),
                    file: Some(file.path.clone().into()),
                    message: format!("public variable `{}` uses `var`", var.name),
                    notes: Vec::new(),
                    suggestions: vec!["add @allow_var_api annotation".into()],
                });
            }
            Item::Class(c) => {
                for method in &c.methods {
                    if method.is_public
                        && has_dynamic_return_or_arg(method)
                        && !has_annotation(&method.attributes, "allow_var_api")
                    {
                        out.push(Diagnostic {
                            id: "lint_public_var_api".into(),
                            severity: Severity::Warning,
                            span: Some(method.span),
                            file: Some(file.path.clone().into()),
                            message: format!(
                                "public method `{}::{}` uses Var in signature",
                                c.name, method.name
                            ),
                            notes: Vec::new(),
                            suggestions: vec!["add @allow_var_api annotation".into()],
                        });
                    }
                }
                for field in &c.fields {
                    if matches!(field.visibility, Visibility::Public) {
                        if let Some(ty) = &field.ty {
                            if matches!(ty.kind, TypeExprKind::Var) {
                                out.push(Diagnostic {
                                    id: "lint_public_var_api".into(),
                                    severity: Severity::Warning,
                                    span: Some(field.span),
                                    file: Some(file.path.clone().into()),
                                    message: format!(
                                        "public field `{}::{}` is of type Var",
                                        c.name, field.name
                                    ),
                                    notes: Vec::new(),
                                    suggestions: vec!["add @allow_var_api annotation".into()],
                                });
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    out
}

pub fn lint_unused_imports(file: &SourceFile) -> Vec<Diagnostic> {
    let mut imported_names: Vec<(String, Span)> = Vec::new();
    let mut used_names: HashSet<String> = HashSet::new();

    for item in &file.items {
        match item {
            Item::Import(imp) => {
                match &imp.kind {
                    ImportKind::Module => {
                        let name = imp.alias.clone()
                            .unwrap_or_else(|| imp.path.last().cloned().unwrap_or_default());
                        imported_names.push((name, imp.span));
                    }
                    ImportKind::From(names) => {
                        for name in names {
                            imported_names.push((name.clone(), imp.span));
                        }
                    }
                }
            }
            _ => {
                collect_used_names(item, &mut used_names);
            }
        }
    }

    let mut out = Vec::new();
    for (name, span) in &imported_names {
        if !used_names.contains(name) {
            out.push(Diagnostic {
                id: "lint_unused_import".into(),
                severity: Severity::Warning,
                span: Some(*span),
                file: Some(file.path.clone().into()),
                message: format!("unused import `{name}`"),
                notes: Vec::new(),
                suggestions: vec!["remove the import or use it".into()],
            });
        }
    }
    out
}

fn collect_used_names(item: &Item, used: &mut HashSet<String>) {
    match item {
        Item::Function(f) => {
            if let Some(body) = &f.body {
                collect_used_in_expr(body, used);
            }
            collect_used_in_type_refs(f, used);
        }
        Item::Class(c) => {
            for base in &c.base_specs {
                used.insert(base.name.clone());
            }
            for method in &c.methods {
                if let Some(body) = &method.body {
                    collect_used_in_expr(body, used);
                }
            }
        }
        Item::Struct(s) => {
            for iface in &s.interfaces {
                used.insert(iface.clone());
            }
            for method in &s.methods {
                if let Some(body) = &method.body {
                    collect_used_in_expr(body, used);
                }
            }
        }
        Item::Variable(v) => {
            if let Some(ty) = &v.explicit_type {
                collect_used_in_type_expr(ty, used);
            }
        }
        _ => {}
    }
}

fn collect_used_in_type_refs(f: &FunctionDecl, used: &mut HashSet<String>) {
    for p in &f.params {
        if let Some(ty) = &p.type_hint {
            collect_used_in_type_expr(ty, used);
        }
    }
    if let Some(ty) = &f.return_type {
        collect_used_in_type_expr(ty, used);
    }
}

fn collect_used_in_type_expr(ty: &TypeExpr, used: &mut HashSet<String>) {
    match &ty.kind {
        TypeExprKind::Named(n) => { used.insert(n.clone()); }
        TypeExprKind::Generic(base, args) => {
            used.insert(base.clone());
            for a in args { collect_used_in_type_expr(a, used); }
        }
        TypeExprKind::Nullable(inner) => collect_used_in_type_expr(inner, used),
        TypeExprKind::Function(params, ret) => {
            for p in params { collect_used_in_type_expr(p, used); }
            collect_used_in_type_expr(ret, used);
        }
        TypeExprKind::Var | TypeExprKind::Unit => {}
    }
}

fn collect_used_in_expr(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Identifier { name, .. } => { used.insert(name.clone()); }
        Expr::Binary { lhs, rhs, .. } => {
            collect_used_in_expr(lhs, used);
            collect_used_in_expr(rhs, used);
        }
        Expr::Unary { expr, .. } => collect_used_in_expr(expr, used),
        Expr::Assign { target, value, .. } => {
            collect_used_in_expr(target, used);
            collect_used_in_expr(value, used);
        }
        Expr::Call { callee, args, .. } => {
            collect_used_in_expr(callee, used);
            for a in args { collect_used_in_expr(a, used); }
        }
        Expr::MemberAccess { receiver, qualifier, .. } => {
            collect_used_in_expr(receiver, used);
            if let Some(q) = qualifier { used.insert(q.clone()); }
        }
        Expr::Block(block) => {
            for stmt in &block.statements {
                collect_used_in_stmt(stmt, used);
            }
        }
        _ => {}
    }
}

fn collect_used_in_stmt(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Expr(e) => collect_used_in_expr(e, used),
        Stmt::Return(Some(e), _) | Stmt::Throw(e, _) => collect_used_in_expr(e, used),
        Stmt::If(i) => {
            collect_used_in_expr(&i.condition, used);
            collect_used_in_stmt(&i.then_branch, used);
            if let Some(el) = &i.else_branch { collect_used_in_stmt(el, used); }
        }
        Stmt::While(w) => {
            collect_used_in_expr(&w.condition, used);
            collect_used_in_stmt(&w.body, used);
        }
        Stmt::For(f) => {
            if let Some(init) = &f.init { collect_used_in_expr(init, used); }
            if let Some(cond) = &f.condition { collect_used_in_expr(cond, used); }
            if let Some(step) = &f.step { collect_used_in_expr(step, used); }
            collect_used_in_stmt(&f.body, used);
        }
        Stmt::Try(t) => {
            for s in &t.body.statements { collect_used_in_stmt(s, used); }
            for c in &t.catches { for s in &c.body.statements { collect_used_in_stmt(s, used); } }
            if let Some(fin) = &t.finally { for s in &fin.statements { collect_used_in_stmt(s, used); } }
        }
        Stmt::Using(u) => {
            collect_used_in_expr(&u.expr, used);
            for s in &u.body.statements { collect_used_in_stmt(s, used); }
        }
        Stmt::Block(b) => { for s in &b.statements { collect_used_in_stmt(s, used); } }
        _ => {}
    }
}

pub fn lint_unreachable(file: &SourceFile) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    for item in &file.items {
        if let Item::Function(f) = item {
            if let Some(Expr::Block(block)) = &f.body {
                check_block_unreachable(&block.statements, &file.path, &mut out);
            }
        }
        if let Item::Class(c) = item {
            for method in &c.methods {
                if let Some(Expr::Block(block)) = &method.body {
                    check_block_unreachable(&block.statements, &file.path, &mut out);
                }
            }
        }
    }
    out
}

fn check_block_unreachable(stmts: &[Stmt], file: &str, out: &mut Vec<Diagnostic>) {
    let mut found_terminal = false;
    for stmt in stmts {
        if found_terminal {
            let span = stmt_span(stmt);
            out.push(Diagnostic {
                id: "lint_unreachable_code".into(),
                severity: Severity::Warning,
                span,
                file: Some(file.into()),
                message: "unreachable code after return/throw/break/continue".into(),
                notes: Vec::new(),
                suggestions: vec!["remove unreachable code".into()],
            });
            break;
        }
        match stmt {
            Stmt::Return(_, _) | Stmt::Throw(_, _) | Stmt::Break(_) | Stmt::Continue(_) => {
                found_terminal = true;
            }
            _ => {}
        }
    }
}

fn stmt_span(stmt: &Stmt) -> Option<Span> {
    match stmt {
        Stmt::Return(_, s) | Stmt::Throw(_, s) | Stmt::Continue(s) | Stmt::Break(s) => Some(*s),
        Stmt::If(i) => Some(i.span),
        Stmt::While(w) => Some(w.span),
        Stmt::For(f) => Some(f.span),
        Stmt::Try(t) => Some(t.body.span),
        Stmt::Using(u) => Some(u.span),
        _ => None,
    }
}

pub fn lint_ambiguous_member(file: &SourceFile) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    for item in &file.items {
        if let Item::Class(c) = item {
            if c.base_specs.len() >= 2 {
                let base_method_names: Vec<HashSet<String>> = c.base_specs.iter().map(|base| {
                    find_class_methods(file, &base.name)
                }).collect();

                for i in 0..base_method_names.len() {
                    for j in (i + 1)..base_method_names.len() {
                        let overlap: Vec<_> = base_method_names[i]
                            .intersection(&base_method_names[j])
                            .cloned()
                            .collect();
                        let overridden: HashSet<_> = c.methods.iter().map(|m| m.name.clone()).collect();
                        for name in overlap {
                            if !overridden.contains(&name) {
                                out.push(Diagnostic {
                                    id: "lint_ambiguous_member".into(),
                                    severity: Severity::Warning,
                                    span: Some(c.span),
                                    file: Some(file.path.clone().into()),
                                    message: format!(
                                        "method `{name}` is inherited from both `{}` and `{}`; override or alias to resolve",
                                        c.base_specs[i].name, c.base_specs[j].name
                                    ),
                                    notes: Vec::new(),
                                    suggestions: vec![
                                        format!("override `{name}` in `{}`", c.name),
                                        format!("use alias: `alias {name}From{} = {}::{name}`", c.base_specs[i].name, c.base_specs[i].name),
                                    ],
                                });
                            }
                        }
                    }
                }
            }
        }
    }
    out
}

fn find_class_methods(file: &SourceFile, class_name: &str) -> HashSet<String> {
    for item in &file.items {
        if let Item::Class(c) = item {
            if c.name == class_name {
                return c.methods.iter().map(|m| m.name.clone()).collect();
            }
        }
    }
    HashSet::new()
}

fn has_dynamic_return_or_arg(function: &FunctionDecl) -> bool {
    if function.return_type.as_ref().is_some_and(|t| matches!(t.kind, TypeExprKind::Var)) {
        return true;
    }
    function.params.iter().any(|p| {
        p.type_hint.as_ref().is_some_and(|t| matches!(t.kind, TypeExprKind::Var))
    })
}

fn is_public_dynamic_var(var: &VarDecl) -> bool {
    matches!(var.explicit_type.as_ref(), Some(t) if matches!(t.kind, TypeExprKind::Var))
        || matches!(var.inferred_type.as_ref(), Some(t) if matches!(t.kind, TypeExprKind::Var))
        || (var.is_dynamic && var.visibility == Visibility::Public)
}

fn has_annotation(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|a| a.name == name)
}
