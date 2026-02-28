use std::collections::HashMap;

use nexc_ast::*;
use nexc_diag::{Diagnostic, DiagnosticSink, Severity};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Invalid,
    Unit,
    Bool,
    Byte,
    Int,
    Int64,
    Float,
    Double,
    Char,
    String,
    Var,
    Null,
    Named(String),
    /// Generic type instantiation, e.g. `List<Int>` → `Generic("List", [Int])`.
    /// At runtime this is erased to the base type (type erasure, like Java).
    Generic(String, Vec<Type>),
    Nullable(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Enum(String),
    Unknown,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Byte | Type::Int | Type::Int64 | Type::Float | Type::Double
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Byte | Type::Int | Type::Int64)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float | Type::Double)
    }

    pub fn display_name(&self) -> String {
        match self {
            Type::Invalid => "<invalid>".into(),
            Type::Unit => "Unit".into(),
            Type::Bool => "Bool".into(),
            Type::Byte => "Byte".into(),
            Type::Int => "Int".into(),
            Type::Int64 => "Int64".into(),
            Type::Float => "Float".into(),
            Type::Double => "Double".into(),
            Type::Char => "Char".into(),
            Type::String => "String".into(),
            Type::Var => "Var".into(),
            Type::Null => "Null".into(),
            Type::Named(n) => n.clone(),
            Type::Generic(base, args) => {
                let params: Vec<_> = args.iter().map(|t| t.display_name()).collect();
                format!("{}<{}>", base, params.join(", "))
            }
            Type::Nullable(inner) => format!("{}?", inner.display_name()),
            Type::Function(params, ret) => {
                let p: Vec<_> = params.iter().map(|t| t.display_name()).collect();
                format!("({}) -> {}", p.join(", "), ret.display_name())
            }
            Type::Enum(n) => n.clone(),
            Type::Unknown => "<unknown>".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub expr: Expr,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub decl: FunctionDecl,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedClass {
    pub decl: ClassDecl,
}

#[derive(Debug, Clone)]
pub struct TypedModule {
    pub file: SourceFile,
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, FunctionSig>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
}

pub fn declare_types(file: &SourceFile, sink: &mut DiagnosticSink) -> TypedModule {
    let mut module = TypedModule {
        file: file.clone(),
        types: HashMap::new(),
        functions: HashMap::new(),
        diagnostics: Vec::new(),
    };
    seed_builtin_types(&mut module.types);
    inject_implicit_object_base(&mut module.file);

    for item in &module.file.items {
        match item {
            Item::Variable(var) if var.is_dynamic => {
                module.types.insert(var.name.clone(), Type::Var);
            }
            Item::Variable(var) => {
                module.types.insert(var.name.clone(), Type::Unknown);
            }
            Item::Class(c) => {
                module
                    .types
                    .insert(c.name.clone(), Type::Named(c.name.clone()));
            }
            Item::Struct(s) => {
                module
                    .types
                    .insert(s.name.clone(), Type::Named(s.name.clone()));
            }
            Item::Interface(i) => {
                module
                    .types
                    .insert(i.name.clone(), Type::Named(i.name.clone()));
            }
            Item::Enum(e) => {
                module
                    .types
                    .insert(e.name.clone(), Type::Enum(e.name.clone()));
            }
            Item::Function(f) => {
                let params: Vec<(String, Type)> = f
                    .params
                    .iter()
                    .map(|p| {
                        let ty = p
                            .type_hint
                            .as_ref()
                            .map(|t| resolve_type_expr(t))
                            .unwrap_or(Type::Unknown);
                        (p.name.clone(), ty)
                    })
                    .collect();
                let ret = f
                    .return_type
                    .as_ref()
                    .map(|t| resolve_type_expr(t))
                    .unwrap_or(Type::Unit);
                module.functions.insert(
                    f.name.clone(),
                    FunctionSig {
                        params,
                        return_type: ret,
                    },
                );
            }
            // Bare module-level assignments like `fps_timer = 0.0f` are parsed as
            // Item::Statement(Assign) instead of Item::Variable.  Infer the type
            // from the initializer literal so the codegen can correctly distinguish
            // float/int/string/bool globals.
            Item::Statement(Stmt::Expr(Expr::Assign { target, value, .. })) => {
                if let Expr::Identifier { name, .. } = target.as_ref() {
                    let ty = infer_literal_type(value);
                    module.types.insert(name.clone(), ty);
                }
            }
            _ => {}
        }
    }
    for d in &module.diagnostics {
        sink.push(d.clone());
    }
    module
}

/// Infer a type from an expression's outermost literal value.
/// Returns `Type::Unknown` for non-literal or compound expressions.
fn infer_literal_type(expr: &Expr) -> Type {
    match expr {
        Expr::Literal { value, .. } => match value {
            Literal::Float(_) => Type::Float,
            Literal::Int(_) => Type::Int64,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::String,
            _ => Type::Unknown,
        },
        // Negative literal: `-0.5f` is Unary { op: Neg, expr: Literal::Float }
        Expr::Unary { op: UnaryOp::Neg, expr, .. } => infer_literal_type(expr),
        _ => Type::Unknown,
    }
}

fn resolve_type_expr(ty: &TypeExpr) -> Type {
    match &ty.kind {
        TypeExprKind::Named(name) => match name.as_str() {
            "Bool" => Type::Bool,
            "Byte" => Type::Byte,
            "Int" => Type::Int,
            "Int64" => Type::Int64,
            "Float" => Type::Float,
            "Double" => Type::Double,
            "Char" => Type::Char,
            "String" => Type::String,
            "Object" => Type::Named("Object".into()),
            other => Type::Named(other.into()),
        },
        TypeExprKind::Generic(base, args) => {
            let resolved_args: Vec<Type> = args.iter().map(|a| resolve_type_expr(a)).collect();
            Type::Generic(base.clone(), resolved_args)
        }
        TypeExprKind::Var => Type::Var,
        TypeExprKind::Unit => Type::Unit,
        TypeExprKind::Nullable(inner) => Type::Nullable(Box::new(resolve_type_expr(inner))),
        TypeExprKind::Function(params, ret) => {
            let ps: Vec<Type> = params.iter().map(|p| resolve_type_expr(p)).collect();
            Type::Function(ps, Box::new(resolve_type_expr(ret)))
        }
    }
}

fn seed_builtin_types(types: &mut HashMap<String, Type>) {
    types.insert("Object".into(), Type::Named("Object".into()));
    types.insert("String".into(), Type::String);
    types.insert("Var".into(), Type::Var);
    types.insert("Unit".into(), Type::Unit);
    types.insert("Int".into(), Type::Int);
    types.insert("Bool".into(), Type::Bool);
    types.insert("Byte".into(), Type::Byte);
    types.insert("Int64".into(), Type::Int64);
    types.insert("Float".into(), Type::Float);
    types.insert("Double".into(), Type::Double);
    types.insert("Char".into(), Type::Char);
    types.insert("List".into(), Type::Named("List".into()));
    types.insert("Tensor".into(), Type::Named("Tensor".into()));
    types.insert("Module".into(), Type::Named("Module".into()));
    types.insert("Optimizer".into(), Type::Named("Optimizer".into()));
}

fn inject_implicit_object_base(file: &mut SourceFile) {
    for item in &mut file.items {
        if let Item::Class(class) = item {
            if class.name != "Object" && class.base_specs.is_empty() {
                class.base_specs.push(BaseSpec {
                    name: "Object".into(),
                    shared: false,
                    ctor_args: Vec::new(),
                    span: class.span,
                });
            }
        }
    }
}

pub fn check_bodies(typed: &mut TypedModule, sink: &mut DiagnosticSink) {
    let file_path = typed.file.path.clone();
    let global_types = typed.types.clone();
    let global_fns = typed.functions.clone();

    for item in &typed.file.items {
        match item {
            Item::Function(func) => {
                check_function(func, &global_types, &global_fns, &file_path, sink);
            }
            Item::Class(class) => {
                for method in &class.methods {
                    let mut local_types = global_types.clone();
                    local_types.insert("self".into(), Type::Named(class.name.clone()));
                    for field in &class.fields {
                        let ty = field
                            .ty
                            .as_ref()
                            .map(|t| resolve_type_expr(t))
                            .unwrap_or(Type::Unknown);
                        local_types.insert(field.name.clone(), ty);
                    }
                    check_function(method, &local_types, &global_fns, &file_path, sink);
                }
            }
            Item::Struct(s) => {
                for method in &s.methods {
                    let mut local_types = global_types.clone();
                    local_types.insert("self".into(), Type::Named(s.name.clone()));
                    for field in &s.fields {
                        let ty = field
                            .ty
                            .as_ref()
                            .map(|t| resolve_type_expr(t))
                            .unwrap_or(Type::Unknown);
                        local_types.insert(field.name.clone(), ty);
                    }
                    check_function(method, &local_types, &global_fns, &file_path, sink);
                }
            }
            _ => {}
        }
    }
}

fn check_function(
    func: &FunctionDecl,
    global_types: &HashMap<String, Type>,
    global_fns: &HashMap<String, FunctionSig>,
    file_path: &str,
    sink: &mut DiagnosticSink,
) {
    let expected_return = func
        .return_type
        .as_ref()
        .map(|t| resolve_type_expr(t))
        .unwrap_or(Type::Unit);

    let mut scope = Scope {
        variables: HashMap::new(),
        global_types: global_types.clone(),
        global_fns: global_fns.clone(),
        expected_return: expected_return.clone(),
        file_path: file_path.into(),
    };

    for param in &func.params {
        let ty = param
            .type_hint
            .as_ref()
            .map(|t| resolve_type_expr(t))
            .unwrap_or(Type::Unknown);
        scope.variables.insert(param.name.clone(), ty);
    }

    if let Some(body) = &func.body {
        if let Expr::Block(block) = body {
            check_block(block, &mut scope, sink);
        }
    }
}

struct Scope {
    variables: HashMap<String, Type>,
    global_types: HashMap<String, Type>,
    global_fns: HashMap<String, FunctionSig>,
    expected_return: Type,
    file_path: String,
}

fn check_block(block: &Block, scope: &mut Scope, sink: &mut DiagnosticSink) {
    for stmt in &block.statements {
        check_stmt(stmt, scope, sink);
    }
}

fn check_stmt(stmt: &Stmt, scope: &mut Scope, sink: &mut DiagnosticSink) {
    match stmt {
        Stmt::Expr(expr) => {
            infer_expr(expr, scope, sink);
        }
        Stmt::Return(maybe_expr, span) => {
            let ret_ty = match maybe_expr {
                Some(expr) => infer_expr(expr, scope, sink),
                None => Type::Unit,
            };
            if scope.expected_return != Type::Unknown
                && scope.expected_return != Type::Var
                && ret_ty != Type::Unknown
                && ret_ty != Type::Var
                && !types_compatible(&ret_ty, &scope.expected_return)
            {
                sink.push(Diagnostic {
                    id: "type_return_mismatch".into(),
                    severity: Severity::Error,
                    span: Some(*span),
                    file: Some(scope.file_path.clone().into()),
                    message: format!(
                        "return type mismatch: expected `{}`, got `{}`",
                        scope.expected_return.display_name(),
                        ret_ty.display_name()
                    ),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
        }
        Stmt::Throw(expr, _) => {
            infer_expr(expr, scope, sink);
        }
        Stmt::VarDecl(var) => {
            let ty = if var.is_dynamic {
                Type::Var
            } else if let Some(explicit) = &var.explicit_type {
                resolve_type_expr(explicit)
            } else {
                Type::Unknown
            };
            scope.variables.insert(var.name.clone(), ty);
        }
        Stmt::If(if_stmt) => {
            let cond_ty = infer_expr(&if_stmt.condition, scope, sink);
            if cond_ty != Type::Unknown && cond_ty != Type::Bool && cond_ty != Type::Var {
                sink.push(Diagnostic {
                    id: "type_condition_not_bool".into(),
                    severity: Severity::Error,
                    span: Some(if_stmt.span),
                    file: Some(scope.file_path.clone().into()),
                    message: format!("condition must be Bool, got `{}`", cond_ty.display_name()),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
            check_stmt(&if_stmt.then_branch, scope, sink);
            if let Some(else_branch) = &if_stmt.else_branch {
                check_stmt(else_branch, scope, sink);
            }
        }
        Stmt::While(while_stmt) => {
            let cond_ty = infer_expr(&while_stmt.condition, scope, sink);
            if cond_ty != Type::Unknown && cond_ty != Type::Bool && cond_ty != Type::Var {
                sink.push(Diagnostic {
                    id: "type_condition_not_bool".into(),
                    severity: Severity::Error,
                    span: Some(while_stmt.span),
                    file: Some(scope.file_path.clone().into()),
                    message: format!("condition must be Bool, got `{}`", cond_ty.display_name()),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
            check_stmt(&while_stmt.body, scope, sink);
        }
        Stmt::For(for_stmt) => {
            if let Some((var_name, iterable)) = &for_stmt.for_each {
                let iter_ty = infer_expr(iterable, scope, sink);
                // Infer element type from generic type parameter
                let elem_ty = match &iter_ty {
                    Type::Generic(_, args) if !args.is_empty() => args[0].clone(),
                    _ => Type::Unknown,
                };
                scope.variables.insert(var_name.clone(), elem_ty);
                check_stmt(&for_stmt.body, scope, sink);
            } else {
                if let Some(init) = &for_stmt.init {
                    infer_expr(init, scope, sink);
                }
                if let Some(cond) = &for_stmt.condition {
                    infer_expr(cond, scope, sink);
                }
                if let Some(step) = &for_stmt.step {
                    infer_expr(step, scope, sink);
                }
                check_stmt(&for_stmt.body, scope, sink);
            }
        }
        Stmt::Try(try_stmt) => {
            check_block(&try_stmt.body, scope, sink);
            for catch in &try_stmt.catches {
                check_block(&catch.body, scope, sink);
            }
            if let Some(finally) = &try_stmt.finally {
                check_block(finally, scope, sink);
            }
        }
        Stmt::Using(using) => {
            infer_expr(&using.expr, scope, sink);
            check_block(&using.body, scope, sink);
        }
        Stmt::Block(block) => {
            check_block(block, scope, sink);
        }
        Stmt::Continue(_) | Stmt::Break(_) => {}
    }
}

fn infer_expr(expr: &Expr, scope: &mut Scope, sink: &mut DiagnosticSink) -> Type {
    match expr {
        Expr::Literal { value, .. } => match value {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Double,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::String,
            Literal::Null => Type::Null,
        },
        Expr::Identifier { name, .. } => {
            if let Some(ty) = scope.variables.get(name) {
                return ty.clone();
            }
            if let Some(ty) = scope.global_types.get(name) {
                return ty.clone();
            }
            if scope.global_fns.contains_key(name) {
                return Type::Unknown;
            }
            Type::Unknown
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            let lt = infer_expr(lhs, scope, sink);
            let rt = infer_expr(rhs, scope, sink);

            if lt == Type::Var || rt == Type::Var {
                return Type::Var;
            }

            match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                    if (lt == Type::String || rt == Type::String) && matches!(op, BinaryOp::Add) {
                        return Type::String;
                    }
                    if lt.is_numeric() && rt.is_numeric() {
                        return numeric_promotion(&lt, &rt);
                    }
                    if lt != Type::Unknown && rt != Type::Unknown {
                        return Type::Unknown;
                    }
                    Type::Unknown
                }
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq => Type::Bool,
                BinaryOp::And | BinaryOp::Or => Type::Bool,
                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor
                | BinaryOp::Shl | BinaryOp::Shr => Type::Int,
            }
        }
        Expr::Unary {
            op, expr: inner, ..
        } => {
            let inner_ty = infer_expr(inner, scope, sink);
            match op {
                UnaryOp::Not => Type::Bool,
                UnaryOp::Neg => {
                    if inner_ty.is_numeric() {
                        inner_ty
                    } else {
                        Type::Unknown
                    }
                }
                UnaryOp::BitNot => Type::Int,
            }
        }
        Expr::Assign {
            target,
            value,
            op,
            span,
        } => {
            let val_ty = infer_expr(value, scope, sink);
            if let Expr::Identifier { name, .. } = target.as_ref() {
                if !scope.variables.contains_key(name)
                    && !scope.global_types.contains_key(name)
                    && matches!(op, AssignOp::Assign)
                {
                    scope.variables.insert(name.clone(), val_ty.clone());
                } else if let Some(existing) = scope.variables.get(name) {
                    if *existing != Type::Unknown
                        && *existing != Type::Var
                        && val_ty != Type::Unknown
                        && val_ty != Type::Var
                        && !types_compatible(&val_ty, existing)
                    {
                        sink.push(Diagnostic {
                            id: "type_assign_mismatch".into(),
                            severity: Severity::Error,
                            span: Some(*span),
                            file: Some(scope.file_path.clone().into()),
                            message: format!(
                                "cannot assign `{}` to variable of type `{}`",
                                val_ty.display_name(),
                                existing.display_name()
                            ),
                            notes: Vec::new(),
                            suggestions: Vec::new(),
                        });
                    }
                }
            }
            val_ty
        }
        Expr::Call { callee, args, type_args, .. } => {
            for arg in args {
                infer_expr(arg, scope, sink);
            }
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                if let Some(sig) = scope.global_fns.get(name) {
                    return sig.return_type.clone();
                }
                if scope.global_types.contains_key(name) {
                    if !type_args.is_empty() {
                        let resolved_args: Vec<Type> = type_args.iter().map(|t| resolve_type_expr(t)).collect();
                        return Type::Generic(name.clone(), resolved_args);
                    }
                    return Type::Named(name.clone());
                }
            }
            Type::Unknown
        }
        Expr::MemberAccess { receiver, name, .. } => {
            let recv_ty = infer_expr(receiver, scope, sink);
            if recv_ty == Type::String && name == "length" {
                return Type::Int;
            }
            Type::Unknown
        }
        Expr::Block(block) => {
            check_block(block, scope, sink);
            Type::Unit
        }
        Expr::Await { expr, .. } => {
            let inner_ty = infer_expr(expr, scope, sink);
            // Await unwraps a Task<T> to T; for now accept Unknown
            match inner_ty {
                Type::Unknown | Type::Var => inner_ty,
                _ => Type::Unknown,
            }
        }
        Expr::Lambda { params, return_type, body, .. } => {
            let param_types: Vec<Type> = params
                .iter()
                .map(|p| {
                    p.type_hint
                        .as_ref()
                        .map(|t| resolve_type_expr(t))
                        .unwrap_or(Type::Unknown)
                })
                .collect();
            // Register lambda params in scope for body type inference
            for (p, ty) in params.iter().zip(param_types.iter()) {
                scope.variables.insert(p.name.clone(), ty.clone());
            }
            let body_ty = infer_expr(body, scope, sink);
            let ret_ty = return_type
                .as_ref()
                .map(|t| resolve_type_expr(t))
                .unwrap_or(body_ty);
            Type::Function(param_types, Box::new(ret_ty))
        }
        Expr::StringInterp { parts, .. } => {
            for part in parts {
                if let nexc_ast::StringInterpPart::Expr(e) = part {
                    infer_expr(e, scope, sink);
                }
            }
            Type::String
        }
        Expr::Ternary { condition, then_expr, else_expr, .. } => {
            infer_expr(condition, scope, sink);
            let then_ty = infer_expr(then_expr, scope, sink);
            let _else_ty = infer_expr(else_expr, scope, sink);
            then_ty
        }
        Expr::Match { scrutinee, arms, .. } => {
            infer_expr(scrutinee, scope, sink);
            let mut result_ty = Type::Unknown;
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    infer_expr(guard, scope, sink);
                }
                let arm_ty = infer_expr(&arm.body, scope, sink);
                if result_ty == Type::Unknown {
                    result_ty = arm_ty;
                }
            }
            result_ty
        }
        Expr::Range { .. } => Type::Unknown,
        Expr::ArrayLiteral { elements, .. } => {
            for el in elements {
                infer_expr(el, scope, sink);
            }
            Type::Unknown
        }
        Expr::Unsupported { .. } => Type::Unknown,
    }
}

fn numeric_promotion(a: &Type, b: &Type) -> Type {
    if *a == Type::Double || *b == Type::Double {
        Type::Double
    } else if *a == Type::Float || *b == Type::Float {
        Type::Float
    } else if *a == Type::Int64 || *b == Type::Int64 {
        Type::Int64
    } else {
        Type::Int
    }
}

fn types_compatible(source: &Type, target: &Type) -> bool {
    if source == target {
        return true;
    }
    if *source == Type::Null {
        return matches!(target, Type::Nullable(_) | Type::Named(_) | Type::String);
    }
    if let Type::Nullable(inner) = target {
        return types_compatible(source, inner);
    }
    if source.is_integer() && target.is_integer() {
        return true;
    }
    if source.is_float() && target.is_float() {
        return true;
    }
    // Generic type erasure: Named("List") is compatible with Generic("List", [...])
    match (source, target) {
        (Type::Named(a), Type::Generic(b, _)) | (Type::Generic(a, _), Type::Named(b)) if a == b => {
            return true;
        }
        _ => {}
    }
    false
}

pub fn validate_inheritance(typed: &TypedModule, sink: &mut DiagnosticSink) {
    let mut class_names = HashMap::new();
    let mut interface_names = HashMap::new();
    let mut struct_names = HashMap::new();

    for item in &typed.file.items {
        match item {
            Item::Class(class) => {
                class_names.insert(class.name.clone(), class);
            }
            Item::Interface(interface) => {
                interface_names.insert(interface.name.clone(), interface);
            }
            Item::Struct(strukt) => {
                struct_names.insert(strukt.name.clone(), strukt);
            }
            _ => {}
        }
    }

    for class in class_names.values() {
        if class.name == "Object" && !class.base_specs.is_empty() {
            sink.push(Diagnostic {
                id: "type_object_has_base".into(),
                severity: Severity::Error,
                span: Some(class.span),
                file: Some(typed.file.path.clone().into()),
                message: "Object must not declare a base class".into(),
                notes: Vec::new(),
                suggestions: Vec::new(),
            });
        }

        for base in &class.base_specs {
            if base.name == class.name {
                sink.push(Diagnostic {
                    id: "type_inheritance_cycle".into(),
                    severity: Severity::Error,
                    span: Some(base.span),
                    file: Some(typed.file.path.clone().into()),
                    message: format!("class `{}` cannot inherit from itself", class.name),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
                continue;
            }

            if struct_names.contains_key(&base.name) {
                sink.push(Diagnostic {
                    id: "type_class_base_value".into(),
                    severity: Severity::Error,
                    span: Some(base.span),
                    file: Some(typed.file.path.clone().into()),
                    message: format!(
                        "class `{}` cannot inherit value type `{}`",
                        class.name, base.name
                    ),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
                continue;
            }

            if !class_names.contains_key(&base.name)
                && !interface_names.contains_key(&base.name)
                && base.name != "Object"
            {
                sink.push(Diagnostic {
                    id: "type_unknown_base".into(),
                    severity: Severity::Error,
                    span: Some(base.span),
                    file: Some(typed.file.path.clone().into()),
                    message: format!("unknown base type `{}`", base.name),
                    notes: Vec::new(),
                    suggestions: Vec::new(),
                });
            }
        }
    }

    let mut state = HashMap::<String, u8>::new();
    let mut stack = Vec::<String>::new();
    for class in class_names.keys() {
        if state.get(class).copied().unwrap_or(0) == 0 {
            validate_inheritance_dfs(
                class,
                &class_names,
                &mut state,
                &mut stack,
                sink,
                &typed.file.path,
            );
        }
    }
}

fn validate_inheritance_dfs(
    class: &str,
    class_names: &HashMap<String, &ClassDecl>,
    state: &mut HashMap<String, u8>,
    stack: &mut Vec<String>,
    sink: &mut DiagnosticSink,
    file: &str,
) {
    state.insert(class.into(), 1);
    stack.push(class.into());

    if let Some(decl) = class_names.get(class) {
        for base in &decl.base_specs {
            if !class_names.contains_key(&base.name) {
                continue;
            }
            match state.get(&base.name).copied().unwrap_or(0) {
                0 => validate_inheritance_dfs(&base.name, class_names, state, stack, sink, file),
                1 => {
                    let mut cycle = Vec::new();
                    if let Some(index) = stack.iter().position(|name| name == &base.name) {
                        cycle.extend(stack[index..].iter().cloned());
                    }
                    cycle.push(base.name.clone());
                    sink.push(Diagnostic {
                        id: "type_inheritance_cycle".into(),
                        severity: Severity::Error,
                        span: Some(base.span),
                        file: Some(file.into()),
                        message: format!("inheritance cycle detected: {}", cycle.join(" -> ")),
                        notes: Vec::new(),
                        suggestions: Vec::new(),
                    });
                }
                _ => {}
            }
        }
    }

    stack.pop();
    state.insert(class.into(), 2);
}

pub fn is_static_type(type_name: &TypeExprKind) -> bool {
    !matches!(type_name, TypeExprKind::Var)
}

pub fn infer_var_type(_name: &str, _expr: &Expr) -> Type {
    Type::Unknown
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexc_diag::DiagnosticSink;
    use nexc_lex::{asi_normalize, lex};
    use nexc_parse::Parser;

    fn parse_source(source: &str) -> SourceFile {
        let mut sink = DiagnosticSink::new();
        let tokens = lex(source, Some("types.nex".into()), &mut sink);
        let tokens = asi_normalize(&tokens);
        let mut parser = Parser::new(&tokens, "types.nex".into());
        let file = parser.parse();
        assert!(sink.is_empty(), "{:?}", sink.diagnostics());
        assert!(
            parser.diagnostics().is_empty(),
            "{:?}",
            parser.diagnostics()
        );
        file
    }

    #[test]
    fn injects_object_base_for_classes_without_explicit_base() {
        let file = parse_source("class Demo { value: Int }");
        let mut sink = DiagnosticSink::new();
        let typed = declare_types(&file, &mut sink);
        let class = typed
            .file
            .items
            .iter()
            .find_map(|item| match item {
                Item::Class(class) => Some(class),
                _ => None,
            })
            .expect("class expected");
        assert_eq!(class.base_specs.len(), 1);
        assert_eq!(class.base_specs[0].name, "Object");
    }

    #[test]
    fn does_not_inject_base_for_object_root() {
        let file = parse_source("class Object { }");
        let mut sink = DiagnosticSink::new();
        let typed = declare_types(&file, &mut sink);
        let class = typed
            .file
            .items
            .iter()
            .find_map(|item| match item {
                Item::Class(class) => Some(class),
                _ => None,
            })
            .expect("class expected");
        assert!(class.base_specs.is_empty());
    }

    #[test]
    fn detects_class_inheritance_cycle() {
        let file = parse_source("class A : B { } class B : A { }");
        let mut sink = DiagnosticSink::new();
        let typed = declare_types(&file, &mut sink);
        validate_inheritance(&typed, &mut sink);
        assert!(sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "type_inheritance_cycle"));
    }

    #[test]
    fn rejects_value_type_as_class_base() {
        let file = parse_source("struct S { x: Int } class C : S { }");
        let mut sink = DiagnosticSink::new();
        let typed = declare_types(&file, &mut sink);
        validate_inheritance(&typed, &mut sink);
        assert!(sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "type_class_base_value"));
    }

    #[test]
    fn checks_return_type_basic() {
        let file = parse_source("def foo() -> Int { return 42 }");
        let mut sink = DiagnosticSink::new();
        let mut typed = declare_types(&file, &mut sink);
        check_bodies(&mut typed, &mut sink);
        assert!(!sink.has_errors());
    }

    #[test]
    fn allows_override_without_virtual_keyword() {
        let file = parse_source(
            "class Base { virtual def f() -> Int { return 1 } } class Derived : Base { override def f() -> Int { return 2 } }",
        );
        let mut sink = DiagnosticSink::new();
        let mut typed = declare_types(&file, &mut sink);
        check_bodies(&mut typed, &mut sink);
        assert!(!sink
            .diagnostics()
            .iter()
            .any(|d| d.id == "type_override_non_virtual"));
    }
}
