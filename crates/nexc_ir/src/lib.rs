use std::collections::HashMap;

use std::path::PathBuf;

use nexc_diag::{DiagnosticSink, Span};
use nexc_layout::ClassLayout;
use nexc_type::{Type, TypedModule};

#[derive(Debug, Clone)]
pub enum IrValue {
    IntConst(i64),
    FloatConst(f64),
    BoolConst(bool),
    StringConst(String),
    NullConst,
    Register(String),
}

#[derive(Debug, Clone)]
pub enum IrInstruction {
    Nop,
    Return(Option<IrValue>),
    Allocate {
        dst: String,
        ty: String,
    },
    Store {
        dst: String,
        src: IrValue,
    },
    Load {
        dst: String,
        src: String,
    },
    Call {
        dst: Option<String>,
        target: String,
        args: Vec<IrValue>,
    },
    VCall {
        dst: Option<String>,
        slot: u32,
        this_ptr: String,
        args: Vec<IrValue>,
    },
    BinOp {
        dst: String,
        op: String,
        lhs: IrValue,
        rhs: IrValue,
    },
    UnaryOp {
        dst: String,
        op: String,
        operand: IrValue,
    },
    Branch {
        cond: IrValue,
        then_label: String,
        else_label: String,
    },
    Jump {
        target: String,
    },
    Print {
        value: IrValue,
    },
    MemberAccess {
        dst: String,
        receiver: IrValue,
        field: String,
    },
    EmitDiag {
        message: String,
    },
}

#[derive(Debug, Clone)]
pub struct IrBlock {
    pub label: String,
    pub instructions: Vec<IrInstruction>,
}

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub return_type: Type,
    pub blocks: Vec<IrBlock>,
    pub span: Option<Span>,
    pub file: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct IrModule {
    pub name: String,
    pub functions: Vec<IrFunction>,
    pub types: HashMap<String, Type>,
    pub layouts: HashMap<String, ClassLayout>,
    /// Module-level variable names (with `%` prefix) shared across all functions.
    pub globals: Vec<String>,
}

struct IrLowering {
    temp_counter: u32,
    current_block: Vec<IrInstruction>,
    blocks: Vec<IrBlock>,
    block_counter: u32,
    pending_label: Option<String>,
    current_class: Option<String>,
    known_classes: Vec<String>,
    /// Variable name → class/type name for type tracking
    var_types: HashMap<String, String>,
    /// class_name → [(op_symbol, method_name)]
    class_operators: HashMap<String, Vec<(String, String)>>,
    /// (class_name, method_name) → return_type_name
    class_method_returns: HashMap<(String, String), String>,
    /// (class_name, field_name) → type_name
    class_fields: HashMap<(String, String), String>,
    /// (type_name, op_symbol) → ffi_function_name for builtin type operators
    builtin_operators: HashMap<(String, String), String>,
    /// (type_name, method_name) → (ffi_function_name, returns_value)
    builtin_methods: HashMap<(String, String), (String, bool)>,
}

impl IrLowering {
    fn new() -> Self {
        Self {
            temp_counter: 0,
            current_block: Vec::new(),
            blocks: Vec::new(),
            block_counter: 0,
            pending_label: None,
            current_class: None,
            known_classes: Vec::new(),
            var_types: HashMap::new(),
            class_operators: HashMap::new(),
            class_method_returns: HashMap::new(),
            class_fields: HashMap::new(),
            builtin_operators: HashMap::new(),
            builtin_methods: HashMap::new(),
        }
    }

    fn fresh_temp(&mut self) -> String {
        self.temp_counter += 1;
        format!("%t{}", self.temp_counter)
    }

    fn fresh_label(&mut self) -> String {
        self.block_counter += 1;
        format!("bb{}", self.block_counter)
    }

    fn emit(&mut self, inst: IrInstruction) {
        self.current_block.push(inst);
    }

    fn block_terminated(&self) -> bool {
        matches!(
            self.current_block.last(),
            Some(IrInstruction::Return(_))
                | Some(IrInstruction::Jump { .. })
                | Some(IrInstruction::Branch { .. })
        )
    }

    fn seal_block(&mut self, label: &str) {
        let actual_label = self
            .pending_label
            .take()
            .unwrap_or_else(|| label.to_string());
        let block = IrBlock {
            label: actual_label,
            instructions: std::mem::take(&mut self.current_block),
        };
        self.blocks.push(block);
    }

    fn lower_function(&mut self, func: &nexc_ast::FunctionDecl) -> IrFunction {
        self.blocks.clear();
        self.current_block.clear();
        self.temp_counter = 0;
        self.block_counter = 0;
        self.pending_label = None;

        // Track parameter types for operator overloading and method resolution
        for p in &func.params {
            if let Some(ty) = &p.type_hint {
                match &ty.kind {
                    nexc_ast::TypeExprKind::Named(name) => {
                        self.var_types.insert(p.name.clone(), name.clone());
                    }
                    nexc_ast::TypeExprKind::Generic(base, _) => {
                        self.var_types.insert(p.name.clone(), base.clone());
                    }
                    _ => {}
                }
            }
        }

        let params: Vec<(String, String)> = func
            .params
            .iter()
            .map(|p| {
                let ty_str = p
                    .type_hint
                    .as_ref()
                    .map(|t| format!("{:?}", t.kind))
                    .unwrap_or_else(|| "Unknown".into());
                (p.name.clone(), ty_str)
            })
            .collect();

        let ret = func
            .return_type
            .as_ref()
            .map(|t| type_expr_to_type(t))
            .unwrap_or(Type::Unit);

        if let Some(body) = &func.body {
            if let nexc_ast::Expr::Block(block) = body {
                self.lower_block(block);
            }
        }

        if !self.current_block.is_empty() || self.blocks.is_empty() {
            if !self.block_terminated() {
                self.emit(IrInstruction::Return(None));
            }
            self.seal_block("entry");
        }

        IrFunction {
            name: func.name.clone(),
            params,
            return_type: ret,
            blocks: std::mem::take(&mut self.blocks),
            span: Some(func.span),
            file: None,
        }
    }

    /// Resolve an unqualified method name to a class-qualified IR function name.
    /// If `qualifier` is given (e.g. from `Base::method` or `self.method` inside
    /// a class), use it directly.  Otherwise search known classes for a match.
    fn resolve_call_target(&self, name: &str, qualifier: Option<&str>) -> String {
        if let Some(q) = qualifier {
            return format!("{q}::{name}");
        }
        // Bare function call -- check if it matches a known class method.
        for class in &self.known_classes {
            let candidate = format!("{class}::{name}");
            // We don't have the full function list here, so we can't verify.
            // Prefer the unqualified name; codegen will resolve it.
            let _ = candidate;
        }
        name.to_string()
    }

    /// Try to resolve the type name of an expression from tracked variable types,
    /// class fields, and method return types.
    fn resolve_expr_type(&self, expr: &nexc_ast::Expr) -> Option<String> {
        match expr {
            nexc_ast::Expr::Identifier { name, .. } => {
                if name == "self" {
                    return self.current_class.clone();
                }
                self.var_types.get(name).cloned()
            }
            nexc_ast::Expr::Call { callee, .. } => match callee.as_ref() {
                nexc_ast::Expr::Identifier { name, .. } => {
                    if self.known_classes.contains(name) || name == "List" {
                        return Some(name.clone());
                    }
                    // Infer return types for well-known free functions.
                    // tensor_* → Tensor, nn_* (except nn_free) → Module
                    if name.starts_with("tensor_") {
                        return Some("Tensor".into());
                    }
                    if name.starts_with("nn_") && name != "nn_free" {
                        return Some("Module".into());
                    }
                    None
                }
                nexc_ast::Expr::MemberAccess {
                    receiver,
                    name: method,
                    ..
                } => {
                    if let Some(recv_type) = self.resolve_expr_type(receiver) {
                        if let Some(ret_type) = self
                            .class_method_returns
                            .get(&(recv_type, method.clone()))
                        {
                            return Some(ret_type.clone());
                        }
                    }
                    None
                }
                _ => None,
            },
            nexc_ast::Expr::MemberAccess {
                receiver, name, ..
            } => {
                if let Some(recv_type) = self.resolve_expr_type(receiver) {
                    if let Some(field_type) =
                        self.class_fields.get(&(recv_type, name.clone()))
                    {
                        return Some(field_type.clone());
                    }
                }
                None
            }
            nexc_ast::Expr::Binary { lhs, .. } => {
                // If lhs has a known builtin type (e.g. Tensor), the binary result
                // is the same type — enables chaining like (a - b).abs()
                self.resolve_expr_type(lhs)
            }
            _ => None,
        }
    }

    fn lower_stmt_to_instructions(&mut self, stmt: &nexc_ast::Stmt) -> Vec<IrInstruction> {
        let saved = std::mem::take(&mut self.current_block);
        self.lower_stmt(stmt);
        let result = std::mem::replace(&mut self.current_block, saved);
        result
    }

    fn lower_block(&mut self, block: &nexc_ast::Block) {
        for stmt in &block.statements {
            self.lower_stmt(stmt);
        }
    }

    fn lower_stmt(&mut self, stmt: &nexc_ast::Stmt) {
        use nexc_ast::Stmt;
        match stmt {
            Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            Stmt::Return(Some(expr), _) => {
                let val = self.lower_expr(expr);
                self.emit(IrInstruction::Return(Some(val)));
            }
            Stmt::Return(None, _) => {
                self.emit(IrInstruction::Return(None));
            }
            Stmt::VarDecl(var) => {
                let dst = format!("%{}", var.name);
                self.emit(IrInstruction::Allocate {
                    dst,
                    ty: "auto".into(),
                });
                // Track variable type from explicit annotation
                if let Some(ty_expr) = &var.explicit_type {
                    match &ty_expr.kind {
                        nexc_ast::TypeExprKind::Named(name) => {
                            self.var_types.insert(var.name.clone(), name.clone());
                        }
                        nexc_ast::TypeExprKind::Generic(base, _) => {
                            self.var_types.insert(var.name.clone(), base.clone());
                        }
                        _ => {}
                    }
                } else if let Some(init) = &var.initializer {
                    // Infer type from constructor call or expression type
                    if let Some(ty_name) = self.resolve_expr_type(init) {
                        self.var_types.insert(var.name.clone(), ty_name);
                    }
                }
                if let Some(init) = &var.initializer {
                    let val = self.lower_expr(init);
                    self.emit(IrInstruction::Store {
                        dst: format!("%{}", var.name),
                        src: val,
                    });
                }
            }
            Stmt::If(if_stmt) => {
                let cond = self.lower_expr(&if_stmt.condition);
                let then_label = self.fresh_label();
                let else_label = self.fresh_label();
                let merge_label = self.fresh_label();

                self.emit(IrInstruction::Branch {
                    cond,
                    then_label: then_label.clone(),
                    else_label: else_label.clone(),
                });
                let pre_if_label = self.fresh_label();
                self.seal_block(&pre_if_label);

                self.lower_stmt(&if_stmt.then_branch);
                let then_terminated = self.block_terminated();
                if !then_terminated {
                    self.emit(IrInstruction::Jump {
                        target: merge_label.clone(),
                    });
                }
                self.seal_block(&then_label);

                if let Some(else_branch) = &if_stmt.else_branch {
                    self.lower_stmt(else_branch);
                }
                let else_terminated = self.block_terminated();
                if !else_terminated {
                    self.emit(IrInstruction::Jump {
                        target: merge_label.clone(),
                    });
                }
                self.seal_block(&else_label);

                if !then_terminated || !else_terminated {
                    self.pending_label = Some(merge_label);
                }
            }
            Stmt::While(while_stmt) => {
                let header = self.fresh_label();
                let body_label = self.fresh_label();
                let exit_label = self.fresh_label();

                self.emit(IrInstruction::Jump {
                    target: header.clone(),
                });
                let pre_while_label = self.fresh_label();
                self.seal_block(&pre_while_label);

                let cond = self.lower_expr(&while_stmt.condition);
                self.emit(IrInstruction::Branch {
                    cond,
                    then_label: body_label.clone(),
                    else_label: exit_label.clone(),
                });
                self.seal_block(&header);

                self.lower_stmt(&while_stmt.body);
                self.emit(IrInstruction::Jump { target: header });
                self.seal_block(&body_label);

                self.pending_label = Some(exit_label);
            }
            Stmt::For(for_stmt) => {
                if let Some((var_name, iterable)) = &for_stmt.for_each {
                    // Desugar: for (x in items) { body }
                    // →  var __iter = items
                    //    var __i = 0
                    //    while (__i < __iter.length()) {
                    //        var x = __iter.get(__i)
                    //        body
                    //        __i = __i + 1
                    //    }
                    let iter_reg = self.fresh_temp();
                    let idx_reg = self.fresh_temp();
                    let iter_val = self.lower_expr(iterable);
                    self.emit(IrInstruction::Store {
                        dst: iter_reg.clone(),
                        src: iter_val,
                    });
                    self.emit(IrInstruction::Store {
                        dst: idx_reg.clone(),
                        src: IrValue::IntConst(0),
                    });

                    let header = self.fresh_label();
                    let body_label = self.fresh_label();
                    let exit_label = self.fresh_label();

                    self.emit(IrInstruction::Jump {
                        target: header.clone(),
                    });
                    let pre_label = self.fresh_label();
                    self.seal_block(&pre_label);

                    // Condition: __i < __iter.length()
                    let len_dst = self.fresh_temp();
                    self.emit(IrInstruction::Call {
                        dst: Some(len_dst.clone()),
                        target: "nex_list_length".into(),
                        args: vec![IrValue::Register(iter_reg.clone())],
                    });
                    let cond_dst = self.fresh_temp();
                    self.emit(IrInstruction::BinOp {
                        dst: cond_dst.clone(),
                        op: "lt".into(),
                        lhs: IrValue::Register(idx_reg.clone()),
                        rhs: IrValue::Register(len_dst),
                    });
                    self.emit(IrInstruction::Branch {
                        cond: IrValue::Register(cond_dst),
                        then_label: body_label.clone(),
                        else_label: exit_label.clone(),
                    });
                    self.seal_block(&header);

                    // Body: var x = __iter.get(__i)
                    let elem_dst = self.fresh_temp();
                    self.emit(IrInstruction::Call {
                        dst: Some(elem_dst.clone()),
                        target: "nex_list_get".into(),
                        args: vec![
                            IrValue::Register(iter_reg.clone()),
                            IrValue::Register(idx_reg.clone()),
                        ],
                    });
                    self.emit(IrInstruction::Store {
                        dst: format!("%{var_name}"),
                        src: IrValue::Register(elem_dst),
                    });

                    self.lower_stmt(&for_stmt.body);

                    // Step: __i = __i + 1
                    let next_idx = self.fresh_temp();
                    self.emit(IrInstruction::BinOp {
                        dst: next_idx.clone(),
                        op: "add".into(),
                        lhs: IrValue::Register(idx_reg.clone()),
                        rhs: IrValue::IntConst(1),
                    });
                    self.emit(IrInstruction::Store {
                        dst: idx_reg,
                        src: IrValue::Register(next_idx),
                    });
                    self.emit(IrInstruction::Jump { target: header });
                    self.seal_block(&body_label);

                    self.pending_label = Some(exit_label);
                } else {
                    // Traditional C-style for loop
                    if let Some(init) = &for_stmt.init {
                        self.lower_expr(init);
                    }
                    let header = self.fresh_label();
                    let body_label = self.fresh_label();
                    let exit_label = self.fresh_label();

                    self.emit(IrInstruction::Jump {
                        target: header.clone(),
                    });
                    let pre_for_label = self.fresh_label();
                    self.seal_block(&pre_for_label);

                    if let Some(cond) = &for_stmt.condition {
                        let cv = self.lower_expr(cond);
                        self.emit(IrInstruction::Branch {
                            cond: cv,
                            then_label: body_label.clone(),
                            else_label: exit_label.clone(),
                        });
                    } else {
                        self.emit(IrInstruction::Jump {
                            target: body_label.clone(),
                        });
                    }
                    self.seal_block(&header);

                    self.lower_stmt(&for_stmt.body);
                    if let Some(step) = &for_stmt.step {
                        self.lower_expr(step);
                    }
                    self.emit(IrInstruction::Jump { target: header });
                    self.seal_block(&body_label);

                    self.pending_label = Some(exit_label);
                }
            }
            Stmt::Throw(expr, _) => {
                let val = self.lower_expr(expr);
                self.emit(IrInstruction::Call {
                    dst: None,
                    target: "nex_throw".into(),
                    args: vec![val],
                });
            }
            Stmt::Try(try_stmt) => {
                let finally_label = self.fresh_label();
                let merge_label = self.fresh_label();

                self.lower_block(&try_stmt.body);
                if !self.block_terminated() {
                    self.emit(IrInstruction::Jump {
                        target: finally_label.clone(),
                    });
                }
                let try_body_label = self.fresh_label();
                self.seal_block(&try_body_label);

                // v1: catch blocks are skipped (no runtime exception propagation yet).
                // Each catch block jumps to finally so that if exception handling
                // is wired up later the control flow is correct.
                for catch in &try_stmt.catches {
                    self.lower_block(&catch.body);
                    if !self.block_terminated() {
                        self.emit(IrInstruction::Jump {
                            target: finally_label.clone(),
                        });
                    }
                    let catch_label = self.fresh_label();
                    self.seal_block(&catch_label);
                }

                self.pending_label = Some(finally_label);
                if let Some(finally) = &try_stmt.finally {
                    self.lower_block(finally);
                }
                if !self.block_terminated() {
                    self.emit(IrInstruction::Jump {
                        target: merge_label.clone(),
                    });
                }
                let fin_block_label = self.fresh_label();
                self.seal_block(&fin_block_label);

                self.pending_label = Some(merge_label);
            }
            Stmt::Using(using) => {
                let res = self.lower_expr(&using.expr);
                let dst = format!("%{}", using.variable_name);
                self.emit(IrInstruction::Store { dst, src: res });
                self.lower_block(&using.body);
                self.emit(IrInstruction::Call {
                    dst: None,
                    target: "dispose".into(),
                    args: vec![IrValue::Register(format!("%{}", using.variable_name))],
                });
            }
            Stmt::Block(block) => {
                self.lower_block(block);
            }
            Stmt::Continue(_) | Stmt::Break(_) => {
                self.emit(IrInstruction::Nop);
            }
        }
    }

    fn lower_expr(&mut self, expr: &nexc_ast::Expr) -> IrValue {
        use nexc_ast::Expr;
        match expr {
            Expr::Literal { value, .. } => match value {
                nexc_ast::Literal::Int(v) => IrValue::IntConst(*v),
                nexc_ast::Literal::Float(v) => IrValue::FloatConst(*v),
                nexc_ast::Literal::Bool(v) => IrValue::BoolConst(*v),
                nexc_ast::Literal::String(v) => IrValue::StringConst(v.clone()),
                nexc_ast::Literal::Char(c) => IrValue::IntConst(*c as i64),
                nexc_ast::Literal::Null => IrValue::NullConst,
            },
            Expr::Identifier { name, .. } => IrValue::Register(format!("%{name}")),
            Expr::Binary { op, lhs, rhs, .. } => {
                // Check for operator overloading on class/struct types
                let op_symbol = match op {
                    nexc_ast::BinaryOp::Add => Some("+"),
                    nexc_ast::BinaryOp::Sub => Some("-"),
                    nexc_ast::BinaryOp::Mul => Some("*"),
                    nexc_ast::BinaryOp::Div => Some("/"),
                    _ => None,
                };
                if let Some(sym) = op_symbol {
                    if let Some(class_name) = self.resolve_expr_type(lhs) {
                        // Check builtin type operators first (Tensor +, -, *, /)
                        if let Some(ffi) = self
                            .builtin_operators
                            .get(&(class_name.clone(), sym.to_string()))
                            .cloned()
                        {
                            let l = self.lower_expr(lhs);
                            let r = self.lower_expr(rhs);
                            let dst = self.fresh_temp();
                            self.emit(IrInstruction::Call {
                                dst: Some(dst.clone()),
                                target: ffi,
                                args: vec![l, r],
                            });
                            return IrValue::Register(dst);
                        }
                        // Then check class-defined operators (from current file)
                        let has_op = self
                            .class_operators
                            .get(&class_name)
                            .map_or(false, |ops| ops.iter().any(|(s, _)| s == sym));
                        if has_op {
                            let l = self.lower_expr(lhs);
                            let r = self.lower_expr(rhs);
                            let dst = self.fresh_temp();
                            self.emit(IrInstruction::Call {
                                dst: Some(dst.clone()),
                                target: format!("{class_name}::operator{sym}"),
                                args: vec![l, r],
                            });
                            return IrValue::Register(dst);
                        }
                    }
                }
                // Primitive binary operation
                let l = self.lower_expr(lhs);
                let r = self.lower_expr(rhs);
                let dst = self.fresh_temp();
                let op_str = match op {
                    nexc_ast::BinaryOp::Add => "add",
                    nexc_ast::BinaryOp::Sub => "sub",
                    nexc_ast::BinaryOp::Mul => "mul",
                    nexc_ast::BinaryOp::Div => "div",
                    nexc_ast::BinaryOp::Mod => "mod",
                    nexc_ast::BinaryOp::EqEq => "eq",
                    nexc_ast::BinaryOp::NotEq => "ne",
                    nexc_ast::BinaryOp::Lt => "lt",
                    nexc_ast::BinaryOp::LtEq => "le",
                    nexc_ast::BinaryOp::Gt => "gt",
                    nexc_ast::BinaryOp::GtEq => "ge",
                    nexc_ast::BinaryOp::And => "and",
                    nexc_ast::BinaryOp::Or => "or",
                };
                self.emit(IrInstruction::BinOp {
                    dst: dst.clone(),
                    op: op_str.into(),
                    lhs: l,
                    rhs: r,
                });
                IrValue::Register(dst)
            }
            Expr::Unary {
                op, expr: inner, ..
            } => {
                let operand = self.lower_expr(inner);
                let dst = self.fresh_temp();
                let op_str = match op {
                    nexc_ast::UnaryOp::Not => "not",
                    nexc_ast::UnaryOp::Neg => "neg",
                };
                self.emit(IrInstruction::UnaryOp {
                    dst: dst.clone(),
                    op: op_str.into(),
                    operand,
                });
                IrValue::Register(dst)
            }
            Expr::Assign { target, value, .. } => {
                // Track type from assignment for operator overloading
                if let Expr::Identifier { name, .. } = target.as_ref() {
                    if let Some(class_name) = self.resolve_expr_type(value) {
                        self.var_types.insert(name.clone(), class_name);
                    }
                }
                let val = self.lower_expr(value);
                match target.as_ref() {
                    Expr::Identifier { name, .. } => {
                        self.emit(IrInstruction::Store {
                            dst: format!("%{name}"),
                            src: val.clone(),
                        });
                    }
                    Expr::MemberAccess {
                        receiver,
                        name: field,
                        ..
                    } => {
                        // self.field = value → Store to %ClassName.field
                        if let Some(class_name) = self.resolve_expr_type(receiver) {
                            self.emit(IrInstruction::Store {
                                dst: format!("%{class_name}.{field}"),
                                src: val.clone(),
                            });
                        }
                    }
                    _ => {}
                }
                val
            }
            Expr::Call { callee, args, .. } => {
                // Check for List constructor: List() or List<T>()
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if name == "List" {
                        let dst = self.fresh_temp();
                        self.emit(IrInstruction::Call {
                            dst: Some(dst.clone()),
                            target: "nex_list_new".into(),
                            args: vec![],
                        });
                        return IrValue::Register(dst);
                    }
                }

                // Check for class/struct constructor: MyClass(args) → MyClass::init(args)
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if self.known_classes.contains(name) {
                        let ir_args: Vec<IrValue> =
                            args.iter().map(|a| self.lower_expr(a)).collect();
                        let dst = self.fresh_temp();
                        self.emit(IrInstruction::Call {
                            dst: Some(dst.clone()),
                            target: format!("{}::init", name),
                            args: ir_args,
                        });
                        return IrValue::Register(dst);
                    }
                }

                // Check for method calls on instances with known types
                if let Expr::MemberAccess {
                    receiver,
                    name: method_name,
                    ..
                } = callee.as_ref()
                {
                    if let Some(recv_type) = self.resolve_expr_type(receiver) {
                        // Check builtin methods (List, Tensor, Module, Optimizer)
                        let key = (recv_type.clone(), method_name.clone());
                        if let Some((ffi, returns)) =
                            self.builtin_methods.get(&key).cloned()
                        {
                            let recv_val = self.lower_expr(receiver);
                            let mut ir_args = vec![recv_val];
                            ir_args.extend(args.iter().map(|a| self.lower_expr(a)));
                            let dst = self.fresh_temp();
                            if returns {
                                self.emit(IrInstruction::Call {
                                    dst: Some(dst.clone()),
                                    target: ffi,
                                    args: ir_args,
                                });
                                return IrValue::Register(dst);
                            } else {
                                self.emit(IrInstruction::Call {
                                    dst: None,
                                    target: ffi,
                                    args: ir_args,
                                });
                                return IrValue::NullConst;
                            }
                        }
                        // Fallback: check List FFI methods
                        if recv_type == "List" {
                            if let Some(ffi) = list_method_to_ffi(method_name) {
                                let recv_val = self.lower_expr(receiver);
                                let mut ir_args = vec![recv_val];
                                ir_args.extend(args.iter().map(|a| self.lower_expr(a)));
                                let dst = self.fresh_temp();
                                let returns_value = matches!(
                                    method_name.as_str(),
                                    "get" | "length" | "remove"
                                );
                                if returns_value {
                                    self.emit(IrInstruction::Call {
                                        dst: Some(dst.clone()),
                                        target: ffi.into(),
                                        args: ir_args,
                                    });
                                    return IrValue::Register(dst);
                                } else {
                                    self.emit(IrInstruction::Call {
                                        dst: None,
                                        target: ffi.into(),
                                        args: ir_args,
                                    });
                                    return IrValue::NullConst;
                                }
                            }
                        }
                    }
                }

                // Default call lowering.
                let ir_args: Vec<IrValue> =
                    args.iter().map(|a| self.lower_expr(a)).collect();

                let dst = self.fresh_temp();
                let target = match callee.as_ref() {
                    Expr::Identifier { name, .. } => {
                        self.resolve_call_target(name, None)
                    }
                    Expr::MemberAccess {
                        receiver,
                        name,
                        qualifier,
                        ..
                    } => {
                        let qual = qualifier.as_deref().or_else(|| {
                            if matches!(receiver.as_ref(), Expr::Identifier { name, .. } if name == "self") {
                                self.current_class.as_deref()
                            } else if let nexc_ast::Expr::Identifier { name, .. } =
                                receiver.as_ref()
                            {
                                self.var_types.get(name).map(|s| s.as_str())
                            } else {
                                None
                            }
                        });
                        self.resolve_call_target(name, qual)
                    }
                    _ => "unknown_callee".into(),
                };
                self.emit(IrInstruction::Call {
                    dst: Some(dst.clone()),
                    target,
                    args: ir_args,
                });
                IrValue::Register(dst)
            }
            Expr::MemberAccess { receiver, name, .. } => {
                // If the receiver has a known type and the field exists in
                // class_fields, emit a Load from the namespaced global
                // (e.g. self.tok_emb → Load %GPT.tok_emb).
                if let Some(class_name) = self.resolve_expr_type(receiver) {
                    if self
                        .class_fields
                        .contains_key(&(class_name.clone(), name.clone()))
                    {
                        let dst = self.fresh_temp();
                        self.emit(IrInstruction::Load {
                            dst: dst.clone(),
                            src: format!("%{class_name}.{name}"),
                        });
                        return IrValue::Register(dst);
                    }
                }
                // Fallback: existing MemberAccess IR (for non-field access)
                let recv = self.lower_expr(receiver);
                let dst = self.fresh_temp();
                self.emit(IrInstruction::MemberAccess {
                    dst: dst.clone(),
                    receiver: recv,
                    field: name.clone(),
                });
                IrValue::Register(dst)
            }
            Expr::Block(block) => {
                self.lower_block(block);
                IrValue::NullConst
            }
            Expr::Unsupported { raw, .. } => {
                self.emit(IrInstruction::EmitDiag {
                    message: format!("unsupported expression: {raw}"),
                });
                IrValue::NullConst
            }
        }
    }
}

fn seed_builtin_dispatch(lowering: &mut IrLowering) {
    let ops = &mut lowering.builtin_operators;
    let methods = &mut lowering.builtin_methods;
    let returns = &mut lowering.class_method_returns;

    // ── Tensor operators ────────────────────────────────────────────────
    ops.insert(("Tensor".into(), "+".into()), "tensor_add".into());
    ops.insert(("Tensor".into(), "-".into()), "tensor_sub".into());
    ops.insert(("Tensor".into(), "*".into()), "tensor_mul".into());
    ops.insert(("Tensor".into(), "/".into()), "tensor_div".into());

    // ── Tensor methods ──────────────────────────────────────────────────
    // Linear algebra
    methods.insert(("Tensor".into(), "matmul".into()), ("tensor_matmul".into(), true));
    methods.insert(("Tensor".into(), "transpose".into()), ("tensor_transpose".into(), true));
    // Activation / reduction
    methods.insert(("Tensor".into(), "softmax".into()), ("tensor_softmax".into(), true));
    methods.insert(("Tensor".into(), "mean".into()), ("tensor_mean".into(), true));
    methods.insert(("Tensor".into(), "sum".into()), ("tensor_sum".into(), true));
    methods.insert(("Tensor".into(), "argmax".into()), ("tensor_argmax".into(), true));
    // Shape
    methods.insert(("Tensor".into(), "narrow".into()), ("tensor_narrow".into(), true));
    methods.insert(("Tensor".into(), "reshape".into()), ("tensor_reshape".into(), true));
    methods.insert(("Tensor".into(), "view".into()), ("tensor_reshape".into(), true));
    methods.insert(("Tensor".into(), "unsqueeze".into()), ("tensor_unsqueeze".into(), true));
    methods.insert(("Tensor".into(), "squeeze".into()), ("tensor_squeeze".into(), true));
    methods.insert(("Tensor".into(), "flatten".into()), ("tensor_flatten".into(), true));
    methods.insert(("Tensor".into(), "shape_dim".into()), ("tensor_shape_dim".into(), true));
    methods.insert(("Tensor".into(), "contiguous".into()), ("tensor_contiguous".into(), true));
    // Scalar ops
    methods.insert(("Tensor".into(), "add_scalar".into()), ("tensor_add_scalar".into(), true));
    methods.insert(("Tensor".into(), "mul_scalar".into()), ("tensor_mul_scalar".into(), true));
    methods.insert(("Tensor".into(), "div_scalar".into()), ("tensor_div_scalar".into(), true));
    methods.insert(("Tensor".into(), "pow_scalar".into()), ("tensor_pow_scalar".into(), true));
    // Masking
    methods.insert(("Tensor".into(), "masked_fill".into()), ("tensor_masked_fill".into(), true));
    methods.insert(("Tensor".into(), "tril".into()), ("tensor_tril".into(), true));
    methods.insert(("Tensor".into(), "triu".into()), ("tensor_triu".into(), true));
    // Math
    methods.insert(("Tensor".into(), "exp".into()), ("tensor_exp".into(), true));
    methods.insert(("Tensor".into(), "log".into()), ("tensor_log".into(), true));
    methods.insert(("Tensor".into(), "sqrt".into()), ("tensor_sqrt".into(), true));
    methods.insert(("Tensor".into(), "neg".into()), ("tensor_neg".into(), true));
    methods.insert(("Tensor".into(), "abs".into()), ("tensor_abs".into(), true));
    // Comparison
    methods.insert(("Tensor".into(), "gt_scalar".into()), ("tensor_gt_scalar".into(), true));
    methods.insert(("Tensor".into(), "lt_scalar".into()), ("tensor_lt_scalar".into(), true));
    methods.insert(("Tensor".into(), "eq_scalar".into()), ("tensor_eq_scalar".into(), true));
    // Autograd
    methods.insert(("Tensor".into(), "backward".into()), ("tensor_backward".into(), false));
    methods.insert(("Tensor".into(), "requires_grad".into()), ("tensor_requires_grad".into(), true));
    methods.insert(("Tensor".into(), "grad".into()), ("tensor_grad".into(), true));
    methods.insert(("Tensor".into(), "detach".into()), ("tensor_detach".into(), true));
    // Data access
    methods.insert(("Tensor".into(), "item".into()), ("tensor_item_float".into(), true));
    methods.insert(("Tensor".into(), "to_string".into()), ("tensor_to_string".into(), true));
    methods.insert(("Tensor".into(), "print".into()), ("tensor_print".into(), false));
    // Device / dtype
    methods.insert(("Tensor".into(), "to_device".into()), ("tensor_to_device".into(), true));
    methods.insert(("Tensor".into(), "to_long".into()), ("tensor_to_dtype_long".into(), true));
    methods.insert(("Tensor".into(), "to_float".into()), ("tensor_to_dtype_float".into(), true));
    methods.insert(("Tensor".into(), "clone".into()), ("tensor_clone".into(), true));
    // Concat / indexing
    methods.insert(("Tensor".into(), "cat".into()), ("tensor_cat".into(), true));
    methods.insert(("Tensor".into(), "index_select".into()), ("tensor_index_select".into(), true));
    // Loss (as Tensor method for convenience)
    methods.insert(("Tensor".into(), "cross_entropy".into()), ("loss_cross_entropy".into(), true));
    methods.insert(("Tensor".into(), "mse_loss".into()), ("loss_mse".into(), true));

    // Tensor method return types (for chaining type resolution)
    for m in &[
        "matmul", "transpose", "softmax", "mean", "sum", "narrow", "reshape",
        "view", "unsqueeze", "squeeze", "flatten", "contiguous", "add_scalar",
        "mul_scalar", "div_scalar", "pow_scalar", "masked_fill", "tril", "triu",
        "exp", "log", "sqrt", "neg", "abs", "requires_grad", "detach", "clone",
        "to_device", "to_long", "to_float", "cat", "index_select",
        "cross_entropy", "mse_loss", "argmax",
        "gt_scalar", "lt_scalar", "eq_scalar",
    ] {
        returns.insert(("Tensor".into(), m.to_string()), "Tensor".into());
    }

    // ── Module (nn sequential) methods ──────────────────────────────────
    methods.insert(("Module".into(), "forward".into()), ("nn_forward".into(), true));
    methods.insert(("Module".into(), "to_device".into()), ("nn_to_device".into(), false));
    returns.insert(("Module".into(), "forward".into()), "Tensor".into());

    // ── Optimizer methods ───────────────────────────────────────────────
    methods.insert(("Optimizer".into(), "step".into()), ("optim_step".into(), false));
    methods.insert(("Optimizer".into(), "zero_grad".into()), ("optim_zero_grad".into(), false));

    // =====================================================================
    // nex3d struct types — dispatch to pure-Nex free functions
    // =====================================================================

    // ── Vec2 operators ──────────────────────────────────────────────────
    ops.insert(("Vec2".into(), "+".into()), "vec2_add".into());
    ops.insert(("Vec2".into(), "-".into()), "vec2_sub".into());

    // ── Vec2 methods ────────────────────────────────────────────────────
    methods.insert(("Vec2".into(), "mul".into()),       ("vec2_mul".into(), true));
    methods.insert(("Vec2".into(), "dot".into()),       ("vec2_dot".into(), true));
    methods.insert(("Vec2".into(), "length".into()),    ("vec2_length".into(), true));
    methods.insert(("Vec2".into(), "normalize".into()), ("vec2_normalize".into(), true));

    // Vec2 return types
    for m in &["mul", "normalize"] {
        returns.insert(("Vec2".into(), m.to_string()), "Vec2".into());
    }

    // ── Vec3 operators ──────────────────────────────────────────────────
    ops.insert(("Vec3".into(), "+".into()), "vec3_add".into());
    ops.insert(("Vec3".into(), "-".into()), "vec3_sub".into());

    // ── Vec3 methods ────────────────────────────────────────────────────
    methods.insert(("Vec3".into(), "mul".into()),       ("vec3_mul".into(), true));
    methods.insert(("Vec3".into(), "dot".into()),       ("vec3_dot".into(), true));
    methods.insert(("Vec3".into(), "length".into()),    ("vec3_length".into(), true));
    methods.insert(("Vec3".into(), "normalize".into()), ("vec3_normalize".into(), true));
    methods.insert(("Vec3".into(), "cross".into()),     ("vec3_cross".into(), true));
    methods.insert(("Vec3".into(), "negate".into()),    ("vec3_negate".into(), true));
    methods.insert(("Vec3".into(), "lerp".into()),      ("vec3_lerp".into(), true));

    // Vec3 return types
    for m in &["mul", "normalize", "cross", "negate", "lerp"] {
        returns.insert(("Vec3".into(), m.to_string()), "Vec3".into());
    }

    // ── Vec4 operators ──────────────────────────────────────────────────
    ops.insert(("Vec4".into(), "+".into()), "vec4_add".into());
    ops.insert(("Vec4".into(), "-".into()), "vec4_sub".into());

    // ── Vec4 methods ────────────────────────────────────────────────────
    methods.insert(("Vec4".into(), "mul".into()),       ("vec4_mul".into(), true));
    methods.insert(("Vec4".into(), "dot".into()),       ("vec4_dot".into(), true));
    methods.insert(("Vec4".into(), "length".into()),    ("vec4_length".into(), true));
    methods.insert(("Vec4".into(), "normalize".into()), ("vec4_normalize".into(), true));

    // Vec4 return types
    for m in &["mul", "normalize"] {
        returns.insert(("Vec4".into(), m.to_string()), "Vec4".into());
    }

    // ── Mat4 operators ──────────────────────────────────────────────────
    ops.insert(("Mat4".into(), "*".into()), "mat4_multiply".into());

    // ── Mat4 methods ────────────────────────────────────────────────────
    methods.insert(("Mat4".into(), "transpose".into()),  ("mat4_transpose".into(), true));
    methods.insert(("Mat4".into(), "mul_vec4".into()),   ("mat4_mul_vec4".into(), true));

    // Mat4 return types
    returns.insert(("Mat4".into(), "transpose".into()), "Mat4".into());
    returns.insert(("Mat4".into(), "mul_vec4".into()),  "Vec4".into());

    // ── Quat operators ──────────────────────────────────────────────────
    ops.insert(("Quat".into(), "*".into()), "quat_multiply".into());

    // ── Quat methods ────────────────────────────────────────────────────
    methods.insert(("Quat".into(), "normalize".into()),   ("quat_normalize".into(), true));
    methods.insert(("Quat".into(), "rotate_vec3".into()), ("quat_rotate_vec3".into(), true));
    methods.insert(("Quat".into(), "to_mat4".into()),     ("quat_to_mat4".into(), true));
    methods.insert(("Quat".into(), "slerp".into()),       ("quat_slerp".into(), true));

    // Quat return types
    returns.insert(("Quat".into(), "normalize".into()),   "Quat".into());
    returns.insert(("Quat".into(), "rotate_vec3".into()), "Vec3".into());
    returns.insert(("Quat".into(), "to_mat4".into()),     "Mat4".into());
    returns.insert(("Quat".into(), "slerp".into()),       "Quat".into());

    // ── Color methods ───────────────────────────────────────────────────
    methods.insert(("Color".into(), "lerp".into()), ("color_lerp".into(), true));
    methods.insert(("Color".into(), "mul".into()),  ("color_mul_scalar".into(), true));

    // Color return types
    returns.insert(("Color".into(), "lerp".into()), "Color".into());
    returns.insert(("Color".into(), "mul".into()),  "Color".into());

    // ── Camera methods ──────────────────────────────────────────────────
    methods.insert(("Camera".into(), "apply".into()),              ("camera_apply".into(), false));
    methods.insert(("Camera".into(), "apply_perspective".into()),  ("camera_apply_perspective".into(), false));
    methods.insert(("Camera".into(), "view_matrix".into()),        ("camera_view_matrix".into(), true));
    methods.insert(("Camera".into(), "proj_matrix".into()),        ("camera_proj_matrix".into(), true));
    methods.insert(("Camera".into(), "view_proj".into()),          ("camera_view_proj".into(), true));

    // Camera return types
    returns.insert(("Camera".into(), "view_matrix".into()), "Mat4".into());
    returns.insert(("Camera".into(), "proj_matrix".into()), "Mat4".into());
    returns.insert(("Camera".into(), "view_proj".into()),   "Mat4".into());

    // ── Texture2D methods ────────────────────────────────────────────────
    methods.insert(("Texture2D".into(), "bind".into()),   ("texture_bind".into(), false));

    // ── BoundingBox methods ─────────────────────────────────────────────
    methods.insert(("BoundingBox".into(), "center".into()),            ("bbox_center".into(), true));
    methods.insert(("BoundingBox".into(), "extents".into()),           ("bbox_extents".into(), true));
    methods.insert(("BoundingBox".into(), "half_extents".into()),      ("bbox_half_extents".into(), true));
    methods.insert(("BoundingBox".into(), "contains_point".into()),    ("bbox_contains_point".into(), true));
    methods.insert(("BoundingBox".into(), "intersects_bbox".into()),   ("bbox_intersects_bbox".into(), true));
    methods.insert(("BoundingBox".into(), "intersects_sphere".into()), ("bbox_intersects_sphere".into(), true));
    methods.insert(("BoundingBox".into(), "merge".into()),             ("bbox_merge".into(), true));

    returns.insert(("BoundingBox".into(), "center".into()),       "Vec3".into());
    returns.insert(("BoundingBox".into(), "extents".into()),      "Vec3".into());
    returns.insert(("BoundingBox".into(), "half_extents".into()), "Vec3".into());
    returns.insert(("BoundingBox".into(), "merge".into()),        "BoundingBox".into());

    // ── BoundingSphere methods ──────────────────────────────────────────
    methods.insert(("BoundingSphere".into(), "contains_point".into()),    ("bsphere_contains_point".into(), true));
    methods.insert(("BoundingSphere".into(), "intersects_sphere".into()), ("bsphere_intersects_sphere".into(), true));

    // ── Ray methods ─────────────────────────────────────────────────────
    methods.insert(("Ray".into(), "point_at".into()),          ("ray_point_at".into(), true));
    methods.insert(("Ray".into(), "intersects_bbox".into()),   ("ray_intersects_bbox".into(), true));
    methods.insert(("Ray".into(), "intersects_sphere".into()), ("ray_intersects_sphere".into(), true));

    returns.insert(("Ray".into(), "point_at".into()), "Vec3".into());

    // ── Plane methods ───────────────────────────────────────────────────
    methods.insert(("Plane".into(), "distance".into()),       ("plane_distance".into(), true));
    methods.insert(("Plane".into(), "intersects_ray".into()), ("plane_intersects_ray".into(), true));

    // ── Sound methods ──────────────────────────────────────────────────
    methods.insert(("Sound".into(), "play".into()),        ("sound_play".into(), false));
    methods.insert(("Sound".into(), "play_looped".into()), ("sound_play_looped".into(), false));
    methods.insert(("Sound".into(), "stop".into()),        ("sound_stop".into(), false));
    methods.insert(("Sound".into(), "set_volume".into()),  ("sound_set_volume".into(), false));
    methods.insert(("Sound".into(), "is_playing".into()),  ("sound_is_playing".into(), true));
    methods.insert(("Sound".into(), "free".into()),        ("sound_free".into(), false));

    // ── RenderTarget methods ───────────────────────────────────────────
    methods.insert(("RenderTarget".into(), "bind".into()),       ("rendertarget_bind".into(), false));
    methods.insert(("RenderTarget".into(), "as_texture".into()), ("rendertarget_as_texture".into(), true));
    methods.insert(("RenderTarget".into(), "free".into()),       ("rendertarget_free".into(), false));

    // ── Model methods ──────────────────────────────────────────────────
    methods.insert(("Model".into(), "draw".into()),         ("model_draw".into(), false));
    methods.insert(("Model".into(), "vertex_count".into()), ("model_vertex_count".into(), true));
    methods.insert(("Model".into(), "free".into()),         ("model_free".into(), false));

    // ── AnimModel methods ──────────────────────────────────────────────
    methods.insert(("AnimModel".into(), "draw".into()),          ("anim_model_draw".into(), false));
    methods.insert(("AnimModel".into(), "play".into()),          ("anim_play".into(), false));
    methods.insert(("AnimModel".into(), "stop".into()),          ("anim_stop".into(), false));
    methods.insert(("AnimModel".into(), "pause".into()),         ("anim_pause".into(), false));
    methods.insert(("AnimModel".into(), "set_speed".into()),     ("anim_set_speed".into(), false));
    methods.insert(("AnimModel".into(), "set_looping".into()),   ("anim_set_looping".into(), false));
    methods.insert(("AnimModel".into(), "set_time".into()),      ("anim_set_time".into(), false));
    methods.insert(("AnimModel".into(), "get_time".into()),      ("anim_get_time".into(), true));
    methods.insert(("AnimModel".into(), "clip_count".into()),    ("anim_clip_count".into(), true));
    methods.insert(("AnimModel".into(), "clip_duration".into()), ("anim_clip_duration".into(), true));
    methods.insert(("AnimModel".into(), "joint_count".into()),   ("anim_joint_count".into(), true));
    methods.insert(("AnimModel".into(), "free".into()),          ("anim_model_free".into(), false));
}

fn list_method_to_ffi(method: &str) -> Option<&str> {
    match method {
        "add" => Some("nex_list_add"),
        "get" => Some("nex_list_get"),
        "set" => Some("nex_list_set"),
        "length" => Some("nex_list_length"),
        "remove" => Some("nex_list_remove"),
        "free" => Some("nex_list_free"),
        _ => None,
    }
}

fn type_expr_to_type(te: &nexc_ast::TypeExpr) -> Type {
    match &te.kind {
        nexc_ast::TypeExprKind::Named(n) => match n.as_str() {
            "Int" => Type::Int,
            "Bool" => Type::Bool,
            "String" => Type::String,
            "Unit" => Type::Unit,
            "Float" => Type::Float,
            "Double" => Type::Double,
            other => Type::Named(other.into()),
        },
        nexc_ast::TypeExprKind::Generic(base, args) => {
            // Type erasure: at IR level, Generic("List", [Int]) → Named("List")
            let _ = args;
            Type::Named(base.clone())
        }
        nexc_ast::TypeExprKind::Var => Type::Var,
        nexc_ast::TypeExprKind::Unit => Type::Unit,
        nexc_ast::TypeExprKind::Nullable(inner) => {
            Type::Nullable(Box::new(type_expr_to_type(inner)))
        }
        nexc_ast::TypeExprKind::Function(params, ret) => Type::Function(
            params.iter().map(type_expr_to_type).collect(),
            Box::new(type_expr_to_type(ret)),
        ),
    }
}

pub fn lower_typed_module(
    typed: &TypedModule,
    layouts: &[ClassLayout],
    _sink: &mut DiagnosticSink,
) -> IrModule {
    lower_typed_module_with_prefix(typed, layouts, _sink, None)
}

/// Like `lower_typed_module` but prefixes all top-level function, class method,
/// and struct method names with `module_prefix::` when provided.  This prevents
/// symbol collisions when multiple libraries export identically-named symbols
/// (e.g. `nex3d::color::COLOR_WHITE` vs `nex_ui::style::COLOR_WHITE`).
pub fn lower_typed_module_with_prefix(
    typed: &TypedModule,
    layouts: &[ClassLayout],
    _sink: &mut DiagnosticSink,
    module_prefix: Option<&str>,
) -> IrModule {
    let mut lowering = IrLowering::new();
    let mut functions = Vec::new();
    let mut globals = Vec::new();
    let mut init_instructions = Vec::new();

    let file_path = PathBuf::from(&typed.file.path);

    // Pre-populate known class/struct names for method resolution.
    for item in &typed.file.items {
        match item {
            nexc_ast::Item::Class(c) => lowering.known_classes.push(c.name.clone()),
            nexc_ast::Item::Struct(s) => lowering.known_classes.push(s.name.clone()),
            _ => {}
        }
    }

    // Collect operator methods, method return types, and field types for type tracking.
    for item in &typed.file.items {
        match item {
            nexc_ast::Item::Class(c) => {
                for method in &c.methods {
                    if let Some(op) = &method.operator {
                        lowering
                            .class_operators
                            .entry(c.name.clone())
                            .or_default()
                            .push((op.clone(), method.name.clone()));
                    }
                    if let Some(ret_ty) = &method.return_type {
                        match &ret_ty.kind {
                            nexc_ast::TypeExprKind::Named(ret_name) => {
                                lowering.class_method_returns.insert(
                                    (c.name.clone(), method.name.clone()),
                                    ret_name.clone(),
                                );
                            }
                            nexc_ast::TypeExprKind::Generic(base, _) => {
                                lowering.class_method_returns.insert(
                                    (c.name.clone(), method.name.clone()),
                                    base.clone(),
                                );
                            }
                            _ => {}
                        }
                    }
                }
                for field in &c.fields {
                    if let Some(ty) = &field.ty {
                        match &ty.kind {
                            nexc_ast::TypeExprKind::Named(type_name) => {
                                lowering.class_fields.insert(
                                    (c.name.clone(), field.name.clone()),
                                    type_name.clone(),
                                );
                            }
                            nexc_ast::TypeExprKind::Generic(base, _) => {
                                lowering.class_fields.insert(
                                    (c.name.clone(), field.name.clone()),
                                    base.clone(),
                                );
                            }
                            _ => {}
                        }
                    }
                }
            }
            nexc_ast::Item::Struct(s) => {
                for method in &s.methods {
                    if let Some(op) = &method.operator {
                        lowering
                            .class_operators
                            .entry(s.name.clone())
                            .or_default()
                            .push((op.clone(), method.name.clone()));
                    }
                    if let Some(ret_ty) = &method.return_type {
                        match &ret_ty.kind {
                            nexc_ast::TypeExprKind::Named(ret_name) => {
                                lowering.class_method_returns.insert(
                                    (s.name.clone(), method.name.clone()),
                                    ret_name.clone(),
                                );
                            }
                            nexc_ast::TypeExprKind::Generic(base, _) => {
                                lowering.class_method_returns.insert(
                                    (s.name.clone(), method.name.clone()),
                                    base.clone(),
                                );
                            }
                            _ => {}
                        }
                    }
                }
                for field in &s.fields {
                    if let Some(ty) = &field.ty {
                        match &ty.kind {
                            nexc_ast::TypeExprKind::Named(type_name) => {
                                lowering.class_fields.insert(
                                    (s.name.clone(), field.name.clone()),
                                    type_name.clone(),
                                );
                            }
                            nexc_ast::TypeExprKind::Generic(base, _) => {
                                lowering.class_fields.insert(
                                    (s.name.clone(), field.name.clone()),
                                    base.clone(),
                                );
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Register builtin type operators and methods (Tensor, Module, Optimizer).
    // These map directly to FFI functions from the torch library.
    seed_builtin_dispatch(&mut lowering);

    // Collect module-level variable assignments into globals and init code.
    for item in &typed.file.items {
        match item {
            nexc_ast::Item::Statement(stmt) => {
                if let nexc_ast::Stmt::Expr(nexc_ast::Expr::Assign { target, .. }) = stmt {
                    if let nexc_ast::Expr::Identifier { name, .. } = target.as_ref() {
                        let gname = format!("%{name}");
                        if !globals.contains(&gname) {
                            globals.push(gname);
                        }
                    }
                }
                let insts = lowering.lower_stmt_to_instructions(stmt);
                init_instructions.extend(insts);
            }
            nexc_ast::Item::Variable(var_decl) => {
                let gname = format!("%{}", var_decl.name);
                if !globals.contains(&gname) {
                    globals.push(gname.clone());
                }
                if let Some(init) = &var_decl.initializer {
                    let val = lowering.lower_expr(init);
                    init_instructions.push(IrInstruction::Store {
                        dst: gname,
                        src: val,
                    });
                }
            }
            nexc_ast::Item::Class(class) => {
                for field in &class.fields {
                    let gname = format!("%{}.{}", class.name, field.name);
                    if !globals.contains(&gname) {
                        globals.push(gname.clone());
                    }
                    if let Some(init) = &field.initializer {
                        let val = lowering.lower_expr(init);
                        init_instructions.push(IrInstruction::Store {
                            dst: gname,
                            src: val,
                        });
                    }
                }
            }
            nexc_ast::Item::Struct(s) => {
                for field in &s.fields {
                    let gname = format!("%{}.{}", s.name, field.name);
                    if !globals.contains(&gname) {
                        globals.push(gname.clone());
                    }
                    if let Some(init) = &field.initializer {
                        let val = lowering.lower_expr(init);
                        init_instructions.push(IrInstruction::Store {
                            dst: gname,
                            src: val,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    // Helper: apply module prefix to a function/method name when provided.
    // The entry point `main` is never prefixed.
    let prefix_name = |name: String| -> String {
        if let Some(pfx) = module_prefix {
            if name == "main" {
                return name;
            }
            format!("{pfx}::{name}")
        } else {
            name
        }
    };

    for item in &typed.file.items {
        match item {
            nexc_ast::Item::Function(func) => {
                lowering.current_class = None;
                let mut ir_fn = lowering.lower_function(func);
                ir_fn.name = prefix_name(ir_fn.name);
                ir_fn.file = Some(file_path.clone());
                functions.push(ir_fn);
            }
            nexc_ast::Item::Class(class) => {
                lowering.current_class = Some(class.name.clone());
                for method in &class.methods {
                    let mut ir_fn = lowering.lower_function(method);
                    ir_fn.name = prefix_name(format!("{}::{}", class.name, ir_fn.name));
                    ir_fn.file = Some(file_path.clone());
                    functions.push(ir_fn);
                }
            }
            nexc_ast::Item::Struct(s) => {
                lowering.current_class = Some(s.name.clone());
                for method in &s.methods {
                    let mut ir_fn = lowering.lower_function(method);
                    ir_fn.name = prefix_name(format!("{}::{}", s.name, ir_fn.name));
                    ir_fn.file = Some(file_path.clone());
                    functions.push(ir_fn);
                }
                // Synthesize init if no explicit init method exists
                let has_init = s.methods.iter().any(|m| m.name == "init");
                if !has_init && !s.fields.is_empty() {
                    let params: Vec<(String, String)> = s
                        .fields
                        .iter()
                        .map(|f| (f.name.clone(), "auto".into()))
                        .collect();
                    let mut init_body = Vec::new();
                    for field in &s.fields {
                        init_body.push(IrInstruction::Store {
                            dst: format!("%{}.{}", s.name, field.name),
                            src: IrValue::Register(format!("%{}", field.name)),
                        });
                    }
                    init_body.push(IrInstruction::Return(None));
                    functions.push(IrFunction {
                        name: prefix_name(format!("{}::init", s.name)),
                        params,
                        return_type: Type::Unit,
                        blocks: vec![IrBlock {
                            label: "entry".into(),
                            instructions: init_body,
                        }],
                        span: Some(s.span),
                        file: Some(file_path.clone()),
                    });
                }
            }
            _ => {}
        }
    }

    // When a module prefix was applied, also rewrite Call targets that reference
    // functions defined in this module so intra-module calls use the prefixed
    // names.  Calls to external / runtime functions are left untouched.
    if module_prefix.is_some() {
        let local_names: std::collections::HashSet<String> =
            functions.iter().map(|f| f.name.clone()).collect();
        for func in &mut functions {
            for block in &mut func.blocks {
                for inst in &mut block.instructions {
                    if let IrInstruction::Call { target, .. } = inst {
                        // Check if the target, after prefixing, matches a local function.
                        let prefixed = prefix_name(target.clone());
                        if local_names.contains(&prefixed) {
                            *target = prefixed;
                        }
                    }
                }
            }
        }
    }

    // Inject module-level init code at the beginning of main.
    if !init_instructions.is_empty() {
        let main_fn = functions.iter_mut().find(|f| f.name == "main");
        if let Some(main_fn) = main_fn {
            if let Some(entry_block) = main_fn.blocks.first_mut() {
                let mut combined = init_instructions;
                combined.append(&mut entry_block.instructions);
                entry_block.instructions = combined;
            }
        }
    }

    if functions.is_empty() {
        functions.push(IrFunction {
            name: "module_entry".into(),
            params: Vec::new(),
            return_type: Type::Unit,
            blocks: vec![IrBlock {
                label: "entry".into(),
                instructions: vec![IrInstruction::Return(None)],
            }],
            span: None,
            file: None,
        });
    }

    IrModule {
        name: typed.file.path.clone(),
        functions,
        types: typed.types.clone(),
        layouts: layouts
            .iter()
            .map(|l| (l.class_name.clone(), l.clone()))
            .collect(),
        globals,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexc_ast::{
        Block, Expr, FunctionDecl, Item, Literal, SourceFile, Stmt, VarDecl, Visibility,
    };
    use std::collections::HashMap;

    #[test]
    fn lowers_var_decl_initializer_into_store() {
        let s = Span::new(0, 0);
        let main = FunctionDecl {
            name: "main".into(),
            type_params: Vec::new(),
            params: Vec::new(),
            return_type: None,
            is_public: false,
            is_virtual: false,
            is_override: false,
            is_static: false,
            operator: None,
            body: Some(Expr::Block(Block {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".into(),
                        inferred_type: None,
                        explicit_type: None,
                        initializer: Some(Expr::Literal {
                            value: Literal::Int(7),
                            span: s,
                        }),
                        is_dynamic: true,
                        visibility: Visibility::Internal,
                        attributes: Vec::new(),
                        span: s,
                    }),
                    Stmt::Return(None, s),
                ],
                span: s,
            })),
            span: s,
            attributes: Vec::new(),
        };

        let typed = TypedModule {
            file: SourceFile {
                path: "test.nex".into(),
                span: s,
                items: vec![Item::Function(main)],
            },
            types: HashMap::new(),
            functions: HashMap::new(),
            diagnostics: Vec::new(),
        };

        let mut sink = DiagnosticSink::new();
        let ir = lower_typed_module(&typed, &[], &mut sink);
        let main_ir = ir
            .functions
            .iter()
            .find(|f| f.name == "main")
            .expect("main should be lowered");

        let has_initializer_store = main_ir.blocks.iter().any(|b| {
            b.instructions.iter().any(|inst| {
                matches!(
                    inst,
                    IrInstruction::Store {
                        dst,
                        src: IrValue::IntConst(7)
                    } if dst == "%x"
                )
            })
        });

        assert!(
            has_initializer_store,
            "var initializer should lower to a Store instruction"
        );
    }
}
