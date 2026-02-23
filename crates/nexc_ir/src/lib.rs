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
                let val = self.lower_expr(value);
                if let Expr::Identifier { name, .. } = target.as_ref() {
                    self.emit(IrInstruction::Store {
                        dst: format!("%{name}"),
                        src: val.clone(),
                    });
                }
                val
            }
            Expr::Call { callee, args, .. } => {
                let ir_args: Vec<IrValue> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dst = self.fresh_temp();
                let target = match callee.as_ref() {
                    Expr::Identifier { name, .. } => self.resolve_call_target(name, None),
                    Expr::MemberAccess {
                        receiver,
                        name,
                        qualifier,
                        ..
                    } => {
                        let qual = qualifier.as_deref().or_else(|| {
                            if matches!(receiver.as_ref(), Expr::Identifier { name, .. } if name == "self") {
                                self.current_class.as_deref()
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
                    let gname = format!("%{}", field.name);
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

    for item in &typed.file.items {
        match item {
            nexc_ast::Item::Function(func) => {
                lowering.current_class = None;
                let mut ir_fn = lowering.lower_function(func);
                ir_fn.file = Some(file_path.clone());
                functions.push(ir_fn);
            }
            nexc_ast::Item::Class(class) => {
                lowering.current_class = Some(class.name.clone());
                for method in &class.methods {
                    let mut ir_fn = lowering.lower_function(method);
                    ir_fn.name = format!("{}::{}", class.name, ir_fn.name);
                    ir_fn.file = Some(file_path.clone());
                    functions.push(ir_fn);
                }
            }
            nexc_ast::Item::Struct(s) => {
                lowering.current_class = Some(s.name.clone());
                for method in &s.methods {
                    let mut ir_fn = lowering.lower_function(method);
                    ir_fn.name = format!("{}::{}", s.name, ir_fn.name);
                    ir_fn.file = Some(file_path.clone());
                    functions.push(ir_fn);
                }
            }
            _ => {}
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
