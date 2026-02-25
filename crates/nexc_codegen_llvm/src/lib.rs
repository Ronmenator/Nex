use nexc_diag::Diagnostic;
use nexc_ir::{IrModule, IrFunction, IrInstruction, IrValue};

#[derive(Debug, Clone)]
pub struct AotConfig {
    pub emit_objects: bool,
    pub output_path: Option<std::path::PathBuf>,
}

impl Default for AotConfig {
    fn default() -> Self {
        Self {
            emit_objects: true,
            output_path: None,
        }
    }
}

pub fn generate(ir: &IrModule) -> Result<Vec<u8>, String> {
    generate_with_config(ir, &AotConfig::default())
}

pub fn generate_with_config(ir: &IrModule, _config: &AotConfig) -> Result<Vec<u8>, String> {
    if ir.functions.is_empty() {
        return Err("no functions to codegen".into());
    }

    let mut out = String::new();
    out.push_str(&format!("; Nex LLVM IR for module: {}\n", ir.name));
    out.push_str("target datalayout = \"e-m:w-i64:64-f80:128-n8:16:32:64-S128\"\n\n");

    emit_runtime_decls(&mut out);

    let mut string_pool: Vec<String> = Vec::new();

    for func in &ir.functions {
        collect_strings(func, &mut string_pool);
    }

    for (i, s) in string_pool.iter().enumerate() {
        let escaped = s.replace('\\', "\\5C").replace('"', "\\22");
        let len = s.len() + 1;
        out.push_str(&format!(
            "@.str.{i} = private unnamed_addr constant [{len} x i8] c\"{escaped}\\00\"\n"
        ));
    }
    if !string_pool.is_empty() {
        out.push('\n');
    }

    for func in &ir.functions {
        emit_function(&mut out, func, &string_pool);
    }

    Ok(out.into_bytes())
}

fn emit_runtime_decls(out: &mut String) {
    out.push_str("; Runtime declarations\n");
    out.push_str("declare i8* @nex_gc_alloc(i8*, i32)\n");
    out.push_str("declare void @nex_gc_collect()\n");
    out.push_str("declare void @nex_gc_safepoint()\n");
    out.push_str("declare void @nex_throw(i8*)\n");
    out.push_str("declare i32 @puts(i8*)\n");
    out.push_str("declare i32 @printf(i8*, ...)\n");
    // List<T> runtime (type-erased)
    out.push_str("declare i64 @nex_list_new()\n");
    out.push_str("declare void @nex_list_add(i64, i64)\n");
    out.push_str("declare i64 @nex_list_get(i64, i64)\n");
    out.push_str("declare void @nex_list_set(i64, i64, i64)\n");
    out.push_str("declare i64 @nex_list_length(i64)\n");
    out.push_str("declare i64 @nex_list_remove(i64, i64)\n");
    out.push_str("declare void @nex_list_free(i64)\n\n");
}

fn collect_strings(func: &IrFunction, pool: &mut Vec<String>) {
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                IrInstruction::Store { src: IrValue::StringConst(s), .. }
                | IrInstruction::Print { value: IrValue::StringConst(s) } => {
                    if !pool.contains(s) {
                        pool.push(s.clone());
                    }
                }
                IrInstruction::Call { args, .. } => {
                    for arg in args {
                        if let IrValue::StringConst(s) = arg {
                            if !pool.contains(s) {
                                pool.push(s.clone());
                            }
                        }
                    }
                }
                IrInstruction::Return(Some(IrValue::StringConst(s))) => {
                    if !pool.contains(s) {
                        pool.push(s.clone());
                    }
                }
                IrInstruction::BinOp { lhs, rhs, .. } => {
                    if let IrValue::StringConst(s) = lhs {
                        if !pool.contains(s) { pool.push(s.clone()); }
                    }
                    if let IrValue::StringConst(s) = rhs {
                        if !pool.contains(s) { pool.push(s.clone()); }
                    }
                }
                _ => {}
            }
        }
    }
}

fn emit_function(out: &mut String, func: &IrFunction, string_pool: &[String]) {
    let ret_type = llvm_type(&func.return_type);
    let params: Vec<String> = func.params.iter().enumerate()
        .map(|(_i, (name, _))| format!("i64 %param.{name}"))
        .collect();

    let safe_name = func.name.replace("::", ".");
    out.push_str(&format!("define {} @\"{}\"({}) {{\n", ret_type, safe_name, params.join(", ")));

    for (i, block) in func.blocks.iter().enumerate() {
        if i == 0 {
            out.push_str("entry:\n");
        } else {
            out.push_str(&format!("{}:\n", block.label));
        }

        let mut terminated = false;
        for inst in &block.instructions {
            if terminated {
                break;
            }
            terminated = matches!(
                inst,
                IrInstruction::Return(_)
                    | IrInstruction::Jump { .. }
                    | IrInstruction::Branch { .. }
            );
            emit_instruction(out, inst, string_pool);
        }
    }

    out.push_str("}\n\n");
}

fn emit_instruction(out: &mut String, inst: &IrInstruction, string_pool: &[String]) {
    match inst {
        IrInstruction::Nop => {
            out.push_str("  ; nop\n");
        }
        IrInstruction::Return(None) => {
            out.push_str("  ret void\n");
        }
        IrInstruction::Return(Some(val)) => {
            let (ty, v) = emit_value(val, string_pool);
            out.push_str(&format!("  ret {ty} {v}\n"));
        }
        IrInstruction::Allocate { dst, ty: _ } => {
            out.push_str(&format!("  {dst} = alloca i64\n"));
        }
        IrInstruction::Store { dst, src } => {
            let (ty, v) = emit_value(src, string_pool);
            out.push_str(&format!("  store {ty} {v}, i64* {dst}\n"));
        }
        IrInstruction::Load { dst, src } => {
            out.push_str(&format!("  {dst} = load i64, i64* {src}\n"));
        }
        IrInstruction::BinOp { dst, op, lhs, rhs } => {
            let (_, lv) = emit_value(lhs, string_pool);
            let (_, rv) = emit_value(rhs, string_pool);
            let llvm_op = match op.as_str() {
                "add" => "add",
                "sub" => "sub",
                "mul" => "mul",
                "div" => "sdiv",
                "mod" => "srem",
                "eq" => "icmp eq",
                "ne" => "icmp ne",
                "lt" => "icmp slt",
                "le" => "icmp sle",
                "gt" => "icmp sgt",
                "ge" => "icmp sge",
                "and" => "and",
                "or" => "or",
                _ => "add",
            };
            if llvm_op.starts_with("icmp") {
                out.push_str(&format!("  {dst} = {llvm_op} i64 {lv}, {rv}\n"));
            } else {
                out.push_str(&format!("  {dst} = {llvm_op} i64 {lv}, {rv}\n"));
            }
        }
        IrInstruction::UnaryOp { dst, op, operand } => {
            let (_, v) = emit_value(operand, string_pool);
            match op.as_str() {
                "neg" => out.push_str(&format!("  {dst} = sub i64 0, {v}\n")),
                "not" => out.push_str(&format!("  {dst} = xor i1 {v}, true\n")),
                _ => out.push_str(&format!("  {dst} = add i64 0, {v}\n")),
            }
        }
        IrInstruction::Branch { cond, then_label, else_label } => {
            let (_, cv) = emit_value(cond, string_pool);
            out.push_str(&format!("  br i1 {cv}, label %{then_label}, label %{else_label}\n"));
        }
        IrInstruction::Jump { target } => {
            out.push_str(&format!("  br label %{target}\n"));
        }
        IrInstruction::Call { dst, target, args } => {
            let arg_strs: Vec<String> = args.iter()
                .map(|a| { let (ty, v) = emit_value(a, string_pool); format!("{ty} {v}") })
                .collect();
            if let Some(d) = dst {
                out.push_str(&format!("  {d} = call i64 @\"{target}\"({})\n", arg_strs.join(", ")));
            } else {
                out.push_str(&format!("  call void @\"{target}\"({})\n", arg_strs.join(", ")));
            }
        }
        IrInstruction::VCall { dst, slot, this_ptr, args: _ } => {
            out.push_str(&format!("  ; vcall slot {slot} on {this_ptr}\n"));
            if let Some(d) = dst {
                out.push_str(&format!("  {d} = call i64 @\"__vcall\"(i64 {this_ptr})\n"));
            }
        }
        IrInstruction::Print { value } => {
            let (ty, v) = emit_value(value, string_pool);
            out.push_str(&format!("  call i32 @puts({ty} {v})\n"));
        }
        IrInstruction::MemberAccess { dst, receiver, field } => {
            let (_, rv) = emit_value(receiver, string_pool);
            out.push_str(&format!("  ; member access .{field} on {rv}\n"));
            out.push_str(&format!("  {dst} = add i64 {rv}, 0\n"));
        }
        IrInstruction::EmitDiag { message } => {
            out.push_str(&format!("  ; diag: {message}\n"));
        }
    }
}

fn emit_value(val: &IrValue, string_pool: &[String]) -> (String, String) {
    match val {
        IrValue::IntConst(v) => ("i64".into(), v.to_string()),
        IrValue::FloatConst(v) => ("double".into(), format!("{v:.6e}")),
        IrValue::BoolConst(v) => ("i1".into(), if *v { "true" } else { "false" }.into()),
        IrValue::StringConst(s) => {
            let idx = string_pool.iter().position(|x| x == s).unwrap_or(0);
            let len = s.len() + 1;
            ("i8*".into(), format!("getelementptr inbounds ([{len} x i8], [{len} x i8]* @.str.{idx}, i32 0, i32 0)"))
        }
        IrValue::NullConst => ("i8*".into(), "null".into()),
        IrValue::Register(r) => ("i64".into(), r.clone()),
    }
}

fn llvm_type(ty: &nexc_type::Type) -> String {
    match ty {
        nexc_type::Type::Unit => "void".into(),
        nexc_type::Type::Bool => "i1".into(),
        nexc_type::Type::Byte => "i8".into(),
        nexc_type::Type::Int => "i64".into(),
        nexc_type::Type::Int64 => "i64".into(),
        nexc_type::Type::Float => "float".into(),
        nexc_type::Type::Double => "double".into(),
        nexc_type::Type::Char => "i32".into(),
        nexc_type::Type::String => "i8*".into(),
        nexc_type::Type::Generic(_, _) => "i64".into(), // type erasure
        _ => "i64".into(),
    }
}

pub fn emit_metadata(_ir: &IrModule) -> Vec<u8> {
    b"{\"kind\":\"llvm-metadata\"}\n".to_vec()
}

pub fn collect_stage_diagnostics(ir: &IrModule) -> Vec<Diagnostic> {
    if ir.name.is_empty() {
        vec![Diagnostic::warning("codegen_empty_module", "module has empty name", None, None)]
    } else {
        Vec::new()
    }
}
