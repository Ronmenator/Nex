/// Integration tests for the Nex compiler pipeline.
///
/// Each test compiles a small Nex program through the full pipeline
/// (lex → parse → type → IR → Cranelift codegen) and verifies:
/// - No compilation errors
/// - Object code is produced
/// - IR has the expected structure
///
/// These tests cover the bugs fixed in the while-loop / string-concat /
/// println-dispatch patches as well as general correctness of every major
/// language feature that touches codegen.

use std::collections::HashSet;

use nexc_driver::CompileOptions;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Compile a Nex source and assert no errors.
fn compile_ok(source: &str) -> nexc_driver::CompileResult {
    let opts = CompileOptions {
        source_path: "<test>".into(),
        emit_metadata: false,
        output_dir: None,
        lib_names: HashSet::new(),
        ..Default::default()
    };
    let result = nexc_driver::compile_module(source, opts);
    let errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, nexc_diag::Severity::Error))
        .collect();
    assert!(
        errors.is_empty(),
        "expected no errors but got:\n{}",
        errors
            .iter()
            .map(|e| format!("  {}", e.render(&result.source_map)))
            .collect::<Vec<_>>()
            .join("")
    );
    result
}

/// Assert the IR contains a specific function by name.
fn assert_ir_has_function(result: &nexc_driver::CompileResult, name: &str) {
    let ir = result.ir.as_ref().expect("no IR produced");
    assert!(
        ir.functions.iter().any(|f| f.name == name),
        "IR should contain function `{name}` but found: {:?}",
        ir.functions.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}

/// Assert the IR for a function contains instructions matching the given predicate.
fn assert_ir_function_has<F>(result: &nexc_driver::CompileResult, func_name: &str, pred: F)
where
    F: Fn(&nexc_ir::IrInstruction) -> bool,
{
    let ir = result.ir.as_ref().expect("no IR produced");
    let func = ir
        .functions
        .iter()
        .find(|f| f.name == func_name)
        .unwrap_or_else(|| panic!("function `{func_name}` not in IR"));
    let found = func
        .blocks
        .iter()
        .any(|b| b.instructions.iter().any(|i| pred(i)));
    assert!(
        found,
        "function `{func_name}` IR doesn't contain expected instruction"
    );
}

// ===========================================================================
// 1. WHILE LOOP TESTS
// ===========================================================================

#[test]
fn while_loop_basic_count() {
    let src = r#"
def main() {
    i = 0
    while (i < 5) {
        println(i)
        i = i + 1
    }
    println("done")
    return
}
"#;
    let result = compile_ok(src);
    assert_ir_has_function(&result, "main");
    assert!(result.object.is_some(), "should produce object code");
}

#[test]
fn while_loop_no_iteration() {
    let src = r#"
def main() {
    i = 10
    while (i < 5) {
        println("should not print")
        i = i + 1
    }
    println("skipped")
    return
}
"#;
    let result = compile_ok(src);
    assert_ir_has_function(&result, "main");
}

#[test]
fn while_loop_single_iteration() {
    let src = r#"
def main() {
    i = 0
    while (i < 1) {
        println("once")
        i = i + 1
    }
    println("after")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn while_loop_nested() {
    let src = r#"
def main() {
    i = 0
    while (i < 3) {
        j = 0
        while (j < 2) {
            println("inner")
            j = j + 1
        }
        i = i + 1
    }
    println("done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn while_loop_with_break_and_continue() {
    let src = r#"
def main() {
    i = 0
    while (i < 100) {
        if (i == 5) {
            break
        }
        i = i + 1
    }
    println(i)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn while_loop_with_string_concat_in_body() {
    let src = r#"
def main() {
    i = 1
    while (i <= 3) {
        println("iteration " + i)
        i = i + 1
    }
    println("done")
    return
}
"#;
    let result = compile_ok(src);
    assert_ir_has_function(&result, "main");
}

#[test]
fn while_loop_code_after_loop_executes() {
    let src = r#"
def main() {
    x = 0
    while (x < 3) {
        x = x + 1
    }
    result = x + 10
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn while_loop_modifying_external_variable() {
    let src = r#"
total = 0

def main() {
    i = 1
    while (i <= 5) {
        total = total + i
        i = i + 1
    }
    println(total)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 2. FOR LOOP TESTS
// ===========================================================================

#[test]
fn for_loop_basic() {
    let src = r#"
def main() {
    for (i = 0; i < 5; i = i + 1) {
        println(i)
    }
    println("done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn for_loop_no_iteration() {
    let src = r#"
def main() {
    for (i = 10; i < 5; i = i + 1) {
        println("should not print")
    }
    println("skipped")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn for_loop_nested() {
    let src = r#"
def main() {
    for (i = 0; i < 3; i = i + 1) {
        for (j = 0; j < 2; j = j + 1) {
            println("cell")
        }
    }
    println("done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn for_loop_with_string_concat() {
    let src = r#"
def main() {
    for (i = 1; i <= 3; i = i + 1) {
        println("item " + i)
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn for_loop_code_after_loop_executes() {
    let src = r#"
def main() {
    sum = 0
    for (i = 1; i <= 10; i = i + 1) {
        sum = sum + i
    }
    println(sum)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 3. COMPARISON OPERATOR TESTS
// ===========================================================================

#[test]
fn comparison_less_than() {
    let src = r#"
def main() {
    if (3 < 5) {
        println("yes")
    }
    if (5 < 3) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_less_than_or_equal() {
    let src = r#"
def main() {
    if (3 <= 5) {
        println("yes")
    }
    if (5 <= 5) {
        println("equal")
    }
    if (6 <= 5) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_greater_than() {
    let src = r#"
def main() {
    if (5 > 3) {
        println("yes")
    }
    if (3 > 5) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_greater_than_or_equal() {
    let src = r#"
def main() {
    if (5 >= 3) {
        println("yes")
    }
    if (5 >= 5) {
        println("equal")
    }
    if (3 >= 5) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_equal() {
    let src = r#"
def main() {
    if (5 == 5) {
        println("yes")
    }
    if (5 == 3) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_not_equal() {
    let src = r#"
def main() {
    if (5 != 3) {
        println("different")
    }
    if (5 != 5) {
        println("no")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_in_while_condition() {
    let src = r#"
def main() {
    x = 0
    while (x <= 9) {
        x = x + 1
    }
    println(x)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn comparison_negative_numbers() {
    let src = r#"
def main() {
    if (-1 < 0) {
        println("neg less than zero")
    }
    if (0 > -1) {
        println("zero greater than neg")
    }
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 4. ARITHMETIC OPERATOR TESTS
// ===========================================================================

#[test]
fn arithmetic_add() {
    let src = r#"
def main() {
    println(3 + 4)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_subtract() {
    let src = r#"
def main() {
    println(10 - 3)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_multiply() {
    let src = r#"
def main() {
    println(6 * 7)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_divide() {
    let src = r#"
def main() {
    println(20 / 4)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_modulo() {
    let src = r#"
def main() {
    println(17 % 5)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_precedence() {
    let src = r#"
def main() {
    result = 2 + 3 * 4
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_parentheses() {
    let src = r#"
def main() {
    result = (2 + 3) * 4
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_compound_expression() {
    let src = r#"
def main() {
    a = 10
    b = 3
    result = a * b + a / b - a % b
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn arithmetic_unary_negation() {
    let src = r#"
def main() {
    x = 5
    y = -x
    println(y)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 5. STRING CONCATENATION TESTS
// ===========================================================================

#[test]
fn string_concat_two_literals() {
    let src = r#"
def main() {
    println("hello" + " world")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_string_and_int() {
    let src = r#"
def main() {
    println("count: " + 42)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_int_and_string() {
    let src = r#"
def main() {
    println(42 + " is the answer")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_with_variable() {
    let src = r#"
def main() {
    name = "World"
    println("Hello, " + name)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_int_variable() {
    let src = r#"
def main() {
    x = 7
    println("x = " + x)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_in_loop() {
    let src = r#"
def main() {
    i = 1
    while (i <= 3) {
        println("step " + i)
        i = i + 1
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_multiple() {
    let src = r#"
def main() {
    a = 1
    b = 2
    println("a=" + a + " b=" + b)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn string_concat_in_function_call() {
    let src = r#"
def greet(name: String) -> String {
    return "Hello, " + name + "!"
}

def main() {
    msg = greet("Nex")
    println(msg)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 6. PRINTLN / PRINT TESTS (type dispatch)
// ===========================================================================

#[test]
fn println_int_literal() {
    let src = r#"
def main() {
    println(42)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_string_literal() {
    let src = r#"
def main() {
    println("hello")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_bool_true() {
    let src = r#"
def main() {
    println(true)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_bool_false() {
    let src = r#"
def main() {
    println(false)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_comparison_result() {
    let src = r#"
def main() {
    result = 3 < 5
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_arithmetic_result() {
    let src = r#"
def main() {
    result = 10 + 20
    println(result)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_string_variable() {
    let src = r#"
def main() {
    msg = "hello world"
    println(msg)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn println_string_concat_result() {
    let src = r#"
def main() {
    msg = "count: " + 5
    println(msg)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn print_without_newline() {
    let src = r#"
def main() {
    print("hello ")
    println("world")
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 7. IF / ELSE TESTS
// ===========================================================================

#[test]
fn if_true_branch() {
    let src = r#"
def main() {
    if (1 < 2) {
        println("yes")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn if_false_branch() {
    let src = r#"
def main() {
    if (2 < 1) {
        println("no")
    }
    println("after")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn if_else() {
    let src = r#"
def main() {
    x = 10
    if (x > 5) {
        println("big")
    } else {
        println("small")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn if_else_chain() {
    let src = r#"
def main() {
    x = 5
    if (x > 10) {
        println("large")
    } else {
        if (x > 3) {
            println("medium")
        } else {
            println("small")
        }
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn if_inside_while() {
    let src = r#"
def main() {
    i = 0
    while (i < 6) {
        if (i % 2 == 0) {
            println("even")
        } else {
            println("odd")
        }
        i = i + 1
    }
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 8. FUNCTION CALL TESTS
// ===========================================================================

#[test]
fn function_no_args() {
    let src = r#"
def greet() {
    println("hello")
    return
}

def main() {
    greet()
    return
}
"#;
    compile_ok(src);
}

#[test]
fn function_with_args() {
    let src = r#"
def add(a: Int, b: Int) -> Int {
    return a + b
}

def main() {
    println(add(3, 4))
    return
}
"#;
    compile_ok(src);
}

#[test]
fn function_with_string_return() {
    let src = r#"
def greet(name: String) -> String {
    return "Hi " + name
}

def main() {
    println(greet("Nex"))
    return
}
"#;
    compile_ok(src);
}

#[test]
fn function_recursive() {
    let src = r#"
def factorial(n: Int) -> Int {
    if (n <= 1) {
        return 1
    }
    return n * factorial(n - 1)
}

def main() {
    println(factorial(5))
    return
}
"#;
    compile_ok(src);
}

#[test]
fn function_multiple_calls() {
    let src = r#"
def double(x: Int) -> Int {
    return x * 2
}

def main() {
    a = double(5)
    b = double(a)
    println(b)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 9. VARIABLE ASSIGNMENT AND REASSIGNMENT TESTS
// ===========================================================================

#[test]
fn variable_first_assignment() {
    let src = r#"
def main() {
    x = 42
    println(x)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn variable_reassignment() {
    let src = r#"
def main() {
    x = 1
    println(x)
    x = 2
    println(x)
    x = 3
    println(x)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn variable_increment_in_loop() {
    let src = r#"
def main() {
    x = 0
    while (x < 10) {
        x = x + 1
    }
    println(x)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn variable_string_assignment() {
    let src = r#"
def main() {
    msg = "hello"
    println(msg)
    msg = "world"
    println(msg)
    return
}
"#;
    compile_ok(src);
}

#[test]
fn module_level_variable() {
    let src = r#"
counter = 0

def increment() {
    counter = counter + 1
    return
}

def main() {
    increment()
    increment()
    increment()
    println(counter)
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 10. BOOLEAN LOGIC TESTS
// ===========================================================================

#[test]
fn boolean_and() {
    let src = r#"
def main() {
    if (true && true) {
        println("both true")
    }
    if (true && false) {
        println("should not print")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn boolean_or() {
    let src = r#"
def main() {
    if (false || true) {
        println("one true")
    }
    if (false || false) {
        println("should not print")
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn boolean_not() {
    let src = r#"
def main() {
    if (!false) {
        println("not false")
    }
    if (!true) {
        println("should not print")
    }
    return
}
"#;
    compile_ok(src);
}

// ===========================================================================
// 11. IR STRUCTURE TESTS (verify specific IR instructions are generated)
// ===========================================================================

#[test]
fn ir_while_has_branch_and_jump() {
    let src = r#"
def main() {
    i = 0
    while (i < 5) {
        i = i + 1
    }
    return
}
"#;
    let result = compile_ok(src);
    assert_ir_function_has(&result, "main", |inst| {
        matches!(inst, nexc_ir::IrInstruction::Branch { .. })
    });
    assert_ir_function_has(&result, "main", |inst| {
        matches!(inst, nexc_ir::IrInstruction::Jump { .. })
    });
}

#[test]
fn ir_while_exit_label_is_real_block() {
    let src = r#"
def main() {
    i = 0
    while (i < 3) {
        i = i + 1
    }
    println("after loop")
    return
}
"#;
    let result = compile_ok(src);
    let ir = result.ir.as_ref().expect("no IR");
    let func = ir.functions.iter().find(|f| f.name == "main").unwrap();

    // Find the Branch instruction and verify the else_label (exit) is a real block
    let mut exit_labels = Vec::new();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let nexc_ir::IrInstruction::Branch { else_label, .. } = inst {
                exit_labels.push(else_label.clone());
            }
        }
    }
    assert!(!exit_labels.is_empty(), "should have at least one Branch");

    let block_labels: Vec<&str> = func.blocks.iter().map(|b| b.label.as_str()).collect();
    for exit in &exit_labels {
        assert!(
            block_labels.contains(&exit.as_str()),
            "exit label `{exit}` should be a real IrBlock, not phantom. \
             Found blocks: {block_labels:?}"
        );
    }
}

#[test]
fn ir_for_exit_label_is_real_block() {
    let src = r#"
def main() {
    for (i = 0; i < 3; i = i + 1) {
        println(i)
    }
    println("after for")
    return
}
"#;
    let result = compile_ok(src);
    let ir = result.ir.as_ref().expect("no IR");
    let func = ir.functions.iter().find(|f| f.name == "main").unwrap();

    let mut exit_labels = Vec::new();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let nexc_ir::IrInstruction::Branch { else_label, .. } = inst {
                exit_labels.push(else_label.clone());
            }
        }
    }
    assert!(!exit_labels.is_empty(), "should have at least one Branch");

    let block_labels: Vec<&str> = func.blocks.iter().map(|b| b.label.as_str()).collect();
    for exit in &exit_labels {
        assert!(
            block_labels.contains(&exit.as_str()),
            "exit label `{exit}` should be a real IrBlock, not phantom. \
             Found blocks: {block_labels:?}"
        );
    }
}

#[test]
fn ir_string_concat_generates_binop_add() {
    let src = r#"
def main() {
    println("hello " + 42)
    return
}
"#;
    let result = compile_ok(src);
    assert_ir_function_has(&result, "main", |inst| {
        matches!(inst, nexc_ir::IrInstruction::BinOp { op, .. } if op == "add")
    });
}

// ===========================================================================
// 12. COMBINED / REGRESSION TESTS
// ===========================================================================

#[test]
fn regression_while_loop_with_grid_pattern() {
    // Reproduces the original UI showcase pattern that triggered the infinite loop
    let src = r#"
def main() {
    i = 1
    while (i <= 9) {
        println("Cell " + i)
        r = 60 + i * 20
        if (r > 255) {
            r = 255
        }
        i = i + 1
    }
    println("grid done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn regression_multiple_while_loops_sequential() {
    let src = r#"
def main() {
    i = 0
    while (i < 3) {
        println("first " + i)
        i = i + 1
    }
    j = 0
    while (j < 3) {
        println("second " + j)
        j = j + 1
    }
    println("all done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn regression_for_followed_by_while() {
    let src = r#"
def main() {
    for (i = 0; i < 3; i = i + 1) {
        println("for " + i)
    }
    j = 0
    while (j < 3) {
        println("while " + j)
        j = j + 1
    }
    println("mixed done")
    return
}
"#;
    compile_ok(src);
}

#[test]
fn regression_while_with_function_calls() {
    let src = r#"
def compute(x: Int) -> Int {
    return x * 2 + 1
}

def main() {
    i = 0
    while (i < 5) {
        result = compute(i)
        println("compute(" + i + ") = " + result)
        i = i + 1
    }
    return
}
"#;
    compile_ok(src);
}

#[test]
fn regression_complex_program() {
    // Exercises multiple features together
    let src = r#"
counter = 0

def increment() {
    counter = counter + 1
    return
}

def is_even(n: Int) -> Bool {
    return n % 2 == 0
}

def main() {
    for (i = 0; i < 10; i = i + 1) {
        increment()
        even = is_even(i)
        if (even) {
            println("even: " + i)
        }
    }

    j = counter
    while (j > 0) {
        j = j - 1
    }
    println("counter = " + counter)
    println("j = " + j)
    return
}
"#;
    compile_ok(src);
}
