# ADR-0009: Type-Discriminating Match

## Status

Accepted

## Context

Nex has pattern matching (`match` expressions) with literal, enum variant, wildcard, binding, and guard patterns. However, there is no way to match on the runtime type of an object. This is needed for polymorphic dispatch in scenarios where a function returns a base class and callers need to inspect the concrete subtype (e.g., `AgentResult` → `SuccessResult` / `ErrorResult`).

Without type-discriminating match, users must resort to enum-tag workarounds or manual reflection API calls to determine the concrete type. This is verbose, error-prone, and doesn't provide compile-time type narrowing inside match arms.

## Decision

Add `is Type as binding` patterns to `match` expressions. The `is` keyword performs a runtime type check; the `as` keyword binds the scrutinee with the narrowed type inside the arm body.

### Syntax

```nex
match value {
    is TypeName as binding -> {
        // binding is typed as TypeName
    }
    is OtherType as other -> { ... }
    _ -> { ... }
}
```

### Runtime Mechanism (Option B — Type ID + Reflection Registry)

Each heap-allocated object carries an 8-byte type ID as the first field of its object header. The type ID is the index into the reflection type registry, assigned by `nex_reflect_register_type` at program startup. Constructors stamp the type ID immediately after allocation.

Type checking calls `nex_reflect_instanceof(object_type_id, "TypeName")` which checks:
1. Exact type match (type name equality)
2. Parent class chain (walks `base_classes`)
3. Implemented interfaces (walks `interfaces`)

This approach was chosen over vtable pointer comparison (Option A) because Nex supports multiple inheritance and interfaces. The reflection registry already exists and stores the full class hierarchy.

### Compiler Changes

- **AST**: Add `Pattern::TypeCheck { type_name: String, binding: String, span: Span }`
- **Lexer**: Reserve `is` as a keyword
- **Parser**: In `parse_pattern`, when current token is `is`: parse `"is" IDENTIFIER "as" IDENTIFIER`
- **IR Lowering**: Load object type ID → call `nex_reflect_instanceof` → branch on result; bind scrutinee as narrowed type in arm body
- **Codegen**: Emit the branch and binding; the binding is the same pointer with a different compile-time type (zero-cost cast)
- **Type Inference**: Inside a type-check arm, the binding has the matched type for field/method resolution

### Semantics

1. Match order matters — arms checked top to bottom, first match wins
2. Wildcard required — compiler emits warning if no `_` or base-type arm is present
3. Works with interfaces, not just class hierarchy
4. Compatible with guards — guards apply after type check passes
5. Works as an expression — returns a value like existing match

## Consequences

- `is` becomes a reserved keyword (was previously unreserved)
- Every heap-allocated object gains 8 bytes of overhead (type ID field in object header)
- Prerequisite: objects must have per-instance heap allocation (the singleton global field model must be replaced with a real object model for classes that participate in type checking)
- The reflection registry must be populated at program startup for all classes, structs, enums, and interfaces
- `nex_reflect_instanceof` is a new runtime function added to `nex_runtime/src/reflect.rs`
