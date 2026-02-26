# Nex Language Specification (v1)

This document captures the v1 language definition for the Nex compiler and runtime.

- Extension: `.nex`
- Static typing by default; explicit `var` for dynamic dispatch (`Var` type).
- Class-based OO with multiple inheritance and state.
- Tracing GC only.
- Deterministic cleanup with `using`.
- ASI behavior for `identifier`, literals, `)`, `]`, `}`, `return` (no expr), `break`, `continue`.
- Pattern matching with `match` expressions: literal, enum variant, wildcard, binding, guard, and type-discriminating (`is Type as binding`) patterns.
- Runtime type checking via `is` patterns: each object carries a type ID; `nex_reflect_instanceof` walks the class hierarchy and interface list. `is` is a reserved keyword.
