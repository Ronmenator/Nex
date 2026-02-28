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
- Default function arguments: `def f(x: Int, y: Int = 10)` — trailing parameters may have `= expr` defaults; missing call-site args filled from defaults.
- Range loops: `for (i in 0..n)` — the `..` operator creates a half-open integer range; for-in with a range desugars to an integer counter loop.
- List literals: `[expr, expr, ...]` — bracket syntax creates a `List` and adds each element; `[]` creates an empty list.
