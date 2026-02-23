# Nex Language Specification (v1)

This document captures the v1 language definition for the Nex compiler and runtime.

- Extension: `.nex`
- Static typing by default; explicit `var` for dynamic dispatch (`Var` type).
- Class-based OO with multiple inheritance and state.
- Tracing GC only.
- Deterministic cleanup with `using`.
- ASI behavior for `identifier`, literals, `)`, `]`, `}`, `return` (no expr), `break`, `continue`.
