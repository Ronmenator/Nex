# Console Showcase

This example demonstrates a compact Nex program using the currently implemented
frontend syntax:

- imports
- interface and class declarations
- qualified base member access with `::`
- assignment and expression precedence
- `if/else`
- `using`
- `try/catch/finally`

## Run with the current CLI stubs

From repository root:

```powershell
.\target\debug\nex.exe build examples\console_showcase\src\main.nex
```

Or directly with `nexc`:

```powershell
.\target\debug\nexc.exe examples\console_showcase\src\main.nex
```

Current milestone behavior:

- frontend pipeline runs (lex/parse/resolve/type/layout/IR/codegen placeholder)
- a placeholder object file is written (`build/main.o` for `nex build`, or
  `<source>.o` for `nexc`)
- no runnable executable is produced yet (link stage not implemented)
