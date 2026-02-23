# Nex v0.1.0

A compiled, statically-typed language with classes, interfaces, and native code generation via Cranelift.

## Directory Layout

```
nex/
├── bin/
│   ├── nex.exe          # Main CLI (build, run, fmt, lint, repl, clean, install)
│   ├── nexc.exe         # Standalone compiler frontend
│   └── nex-lsp.exe      # Language server (LSP)
├── config/
│   ├── nex.toml         # Toolchain configuration
│   └── project.toml     # Template for new projects
├── examples/
│   ├── hello/           # Hello world with functions, loops, strings
│   └── console_showcase/ # Classes, inheritance, try/finally
└── README.md
```

## Quick Start

### 1. Add `nex/bin` to your PATH

```powershell
# PowerShell (current session)
$env:PATH += ";C:\path\to\nex\bin"

# Or permanently via System Environment Variables
```

### 2. Create a project

```
mkdir my_project/src
```

Copy `config/project.toml` to `my_project/project.toml` and edit the project name.

### 3. Write code

Create `my_project/src/main.nex`:

```
def main() -> Unit {
    println("Hello from Nex!")
    return
}
```

### 4. Build and run

```bash
nex file.nex              # JIT compile and execute (scripting mode)
nex run src/main.nex      # JIT compile and execute
nex build src/main.nex    # Compile to native executable (AOT)
nex fmt src/main.nex      # Format source code
nex lint src/main.nex     # Run linter
nex repl                 # Interactive REPL
nex clean                # Remove build artifacts
nex install              # Build release binaries and install to nex/bin/
```

## Execution Modes

**JIT (default for `nex run` / `nex file.nex`)** -- compiles in memory and executes
immediately using Cranelift JIT. No intermediate files, no external tools needed.
Feels like running a script.

**AOT (`nex build`)** -- compiles to a native object file via Cranelift, then links
with the system linker to produce a standalone executable. Requires a linker
(MSVC Build Tools on Windows, `cc`/`ld` on Linux/macOS).

## Requirements

- **JIT mode**: No external tools needed. The compiler is fully self-contained.
- **AOT mode**: A system linker must be available:
  - **Windows**: MSVC Build Tools (link.exe), GCC (MinGW), or Clang
  - **Linux/macOS**: GCC or Clang (usually pre-installed)

## Language Features (v0.1)

- Functions with typed parameters and return types
- Classes with fields, methods, and single/multiple inheritance
- Structs (value types) and interfaces
- Control flow: `if` / `else if` / `else`, `while`, `for`
- `try` / `catch` / `finally` and `using` blocks
- String concatenation and comparison
- `print()` and `println()` builtins
- Automatic semicolon insertion (ASI)
- `var` keyword for dynamically-typed variables

## VS Code Extension

A syntax highlighting extension is included in the repository under `vscode-extension/`.
Install it by symlinking to your VS Code extensions directory:

```powershell
# Windows
cmd /c mklink /D "%USERPROFILE%\.vscode\extensions\nex-language" "path\to\vscode-extension"
```
