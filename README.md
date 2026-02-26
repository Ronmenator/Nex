# Nex

A compiled, statically-typed programming language with classes, interfaces, multiple inheritance, and native code generation. Designed for both rapid prototyping and production applications.

## Install

```powershell
irm https://raw.githubusercontent.com/Ronmenator/Nex/master/install.ps1 | iex
```

This downloads the latest release, extracts it to `~/.nex/`, and adds it to your PATH. After install, restart your terminal and run `nex --version` to verify.

## Features

- **Static typing** with local type inference
- **OOP** — classes, interfaces, multiple inheritance, virtual dispatch
- **Enums** — named variants with pattern matching support
- **Pattern matching** — `match` expressions with literal, enum, wildcard, binding, and guard patterns
- **String interpolation** — `$"Hello {name}!"` syntax for embedding expressions in strings
- **Closures & lambdas** — `|x| x + 1` anonymous functions with variable capture
- **Async/await** — concurrent programming with async functions
- **Ternary expressions** — `value if condition else other` Python-style conditionals
- **Native performance** — compiles to machine code via Cranelift (AOT and JIT)
- **Garbage collected** with deterministic cleanup via `using` blocks
- **Rich standard library** — collections, I/O, networking, JSON, crypto, threading, and more
- **3D game engine** — `nex3d` with sprites, 3D models, animation, audio, lighting, fonts, gamepad input, and more
- **Machine learning** — `torch` library with tensors, neural networks, CUDA, and training utilities
- **Declarative UI** — `.nexui` markup for desktop GUI applications
- **Package manager** — install libraries from GitHub with `nex install`
- **Tooling** — formatter, linter, REPL, LSP, and VS Code extension

## Quick Start

```
nex new hello
cd hello
nex run
```

This creates a new project with a `main.nex` entry point and runs it via JIT.

## Hello World

```
def main() -> Unit {
    name = "world"
    println($"hello, {name}!")
    println("value:", 42, "pi:", 3.14)
    return
}
```

## CLI Usage

```
nex <command> [args]

Flags:
  --version, -V               Print version and exit

Commands:
  new <name> [--lib]          Create a new project (--lib for a library)
  build [path]                Compile to a native executable (AOT)
  build --lib [path]          Compile a library to a shared library (DLL)
  run [path] [-- args]        JIT compile and execute
  <file>.nex [args]           Shorthand for: nex run <file>.nex -- [args]
  install <user/repo[:ver]>   Install a library from GitHub
  uninstall <name>            Remove a library from project.toml
  list                        List project dependencies
  fmt <file>                  Format source code
  lint <file>                 Run linter
  repl                        Interactive REPL
  clean                       Remove build artifacts
```

## Standard Libraries

Opt-in libraries that ship with Nex. Add them to your `project.toml` under `[libs]`:

| Library | Description |
|---------|-------------|
| **crypto** | SHA-256, SHA-512, MD5, HMAC, Base64, secure random bytes |
| **http** | HTTP client for GET/POST with response status, body, and headers |
| **regex** | Regular expression compilation, matching, searching, and replacement |
| **torch** | PyTorch tensors, neural networks, loss functions, optimizers, CUDA, training utilities |
| **nex_ui** | Desktop GUI (WGPU) and terminal TUI with flexbox layout, canvas, and dialogs |
| **nex3d** | 3D game engine — sprites, models, animation, audio, lighting, fonts, gamepad, render targets |

## Project Structure

A Nex project uses a `project.toml` manifest:

```toml
name = "myapp"
version = "0.1.0"
entry = "src/main.nex"
```

Source files use the `.nex` extension. Declarative UI markup uses `.nexui`.

## Building from Source

Requires Rust 1.75+ and Cargo.

```
cargo xtask deploy
```

This builds all binaries (`nex`, `nexc`, `nex-lsp`) and native DLLs, then copies them to `nex/bin/` and `libs/`.

To create a GitHub release:

```
cargo xtask release [--draft]
```

## Examples

See the [examples/](examples/) directory:

- **hello** — language basics (factorial, FizzBuzz, string concatenation)
- **console_showcase** — imports, interfaces, classes, control flow
- **game_showcase** — 3D engine with window, camera, keyboard input, and HUD
- **ui_declarative** — desktop GUI with `.nexui` markup and styling
- **gpt2_transformer** — GPT-2 language model using the `torch` library
- **cuda_test** — CUDA device detection and GPU tensor operations

## Documentation

Full docs at [docs/site/](docs/site/index.html) covering the language, standard library, tooling, and all libraries.

## License

Apache-2.0 OR MIT
