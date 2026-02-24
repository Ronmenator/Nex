# Nex

A compiled, statically-typed programming language with classes, interfaces, multiple inheritance, and native code generation. Designed for both rapid prototyping and production applications.

## Features

- **Static typing** with local type inference
- **OOP** — classes, interfaces, multiple inheritance, virtual dispatch
- **Native performance** — compiles to machine code via Cranelift (AOT and JIT)
- **Garbage collected** with deterministic cleanup via `using` blocks
- **Rich standard library** — collections, I/O, networking, JSON, crypto, threading, and more
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
    println("hello, world")
    return
}
```

## CLI Usage

```
nex <command> [args]

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
cargo build --release -p nex -p nexc -p nex_lsp
```

Or use the built-in install command to build and copy binaries to `nex/bin/`:

```
nex install
```

## Examples

See the [examples/](examples/) directory:

- **hello** — language basics (factorial, FizzBuzz, string concatenation)
- **console_showcase** — imports, interfaces, classes, control flow
- **game_showcase** — 3D engine with window, camera, keyboard input, and HUD
- **ui_declarative** — desktop GUI with `.nexui` markup and styling
- **gpt2_transformer** — GPT-2 language model using the `torch` library

## License

Apache-2.0 OR MIT
