# Nex VS Code Extension

Supports `.nex` files with:
- Syntax highlighting
- Nex LSP integration (`nex-lsp`)
- Commands: Build/Run/Test/Format
- Hover documentation for all 22 standard library modules including `std.ui` and `std.torch`, and libraries such as `crypto`, `http`, `net`, and `nex3d`

## Development
1. `cd vscode-extension`
2. `npm install`
3. `npm run compile`
4. Open this folder in VS Code
5. Run `Run Extension` (F5) to launch a VS Code Extension Development Host

## LSP Server
The extension will try to launch `nex-lsp` via:
1. Setting `nex.lspPath` (absolute path)
2. Workspace binaries:
   - `target/debug/nex-lsp`
   - `target/release/nex-lsp`
   - `bin/nex-lsp`
3. PATH (`nex-lsp`)

## Settings
- `nex.enableLsp`: enable/disable LSP
- `nex.lspPath`: set explicit path to nex-lsp
- `nex.trace.server`: off/messages/verbose
