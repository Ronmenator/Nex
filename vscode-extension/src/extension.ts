import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function isWindows(): boolean {
  return process.platform === "win32";
}

function exeName(base: string): string {
  return isWindows() ? `${base}.exe` : base;
}

function fileExists(p: string): boolean {
  try {
    fs.accessSync(p, fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

/**
 * Build a list of candidate directories where Nex binaries may live,
 * relative to the workspace root and well-known install locations.
 */
function candidateDirs(workspaceRoot: string): string[] {
  const dirs = [
    path.join(workspaceRoot, "nex", "bin"),
    path.join(workspaceRoot, "target", "debug"),
    path.join(workspaceRoot, "target", "release"),
    path.join(workspaceRoot, "bin"),
    workspaceRoot,
  ];

  // Check the standard install location (~/.nex/bin)
  const home = process.env.USERPROFILE || process.env.HOME;
  if (home) {
    dirs.push(path.join(home, ".nex", "bin"));
  }

  return dirs;
}

/**
 * Resolve the nex-lsp executable path:
 *   1) User setting (explicit absolute path)
 *   2) Workspace-local candidates (nex/bin, target/debug, target/release, bin/)
 *   3) PATH (fallback)
 */
function resolveServerPath(workspaceRoot: string | undefined): string {
  const cfg = vscode.workspace.getConfiguration("nex");
  const configured = (cfg.get<string>("lspPath") ?? "").trim();
  if (configured.length > 0) {
    return configured;
  }

  const names = [exeName("nex-lsp"), exeName("nex_lsp")];

  if (workspaceRoot) {
    for (const dir of candidateDirs(workspaceRoot)) {
      for (const name of names) {
        const p = path.join(dir, name);
        if (fileExists(p)) return p;
      }
    }
  }

  return exeName("nex-lsp");
}

/**
 * Resolve the nex CLI executable path.
 * Checks workspace-local directories first, then falls back to PATH.
 */
function resolveNexPath(workspaceRoot: string | undefined): string {
  const name = exeName("nex");
  if (workspaceRoot) {
    for (const dir of candidateDirs(workspaceRoot)) {
      const p = path.join(dir, name);
      if (fileExists(p)) return p;
    }
  }
  return name;
}

function createTerminal(name: string): vscode.Terminal {
  const existing = vscode.window.terminals.find(t => t.name === name);
  return existing ?? vscode.window.createTerminal(name);
}

function runNexCommand(subcommand: string): void {
  const folder = vscode.workspace.workspaceFolders?.[0];
  const cwd = folder?.uri.fsPath;
  const nexPath = resolveNexPath(cwd);

  const terminal = createTerminal("Nex");
  if (cwd) {
    terminal.sendText(isWindows() ? `cd /d "${cwd}"` : `cd "${cwd}"`);
  }
  const quoted = nexPath.includes(" ") ? `"${nexPath}"` : nexPath;
  terminal.sendText(`${quoted} ${subcommand}`);
  terminal.show();
}

export async function activate(context: vscode.ExtensionContext) {
  const cfg = vscode.workspace.getConfiguration("nex");
  const enableLsp = cfg.get<boolean>("enableLsp") ?? true;

  // Commands
  context.subscriptions.push(
    vscode.commands.registerCommand("nex.build", () => runNexCommand("build")),
    vscode.commands.registerCommand("nex.run", () => runNexCommand("run")),
    vscode.commands.registerCommand("nex.test", () => runNexCommand("test")),
    vscode.commands.registerCommand("nex.format", () => runNexCommand("fmt")),
    vscode.commands.registerCommand("nex.newProject", async () => {
      const name = await vscode.window.showInputBox({ prompt: "Project name", placeHolder: "my_app" });
      if (name) runNexCommand(`new ${name}`);
    }),
    vscode.commands.registerCommand("nex.newLibrary", async () => {
      const name = await vscode.window.showInputBox({ prompt: "Library name", placeHolder: "my_lib" });
      if (name) runNexCommand(`new ${name} --lib`);
    }),
    vscode.commands.registerCommand("nex.installLib", async () => {
      const spec = await vscode.window.showInputBox({
        prompt: "Library to install (user/repo or user/repo:version)",
        placeHolder: "nexlang/nex3d:0.1.0",
      });
      if (spec) runNexCommand(`install ${spec}`);
    }),
    vscode.commands.registerCommand("nex.listLibs", () => runNexCommand("list")),
  );

  // Hover provider for keywords, types, and stdlib functions
  context.subscriptions.push(
    vscode.languages.registerHoverProvider("nex", { provideHover: nexHoverProvider }),
    vscode.languages.registerHoverProvider("nexui", { provideHover: nexHoverProvider })
  );

  if (!enableLsp) {
    return;
  }

  const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  const serverPath = resolveServerPath(workspaceRoot);

  const trace = cfg.get<string>("trace.server") ?? "off";

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
    transport: TransportKind.stdio,
    options: workspaceRoot ? { cwd: workspaceRoot } : undefined,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "nex" },
      { scheme: "file", language: "nexui" },
    ],
    synchronize: {
      fileEvents: [
        vscode.workspace.createFileSystemWatcher("**/*.nex"),
        vscode.workspace.createFileSystemWatcher("**/*.nexui"),
      ],
    },
    traceOutputChannel: trace === "off" ? undefined : vscode.window.createOutputChannel("Nex LSP Trace"),
  };

  client = new LanguageClient(
    "nexLsp",
    "Nex Language Server",
    serverOptions,
    clientOptions
  );

  try {
    await client.start();
    vscode.window.setStatusBarMessage(`Nex: LSP started (${serverPath})`, 5000);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    vscode.window.showWarningMessage(
      `Nex LSP failed to start: ${msg}. Syntax highlighting still works.`
    );
    client = undefined;
  }

  context.subscriptions.push({
    dispose: () => {
      void client?.stop();
    },
  });
}

// ---------------------------------------------------------------------------
// Hover documentation
// ---------------------------------------------------------------------------

const DOCS: Record<string, string> = {
  // Keywords
  "if": "**if** statement\n\n```nex\nif (condition) { ... } else { ... }\n```\nConditionally execute a block.",
  "else": "**else** clause\n\nFollows an `if` block to provide an alternative branch.",
  "while": "**while** loop\n\n```nex\nwhile (condition) { ... }\n```\nRepeatedly execute the body while the condition is true.",
  "for": "**for** loop (C-style)\n\n```nex\nfor (init; condition; step) { ... }\n```",
  "return": "**return** statement\n\nExit the current function, optionally with a value.",
  "break": "**break** — exit the enclosing loop.",
  "continue": "**continue** — skip to the next iteration of the enclosing loop.",
  "try": "**try / catch / finally**\n\n```nex\ntry { ... } catch (e: Type) { ... } finally { ... }\n```\nStructured exception handling.",
  "catch": "**catch** clause — handles exceptions thrown inside a `try` block.",
  "finally": "**finally** clause — always executes after `try`/`catch`, even on exception.",
  "throw": "**throw** expression\n\nRaise an exception object.",
  "using": "**using** block\n\n```nex\nusing (resource = expr) { ... }\n```\nAcquires a `Disposable` resource and calls `dispose()` when the block exits.",
  "class": "**class** declaration\n\n```nex\npublic class MyClass : BaseClass { ... }\n```\nDefines a reference type with fields, methods, and optional inheritance.",
  "interface": "**interface** declaration\n\n```nex\npublic interface Greeter { def greet(name: String) -> String }\n```\nDeclares method signatures without implementation.",
  "struct": "**struct** declaration\n\n```nex\npublic struct Vec2 { x: Float; y: Float }\n```\nDefines a value type stored inline. No inheritance, but may implement interfaces.",
  "def": "**def** — define a function or method.\n\n```nex\ndef add(a: Int, b: Int) -> Int { return a + b }\n```",
  "var": "**var** — declare a dynamic variable of type `Var`.\n\n```nex\nvar x = 1\nx = \"hello\"  // allowed at runtime\n```",
  "public": "**public** — export a declaration across modules.\n\nDeclarations are module-internal by default.",
  "import": "**import** — bring a module into scope.\n\n```nex\nimport std.math\nimport mylib.utils as utils\n```",
  "from": "**from** ... **import** — selectively import symbols.\n\n```nex\nfrom std.io import read_line, file_exists\n```",
  "as": "**as** — alias an imported module.\n\n```nex\nimport std.json as json\n```",
  "virtual": "**virtual** — enable dynamic dispatch on a method.",
  "override": "**override** — override a virtual method from a base class.",
  "shared": "**shared** — mark a base class as shared in diamond inheritance.\n\n```nex\nclass B : shared A { }\n```",
  "alias": "**alias** — create a local alias for an inherited member.\n\n```nex\nalias fooFromB = B::foo\n```",
  "self": "**self** — reference to the current object instance inside a method.",
  "static": "**static** — declare a method or operator that does not require an instance.",
  "operator": "**operator** — overload an operator for a class or struct.\n\n```nex\npublic class Vec2 {\n    x: Double; y: Double\n    operator +(other: Vec2) -> Vec2 { ... }\n}\n```",
  "null": "**null** — the null literal, assignable to any nullable type `T?`.",
  "true": "**true** — boolean literal.",
  "false": "**false** — boolean literal.",
  "async": "**async** — Declares an asynchronous function.\n\n```nex\nasync def fetch(url: String) -> String {\n    // async operation\n    return result\n}\n```\n\nAsync functions return a future. Use `await` to get the result.",
  "await": "**await** — Waits for an async operation to complete.\n\n```nex\nresult = await fetch_data(url)\n```\n\nCan only be used to await async function calls.",
  "enum": "**enum** — Declares an enumerated type with named variants.\n\n```nex\nenum Color {\n    Red,\n    Green,\n    Blue\n}\n```\n\nAccess variants with `Color.Red`. Use with `match` for pattern matching.",
  "match": "**match** — Pattern matching expression. Compares a value against patterns.\n\n```nex\nmatch value {\n    1 -> println(\"one\")\n    2 -> println(\"two\")\n    _ -> println(\"other\")\n}\n```\n\nSupports literal, enum variant, wildcard (`_`), and binding patterns. Guards: `pattern if condition -> body`",
  "Reflect": "**Reflect** — Built-in reflection API for runtime type introspection.\n\n```nex\nvar ti = Reflect.findType(\"Animal\")\nprintln(Reflect.typeName(ti))\nprintln(Reflect.fieldCount(ti))\n```\n\nMethods: `findType`, `typeName`, `typeModule`, `typeKind`, `fieldCount`, `fieldName`, `fieldType`, `methodCount`, `methodName`, `methodReturnType`, `implements`, `interfaces`, `typeCount`, `typeNameAt`, `isReflectable`, `invoke`, `createInstance`.\n\nTypes must be marked `[Reflectable]` to expose fields and methods.",
  "Reflectable": "**[Reflectable]** — Attribute that enables full runtime reflection on a type.\n\n```nex\n[Reflectable]\nclass Animal {\n    name: String\n    age: Int\n}\n```\n\nWithout this attribute, the type is registered but its fields/methods are not exposed via `Reflect` queries.",

  // Built-in types
  "Bool": "**Bool** — boolean value type (`true` / `false`).",
  "Byte": "**Byte** — unsigned 8-bit integer (u8).",
  "Int": "**Int** — signed 32-bit integer (i32).",
  "Int64": "**Int64** — signed 64-bit integer (i64).",
  "Float": "**Float** — 32-bit floating point (f32).",
  "Double": "**Double** — 64-bit floating point (f64). Default for float literals.",
  "Char": "**Char** — single Unicode character.",
  "String": "**String** — immutable, heap-allocated, GC-managed string. Derived from `Object`.",
  "Unit": "**Unit** — the void return type. Functions that return nothing return `Unit`.",
  "Var": "**Var** — runtime dynamic type. Holds any value; operators and members resolved at runtime.",
  "Object": "**Object** — universal base class for all reference types.",
  "Int8": "**Int8** — signed 8-bit integer (-128 to 127).",
  "Int16": "**Int16** — signed 16-bit integer (-32768 to 32767).",
  "Int32": "**Int32** — signed 32-bit integer. Alias for `Int`.",
  "Short": "**Short** — signed 16-bit integer. Alias for `Int16`.",
  "UInt": "**UInt** — unsigned 32-bit integer (0 to 4294967295).",
  "UInt8": "**UInt8** — unsigned 8-bit integer (0 to 255). Alias for `Byte`.",
  "UInt16": "**UInt16** — unsigned 16-bit integer (0 to 65535).",
  "UInt32": "**UInt32** — unsigned 32-bit integer. Alias for `UInt`.",
  "UInt64": "**UInt64** — unsigned 64-bit integer (0 to 18446744073709551615).",
  "UShort": "**UShort** — unsigned 16-bit integer. Alias for `UInt16`.",
  "Long": "**Long** — signed 64-bit integer. Alias for `Int64`.",
  "ULong": "**ULong** — unsigned 64-bit integer. Alias for `UInt64`.",
  "Float32": "**Float32** — 32-bit floating point. Alias for `Float`.",
  "Float64": "**Float64** — 64-bit floating point. Alias for `Double`.",
  "Disposable": "**Disposable** — interface with `dispose()` for deterministic resource cleanup via `using`.",

  // std.core
  "print": "**print**(x: Var) -> Unit\n\n`std.core` — Print a value to stdout (no newline).",
  "println": "**println**(x: Var) -> Unit\n\n`std.core` — Print a value to stdout followed by a newline.",

  // std.math
  "abs_int": "**abs_int**(v: Int) -> Int\n\n`std.math` — Absolute value of an integer.",
  "abs_float": "**abs_float**(v: Double) -> Double\n\n`std.math` — Absolute value of a float.",
  "min_int": "**min_int**(a: Int, b: Int) -> Int\n\n`std.math` — Return the smaller of two integers.",
  "max_int": "**max_int**(a: Int, b: Int) -> Int\n\n`std.math` — Return the larger of two integers.",
  "min_float": "**min_float**(a: Double, b: Double) -> Double\n\n`std.math`",
  "max_float": "**max_float**(a: Double, b: Double) -> Double\n\n`std.math`",
  "clamp_int": "**clamp_int**(v: Int, lo: Int, hi: Int) -> Int\n\n`std.math` — Clamp `v` to the range [lo, hi].",
  "clamp_float": "**clamp_float**(v: Double, lo: Double, hi: Double) -> Double\n\n`std.math`",
  "floor": "**floor**(v: Double) -> Double\n\n`std.math` — Round down to the nearest integer.",
  "ceil": "**ceil**(v: Double) -> Double\n\n`std.math` — Round up to the nearest integer.",
  "round": "**round**(v: Double) -> Double\n\n`std.math` — Round to the nearest integer.",
  "sqrt": "**sqrt**(v: Double) -> Double\n\n`std.math` — Square root.",
  "pow": "**pow**(base: Double, exp: Double) -> Double\n\n`std.math` — Raise `base` to the power `exp`.",
  "sin": "**sin**(v: Double) -> Double\n\n`std.math` — Sine (radians).",
  "cos": "**cos**(v: Double) -> Double\n\n`std.math` — Cosine (radians).",
  "tan": "**tan**(v: Double) -> Double\n\n`std.math` — Tangent (radians).",
  "log": "**log**(v: Double) -> Double\n\n`std.math` — Natural logarithm (ln).",
  "log2": "**log2**(v: Double) -> Double\n\n`std.math` — Base-2 logarithm.",
  "log10": "**log10**(v: Double) -> Double\n\n`std.math` — Base-10 logarithm.",
  "exp": "**exp**(v: Double) -> Double\n\n`std.math` — e raised to the power `v`.",
  "random": "**random**() -> Double\n\n`std.math` — Random float in [0.0, 1.0).",
  "random_range": "**random_range**(lo: Int, hi: Int) -> Int\n\n`std.math` — Random integer in [lo, hi).",

  // std.string
  "str_trim": "**str_trim**(s: String) -> String\n\n`std.string` — Remove leading and trailing whitespace.",
  "str_trim_start": "**str_trim_start**(s: String) -> String\n\n`std.string` — Remove leading whitespace.",
  "str_trim_end": "**str_trim_end**(s: String) -> String\n\n`std.string` — Remove trailing whitespace.",
  "starts_with": "**starts_with**(s: String, prefix: String) -> Bool\n\n`std.string`",
  "ends_with": "**ends_with**(s: String, suffix: String) -> Bool\n\n`std.string`",
  "contains": "**contains**(s: String, needle: String) -> Bool\n\n`std.string` — Check if `s` contains `needle`.",
  "index_of": "**index_of**(s: String, needle: String) -> Int\n\n`std.string` — First index of `needle` in `s`, or -1.",
  "str_replace": "**str_replace**(s: String, old: String, new: String) -> String\n\n`std.string` — Replace all occurrences.",
  "to_upper": "**to_upper**(s: String) -> String\n\n`std.string` — Convert to uppercase.",
  "to_lower": "**to_lower**(s: String) -> String\n\n`std.string` — Convert to lowercase.",
  "str_repeat": "**str_repeat**(s: String, count: Int) -> String\n\n`std.string` — Repeat `s` `count` times.",
  "char_at": "**char_at**(s: String, index: Int) -> Int\n\n`std.string` — Character code at position, or -1.",
  "str_reverse": "**str_reverse**(s: String) -> String\n\n`std.string` — Reverse the string.",
  "str_split": "**str_split**(s: String, delim: String) -> String\n\n`std.string` — Split by delimiter (NUL-separated result).",

  // std.convert
  "parse_int": "**parse_int**(s: String) -> Int\n\n`std.convert` — Parse a string to an integer (0 on failure).",
  "parse_float": "**parse_float**(s: String) -> Double\n\n`std.convert` — Parse a string to a float (0.0 on failure).",
  "parse_bool": "**parse_bool**(s: String) -> Bool\n\n`std.convert` — Parse \"true\"/\"1\"/\"yes\" to true.",
  "char_to_str": "**char_to_str**(c: Int) -> String\n\n`std.convert` — Character code to a one-character string.",
  "str_to_chars": "**str_to_chars**(s: String) -> Int[]\n\n`std.convert` — Convert a string to an array of character codes.",

  // std.env
  "env_get": "**env_get**(name: String) -> String\n\n`std.env` — Get an environment variable (empty if unset).",
  "env_set": "**env_set**(name: String, value: String) -> Unit\n\n`std.env` — Set an environment variable.",
  "env_has": "**env_has**(name: String) -> Bool\n\n`std.env` — Check if an environment variable exists.",
  "env_args_count": "**env_args_count**() -> Int\n\n`std.env` — Number of command-line arguments.",
  "env_args_get": "**env_args_get**(index: Int) -> String\n\n`std.env` — Get a command-line argument by index.",
  "env_cwd": "**env_cwd**() -> String\n\n`std.env` — Current working directory.",

  // std.time
  "now_millis": "**now_millis**() -> Int\n\n`std.time` — Unix epoch timestamp in milliseconds.",
  "now_nanos": "**now_nanos**() -> Int\n\n`std.time` — Unix epoch timestamp in nanoseconds.",
  "sleep_millis": "**sleep_millis**(ms: Int) -> Unit\n\n`std.time` — Sleep the current thread.",
  "elapsed_millis": "**elapsed_millis**(start: Int) -> Int\n\n`std.time` — Milliseconds elapsed since `start`.",

  // std.io
  "read_line": "**read_line**() -> String\n\n`std.io` — Read a line from stdin.",
  "file_exists": "**file_exists**(path: String) -> Bool\n\n`std.io` — Check if a file exists.",
  "file_delete": "**file_delete**(path: String) -> Bool\n\n`std.io` — Delete a file.",
  "file_rename": "**file_rename**(from: String, to: String) -> Bool\n\n`std.io`",
  "file_copy": "**file_copy**(from: String, to: String) -> Bool\n\n`std.io`",
  "file_size": "**file_size**(path: String) -> Int\n\n`std.io` — File size in bytes (-1 on error).",
  "file_read_bytes": "**file_read_bytes**(path: String, buf: Var, max_len: Int) -> Int\n\n`std.io` — Read raw bytes from a file into a buffer. Returns number of bytes read.",
  "file_write_bytes": "**file_write_bytes**(path: String, buf: Var, len: Int) -> Bool\n\n`std.io` — Write raw bytes from a buffer to a file.",
  "mkdir": "**mkdir**(path: String) -> Bool\n\n`std.io` — Create directory (and parents).",
  "list_dir": "**list_dir**(path: String) -> String\n\n`std.io` — List directory entries (newline-separated).",

  // std.path
  "path_join": "**path_join**(a: String, b: String) -> String\n\n`std.path` — Join two path segments.",
  "path_parent": "**path_parent**(p: String) -> String\n\n`std.path` — Parent directory.",
  "path_file_name": "**path_file_name**(p: String) -> String\n\n`std.path` — File name component.",
  "path_extension": "**path_extension**(p: String) -> String\n\n`std.path` — File extension (without dot).",
  "path_stem": "**path_stem**(p: String) -> String\n\n`std.path` — File name without extension.",
  "path_is_absolute": "**path_is_absolute**(p: String) -> Bool\n\n`std.path`",
  "path_normalize": "**path_normalize**(p: String) -> String\n\n`std.path` — Canonicalize the path.",
  "path_separator": "**path_separator**() -> String\n\n`std.path` — OS path separator (`/` or `\\\\`).",

  // std.json
  "json_parse": "**json_parse**(text: String) -> Var\n\n`std.json` — Parse JSON text into an opaque handle.",
  "json_stringify": "**json_stringify**(handle: Var) -> String\n\n`std.json` — Serialize a JSON handle to text.",
  "json_get_string": "**json_get_string**(handle: Var, key: String) -> String\n\n`std.json`",
  "json_get_int": "**json_get_int**(handle: Var, key: String) -> Int\n\n`std.json`",
  "json_get_float": "**json_get_float**(handle: Var, key: String) -> Double\n\n`std.json`",
  "json_get_bool": "**json_get_bool**(handle: Var, key: String) -> Bool\n\n`std.json`",

  // std.regex
  "regex_new": "**regex_new**(pattern: String) -> Var\n\n`std.regex` — Compile a regular expression.",
  "regex_is_match": "**regex_is_match**(re: Var, s: String) -> Bool\n\n`std.regex` — Test if the string matches.",
  "regex_find": "**regex_find**(re: Var, s: String) -> String\n\n`std.regex` — Find first match.",
  "regex_replace": "**regex_replace**(re: Var, s: String, rep: String) -> String\n\n`std.regex` — Replace all matches.",
  "regex_free": "**regex_free**(re: Var) -> Unit\n\n`std.regex` — Free the compiled regex.",

  // std.process
  "process_exec": "**process_exec**(cmd: String) -> Int\n\n`std.process` — Run a shell command, return exit code.",
  "process_exec_output": "**process_exec_output**(cmd: String) -> String\n\n`std.process` — Run a shell command, capture stdout.",
  "process_exit": "**process_exit**(code: Int) -> Unit\n\n`std.process` — Terminate the process.",
  "process_pid": "**process_pid**() -> Int\n\n`std.process` — Current process ID.",
  "process_spawn": "**process_spawn**(cmd: String) -> Var\n\n`std.process` — Spawn a background process.",
  "process_wait": "**process_wait**(handle: Var) -> Int\n\n`std.process` — Wait for a spawned process to exit.",

  // std.net
  "tcp_connect": "**tcp_connect**(host: String, port: Int) -> Var\n\n`std.net` — Open a TCP connection.",
  "tcp_close": "**tcp_close**(handle: Var) -> Unit\n\n`std.net` — Close a TCP connection.",
  "tcp_send": "**tcp_send**(handle: Var, data: Var, len: Int) -> Int\n\n`std.net` — Send bytes over TCP.",
  "tcp_recv": "**tcp_recv**(handle: Var, buf: Var, max: Int) -> Int\n\n`std.net` — Receive bytes over TCP.",
  "tcp_listen": "**tcp_listen**(host: String, port: Int) -> Var\n\n`std.net` — Start a TCP listener.",
  "tcp_accept": "**tcp_accept**(listener: Var) -> Var\n\n`std.net` — Accept a TCP connection.",
  "udp_bind": "**udp_bind**(host: String, port: Int) -> Var\n\n`std.net` — Bind a UDP socket.",
  "udp_close": "**udp_close**(handle: Var) -> Unit\n\n`std.net`",
  "udp_send": "**udp_send**(handle: Var, data: Var, len: Int, host: String, port: Int) -> Int\n\n`std.net` — Send bytes over UDP to a destination.",
  "udp_recv": "**udp_recv**(handle: Var, buf: Var, max: Int) -> Int\n\n`std.net` — Receive bytes from a UDP socket.",

  // std.http
  "http_get": "**http_get**(url: String) -> Var\n\n`std.http` — HTTP GET request, returns response handle.",
  "http_post": "**http_post**(url: String, body: String, content_type: String) -> Var\n\n`std.http` — HTTP POST request.",
  "http_response_status": "**http_response_status**(resp: Var) -> Int\n\n`std.http` — HTTP status code (200, 404, etc.).",
  "http_response_body": "**http_response_body**(resp: Var) -> String\n\n`std.http` — Response body as string.",
  "http_response_header": "**http_response_header**(resp: Var, name: String) -> String\n\n`std.http`",
  "http_response_free": "**http_response_free**(resp: Var) -> Unit\n\n`std.http` — Free the response handle.",

  // std.threading
  "thread_spawn": "**thread_spawn**(func: Var) -> Var\n\n`std.threading` — Spawn a new thread.",
  "thread_join": "**thread_join**(handle: Var) -> Int\n\n`std.threading` — Wait for a thread to finish.",
  "thread_sleep": "**thread_sleep**(ms: Int) -> Unit\n\n`std.threading` — Sleep the current thread.",
  "thread_current_id": "**thread_current_id**() -> Int\n\n`std.threading` — Current thread ID.",
  "mutex_new": "**mutex_new**() -> Var\n\n`std.threading` — Create a new mutex.",
  "mutex_lock": "**mutex_lock**(handle: Var) -> Unit\n\n`std.threading` — Acquire the mutex.",
  "mutex_unlock": "**mutex_unlock**(handle: Var) -> Unit\n\n`std.threading` — Release the mutex.",
  "mutex_free": "**mutex_free**(handle: Var) -> Unit\n\n`std.threading` — Free the mutex.",

  // std.crypto
  "sha256": "**sha256**(s: String) -> String\n\n`std.crypto` — SHA-256 hex digest.",
  "sha512": "**sha512**(s: String) -> String\n\n`std.crypto` — SHA-512 hex digest.",
  "md5": "**md5**(s: String) -> String\n\n`std.crypto` — MD5 hex digest.",
  "random_bytes": "**random_bytes**(buf: Var, len: Int) -> Unit\n\n`std.crypto` — Fill buffer with cryptographic random bytes.",
  "base64_encode": "**base64_encode**(s: String) -> String\n\n`std.crypto` — Base64 encode.",
  "base64_decode": "**base64_decode**(s: String) -> String\n\n`std.crypto` — Base64 decode.",
  "hmac_sha256": "**hmac_sha256**(key: String, msg: String) -> String\n\n`std.crypto` — HMAC-SHA256 hex digest.",

  // std.logging
  "log_debug": "**log_debug**(msg: String) -> Unit\n\n`std.logging` — Log at DEBUG level.",
  "log_info": "**log_info**(msg: String) -> Unit\n\n`std.logging` — Log at INFO level.",
  "log_warn": "**log_warn**(msg: String) -> Unit\n\n`std.logging` — Log at WARN level.",
  "log_error": "**log_error**(msg: String) -> Unit\n\n`std.logging` — Log at ERROR level.",
  "log_set_level": "**log_set_level**(level: Int) -> Unit\n\n`std.logging` — Set minimum log level (0=debug, 1=info, 2=warn, 3=error).",
  "log_with_tag": "**log_with_tag**(tag: String, msg: String) -> Unit\n\n`std.logging` — Log with a custom tag.",

  // std.testing
  "assert": "**assert**(condition: Bool, msg: String) -> Unit\n\n`std.testing` — Abort if condition is false.",
  "assert_eq_int": "**assert_eq_int**(a: Int, b: Int, msg: String) -> Unit\n\n`std.testing` — Abort if a != b.",
  "assert_eq_str": "**assert_eq_str**(a: String, b: String, msg: String) -> Unit\n\n`std.testing`",
  "assert_eq_float": "**assert_eq_float**(a: Double, b: Double, msg: String) -> Unit\n\n`std.testing`",
  "assert_eq_bool": "**assert_eq_bool**(a: Bool, b: Bool, msg: String) -> Unit\n\n`std.testing`",
  "assert_ne_int": "**assert_ne_int**(a: Int, b: Int, msg: String) -> Unit\n\n`std.testing` — Abort if a == b.",
  "assert_ne_str": "**assert_ne_str**(a: String, b: String, msg: String) -> Unit\n\n`std.testing`",
  "assert_true": "**assert_true**(condition: Bool, msg: String) -> Unit\n\n`std.testing`",

  // std.collections
  "list_new": "**list_new**() -> Var\n\n`std.collections` — Create a new empty List.",
  "list_add": "**list_add**(list: Var, item: Var) -> Unit\n\n`std.collections` — Append an item to the list.",
  "list_get": "**list_get**(list: Var, index: Int) -> Var\n\n`std.collections` — Get item at index.",
  "list_set": "**list_set**(list: Var, index: Int, value: Var) -> Unit\n\n`std.collections` — Set item at index.",
  "list_length": "**list_length**(list: Var) -> Int\n\n`std.collections` — Number of items in the list.",
  "list_remove": "**list_remove**(list: Var, index: Int) -> Var\n\n`std.collections` — Remove and return item at index.",
  "list_sort_int": "**list_sort_int**(list: Var) -> Unit\n\n`std.collections` — Sort an integer list in-place.",
  "list_reverse": "**list_reverse**(list: Var) -> Unit\n\n`std.collections` — Reverse list in-place.",
  "list_clear": "**list_clear**(list: Var) -> Unit\n\n`std.collections` — Remove all items from the list.",
  "list_contains_int": "**list_contains_int**(list: Var, value: Int) -> Bool\n\n`std.collections` — Check if list contains value.",
  "list_index_of_int": "**list_index_of_int**(list: Var, value: Int) -> Int\n\n`std.collections` — Find index of value, or -1.",
  "map_new": "**map_new**() -> Var\n\n`std.collections` — Create a new empty Map.",
  "map_put": "**map_put**(map: Var, key: Var, value: Var) -> Unit\n\n`std.collections` — Insert or update a key-value pair.",
  "map_get": "**map_get**(map: Var, key: Var) -> Var\n\n`std.collections` — Get value by key.",
  "map_try_get": "**map_try_get**(map: Var, key: Var) -> Var\n\n`std.collections` — Get value by key, returns null if not found.",
  "map_contains": "**map_contains**(map: Var, key: Var) -> Bool\n\n`std.collections` — Check if a key exists.",
  "map_remove": "**map_remove**(map: Var, key: Var) -> Var\n\n`std.collections` — Remove a key-value pair.",
  "map_size": "**map_size**(map: Var) -> Int\n\n`std.collections` — Number of entries in the map.",
  "set_new": "**set_new**() -> Var\n\n`std.collections` — Create a new empty Set.",
  "set_add": "**set_add**(set: Var, value: Int) -> Unit\n\n`std.collections` — Add a value to the set.",
  "set_contains": "**set_contains**(set: Var, value: Int) -> Bool\n\n`std.collections` — Check if value is in the set.",
  "set_remove": "**set_remove**(set: Var, value: Int) -> Unit\n\n`std.collections` — Remove a value from the set.",
  "set_size": "**set_size**(set: Var) -> Int\n\n`std.collections` — Number of items in the set.",

  // std.ui (optional)
  "ui_app_create": "**ui_app_create**(title: String, width: Int, height: Int) -> Int64\n\n`std.ui` — Create a UI application with a window title and size. Returns an app handle. Only one app may exist at a time.\n\n```nex\napp = ui_app_create(\"My App\", 800, 600)\n```",
  "ui_app_set_backend": "**ui_app_set_backend**(app: Int64, backend: Int) -> Unit\n\n`std.ui` — Set the rendering backend before `ui_app_run`. `0` = WGPU (GPU desktop, default), `1` = Terminal (TUI via crossterm).",
  "ui_app_set_root": "**ui_app_set_root**(app: Int64, root: Int64) -> Unit\n\n`std.ui` — Set the root widget of the application. The widget tree renders starting from this root.",
  "ui_app_run": "**ui_app_run**(app: Int64) -> Unit\n\n`std.ui` — Start the event loop. **Blocks** until the window is closed or `ui_app_quit` is called. Events dispatch to registered callbacks.",
  "ui_app_quit": "**ui_app_quit**(app: Int64) -> Unit\n\n`std.ui` — Signal the event loop to stop. The window closes and `ui_app_run` returns.",
  "ui_app_destroy": "**ui_app_destroy**(app: Int64) -> Unit\n\n`std.ui` — Release all resources (widgets, renderer, window). Call after `ui_app_run` returns.",
  "ui_is_running": "**ui_is_running**(app: Int64) -> Bool\n\n`std.ui` — Returns `true` while the app has not been closed or quit.",
  "ui_app_render": "**ui_app_render**(app: Int64) -> Unit\n\n`std.ui` — Trigger an immediate re-render. Called automatically by the event loop; only needed for the polling model.",
  "ui_render": "**ui_render**(app: Int64) -> Unit\n\n`std.ui` — Alias for `ui_app_render`. Trigger an immediate re-render.",
  "ui_poll_event": "**ui_poll_event**(app: Int64) -> Int\n\n`std.ui` — Dequeue the next event. Returns the event type: 0=None, 1=Click, 2=Hover, 4=KeyPress, 6=TextInput, 7=ValueChange, 10=WindowClose, 11=WindowResize.",
  "ui_event_type": "**ui_event_type**(event: Int) -> Int\n\n`std.ui` — Returns the type code of a polled event.",
  "ui_event_widget": "**ui_event_widget**(event: Int) -> Int64\n\n`std.ui` — Returns the widget handle that originated the event, or -1.",

  "ui_text": "**ui_text**(text: String) -> Int64\n\n`std.ui` — Create a static text label widget.\n\n```nex\nlabel = ui_text(\"Hello World\")\n```",
  "ui_button": "**ui_button**(label: String) -> Int64\n\n`std.ui` — Create a clickable button. Default style: blue background, white text, rounded corners.\n\n```nex\nbtn = ui_button(\"Click Me\")\nui_on_click(btn, handler)\n```",
  "ui_text_input": "**ui_text_input**(placeholder: String) -> Int64\n\n`std.ui` — Create an editable text field with placeholder text. Click to focus, type to enter text, Backspace to delete.\n\n```nex\ninput = ui_text_input(\"Enter name...\")\n```",
  "ui_image": "**ui_image**(path: String) -> Int64\n\n`std.ui` — Create an image widget from a file path.",
  "ui_checkbox": "**ui_checkbox**(label: String) -> Int64\n\n`std.ui` — Create a toggleable checkbox with a label. Use `ui_get_value_float` to read state (1.0 = checked).\n\n```nex\ncb = ui_checkbox(\"Enable dark mode\")\n```",
  "ui_slider": "**ui_slider**(min: Double, max: Double) -> Int64\n\n`std.ui` — Create a horizontal slider with a min/max range. Read position with `ui_get_value_float`.\n\n```nex\nslider = ui_slider(0.0, 100.0)\n```",
  "ui_row": "**ui_row**() -> Int64\n\n`std.ui` — Create a horizontal flex container. Children are laid out left-to-right.\n\n```nex\nrow = ui_row()\nui_add_child(row, child1)\nui_add_child(row, child2)\n```",
  "ui_column": "**ui_column**() -> Int64\n\n`std.ui` — Create a vertical flex container. Children are laid out top-to-bottom.\n\n```nex\ncol = ui_column()\nui_set_gap(col, 8.0)\n```",
  "ui_stack": "**ui_stack**() -> Int64\n\n`std.ui` — Create a stacking container where children overlap.",
  "ui_scroll": "**ui_scroll**() -> Int64\n\n`std.ui` — Create a scrollable vertical container.",
  "ui_grid": "**ui_grid**(cols: Int) -> Int64\n\n`std.ui` — Create a CSS-grid container with the given number of equal-width columns.\n\n```nex\ngrid = ui_grid(3)\n```",
  "ui_canvas": "**ui_canvas**(width: Int, height: Int) -> Int64\n\n`std.ui` — Create a fixed-size drawing surface for custom 2D rendering. Use `ui_canvas_*` functions to draw.\n\n```nex\nc = ui_canvas(400, 300)\nui_canvas_fill_rect(c, 0.0, 0.0, 100.0, 50.0, 0xFF0000FF)\n```",

  "ui_add_child": "**ui_add_child**(parent: Int64, child: Int64) -> Unit\n\n`std.ui` — Append a child widget to a container. The child is laid out and rendered within the parent.",
  "ui_remove_child": "**ui_remove_child**(parent: Int64, child: Int64) -> Unit\n\n`std.ui` — Remove a child widget from a container.",
  "ui_set_id": "**ui_set_id**(widget: Int64, id: String) -> Unit\n\n`std.ui` — Assign a string identifier to a widget for debugging or lookup.",
  "ui_get_id": "**ui_get_id**(widget: Int64) -> String\n\n`std.ui` — Read the string identifier of a widget.",

  "ui_set_text": "**ui_set_text**(widget: Int64, text: String) -> Unit\n\n`std.ui` — Set the text content of a Text, Button, TextInput, or Checkbox widget.",
  "ui_get_text": "**ui_get_text**(widget: Int64) -> String\n\n`std.ui` — Read the text content of a widget.",
  "ui_set_visible": "**ui_set_visible**(widget: Int64, visible: Bool) -> Unit\n\n`std.ui` — Show or hide a widget. Hidden widgets do not participate in layout.",
  "ui_set_enabled": "**ui_set_enabled**(widget: Int64, enabled: Bool) -> Unit\n\n`std.ui` — Enable or disable a widget. Disabled widgets ignore clicks.",
  "ui_get_value_float": "**ui_get_value_float**(widget: Int64) -> Double\n\n`std.ui` — Read the numeric value of a Slider (position) or Checkbox (0.0/1.0).",
  "ui_set_value_float": "**ui_set_value_float**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set the numeric value of a Slider or Checkbox.",

  "ui_set_width": "**ui_set_width**(widget: Int64, width: Double) -> Unit\n\n`std.ui` — Set the width in logical pixels.",
  "ui_set_height": "**ui_set_height**(widget: Int64, height: Double) -> Unit\n\n`std.ui` — Set the height in logical pixels.",
  "ui_set_min_width": "**ui_set_min_width**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set the minimum width.",
  "ui_set_min_height": "**ui_set_min_height**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set the minimum height.",
  "ui_set_padding": "**ui_set_padding**(widget: Int64, packed: Int64) -> Unit\n\n`std.ui` — Set padding. Packed as four Int16 values (top, right, bottom, left) in one Int64.",
  "ui_set_margin": "**ui_set_margin**(widget: Int64, packed: Int64) -> Unit\n\n`std.ui` — Set margin. Same packing as padding.",
  "ui_set_bg_color": "**ui_set_bg_color**(widget: Int64, rgba: Int64) -> Unit\n\n`std.ui` — Set background color. RGBA packed as `(R<<24)|(G<<16)|(B<<8)|A`. Example: solid red = `0xFF0000FF`.",
  "ui_set_fg_color": "**ui_set_fg_color**(widget: Int64, rgba: Int64) -> Unit\n\n`std.ui` — Set foreground (text) color. Same RGBA packing.",
  "ui_set_font_size": "**ui_set_font_size**(widget: Int64, size: Double) -> Unit\n\n`std.ui` — Set the font size in logical pixels. Default is 16.0.",
  "ui_set_border": "**ui_set_border**(widget: Int64, width: Double, rgba: Int64) -> Unit\n\n`std.ui` — Set border width and color.",
  "ui_set_border_radius": "**ui_set_border_radius**(widget: Int64, radius: Double) -> Unit\n\n`std.ui` — Set corner radius for rounded borders.",
  "ui_set_flex_grow": "**ui_set_flex_grow**(widget: Int64, grow: Double) -> Unit\n\n`std.ui` — Set flex grow factor. Widgets with grow > 0 expand to fill available space.",
  "ui_set_align_self": "**ui_set_align_self**(widget: Int64, align: Int) -> Unit\n\n`std.ui` — Set cross-axis alignment: 0=Start, 1=Center, 2=End, 3=Stretch.",
  "ui_set_justify_content": "**ui_set_justify_content**(widget: Int64, justify: Int) -> Unit\n\n`std.ui` — Set main-axis distribution: 0=Start, 1=Center, 2=End, 3=SpaceBetween, 4=SpaceAround, 5=SpaceEvenly.",
  "ui_set_align_items": "**ui_set_align_items**(widget: Int64, align: Int) -> Unit\n\n`std.ui` — Set cross-axis alignment for all children: 0=Start, 1=Center, 2=End, 3=Stretch.",
  "ui_set_gap": "**ui_set_gap**(widget: Int64, gap: Double) -> Unit\n\n`std.ui` — Set the spacing between children in a container.\n\n```nex\ncol = ui_column()\nui_set_gap(col, 12.0)\n```",
  "ui_set_max_width": "**ui_set_max_width**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set the maximum width in logical pixels.",
  "ui_set_max_height": "**ui_set_max_height**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set the maximum height in logical pixels.",
  "ui_set_padding_all": "**ui_set_padding_all**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set equal padding on all four sides.",
  "ui_set_margin_all": "**ui_set_margin_all**(widget: Int64, value: Double) -> Unit\n\n`std.ui` — Set equal margin on all four sides.",
  "ui_set_flex_shrink": "**ui_set_flex_shrink**(widget: Int64, shrink: Double) -> Unit\n\n`std.ui` — Set flex shrink factor. Default is 1.0. Use 0.0 to prevent shrinking.",
  "ui_set_border_width": "**ui_set_border_width**(widget: Int64, width: Double) -> Unit\n\n`std.ui` — Set border width in logical pixels.",
  "ui_set_border_color": "**ui_set_border_color**(widget: Int64, rgba: Int64) -> Unit\n\n`std.ui` — Set border color. RGBA packed as `(R<<24)|(G<<16)|(B<<8)|A`.",
  "ui_set_checked": "**ui_set_checked**(widget: Int64, checked: Bool) -> Unit\n\n`std.ui` — Set the checked state of a Checkbox widget.",
  "ui_set_h_align": "**ui_set_h_align**(widget: Int64, align: Int) -> Unit\n\n`std.ui` — Set horizontal alignment when the widget is a child of a Column: 0=Left, 1=Center, 2=Right, 3=Stretch.",
  "ui_set_v_align": "**ui_set_v_align**(widget: Int64, align: Int) -> Unit\n\n`std.ui` — Set vertical alignment when the widget is a child of a Row: 0=Top, 1=Center, 2=Bottom, 3=Stretch.",

  "ui_on_click": "**ui_on_click**(widget: Int64, callback: (Int64, Int64) -> Unit) -> Unit\n\n`std.ui` — Register a click handler. Callback receives `(widget_id, event_kind)`.\n\n```nex\ndef on_click(id: Int64, kind: Int64) {\n    println(\"Clicked!\")\n}\nui_on_click(btn, on_click)\n```",
  "ui_on_change": "**ui_on_change**(widget: Int64, callback: (Int64, Int64) -> Unit) -> Unit\n\n`std.ui` — Register a value-change handler. Fires on TextInput changes, Checkbox toggles, and Slider moves.",
  "ui_on_hover": "**ui_on_hover**(widget: Int64, callback: (Int64, Int64) -> Unit) -> Unit\n\n`std.ui` — Register a mouse-enter handler.",
  "ui_on_key": "**ui_on_key**(widget: Int64, callback: (Int64, Int64) -> Unit) -> Unit\n\n`std.ui` — Register a key-press handler.",

  "ui_canvas_fill_rect": "**ui_canvas_fill_rect**(canvas, x, y, w, h, rgba) -> Unit\n\n`std.ui` — Draw a filled rectangle on the canvas. Coordinates relative to canvas top-left.",
  "ui_canvas_stroke_rect": "**ui_canvas_stroke_rect**(canvas, x, y, w, h, line_width, rgba) -> Unit\n\n`std.ui` — Draw a rectangle outline on the canvas.",
  "ui_canvas_fill_circle": "**ui_canvas_fill_circle**(canvas, cx, cy, radius, rgba) -> Unit\n\n`std.ui` — Draw a filled circle on the canvas.",
  "ui_canvas_draw_line": "**ui_canvas_draw_line**(canvas, x1, y1, x2, y2, line_width, rgba) -> Unit\n\n`std.ui` — Draw a line on the canvas.",
  "ui_canvas_draw_text": "**ui_canvas_draw_text**(canvas, text, x, y, size, rgba) -> Unit\n\n`std.ui` — Draw text on the canvas at the given position and font size.",
  "ui_canvas_clear": "**ui_canvas_clear**(canvas: Int64, rgba: Int64) -> Unit\n\n`std.ui` — Clear all drawing commands and fill with a solid color.",

  "ui_dialog_message": "**ui_dialog_message**(title: String, message: String) -> Unit\n\n`std.ui` — Show a modal message dialog.",
  "ui_dialog_confirm": "**ui_dialog_confirm**(title: String, message: String) -> Bool\n\n`std.ui` — Show a confirmation dialog. Returns `true` if the user clicked OK.",
  "ui_dialog_open_file": "**ui_dialog_open_file**(title: String, filter: String) -> String\n\n`std.ui` — Show a native open-file picker. Returns the selected path or empty string.",
  "ui_dialog_save_file": "**ui_dialog_save_file**(title: String, filter: String) -> String\n\n`std.ui` — Show a native save-file picker. Returns the selected path or empty string.",

  // std.torch (optional)
  "tensor_zeros": "**tensor_zeros**(shape, ndims) -> Tensor\n\n`std.torch` — Create a zero-filled tensor.",
  "tensor_ones": "**tensor_ones**(shape, ndims) -> Tensor\n\n`std.torch` — Create a tensor filled with ones.",
  "tensor_rand": "**tensor_rand**(shape, ndims) -> Tensor\n\n`std.torch` — Create a tensor with random values in [0, 1).",
  "tensor_randn": "**tensor_randn**(shape, ndims) -> Tensor\n\n`std.torch` — Create a tensor with standard normal random values.",
  "tensor_from_float_data": "**tensor_from_float_data**(data: Var, shape: Var, ndims: Int) -> Tensor\n\n`std.torch` — Create a tensor from a float data buffer and shape.",
  "tensor_arange": "**tensor_arange**(start: Double, end: Double, step: Double) -> Tensor\n\n`std.torch` — Create a 1D tensor with evenly spaced values.",
  "tensor_eye": "**tensor_eye**(n: Int) -> Tensor\n\n`std.torch` — Create an n×n identity matrix.",
  "tensor_add": "**tensor_add**(a, b) -> Tensor\n\n`std.torch` — Element-wise addition.",
  "tensor_sub": "**tensor_sub**(a, b) -> Tensor\n\n`std.torch` — Element-wise subtraction.",
  "tensor_mul": "**tensor_mul**(a, b) -> Tensor\n\n`std.torch` — Element-wise multiplication.",
  "tensor_div": "**tensor_div**(a, b) -> Tensor\n\n`std.torch` — Element-wise division.",
  "tensor_matmul": "**tensor_matmul**(a, b) -> Tensor\n\n`std.torch` — Matrix multiplication.",
  "tensor_neg": "**tensor_neg**(t) -> Tensor\n\n`std.torch` — Element-wise negation.",
  "tensor_exp": "**tensor_exp**(t) -> Tensor\n\n`std.torch` — Element-wise exponential (e^x).",
  "tensor_log": "**tensor_log**(t) -> Tensor\n\n`std.torch` — Element-wise natural logarithm.",
  "tensor_sum": "**tensor_sum**(t) -> Tensor\n\n`std.torch` — Sum all elements, returns a scalar tensor.",
  "tensor_mean": "**tensor_mean**(t) -> Tensor\n\n`std.torch` — Mean of all elements, returns a scalar tensor.",
  "tensor_reshape": "**tensor_reshape**(t, shape: Var, ndims: Int) -> Tensor\n\n`std.torch` — Reshape tensor to the given dimensions.",
  "tensor_transpose": "**tensor_transpose**(t, dim0: Int, dim1: Int) -> Tensor\n\n`std.torch` — Transpose two dimensions.",
  "tensor_squeeze": "**tensor_squeeze**(t) -> Tensor\n\n`std.torch` — Remove all dimensions of size 1.",
  "tensor_unsqueeze": "**tensor_unsqueeze**(t, dim: Int) -> Tensor\n\n`std.torch` — Insert a dimension of size 1 at the given position.",
  "tensor_get_float": "**tensor_get_float**(t, index: Int) -> Double\n\n`std.torch` — Get a float value at a flat index.",
  "tensor_item_float": "**tensor_item_float**(t) -> Double\n\n`std.torch` — Get the scalar value of a single-element tensor.",
  "tensor_ndim": "**tensor_ndim**(t) -> Int\n\n`std.torch` — Number of dimensions.",
  "tensor_numel": "**tensor_numel**(t) -> Int\n\n`std.torch` — Total number of elements.",
  "tensor_shape_dim": "**tensor_shape_dim**(t, dim: Int) -> Int\n\n`std.torch` — Size of a specific dimension.",
  "tensor_print": "**tensor_print**(t) -> Unit\n\n`std.torch` — Print tensor contents to stdout.",
  "tensor_backward": "**tensor_backward**(t) -> Unit\n\n`std.torch` — Backpropagate gradients.",
  "tensor_grad": "**tensor_grad**(t) -> Tensor\n\n`std.torch` — Get the gradient tensor.",
  "tensor_requires_grad": "**tensor_requires_grad**(t, requires: Bool) -> Tensor\n\n`std.torch` — Set whether tensor tracks gradients for autograd.",
  "tensor_to_device": "**tensor_to_device**(t, device: String) -> Tensor\n\n`std.torch` — Move tensor to a device (\"cpu\" or \"cuda\").",
  "nn_sequential_new": "**nn_sequential_new**() -> Module\n\n`std.torch` — Create a new sequential neural network model.",
  "nn_linear": "**nn_linear**(module, in_features, out_features)\n\n`std.torch` — Add a fully connected layer.",
  "nn_conv2d": "**nn_conv2d**(module, in_channels, out_channels, kernel_size)\n\n`std.torch` — Add a 2D convolutional layer.",
  "nn_relu": "**nn_relu**(module)\n\n`std.torch` — Add a ReLU activation layer.",
  "nn_sigmoid": "**nn_sigmoid**(module)\n\n`std.torch` — Add a Sigmoid activation layer.",
  "nn_tanh": "**nn_tanh**(module)\n\n`std.torch` — Add a Tanh activation layer.",
  "nn_softmax": "**nn_softmax**(module, dim: Int)\n\n`std.torch` — Add a Softmax layer along the given dimension.",
  "nn_dropout": "**nn_dropout**(module, p: Double)\n\n`std.torch` — Add a Dropout layer with probability `p`.",
  "nn_batch_norm": "**nn_batch_norm**(module, features: Int)\n\n`std.torch` — Add a Batch Normalization layer.",
  "nn_to_device": "**nn_to_device**(module, device: String)\n\n`std.torch` — Move the model to a device (\"cpu\" or \"cuda\").",
  "nn_forward": "**nn_forward**(module, input) -> Tensor\n\n`std.torch` — Forward pass through the model.",
  "nn_free": "**nn_free**(module)\n\n`std.torch` — Free the neural network module.",
  "loss_mse": "**loss_mse**(pred, target) -> Tensor\n\n`std.torch` — Mean squared error loss.",
  "loss_cross_entropy": "**loss_cross_entropy**(pred, target) -> Tensor\n\n`std.torch` — Cross-entropy loss.",
  "optim_adam": "**optim_adam**(module, lr) -> Optimizer\n\n`std.torch` — Adam optimizer.",
  "optim_sgd": "**optim_sgd**(module, lr) -> Optimizer\n\n`std.torch` — Stochastic gradient descent optimizer.",
  "optim_step": "**optim_step**(opt) -> Unit\n\n`std.torch` — Perform one optimization step.",
  "optim_zero_grad": "**optim_zero_grad**(opt) -> Unit\n\n`std.torch` — Zero all parameter gradients.",
  "cuda_is_available": "**cuda_is_available**() -> Bool\n\n`std.torch` — Check if CUDA GPU is available.",
  "cuda_device_count": "**cuda_device_count**() -> Int\n\n`std.torch` — Number of available CUDA devices.",
  "no_grad": "**no_grad**(flag: Bool) -> Unit\n\n`std.torch` — Enable or disable gradient computation globally. Use `no_grad(true)` for inference.",
  "set_num_threads": "**set_num_threads**(n: Int) -> Unit\n\n`std.torch` — Set the number of threads for CPU parallelism.",
  "model_save": "**model_save**(module, path) -> Unit\n\n`std.torch` — Save model weights to disk.",
  "model_load": "**model_load**(module, path) -> Unit\n\n`std.torch` — Load model weights from disk.",
  "jit_load": "**jit_load**(path) -> Module\n\n`std.torch` — Load a TorchScript model.",
  "torch_manual_seed": "**torch_manual_seed**(seed) -> Unit\n\n`std.torch` — Set the random seed for reproducibility.",
};

function nexHoverProvider(
  document: vscode.TextDocument,
  position: vscode.Position,
): vscode.Hover | undefined {
  const wordRange = document.getWordRangeAtPosition(position, /[a-zA-Z_][a-zA-Z0-9_]*/);
  if (!wordRange) return undefined;
  const word = document.getText(wordRange);

  const doc = DOCS[word];
  if (!doc) return undefined;

  const md = new vscode.MarkdownString(doc);
  md.isTrusted = true;
  return new vscode.Hover(md, wordRange);
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
