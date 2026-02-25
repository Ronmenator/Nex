## Nex v1 Final Spec and Tooling Blueprint (Codex-ready)

This is the canonical v1 specification for **Nex**, including language semantics, runtime ABI contracts, compiler pipeline, REPL+JIT, and VS Code tooling. Implement this as written. Where choices exist, follow the “v1 decision” stated here.

---

# 1) Language Identity and Goals

## Goals

* Compiled AOT native binaries via **LLVM**.
* Interactive development via **REPL + JIT** using **LLVM ORC**.
* **Brace-based blocks** `{}`.
* **Automatic Semicolon Insertion (ASI)**, semicolons optional.
* **OOP-first** with **real multiple inheritance** (classes with state).
* **Tracing GC only** (no reference counting).
* Deterministic resource cleanup via **`using` blocks**.
* Static typing by default with local inference by first assignment.
* Explicit runtime dynamic variables via keyword **`var`** (type `Var`).
* **Structs exist in v1** (value types with fields).
* **Operator overloading allowed** (with strict resolution rules).
* Tooling: build tool, formatter, linter, LSP, VS Code extension.

## Non-goals v1

* Stable ABI across compiler versions.
* Macros/metaprogramming.
* Full reflection beyond required RTTI and `Var` dispatch.
* Hot reload, deoptimization, advanced debug adapter.

---

# 2) Files, Modules, Packages

## Files

* Source extension: `.nex`
* Package root: `project.toml`
* Source directory: `src/`
* Module name from path: `src/net/http.nex` → `net.http`

## Imports

Supported:

* `import net.http`
* `import net.json as json`
* `from net.http import Client, Request`
* `import mylib.utils` (external library — resolved via `[libs]` in `project.toml`)

Not supported v1:

* wildcard imports

## Library Dependencies

External libraries are declared in `project.toml` under a `[libs]` section. Libraries can be referenced by local path, version (from the global cache), or GitHub source:

```toml
[libs]
nex3d = { git = "nexlang/nex3d", version = "0.1.0" }
graphics = { path = "../graphics" }
utils = "1.2.0"
```

### Dependency formats

| Format | Example | Resolution |
|--------|---------|------------|
| Version string | `nex3d = "0.1.0"` | Global cache `~/.nex/libs/nex3d/0.1.0/` |
| Git source | `{ git = "user/repo", version = "0.1.0" }` | Global cache, records source for reinstall |
| Local path | `{ path = "../graphics" }` | Relative to project root |

### Installing libraries

Libraries are installed from GitHub via the `nex install` command:

```
nex install user/repo           # install latest tag
nex install user/repo:0.1.0     # install specific version
nex install user/repo:v0.1.0    # tag with v-prefix
```

The command downloads the library source to the global cache at `~/.nex/libs/<name>/<version>/`, checks for native DLL assets in GitHub release artifacts, and updates the project's `project.toml`.

### Uninstalling

```
nex uninstall <name>            # remove from project.toml
```

Cached files in `~/.nex/libs/` are retained (other projects may depend on them).

### Listing dependencies

```
nex list                        # show all [libs] entries
```

Library rules:

* A library is a Nex project with its own `project.toml` and `src/` directory.
* The library name in `[libs]` becomes the import namespace prefix: modules in `graphics/src/shapes.nex` are imported as `import graphics.shapes`.
* Library sources are compiled alongside the main project and linked into a single executable.
* v1: GitHub is the only remote source (no central registry).
* v1: flat resolution — libraries cannot themselves declare `[libs]` (no transitive dependencies).
* If a library's `project.toml` declares `native = "name"`, the installer looks for prebuilt DLLs in GitHub release assets.

## Visibility

* Declarations are **module-internal by default**.
* `public` exports across modules.
* No implicit exports.

---

# 3) Lexical Rules

## Comments

* Line: `// ...`
* Block: `/* ... */` (v1: non-nested)

## String literals

* Double-quoted: `"text"`
* Escapes supported: `\"`, `\\`, `\n`, `\t`, `\r`

## Char literals

* Single-quoted: `'A'`
* Escapes: `\\`, `\'`, `\n`, `\t`, `\r`, `\uFFFF` (4 hex digits)

## Numeric literals

Types supported in v1:

* `Byte` (u8)
* `Int` (i32)
* `Int64` (i64)
* `Float` (f32)
* `Double` (f64)

Literal forms:

* Integer: `123`, `255b`, `42i`, `42i64`
* Float: `1.0`, `1.0f`, `1.0d` (default is `Double` if no suffix)

Default typing:

* Integer literal defaults to `Int` if it fits, else `Int64` if it fits, else compile error unless a larger type is added later.
* Float literal defaults to `Double` unless suffixed with `f`.

---

# 4) Statement Termination (ASI)

Blocks are `{}`. Indentation has no semantic meaning.

## ASI rule

A newline terminates a statement unless the previous token is one that cannot end a statement.

Insert an implicit terminator after newline if previous token is:

* identifier
* literal
* `)` `]` `}`
* `return` (no expression), `break`, `continue`

Do not terminate if previous token is:

* binary operator (`+ - * / % && || == != < > <= >=`)
* assignment operator (`= += -= *= /=`)
* `.` `,` `:` `(` `[` `{`
* `::` `->`

Semicolons `;` are permitted to place multiple statements on one line.

---

# 5) Types and Core Object Model

## 5.1 Universal base class

* Every **class** implicitly derives from `Object` unless it declares bases.
* `Object` has no base.
* **Value types do not inherit from Object.** This is locked.

## 5.2 Type categories

### Value types

* `Bool`, `Byte`, `Int`, `Int64`, `Float`, `Double`, `Char`
* `struct` types (user-defined value types)

Value types:

* are stored inline (stack/local/field inline) unless boxed.
* do not derive from `Object`.
* can implement interfaces if you support it (v1 decision: allow interface implementation, but no inheritance).

### Reference types

* All `class` types
* `String`
* `Var`

Reference types:

* are heap allocated and traced by GC.
* derive from `Object` (except `Var` may be a special runtime type but still must behave as a reference type).

## 5.3 `String` (primitive reference type)

* `String` is a compiler-known primitive reference type.
* Immutable.
* Heap allocated, GC-managed.
* Derived from `Object`.

Minimum API:

* `length() -> Int`
* `substring(start: Int, length: Int) -> String`
* `to_string() -> String` returns itself.

---

# 6) Variables, Binding, and `var`

## 6.1 Static locals by default

First assignment introduces a new local if the name is undefined in the current scope.

Example:

```au
a = 1        // a: Int
a = "test"   // compile error
```

## 6.2 Runtime dynamic via `var`

`var` introduces a variable of type `Var`:

```au
var b = 1
b = "test"   // ok
```

Rules:

* `var` variables may hold any value: boxed primitives, object references, null.
* Expressions involving `Var` are bound at runtime (operators, member calls, indexing if implemented).

## 6.3 Public API lint policy

* `Var` is allowed in public APIs but discouraged:

  * warning if a `public` function parameter or return type is `Var`
  * warning if a `public` field is `Var`
* Suppression:

  * `@allow_var_api` annotation on the function/field.

---

# 7) Structs (v1)

## 7.1 Definition

```au
public struct Vec2 {
    x: Float
    y: Float
}
```

Struct rules:

* Value type, stored inline.
* No inheritance.
* May have methods.
* May implement interfaces (v1: allowed).
* Default constructor:

  * if no `init` defined, fields default to zero/false/null-equivalent for that value type.
* Taking address and mutability model is v1-limited:

  * v1 decision: structs are mutable by default; copy semantics on assignment and pass-by-value.

## 7.2 Boxing

Structs box when:

* converted to `Var`
* converted to `Object` (if you allow boxing-to-Object; since value types do not inherit Object, the only way is explicit boxing)

v1 decision:

* Boxing to `Object` exists only through `Var` (simplify).
* If you want direct `Object` boxing later, add `box(x)` builtin.

---

# 7.5) Interfaces

## 7.5.1 Declaration

```au
public interface Greeter {
    def greet(name: String) -> String
}
```

Interface rules:

* An interface declares method signatures only (no fields, no state).
* Methods in an interface are implicitly `public` and `virtual`.
* Interfaces may extend other interfaces: `interface A : B { }`.
* Interfaces may have type parameters: `interface Comparable[T] { }`.
* Interfaces cannot be instantiated directly.

## 7.5.2 Implementation

* Classes implement interfaces by listing them in the base spec:

```au
class ConsoleGreeter : BaseGreeter, Greeter {
    def greet(name: String) -> String {
        return BaseGreeter::format(name)
    }
}
```

* Structs may implement interfaces:

```au
struct Point : Printable {
    x: Float
    y: Float
    def to_string() -> String { return "(" + x + "," + y + ")" }
}
```

* A class or struct that lists an interface must provide concrete definitions for all methods declared by that interface, or a compile error is emitted.

## 7.5.3 Interface vs class in base spec

* When a name in the base spec resolves to an interface, no subobject or vtable slot is allocated for it as a base class — instead, the compiler verifies method conformance.
* An interface base does not participate in MI layout (no base offset).

---

# 8) Classes and Multiple Inheritance

## 8.1 Class declaration

```au
public class D : B, C {
    ...
}
```

## 8.2 Diamond rule

Default: duplicated common ancestors.

Explicit shared base:

```au
class B : shared A { }
class C : shared A { }
class D : B, C { }  // single A subobject
```

## 8.3 Physical layout model (subobject composition)

Derived object layout:

```
[ObjHeader | SharedBases... | BaseSubobject(B) | BaseSubobject(C) | DerivedFields...]
```

* Each base subobject contains its own fields and vptr.
* Shared bases appear once.

## 8.4 `self` keyword

* Inside any instance method body, `self` is an implicitly bound reference to the receiver object.
* `self` has the type of the enclosing class or struct.
* Field access: `self.field_name`
* Method call: `self.method_name(args)`
* Qualified base call: `self.Base::method_name(args)` (used for MI disambiguation)
* `self` is not available in `static` methods — using it is a compile error.

## 8.5 Ambiguity and conflict resolution (compile-time)

If two bases provide a member with the same name:

* `d.foo()` is a compile error unless resolved.

Resolution mechanisms:

1. Override in derived.
2. Qualified base access/call: `self.B::foo()`
3. Alias: `alias fooFromB = B::foo`

Fields follow the same rule.

## 8.5 Construction order

1. Shared bases (stable topological order)
2. Non-shared bases left-to-right as declared
3. Derived fields
4. Derived constructor body

Base constructor args are explicit if needed:

```au
class D : B(a: 1), C(name: "x") { }
```

---

# 9) Methods and Dispatch

## 9.1 Virtual dispatch

* Non-virtual by default.
* `virtual` enables dynamic dispatch.
* `override` required for overriding.

## 9.2 Vtables

* Vtable per base “view” (per subobject).
* Virtual call lowering:

  * adjust `this` to subobject pointer
  * load vptr
  * call slot by index

---

# 10) Operator Overloading (v1)

## 10.1 Syntax

Operators are functions with reserved names.

Example:

```au
public struct Vec2 {
    x: Float
    y: Float

    public static def operator+(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x + b.x, a.y + b.y)
    }
}
```

Required operator function naming:

* `operator+`, `operator-`, `operator*`, `operator/`, `operator%`
* `operator==`, `operator!=`
* `operator<`, `operator<=`, `operator>`, `operator>=`
* `operator!` unary
* `operator-` unary (distinguish by arity)

## 10.2 Resolution rules

When parsing `a + b`:

1. If both operands are numeric primitives, use built-in numeric operator (no overload).
2. Else if left operand type defines matching `operator+`, use it.
3. Else if right operand type defines matching operator and it is marked commutative (v1: do not implement commutative fallback, keep simple).
4. Else compile error.

Equality:

* For classes, default `==` is reference equality unless overridden by `operator==` or `equals`.
* For structs, default `==` is fieldwise compare unless overridden (v1 decision: allow derived-by-compiler fieldwise equality).

Do not allow ambiguous operator overload resolution.

---

# 11) Control Flow and Exceptions

Minimum statements:

* `if (cond) { ... }` with optional `else { ... }`
* `while (cond) { ... }`
* `for (init; cond; step) { ... }` (C-style)
* `return [expr]`
* `break`, `continue`
* `try { } catch (e: Type) { } finally { }`
* `throw expr`

Exceptions are heap objects derived from `Object`.

---

# 12) `using` Blocks and Disposal

## 12.1 Disposable contract

```au
public interface Disposable {
    def dispose() -> Unit
}
```

## 12.2 Using syntax

```au
using (f = File.open("a.txt")) {
    f.write("hello")
}
```

Multi:

```au
using (a = A(), b = B()) { ... }
```

## 12.3 Semantics

`using` lowers to `try/finally`, disposal in reverse acquisition order.

Type rule:

* In normal compilation mode, using binders must be `Disposable` (or nullable if you implement `T?`).
* In REPL mode, `using (var x = ...)` is permitted with runtime check and a warning.

---

# 12.5) Nullable Types

## 12.5.1 Syntax

A type followed by `?` denotes the nullable variant: `T?`.

```au
name: String? = null
```

## 12.5.2 Semantics

* `null` is a literal of type `Null` that is assignable to any nullable type.
* A variable of type `T?` may hold either a value of type `T` or `null`.
* Accessing members on a nullable type without a check is a compile error (v1: simple null-check required).
* Null-check pattern:

```au
if (name != null) {
    // name is narrowed to String here (v1: advisory, not enforced flow typing)
}
```

## 12.5.3 Boxing interaction

* For value types, `T?` boxes the value when non-null and uses a null pointer when null.
* For reference types, `T?` is the same representation as `T` (pointer that may be null).

---

# 12.6) Function Types

## 12.6.1 Syntax

Function type expressions use arrow notation:

```au
callback: (Int, Int) -> Bool
```

## 12.6.2 Semantics

* Function types are first-class: they can be stored in variables, passed as arguments, and returned from functions.
* v1 supports function references only (no closures/lambdas). A function reference is created by naming a function: `callback = safe_divide`.
* Function types do not derive from `Object`; they are represented as function pointers at the ABI level.
* v1 decision: closures and lambdas are deferred to v2.

---

# 12.7) Generics (Type Parameters)

## 12.7.1 Syntax

Type parameters use bracket syntax:

```au
public class List[T] {
    def add(item: T) -> Unit { ... }
    def get(index: Int) -> T { ... }
}

public def identity[T](value: T) -> T {
    return value
}
```

## 12.7.2 Semantics

* v1 strategy: **monomorphization** — each unique instantiation generates a specialized copy at compile time.
* Type parameters may appear on classes, structs, interfaces, and functions.
* v1 does not support type parameter constraints (bounded generics). All operations on generic values must be statically resolved.
* Instantiation syntax: `List[Int]`, `Map[String, Int]`.

## 12.7.3 Limitations v1

* No variance annotations (`in`/`out`).
* No default type parameters.
* No constraint syntax (`where T : Comparable`).

---

# 12.8) Annotations

## 12.8.1 Syntax

Annotations use `@` prefix before declarations:

```au
@allow_var_api
public def dispatch(action: Var) -> Var { ... }
```

## 12.8.2 Built-in annotations v1

* `@allow_var_api` — suppresses the `Var` in public API lint warning on the annotated function or field.

## 12.8.3 Semantics

* Annotations are compile-time metadata; they do not affect runtime behavior.
* Unknown annotation names produce a compile warning.
* v1 does not support annotation arguments (e.g., `@deprecated("use X")`).

---

# 13) Runtime System

## 13.1 GC policy

* **Tracing GC only**.
* v1 GC: stop-the-world tracing, mark-sweep is acceptable.
* Conservative stack scan is acceptable v1.
* Heap scanning must be precise using type pointer maps.

Generational GC planned later:

* Define write barrier ABI symbol now.
* Implement in v2.

## 13.2 Object header

Every heap object begins with:

* GC word(s)
* `Type*` pointer
* optional sync word (optional v1)

## 13.3 RTTI Type descriptor

Each `Type` descriptor includes:

* size, alignment
* pointer map for fields
* base offsets for MI casts
* vtable pointers per view
* optional debug info

## 13.4 `Var` runtime

* `Var` is a GC-managed object.
* Holds a type tag and boxed payload.
* Supports runtime operator dispatch and member invocation.

---

# 14) Runtime ABI (canonical C ABI)

Create `docs/runtime-abi.h` and implement exactly.

Minimum ABI:

Allocation and GC:

* `nex_gc_alloc(const NexType* t, uint32_t size) -> NexObj*`
* `nex_gc_collect(void)`
* `nex_gc_safepoint(void)`
* `nex_gc_write_barrier(NexObj* obj)` (defined v1, may be stub until v2)

Exceptions:

* `nex_throw(NexObj* ex) -> noreturn`
* `nex_new_exception(const char* msg) -> NexObj*`

Var:

* `nex_var_from_i64(int64_t v) -> NexVar*`
* `nex_var_from_f64(double v) -> NexVar*`
* `nex_var_from_obj(NexObj* o) -> NexVar*`
* `nex_var_add(NexVar* a, NexVar* b) -> NexVar*`
* `nex_var_eq(NexVar* a, NexVar* b) -> NexVar*`
* `nex_var_invoke_member(NexVar* recv, NexObj* name_str, NexVar** args, uint32_t argc) -> NexVar*`
* `nex_var_typeof(NexVar* v) -> const NexType*`

---

# 15) Standard Library

Namespace layout (17 modules, ~170 functions):

* `std.core` -- Object, print, println
* `std.runtime` -- Disposable, gc_collect
* `std.math` -- abs, sin, cos, sqrt, pow, random, floor, ceil, round, log, exp, min, max, clamp
* `std.string` -- trim, split, replace, upper/lower, starts_with, ends_with, contains, index_of, reverse
* `std.convert` -- parse_int, parse_float, parse_bool, char_to_str
* `std.collections` -- List, Map, Set (sort, reverse, clear, contains)
* `std.io` -- File, read_line, file_exists, file_delete, file_copy, mkdir, list_dir
* `std.env` -- env_get, env_set, env_has, args, cwd
* `std.time` -- now_millis, now_nanos, sleep_millis, elapsed_millis
* `std.path` -- join, parent, file_name, extension, stem, is_absolute, normalize, separator
* `std.json` -- parse, stringify, get_string, get_int, get_float, get_bool
* `std.regex` -- new, is_match, find, replace, free
* `std.process` -- exec, exec_output, exit, pid, spawn, wait
* `std.net` -- TCP (connect, listen, accept, send, recv, close), UDP (bind, send, recv, close)
* `std.http` -- get, post, response_status, response_body, response_header, response_free
* `std.threading` -- spawn, join, sleep, current_id, mutex (new, lock, unlock, free)
* `std.crypto` -- sha256, sha512, md5, hmac_sha256, base64_encode, base64_decode, random_bytes
* `std.logging` -- debug, info, warn, error, set_level, with_tag
* `std.testing` -- assert, assert_eq (int/str/float/bool), assert_ne (int/str), assert_true

All stdlib functions are C ABI runtime functions in `nex_runtime`, registered in the Cranelift codegen for both JIT and AOT. Nex names map to `nex_`-prefixed runtime symbols via `stdlib_function_name()` in the codegen.

Engine and UI functions live in separate native DLLs (`nex3d_native`, `nex_ui_native`) that are loaded dynamically at JIT time via `libloading`. Libraries declare their native DLL dependency via a `native` field in `project.toml`.

## 15.1 `std.core`

* `Object` class (root)
* printing (variadic, like Python's `print` — auto-converts each arg to string, joins with spaces):

  * `public def print(values...) -> Unit`
  * `public def println(values...) -> Unit`

## 15.2 `std.runtime`

* `Disposable`
* `public def gc_collect() -> Unit` (optional but recommended)

## 15.3 `std.collections` (C#-like)

Generics in v1 decision: **monomorphization** (compile-time specialization).

* `public class List[T] : Object`

  * `init()`
  * `add(item: T)`
  * `get(index: Int) -> T`
  * `set(index: Int, value: T)`
  * `length() -> Int`

* `public class Map[K,V] : Object`

  * `init()`
  * `put(key: K, value: V)`
  * `get(key: K) -> V` with “not found” behavior:

    * v1 decision: provide `try_get(key: K) -> (Bool, V)` to avoid nullable types in v1.
  * `contains(key: K) -> Bool`
  * `remove(key: K)`
  * `size() -> Int`

Keys use:

* built-in hashing for primitives
* virtual `hash_code` + `equals` for classes

## 15.4 `std.math`

* `abs_int(Int) -> Int`, `abs_float(Double) -> Double`
* `min_int`, `max_int`, `min_float`, `max_float`, `clamp_int`, `clamp_float`
* `floor`, `ceil`, `round`, `sqrt`, `pow`
* `sin`, `cos`, `tan`, `log`, `log2`, `log10`, `exp`
* `random() -> Double`, `random_range(Int, Int) -> Int`

## 15.5 `std.string`

* `str_trim`, `str_trim_start`, `str_trim_end`
* `starts_with`, `ends_with`, `contains`, `index_of`
* `str_replace`, `to_upper`, `to_lower`, `str_repeat`, `char_at`, `str_reverse`, `str_split`

## 15.6 `std.convert`

* `parse_int(String) -> Int`, `parse_float(String) -> Double`, `parse_bool(String) -> Bool`
* `char_to_str(Int) -> String`, `str_to_chars(String) -> Var`

## 15.7 `std.io`

* `File` class: `open`, `read_all`, `write`, `dispose`
* `read_line`, `file_exists`, `file_delete`, `file_rename`, `file_copy`, `file_size`, `mkdir`, `list_dir`

## 15.8 `std.env`

* `env_get`, `env_set`, `env_has`, `env_args_count`, `env_args_get`, `env_cwd`

## 15.9 `std.time`

* `now_millis`, `now_nanos`, `sleep_millis`, `elapsed_millis`

## 15.10 `std.path`

* `path_join`, `path_parent`, `path_file_name`, `path_extension`, `path_stem`, `path_is_absolute`, `path_normalize`, `path_separator`

## 15.11 `std.json`

* `json_parse`, `json_stringify`, `json_get_string`, `json_get_int`, `json_get_float`, `json_get_bool`

## 15.12 `std.regex`

* `regex_new`, `regex_is_match`, `regex_find`, `regex_replace`, `regex_free`

## 15.13 `std.process`

* `process_exec`, `process_exec_output`, `process_exit`, `process_pid`, `process_spawn`, `process_wait`

## 15.14 `std.net`

* TCP: `tcp_connect`, `tcp_close`, `tcp_send`, `tcp_recv`, `tcp_listen`, `tcp_accept`
* UDP: `udp_bind`, `udp_close`, `udp_send`, `udp_recv`

## 15.15 `std.http`

* `http_get`, `http_post`, `http_response_status`, `http_response_body`, `http_response_header`, `http_response_free`

## 15.16 `std.threading`

* `thread_spawn`, `thread_join`, `thread_sleep`, `thread_current_id`
* `mutex_new`, `mutex_lock`, `mutex_unlock`, `mutex_free`

## 15.17 `std.crypto`

* `sha256`, `sha512`, `md5`, `random_bytes`, `base64_encode`, `base64_decode`, `hmac_sha256`

## 15.18 `std.logging`

* `log_debug`, `log_info`, `log_warn`, `log_error`, `log_set_level`, `log_with_tag`

## 15.19 `std.testing`

* `assert`, `assert_true`, `assert_eq_int`, `assert_eq_str`, `assert_eq_float`, `assert_eq_bool`, `assert_ne_int`, `assert_ne_str`

## 15.20 `std.torch` (optional -- requires `--features torch`)

Wraps libtorch via the `tch-rs` Rust crate. Gated behind the `torch` cargo feature; not compiled by default.

Build with torch: `cargo build --features torch` (auto-downloads libtorch via `download-libtorch`).

Tensor creation:

* `tensor_zeros(shape, ndims) -> Tensor`, `tensor_ones`, `tensor_rand`, `tensor_randn`
* `tensor_from_float_data(data, shape, ndims) -> Tensor`
* `tensor_arange(start, end, step) -> Tensor`, `tensor_eye(n) -> Tensor`
* `tensor_free(t)`

Tensor operations:

* `tensor_add`, `tensor_sub`, `tensor_mul`, `tensor_div`, `tensor_matmul`
* `tensor_neg`, `tensor_exp`, `tensor_log`, `tensor_sum`, `tensor_mean`
* `tensor_reshape`, `tensor_transpose`, `tensor_squeeze`, `tensor_unsqueeze`
* `tensor_print`, `tensor_shape_dim`

Tensor data access:

* `tensor_get_float(t, index) -> Double`, `tensor_item_float(t) -> Double`
* `tensor_ndim(t) -> Int`, `tensor_numel(t) -> Int`

Device management:

* `cuda_is_available() -> Bool`, `cuda_device_count() -> Int`
* `tensor_to_device(t, device_str) -> Tensor`, `set_num_threads(n)`

Autograd:

* `tensor_requires_grad(t, Bool) -> Tensor`, `tensor_backward(t)`
* `tensor_grad(t) -> Tensor`, `torch_no_grad(flag)`

Neural network layers:

* `nn_sequential_new() -> Module`
* `nn_linear(module, in, out)`, `nn_conv2d(module, in_ch, out_ch, kernel)`
* `nn_relu(module)`, `nn_sigmoid(module)`, `nn_tanh(module)`, `nn_softmax(module, dim)`
* `nn_dropout(module, p)`, `nn_batch_norm(module, features)`
* `nn_forward(module, input) -> Tensor`, `nn_free(module)`

Loss functions:

* `loss_mse(pred, target) -> Tensor`, `loss_cross_entropy`, `loss_bce`

Optimizers:

* `optim_sgd(module, lr) -> Optimizer`, `optim_adam(module, lr) -> Optimizer`
* `optim_step(opt)`, `optim_zero_grad(opt)`, `optim_free(opt)`

Model I/O:

* `model_save(module, path)`, `model_load(module, path)`
* `jit_load(path) -> Module`, `jit_forward(module, input) -> Tensor`

Utility:

* `torch_manual_seed(seed)`, `torch_version() -> String`, `tensor_to_string(t) -> String`

## 15.21 `std.ui` (separate native DLL)

Cross-platform UI framework with WGPU-based desktop rendering and a terminal (TUI) backend. Compiled as a separate native DLL (`nex_ui_native.dll` / `libnex_ui_native.so`) and loaded dynamically at JIT time.

Build the DLL: `cargo build -p nex_ui_native`. Place it next to `nex.exe` (auto-discovered when `import std.ui` is used).

Dependencies (in `nex_ui_native` crate): `winit` (windowing), `wgpu` (GPU rendering), `cosmic-text` (font shaping), `taffy` (flexbox layout), `crossterm` (terminal backend), `pollster` (async bridge).

Backends:

* `0` = WGPU (default) — GPU-accelerated desktop window via winit + wgpu
* `1` = Terminal — text-based TUI via crossterm

Application lifecycle:

* `ui_app_create(title, width, height) -> AppHandle`
* `ui_app_set_backend(app, backend_id)`
* `ui_app_set_root(app, widget)`
* `ui_app_run(app)` — blocks and runs the event loop
* `ui_app_quit(app)`, `ui_app_destroy(app)`
* `ui_is_running(app) -> Bool`
* `ui_app_render(app)`, `ui_poll_event(app) -> EventType`
* `ui_event_type(event) -> Int`, `ui_event_widget(event) -> WidgetHandle`

Widget creation:

* `ui_text(text) -> Widget`, `ui_button(label) -> Widget`
* `ui_text_input(placeholder) -> Widget`, `ui_image(path) -> Widget`
* `ui_checkbox(label) -> Widget`, `ui_slider(min, max) -> Widget`
* `ui_row() -> Widget`, `ui_column() -> Widget`, `ui_stack() -> Widget`
* `ui_scroll() -> Widget`, `ui_grid(cols) -> Widget`, `ui_canvas(w, h) -> Widget`

Widget tree:

* `ui_add_child(parent, child)`, `ui_remove_child(parent, child)`
* `ui_set_id(widget, id)`, `ui_get_id(widget) -> String`

Widget properties:

* `ui_set_text(widget, text)`, `ui_get_text(widget) -> String`
* `ui_set_visible(widget, bool)`, `ui_set_enabled(widget, bool)`
* `ui_get_value_float(widget) -> Double`, `ui_set_value_float(widget, val)`

Styling:

* `ui_set_width`, `ui_set_height`, `ui_set_min_width`, `ui_set_min_height`
* `ui_set_padding(widget, packed_edges)`, `ui_set_margin(widget, packed_edges)`
* `ui_set_bg_color(widget, rgba_packed)`, `ui_set_fg_color(widget, rgba_packed)`
* `ui_set_font_size`, `ui_set_border(widget, width, rgba)`, `ui_set_border_radius`
* `ui_set_flex_grow`, `ui_set_align_self`, `ui_set_justify_content`, `ui_set_align_items`, `ui_set_gap`

Event callbacks (function references):

* `ui_on_click(widget, callback)`, `ui_on_change(widget, callback)`
* `ui_on_hover(widget, callback)`, `ui_on_key(widget, callback)`

Canvas drawing:

* `ui_canvas_fill_rect`, `ui_canvas_stroke_rect`, `ui_canvas_fill_circle`
* `ui_canvas_draw_line`, `ui_canvas_draw_text`, `ui_canvas_clear`

Dialogs:

* `ui_dialog_message(title, msg)`, `ui_dialog_confirm(title, msg) -> Bool`
* `ui_dialog_open_file(title, filter) -> String`, `ui_dialog_save_file(title, filter) -> String`

### 15.21.1 `.nexui` Declarative Markup

An XAML-style declarative markup system that compiles to the imperative `std.ui` API above:

* `.nexui` files contain XML markup defining the UI tree
* Code-behind `.nex` files use `partial class` to define event handlers and binding sources
* The compiler discovers `.nexui` files adjacent to `.nex` source, parses XML, lowers to imperative `std.ui` calls, and merges partial classes

Binding engine (MVVM):

* `nex_ui_bind(property_name, callback)` — registers an update callback for a binding path
* `nex_ui_unbind(property_name)` — removes bindings
* `nex_ui_notify_changed(property_name)` — triggers all registered callbacks for the given property
* `nex_ui_bindings_clear()` — clears all bindings

Binding expression syntax in `.nexui`:

* `{Binding PropertyName}` — one-way binding (model → UI)
* `{Binding PropertyName, Mode=TwoWay}` — two-way binding (model ↔ UI)
* `{Binding PropertyName, Converter=MethodName}` — with value converter

Generated methods on the partial class:

* `_init_ui()` — builds the widget tree from markup and sets the app root
* `_setup_bindings()` — registers property-change listeners for all `{Binding}` expressions
* `_run_app()` — calls `ui_app_run` + `ui_app_destroy`
* `_update_PropertyName()` — per-binding updater invoked on `notify_changed`

Class fields (both markup-generated widget handles and code-behind model state) are promoted to IR globals so they are shared across all methods.

#### Named Style System

`.nexui` supports reusable named styles defined in a `<Window.Styles>` (or `<Widget.Styles>`) block at the top of the root element. Styles are resolved entirely at compile time — they expand to the same `ui_set_*` calls as equivalent inline attributes.

**Syntax:**

```xml
<Window xmlns="std.ui" Title="App" Width="900" Height="650">
  <Window.Styles>
    <Style Name="PrimaryButton" TargetType="Button">
      <Setter Property="BgColor" Value="0x3B82F6FF"/>
      <Setter Property="FgColor" Value="0xFFFFFFFF"/>
      <Setter Property="BorderRadius" Value="6"/>
    </Style>
    <Style Name="Header" TargetType="Text">
      <Setter Property="FontSize" Value="28"/>
      <Setter Property="FgColor" Value="0x1E293BFF"/>
    </Style>
  </Window.Styles>

  <Column>
    <Text Style="Header" Text="Welcome" />
    <!-- Inline BgColor overrides the style's BgColor -->
    <Button Style="PrimaryButton" Label="Cancel" BgColor="0x6B7280FF" Click="on_cancel" />
  </Column>
</Window>
```

**Rules:**

* `<Window.Styles>` / `<Widget.Styles>` — dot-notation property element; removed from the widget tree before lowering
* `<Style Name="..." TargetType="...">` — defines a named style; `TargetType` is informational only
* `<Setter Property="..." Value="..."/>` — sets a style property; only literal values are supported (no bindings)
* `Style="Name"` on an element — applies the named style's setters before inline attributes; **inline attributes override style setters**
* Referencing an undefined style name emits a compiler warning (not an error)

#### External Stylesheets (`.nexuistyle`)

Styles can be defined in separate `.nexuistyle` files and imported into any `.nexui` file via `<Window.Resources>` / `<Widget.Resources>`. This allows a single stylesheet to be shared across all windows and widgets in a project.

**Stylesheet file** (`*.nexuistyle`) — uses `<ResourceDictionary>` as the root element:

```xml
<!-- src/Styles/AppStyles.nexuistyle -->
<ResourceDictionary xmlns="std.ui">
  <Style Name="Primary" TargetType="Button">
    <Setter Property="BgColor" Value="0x3B82F6FF"/>
    <Setter Property="FgColor" Value="0xFFFFFFFF"/>
    <Setter Property="BorderRadius" Value="6"/>
  </Style>
</ResourceDictionary>
```

**Importing** — declare one or more `<ResourceDictionary Source="…"/>` elements inside `<Window.Resources>`:

```xml
<Window xmlns="std.ui">
  <Window.Resources>
    <ResourceDictionary Source="Styles/AppStyles.nexuistyle"/>
    <ResourceDictionary Source="Styles/Theme.nexuistyle"/>
  </Window.Resources>
  <Window.Styles>
    <!-- Local styles override imported ones with the same name -->
    <Style Name="Primary">
      <Setter Property="BgColor" Value="0xEF4444FF"/>
    </Style>
  </Window.Styles>
  <Button Style="Primary" Label="OK"/>
</Window>
```

**Rules:**
* `Source` paths are relative to the importing `.nexui` file
* **Local `<Window.Styles>` definitions override imported styles** with the same name
* Multiple `<ResourceDictionary>` elements can be listed; they are merged in declaration order
* Missing resource files produce a `Severity::Error` compiler diagnostic
* `.nexuistyle` files are never lowered to partial classes — they exist only to supply styles
* The `<Window.Resources>` / `<Widget.Resources>` block is removed from the widget tree before lowering

**Supported style/layout attributes** (in addition to previously-supported `BgColor`, `FgColor`, `FontSize`, `Visible`, `Enabled`, `Gap`, `Padding`, `FlexGrow`):

| Attribute | Runtime call |
|-----------|-------------|
| `BorderRadius` | `ui_set_border_radius(widget, float)` |
| `BorderWidth` | `ui_set_border_width(widget, float)` |
| `BorderColor` | `ui_set_border_color(widget, int)` |
| `Margin` | `ui_set_margin_all(widget, float)` |
| `Width` / `Height` (non-Window, non-Canvas) | `ui_set_width` / `ui_set_height` |
| `MinWidth` / `MinHeight` | `ui_set_min_width` / `ui_set_min_height` |
| `MaxWidth` / `MaxHeight` | `ui_set_max_width` / `ui_set_max_height` |
| `FlexShrink` | `ui_set_flex_shrink(widget, float)` |
| `AlignItems` | `ui_set_align_items(widget, string)` |
| `JustifyContent` | `ui_set_justify_content(widget, string)` |
| `AlignSelf` | `ui_set_align_self(widget, string)` |
| `Checked` | `ui_set_checked(widget, int)` |
| `Value` (literal) | `ui_set_value_float(widget, float)` |

New compiler crates:

* `nexui_parse` — lightweight XML parser producing `UIDocument` AST
* `nexui_lower` — transforms `UIDocument` into a partial `ClassDecl` with imperative `std.ui` code

Language additions:

* `partial class` — allows splitting a class definition across multiple files (`.nexui` generated + `.nex` code-behind), merged at resolve time

---

# 16) Compiler Architecture (Rust)

## 16.1 Crates (monorepo)

* `nexc_diag`: SourceMap, spans, diagnostics, renderer
* `nexc_lex`: lexer + ASI normalization
* `nexc_ast`: AST types
* `nexc_parse`: parser (Pratt for expressions)
* `nexc_resolve`: module graph, symbols, imports, visibility
* `nexc_type`: type system and typecheck
* `nexc_layout`: MI layout and vtables
* `nexc_ir`: minimal IR
* `nexc_codegen_llvm`: AOT LLVM backend
* `nexc_codegen_llvm_jit`: ORC JIT backend
* `nexc_meta`: `.nexmeta` metadata
* `nexc_driver`: orchestrates pipeline and incremental builds
* `nexc`: compiler CLI

Tooling:

* `nex`: build tool CLI
* `nexc_fmt`: formatter
* `nexc_lint`: linter
* `nex_lsp`: LSP server
* `nex_repl`: REPL client
* `nex_runtime`: runtime library (statically linked into nex.exe)
* `nex_std`: stdlib package

Native DLLs (cdylib, loaded dynamically at JIT time via `libloading`):

* `nex3d_native`: 3D engine (windowing, rendering, input, camera, drawing) — shipped with the nex3d library
* `nex_ui_native`: UI framework (widgets, layout, text rendering) — shipped alongside nex.exe

UI markup:

* `nexui_parse`: `.nexui` XML parser producing `UIDocument` AST
* `nexui_lower`: transforms `UIDocument` into imperative `std.ui` code

## 16.2 Crate dependency graph

```
nexc_diag          (no deps — foundation for spans, diagnostics, source map)
  ↑
nexc_lex           (depends: nexc_diag)
  ↑
nexc_ast           (depends: nexc_diag)
  ↑
nexc_parse         (depends: nexc_ast, nexc_diag, nexc_lex)
  ↑
nexc_resolve       (depends: nexc_ast, nexc_diag)
  ↑
nexc_type          (depends: nexc_ast, nexc_diag)
  ↑
nexc_layout        (depends: nexc_ast, nexc_diag, nexc_type)
  ↑
nexc_ir            (depends: nexc_diag, nexc_type, nexc_layout)
  ↑
nexc_codegen_llvm      (depends: nexc_diag, nexc_ir)
nexc_codegen_llvm_jit  (depends: nexc_diag, nexc_ir)
nexc_codegen_cranelift (depends: nexc_ir, nex_runtime, libloading — JIT backend + dynamic native lib loading)
  ↑
nexc_meta          (depends: nexc_diag, serde, serde_json)
  ↑
nexc_driver        (depends: all compiler crates — orchestrates full pipeline)
  ↑
nexc               (depends: nexc_diag, nexc_driver — compiler CLI)
nex                (depends: nexc_driver, nexc_diag, nexc_fmt, nexc_lint, nexc_resolve, nex_repl — build tool CLI)
nex_lsp            (depends: nexc_ast, nexc_diag, nexc_driver, nexc_lint, nexc_parse, nexc_lex)
nex_repl           (depends: nexc_driver)

nex3d_native       (standalone cdylib — winit, wgpu, pollster, bytemuck)
nex_ui_native      (standalone cdylib — winit, wgpu, cosmic-text, taffy, crossterm, pollster)
```

## 16.3 Data flow between stages

Key data types flowing through the pipeline:

| Stage | Input | Output |
|---|---|---|
| Lex + ASI | `&str` source text | `Vec<Token>` |
| Parse | `&[Token]` | `SourceFile` (AST) |
| Module graph | `&[SourceFile]` | `ModuleGraph` (imports, dependency order) |
| Resolve + visibility | `&SourceFile`, `&ModuleGraph` | `SourceFile` (resolved), `HashMap<String, Visibility>` |
| Declare types | `&SourceFile` | `TypedModule` (types map, annotated AST) |
| Validate inheritance | `&TypedModule` | diagnostics (mutates `TypedModule`) |
| Compute layout | `&TypedModule` | `Vec<ClassLayout>` |
| Typecheck bodies | `&mut TypedModule` | diagnostics |
| Lower to IR | `&TypedModule`, `&[ClassLayout]` | `IrModule` |
| LLVM codegen | `&IrModule` | `Vec<u8>` (object bytes) |

## 16.4 Diagnostic system

All crates use the shared `nexc_diag` infrastructure:

* `Span { lo, hi }` — byte offset range in source.
* `Diagnostic { id, severity, span, file, message, notes, suggestions }` — a single error/warning/note.
* `DiagnosticSink` — accumulator; passed through every pipeline stage. Has `push`, `has_errors`, `render_all`.
* `Severity` — `Error`, `Warning`, `Note`, `Help`.
* `SourceMap` — maps file IDs to source text; provides `line_col` and `line_text` for rendering.

Each diagnostic has a stable string `id` (e.g., `"type_inheritance_cycle"`, `"lex_invalid_char"`) for programmatic matching and testing.

## 16.5 Error recovery strategy

* The parser uses **panic-mode recovery**: on an unexpected token, it emits a diagnostic and advances to the next item boundary (`;`, `}`, or a top-level keyword like `class`/`def`/`import`).
* Inside class/struct bodies, recovery advances to the next method/field boundary.
* The lexer continues after invalid characters, emitting `Unknown` tokens.
* No pipeline stage panics on user code — all errors flow through `DiagnosticSink`.

## 16.6 `project.toml` format

```toml
name = "my_project"
version = "0.1.0"
entry = "src/main.nex"
src = "src"                # optional, defaults to "src"

[libs]
graphics = { path = "../graphics" }
```

Fields:

* `name` — package name (required).
* `version` — semver string (required).
* `entry` — entry point file path, relative to project root.
* `src` — source directory, relative to project root (default `"src"`).
* `native` — optional name of a native DLL shipped with the library. The runtime looks for `<native>.dll` (Windows) or `lib<native>.so` (Linux) in the library's root directory and loads it at JIT time.
* `[libs]` — optional section declaring library dependencies. Supported entry formats:
  * Version string: `nex3d = "0.1.0"` — resolved from global cache `~/.nex/libs/<name>/<version>/`
  * Git source: `nex3d = { git = "user/repo", version = "0.1.0" }` — resolved from global cache, records origin
  * Local path: `graphics = { path = "../graphics" }` — relative to the project root
  The library name becomes the import namespace prefix.

## 16.7 Canonical pipeline order

1. Lex + ASI normalize
2. Parse AST
3. Build module graph
4. Resolve symbols + enforce `public`
5. Declare types (class/struct headers, signatures)
6. Validate inheritance DAG
7. Compute layout + vtables
8. Typecheck bodies
9. Lower to IR
10. LLVM codegen + link

Metadata (`.nexmeta`) emission is conditional and runs after codegen if `--emit-metadata` is set.

No stage may panic on user code. All errors flow through `DiagnosticSink`.

---

# 17) REPL + JIT

Command: `nex repl`

## Session model

* Each submission compiles as module `repl$NNNN`.
* Wrap statements into `repl_entry$NNNN()`; print expression results.

## Redefinition

* Functions are redefinable via **indirection cells** (mutable function pointer slots).
* Class layout-breaking redefinition is disallowed in v1.

## GC interaction

* v1 conservative stack scan permitted.
* Insert safepoints at allocation and loop backedges.

---

# 18) Metadata and Incremental Build

`.nexmeta` (JSON v1) includes:

* module name
* compiler version stamp
* exported symbols and signatures
* type/layout hashes
* dependency list and imported metadata hashes

Rebuild if:

* source hash changes
* imported metadata hash changes
* compiler version changes

---

# 19) Tooling

## Build tool `nex`

Commands:

* `build`, `run`, `test`, `repl`, `fmt`, `lint`, `clean`
* `install <user/repo[:version]>` — download a library from GitHub to the global cache and add it to `project.toml`
* `uninstall <name>` — remove a library entry from `project.toml`
* `list` — display all project dependencies

Responsibilities:

* parse `project.toml`
* build module DAG
* incremental compilation via `.nexmeta`
* link runtime + stdlib + objects
* package management (install, uninstall, list)

## Formatter

* Parse then print canonical format.
* Must preserve ASI semantics.

## Linter

Rules v1:

* warn on `Var` in public API unless `@allow_var_api`
* unused imports
* unreachable code
* ambiguous member access suggestions

## LSP

Minimum:

* diagnostics
* go-to-definition
* hover types
* references
* rename
* code actions: add `public`, qualify base call, introduce alias, add `@allow_var_api`

---

# 20) VS Code Extension

* `.nex` language registration
* TextMate grammar for highlighting
* LSP client launching `nex-lsp`
* commands to invoke `nex build/run/test/fmt`

---

# 21) Conformance Tests (required)

* ASI cases
* lexer spans
* parse golden AST
* visibility enforcement
* static local inference and reassignment error
* `var` dynamic behavior
* struct semantics and operator overload resolution tests
* MI layout offsets:

  * diamond duplication
  * shared base collapsing
* ambiguity diagnostics and fix suggestions
* vtable override correctness
* using disposal (including exception path)
* REPL redefinition behavior

---

## Codex implementation note

If any component is missing today, implement it in the simplest correct way consistent with this spec. Do not expand scope.
