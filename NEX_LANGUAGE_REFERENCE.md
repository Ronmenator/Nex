# Nex Language Reference — Complete Guide for AI Code Generation

> **Purpose**: This document describes the Nex programming language in full detail so that an AI model can write correct, idiomatic Nex code to build applications. Every syntax rule, type, operator, standard library function, and convention is documented here.

---

## Table of Contents

1. [Language Overview](#1-language-overview)
2. [Project Structure & Tooling](#2-project-structure--tooling)
3. [Syntax Fundamentals](#3-syntax-fundamentals)
4. [Type System](#4-type-system)
5. [Variables & Assignment](#5-variables--assignment)
6. [Operators](#6-operators)
7. [Control Flow](#7-control-flow)
8. [Functions](#8-functions)
9. [Classes](#9-classes)
10. [Structs (Value Types)](#10-structs-value-types)
11. [Interfaces](#11-interfaces)
12. [Generics](#12-generics)
13. [Modules & Imports](#13-modules--imports)
14. [Error Handling](#14-error-handling)
15. [Resource Management (using)](#15-resource-management-using)
16. [Dynamic Typing (var)](#16-dynamic-typing-var)
17. [Nullable Types](#17-nullable-types)
18. [Operator Overloading](#18-operator-overloading)
19. [Standard Library Reference](#19-standard-library-reference)
20. [Collections (List, Map, Set)](#20-collections-list-map-set)
21. [File I/O](#21-file-io)
22. [Networking](#22-networking)
23. [Threading](#23-threading)
24. [JSON](#24-json)
25. [Cryptography Library](#25-cryptography-library)
26. [HTTP Library](#26-http-library)
27. [Regex Library](#27-regex-library)
28. [Torch Library (Machine Learning)](#28-torch-library-machine-learning)
29. [nex3d Library (3D Game Engine)](#29-nex3d-library-3d-game-engine)
30. [nex_ui Library (Desktop GUI)](#30-nex_ui-library-desktop-gui)
31. [Declarative UI (.nexui Markup)](#31-declarative-ui-nexui-markup)
32. [Complete Code Examples](#32-complete-code-examples)
33. [Common Patterns & Idioms](#33-common-patterns--idioms)
34. [Limitations & Gotchas](#34-limitations--gotchas)
35. [EBNF Grammar Reference](#35-ebnf-grammar-reference)

---

## 1. Language Overview

**Nex** is a compiled, statically-typed, class-based programming language designed for native performance with a clean, modern syntax. Key characteristics:

- **File extension**: `.nex`
- **Compiled**: AOT via Cranelift or LLVM; JIT for scripting
- **Statically typed** with local type inference by first assignment
- **Class-based OOP** with real multiple inheritance and interfaces
- **Garbage collected** (tracing, mark-and-sweep)
- **Automatic Semicolon Insertion (ASI)** — semicolons are optional
- **Brace-delimited blocks** `{ }`
- **No closures/lambdas** in v1 — function references only
- **Comments**: `// line` and `/* block */`

### Hello World

```nex
def main() -> Unit {
    println("Hello, World!")
    return
}
```

Every Nex application has a `main` function as its entry point.

---

## 2. Project Structure & Tooling

### 2.1 Creating a Project

```bash
nex new myapp          # Create new application project
nex new mylib --lib    # Create new library project
```

This creates:

```
myapp/
├── project.toml       # Project manifest
└── src/
    └── main.nex       # Entry point
```

### 2.2 project.toml

```toml
name = "myapp"
version = "0.1.0"
entry = "src/main.nex"

[libs]
nex3d = { git = "nexlang/nex3d", version = "0.1.0" }
torch = { path = "../libs/torch" }

[native]
nex3d_native = "nex3d_native.dll"
```

### 2.3 CLI Commands

```bash
nex build              # Compile to executable (AOT)
nex build --lib        # Compile to shared library (DLL)
nex run                # JIT compile and execute
nex run -- --arg1 val  # Pass arguments to program
nex install user/repo  # Install library from GitHub
nex uninstall name     # Remove library
nex list               # List dependencies
nex fmt file.nex       # Format source code
nex lint file.nex      # Run linter
nex repl               # Interactive REPL
nex clean              # Remove build artifacts
```

### 2.4 Module-to-File Mapping

Source files map to module names by path:

| File Path | Module Name |
|-----------|-------------|
| `src/main.nex` | Entry point (no module name) |
| `src/utils.nex` | `utils` |
| `src/net/http.nex` | `net.http` |
| `src/data/models.nex` | `data.models` |

---

## 3. Syntax Fundamentals

### 3.1 Comments

```nex
// This is a line comment

/* This is a
   block comment */
```

### 3.2 Automatic Semicolon Insertion (ASI)

Semicolons are **optional**. The compiler inserts them automatically after:
- Identifiers, literals, `self`
- Closing delimiters: `)`, `]`, `}`
- Keywords: `return`, `break`, `continue`

You may use explicit semicolons, but idiomatic Nex omits them:

```nex
// Idiomatic — no semicolons
x = 10
y = 20
println(x + y)

// Also valid — explicit semicolons
x = 10; y = 20; println(x + y)
```

### 3.3 Blocks

Blocks are brace-delimited sequences of statements:

```nex
{
    statement1
    statement2
}
```

### 3.4 Identifiers

Identifiers start with a letter or underscore, followed by letters, digits, or underscores. They are case-sensitive.

```nex
myVar
_private
camelCase
PascalCase
snake_case
```

### 3.5 Reserved Keywords

```
as        break     catch     class     continue
def       else      false     finally   for
from      if        import    interface null
override  partial   public    return    self
shared    static    struct    throw     true
try       using     var       virtual   while
```

### 3.6 Built-in Type Keywords

```
Bool     Byte     Char     Double   Float
Float32  Float64  Int      Int8     Int16
Int32    Int64    Long     Short    String
UInt     UInt8    UInt16   UInt32   UInt64
ULong    UShort   Unit     Var
```

---

## 4. Type System

Nex is statically typed with local type inference.

### 4.1 Primitive Value Types

| Type | Size | Description | Literal Examples |
|------|------|-------------|------------------|
| `Bool` | 1 byte | Boolean | `true`, `false` |
| `Byte` | 1 byte | Unsigned 8-bit integer | `255b` |
| `Int` | 4 bytes | Signed 32-bit integer | `42`, `0xFF` |
| `Int64` | 8 bytes | Signed 64-bit integer | `42i64` |
| `Float` | 4 bytes | 32-bit IEEE 754 | `3.14f` |
| `Double` | 8 bytes | 64-bit IEEE 754 | `3.14`, `3.14d` |
| `Char` | 4 bytes | Unicode scalar | `'A'`, `'\n'`, `'\u0041'` |

**Default literal types:**
- Bare integer literals (`42`) are `Int`
- Bare float literals (`3.14`) are `Double`

**Integer suffixes:** `i` (Int), `u` (UInt), `l` (Long/Int64)
**Float suffixes:** `f` (Float), `d` (Double)

### 4.2 Reference Types

| Type | Description |
|------|-------------|
| `String` | Immutable, heap-allocated UTF-8 string |
| `Unit` | The "void" type — functions that return nothing return `Unit` |
| `Null` | The type of the `null` literal |
| `Object` | Universal base class of all classes |
| `Var` | Runtime dynamic type (see section 16) |

### 4.3 Composite Types

- **Classes** — Reference types with inheritance, methods, fields (see section 9)
- **Structs** — Value types with copy semantics (see section 10)
- **Interfaces** — Method-only contracts (see section 11)

### 4.4 Generic Types

```nex
List[T]            // Generic list
Map[K, V]          // Generic map
Box[Int]           // Instantiated generic
```

### 4.5 Nullable Types

Any type can be made nullable by appending `?`:

```nex
name: String? = null
count: Int? = null
```

### 4.6 Function Types

```nex
callback: (Int, Int) -> Bool
handler: (String) -> Unit
```

### 4.7 Numeric Promotion

When mixing numeric types in operations, the result is promoted to the wider type:

```
Double > Float > Int64 > Int > Byte
```

- `Int + Double` → `Double`
- `Float + Int` → `Float`
- `Int + Int64` → `Int64`

### 4.8 String Escape Sequences

```nex
"\\"     // Backslash
"\""     // Double quote
"\n"     // Newline
"\r"     // Carriage return
"\t"     // Tab
"\0"     // Null character
"\b"     // Backspace
"\xHH"   // Hex byte (e.g., "\x41" = 'A')
```

For `Char` literals: `'\uFFFF'` for Unicode code points.

---

## 5. Variables & Assignment

### 5.1 Type-Inferred Variables

Variables are created by first assignment. The type is inferred from the right-hand side:

```nex
name = "Alice"       // name: String
age = 30             // age: Int
pi = 3.14            // pi: Double
active = true        // active: Bool
```

**IMPORTANT**: Once a variable's type is inferred, it cannot be reassigned to a different type:

```nex
x = 10               // x: Int
x = 20               // OK — same type
x = "hello"          // COMPILE ERROR — type mismatch
```

### 5.2 Explicit Type Annotation

```nex
count: Int = 0
name: String = "Alice"
pi: Float = 3.14f
```

### 5.3 Dynamic Variables (var)

Use the `var` keyword for runtime dynamic typing:

```nex
var x = 10
x = "hello"          // OK — var allows type changes at runtime
x = 3.14             // OK
```

See [section 16](#16-dynamic-typing-var) for full details.

### 5.4 Assignment Operators

```nex
x = 10               // Simple assignment
x += 5               // x = x + 5
x -= 3               // x = x - 3
x *= 2               // x = x * 2
x /= 4               // x = x / 4
```

---

## 6. Operators

### 6.1 Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition / String concatenation | `a + b`, `"Hi " + name` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Modulo (remainder) | `a % b` |

### 6.2 Comparison Operators

| Operator | Description | Returns |
|----------|-------------|---------|
| `==` | Equal | `Bool` |
| `!=` | Not equal | `Bool` |
| `<` | Less than | `Bool` |
| `<=` | Less than or equal | `Bool` |
| `>` | Greater than | `Bool` |
| `>=` | Greater than or equal | `Bool` |

### 6.3 Logical Operators

| Operator | Description |
|----------|-------------|
| `&&` | Logical AND (short-circuit) |
| `\|\|` | Logical OR (short-circuit) |
| `!` | Logical NOT (unary) |

### 6.4 Unary Operators

| Operator | Description |
|----------|-------------|
| `-` | Numeric negation |
| `!` | Logical NOT |
| `+` | Unary plus (no-op) |

### 6.5 String Concatenation

The `+` operator concatenates strings. Non-string values are automatically converted:

```nex
"Hello, " + name + "!"        // String + String
"Age: " + age                  // String + Int → String
"Value: " + 3.14              // String + Double → String
```

### 6.6 Operator Precedence (highest to lowest)

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 12 | `*`, `/`, `%` | Left |
| 10 | `+`, `-` | Left |
| 8 | `<`, `<=`, `>`, `>=` | Left |
| 6 | `==`, `!=` | Left |
| 4 | `&&` | Left |
| 2 | `\|\|` | Left |
| 1 | `=`, `+=`, `-=`, `*=`, `/=` | Right |

### 6.7 Member Access

```nex
object.field           // Dot access
object.method()        // Method call
object::member         // Qualified access (for MI disambiguation)
self.Base::method()    // Call specific base class method
```

---

## 7. Control Flow

### 7.1 if / else if / else

```nex
if (condition) {
    // ...
}

if (condition) {
    // ...
} else {
    // ...
}

if (condition1) {
    // ...
} else if (condition2) {
    // ...
} else {
    // ...
}
```

**Note**: Parentheses around the condition are required. Braces are required.

### 7.2 while Loop

```nex
i = 0
while (i < 10) {
    println(i)
    i = i + 1
}
```

### 7.3 for Loop (C-style)

```nex
for (i = 0; i < 10; i = i + 1) {
    println(i)
}
```

**Note**: There is no `++` or `--` operator. Use `i = i + 1`.

### 7.4 for-in Loop (Iteration)

```nex
for (item in collection) {
    println(item)
}
```

Works with `List` and other iterable types.

### 7.5 break and continue

```nex
i = 0
while (i < 100) {
    if (i == 50) {
        break            // Exit loop
    }
    if (i % 2 == 0) {
        i = i + 1
        continue         // Skip to next iteration
    }
    println(i)
    i = i + 1
}
```

### 7.6 return

```nex
def add(a: Int, b: Int) -> Int {
    return a + b
}

def greet() -> Unit {
    println("Hello")
    return                // Explicit return for Unit functions
}
```

---

## 8. Functions

### 8.1 Function Declaration

```nex
def functionName(param1: Type1, param2: Type2) -> ReturnType {
    // body
    return value
}
```

**Rules:**
- Parameters require explicit type annotations
- Return type is specified after `->`
- Use `-> Unit` for functions that don't return a value
- Functions without a return type annotation default to `-> Unit`
- All code paths should have an explicit `return` statement

### 8.2 Examples

```nex
// Function with parameters and return value
def add(a: Int, b: Int) -> Int {
    return a + b
}

// Function returning Unit (void)
def greet(name: String) -> Unit {
    println("Hello, " + name + "!")
    return
}

// Recursive function
def factorial(n: Int) -> Int {
    if (n <= 1) {
        return 1
    }
    return n * factorial(n - 1)
}
```

### 8.3 Function Visibility

```nex
def internal_helper() -> Unit { }      // Module-internal (default)
public def api_function() -> Unit { }  // Exported to other modules
```

### 8.4 Static Functions

```nex
class MyClass {
    static def create() -> MyClass {
        return MyClass()
    }
}
```

### 8.5 Function References

Functions are first-class values. You can pass them as callbacks:

```nex
def on_click() {
    println("Clicked!")
}

// Pass function reference as callback
engine_set_update(on_click)
```

**Note**: Nex v1 does not have closures or lambdas. Only named function references.

---

## 9. Classes

Classes are reference types allocated on the heap and managed by the garbage collector.

### 9.1 Basic Class

```nex
class Person {
    name: String
    age: Int

    def init(name: String, age: Int) -> Unit {
        self.name = name
        self.age = age
        return
    }

    def greet() -> String {
        return "Hi, I'm " + self.name
    }
}

def main() -> Unit {
    p = Person("Alice", 30)
    println(p.greet())            // "Hi, I'm Alice"
    println(p.name)               // "Alice"
    return
}
```

### 9.2 Constructors

The constructor is always named `init`:

```nex
class Point {
    x: Float
    y: Float

    def init(x: Float, y: Float) -> Unit {
        self.x = x
        self.y = y
        return
    }
}

// Construct by calling the class name
p = Point(1.0f, 2.0f)
```

### 9.3 The self Keyword

Inside methods, `self` refers to the current instance:

```nex
class Counter {
    count: Int

    def init() -> Unit {
        self.count = 0
        return
    }

    def increment() -> Unit {
        self.count = self.count + 1
        return
    }

    def get() -> Int {
        return self.count
    }
}
```

### 9.4 Visibility

```nex
class MyClass {
    name: String               // Internal (default)
    public id: Int             // Public — accessible from other modules

    def helper() -> Unit { }   // Internal method
    public def api() -> Unit { }  // Public method
}
```

### 9.5 Single Inheritance

```nex
class Animal {
    name: String

    def init(name: String) -> Unit {
        self.name = name
        return
    }

    def speak() -> String {
        return self.name + " makes a sound"
    }
}

class Dog : Animal {
    breed: String

    def init(name: String, breed: String) -> Unit {
        // Base constructor called via base spec
        self.name = name
        self.breed = breed
        return
    }

    def fetch() -> String {
        return self.name + " fetches the ball!"
    }
}
```

### 9.6 Multiple Inheritance

Nex supports real multiple inheritance with the subobject model:

```nex
class Flyable {
    def fly() -> String {
        return "Flying!"
    }
}

class Swimmable {
    def swim() -> String {
        return "Swimming!"
    }
}

class Duck : Flyable, Swimmable {
    def describe() -> String {
        return self.fly() + " and " + self.swim()
    }
}
```

**Diamond Problem — Shared Bases:**

```nex
class Base {
    value: Int
}

class Left : shared Base { }
class Right : shared Base { }
class Diamond : Left, Right {
    // Only ONE copy of Base exists (shared)
}
```

**Base Disambiguation:**

```nex
class A {
    def method() -> String { return "A" }
}
class B {
    def method() -> String { return "B" }
}
class C : A, B {
    def call_a() -> String {
        return self.A::method()    // Explicitly call A's version
    }
    def call_b() -> String {
        return self.B::method()    // Explicitly call B's version
    }
}
```

### 9.7 Virtual Methods and Override

Methods are non-virtual by default. Use `virtual` and `override`:

```nex
class Shape {
    virtual def area() -> Double {
        return 0.0
    }
}

class Circle : Shape {
    radius: Double

    def init(r: Double) -> Unit {
        self.radius = r
        return
    }

    override def area() -> Double {
        return 3.14159265358979 * self.radius * self.radius
    }
}
```

### 9.8 Base Constructor Arguments

```nex
class Base {
    x: Int
    def init(x: Int) -> Unit {
        self.x = x
        return
    }
}

class Child : Base(42) {
    // Base is constructed with x=42
}
```

### 9.9 Partial Classes

Used for code-behind in UI systems:

```nex
public partial class MainWindow {
    title: String = "My App"

    def on_click(widget_id: Int64, event_kind: Int64) {
        println("Clicked!")
        return
    }
}
```

### 9.10 Implicit Object Base

All classes implicitly inherit from `Object` unless they explicitly specify a base class. `Object` is the universal base.

---

## 10. Structs (Value Types)

Structs are value types: stored inline, copied on assignment.

### 10.1 Basic Struct

```nex
struct Point {
    x: Float
    y: Float
}

// Construct
p = Point(1.0f, 2.0f)
println(p.x)           // 1.0
```

### 10.2 Struct with Methods

```nex
public struct Vec2 {
    x: Float
    y: Float

    public def length() -> Float {
        return sqrt(self.x * self.x + self.y * self.y)
    }
}
```

### 10.3 Struct Rules

- **No inheritance** (but can implement interfaces)
- **Copy semantics** — assignment copies all fields
- **No GC overhead** — stored inline on stack or in containing object
- Fields are value-initialized (0, 0.0, false, null) by default
- Structs are boxed when assigned to a `Var`

---

## 11. Interfaces

Interfaces define method contracts without implementation.

### 11.1 Basic Interface

```nex
public interface Greeter {
    def greet(name: String) -> String
}
```

### 11.2 Implementing an Interface

```nex
class FriendlyGreeter : Greeter {
    def greet(name: String) -> String {
        return "Hello, " + name + "!"
    }
}
```

### 11.3 Multiple Interfaces

```nex
interface Serializable {
    def serialize() -> String
}

interface Printable {
    def display() -> Unit
}

class Document : Serializable, Printable {
    content: String

    def serialize() -> String {
        return self.content
    }

    def display() -> Unit {
        println(self.content)
        return
    }
}
```

### 11.4 Interface Rules

- Methods are implicitly `public` and `virtual`
- No fields, no state, no constructors
- Cannot be instantiated directly
- Interfaces can extend other interfaces

---

## 12. Generics

### 12.1 Generic Classes

```nex
public class Box[T] {
    value: T

    def init(v: T) -> Unit {
        self.value = v
        return
    }

    def get() -> T {
        return self.value
    }
}

// Usage
intBox = Box[Int](42)
strBox = Box[String]("hello")
println(intBox.get())     // 42
```

### 12.2 Generic Functions

```nex
public def identity[T](value: T) -> T {
    return value
}

result = identity[Int](42)
```

### 12.3 Generic Structs

```nex
struct Pair[A, B] {
    first: A
    second: B
}

p = Pair[Int, String](1, "one")
```

### 12.4 Generic Interfaces

```nex
interface Comparable[T] {
    def compare_to(other: T) -> Int
}
```

### 12.5 Built-in Generic Types

- `List[T]` — Dynamic array
- `Map[K, V]` — Hash map
- `Set[T]` — Hash set

### 12.6 Limitations (v1)

- **No type constraints** (no `where T : Comparable`)
- **No variance annotations** (no `in`/`out`)
- **No default type parameters**
- Implementation uses **monomorphization** (compile-time specialization)

---

## 13. Modules & Imports

### 13.1 Import a Module

```nex
import std.math                    // Import module
import std.math as math            // Import with alias
```

After importing, access members with dot notation:

```nex
import std.math
result = std.math.sqrt(16.0)

import std.math as m
result = m.sqrt(16.0)
```

### 13.2 Import Specific Names

```nex
from std.io import open_console, File
from std.math import sqrt, sin, cos
```

These names become available directly without module prefix:

```nex
from std.math import sqrt
result = sqrt(16.0)         // No prefix needed
```

### 13.3 Visibility

Declarations are **module-internal by default**. Use `public` to export:

```nex
// In mylib/src/utils.nex
def internal_helper() -> Unit { }       // Only visible within this module
public def public_api() -> Unit { }     // Visible to importers
```

### 13.4 External Libraries

Add to `project.toml`:

```toml
[libs]
nex3d = { git = "nexlang/nex3d", version = "0.1.0" }
mylib = { path = "../mylib" }
```

Then import:

```nex
import nex3d.engine
import nex3d.math
```

---

## 14. Error Handling

### 14.1 try / catch / finally

```nex
try {
    result = risky_operation()
    println(result)
} catch (err: Error) {
    println("Error: " + err)
} finally {
    println("Cleanup always runs")
}
```

### 14.2 throw

```nex
def validate(age: Int) -> Unit {
    if (age < 0) {
        throw Error("Age cannot be negative")
    }
    return
}
```

### 14.3 Multiple Catch Blocks

```nex
try {
    // ...
} catch (e: FileNotFoundError) {
    println("File not found")
} catch (e: PermissionError) {
    println("Permission denied")
} catch (e: Error) {
    println("Unknown error: " + e)
}
```

### 14.4 Nested try/catch

```nex
try {
    try {
        // inner operation
    } catch (e: Error) {
        println("Inner error handled")
    }
    // continues here after inner catch
} catch (e: Error) {
    println("Outer error")
}
```

---

## 15. Resource Management (using)

The `using` statement provides deterministic cleanup for resources implementing the `Disposable` interface:

```nex
using (resource = open_console()) {
    // Use resource
    println("Working with resource")
}
// resource.dispose() is called automatically here, even if an exception occurs
```

### Multiple Resources

```nex
using (file = File.open("data.txt"), conn = db_connect()) {
    content = file.read_all()
    conn.execute(content)
}
// Both file and conn are disposed
```

---

## 16. Dynamic Typing (var)

The `var` keyword creates a dynamically-typed variable. Operations are resolved at runtime.

### 16.1 Basic Usage

```nex
var x = 10          // x holds Int
x = "hello"         // OK — now holds String
x = 3.14            // OK — now holds Double
x = true            // OK — now holds Bool
```

### 16.2 Runtime Operations

```nex
var a = 10
var b = 20
var result = a + b   // Runtime dispatch → 30

var s = "hello"
var t = " world"
var msg = s + t      // Runtime dispatch → "hello world"
```

### 16.3 Suppress API Warning

Using `Var` in public APIs generates a warning. Suppress with:

```nex
@allow_var_api
public def dynamic_handler(input: Var) -> Var {
    // ...
}
```

### 16.4 Var Internals

At runtime, `Var` is a tagged union: `{ tag: u8, int_val: i64, float_val: f64, obj_val: *NexObj }`.

---

## 17. Nullable Types

### 17.1 Declaring Nullable Variables

```nex
name: String? = null
count: Int? = null
user: Person? = null
```

### 17.2 Null Checks

```nex
name: String? = get_name()

if (name != null) {
    // name is narrowed to String inside this block
    println(name)
}
```

### 17.3 Assigning Null

```nex
result: String? = null
result = "found"      // OK
result = null          // OK — back to null
```

---

## 18. Operator Overloading

Structs and classes can define custom operator behavior:

```nex
public struct Vec2 {
    x: Float
    y: Float

    public static def operator+(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x + b.x, a.y + b.y)
    }

    public static def operator-(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x - b.x, a.y - b.y)
    }

    public static def operator*(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x * b.x, a.y * b.y)
    }

    public static def operator==(a: Vec2, b: Vec2) -> Bool {
        return a.x == b.x && a.y == b.y
    }
}

// Usage
v1 = Vec2(1.0f, 2.0f)
v2 = Vec2(3.0f, 4.0f)
v3 = v1 + v2          // Vec2(4.0, 6.0)
```

**Overloadable Operators**: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `!` (unary)

---

## 19. Standard Library Reference

The standard library is automatically available. Import specific modules as needed.

### 19.1 Core (always available)

```nex
print(value)           // Print without newline (accepts Int, String, Double, Bool, Char)
println(value)         // Print with newline
```

### 19.2 std.math

```nex
import std.math

abs(x)                 // Absolute value
min(a, b)              // Minimum of two values
max(a, b)              // Maximum of two values
clamp(x, lo, hi)       // Clamp x between lo and hi
floor(x)               // Floor (Double → Int)
ceil(x)                // Ceiling (Double → Int)
round(x)               // Round to nearest (Double → Int)
sqrt(x)                // Square root
pow(base, exp)         // Power (base^exp)
sin(x)                 // Sine (radians)
cos(x)                 // Cosine (radians)
tan(x)                 // Tangent (radians)
log(x)                 // Natural logarithm
exp(x)                 // e^x
random()               // Random Double in [0.0, 1.0)
```

### 19.3 std.string

```nex
import std.string

str_length(s)                       // Length of string
str_substring(s, start, len)        // Substring
str_split(s, delimiter)             // Split into List
str_trim(s)                         // Trim whitespace
str_starts_with(s, prefix)          // Check prefix
str_ends_with(s, suffix)            // Check suffix
str_contains(s, substring)          // Check containment
str_index_of(s, substring)          // First index (-1 if not found)
str_replace(s, old, new_str)        // Replace all occurrences
str_to_upper(s)                     // To uppercase
str_to_lower(s)                     // To lowercase
str_repeat(s, count)                // Repeat N times
str_char_at(s, index)               // Character at index
str_reverse(s)                      // Reverse string
```

### 19.4 std.convert

```nex
import std.convert

parse_int(s)           // String → Int
parse_float(s)         // String → Double
parse_bool(s)          // String → Bool ("true"/"false")
char_to_str(c)         // Char → String
str_to_chars(s)        // String → List of Chars
```

### 19.5 std.env

```nex
import std.env

env_get(name)          // Get environment variable (String)
env_set(name, value)   // Set environment variable
env_has(name)          // Check if env var exists (Bool)
env_args_count()       // Number of command-line arguments
env_args_get(index)    // Get argument by index (String)
cwd()                  // Current working directory
```

### 19.6 std.time

```nex
import std.time

now_millis()           // Current time in milliseconds (Int64)
now_nanos()            // Current time in nanoseconds (Int64)
sleep_millis(ms)       // Sleep for N milliseconds
elapsed_millis(start)  // Milliseconds since start timestamp
```

### 19.7 std.path

```nex
import std.path

path_join(a, b)        // Join two path segments
path_parent(p)         // Parent directory
path_file_name(p)      // File name component
path_extension(p)      // File extension
path_stem(p)           // File name without extension
path_is_absolute(p)    // Check if absolute path (Bool)
path_normalize(p)      // Normalize path separators
path_separator()       // OS path separator ("/" or "\\")
```

### 19.8 std.process

```nex
import std.process

process_exec(command)              // Execute command (blocking)
process_exec_output(command)       // Execute and capture output (String)
process_exit(code)                 // Exit process with code
process_pid()                      // Current process ID
process_spawn(command)             // Spawn child process (non-blocking)
process_wait(handle)               // Wait for spawned process
```

### 19.9 std.logging

```nex
import std.logging

log_debug(message)         // Debug-level log
log_info(message)          // Info-level log
log_warn(message)          // Warning-level log
log_error(message)         // Error-level log
log_set_level(level)       // Set minimum log level (0=debug, 1=info, 2=warn, 3=error)
log_with_tag(tag, message) // Log with custom tag
```

### 19.10 std.testing

```nex
import std.testing

assert(condition)                    // Assert condition is true
assert_eq_int(actual, expected)      // Assert two ints are equal
assert_eq_str(actual, expected)      // Assert two strings are equal
assert_eq_float(actual, expected)    // Assert two floats are equal
assert_ne_int(actual, expected)      // Assert two ints differ
assert_true(condition)               // Assert true
```

---

## 20. Collections (List, Map, Set)

### 20.1 List

```nex
// Create list
items = List()

// Add items
items.add("first")
items.add("second")
items.add("third")

// Access
items.get(0)               // "first"
items.set(1, "modified")   // Replace at index
items.length()             // 3
items.remove(0)            // Remove at index

// Iteration
for (item in items) {
    println(item)
}

// Extended operations (import std.collections)
list_sort(items)           // Sort in place
list_reverse(items)        // Reverse in place
list_clear(items)          // Remove all elements
list_contains(items, val)  // Check if value exists
list_index_of(items, val)  // Find index (-1 if not found)
```

### 20.2 Map

```nex
// Create map
data = Map()

// Add entries
data.put("name", "Alice")
data.put("age", "30")

// Access
data.get("name")           // "Alice"
data.contains("name")      // true
data.remove("name")        // Remove entry
data.size()                // Number of entries
```

### 20.3 Set

```nex
// Create set
tags = Set()

// Add items
tags.add("nex")
tags.add("language")
tags.contains("nex")       // true
tags.remove("nex")
```

---

## 21. File I/O

```nex
from std.io import File, read_line

// Read a file
content = File.read_all("data.txt")
println(content)

// Write a file
File.write_text("output.txt", "Hello, World!")

// File operations
file_exists("data.txt")              // Bool
file_delete("temp.txt")              // Delete file
file_rename("old.txt", "new.txt")    // Rename file
file_copy("src.txt", "dst.txt")      // Copy file
file_size("data.txt")                // File size in bytes

// Directory operations
mkdir("new_dir")                      // Create directory
list_dir(".")                         // List directory contents

// Read line from stdin
input = read_line()

// Resource-safe file I/O
using (f = File.open("data.txt")) {
    content = f.read_all()
    println(content)
}
```

---

## 22. Networking

```nex
import std.net

// TCP Client
sock = tcp_connect("example.com", 80)
tcp_send(sock, "GET / HTTP/1.0\r\n\r\n")
response = tcp_recv(sock, 4096)
tcp_close(sock)

// TCP Server
server = tcp_listen("0.0.0.0", 8080)
client = tcp_accept(server)
data = tcp_recv(client, 1024)
tcp_send(client, "HTTP/1.0 200 OK\r\n\r\nHello!")
tcp_close(client)
tcp_close(server)

// UDP
sock = udp_bind("0.0.0.0", 9000)
udp_send(sock, "message", "127.0.0.1", 9001)
data = udp_recv(sock, 1024)
udp_close(sock)
```

---

## 23. Threading

```nex
import std.threading

// Spawn a thread
handle = thread_spawn(my_function)

// Wait for thread to finish
thread_join(handle)

// Sleep
thread_sleep(1000)         // Sleep 1 second

// Current thread ID
id = thread_current_id()

// Mutex
mtx = mutex_new()
mutex_lock(mtx)
// ... critical section ...
mutex_unlock(mtx)
```

---

## 24. JSON

```nex
import std.json

// Parse JSON string
obj = json_parse("{\"name\": \"Alice\", \"age\": 30}")

// Extract values
name = json_get_string(obj, "name")     // "Alice"
age = json_get_int(obj, "age")          // 30
score = json_get_float(obj, "score")    // Double
active = json_get_bool(obj, "active")   // Bool

// Create JSON string
output = json_stringify(obj)
```

---

## 25. Cryptography Library

Add to `project.toml`:

```toml
[libs]
crypto = { path = "../libs/crypto" }
```

```nex
import crypto.hash
import crypto.encode
import crypto.hmac
import crypto.random

// Hashing
hash = sha256("hello")                  // HashResult { hex, algorithm }
println(hash.hex)
sha256_verify("hello", hash.hex)        // Bool

hash512 = sha512("hello")
md5hash = md5("hello")

// Base64
encoded = base64_encode("hello")        // "aGVsbG8="
decoded = base64_decode(encoded)        // "hello"

// HMAC
mac = crypto_hmac_sha256("secret-key", "message")

// Secure random
buf = random_bytes(32)                  // 32 random bytes
```

---

## 26. HTTP Library

Add to `project.toml`:

```toml
[libs]
http = { path = "../libs/http" }
```

```nex
import http.client

// Simple GET
response = fetch("https://api.example.com/data")
println(response.status)    // 200
println(response.body)      // Response body as String

// GET returning body only
body = get_text("https://api.example.com/data")

// POST JSON
response = post_json("https://api.example.com/create", "{\"name\": \"test\"}")

// POST Form
response = post_form("https://api.example.com/login", "user=admin&pass=secret")

// Low-level API
handle = http_get("https://api.example.com/data")
status = response_status(handle)
body = response_body(handle)
header = response_header(handle, "Content-Type")
response_free(handle)

// Status helpers
is_success(200)        // true
is_redirect(301)       // true
is_client_error(404)   // true
is_server_error(500)   // true

// Status constants
STATUS_OK()            // 200
STATUS_CREATED()       // 201
STATUS_NOT_FOUND()     // 404

// Content type constants
CONTENT_JSON()         // "application/json"
CONTENT_FORM()         // "application/x-www-form-urlencoded"
```

---

## 27. Regex Library

Add to `project.toml`:

```toml
[libs]
regex = { path = "../libs/regex" }
```

```nex
import regex.pattern

// Compile and use
re = regex_compile("[0-9]+")
regex_is_match(re, "hello123")           // true
result = regex_find(re, "abc 42 def")    // MatchResult { matched: true, value: "42" }
replaced = regex_replace_all(re, "a1b2c3", "X")  // "aXbXcX"
regex_free(re)

// One-shot helpers (compile, use, free in one call)
quick_match("[a-z]+", "hello")           // true
result = quick_find("\\d+", "test 42")   // MatchResult
replaced = quick_replace("\\s+", "a b c", "-")  // "a-b-c"

// Validation helpers
is_email("user@example.com")             // true
is_url("https://example.com")            // true
is_ipv4("192.168.1.1")                   // true
contains_digits("hello123")              // true

// Built-in patterns
PATTERN_EMAIL()
PATTERN_URL()
PATTERN_IPV4()
PATTERN_DIGITS()
```

---

## 28. Torch Library (Machine Learning)

Add to `project.toml`:

```toml
[libs]
torch = { path = "../libs/torch" }

[native]
nex_torch_native = "nex_torch_native.dll"
```

Requires `libtorch` installed on the system.

### 28.1 Tensor Creation

```nex
import torch.tensor

// Basic creation
a = tensor_zeros(shape, ndims)       // All zeros
b = tensor_ones(shape, ndims)        // All ones
c = tensor_rand(shape, ndims)        // Random uniform [0, 1)
d = tensor_randn(shape, ndims)       // Random normal (mean=0, std=1)
e = tensor_eye(n)                    // Identity matrix
f = tensor_arange(0.0, 10.0, 1.0)   // [0, 1, 2, ..., 9]

// From data
t = tensor_from_float_data(data_ptr, shape, ndims)
```

### 28.2 Tensor Operations

```nex
// Arithmetic
result = tensor_add(a, b)       // or: a + b  (operator overloaded)
result = tensor_sub(a, b)       // or: a - b
result = tensor_mul(a, b)       // Element-wise multiply
result = tensor_div(a, b)       // Element-wise divide
result = tensor_neg(a)          // Negate all elements

// Matrix operations
result = tensor_matmul(a, b)    // Matrix multiplication: a @ b
result = a.matmul(b)            // Method syntax

// Element-wise math
tensor_exp(t)
tensor_log(t)
tensor_sqrt(t)
tensor_abs(t)
tensor_clamp(t, min_val, max_val)

// Reductions
tensor_sum(t)                   // Sum all elements
tensor_mean(t)                  // Mean of all elements
tensor_sum_dim(t, dim, keepdim) // Sum along dimension
tensor_mean_dim(t, dim, keepdim)

// Softmax
tensor_softmax(t, dim)

// Shape operations
tensor_reshape(t, shape, ndims)
tensor_transpose(t, dim0, dim1)
tensor_squeeze(t)               // Remove dimensions of size 1
tensor_unsqueeze(t, dim)        // Add dimension
tensor_flatten(t, start, end)
tensor_cat(a, b, dim)           // Concatenate
tensor_narrow(t, dim, start, len)
tensor_index_select(t, dim, indices)

// Shape queries
tensor_shape_dim(t, dim)        // Size of dimension
tensor_ndim(t)                  // Number of dimensions
tensor_numel(t)                 // Total number of elements

// Data access
tensor_get_float(t, index)      // Read single value
tensor_item_float(t)            // Read scalar tensor value (named item())
tensor_to_string(t)             // String representation
tensor_print(t)                 // Print tensor

// Autograd
tensor_requires_grad(t, true)   // Enable gradient tracking
tensor_backward(t)              // Backpropagation
tensor_grad(t)                  // Get gradient tensor
torch_no_grad(1)                // Disable gradient tracking
torch_no_grad(0)                // Re-enable

// Device
cuda_is_available()             // Check CUDA availability (Bool-like Int)
tensor_to_device(t, "cuda")     // Move tensor to GPU
tensor_to_device(t, "cpu")      // Move tensor to CPU

// Utility
tensor_clone(t)                 // Deep copy
tensor_detach(t)                // Detach from computation graph
tensor_contiguous(t)            // Make memory contiguous
tensor_to_dtype_float(t)        // Convert to float32
tensor_to_dtype_long(t)         // Convert to int64

// Comparison
tensor_eq_scalar(t, value)      // Element-wise == scalar
tensor_gt_scalar(t, value)      // Element-wise > scalar
tensor_lt_scalar(t, value)      // Element-wise < scalar

// Seed
torch_manual_seed(42)           // Set random seed
```

### 28.3 Neural Network Layers

```nex
import torch.nn

// Create a sequential module container
model = nn_sequential_new()

// Add layers
nn_linear(model, in_features, out_features)   // Fully connected
nn_conv2d(model, in_channels, out_channels, kernel_size)

// Activations
nn_relu(model)
nn_sigmoid(model)
nn_tanh(model)
nn_gelu(model)
nn_softmax(model, dim)

// Normalization
nn_batch_norm(model, num_features)
nn_layer_norm(model, normalized_shape)

// Regularization
nn_dropout(model, probability)

// Embeddings
nn_embedding(model, num_embeddings, embedding_dim)

// Forward pass
output = nn_forward(model, input_tensor)
// or method syntax:
output = model.forward(input_tensor)

// Move to device
nn_to_device(model, "cuda")
// or method syntax:
model.to_device("cuda")

// Free resources
nn_free(model)

// High-level builders
model = build_mlp(in_features, hidden_size, out_features)
model = build_classifier(in_features, hidden_size, num_classes)
```

### 28.4 Loss Functions

```nex
import torch.loss

loss = mse_loss(predictions, targets)              // Mean Squared Error
loss = cross_entropy_loss(predictions, targets)     // Cross-Entropy
loss = bce_loss(predictions, targets)               // Binary Cross-Entropy
```

### 28.5 Optimizers

```nex
import torch.optim

optimizer = optim_adam(model, learning_rate)
optimizer = optim_sgd(model, learning_rate)
optimizer = adam_default(model)       // lr = 0.001
optimizer = sgd_default(model)       // lr = 0.01

// Training step
optim_zero_grad(optimizer)           // Zero gradients
// ... forward + backward ...
optim_step(optimizer)                // Update weights

optim_free(optimizer)                // Clean up
```

### 28.6 Training Utilities

```nex
import torch.train

// Complete training step (zero_grad → forward → loss → backward → step)
loss = train_step_mse(model, optimizer, input, target)
loss = train_step_ce(model, optimizer, input, target)
```

### 28.7 Model I/O

```nex
import torch.model

model_save(module, "weights.pt")     // Save model weights
model_load(module, "weights.pt")     // Load model weights

// TorchScript
module = jit_load("model.pt")       // Load TorchScript model
output = jit_forward(module, input)  // Run inference
```

---

## 29. nex3d Library (3D Game Engine)

Add to `project.toml`:

```toml
[libs]
nex3d = { path = "../libs/nex3d" }

[native]
nex3d_native = "nex3d_native.dll"
```

### 29.1 Window & Game Loop

```nex
import nex3d.engine

// Create window
handle = engine_create_window("My Game", 1024, 768)

// Configure
engine_set_clear_color(0.1f, 0.1f, 0.1f, 1.0f)

// Camera setup
engine_camera_position(0.0f, 5.0f, 10.0f)
engine_camera_target(0.0f, 0.0f, 0.0f)
engine_camera_up(0.0f, 1.0f, 0.0f)
engine_camera_perspective(fov, aspect, near, far)

// Set update callback and run
engine_set_update(update_function)
engine_run(handle)
engine_destroy(handle)
```

### 29.2 Drawing Primitives

```nex
import nex3d.draw
import nex3d.color

// 3D primitives
draw_cube(position_vec3, size, color)
draw_grid(size, divisions, color)
draw_triangle(p1, c1, p2, c2, p3, c3)
draw_line(start, end, color)

// Flush all geometry
flush_triangles()

// Colors
COLOR_RED()
COLOR_GREEN()
COLOR_BLUE()
COLOR_YELLOW()
COLOR_CYAN()
COLOR_WHITE()
COLOR_GRAY()
COLOR_BLACK()
```

### 29.3 Input

```nex
import nex3d.input

// Keyboard
key_down(KEY_W())          // Currently held
key_pressed(KEY_SPACE())   // Just pressed this frame
key_released(KEY_ESC())    // Just released this frame

// Key constants
KEY_W(), KEY_A(), KEY_S(), KEY_D()
KEY_UP(), KEY_DOWN(), KEY_LEFT(), KEY_RIGHT()
KEY_SPACE(), KEY_ENTER(), KEY_ESC()
KEY_SHIFT(), KEY_CTRL(), KEY_ALT()
```

### 29.4 Math

```nex
import nex3d.math

// Vector creation
v = vec3(x, y, z)
v = vec2(x, y)

// Math utilities
deg_to_rad(degrees)
rad_to_deg(radians)
```

### 29.5 Time

```nex
import nex3d.time

dt = delta_time()          // Time since last frame (Float, seconds)
t = elapsed_time()         // Time since engine start (Float, seconds)
```

### 29.6 Additional Modules

```nex
import nex3d.camera       // Camera management
import nex3d.light        // Lighting (directional, point, spotlight)
import nex3d.texture      // Texture loading
import nex3d.model        // 3D model loading
import nex3d.sprite       // 2D sprites
import nex3d.audio        // Sound/music playback
import nex3d.font         // Text rendering
import nex3d.anim         // Animation/skeletal animation
import nex3d.bounds       // AABB collision detection
import nex3d.gamepad      // Gamepad/controller input
import nex3d.rendertarget // Off-screen rendering
import nex3d.states       // Game state management
```

---

## 30. nex_ui Library (Desktop GUI)

Add to `project.toml`:

```toml
[libs]
nex_ui = { path = "../libs/nex_ui" }

[native]
nex_ui_native = "nex_ui_native.dll"
```

### 30.1 Creating Widgets

```nex
import nex_ui.app
import nex_ui.widget
import nex_ui.style

// Layout containers
root = ui_column()
row = ui_row()
stack = ui_stack()
grid = ui_grid()
scroll = ui_scroll()

// Widgets
label = ui_text("Hello")
btn = ui_button("Click Me")
input = ui_text_input("Placeholder")
img = ui_image("path/to/image.png")
check = ui_checkbox()
slider = ui_slider(0.0f, 100.0f)
canvas = ui_canvas()

// Add children
ui_add_child(root, label)
ui_add_child(root, btn)
```

### 30.2 Styling

```nex
ui_set_width(widget, 200.0f)
ui_set_height(widget, 40.0f)
ui_set_bg_color(widget, 0x3B82F6FF)    // RGBA hex
ui_set_fg_color(widget, 0xFFFFFFFF)
ui_set_font_size(widget, 16.0f)
ui_set_padding_all(widget, 8.0f)
ui_set_gap(widget, 12.0f)
ui_set_border(widget, 1.0f)
ui_set_border_radius(widget, 4.0f)
```

### 30.3 Event Handling

```nex
import nex_ui.event

def on_button_click(widget_id: Int64, event_kind: Int64) {
    println("Button clicked!")
    return
}

ui_on_click(btn, on_button_click)
```

### 30.4 Updating Widget Text

```nex
ui_set_text(label, "New text")
```

### 30.5 Running the App

```nex
ui_app_set_root(0, root)
// The event loop is managed by the engine if using nex3d overlay,
// or by the UI runtime directly
```

### 30.6 Dialogs

```nex
import nex_ui.dialog

dialog_message("Title", "Message body")
result = dialog_confirm("Title", "Are you sure?")
path = dialog_file_open("Select File", "*.nex")
path = dialog_file_save("Save As", "output.nex")
input = dialog_input("Title", "Enter value:")
```

### 30.7 Data Binding

```nex
import nex_ui.binding

// Notify that a property changed (triggers UI update)
notify_changed("PropertyName")
```

---

## 31. Declarative UI (.nexui Markup)

Nex supports WPF/XAML-inspired declarative UI via `.nexui` files.

### 31.1 Basic Structure

```xml
<Window xmlns="nex_ui" Title="My App" Width="800" Height="600" Background="0x1E293BFF">
  <Column Gap="16" Padding="20">
    <Text Text="Welcome to Nex!" FontSize="24" FgColor="0xFFFFFFFF" />
    <Button Label="Click Me" Click="on_click" />
    <TextInput Placeholder="Enter text..." />
  </Column>
</Window>
```

### 31.2 Layout Containers

```xml
<Column Gap="8">...</Column>        <!-- Vertical stack -->
<Row Gap="8">...</Row>              <!-- Horizontal stack -->
<Stack>...</Stack>                  <!-- Overlapping layers -->
<Grid Columns="3" Rows="2">...</Grid>
<Scroll>...</Scroll>                <!-- Scrollable area -->
```

### 31.3 Data Binding

```xml
<!-- One-way binding (model → UI) -->
<Text Text="{Binding Title}" />

<!-- Two-way binding -->
<TextInput Text="{Binding UserName, Mode=TwoWay}" />

<!-- With converter -->
<Text Text="{Binding Price, Converter=FormatCurrency}" />
```

### 31.4 Widget Switcher

```xml
<WidgetSwitcher Active="{Binding ActiveScreen}">
  <Include Widget="HomeWidget" />
  <Include Widget="CounterWidget" />
  <Include Widget="SettingsWidget" />
</WidgetSwitcher>
```

### 31.5 Styles

**Inline:**

```xml
<Button BgColor="0x3B82F6FF" FgColor="0xFFFFFFFF" FontSize="14" />
```

**Named styles:**

```xml
<Window.Styles>
  <Style Name="PrimaryButton" TargetType="Button">
    <Setter Property="BgColor" Value="0x3B82F6FF"/>
    <Setter Property="FgColor" Value="0xFFFFFFFF"/>
    <Setter Property="FontSize" Value="14"/>
  </Style>
</Window.Styles>

<Button Style="PrimaryButton" Label="Submit" />
```

**External style files (.nexuistyle):**

```xml
<Window.Resources>
  <ResourceDictionary Source="Styles/AppStyles.nexuistyle"/>
</Window.Resources>
```

### 31.6 Code-Behind Pattern

For each `.nexui` file, create a matching `.nex` file:

```
src/MainWindow.nexui    → UI layout
src/MainWindow.nex      → Event handlers and logic
```

The `.nex` file uses `partial class`:

```nex
public partial class MainWindow {
    counter = 0

    def on_increment(widget_id: Int64, event_kind: Int64) {
        counter = counter + 1
        notify_changed("CounterText")
        return
    }

    def CounterText() -> String {
        return "Count: " + counter
    }
}
```

---

## 32. Complete Code Examples

### 32.1 Hello World

```nex
def main() -> Unit {
    println("Hello, World!")
    return
}
```

### 32.2 Factorial with Recursion

```nex
def factorial(n: Int) -> Int {
    if (n <= 1) {
        return 1
    }
    return n * factorial(n - 1)
}

def main() -> Unit {
    println("Factorial of 10: " + factorial(10))
    return
}
```

### 32.3 FizzBuzz

```nex
def fizzbuzz(n: Int) -> String {
    if (n % 15 == 0) {
        return "FizzBuzz"
    } else if (n % 3 == 0) {
        return "Fizz"
    } else if (n % 5 == 0) {
        return "Buzz"
    }
    return ""
}

def main() -> Unit {
    i = 1
    while (i <= 100) {
        result = fizzbuzz(i)
        if (result == "") {
            println(i)
        } else {
            println(result)
        }
        i = i + 1
    }
    return
}
```

### 32.4 Class Hierarchy with Inheritance

```nex
class BaseGreeter {
    prefix: String

    def init(prefix: String) -> Unit {
        self.prefix = prefix
        return
    }

    def format(name: String) -> String {
        return self.prefix + ", " + name
    }
}

class ConsoleGreeter : BaseGreeter {
    def greet(name: String) -> String {
        return self.format(name)
    }
}

def main() -> Unit {
    greeter = ConsoleGreeter("Hello")
    println(greeter.greet("World"))    // "Hello, World"
    return
}
```

### 32.5 Error Handling with Resources

```nex
from std.io import open_console

def safe_divide(a: Int, b: Int) -> Int {
    if (b == 0) {
        throw Error("Division by zero")
    }
    return a / b
}

def main() -> Unit {
    using (resource = open_console()) {
        try {
            result = safe_divide(10, 0)
            println(result)
        } catch (err: Error) {
            println("Error caught: " + err)
        } finally {
            println("Cleanup complete")
        }
    }
    return
}
```

### 32.6 Struct with Operator Overloading

```nex
public struct Vec2 {
    x: Float
    y: Float

    public def length() -> Float {
        return sqrt(self.x * self.x + self.y * self.y)
    }

    public static def operator+(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x + b.x, a.y + b.y)
    }

    public static def operator-(a: Vec2, b: Vec2) -> Vec2 {
        return Vec2(a.x - b.x, a.y - b.y)
    }
}

def main() -> Unit {
    a = Vec2(3.0f, 4.0f)
    b = Vec2(1.0f, 2.0f)
    c = a + b
    println("Sum: (" + c.x + ", " + c.y + ")")
    println("Length: " + a.length())
    return
}
```

### 32.7 Command-Line Argument Parsing

```nex
import std.env
import std.convert

def main() -> Unit {
    name = "World"
    count = 1

    argc = env_args_count()
    i = 1
    while (i < argc) {
        arg = env_args_get(i)
        if (arg == "--name") {
            i = i + 1
            name = env_args_get(i)
        }
        if (arg == "--count") {
            i = i + 1
            count = parse_int(env_args_get(i))
        }
        i = i + 1
    }

    j = 0
    while (j < count) {
        println("Hello, " + name + "!")
        j = j + 1
    }
    return
}
```

### 32.8 HTTP Client

```nex
import http.client

def main() -> Unit {
    response = fetch("https://api.example.com/data")

    if (is_success(response.status)) {
        println("Response: " + response.body)
    } else {
        println("Request failed with status: " + response.status)
    }
    return
}
```

### 32.9 Simple Neural Network

```nex
import torch.tensor
import torch.nn
import torch.loss
import torch.optim

def main() -> Unit {
    // Build a simple 2-layer MLP
    model = build_mlp(10, 64, 2)

    // Create optimizer
    optimizer = optim_adam(model, 0.001)

    // Training loop
    epoch = 0
    while (epoch < 100) {
        input = tensor_randn_2d(32, 10)      // Batch of 32, 10 features
        target = tensor_randn_2d(32, 2)       // Batch of 32, 2 outputs

        // Training step
        optim_zero_grad(optimizer)
        output = model.forward(input)
        loss = mse_loss(output, target)
        loss.backward()
        optim_step(optimizer)

        if (epoch % 10 == 0) {
            print("Epoch " + epoch + " loss = ")
            println(loss.item())
        }

        epoch = epoch + 1
    }

    // Save trained model
    model_save(model, "model.pt")

    // Cleanup
    nn_free(model)
    optim_free(optimizer)
    println("Done!")
    return
}
```

### 32.10 3D Game with Camera and Input

```nex
import nex3d.engine
import nex3d.math
import nex3d.color
import nex3d.input
import nex3d.time
import nex3d.draw

cam_angle = 0.0f
cam_distance = 8.0f

def update() {
    dt = delta_time()

    // Camera rotation
    if (key_down(KEY_LEFT())) {
        cam_angle = cam_angle - 2.0f * dt
    }
    if (key_down(KEY_RIGHT())) {
        cam_angle = cam_angle + 2.0f * dt
    }

    // Update camera
    cx = cos(cam_angle) * cam_distance
    cz = sin(cam_angle) * cam_distance
    engine_camera_position(cx, 3.0f, cz)
    engine_camera_target(0.0f, 0.0f, 0.0f)

    // Draw scene
    draw_grid(20.0f, 20, COLOR_GRAY())
    draw_cube(vec3(0.0f, 0.5f, 0.0f), 1.0f, COLOR_RED())

    flush_triangles()
    return
}

def main() -> Unit {
    handle = engine_create_window("My Game", 1024, 768)
    engine_set_clear_color(0.05f, 0.05f, 0.08f, 1.0f)
    engine_camera_perspective(deg_to_rad(60.0f), 1024.0f / 768.0f, 0.1f, 1000.0f)
    engine_set_update(update)
    engine_run(handle)
    engine_destroy(handle)
    return
}
```

---

## 33. Common Patterns & Idioms

### 33.1 Builder Pattern

```nex
def build_network(input_size: Int, hidden_size: Int, output_size: Int) -> Module {
    var m: Module = nn_sequential_new()
    nn_linear(m, input_size, hidden_size)
    nn_relu(m)
    nn_linear(m, hidden_size, output_size)
    return m
}
```

### 33.2 Callback Registration

```nex
def on_event(widget_id: Int64, event_kind: Int64) {
    // Handle event
    println("Event received!")
    return
}

ui_on_click(widget, on_event)
```

### 33.3 Resource Collection and Cleanup

```nex
class Model {
    layers: List

    def init() {
        self.layers = List()
    }

    def add_layer(layer: Module) {
        self.layers.add(layer)
    }

    def free() {
        i = 0
        while (i < self.layers.length()) {
            nn_free(self.layers.get(i))
            i = i + 1
        }
    }
}
```

### 33.4 Printing Mixed Types

```nex
// String concatenation auto-converts
println("Name: " + name)
println("Count: " + count)
println("Pi: " + 3.14)

// For standalone values, use print/println directly
print("value = ")
println(42)
```

### 33.5 Iteration with Index

```nex
i = 0
while (i < items.length()) {
    item = items.get(i)
    println("Item " + i + ": " + item)
    i = i + 1
}
```

### 33.6 for-in Loop

```nex
for (item in items) {
    println(item)
}
```

---

## 34. Limitations & Gotchas

### Things Nex v1 Does NOT Have

1. **No closures or lambdas** — use named function references only
2. **No `++` or `--` operators** — use `i = i + 1`
3. **No string interpolation** — use `+` concatenation
4. **No pattern matching / switch** — use if/else chains
5. **No async/await** — use threading for concurrency
6. **No macros or metaprogramming**
7. **No reflection** (only RTTI via `Var` dispatch)
8. **No enums** — use integer constants or classes
9. **No tuple types** — use structs
10. **No destructuring** — access fields individually
11. **No default parameter values** — all parameters required
12. **No named arguments** — positional only
13. **No generic constraints** — no `where T : Interface`
14. **No implicit type conversions** — explicit casts needed
15. **No ternary operator** — use if/else

### Important Notes

- **Semicolons are optional** but ASI rules apply — be aware of multi-line expressions
- **All classes inherit from Object** implicitly
- **Fields must be initialized** in the constructor or have default values
- **Return is explicit** — always include `return` at the end of functions
- **Parentheses required** around if/while/for conditions
- **Braces required** for all blocks (no single-statement bodies)
- **No unary minus on literals** — write `0.0f - value` or `0 - value` instead of `-value` in some contexts
- **Float literals need suffix**: `1.0f` for Float, `1.0` or `1.0d` for Double
- **Int64 for FFI**: Many FFI functions use `Int64` parameters

---

## 35. EBNF Grammar Reference

```ebnf
Program           = { Item } EOF ;

Item              = ImportDecl
                  | ClassDecl
                  | InterfaceDecl
                  | FunctionDecl
                  | Statement ;

ImportDecl        = "import" ModulePath [ "as" Ident ] Terminator
                  | "from" ModulePath "import" ImportList Terminator ;

ImportList        = Ident { "," Ident } ;
ModulePath        = Ident { "." Ident } ;

ClassDecl         = [ "public" ] [ "partial" ] "class" Ident [ "[" TypeParams "]" ]
                    [ ":" BaseSpecList ] "{" { ClassMember } "}" ;

BaseSpecList      = BaseSpec { "," BaseSpec } ;
BaseSpec          = [ "shared" ] TypeRef [ "(" [ ArgList ] ")" ] ;

InterfaceDecl     = [ "public" ] "interface" Ident [ "[" TypeParams "]" ]
                    "{" { FunctionSig Terminator } "}" ;

StructDecl        = [ "public" ] "struct" Ident [ "[" TypeParams "]" ]
                    [ ":" InterfaceList ] "{" { StructMember } "}" ;

ClassMember       = FieldDecl Terminator | FunctionDecl ;
StructMember      = FieldDecl Terminator | FunctionDecl ;

FieldDecl         = [ "public" ] Ident ":" TypeRef [ "=" Expr ] ;

FunctionDecl      = [ "public" ] [ "static" ] [ "virtual" ] [ "override" ]
                    "def" ( Ident | "operator" OperatorSymbol )
                    "(" [ Parameters ] ")" [ "->" TypeRef ] Block ;

Parameters        = Param { "," Param } ;
Param             = Ident ":" TypeRef ;
TypeParams        = Ident { "," Ident } ;

Block             = "{" { Statement } "}" ;

Statement         = Block
                  | IfStmt | WhileStmt | ForStmt
                  | TryStmt | UsingStmt
                  | ReturnStmt | BreakStmt | ContinueStmt | ThrowStmt
                  | VarStmt | ExprStmt ;

IfStmt            = "if" "(" Expr ")" Block [ "else" ( IfStmt | Block ) ] ;
WhileStmt         = "while" "(" Expr ")" Block ;
ForStmt           = "for" "(" ( VarInit ";" [ Expr ] ";" [ Expr ] | Ident "in" Expr ) ")" Block ;
TryStmt           = "try" Block { "catch" "(" Ident ":" TypeRef ")" Block } [ "finally" Block ] ;
UsingStmt         = "using" "(" Ident "=" Expr { "," Ident "=" Expr } ")" Block ;
ReturnStmt        = "return" [ Expr ] Terminator ;
ThrowStmt         = "throw" Expr Terminator ;
VarStmt           = "var" Ident [ ":" TypeRef ] "=" Expr Terminator ;
ExprStmt          = Expr Terminator ;

Expr              = Assignment ;
Assignment        = LogicalOr [ ( "=" | "+=" | "-=" | "*=" | "/=" ) Assignment ] ;
LogicalOr         = LogicalAnd { "||" LogicalAnd } ;
LogicalAnd        = Equality { "&&" Equality } ;
Equality          = Relational { ( "==" | "!=" ) Relational } ;
Relational        = Additive { ( "<" | ">" | "<=" | ">=" ) Additive } ;
Additive          = Multiplicative { ( "+" | "-" ) Multiplicative } ;
Multiplicative    = Unary { ( "*" | "/" | "%" ) Unary } ;
Unary             = ( "!" | "-" | "+" ) Unary | Postfix ;
Postfix           = Primary { "(" [ ArgList ] ")" | "." Ident | "::" Ident } ;
ArgList           = Expr { "," Expr } ;

Primary           = IntLiteral | FloatLiteral | StringLiteral | CharLiteral
                  | "true" | "false" | "null"
                  | Ident | "self" | "(" Expr ")" ;

TypeRef           = BuiltinType | Ident [ "[" TypeRef { "," TypeRef } "]" ] [ "?" ] ;

BuiltinType       = "Bool" | "Byte" | "Int" | "Int64" | "Float" | "Double"
                  | "Char" | "String" | "Unit" | "Var" ;

Terminator        = ";" | ASI_NEWLINE ;
```

---

## Appendix A: Quick Cheat Sheet

```
╔══════════════════════════════════════════════════════════════════╗
║                    NEX LANGUAGE CHEAT SHEET                      ║
╠══════════════════════════════════════════════════════════════════╣
║                                                                  ║
║  TYPES         Bool  Byte  Int  Int64  Float  Double  Char      ║
║                String  Unit  Var  T?  (T) -> R                   ║
║                                                                  ║
║  VARIABLES     x = 42                  // inferred Int           ║
║                x: Int = 42             // explicit               ║
║                var x = 42              // dynamic                ║
║                                                                  ║
║  FUNCTIONS     def name(p: T) -> R { return val }               ║
║                public def name() -> Unit { return }             ║
║                                                                  ║
║  CLASS         class Name : Base, Interface {                    ║
║                    field: Type                                   ║
║                    def init(p: T) -> Unit { self.field = p }    ║
║                    def method() -> R { return self.field }      ║
║                }                                                 ║
║                                                                  ║
║  STRUCT        struct Name { x: Float  y: Float }               ║
║                                                                  ║
║  INTERFACE     interface Name { def method(p: T) -> R }         ║
║                                                                  ║
║  GENERICS      class Box[T] { value: T }                        ║
║                                                                  ║
║  CONTROL       if (cond) { } else { }                           ║
║                while (cond) { }                                  ║
║                for (i = 0; i < n; i = i + 1) { }               ║
║                for (item in list) { }                            ║
║                                                                  ║
║  ERRORS        try { } catch (e: Error) { } finally { }        ║
║                throw Error("message")                            ║
║                                                                  ║
║  RESOURCES     using (r = open()) { }                           ║
║                                                                  ║
║  IMPORTS       import mod.path                                   ║
║                from mod.path import Name                         ║
║                                                                  ║
║  PRINT         print(val)   println(val)                        ║
║                "str" + val   // auto-converts                    ║
║                                                                  ║
║  OPERATORS     + - * / %  == != < <= > >=  && || !              ║
║                = += -= *= /=                                     ║
║                                                                  ║
║  NULL          x: String? = null                                ║
║                if (x != null) { /* x narrowed */ }              ║
║                                                                  ║
║  ENTRY POINT   def main() -> Unit { return }                    ║
║                                                                  ║
╚══════════════════════════════════════════════════════════════════╝
```

---

*Document generated for Nex v0.1.90. This is a complete reference for AI-assisted code generation in the Nex programming language.*
