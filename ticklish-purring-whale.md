# Nex for Neural Networks: Language Design & Implementation Plan

## Implementation Status

| Phase | Name | Status |
|-------|------|--------|
| 1 | Foundation (Torch Bindings) | **Complete** — see `phase1-changes.md` |
| 2 | Module System (`module` keyword) | **Complete** — see `phase2-changes.md` |
| 3 | Pipe Operator (`\|>`) | **Complete** — see `phase3-changes.md` |
| 4 | Training DSL | **Complete** — see `phase4-changes.md` |
| 5 | Typed Tensor Shapes (5a: Warnings) | **Complete** — see `phase5-changes.md` |
| 6 | Memory Management (Auto-Free) | **Complete** — see `phase6-changes.md` |

## Context

Nex is a compiled language (Rust compiler, Cranelift backend) with libtorch FFI bindings. A working GPT transformer exists in `nex/src/main.nex` (918 lines), but the implementation reveals deep pain points: fragile module storage via flat `List` with magic index offsets, manual memory management (`tensor_free`/`nn_free` everywhere), no batch dimension, per-head attention loops, one optimizer per module, FP32 only, and a broken `no_grad` implementation.

The goal is to evolve Nex into a language where transformer development is dramatically easier, faster, and cheaper — reducing the GPT implementation from ~918 lines to ~250 lines while catching shape errors at compile time.

The plan is structured in 6 phases, each producing a working compiler with measurable improvement. Phases 1-3 are the core deliverables; Phases 4-6 are follow-on work.

---

## Phase 1: Foundation (Torch Bindings Only — No Compiler Changes) [COMPLETE]

**Goal**: Fix broken functionality and add missing FFI functions needed for modern transformer training. All changes are in `nex_torch_native` + compiler dispatch tables.

### 1.1 Fix `torch_no_grad` Bug

The current implementation drops the `NoGradGuard` immediately (it's a no-op).

**File**: `F:/Development/Personal/Nex/crates/nex_torch_native/src/lib.rs` (~line 270)

```rust
use std::cell::RefCell;

thread_local! {
    static NO_GRAD_GUARD: RefCell<Option<tch::no_grad_guard::NoGradGuard>> = RefCell::new(None);
}

#[no_mangle]
pub unsafe extern "C" fn nex_torch_no_grad(flag: i32) {
    NO_GRAD_GUARD.with(|cell| {
        let mut guard = cell.borrow_mut();
        if flag != 0 {
            *guard = Some(tch::no_grad_guard());
        } else {
            *guard = None; // drop guard, re-enable gradients
        }
    });
}
```

### 1.2 Add Missing Builtin Method Dispatch Entries

Several tensor methods exist in the FFI DLL but are missing from the IR dispatch table, breaking method chaining (e.g. `x.cos().item()` fails because the return type isn't tracked).

**File**: `F:/Development/Personal/Nex/crates/nexc_ir/src/lib.rs` — `seed_builtin_dispatch()`

Add entries for: `sin`, `cos`, `clamp`, `where_self`, `sum_dim`, `mean_dim`, `max_dim`, `min_dim`, `ones_like`, `zeros_like`, `full_like`, `pow_scalar`, `contiguous`, `flatten`, `index_select`, `unsqueeze`, `squeeze`, `ndim`, `numel`, `shape_dim`, `requires_grad`, `detach`, `grad`, `to_float`.

### 1.3 New FFI Functions

All added to `nex_torch_native/src/lib.rs`, registered in `torch_function_name()` in codegen, and added to `seed_builtin_dispatch()` in IR:

| Function | Signature | Purpose |
|----------|-----------|---------|
| `optim_adamw` | `(module, lr, beta1, beta2, wd) -> Optimizer` | AdamW with weight decay |
| `nn_set_training` | `(module, flag)` | Train/eval mode toggle |
| `tensor_view3` | `(t, d0, d1, d2) -> Tensor` | 3D reshape |
| `tensor_view4` | `(t, d0, d1, d2, d3) -> Tensor` | 4D reshape |
| `tensor_sdpa` | `(q, k, v, is_causal) -> Tensor` | Scaled dot-product attention |
| `tensor_to_bf16` | `(t) -> Tensor` | Cast to bfloat16 |
| `tensor_to_half` | `(t) -> Tensor` | Cast to float16 |
| `tensor_stack` | `(tensor_list, dim) -> Tensor` | Stack list of tensors into batch |
| `nn_scale_gradients` | `(module, scale)` | Scale gradients for accumulation |
| `optim_step_and_zero` | `(opt)` | Combined step + zero_grad |
| `tensor_outer` | `(a, b) -> Tensor` | Outer product (for rotary embeddings) |
| `tensor_chunk` | `(t, chunks, dim) -> List` | Split tensor into chunks |

**Files changed**:
- `F:/Development/Personal/Nex/crates/nex_torch_native/src/lib.rs` — new FFI functions
- `F:/Development/Personal/Nex/crates/nexc_ir/src/lib.rs` — dispatch table entries
- `F:/Development/Personal/Nex/crates/nexc_codegen_cranelift/src/lib.rs` — `torch_function_name()` + `NATIVE_SYMBOL_NAMES` + signature declarations

### 1.4 Verification

- Rebuild `nex_torch_native.dll`
- Run existing `main.nex` demo mode — should still work
- Test new functions: create a small Nex script that uses `optim_adamw`, `tensor_sdpa`, `tensor_view3`, `nn_set_training`, and verify they work

---

## Phase 2: Module System (`module` Keyword) [COMPLETE]

**Goal**: Replace fragile `List`-based module storage with named, typed sub-module fields. Auto-generate boilerplate methods (`to_device`, `all_modules`, `save_checkpoint`, `load_checkpoint`, `free`).

### 2.1 Syntax

```nex
module MHA(n_embd: Int, n_head: Int) {
    q_proj: LinearNoBias(n_embd, n_embd)
    k_proj: LinearNoBias(n_embd, n_embd)
    v_proj: LinearNoBias(n_embd, n_embd)
    o_proj: LinearNoBias(n_embd, n_embd)

    def forward(x: Tensor, mask: Tensor) -> Tensor {
        var q = self.q_proj(x)
        var k = self.k_proj(x)
        var v = self.v_proj(x)
        var attn = sdpa(q, k, v, causal=true)
        return self.o_proj(attn)
    }
}

module GPT(config: GPTConfig) {
    tok_emb: Embedding(config.vocab_size, config.n_embd)
    blocks: TransformerBlock[config.n_layer](config.n_embd, config.n_head)
    ln_f: RMSNorm(config.n_embd)
    lm_head: Linear(config.n_embd, config.vocab_size)

    def forward(x: Tensor, pos: Tensor, mask: Tensor) -> Tensor {
        var h = self.tok_emb(x) + self.pos_emb(pos)
        for (block in self.blocks) {
            h = block(h, mask)
        }
        return self.lm_head(self.ln_f(h))
    }
}
```

### 2.2 Design Decisions

- `module` is a new top-level keyword, sibling to `class` and `struct`
- Fields use layer constructor syntax: `name: LayerType(args)`
- `self.field(args)` is sugar for `self.field.forward(args)`
- `blocks: Block[N](args)` declares an array of N identical sub-modules (stored as `List`)
- Each leaf field is backed by a `NexModule` (VarStore + Sequential)
- User-defined `module` fields are backed by a module struct (class instance)
- Auto-generated methods iterate the module hierarchy

### 2.3 Built-in Layer Registry

The IR lowerer maintains a mapping from layer type names to FFI initialization sequences:

| Layer Type | Init Sequence |
|------------|--------------|
| `Linear(in, out)` | `nn_sequential_new()` + `nn_linear(m, in, out)` |
| `LinearNoBias(in, out)` | `nn_sequential_new()` + `nn_linear_no_bias(m, in, out)` |
| `Embedding(num, dim)` | `nn_sequential_new()` + `nn_embedding(m, num, dim)` |
| `RMSNorm(dim)` | `nn_sequential_new()` + `nn_rms_norm(m, dim)` |
| `LayerNorm(dim)` | `nn_sequential_new()` + `nn_layer_norm(m, dim)` |
| `Dropout(p)` | `nn_sequential_new()` + `nn_dropout(m, p)` |
| `ReLU` | `nn_sequential_new()` + `nn_relu(m)` |
| `GELU` | `nn_sequential_new()` + `nn_gelu(m)` |
| Other names | Treated as user-defined module — calls `OtherModule::init(args)` |

### 2.4 Auto-Generated Methods

For every `module`, the compiler auto-generates:

**`to_device(device: String)`** — calls `m.to_device(device)` on every leaf `Module` field and recurses into sub-module fields.

**`all_modules() -> List`** — returns a flat list of all leaf `Module` pointers (for creating optimizers).

**`save_checkpoint(dir: String)`** — calls `model_save(field, dir + "/field_name.pt")` for each leaf, recurses for sub-modules with path nesting.

**`load_checkpoint(dir: String)`** — inverse of save.

**`free()`** — calls `nn_free` on every leaf, recurses for sub-modules.

**`init_weights(std: Float)`** — calls `nn_init_normal(field, std)` on every leaf.

### 2.5 Compiler Changes

| Crate | File | Change |
|-------|------|--------|
| `nexc_lex` | `lib.rs` | Add `Module` token kind, add `"module"` to keyword matching |
| `nexc_ast` | `lib.rs` | Add `ModuleFieldDecl`, `ModuleDecl`, add `Module(ModuleDecl)` to `Item` enum |
| `nexc_parse` | `lib.rs` | Add `TokenKind::Module` to top-level dispatch, implement `parse_module()` |
| `nexc_type` | `lib.rs` | Register module names as `Type::Named(name)`, register methods |
| `nexc_layout` | `lib.rs` | Compute module struct layout (fields at 8-byte offsets, like classes) |
| `nexc_ir` | `lib.rs` | Module init lowering, `self.field(x)` → `forward` desugaring, auto-generated method emission, layer registry |
| `nexc_codegen` | `lib.rs` | Module types use class-like codegen (field access, method calls) |

### 2.6 How It Lowers

**Module instantiation** `var model = GPT(config)`:
```
// IR pseudo-code:
%model = gc_alloc(GPT_layout)

// Init tok_emb field:
%tok_emb = call nn_sequential_new()
call nn_embedding(%tok_emb, config.vocab_size, config.n_embd)
store %model.tok_emb = %tok_emb

// Init blocks field (array):
%blocks = call list_new()
for i in 0..config.n_layer:
    %block = call TransformerBlock::init(config.n_embd, config.n_head)
    call list_add(%blocks, %block)
store %model.blocks = %blocks

// ... repeat for ln_f, lm_head
```

**Method call** `self.q_proj(x)`:
```
// IR: detect q_proj is a Module-typed field → desugar to forward
%proj = load %self.q_proj
%result = call nn_forward(%proj, x)
```

**Sub-module call** `block(h, mask)`:
```
// IR: detect block is a user-defined module → desugar to forward method
%result = call TransformerBlock::forward(%block, h, mask)
```

### 2.7 Verification

- Port `main.nex` GPT class to `module` syntax
- Verify demo mode produces same results
- Verify checkpoint save/load still works
- Measure line count reduction (expect ~918 → ~500)

---

## Phase 3: Tensor Operations & Pipe Operator [COMPLETE]

**Goal**: Add `|>` pipe operator for fluent tensor chains. Improve multi-dim tensor ops.

### 3.1 Pipe Operator `|>`

**Syntax**: `expr |> func` desugars to `func(expr)`.

```nex
// Before (deeply nested):
var h = self.lm_head(self.ln_f(self.emb_norm(self.tok_emb(x) + self.pos_emb(pos))))

// After (linear pipeline):
var h = self.tok_emb(x) + self.pos_emb(pos)
    |> self.emb_norm
    |> process_blocks
    |> self.ln_f
    |> self.lm_head
```

**Desugaring rules**:
- `expr |> func` → `func(expr)` (free function)
- `expr |> self.field` → `self.field.forward(expr)` (module field, via Phase 2 sugar)
- `expr |> func(extra_args)` → `func(expr, extra_args)` (partial application style)
- `expr |> |x| body` → `(|x| body)(expr)` (inline lambda)

### 3.2 Compiler Changes

| Crate | File | Change |
|-------|------|--------|
| `nexc_lex` | `lib.rs` | Add `PipeForward` token for `\|>`. In the `'\|'` scanner arm, check if next char is `>` → emit `PipeForward` instead of `Pipe`. Add `PipeForward` to `is_continuation_operator()` (prevents ASI after `\|>`) |
| `nexc_ast` | `lib.rs` | Add `Pipe { lhs, rhs, span }` to `Expr` enum |
| `nexc_parse` | `lib.rs` | Add `PipeForward` as infix operator in Pratt parser. Binding power: left=3, right=4 (above ternary, below `\|\|`). RHS is parsed as a primary expression (identifier, member access, call, or lambda) |
| `nexc_ir` | `lib.rs` | Lower `Expr::Pipe` to `IrInstruction::Call` based on RHS type (free function, module field, or lambda) |

### 3.3 ASI Consideration

`|>` must be a continuation operator so that:
```nex
x
    |> self.ln_f
    |> self.lm_head
```
does not insert semicolons. The lexer already handles multi-character continuation tokens (`&&`, `||`, etc.) — `|>` follows the same pattern.

### 3.4 Verification

- Write test expressions using `|>` with free functions, module fields, and lambdas
- Verify ASI does not break multi-line pipes
- Port `main.nex` forward methods to use `|>` where it improves readability

---

## Phase 4: Training DSL [COMPLETE]

**Goal**: Training library in pure Nex + optimizer annotations. No new syntax (library approach, not DSL).

### 4.1 LR Schedule Library (`lib/schedule.nex`)

Pure Nex functions using `std.math` (already available):

```nex
import std.math

def lr_cosine(step: Int, warmup: Int, total: Int, max_lr: Float, min_lr: Float) -> Float {
    if (step < warmup) { return max_lr * (step * 1.0) / (warmup * 1.0) }
    var progress = (step - warmup) * 1.0 / ((total - warmup) * 1.0)
    if (progress > 1.0) { progress = 1.0 }
    var decay = (1.0 + cos(progress * 3.14159265358979)) / 2.0
    return min_lr + (max_lr - min_lr) * decay
}

def lr_trapezoidal(step: Int, total: Int, warmup_ratio: Float,
                   warmdown_ratio: Float, max_lr: Float, final_frac: Float) -> Float {
    var warmup = (warmup_ratio * total * 1.0)  // cast
    var warmdown = (warmdown_ratio * total * 1.0)
    if (step < warmup) { return max_lr * (step + 1) * 1.0 / warmup }
    if (step <= total - warmdown) { return max_lr }
    var progress = (total - step) * 1.0 / warmdown
    return max_lr * (progress + (1.0 - progress) * final_frac)
}
```

### 4.2 Training Loop Library (`lib/train.nex`)

```nex
def train_loop(model: GPT, optims: List, data_iter: List,
               total_steps: Int, warmup: Int, max_lr: Float, min_lr: Float,
               clip_norm: Float, log_every: Int) {
    var step = 0
    for (batch in data_iter) {
        var lr = lr_cosine(step, warmup, total_steps, max_lr, min_lr)
        set_lr_all(optims, lr)

        var logits = model.forward(batch.input, batch.pos, batch.mask)
        var loss = logits.cross_entropy(batch.target)

        loss.backward()
        model.clip_gradients(clip_norm)
        for (opt in optims) {
            optim_step(opt)
            optim_zero_grad(opt)
        }

        if (step % log_every == 0) {
            print("step "); print(step); print(" loss="); println(loss.item())
        }
        step = step + 1
    }
}
```

### 4.3 Optimizer Annotations (requires IR changes)

Using existing `[Attribute]` syntax on module fields:

```nex
module GPT(config: GPTConfig) {
    [Optim("adam", "lr=0.3")]
    tok_emb: Embedding(config.vocab_size, config.n_embd)

    [Optim("adamw", "lr=0.02", "wd=0.2")]
    blocks: TransformerBlock[config.n_layer](config)

    [Optim("adam", "lr=0.004")]
    lm_head: Linear(config.n_embd, config.vocab_size)
}
```

The IR lowerer reads `[Optim(...)]` attributes on module fields and auto-generates a `setup_optimizer() -> List` method that creates one optimizer per config group.

**Compiler changes**: `nexc_ir/src/lib.rs` — when generating auto methods for `module` types, check for `Optim` attributes on fields and emit the appropriate `optim_adam`/`optim_adamw` calls.

### 4.4 Verification

- Implement schedule functions, test against known curves
- Port `main.nex` training loop to use the library
- Test `[Optim]` annotation generates correct optimizer setup

---

## Phase 5: Type-Level Tensor Shapes (Long-Term)

**Goal**: Named tensor dimensions with compile-time shape checking. Gradual adoption — warnings first, errors later.

### 5.1 Dimension Declarations

```nex
dim Batch                    // abstract (size determined at runtime)
dim Seq                      // abstract
dim Embd = 768               // concrete (known at compile time)
dim Heads = 12
dim HeadDim = 64
```

### 5.2 Typed Tensors

```nex
var x: Tensor[Batch, Seq, Embd]
var q: Tensor[Batch, Seq, Heads, HeadDim]

// Compiler checks:
var scores = q.matmul(k.transpose(-1, -2))  // OK: [B,S,H,D] @ [B,S,D,H] -> [B,S,H,H]... wait, that's wrong
// The compiler would catch this: k needs transpose on the last two dims
```

### 5.3 Type System Extension

```rust
// In nexc_type/src/lib.rs:
pub enum DimExpr {
    Named(String),      // abstract dimension: Batch, Seq
    Literal(i64),       // concrete: 768
    Inferred,           // _ (let compiler figure it out)
    Dynamic,            // ? (skip checking)
}

pub struct TensorShape {
    pub dims: Vec<DimExpr>,
}

// Extend Type enum:
pub enum Type {
    // ... existing ...
    Tensor(Option<TensorShape>),  // None = untyped (backward compat)
}
```

### 5.4 Shape Checking Rules

- `matmul`: last dim of LHS must equal second-to-last dim of RHS
- `view`/`reshape`: product of dims must be preserved (when all known)
- `cat(dim=d)`: all dims except `d` must match
- `narrow(dim, start, len)`: output dim at `dim` becomes `len`
- `transpose(d0, d1)`: swaps dimensions `d0` and `d1`
- `softmax(dim)`: preserves shape
- `linear.forward`: last dim transforms from `in_features` to `out_features`

### 5.5 Gradual Adoption

**Phase 5a**: Shape annotations produce **warnings** only. Existing code with `var x: Tensor` (no shape) continues to work with zero checking. This lets users opt in incrementally.

**Phase 5b**: `[StrictShapes]` attribute on a module enables compile errors for shape mismatches.

### 5.6 Compiler Changes

| Crate | Change |
|-------|--------|
| `nexc_lex` | Add `Dim` keyword token |
| `nexc_ast` | Add `DimDecl` item, extend `TypeExpr` for `Tensor[dims]` |
| `nexc_parse` | Parse `dim` declarations, parse `Tensor[A, B, C]` type syntax |
| `nexc_type` | Add `TensorShape`, dimension unification engine, shape checking pass |
| New crate: `nexc_dims` | Dimension inference and arithmetic |

### 5.7 Risk

This is the highest-risk phase — effectively building a dependent type system for dimensions. Start with Phase 5a (warnings only) to validate the design before committing to full enforcement.

---

## Phase 6: Memory Management

**Goal**: Eliminate manual `tensor_free()` / `nn_free()` calls via scope-based automatic cleanup.

### 6.1 Type-Aware `using` (Quick Win)

Extend existing `using` blocks to emit type-specific cleanup:

```nex
using (mask = make_causal_mask(seq_len, device)) {
    // mask is automatically freed at block exit via tensor_free()
}
```

**Change in `nexc_ir/src/lib.rs`**: When lowering `Stmt::Using`, check the expression's resolved type. If `"Tensor"` → emit `tensor_free()`. If `"Module"` → emit `nn_free()`. If `"Optimizer"` → emit `optim_free()`. Otherwise → emit generic `dispose()`.

### 6.2 Scope-Based Auto-Free for Tensors

All tensor-typed locals are automatically freed at scope exit, except those that are returned or stored into fields.

**IR lowering changes**:
- Maintain `tensor_locals: Vec<Vec<String>>` — a stack of scopes, each tracking tensor-typed variable names
- On `var x: Tensor = ...` → push `x` to current scope's tracker
- On `return x` → mark `x` as "escaped" (don't free)
- On scope exit → emit `tensor_free` for all non-escaped locals

**Edge cases**:
- Re-assignment `x = new_tensor` → free old value before storing new
- Field assignment `self.field = x` → mark `x` as escaped
- Method chaining `a.matmul(b).softmax(1)` → intermediate `matmul` result is a compiler temporary → freed at statement end

### 6.3 Module Auto-Free

The auto-generated `free()` method from Phase 2 handles module cleanup. Phase 6 adds automatic invocation at scope exit for module-typed locals.

### 6.4 Verification

- Run existing `main.nex` with auto-free enabled — should produce identical results
- Use CUDA memory tracking to verify no leaks
- Verify tensors that escape (via `return`) are not prematurely freed

---

## Implementation Sequence & Dependencies

```
Phase 1 (Foundation)          [DONE]
    │
    ├── Phase 2 (Module System) [DONE]
    │       │
    │       └── Phase 4 (Training DSL) [DONE]
    │
    ├── Phase 3 (Pipe Operator) [DONE]
    │
    ├── Phase 6 (Memory Mgmt)   ← requires Phase 2 for module auto-free
    │
    └── Phase 5 (Typed Shapes)  ← independent but hardest, long-term
```

Phases 1, 3, and 5 are independent of each other. Phase 2 is the prerequisite for Phases 4 and 6.

## Critical Files

| File | Phases | Role |
|------|--------|------|
| `F:/Development/Personal/Nex/crates/nex_torch_native/src/lib.rs` | 1 | All FFI additions |
| `F:/Development/Personal/Nex/crates/nexc_lex/src/lib.rs` | 2, 3, 5 | New tokens: `Module`, `PipeForward`, `Dim` |
| `F:/Development/Personal/Nex/crates/nexc_ast/src/lib.rs` | 2, 3, 5 | New AST nodes: `ModuleDecl`, `Pipe`, `DimDecl` |
| `F:/Development/Personal/Nex/crates/nexc_parse/src/lib.rs` | 2, 3, 5 | New parse functions: `parse_module()`, pipe infix, dim types |
| `F:/Development/Personal/Nex/crates/nexc_type/src/lib.rs` | 2, 5 | Module type registration, tensor shape types |
| `F:/Development/Personal/Nex/crates/nexc_ir/src/lib.rs` | 1-6 | Central hub: dispatch tables, module lowering, pipe desugaring, auto-free, optimizer annotations |
| `F:/Development/Personal/Nex/crates/nexc_codegen_cranelift/src/lib.rs` | 1, 2 | FFI symbol registration, module struct codegen |
| `c:/Users/ronni/.../nc/nex/src/main.nex` | all | Port to new syntax at each phase |

## End State: What the GPT Looks Like After All Phases

```nex
import torch.tensor
import torch.nn

struct GPTConfig {
    vocab_size: Int
    n_embd: Int
    n_head: Int
    n_layer: Int
    seq_len: Int
    head_dim: Int
    mlp_hidden: Int
}

module MHA(n_embd: Int, n_head: Int, head_dim: Int) {
    q_proj: LinearNoBias(n_embd, n_embd)
    k_proj: LinearNoBias(n_embd, n_embd)
    v_proj: LinearNoBias(n_embd, n_embd)
    o_proj: LinearNoBias(n_embd, n_embd)

    def forward(x: Tensor, mask: Tensor) -> Tensor {
        var q = self.q_proj(x).view3(-1, n_head, head_dim)
        var k = self.k_proj(x).view3(-1, n_head, head_dim)
        var v = self.v_proj(x).view3(-1, n_head, head_dim)
        return sdpa(q, k, v, 1).view3(-1, 1, n_embd) |> self.o_proj
    }
}

module TransformerBlock(n_embd: Int, n_head: Int, head_dim: Int, mlp_hidden: Int) {
    ln1: RMSNorm(n_embd)
    attn: MHA(n_embd, n_head, head_dim)
    ln2: RMSNorm(n_embd)
    ffn_up: LinearNoBias(n_embd, mlp_hidden)
    ffn_down: LinearNoBias(mlp_hidden, n_embd)

    def forward(x: Tensor, mask: Tensor) -> Tensor {
        x = x + self.attn(self.ln1(x), mask)
        var h = self.ln2(x) |> self.ffn_up
        h = h * h  // ReLU² (relu already in ffn_up sequential)
        return x + self.ffn_down(h)
    }
}

module GPT(config: GPTConfig) {
    [Optim("adam", "lr=0.3")]
    tok_emb: Embedding(config.vocab_size, config.n_embd)
    pos_emb: Embedding(config.seq_len, config.n_embd)
    emb_norm: RMSNorm(config.n_embd)

    [Optim("adam", "lr=0.02")]
    blocks: TransformerBlock[config.n_layer](
        config.n_embd, config.n_head, config.head_dim, config.mlp_hidden)

    ln_f: RMSNorm(config.n_embd)

    [Optim("adam", "lr=0.004")]
    lm_head: Linear(config.n_embd, config.vocab_size)

    def forward(x: Tensor, pos: Tensor, mask: Tensor) -> Tensor {
        self.tok_emb(x) + self.pos_emb(pos)
            |> self.emb_norm
            |> self.forward_blocks(mask)
            |> self.ln_f
            |> self.lm_head
    }

    def forward_blocks(h: Tensor, mask: Tensor) -> Tensor {
        for (block in self.blocks) {
            h = block(h, mask)
        }
        return h
    }
}

def main() {
    var config = GPTConfig(32768, 768, 6, 12, 1024, 128, 3072)
    var device = "cpu"
    if (cuda_is_available() == 1) { device = "cuda" }

    var model = GPT(config)
    model.init_weights(0.02)
    model.to_device(device)

    var optims = model.setup_optimizer()
    var mask = make_causal_mask(config.seq_len, device)
    var pos = make_positions(config.seq_len, device)

    // Training loop
    for (epoch in 0..100) {
        var lr = lr_cosine(epoch, 10, 100, 0.0003, 0.00003)
        set_lr_all(optims, lr)

        var logits = model.forward(input_ids, pos, mask)
        var loss = logits.cross_entropy(target)
        loss.backward()
        model.clip_gradients(1.0)
        for (opt in optims) { optim_step(opt); optim_zero_grad(opt) }
    }

    model.save_checkpoint("ckpt/")
}
// No manual free() calls — handled automatically
```

~150 lines of model + training vs the current 918 lines.
