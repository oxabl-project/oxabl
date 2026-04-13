---
title: "feat: Preprocessor Include File Arguments"
type: feat
status: completed
date: 2026-04-13
---

# feat: Preprocessor Include File Arguments

## Overview

The `oxabl_preprocessor` crate currently expands include files but ignores arguments
passed at the call site. ABL include files support two argument styles — positional and
named — and both are heavily used in the corpus. Adding argument support will resolve a
significant portion of the preprocessing→parse failures (currently 83.6% pass rate with
preprocessing, vs 98.9% without).

**Corpus scale:**
- 2,222 source files pass named arguments (`{file.i &name=value}`)
- 358 source files pass positional arguments (`{file.i "arg1" arg2}`)
- 23 include files consume `{1}` or `{2}` positional references
- 1 include file references `{0}` (the include name itself)

---

## Problem Statement

When the preprocessor expands `{file.i "SHARED"}`, it resolves `file.i` and processes
its contents — but the `"SHARED"` argument is discarded. Inside the include file, `{1}`
references expand to nothing, producing broken ABL:

```
/* Call site: */
{ gl/global-pdf.i "SHARED"}

/* Inside global-pdf.i: */
DEFINE {1} VARIABLE h_PDFinc AS HANDLE NO-UNDO.
/*     ^^^ expands to empty, producing: */
DEFINE  VARIABLE h_PDFinc AS HANDLE NO-UNDO.
/*     ^ wrong — should be "DEFINE SHARED VARIABLE ..." */
```

Named arguments have the same issue:

```
/* Call site: */
{api/promo/ps_inv_service.i &dataset="InventoryLevels"}

/* Inside include: */
&IF "{&dataset}" EQ "InventoryLevels" &THEN
/*    ^^^^^^^^^^ expands to empty, condition is always false */
```

The named argument case is especially impactful because it feeds into `&IF` condition
evaluation — without the arg, the wrong branch is selected, producing incorrect expanded
source.

---

## Proposed Solution

### ABL Include Argument Semantics

ABL include arguments follow these rules:

**Positional arguments** — space-delimited tokens after the include name:
```
{file.i arg1 "arg 2" arg3}
```
- `{0}` = the include file name (`file.i`)
- `{1}` = `arg1`
- `{2}` = `arg 2` (quotes stripped)
- `{3}` = `arg3`

**Named arguments** — `&name=value` pairs:
```
{file.i &table=customer &field="cust-num"}
```
- Inside the include, `{&table}` resolves to `customer`, `{&field}` to `cust-num`
- Named arguments are equivalent to scoped `&SCOPED-DEFINE` variables that exist
  only for the duration of the include expansion
- Quotes around values are stripped

**Mixed arguments** — positional and named can coexist:
```
{file.i "SHARED" &extra=yes}
```

### Implementation

#### 1. Argument Parsing

Add `parse_include_args()` to extract arguments from the include reference content
(everything after the include name inside `{...}`).

```
crates/oxabl_preprocessor/src/preprocessor.rs
```

**New types:**

```rust
struct IncludeArgs {
    /// Positional arguments: {0} is the include name, {1}+ are user args
    positional: Vec<String>,
    /// Named arguments: &name=value pairs
    named: Vec<(String, String)>,
}
```

**Parsing rules:**
1. First token = include name (already parsed by `parse_include_name`)
2. Remaining tokens, space-delimited:
   - If starts with `&` → named arg: split on first `=`, strip quotes from value
   - Otherwise → positional arg: strip surrounding quotes if present
3. The include name itself becomes positional arg `{0}`

#### 2. Argument Scoping in `expand_include`

Modify `expand_include()` to accept parsed arguments and inject them into the
variable table before processing the included file:

```rust
fn expand_include(
    &mut self,
    include_name: &str,
    site: FileSpan,
    inner: &str,       // ← currently `_inner`, now used
    depth: usize,
) -> Vec<SpanNode> {
    let args = parse_include_args(inner, include_name);

    // Save current var state
    let saved_vars = self.vars.clone();

    // Inject named args as scoped defines
    for (name, value) in &args.named {
        self.vars.define(name, value);
    }

    // ... resolve, read, process ...

    // Restore var state (named args don't leak to parent)
    self.vars = saved_vars;
}
```

#### 3. Positional Argument Resolution

Positional args (`{0}`, `{1}`, etc.) are syntactically identical to include argument
references that the lexer already tokenizes as `Kind::IncludeArgReference`. But in the
preprocessor, these need to resolve at text-scan time (before lexing).

The preprocessor already handles `{` followed by a digit — it currently skips these.
Change this to resolve them against the current include's positional args:

- Thread a `positional_args: &[String]` through `process_source()`
- When `{N}` is encountered during scanning, look up `positional_args[N]`
- If defined, emit the value as a `SpanNode::Include` (same as `{&variable}` expansion)
- If not defined, emit nothing (current behavior)

#### 4. Scope Isolation

Arguments must not leak between include levels:

- **Named args** are scoped to the include call — save/restore the var table
- **Positional args** are scoped to the include call — pass them as a parameter to
  `process_source()`, not as mutable state
- **`&GLOBAL-DEFINE` inside the include** still propagates upward (existing behavior)
- **`&SCOPED-DEFINE` inside the include** does NOT propagate (this is a pre-existing
  TODO — currently scoped defines leak; this plan does NOT fix that, but argument
  scoping specifically must be correct)

---

## Acceptance Criteria

- [x] Positional args: `{file.i "SHARED"}` makes `{1}` expand to `SHARED` inside the include
- [x] Positional arg `{0}` expands to the include file name
- [x] Named args: `{file.i &name=value}` makes `{&name}` expand to `value` inside the include
- [x] Quoted values: quotes are stripped (`&x="hello"` → `{&x}` = `hello`)
- [x] Mixed args: positional and named coexist correctly
- [x] Scope isolation: named args do not leak to the parent after include returns
- [x] Scope isolation: positional args do not leak to the parent
- [x] Nested includes: args are scoped per-level (inner include's args don't shadow outer)
- [x] All existing preprocessor tests pass unchanged
- [x] Full corpus `--preprocess` pass rate improves (target: measurable improvement from 83.6%)

---

## Dependencies & Prerequisites

**Internal:**
- `oxabl_preprocessor` crate (Phase 3, implemented)
- `oxabl check --preprocess` CLI integration (implemented)

**Files to modify:**
- `crates/oxabl_preprocessor/src/preprocessor.rs` — argument parsing, `expand_include`, `process_source` signature
- No new files needed; no changes to other crates

---

## Sources & References

### Internal References

- Current `expand_include`: `crates/oxabl_preprocessor/src/preprocessor.rs:562`
- Current `parse_include_name`: `crates/oxabl_preprocessor/src/preprocessor.rs:799`
- Lexer `IncludeArgReference` handling: `crates/oxabl_lexer/src/lib.rs:699`
- Corpus example (positional): `{gl/global-pdf.i "SHARED"}` → `DEFINE {1} VARIABLE ...`
- Corpus example (named): `{api/promo/ps_inv_service.i &dataset="InventoryLevels"}`
