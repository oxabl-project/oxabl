---
title: "feat: Within-file SHARED / NEW SHARED symbol flags on VariableDeclaration, DefineTempTable, DefineBuffer"
type: feat
status: draft
date: 2026-07-16
origin: GitHub #58
branch: feat/semantic-within-file-shared
---

# feat: Within-file SHARED / NEW SHARED symbol flags

## Problem Statement

The parser already recognizes the `SHARED` / `NEW [GLOBAL] SHARED` prefix on
`DEFINE` statements and captures it in local variables
(`crates/oxabl_parser/src/parser/statements.rs:829-849`), but for
`VariableDeclaration`, `DefineTempTable`, and `DefineBuffer` it **drops those
captured booleans on the floor** — the constructor calls at statements.rs
`1096-1102`, `1647-1656`, and `1724-1730` never pass them into the AST node.
Only `DefineDataset` (statement.rs:448-461) stores them today, and its field
comment records the debt explicitly:

```rust
is_new_shared: bool, // TODO: Retrofit on DefineTempTable, DefineBuffer, VariableDeclaration
is_shared: bool,     // TODO: Retrofit on DefineTempTable, DefineBuffer, VariableDeclaration
```

The consequence is that the semantic model is **unfaithful**: a variable
declared `DEFINE NEW SHARED VARIABLE gTotal AS DECIMAL.` produces a `Symbol`
with `SymbolFlags::empty()`, indistinguishable from a plain local. The
information the parser worked to extract is discarded before the declare pass
can see it.

### Honest framing (read this before estimating value)

**This feature does not primarily reduce undefined-symbol false positives.**
Within a single file, a consumer's `DEFINE SHARED VARIABLE y.` *already declares
`y` locally* in the current scope — `undefined-symbol` (LINT0001) resolves it
just fine today, flags or no flags. Do not sell this as a lint-accuracy win.

Its actual value is **SHARED correctness — faithful symbol metadata**:

1. It is the enabling substrate for **future cross-file resolution** (roadmap
   feature #5, deferred): a producer's `NEW SHARED` symbol in one file must be
   matchable to a consumer's `SHARED` symbol in another, which is impossible if
   the producer/consumer distinction is not recorded.
2. It unlocks **future SHARED-specific lints** (NEW-SHARED-never-consumed,
   SHARED-without-producer) — all of which are cross-file and out of scope here.
3. It makes the semantic model **faithful to ABL semantics** and pays down the
   documented AST TODO.

This is plumbing and foundation work. It is correct to do, small, and low-risk.
It is not a headline feature.

---

## Goals

- Retrofit `is_shared` / `is_new_shared` (and, per the Design decision below,
  `is_new_global_shared`) onto `StatementKind::VariableDeclaration`,
  `DefineTempTable`, and `DefineBuffer`.
- Thread the parser-captured booleans through the existing constructor sites.
- Apply `SymbolFlags::SHARED` / `NEW_SHARED` / `NEW_GLOBAL_SHARED` in the declare
  pass using the existing `flag_if` pattern already proven on `DefineDataset`
  (resolve.rs:200-218).
- Update `docs/design/ast-invariants.md` in the same PR (mandated by CLAUDE.md
  for any reshape of a public `oxabl_ast` type).
- Keep behavior for non-shared defines byte-for-byte unchanged.

## Non-Goals (explicit)

- **Cross-file SHARED resolution.** Matching a `NEW SHARED` producer in one file
  to a `SHARED` consumer in another is deferred to roadmap feature #5.
- **Wiring `rebinding_scopes`** (symbol.rs:120-125). See the Design argument for
  why this stays empty in v1.
- **NEW-SHARED-never-consumed / SHARED-without-producer lints.** All cross-file,
  all deferred.
- **Scope-kind validation** (e.g. rejecting a loose `DEFINE SHARED` at file
  root). Deferred.
- No runtime SHARED visibility modeling; static analysis only.

---

## Design

### AST reshape

Each of the three variants gains the shared-mode flags. To match the shape the
codebase already established on `DefineDataset`, the two core booleans keep the
same names and ordering:

```rust
// VariableDeclaration (statement.rs:26-33) — after:
VariableDeclaration {
    name: Identifier,
    type_source: TypeSource,
    initial_value: Option<Expression>,
    no_undo: bool,
    extent: Option<u32>,
    is_new_shared: bool,
    is_shared: bool,
    is_new_global_shared: bool,   // see "GLOBAL handling" below
},
```

The same three fields append to `DefineTempTable` (statement.rs:185-202, after
`xml_options`) and `DefineBuffer` (statement.rs:208-219, after `xml_options`).
Appending at the end minimizes churn in `..`-based match arms and keeps the
field order legible.

Delete the two `// TODO: Retrofit …` comments on `DefineDataset`
(statement.rs:452-453) — the debt is paid.

### GLOBAL handling — a deliberate improvement over DefineDataset

The parser today (statements.rs:834-836) **consumes** the `GLOBAL` keyword in
`NEW GLOBAL SHARED` but records nothing distinct: both `NEW SHARED` and
`NEW GLOBAL SHARED` collapse to `is_new_shared = true`. `DefineDataset` inherits
that collapse — it cannot tell the two apart.

ABL treats them differently: `NEW SHARED` is visible to callees invoked from
this file; `NEW GLOBAL SHARED` is visible session-wide. `SymbolFlags` already
reserves a distinct bit for this (`NEW_GLOBAL_SHARED = 1 << 11`,
symbol.rs:63-85). To set that bit faithfully — and to satisfy the required test
that `NEW GLOBAL SHARED` sets `NEW_GLOBAL_SHARED` — the parser must record the
`GLOBAL` bit rather than discard it.

**Decision:** capture a third boolean `is_new_global_shared`. When the parser
sees `Kind::Global` at statements.rs:834, set `is_new_global_shared = true`
instead of silently advancing. This is a ~3-line change inside the existing
capture block. The three booleans are mutually exclusive by construction (the
capture grammar can only produce one true at a time — see Risks).

**This forces a change to `DefineDataset` too — do it in this PR, not later.**
Making the booleans mutually exclusive silently *regresses* `DefineDataset`
unless we retrofit it in lockstep. Today the capture block collapses both
`NEW SHARED` and `NEW GLOBAL SHARED` into `is_new_shared = true`, which
`parse_define_dataset` receives (statements.rs:911-918) and the declare arm
turns into `NEW_SHARED` (resolve.rs:199-218). Once `GLOBAL` sets
`is_new_global_shared` *instead* of `is_new_shared`, a
`DEFINE NEW GLOBAL SHARED DATASET` arrives with `is_new_shared = false`, no
third field to carry the `GLOBAL` bit, and the declare pass sets **no flag at
all** — strictly worse than today, and no existing test covers it (a silent
regression). Therefore Open Question #1 is resolved to **retrofit
`DefineDataset` now**: add `is_new_global_shared` to `DefineDataset`
(statement.rs:448-461), pass it through `parse_define_dataset` (signature at
statements.rs:1734-1739; call site 911-918), and extend its declare arm
(resolve.rs:199-218) with `flag_if(is_new_global_shared,
SymbolFlags::NEW_GLOBAL_SHARED)`. This makes all four variants consistent and
faithful in the same PR.

### Parser threading

No new parsing logic — the capture block at statements.rs:829-849 already runs
before the `DEFINE` dispatch. The changes:

1. Extend the capture block to set `is_new_global_shared` when `GLOBAL` is seen
   (statements.rs:834-836). The `is_shared` capture guard (statements.rs:844),
   today `!is_new_shared`, must become `!is_new_shared && !is_new_global_shared`
   so a `NEW GLOBAL SHARED` form never also sets the consumer flag.

2. Pass `is_new_shared`, `is_shared`, `is_new_global_shared` into **every**
   `VariableDeclaration` constructor in `parse_define_variable` — there are
   **three**, not one:
   - statements.rs:1096-1102 — the normal terminal constructor.
   - statements.rs:1082-1088 — the `Kind::ViewAs | Kind::Size` **early-return**
     in the same function. Missing this is a real behavior bug:
     `DEFINE SHARED VAR x AS INT VIEW-AS …` would silently drop its flags.
   - statements.rs:1149-1155 — `parse_var_statement` (the `VAR` short form). `VAR`
     has no `SHARED` syntax, so these are legitimately `false`, but the literal
     still must gain the three fields or it fails to compile.

3. `parse_define_temp_table()` and `parse_define_buffer()` take **no shared
   arguments today** (call sites statements.rs:932 and :937). Both must **gain
   three parameters** — `is_new_shared: bool, is_shared: bool,
   is_new_global_shared: bool` — mirroring `parse_define_dataset`'s signature
   (statements.rs:1734-1739), and the dispatch must pass the captured booleans at
   the call sites. Then thread them into the constructors at statements.rs:1647-1656
   (temp-table) and :1724-1730 (buffer), exactly as `DefineDataset` does at
   statements.rs:911-918.

4. `parse_define_dataset` already receives `is_new_shared` / `is_shared`; add the
   new `is_new_global_shared` parameter to its signature (statements.rs:1734-1739)
   and pass it at the call site (911-918) — see the GLOBAL-handling decision
   above for why this is mandatory, not optional.

### Declare-pass flag application

Mirror the `DefineDataset` handler (resolve.rs:200-218) which builds flags with
`flag_if` (resolve.rs:1864):

```rust
let flags = flag_if(is_shared, SymbolFlags::SHARED)
    | flag_if(is_new_shared, SymbolFlags::NEW_SHARED)
    | flag_if(is_new_global_shared, SymbolFlags::NEW_GLOBAL_SHARED);
```

- **VariableDeclaration** (declare arm resolve.rs:114-120 → `declare_variable`,
  resolve.rs:472-513): add a `flags: SymbolFlags` parameter to
  `declare_variable` and pass it into the `self.declare(...)` call that today
  hard-codes `SymbolFlags::empty()` (resolve.rs:511). Compute `flags` in the arm
  from the destructured booleans.
- **DefineTempTable** (declare arm resolve.rs:192-194 → `declare_temp_table`,
  resolve.rs:574+): same — add a `flags` parameter; the `tt_sym` `declare` call
  currently passes `SymbolFlags::empty()`. Field symbols remain unflagged.
- **DefineBuffer** (declare arm resolve.rs:197-198): today calls the flag-less
  helper `declare_simple` (resolve.rs:614-623). Rather than widen
  `declare_simple`'s signature (it has other callers — Stream, Frame), inline a
  direct `self.declare(stmt, scope, name, NamespaceId::Buffers,
  SymbolKind::Buffer, None, flags)` call in the arm, matching the `DefineEvent`
  style already in the file. `declare_simple` stays flag-less for its other
  callers.
- **DefineDataset** (declare arm resolve.rs:199-218): already builds `flags`
  with `flag_if(is_shared, …) | flag_if(is_new_shared, …)`. Add
  `| flag_if(is_new_global_shared, SymbolFlags::NEW_GLOBAL_SHARED)` and
  destructure the new field. This keeps the four variants consistent and closes
  the regression identified in the GLOBAL-handling decision.

### ABL SHARED semantics (reference for reviewers)

| Syntax | Role | Flag set |
|---|---|---|
| `DEFINE SHARED …` | consumer — binds an existing shared var from a caller | `SHARED` |
| `DEFINE NEW SHARED …` | producer — visible to callees in this file | `NEW_SHARED` |
| `DEFINE NEW GLOBAL SHARED …` | producer — visible session-wide | `NEW_GLOBAL_SHARED` |

Within a single file these still each declare the symbol locally; the flags are
metadata, not resolution behavior.

### Should we wire `rebinding_scopes` now? — No, defer. (Argument)

`rebinding_scopes` (symbol.rs:120-125) exists to track *additional* scopes where
a single logical shared symbol is re-introduced via `NEW SHARED` — mirroring
Ruff's `global`/`nonlocal` rebinding map. Its only consumer is cross-file /
inter-procedural analysis: within one file, every declaration already gets its
own `Symbol` in its own scope in a single declare pass, so there is nothing to
rebind and nothing to read the map. Populating it now would be speculative
plumbing with zero v1 reader — a YAGNI violation, and it would invite
inconsistency when the real cross-file design lands. **Leave it empty; document
in ast-invariants.md that within-file v1 sets flags only.**

### Struct / enum size impact

Adding three `bool`s to three variants adds at most 3 bytes to each affected
variant. `StatementKind`'s size is governed by its *largest* variant
(`Class`, `Method`, `DefineDataset`, etc. — all far larger than these three).
It is very likely the total `size_of::<StatementKind>()` is unchanged because
the added bytes fit in existing padding / are dominated by the largest variant.
This is verified, not assumed — see Benchmark.

### ast-invariants.md update (required content)

Add a new section (after §11 "Ancillary invariants", before "Debug assertions"):

> ## 12. SHARED / NEW SHARED declaration flags
>
> `VariableDeclaration`, `DefineTempTable`, `DefineBuffer`, and `DefineDataset`
> each carry `is_shared`, `is_new_shared`, and `is_new_global_shared` booleans
> capturing the `[NEW [GLOBAL]] SHARED` prefix of the originating `DEFINE`. All
> four variants carry the identical triple — there is no longer a two-flag
> odd-one-out.
>
> - **At most one is true.** The parser's capture grammar
>   (`statements.rs:829-849`) produces `SHARED` (consumer), `NEW SHARED`, or
>   `NEW GLOBAL SHARED` (producers) as mutually exclusive alternatives. A tree
>   with two of these `true` on one node is a parser bug.
> - **Non-shared defines set all three `false`** — the common case; behavior is
>   identical to before these flags existed.
> - **Semantic contract:** the declare pass maps these to
>   `SymbolFlags::SHARED` / `NEW_SHARED` / `NEW_GLOBAL_SHARED` respectively via
>   `flag_if`. The flags are metadata; within a single file they do not change
>   symbol resolution — a consumer's `SHARED` declaration still declares the
>   symbol locally in its own scope.
> - **`rebinding_scopes` is not populated by within-file analysis.** It is
>   reserved for cross-file work (roadmap feature #5) that re-links a `SHARED`
>   consumer to a `NEW SHARED` producer across files. Within-file v1 sets flags
>   only.
> - `DefineDataset` previously stored only `is_shared` / `is_new_shared` and
>   collapsed `NEW GLOBAL SHARED` into `NEW_SHARED`. This change adds its third
>   flag in lockstep so the `GLOBAL` distinction is observable and no dataset
>   form regresses — all four `DEFINE` variants now behave identically.

---

## Implementation Steps (ordered)

1. **Update `docs/design/ast-invariants.md`** — add §12 as specified above.
   Doing this first satisfies the CLAUDE.md same-PR mandate and forces the
   design to be pinned before code.

2. **AST variant fields** (`crates/oxabl_ast/src/statement.rs`):
   - Add `is_new_shared`, `is_shared`, `is_new_global_shared` to
     `VariableDeclaration` (26-33), `DefineTempTable` (185-202), `DefineBuffer`
     (208-219).
   - Add `is_new_global_shared` to `DefineDataset` (448-461) — required in this
     PR (see GLOBAL-handling decision). Remove its two stale TODO comments
     (452-453).

3. **Parser** (`crates/oxabl_parser/src/parser/statements.rs`):
   - Extend capture block to set `is_new_global_shared` on `GLOBAL` (834-836);
     change the `is_shared` guard at 844 to
     `!is_new_shared && !is_new_global_shared`.
   - `VariableDeclaration`: pass the three booleans at **all three** constructor
     sites — 1096-1102, 1082-1088 (ViewAs/Size early-return), 1149-1155
     (`parse_var_statement`, values `false`).
   - Give `parse_define_temp_table()` and `parse_define_buffer()` three new
     `bool` parameters (mirroring `parse_define_dataset`, sig 1734-1739); pass
     the captured booleans at their call sites (932, 937); thread into the
     constructors (1647-1656, 1724-1730).
   - Add `is_new_global_shared` to `parse_define_dataset`'s signature (1734-1739)
     and its call site (911-918).

4. **Declare pass** (`crates/oxabl_semantic/src/resolve.rs`):
   - `declare_variable` (472-513): add `flags` param; use it at the `declare`
     call (511). Update the arm (114-120) to destructure the booleans and build
     `flags`.
   - `declare_temp_table` (574+): add `flags` param; use at the `tt_sym`
     `declare`. Update arm (192-194).
   - Buffer arm (197-198): inline a direct `self.declare(...)` with `flags`
     instead of `declare_simple`. Leave `declare_simple` (614-623) unchanged.
   - DefineDataset arm (199-218): add
     `| flag_if(is_new_global_shared, SymbolFlags::NEW_GLOBAL_SHARED)`.

5. **Fix exhaustive-match / construction ripple sites** (compile-breaks until
   updated). Production match arms all use `..` and are **unaffected**; only
   struct-literal *construction* sites break:
   - `crates/oxabl_analyze/src/lib.rs:492` — test helper `var_decl` (in
     `#[cfg(test)]`); add the three fields (`false`).
   - `crates/oxabl_lint/src/rules/unused_variable.rs:151` — test constructor.
   - `crates/oxabl_lint/src/rules/type_mismatch_assignment.rs:305,315` — test
     constructors. (Match arm at 52 uses `..` — fine.)
   - `crates/oxabl_lint/src/rules/unknown_table_or_field.rs:404` (DefineBuffer),
     `447` (VariableDeclaration) — test constructors. (Arms 51,57 use `..`.)
   - `crates/oxabl_lint/src/rules/undefined_symbol.rs:505` — test constructor.
     (Arms 47,52,312 use `..`/field patterns — fine.)
   - `crates/oxabl_semantic/src/check.rs:805,1081,1127,1274` — test
     constructors. (Arms 72,78,87 use `..` — fine.)
   - `crates/oxabl_semantic/src/resolve.rs` — test constructors (these break the
     most): VariableDeclaration at 1945, 1955, 2018, 2865, 2875; DefineBuffer at
     2104, 3193, 3289, 3496, 3633, 3661, 4005; DefineTempTable at 2117; plus the
     existing `DefineDataset` shared-flag test literal at 2500-2520 (gains
     `is_new_global_shared`).
   - `crates/oxabl_parser/src/parser/tests.rs` — any explicit-literal
     construction of these variants (dossier notes lines ~2332-2558,
     ~2645/4627/4671/4699, ~4716-4781), plus the existing DefineDataset
     assertion at ~7819-7835 (gains `is_new_global_shared`). Most parser
     assertions match against `..` patterns and are unaffected; only hand-built
     literals break.

   Fastest path: `cargo check --workspace` after step 4; the compiler
   enumerates every remaining site.

   **No `oxabl_analyze` dump-version bump is needed.** `symbol_flags_list`
   (`crates/oxabl_analyze/src/lib.rs:379-417`) already emits `shared`,
   `new_shared`, and `new_global_shared` entries whenever those bits are set, so
   the JSON/text dump gains the new flag data for free once the declare pass sets
   them — no schema change, no section-version increment.

---

## Testing

### Existing tests that WILL break (and how to fix)

Every site listed in Implementation Step 5 that **constructs** one of these
variants as a struct literal fails to compile until the three fields are added.
Fix mechanically: add `is_new_shared: false, is_shared: false,
is_new_global_shared: false` to each literal (all these fixtures describe
non-shared defines, so `false` preserves their meaning and asserted outcomes).
Tests that only *match* these variants with `..` need no change.

### New tests

**Parser** (`crates/oxabl_parser/src/parser/tests.rs`):
- `fn parse_shared_variable_sets_is_shared` — `DEFINE SHARED VARIABLE x AS
  INTEGER.` parses to `VariableDeclaration { is_shared: true, is_new_shared:
  false, is_new_global_shared: false, .. }`.
- `fn parse_new_shared_variable_sets_is_new_shared` — `DEFINE NEW SHARED
  VARIABLE y AS CHARACTER.` → `is_new_shared: true`, others false.
- `fn parse_new_global_shared_variable_sets_global` — `DEFINE NEW GLOBAL SHARED
  VARIABLE z AS DECIMAL.` → `is_new_global_shared: true`, others false.
- `fn parse_shared_temp_table_and_buffer` — `DEFINE SHARED TEMP-TABLE tt …` and
  `DEFINE NEW SHARED BUFFER b FOR customer.` capture the flags.
- `fn parse_plain_variable_has_no_shared_flags` — regression guard: a plain
  `DEFINE VARIABLE n AS INTEGER.` has all three `false`.

**Semantic** (`crates/oxabl_semantic/src/resolve.rs`, alongside the existing
`dataset_declared_with_shared_flag` template at ~2500-2520):
- `fn shared_variable_has_shared_flag` — analyze a `VariableDeclaration { is_shared:
  true, .. }`; assert the symbol's flags contain `SymbolFlags::SHARED`.
- `fn new_shared_variable_has_new_shared_flag` — `is_new_shared: true` →
  `NEW_SHARED`.
- `fn new_global_shared_variable_has_new_global_shared_flag` —
  `is_new_global_shared: true` → `NEW_GLOBAL_SHARED`.
- `fn shared_temp_table_has_shared_flag` — DefineTempTable with `is_shared:
  true` → `SHARED` on the temp-table symbol (fields unflagged).
- `fn shared_buffer_has_shared_flag` — DefineBuffer with `is_new_shared: true` →
  `NEW_SHARED` on the buffer symbol.
- `fn plain_variable_has_empty_flags` — regression: no shared flags → symbol
  flags do not contain SHARED/NEW_SHARED/NEW_GLOBAL_SHARED (behavior unchanged).
- `fn new_global_shared_dataset_has_new_global_shared_flag` — regression guard
  for the DefineDataset retrofit: a `DefineDataset { is_new_global_shared: true,
  .. }` symbol has `NEW_GLOBAL_SHARED` (and *not* the old collapsed
  `NEW_SHARED`). Add alongside the existing `dataset_declared_with_shared_flag`.

Also add a parser test `fn parse_new_global_shared_dataset_sets_global` in
tests.rs confirming `DEFINE NEW GLOBAL SHARED DATASET ds …` sets
`is_new_global_shared: true`, `is_new_shared: false`.

---

## Benchmark

**Honest assessment: no dedicated benchmark is warranted.** This is struct-field
plumbing threaded through an existing declare pass — no new hot path, no new
algorithm. The existing `crates/oxabl_parser/benches/parser_bench.rs` and
`crates/oxabl_semantic/benches/semantic_bench.rs` already exercise these three
variants and will catch any regression via CodSpeed CI.

The one real concern is **AST size regression**. Guard it with a compile-time
assertion rather than a benchmark. After measuring the actual value with
`cargo test` / a scratch `dbg!(std::mem::size_of::<StatementKind>())`, add to
`crates/oxabl_ast/src/statement.rs`:

```rust
const _: () = assert!(std::mem::size_of::<StatementKind>() <= N);
```

with `N` set to the measured post-change size (expected: unchanged from today,
since the largest variant dominates). This makes any future accidental
size blow-up a compile error, at zero runtime cost.

---

## Risks & Edge Cases

- **Mutual exclusivity of the flags.** The capture grammar
  (statements.rs:829-849) is `if NEW [GLOBAL] SHARED { … } else if SHARED { … }`
  — structurally it can set at most one mode. No runtime enforcement needed;
  assert it in ast-invariants.md §12 and rely on the grammar. A `debug_assert!`
  in `Parser::stmt` is possible but optional (the invariant is trivial to
  re-derive from the capture code).
- **GLOBAL handling.** The parser previously discarded `GLOBAL`; the new capture
  must set `is_new_global_shared` *and* still consume the token. Verify the
  advance logic at 834-836 stays correct (consume `GLOBAL`, then `SHARED`).
  Note the `SHARED` after `NEW GLOBAL` is optional in the grammar (837-839), so
  a bare `DEFINE NEW GLOBAL VARIABLE` sets `is_new_global_shared = true` with no
  `SHARED` token. This is harmless — it maps to `NEW_GLOBAL_SHARED`, which is the
  correct semantic — and matches today's behavior of treating the `GLOBAL` form
  as a shared producer.
- **DefineDataset regression (now closed).** Because the flags became mutually
  exclusive, `DefineDataset` had to be retrofitted with `is_new_global_shared`
  in the same PR; otherwise `DEFINE NEW GLOBAL SHARED DATASET` would have lost
  its flag entirely. This is handled in Steps 2-4 above; the parser test at
  tests.rs:7819-7835 and the semantic test at resolve.rs:2500-2520 guard it.
- **No behavior change for non-shared defines.** All three flags default
  `false`; the declare pass produces `SymbolFlags::empty()` exactly as before.
  The `plain_variable_has_empty_flags` test locks this in.
- **Test-fixture churn.** Many construction sites across parser/semantic/lint/
  analyze must gain three fields. Purely mechanical, but touches ~a dozen files;
  lean on `cargo check --workspace` to enumerate them rather than hunting by
  hand.
- **DefineDataset inconsistency.** If we do not retrofit the third flag onto
  DefineDataset, it remains the odd one out (collapses GLOBAL). Acceptable for a
  focused PR; tracked in Open Questions.

---

## Rollout

- Branch: `feat/semantic-within-file-shared` off `master`.
- Single PR (small, mechanical, well-tested). CI must be green:
  `cargo check`, `cargo test`, `cargo fmt --check`, `cargo clippy -D warnings`.
- PR description cites the ast-invariants.md §12 addition (CLAUDE.md mandate).
- Conventional-commit subject: `feat: capture SHARED/NEW SHARED flags on
  variable, temp-table, and buffer defines`.

---

## Open Questions

1. **Retrofit `is_new_global_shared` onto `DefineDataset`? — RESOLVED: yes, in
   this PR.** Deferring it would silently regress `NEW GLOBAL SHARED DATASET`
   under the new mutually-exclusive capture (the `GLOBAL` bit would land nowhere
   and the declare pass would set no flag). It is ~5 lines plus two test-literal
   updates (resolve.rs:2500-2520, tests.rs:7819-7835). Done in Steps 2-4.
2. **Third bool vs. a `SharedMode` enum.** Three mutually-exclusive booleans
   match the established `DefineDataset` shape and the `flag_if` idiom, at the
   cost of an "at most one true" invariant. An enum
   (`None | Shared | NewShared | NewGlobalShared`) would encode exclusivity in
   the type. Chosen: booleans, for consistency with existing code. Revisit if a
   fourth mode ever appears.
3. **Should `declare_simple` gain a `flags` parameter** instead of inlining the
   buffer `declare` call? Inlining keeps the shared-flag logic local to the one
   arm that needs it and avoids perturbing Stream/Frame callers; if a second
   flag-bearing simple declaration appears, revisit.
