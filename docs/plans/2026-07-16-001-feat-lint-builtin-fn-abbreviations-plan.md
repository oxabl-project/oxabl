---
title: "feat: Built-in Function Abbreviation Support in the Lint/Semantic Registry"
type: feat
status: draft
date: 2026-07-16
origin: GitHub #58
branch: feat/lint-builtin-fn-abbreviations
---

# feat: Built-in Function Abbreviation Support in the Lint/Semantic Registry

## Problem Statement

ABL lets you abbreviate reserved keywords down to a documented minimum prefix.
Sixteen built-in functions are also reserved keywords with such an abbreviation
— for example `AVAILABLE` may be written `AVAIL`, `DBRESTRICTIONS` as `DBREST`,
`TERMINAL` as `TERM`. Real ABL source uses these short forms freely.

The built-in function registry (`BUILTIN_FUNCTIONS` in
`crates/oxabl_lexer/src/builtins.rs`) currently contains only the **full**
function names. The semantic resolver folds a call's name to a lowercase atom
and asks `is_builtin_function()` whether it is a known built-in
(`crates/oxabl_semantic/src/resolve.rs:1703`). Because the registry omits
abbreviations, an abbreviated call resolves as `NotInScope` and the linter
fires a false-positive `undefined-symbol` (LINT0001).

**Concrete false positive:**

```abl
IF AVAIL(customer) THEN MESSAGE "found".
```

The parser stores the raw source slice `"AVAIL"` as the call name
(`crates/oxabl_parser/src/parser/expressions.rs:962`; the AVAILABLE/AMBIGUOUS
call special-case is at `parser/expressions.rs:955`). Resolve folds it to
`avail`; `is_builtin_function("avail")` returns `false` (only `"available"` is
registered); LINT0001 fires on legal code.

## Goals / Non-Goals

### Goals

- Register every valid abbreviation prefix (from each function's documented
  `min_abbreviation` up to the full name) for the 16 built-in functions that
  are also reserved keywords with an abbreviation.
- Keep the fix entirely in codegen — `builtins.rs` carries a `DO NOT EDIT`
  header, so the change lives in `crates/oxabl_codegen/src/main.rs` and is
  materialized by regeneration.
- Preserve the registry's invariants: sorted, deduped, ASCII-lowercased, no
  whitespace, `binary_search`-compatible.
- No change to the hot path: lookup stays a zero-allocation binary search over
  a static slice.

### Non-Goals

- **Non-reserved built-in functions get nothing.** Functions like `LENGTH`,
  `SUBSTRING`, `ENTRY`, `ROUND` are not reserved keywords and ABL does **not**
  permit abbreviating them. `LENGT` is not a legal alias for `LENGTH` and must
  keep firing LINT0001. Only reserved-keyword functions with a documented
  `min_abbreviation` are in scope.
- **No expansion beyond the 16 functions** enumerated below. This is a targeted
  join between the JSON function list and the reserved-keyword abbreviation
  column; it does not invent abbreviations.
- **No punctuation normalization** (e.g. space ↔ hyphen forms of
  `PROC-HANDLE`). The registry is keyed by folded atoms, which normalize case
  only. Hyphen handling stays in the lexer's keyword matcher.
- **No user-defined abbreviation mechanism** (pragmas, config). ABL abbreviation
  is a fixed property of reserved keywords.

### The 16 in-scope functions

| Function | min_abbreviation | Token Kind |
|----------|------------------|------------|
| AVAILABLE | AVAIL | `Kind::Available` |
| AMBIGUOUS | AMBIG | `Kind::Ambiguous` |
| DBRESTRICTIONS | DBREST | `Kind::Dbrestrictions` |
| DBVERSION | DBVERS | `Kind::Dbversion` |
| GATEWAYS | GATEWAY | `Kind::Gateways` |
| IS-ATTR-SPACE | IS-ATTR | `Kind::IsAttrSpace` |
| IS-LEAD-BYTE | IS-ATTR | `Kind::IsLeadByte` |
| KEYFUNCTION | KEYFUNC | `Kind::Keyfunction` |
| LINE-COUNTER | LINE-COUNT | `Kind::LineCounter` |
| NUM-ALIASES | NUM-ALI | `Kind::NumAliases` |
| PAGE-NUMBER | PAGE-NUM | `Kind::PageNumber` |
| PROC-HANDLE | PROC-HA | `Kind::ProcHandle` |
| PROC-STATUS | PROC-ST | `Kind::ProcStatus` |
| PROVERSION | PROVERS | `Kind::Proversion` |
| SETUSERID | SETUSER | `Kind::Setuserid` |
| TERMINAL | TERM | `Kind::Terminal` |

---

## Design

### Chosen approach: expand abbreviations at codegen time

`is_builtin_function` is a `binary_search` over a static `&[&str]`
(`crates/oxabl_lexer/src/builtins.rs:242-244`). The cleanest fix adds the
abbreviation prefixes as ordinary entries in `BUILTIN_FUNCTIONS`: no runtime
logic changes, the hot path stays allocation-free, and the resolver
(`resolve.rs:1703`) needs no edit at all.

The prefix-expansion logic already exists for keyword matching in
`generate_keyword_match` (`crates/oxabl_codegen/src/main.rs:787-798`):

```rust
let min_len = if let Some(ref abbrev) = kw.min_abbreviation {
    abbrev.len()
} else {
    lower.len() // no abbreviation ⇒ exact match only
};
for len in min_len..=lower.len() {
    let prefix = &lower[..len];
    add_match(prefix.to_string(), &variant);
}
```

We mirror this inside `generate_builtins_rs`.

### Plumbing change

Today `generate_builtins_rs` receives only the flat JSON name list and cannot
see abbreviations:

- `generate_builtins_rs(function_names: &[String])`
  — `crates/oxabl_codegen/src/main.rs:940`
- Called from `main()` at `:1108` and `:1124`, both times with
  `parse_builtin_functions(&json_path)` (JSON only).

The enriched `Keyword` structs — which carry `min_abbreviation`
(`main.rs:152`) — are already built in `main()` and in scope at both call
sites (used for `generate_kind_rs`/`generate_build_rs` at `:1113`/`:1116`).

**Selecting the abbreviable set — do NOT filter on `keyword_type`.** The
`keyword_type` field is populated from `parse_json`'s collapsed
one-type-per-name map (`main.rs:345-362`, last matching title wins). Under that
collapse, `AVAILABLE` and `AMBIGUOUS` resolve to `Attribute` and `TERMINAL` to
`Statement` — so a `KeywordType::Function` filter would silently drop 3 of the
16, including the headline `AVAILABLE`, and `AVAIL(cust)` would keep firing.

Instead, join on two facts that are both reliable:

1. `kw.min_abbreviation.is_some()` — the keyword has a documented abbreviation.
2. The keyword's **uppercased full name is a member of the built-in function
   set** already computed by `parse_builtin_functions(&json_path)` — which
   returns uppercased names (`main.rs:387`) and is available at both call
   sites (`:1107`, `:1123`).

**Change:** extend the signature to take the keyword list and (for a cheap
lookup) a set of the built-in names:

```rust
pub fn generate_builtins_rs(function_names: &[String], keywords: &[Keyword]) -> String
```

Build a `HashSet<String>` of the uppercased `function_names` inside the
function, then expand:

```rust
let builtin_upper: HashSet<&str> =
    function_names.iter().map(|n| n.as_str()).collect(); // already UPPERCASE

for kw in keywords {
    let Some(ref abbrev) = kw.min_abbreviation else { continue };
    if !builtin_upper.contains(kw.name.to_uppercase().as_str()) {
        continue; // not a built-in function; skip (e.g. pure statements)
    }
    let lower = kw.name.to_ascii_lowercase();
    let min_len = abbrev.len();
    for len in min_len..lower.len() {
        // full name already contributed by the JSON list; emit only the
        // shorter prefixes here (min_len..len, exclusive of full length)
        names.push(lower[..len].to_string());
    }
}
```

The prefixes join the existing `names` vector (which already contains every
full built-in name from `function_names`), then sorted + deduped (the existing
`names.sort(); names.dedup();` at `main.rs:942-943` handles ordering and
duplicate removal). The entry-count in the doc comment (`main.rs:957`) is
computed from `names.len()`, so it updates automatically. No "defensive
full-name push" is needed — the full name comes from the JSON set by
construction.

Prefix slicing is byte-safe: all 16 names are ASCII (letters and `-`), so
`&lower[..len]` never splits a UTF-8 boundary. `min_abbreviation.len()` is the
byte length, which equals the char length for these ASCII abbreviations.

### IS-ATTR-SPACE / IS-LEAD-BYTE — no actual overlap

`IS-ATTR-SPACE` and `IS-LEAD-BYTE` both carry `min_abbreviation = IS-ATTR`, but
prefix expansion slices each keyword's **own** name by `abbrev.len()` (7), so
they produce **distinct** shortest forms: `IS-ATTR-SPACE` yields `is-attr`,
`is-attr-` ... while `IS-LEAD-BYTE` yields `is-lead`, `is-lead-` ... There is no
shared entry to dedup. This matches the generated keyword matcher, where
`kind.rs:993-994` maps `"is-attr" → IsAttrSpace` and `"is-lead" → IsLeadByte` as
separate arms. Both `is-attr` and `is-lead` must therefore be registered and
tested; there is no "deliberate overlap" to document.

### Alternatives considered / rejected

- **Hand-maintained abbreviation list in `builtins.rs`.** Rejected: the file
  carries a `DO NOT EDIT` header and is regenerated; a hand-edited list would
  be silently clobbered on the next `cargo run -p oxabl_codegen` and would
  drift from the reserved-keyword source of truth.
- **Runtime abbreviation folding in the resolver.** Rejected: it would push
  prefix-matching logic onto the hot resolve path (per unresolved identifier),
  violating the "no work on the hot path" principle, and would duplicate the
  abbreviation knowledge the codegen already owns. Binary search over a static
  slice stays O(log n) and allocation-free.
- **A separate `BUILTIN_ABBREVIATIONS` slice checked as a fallback.** Rejected:
  a second binary search adds branchy lookup code for no benefit; merging into
  the one sorted slice is simpler and faster.

---

## Implementation Steps

1. **Extend `generate_builtins_rs` signature** to accept the keyword list.
   `crates/oxabl_codegen/src/main.rs:940` —
   `pub fn generate_builtins_rs(function_names: &[String], keywords: &[Keyword]) -> String`.

2. **Expand abbreviation prefixes** inside `generate_builtins_rs`, after the
   existing `function_names` lowercase/collect at `main.rs:941` and before the
   `sort`/`dedup` at `main.rs:942-943`. Build a `HashSet` of the uppercased
   `function_names`, then for each keyword with `min_abbreviation.is_some()`
   whose uppercased name is in that set, push every prefix from
   `abbrev.len()..name.len()` (the full name is already present from the JSON
   list). This mirrors `generate_keyword_match` (`main.rs:787-798`) but keys off
   the JSON built-in set, **not** `keyword_type` (which collapses AVAILABLE,
   AMBIGUOUS, and TERMINAL to non-Function types — see Design).

3. **Update both call sites in `main()`** to pass `&keywords`:
   - `crates/oxabl_codegen/src/main.rs:1108` (the `"builtins"` arm)
   - `crates/oxabl_codegen/src/main.rs:1124` (the `"all" | ""` arm)

4. **Regenerate** the registry:
   ```bash
   cargo run -p oxabl_codegen -- builtins
   ```
   Confirm `crates/oxabl_lexer/src/builtins.rs` now contains `avail`, `ambig`,
   `dbrest`, ... and that the entry-count in the module doc comment
   (`builtins.rs:10`) has grown, while the `DO NOT EDIT` header
   (`builtins.rs:1`) is preserved.

5. **No resolver change.** `crates/oxabl_semantic/src/resolve.rs:1703` already
   calls `is_builtin_function(&atom)`; the new entries are picked up
   automatically. Verify by inspection only.

6. **Add/adjust tests** (see Testing) and run
   `cargo test -p oxabl_lexer -p oxabl_lint -p oxabl_codegen`.

---

## Testing

### Existing tests that must stay green

- `builtin_functions_registry` — `crates/oxabl_lexer/src/lib.rs:839`
  (full names resolve; `frobnicate`/`define` do not).
- `builtin_functions_slice_is_sorted` — `crates/oxabl_lexer/src/lib.rs:859`
  (binary-search precondition).
- `builtin_function_calls_do_not_fire` —
  `crates/oxabl_lint/src/rules/undefined_symbol.rs:647`.
- `builtin_function_matching_is_case_insensitive` —
  `crates/oxabl_lint/src/rules/undefined_symbol.rs:672`.
- `non_builtin_function_still_fires` —
  `crates/oxabl_lint/src/rules/undefined_symbol.rs:685`.

### New tests

**Lexer unit — `crates/oxabl_lexer/src/lib.rs` (tests module):**

- `fn builtin_abbreviations_are_registered()` — assert
  `is_builtin_function` is `true` for each min-abbreviation and full name of all
  16 functions: `avail`, `available`, `ambig`, `ambiguous`, `dbrest`,
  `dbrestrictions`, `dbvers`, `dbversion`, `gateway`, `gateways`, `is-attr`,
  `is-attr-space`, `is-lead`, `is-lead-byte`, `keyfunc`, `keyfunction`,
  `line-count`, `line-counter`, `num-ali`, `num-aliases`, `page-num`,
  `page-number`, `proc-ha`, `proc-handle`, `proc-st`, `proc-status`, `provers`,
  `proversion`, `setuser`, `setuserid`, `term`, `terminal`. Note both `is-attr`
  (from IS-ATTR-SPACE) and `is-lead` (from IS-LEAD-BYTE) — they are distinct,
  not a shared prefix. Assert `false` for below-minimum fragments one char short
  of their `min_abbrev`: `avai` (AVAIL is 5), `ambi` (AMBIG is 5), `is-att`
  (IS-ATTR is 7), `ter` (TERM is 4).
- Registry invariants are already covered by the existing
  `builtin_functions_slice_is_sorted` (`lib.rs:859`), which asserts strict
  sorting (hence dedup) over the whole slice — that test continues to guard the
  enlarged registry unchanged. The **only** new invariant worth adding is
  ASCII-lowercase + no-whitespace on every entry; fold a single loop asserting
  `s == s.to_ascii_lowercase() && !s.chars().any(char::is_whitespace)` into the
  existing test (or add a small dedicated `builtin_entries_are_ascii_lowercase`
  test) rather than duplicating the sort/dedup checks.

**Lint integration — `crates/oxabl_lint/src/rules/undefined_symbol.rs` (tests):**

- `fn builtin_abbreviation_calls_do_not_fire()` — for each abbreviation
  (`avail`, `ambig`, `dbrest`, `dbvers`, `gateway`, `is-attr`, `is-lead`,
  `keyfunc`, `line-count`, `num-ali`, `page-num`, `proc-ha`, `proc-st`,
  `provers`, `setuser`, `term`) build a `FunctionCall` with `name: id(abbrev)`
  and assert
  `analyze_and_lint(...)` produces no diagnostics. Mirror the harness in
  `builtin_function_calls_do_not_fire` (`:647`). Include a mixed-case case
  (`AVAIL`, `Avail`) to prove folding still applies.
- `fn non_abbreviable_builtin_truncation_still_fires()` (negative test) —
  `LENGT` (truncation of the non-reserved `LENGTH`) must fire exactly one
  LINT0001, proving we did not over-register non-reserved functions. Mirror
  `non_builtin_function_still_fires` (`:685`).

---

## Benchmark

**A dedicated micro-benchmark is not warranted, and adding one would be
misleading.** `is_builtin_function` is a `binary_search` over a static slice
(`builtins.rs:242-244`). Summing `(name_len − abbrev_len)` across the 16
functions adds ~61 shorter-prefix entries (DBRESTRICTIONS alone contributes 8:
`dbrest`..`dbrestriction`), taking the registry from 224 to ~285 entries. That
moves worst-case comparisons from ⌈log₂224⌉ = 8 to ⌈log₂285⌉ = 9 — one extra
comparison, still negligible. A bench isolating this call would measure noise.

**Substitute regression guard** (satisfies the "each feature independently
guarded" intent without a vanity bench):

1. The existing `builtin_functions_slice_is_sorted` (`lib.rs:859`) is the
   correctness guard — if a future codegen change breaks sorting/dedup (the only
   way binary search could silently misbehave), it fails loudly, now over the
   enlarged registry.
2. The **coverage assertion** in `builtin_abbreviations_are_registered` pins the
   exact set of expected abbreviations, so accidental loss or over-expansion is
   caught in CI.
3. **Existing `crates/oxabl_semantic/benches/semantic_bench.rs`** (the `resolve`
   group, `semantic_bench.rs:73-83`) already exercises `is_builtin_function` in
   context on every unresolved identifier; CodSpeed will catch any real
   regression in the resolve path. No fixture change is needed, but the corpus
   fixtures can optionally gain one abbreviated call to keep the path warm.

State plainly in the PR description: no new bench added; rationale is O(log n)
lookup with a ~7% entry growth; guarded by invariant + coverage tests and the
existing semantic resolve bench.

---

## Risks & Edge Cases

- **Prefix collisions with user identifiers.** The abbreviations for reserved
  keywords cannot legally be user symbol names (they are reserved), so a
  `gateway`/`term` collision cannot occur in valid ABL. Even if it did,
  `is_builtin_function` only suppresses LINT0001 for the exact folded string;
  it does not shadow a resolved local (locals resolve first at
  `resolve.rs:1679` before the built-in check at `:1703`).
- **IS-ATTR-SPACE / IS-LEAD-BYTE.** Both document `min_abbreviation = IS-ATTR`,
  but prefix expansion slices each keyword's own name, so they emit distinct
  shortest forms `is-attr` and `is-lead` (matching `kind.rs:993-994`). No shared
  entry, no dedup collapse; both must appear in the registry and both are tested.
- **Over-suppression of real undefined symbols.** The negative test
  (`non_abbreviable_builtin_truncation_still_fires`) guards that we only add
  prefixes for the 16 reserved functions — non-reserved truncations like
  `LENGT` still fire. The expansion is bounded by each function's documented
  `min_abbreviation`, so `av` (below `AVAIL`) stays unregistered and still
  fires.
- **Codegen drift.** If a future `abl_reserved_keywords.txt`/override changes a
  `min_abbreviation`, regeneration updates the registry and the coverage test
  must be updated in lockstep — this is the intended source-of-truth coupling.
- **Byte-boundary slicing.** All 16 names are ASCII; `&lower[..len]` is safe.
  If a non-ASCII abbreviated keyword were ever added, slicing would need a char
  boundary guard — out of scope but noted.

---

## Rollout

- Feature branch `feat/lint-builtin-fn-abbreviations` off `master`.
- Single self-contained PR: codegen change (~15 lines), regenerated
  `builtins.rs`, and the four new tests. Reviewable in isolation.
- CI (`cargo check`/`test`/`fmt --check`/`clippy -D warnings`) and CodSpeed run
  on the branch as normal; no bench target added, existing semantic bench
  covers the path.
- Conventional-commit `feat:` so Release Please picks it up as a minor bump.

---

## Open Questions

1. Should the codegen also emit a compile-time sanity assertion on registry
   size (e.g. `const _: () = assert!(BUILTIN_FUNCTIONS.len() >= 224);`), or is
   the coverage test sufficient? (Leaning: test only — a size floor assertion
   is brittle and adds little over the explicit coverage test.)
2. Do we want the module doc comment in `builtins.rs` to explicitly mention
   that abbreviations are included, or leave the header generic? (Leaning:
   add one sentence noting reserved-keyword abbreviations are expanded.)
3. Is there any consumer besides `resolve.rs:1703` that reads
   `BUILTIN_FUNCTIONS` and might assume "full names only"? (Believed no —
   verify with a workspace grep before merge.)
