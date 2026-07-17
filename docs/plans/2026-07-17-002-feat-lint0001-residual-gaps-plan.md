---
title: "feat: LINT0001 residual language-coverage gaps (#58 follow-up)"
type: feat
status: ready
date: 2026-07-17
origin: GitHub #58 (2026-07-17 corpus re-run comment)
branch: feat/lint0001-residual-gaps
depends_on:
  - "fix/undefined-preproc-macro-empty (#64) — preferred first for corpus impact"
---

# feat: LINT0001 residual language-coverage gaps (#58)

## Context

After builtins, abbreviations, schema, SHARED flags, and include-path work:

- LINT0003 −86%, LINT0001 −45% raw.
- ~97% of raw LINT0001 is include-multiplicity (consumer can dedupe).
- **~21.9k unique** findings remain; ~**39%** are rule/parser gaps below;
  ~**61%** are ambient/cross-scope (defer to multi-file / ambient API).

Also: **#64** (undefined macro → empty) is the dominant *shared-global*
false-positive driver and is a **preprocessor** fix, tracked separately. Land
#64 first when possible.

## Goals (this plan)

Close the five categorized unique gaps from the 2026-07-17 #58 comment, in
frequency order. Each item is independently testable.

## Non-Goals

- Cross-file GLOBAL SHARED / super-procedure ambient symbols (~61%).
- Full OO type graph for static members (mark External, don't resolve members).
- Changing LINT0001 severity or adding conservative-mode config (follow-up).

---

## Item A — System handles seed list (~3% unique, high confidence)

**Today:** `crates/oxabl_semantic/src/builtins.rs` seeds only:
`this-object`, `super`, `self`, `session`, `error-status`.

**Corpus needs at least:** `THIS-PROCEDURE`, `CURRENT-WINDOW`, `WEB-CONTEXT`,
and the rest of the standard system-handle set (ACTIVE-WINDOW, FOCUS, FILE-INFO,
SESSION already seeded, COM-SELF, LAST-EVENT, LOG-MANAGER, TERMINAL, …).

**Work:** Expand the seed list to the full standard Progress system-handle set
used in expression position. Keep `SymbolKind::BuiltIn`, Values namespace, root
scope. Update tests from "five" to the new count.

**Reference list (minimum — add any already recognized as Kind in lexer):**

```
this-object, super, self, session, error-status,
this-procedure, source-procedure, target-procedure, current-window,
active-window, focus, file-info, last-event, log-manager, terminal,
web-context, com-self, debugger, session (already), clipboard,
color-table, font-table, rcode-info, compiler, first-object, last-object,
current-iteration (if applicable), audit-control, audit-policy,
security-policy, codepage-table, default-window
```

Prefer a static `&[&str]` grown from Progress docs + corpus evidence. No need
for attributes (`SESSION:BATCH-MODE`) — member access on a seeded handle is
already skipped as External for the member.

**Tests:** each new handle resolves; LINT0001 silent on `THIS-PROCEDURE:HANDLE`
and `WEB-CONTEXT:IS-LOGGED-IN` style member access (object resolves).

**Effort:** 0.5d.

---

## Item B — PROPERTY SET accessor parameter (~10%)

**Today:** Parser **skips** `SET (INPUT pv AS CHARACTER)` without recording the
parameter (`statements.rs` ~4696–4712 depth-skip). Declare opens `PropertySet`
scope but never inserts `pv`.

**Work:**

1. **AST:** Extend `StatementKind::Property` with:
   ```rust
   set_parameter: Option<Box<Statement>>, // DefineParameter shape, or dedicated fields
   ```
   Prefer reusing `DefineParameter` / `ParameterType::Variable` as a single
   optional statement for consistency with Method/Constructor params.

2. **Parser:** When `SET (` seen, parse parameter list (at least one
   `INPUT name AS type`) into AST instead of skip-loop. Support `GET` param if
   ABL allows (rare); SET is the corpus hit.

3. **Declare:** When pushing `PropertySet` scope, declare the set parameter into
   that scope (same path as method parameters).

4. **Resolve/check:** Walk set body in PropertySet scope (already); param must
   be visible.

5. **ast-invariants.md** update (required by CLAUDE.md for AST reshape).

**Repro:**
```
DEFINE PUBLIC PROPERTY Title AS CHARACTER
  GET.
  SET (INPUT pv AS CHARACTER):  ttRec.Title = pv.  END SET.
```
→ `pv` resolved, no LINT0001.

**Effort:** 1–1.5d.

---

## Item C — Dynamic handle syntax `QUERY qh:...` (~13%)

**Today:** Parser builds a **compound Identifier** `"QUERY qh"` then postfix
`:attr`. Resolve looks up atom `query qh` → NotInScope → LINT0001.

**Preferred fix (parse):** Emit Identifier with **only the handle name** (`qh`),
not the keyword prefix. Keywords `QUERY`/`BUFFER`/`TEMP-TABLE`/`FRAME`/
`DATASET`/`STREAM`/`BROWSE` are syntactic sugar for handle-qualified access.

**Resolve:** Lookup `qh` in Values (and any Query namespace if exists). If
missing → `UnresolvedReason::External` (dynamic handle) **not** `NotInScope`,
so LINT0001 skips it. If `DEFINE QUERY qh` / `DEFINE VARIABLE qh AS HANDLE`
exists, resolve normally.

**Tests:**
- `QUERY qh:QUERY-PREPARE("...").` with no define → no LINT0001 (External).
- With `DEFINE VARIABLE qh AS HANDLE.` → Resolved.

**Effort:** 0.5–1d.

---

## Item D — Package-qualified / static class refs (~7%)

**Repro:**
```
acme.security.Auth:CheckUser(INPUT uid).
MyStatics:CurrentCompany = coId.
```

**Today:** Leading `acme` / `MyStatics` → NotInScope.

**Work:** In resolve, when walking `MethodCall` / `MemberAccess`:

- Recurse into `object` with a mode (or post-pass) such that if the object is
  `Identifier` or a chain of `FieldAccess` of identifiers and does **not**
  resolve as buffer/table/variable, record **`External`** (cross-file / type
  name) instead of `NotInScope`.

Cleanest approach:

```rust
// walk_expression for MethodCall/MemberAccess:
self.walk_expression_as_receiver(object, scope);
// where unresolved receivers become External, not NotInScope
```

Do **not** try to resolve class members in v1.

**Tests:** both repros → no LINT0001 on receiver.

**Effort:** 0.5–1d.

---

## Item E — Keywords / options as false identifiers (~6%)

### E1 — Logical `no` / `yes`

- `yes` is `Kind::Yes` → boolean literal.
- Bare `no` is **not** a keyword (reserved for `NO LOCK` space-separated path).
  It lexes as Identifier → LINT0001 on `ttRec.Flag = no.`

**Fix:** In `match_keyword`, map `"no"` → new `Kind::No` (or reuse a boolean
kind) **only when** `try_read_space_separated_lock` does not claim `NO LOCK`.
Order already: lock check runs **before** `match_keyword` on the word `no`.
So adding `"no" => Some(Kind::No)` is safe for `NO LOCK`.

Parser: treat `Kind::No` like `Kind::Yes` in literal path (`expressions.rs` +
`literal.rs`).

### E2 — `NO-LOCK` / `NO-WAIT` leftover tokens

If FIND/FOR still leave these as identifiers in the AST, fix **statement
parsers** to consume them. If they appear only when parse recovery fails, fix
recovery. Add tests: `FIND FIRST cust NO-LOCK NO-WAIT.` → no LINT0001.

### E3 — `SUBSTR`

`SUBSTRING` is in the builtin function registry; `SUBSTR` is not (non-reserved
function, but Progress documents SUBSTR as abbreviation in practice / corpus
uses it).

**Fix options:**
1. Add `substr` to builtin registry via codegen override / manual entry in
   `keyword_overrides.toml` or builtins generator exception list.
2. Or treat known Progress function abbreviations in builtins codegen.

Prefer explicit `substr` (and any other high-frequency corpus abbrevs) in
overrides if not generable from reserved-keyword rules.

**Effort:** 0.5–1d for E1–E3.

---

## Implementation order inside this plan

```
A system handles     (fast, pure semantic)
E1 no/yes literal    (lexer+parser, small)
E3 SUBSTR builtin    (codegen/registry)
C handle QUERY syntax (parser+resolve)
D static receivers   (resolve only)
B property SET param (AST+parser+semantic — largest)
E2 NO-LOCK consume   (as needed after repro)
```

Ship as **one branch** with sequential commits, or split A/E vs B vs C/D if
review size is a concern. Prefer one PR titled
`feat: close residual LINT0001 language gaps (#58)` with clear commit log.

## Verification

```bash
cargo test --workspace
# focused:
cargo test -p oxabl_semantic
cargo test -p oxabl_lint
cargo test -p oxabl_parser
cargo test -p oxabl_lexer
```

Manual: `oxabl analyze` on each minimal repro → zero LINT0001.

## Deferred (document on #58, do not implement here)

- Ambient GLOBAL SHARED from other compilation units.
- Super-procedure methods/functions.
- Include-multiplicity dedupe in oxabl itself (consumer already dedupes).
- Full static type resolution for class members.

## Effort

~3–5 days total for A–E.
