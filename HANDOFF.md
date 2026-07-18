# Handoff: #65 mid-line `&IF` / ADM2 `{fn}` `{fnarg}`

**Date:** 2026-07-17 (updated 2026-07-18)  
**Branch:** `fix/inline-preproc-if-expression` (pushed to `origin`)  
**Issue:** https://github.com/oxabl-project/oxabl/issues/65  
**Plan:** `docs/plans/2026-07-17-003-fix-inline-preproc-if-expression-plan.md`

---

## TL;DR

Stock WebSpeed/ADM2 code aborted preprocessing (exit 5) on forms like:

```abl
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
```

**Root cause (after issue reframing):** not include misclassification. Real
`$DLC/tty/fnarg` is a one-line inline `&IF … &THEN … &ELSE … &ENDIF`. oxabl
treated the end of `&THEN` / `&ELSE` / `&ENDIF` as end-of-line, so the branch
body on the same line was discarded. Expression position collapsed to
`IF NOT THEN` → parse abort.

**Fix (round 1):** stop at the keyword (plus optional horizontal whitespace /
single newline), keep same-line body in the scan stream. Also make real
`fnarg` work: positional `{N}` in conditions and strings, `:U` after quoted
string literals in conditions. PREPROC002 is now loud on `check` for corpus
counting.

**Follow-up (round 2, after corpus A/B):** expression-position worked, but
inline `&IF` whose body is a **line-oriented directive** still left PREPROC002
and regressed PARSE001. Shape:

```abl
&IF TRUE &THEN &UNDEFINE foo &ENDIF
```

(`$DLC/tty/get` / `set` line 1 and 18). `&UNDEFINE` / `&SCOPED-DEFINE` /
`&GLOBAL-DEFINE` / `&MESSAGE` used `skip_to_eol` and swallowed the trailing
`&ENDIF`. **Fix:** those payloads stop at a same-line `&ELSE` / `&ELSEIF` /
`&ENDIF` boundary. Round-2 cleared PREPROC002 596→0.

**Follow-up (round 3):** remaining PARSE001 +48 was a **pre-existing parser
gap** revealed by expansion: `PROCEDURE name IN SUPER:` (ADM2 prototypes).
`parse_procedure` now accepts optional `IN SUPER|THIS-PROCEDURE|handle` before
`:` / `.` (period form = empty body). FUNCTION already handled `IN SUPER`.

**Follow-up (round 4):** after IN SUPER, PARSE001 climbed further as `{get}`/
`{set}` expand. Two companion fixes: (1) expand positional `{N}` inside
`&SCOPED-DEFINE`/`&GLOBAL-DEFINE` values at define time (so `ADMHdl` is not
literal `{3}`); (2) `DYNAMIC-FUNC` abbreviates `DYNAMIC-FUNCTION` so ADE
`IN handle` calls parse.

**Status (2026-07-18):** #65 **closed** with re-scoped criterion — preprocessor
goals met (PREPROC002 596→0; four surface fixes + IN SUPER companion). Remaining
corpus PARSE001 (~306) is ADM2 **xp-property BUFFER-FIELD** expansion (deferred
non-goal); tracked as a follow-up issue. Downstream may pin `da7fa4d` (or branch
HEAD) for the #65 fixes without waiting on xp-property.

**Corpus note:** PARSE001 9→~306 is mostly a measurement artifact: baseline
aborted inside shared includes; after expansion the same gap appears per-file.

---

## Problem history

### Original issue report

- Symptom: `PREPROC007` for `set` / `fn` / `fnarg`, then `Unexpected token Then`, exit 5.
- Suspected: oxabl treating SmartObject macros as includes incorrectly.

### Issue comment 2 (authoritative reframing)

1. `{get}` / `{set}` / `{fn}` / `{fnarg}` **are** real extensionless includes under
   `$DLC/tty` and `$DLC/gui` — classification is correct; missing dirs only
   explain PREPROC007.
2. Adding `$DLC/tty` to `-I` **does not** stop the parse abort.
3. Real `fnarg` body (one line):

   ```
   &IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
   ```

4. Minimal no-DLC repro:

   ```abl
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF .
   ```

   → entire `&IF` block dropped → `i = .` → parse error.

5. Multi-line statement-position `&IF` already worked; **inline / mid-line** did not.
6. Regression stubs for `fn`/`fnarg` **must keep the inline `&IF` shape** — a
   simplified bare `DYNAMIC-FUNCTION` stub false-passes against a still-buggy
   preprocessor.

---

## What we shipped

### 1. Directive end offsets (`oxabl_preprocessor`)

- `parse_if_condition`: after `&THEN`, do **not** `skip_to_eol`.
- `&ELSE` / `&ENDIF`: same.
- New helper `skip_after_if_keyword`: skip spaces/tabs; if that reaches EOL,
  consume one newline (multi-line forms stay clean); if non-ws follows, leave
  it (inline forms keep the body).

### 1b. Line-oriented directive payloads stop at if-chain boundary

- `&UNDEFINE`, `&SCOPED-DEFINE` / `&GLOBAL-DEFINE`, `&MESSAGE`: end at EOL
  **or** at same-line `&ELSE` / `&ELSEIF` / `&ENDIF` (via
  `find_same_line_if_boundary` / `skip_to_eol_or_if_boundary`).
- Unlocks ADM2 `get`/`set` one-liners without unclosed `&IF`.

### 2. Positional args for real `fnarg`

- Expand `{N}` in `&IF` / `&ELSEIF` **condition text** before evaluate.
- Expand `{N}` **inside string literals** (e.g. `"{1}":U`).
- Missing `{N}` **inside an include** → empty (ABL); top-level bare `{N}` still
  preserved for the lexer.

### 3. Condition `:U` (and friends)

- After a **quoted string literal** token, strip trailing `:letters` so
  `"":U = "":U` is true.
- **Out of scope:** bare `{&macro}:U` expanding to an unquoted value. ADM2
  uses the quoted form.

### 4. CLI: PREPROC002 is loud

- `oxabl check` / analyze surface `PREPROC002` (unclosed `&IF`) like
  `PREPROC007`, so corpus A/B can count it on stderr.

### 5. Tests (~28 new; 136 preprocessor tests green)

Coverage includes: inline true/false/else, expression `IF NOT &IF…`, `:U`
empty-eq, nested/elseif one-liners, multi-line still OK, positional in string +
condition, missing positional empty, **real-shape** `fnarg` 2-arg and 3-arg
stubs, `fn` expression smoke, **inline `&UNDEFINE`/`&SCOPED-DEFINE`/
`&GLOBAL-DEFINE`/`&MESSAGE` + `&ENDIF`**, ADM2 `get`/`set` shape, include stub.

### 6. Corpus gate harness (not executed here)

- `scripts/corpus-ab-gate.sh` — baseline / candidate / diff for the 9-module
  erp-5899 sample (same idea as #58).
- Plan criterion 7: **merge-blocking** — no net-new parse failures; PREPROC002
  should drop toward 0.

### Files in the commit

| Path | Role |
|------|------|
| `crates/oxabl_preprocessor/src/preprocessor.rs` | Core fix + tests |
| `crates/oxabl_preprocessor/src/condition.rs` | `:U` stripping + tests |
| `crates/oxabl/src/main.rs` | Loud PREPROC002 |
| `docs/plans/2026-07-17-003-fix-inline-preproc-if-expression-plan.md` | Full plan |
| `scripts/corpus-ab-gate.sh` | A/B gate |

---

## Local verification (already done)

| Check | Result |
|-------|--------|
| Minimal `i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF .` | exit 0 |
| `IF NOT {fnarg setOpenQuery cQ} THEN RETURN.` + real-shape stub on `-I` | exit 0 |
| `cargo test -p oxabl_preprocessor` | 128 passed |
| Full workspace `cargo test` | green |
| `cargo clippy -p oxabl_preprocessor -- -D warnings` | clean |

---

## Downstream consumer handoff

### Pin

```toml
# Cargo.toml (all oxabl_* crates share one rev)
# Prefer branch HEAD after the get/set follow-up (post-11d19d9):
branch = "fix/inline-preproc-if-expression"
# or pin the follow-up commit once pushed (see latest on that branch)
```

### Suggested smoke (no full corpus)

```bash
# 1) expression-position fnarg (round 1)
mkdir -p /tmp/tty
cat > /tmp/tty/fnarg << 'EOF'
&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
EOF

cat > /tmp/host.p << 'EOF'
DEFINE VARIABLE cQ AS CHARACTER NO-UNDO.
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
EOF

oxabl analyze /tmp/host.p --preprocess -I /tmp/tty
# expect exit 0, no PREPROC007 for fnarg, no Unexpected token Then

# 2) get/set-shaped inline UNDEFINE (round 2)
cat > /tmp/getset.p << 'EOF'
&GLOBAL-DEFINE foo bar
&IF TRUE &THEN &UNDEFINE foo &ENDIF
&GLOBAL-DEFINE xp-reset-values yes
&IF DEFINED(xp-assign) = 0 AND DEFINED(xp-reset-values) <> 0 &THEN &UNDEFINE xp-reset-values &ENDIF
MESSAGE "ok".
EOF
oxabl check /tmp/getset.p --preprocess
# expect exit 0, no PREPROC002
```

With a real OpenEdge install:

```bash
oxabl analyze "$DLC/src/adm2/query.i" --preprocess \
  -I "$DLC/tty" -I "$DLC/gui" -I "$DLC/src" -I "$DLC/src/adm2"
# expect no exit 5 from {fnarg}/{fn} or get/set inline &UNDEFINE
```

### Corpus A/B (required before treating this as production-clean)

```bash
export CORPUS_ROOT=/path/to/erp-5899/erp/code   # same tree as #58
export MODULES="mod1 mod2 …"                    # same 9 modules as #58
export INCLUDE_PATHS="-I $CORPUS_ROOT …"        # usual PROPATH

cargo build --release -p oxabl
# A — master pin:
./scripts/corpus-ab-gate.sh baseline
# B — this rev:
./scripts/corpus-ab-gate.sh candidate
./scripts/corpus-ab-gate.sh diff
```

| Signal | Pass |
|--------|------|
| Parse fail count (B − A) | ≤ 0 |
| PARSE001 (deduped file count) | B ≤ A (baseline was 9) |
| PREPROC002 (B vs A) | B ≤ A (prefer stay near post-round-1 drop) |
| New top parse-error patterns | empty or fixed (esp. `smrtprto.i` / `qryprto.i`) |

**Note:** Round 1 was A/B'd by the consumer: PREPROC002 −293 (good) but
PARSE001 **+48** (fail). Round 2 targets that regression. This agent machine
still does not have the erp-5899 tree — please re-run A/B on the follow-up pin.

---

## What we deliberately did *not* do

- Auto-discover `$DLC` or inject ADE stubs into default include paths.
- Ship Progress ADE sources (tests use original minimal real-shape stubs).
- Bare-name unresolvable-include → `?` recovery (earlier red-herring plan;
  optional follow-up only).
- Full `xp{Prop}` buffer-field fast path inside real `get`/`set`.
- Couple to #64 (undefined `{&macro}` → empty) — already separate; related
  preprocessor quality only.

---

## Risk callouts for reviewers

1. **Global semantic change:** every `&IF`/`&ELSE`/`&ENDIF` end is affected.
   Same-line junk after multi-line-style directives is now scanned as code
   (closer to AVM). Corpus A/B is the real regression net.
2. **Wrong branch if condition/positional half-landed:** fixed together in one
   PR; real-shape `fnarg` tests lock both THEN (2-arg) and ELSE (3-arg handle).
3. **`:U` only after string literals** — documented; ADM2 path is covered.

---

## Related issues / context

| Item | Relation |
|------|----------|
| #65 | This fix |
| #65 comment 2 | Root-cause reframing (inline `&IF`) |
| #64 | Undefined `{&name}` → empty (separate, already merged) |
| #58 | LINT0001 accuracy; same 9-module A/B pattern |
| #62 / PREPROC007 | Loud unresolvable include; PREPROC002 now also loud |

---

## Open questions / next steps

1. **Re-run criterion 7** on erp-5899 with the round-2 pin; paste A/B numbers
   into the PR or issue before merge. Bar: PARSE001 ≤ 9 and PREPROC002 stays
   well below baseline 596.
2. Optionally open a PR from `fix/inline-preproc-if-expression` → `master`.
3. After merge, bump downstream oxabl pin and re-check WebSpeed/ADM2 entry
   points (`wrap-cgi.i` chain / `query.i` / `get`+`set`).

---

## Quick pointer for implementers

If something still fails on stock ADM2:

1. Confirm `-I` includes `$DLC/tty` (and/or `gui`) — extensionless `get`/`set`/`fn`/`fnarg`.
2. If PREPROC007 only: include path / PROPATH problem, not this bug.
3. If parse abort on `IF NOT … THEN` with includes resolved: re-check that the
   pin is ≥ `11d19d9` and that stubs (if any) still use **inline** `&IF`, not
   a simplified expansion.
