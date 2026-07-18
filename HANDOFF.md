# Handoff: #65 closed → #66 next

**Date:** 2026-07-18  
**Branch:** `fix/inline-preproc-if-expression` (pushed to `origin`)  
**Branch tip:** `a43546d` (docs)  
**Functional pin (all #65 fixes):** `da7fa4d5650839e162bb4da29d161177e8e12bfd`  
**Closed:** https://github.com/oxabl-project/oxabl/issues/65  
**Open next:** https://github.com/oxabl-project/oxabl/issues/66  

**Plans:**
- `docs/plans/2026-07-17-003-fix-inline-preproc-if-expression-plan.md` (#65 original)
- `docs/plans/2026-07-18-001-fix-procedure-in-super-plan.md`
- `docs/plans/2026-07-18-002-fix-positional-in-define-values-plan.md`
- `docs/plans/2026-07-18-003-close-65-rescope-plan.md`

---

## Current state

| Item | Status |
|------|--------|
| #65 (mid-line `&IF` / ADM2 accessor preproc) | **Closed** (completed, re-scoped) |
| PREPROC002 on 9-module corpus | **596 → 0** |
| Original `Unexpected token Then` / empty mid-line `&IF` abort | **Fixed** |
| Branch vs `master` | Not merged yet — open PR when ready |
| #66 (xp-property `BUFFER-FIELD` path) | **Open** — next corpus blocker |
| PARSE001 after #65 | ~306 (not a #65 regression; see below) |

Downstream can pin `da7fa4d` (or branch HEAD) for the #65 wins without waiting on #66.

---

## What we got done (#65)

Stock WebSpeed/ADM2 aborted preprocessing on forms like:

```abl
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
```

Root cause was **not** misclassified includes (`get`/`set`/`fn`/`fnarg` are real
extensionless ADE files under `$DLC/tty`). Four layers of expansion fidelity
were fixed iteratively against corpus A/B:

| Round | Commit | Fix |
|-------|--------|-----|
| 1 | `11d19d9` | Mid-line `&IF`: keep branch body after `&THEN`/`&ELSE`/`&ENDIF` (`skip_after_if_keyword`). Positional `{N}` in `&IF` conditions + string literals. `:U` after quoted strings in conditions. Loud PREPROC002 on `check`. |
| 2 | `1aedb17` | Line-oriented directives (`&UNDEFINE` / define / `&MESSAGE`) stop at same-line `&ELSE`/`&ELSEIF`/`&ENDIF` so get/set one-liners close. **PREPROC002 → 0.** |
| 3 | `ae462b1` | Parser: `PROCEDURE name IN SUPER:` (and `THIS-PROCEDURE` / handle / period-only empty body). Companion gap revealed once prototypes expanded. |
| 4 | `da7fa4d` | Positional `{N}` expanded **inside** `&SCOPED-DEFINE`/`&GLOBAL-DEFINE` values at define time (include scope only). `DYNAMIC-FUNC` min-abbrev of `DYNAMIC-FUNCTION`. |

### Files touched (high level)

| Area | Path |
|------|------|
| Preprocessor | `crates/oxabl_preprocessor/src/preprocessor.rs`, `condition.rs` |
| Parser | `crates/oxabl_parser/src/parser/statements.rs` (+ tests) |
| Lexer | `resources/keyword_overrides.toml` → regenerated `kind.rs` / `build.rs` |
| CLI | `crates/oxabl/src/main.rs` (loud PREPROC002) |
| Gate | `scripts/corpus-ab-gate.sh` |

### Tests (at functional pin)

- `oxabl_preprocessor`: **141** passing (inline `&IF`, get/set shapes, define-value `{N}`, top-level `{N}` preserve, real-shape `fn`/`fnarg` stubs)
- `oxabl_parser`: **462** passing (incl. `IN SUPER`, `DYNAMIC-FUNC … IN handle`)

### Re-scoped done-when (why close without PARSE001 ≤ 9)

Original criterion 7 (whole-tree PARSE001 ≤ baseline) mixed preprocessor bugs with
deeper ADM2 SmartObject expansion. After PREPROC002 hit 0, remaining PARSE001 is
almost entirely the **xp-property `BUFFER-FIELD` fast path** — an explicit
non-goal of the #65 plan.

**Dedup skew:** baseline concentrated errors in a few shared includes that aborted
early (~9 files). After expansion succeeds, the same construct surfaces once per
WebSpeed file (~300 × 1). That is a measurement artifact of progress, not a
34× regression of the four surface fixes.

---

## Downstream pin & smoke

```toml
# Cargo.toml — all oxabl_* crates share one rev
rev = "da7fa4d5650839e162bb4da29d161177e8e12bfd"
# or branch = "fix/inline-preproc-if-expression"
```

```bash
# fnarg expression position
mkdir -p /tmp/tty
cat > /tmp/tty/fnarg << 'EOF'
&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
EOF
cat > /tmp/host.p << 'EOF'
DEFINE VARIABLE cQ AS CHARACTER NO-UNDO.
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
EOF
oxabl analyze /tmp/host.p --preprocess -I /tmp/tty
# exit 0; no Unexpected token Then

# get/set inline UNDEFINE + set DYNAMIC-FUNC handle path
cat > /tmp/tty/set << 'EOF'
&IF "{3}":U = "":U &THEN &SCOPED-DEFINE ADMHdl TARGET-PROCEDURE &ELSE &SCOPED-DEFINE ADMHdl {3} &ENDIF
DYNAMIC-FUNC("set{1}":U IN {&ADMHdl}, {2})
EOF
cat > /tmp/set_host.p << 'EOF'
{set DataSourceEvents evtList TARGET-PROCEDURE}
EOF
oxabl check /tmp/set_host.p --preprocess -I /tmp/tty
# exit 0; expands IN TARGET-PROCEDURE, not IN {3}

# IN SUPER
cat > /tmp/in_super.p << 'EOF'
PROCEDURE assignDBRow IN SUPER:
  DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE.
END PROCEDURE.
EOF
oxabl check /tmp/in_super.p
# exit 0
```

With real OpenEdge: put `$DLC/tty` (and `gui`/`src`/`adm2` as needed) on `-I`.

---

## What's waiting: #66

**Issue:** https://github.com/oxabl-project/oxabl/issues/66  
**Title:** ADM2 get/set xp-property BUFFER-FIELD fast path still fails to parse after #65  
**State:** Open  

### Problem

Round-4 fixed the `DYNAMIC-FUNC(... IN {&ADMHdl}, …)` branch of real `set`/`get`.
Most corpus WebSpeed programs take the **other** branch: when `DEFINED(xp{1})`
(and related xp-* defines) is true, ADE emits grouped buffer-field assignments
instead of dynamic-function calls.

### Failure modes (from corpus A/B @ `da7fa4d`)

1. **~299× `Unexpected token Comma`** — unquoted comma-list as BUFFER-VALUE:

   ```abl
   ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest
   ```

2. **~4× `Expected '.' to end statement`** — conditional `ASSIGN` not emitted
   (gated on `xp-assign` / `xp{1}` from ADM property includes), so consecutive
   accessor assignments run bare without a statement opener/terminator.

### Likely work (larger than a one-commit polish)

- Fixtures: real or stubbed `get`/`set` with `DEFINED(xp…)` true so the
  BUFFER-FIELD branch is taken (do not only test the DYNAMIC-FUNC branch).
- Faithful handling of xp-property DEFINEs (`xp-assign`, `xp{Prop}`,
  `xp-no-repository`, …) that gate which branch emits.
- Grouped `ASSIGN` emission when those gates fire.
- Character values that are unquoted comma-separated lists (parse as one
  expression, or match whatever ABL expansion actually produces).
- Possibly parser support for chained `handle:BUFFER-FIELD(...):BUFFER-VALUE = …`
  statement forms if that is what remains after expansion.

### Success bar for #66

Same 9-module erp-5899 sample, **schema-loaded**, oxabl ≥ `da7fa4d`:

| Signal | Pass |
|--------|------|
| PREPROC002 | stay **0** |
| PARSE001 (deduped) | ≤ baseline **9** |
| Net parse fails | ≤ 0 |

### Suggested first steps for #66

1. Capture expanded text of one failing file (or a minimal stub of real
   `$DLC/tty/set` with xp-* defines forced true).
2. Reduce to the smallest assign/BUFFER-FIELD/comma-list that still fails.
3. Decide preproc vs parser ownership per symptom before coding.
4. Re-run `scripts/corpus-ab-gate.sh` after each slice (same modules as #58/#65).

---

## Still open (process)

1. **Open PR** `fix/inline-preproc-if-expression` → `master` for the #65 stack.
2. After merge, bump downstream oxabl pin; re-smoke WebSpeed entry points
   (`wrap-cgi.i` / `query.i` / `get`+`set` with `$DLC/tty` on `-I`).
3. Tackle **#66** when ready to invest in xp-property fidelity.

---

## Related issues

| Issue | Relation |
|-------|----------|
| #65 | Closed — preprocessor / mid-line `&IF` / companion parser gaps |
| #66 | Open — xp-property BUFFER-FIELD fast path (next) |
| #64 | Undefined `{&name}` → empty (merged; related preproc quality) |
| #58 | LINT0001 accuracy; same 9-module A/B harness pattern |
| #62 | Loud PREPROC007; PREPROC002 also loud after #65 |

---

## Quick triage if ADM2 still fails after pin ≥ `da7fa4d`

1. **PREPROC007** for `get`/`set`/`fn`/`fnarg` → missing `$DLC/tty` (or `gui`) on `-I`.
2. **PREPROC002** / mid-line empty `&IF` → pin too old; need ≥ `1aedb17` (prefer `da7fa4d`).
3. **`Expected ':' after procedure name` on `IN SUPER`** → pin ≥ `ae462b1`.
4. **`IN {3}` / Expected ')' on DYNAMIC-FUNC** → pin ≥ `da7fa4d`.
5. **`Unexpected token Comma` on BUFFER-VALUE = a,b,c** or bare accessor assigns → **#66**, not a #65 regression.
