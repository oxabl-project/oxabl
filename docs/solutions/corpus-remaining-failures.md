# Corpus Remaining Failures — >99%

All require preprocessor evaluation or are correct parser errors that belong in the tooling layer.

**Decision (2026-04-12):** No parser-level workarounds will be implemented for these failures. The correct fix is an `oxabl_preproc` crate that runs before the parser and flattens the token stream. See [docs/plans/2026-04-12-001-feat-dynamic-include-preproc-aware-if-plan.md](../plans/2026-04-12-001-feat-dynamic-include-preproc-aware-if-plan.md).

The examples below are synthetic constructs that reproduce the *shape* of each failure pattern; the real triggering files are kept out of the repo.

---

## 1. Fragment-expression include — `Expected '.' to end statement`

**Root cause:** The file contains a bare expression with no trailing period:

```abl
(
CAN-FIND(FIRST item NO-LOCK WHERE ...) OR
CAN-FIND(FIRST detail-line NO-LOCK WHERE ...)
)
```

The expression itself parses successfully — the error fires when the parser expects `.` to end the statement and hits EOF instead. The period is intentionally absent because this file is designed to be inlined at the call site:

```abl
IF {frag-expr.i}
THEN ...
```

The `.` belongs to the surrounding statement in the including file.

**Fix:** The parser error is correct — a missing terminator is a real error. The right place to handle this is in the tooling layer (e.g., an LSP diagnostic that annotates the error: "this `.i` file may be intentionally included as a sub-expression rather than a standalone program"). The parser should not be made lenient here.

---

## 2. Non-ABL include payload — `Expected '.' to end statement`

**Root cause:** File contains a single string literal (e.g. a comma-separated JavaScript file list) — not ABL code.

**Fix:** The parser error is correct. Same tooling-layer approach as #1 applies if needed.

---

## 3. Doubly-nested include — `Unexpected token LeftBrace`

**Root cause:** Doubly-nested include reference:
```
{{&include-prog} &to = "{&to}" &from = "{&from}" ...}
```
The outer `{` is tokenized as `LeftBrace` because the lexer only handles `{identifier...}` and `{&var}`, not `{{&var} args}` (dynamic include where the filename is itself a preprocessor variable).

**Fix options:**

- **Lexer fix (correct, hard):** When `{` is followed by `{`, scan forward to find the balanced closing `}` (tracking brace depth through nested `{&var}` args) and emit the whole outer `{...}` as a single `IncludeReference` token. Meaningfully complicates the lexer.
- **Parser error recovery (simple, lossy):** When the parser sees `LeftBrace` at statement position, consume to the matching `RightBrace` and emit `Statement::Empty`. Gets the file parsing but throws away the include entirely.

**Recommendation:** Leave as a known limitation. This is one file in the corpus. Even with a correct lexer fix, the resulting AST node would be `IncludeReference(dynamic: true)` — a black box that no tooling can navigate, rename, or resolve without running the preprocessor first. The pattern actively resists static analysis and is not worth complicating the lexer for.

---

## 4. Dangling ELSE across `&IF...&ENDIF` — `Unexpected token KwElse`

**Root cause:** An IF/ELSE statement has its ELSE branch separated from THEN by a `&IF...&ENDIF` block:
```abl
If not available detail-buffer Then
   Assign lFlag = false.
&IF DBTYPE("sample_db") ne ? &THEN
   Else If can-find(first lookup-table) ... Then do: ... End.
&ENDIF
   Else Assign ...   /* orphaned ELSE at statement level */
```
The parser closes the IF statement after `Assign lFlag = false.` and then sees `Else` as a bare statement.

**Fix:** Would require tracking "dangling ELSE" state across preprocessor boundaries — not achievable without evaluating `DBTYPE()`.

---

## 5. DO block opened/closed in separate `&IF` blocks — `Unexpected token PreprocElse`

**Root cause:** A DO block is opened in one `&IF` branch but its `END.` is in a separate `&IF` block further down the file:
```abl
&IF "{&mode}" eq "variant-a" &THEN
Do transaction:           /* block opened here */
   Find rec ... no-error.
&ELSE                     /* parser is inside DO body, sees PreprocElse */
   Find rec ... no-error.
&ENDIF
... (much later) ...
&IF "{&mode}" eq "variant-a" &THEN
END.                      /* END for the DO block */
&ENDIF
```

**Fix:** Requires preprocessor evaluation of `{&mode}` to know whether the DO block is present and where it ends.

---

## 6. Expression split mid-`OR` — `Unexpected token PreprocElse`

**Root cause:** An IF condition is split across a preprocessor boundary — the right-hand operand of `OR` is conditional:
```abl
&IF "{&mode}" eq "variant-a" &THEN
IF (expr1 NE (qty + adjustment)) OR
&ELSE
IF (expr1 NE qty) OR          /* PreprocElse appears mid-expression */
&ENDIF
   (rec.field-a OR rec.field-b) THEN
```
The expression parser consumes `OR` then hits `PreprocElse` as the right operand.

**Fix:** Requires preprocessing; the expression structure only becomes valid after one `&IF` branch is selected.

---

## 7. Conditional METHOD/FUNCTION header, shared body — `Unexpected token PreprocElse`

**Root cause:** The header is conditionally `METHOD` or `FUNCTION`, but the body is shared after `&ENDIF`:
```abl
&IF "{&class-mode}" EQ "true":U &THEN
METHOD PUBLIC LOG doCheck (...):   /* parser enters method body */
&ELSE                              /* PreprocElse inside method body loop */
FUNCTION doCheck LOG (...):
&ENDIF
    IF CAN-FIND(...) THEN RETURN TRUE.   /* shared body */
END.
```
The method body parser sees `PreprocElse` before any `END METHOD.`.

**Fix:** Breaking out of the method body at `PreprocElse` moves the error to `Unexpected token End` for the orphaned `END.` — the failure count stays the same.

---

## 8. Three-way conditional header — `Unexpected token PreprocElseif`

**Root cause:** Same pattern as #7 but with a three-way `&IF/&ELSEIF/&ELSE` for static METHOD / instance METHOD / FUNCTION — repeated for multiple functions in the same file.

**Fix:** Same as #7 — breaking out of method bodies at preproc boundaries shifts errors to orphaned `END.` tokens; net failure count unchanged.

---

## Summary

| # | Pattern | Category | Fixable without preprocessor? |
|---|---|---|---|
| 1 | Fragment-expression include | Expression fragment, missing trailing period | Parser error is correct; handle in tooling layer |
| 2 | Non-ABL include payload | Not ABL source | Parser error is correct; handle in tooling layer |
| 3 | Doubly-nested include | Lexer: `{{&var} args}` | Possible (hard) |
| 4 | Dangling ELSE across `&IF...&ENDIF` | IF/ELSE split by `&IF...&ENDIF` | No |
| 5 | DO block opened/closed in separate `&IF` blocks | Block opened/closed in separate `&IF` blocks | No |
| 6 | Expression split mid-`OR` | Expression split mid-`OR` | No |
| 7–8 | Conditional METHOD/FUNCTION header, shared body | METHOD/FUNCTION header in `&IF`, shared body | No (error shifts) |
