# Corpus Remaining Failures — 99.5% (1825/1834)

All require preprocessor evaluation or are correct parser errors that belong in the tooling layer.

**Decision (2026-04-12):** No parser-level workarounds will be implemented for these failures. The correct fix is an `oxabl_preproc` crate that runs before the parser and flattens the token stream. See [docs/plans/2026-04-12-001-feat-dynamic-include-preproc-aware-if-plan.md](../plans/2026-04-12-001-feat-dynamic-include-preproc-aware-if-plan.md).

---

## 1. `hasDeco.i:31:1` — `Expected '.' to end statement`

**Root cause:** The file contains a bare expression with no trailing period:

```abl
(
CAN-FIND(FIRST order_line_art NO-LOCK WHERE ...) OR
CAN-FIND(FIRST order-line NO-LOCK WHERE ...)
)
```

The expression itself parses successfully — the error fires when the parser expects `.` to end the statement and hits EOF instead. The period is intentionally absent because this file is designed to be inlined at the call site:

```abl
IF {hasDeco.i}
THEN ...
```

The `.` belongs to the surrounding statement in the including file.

**Fix:** The parser error is correct — a missing terminator is a real error. The right place to handle this is in the tooling layer (e.g., an LSP diagnostic that annotates the error: "this `.i` file may be intentionally included as a sub-expression rather than a standalone program"). The parser should not be made lenient here.

---

## 2. `wsoecontainerall.i:2:1` — `Expected '.' to end statement`

**Root cause:** File contains a single string literal `"crm_webclient/wsoecontainerall.js,..."` — a JavaScript file list, not ABL code.

**Fix:** The parser error is correct. Same tooling-layer approach as #1 applies if needed.

---

## 3. `oecrhead.i:644:1` — `Unexpected token LeftBrace`

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

## 4. `UPSReady.cls:70:9` — `Unexpected token KwElse`

**Root cause:** An IF/ELSE statement has its ELSE branch separated from THEN by a `&IF...&ENDIF` block:
```abl
If not available carrier-code Then
   Assign pcUseUPSReady = false.
&IF DBTYPE("freight_db") ne ? &THEN
   Else If can-find(first ups_service_types) ... Then do: ... End.
&ENDIF
   Else Assign ...   /* orphaned ELSE at statement level */
```
The parser closes the IF statement after `Assign pcUseUPSReady = false.` and then sees `Else` as a bare statement.

**Fix:** Would require tracking "dangling ELSE" state across preprocessor boundaries — not achievable without evaluating `DBTYPE()`.

---

## 5. `oe100fr.p:164:1` — `Unexpected token PreprocElse`

**Root cause:** A DO block is opened in one `&IF` branch but its `END.` is in a separate `&IF` block further down the file:
```abl
&IF "{&order-buffer}" eq "web-order" &THEN
Do transaction:           /* block opened here */
   Find order ... no-error.
&ELSE                     /* parser is inside DO body, sees PreprocElse */
   Find order ... no-error.
&ENDIF
... (much later) ...
&IF "{&order-buffer}" eq "web-order" &THEN
END.                      /* END for the DO block */
&ENDIF
```

**Fix:** Requires preprocessor evaluation of `{&order-buffer}` to know whether the DO block is present and where it ends.

---

## 6. `oe_deco_price_line.p:398:4` — `Unexpected token PreprocElse`

**Root cause:** An IF condition is split across a preprocessor boundary — the right-hand operand of `OR` is conditional:
```abl
&IF "{&order}" eq "web-order" &THEN
IF (expr1 NE (qty + backordered)) OR
&ELSE
IF (expr1 NE qty) OR          /* PreprocElse appears mid-expression */
&ENDIF
   (order.invoiced OR order.need-to-inv) THEN
```
The expression parser consumes `OR` then hits `PreprocElse` as the right operand.

**Fix:** Requires preprocessing; the expression structure only becomes valid after one `&IF` branch is selected.

---

## 7. `check-vas-item.i:17:1` — `Unexpected token PreprocElse`

**Root cause:** METHOD/FUNCTION header is conditionally `METHOD` or `FUNCTION`, but the body is shared after `&ENDIF`:
```abl
&IF "{&classDef}" EQ "true":U &THEN
METHOD PUBLIC LOG itemIsVAS (...):   /* parser enters method body */
&ELSE                               /* PreprocElse inside method body loop */
FUNCTION itemIsVAS LOG (...):
&ENDIF
    IF CAN-FIND(...) THEN RETURN TRUE.   /* shared body */
END.
```
The method body parser sees `PreprocElse` before any `END METHOD.`.

**Fix:** Breaking out of the method body at `PreprocElse` moves the error to `Unexpected token End` for the orphaned `END.` — the failure count stays the same.

---

## 8. `perfectly_packaged_common.i:21:1` — `Unexpected token PreprocElseif`

**Root cause:** Same pattern as `check-vas-item.i` but with a three-way `&IF/&ELSEIF/&ELSE` for static METHOD / instance METHOD / FUNCTION — repeated for multiple functions in the same file.

**Fix:** Same as #7 — breaking out of method bodies at preproc boundaries shifts errors to orphaned `END.` tokens; net failure count unchanged.

---

## Summary

| # | File | Category | Fixable without preprocessor? |
|---|---|---|---|
| 1 | `hasDeco.i` | Expression fragment, missing trailing period | Parser error is correct; handle in tooling layer |
| 2 | `wsoecontainerall.i` | Not ABL source | Parser error is correct; handle in tooling layer |
| 3 | `oecrhead.i` | Lexer: `{{&var} args}` | Possible (hard) |
| 4 | `UPSReady.cls` | IF/ELSE split by `&IF...&ENDIF` | No |
| 5 | `oe100fr.p` | Block opened/closed in separate `&IF` blocks | No |
| 6 | `oe_deco_price_line.p` | Expression split mid-`OR` | No |
| 7–8 | `check-vas-item.i`, `perfectly_packaged_common.i` | METHOD/FUNCTION header in `&IF`, shared body | No (error shifts) |
