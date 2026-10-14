---
title: "Strict analysis and strictabl"
status: accepted
date: 2026-10-13
---

# Strict analysis and strictabl

## Purpose and scope

Add strict analyses to oxabl that make transactions, database access, handle lifetimes and
unknown values explicit and checkable. Grow a separate strictabl language only where plain ABL
cannot express a rule.

The targets are the three biggest problems seen in large production ABL codebases: memory leaks,
hidden database round trips, and code whose behaviour can't be told by reading it.

- **Adoption model:** TypeScript-style. Strict rule groups are opt-in per directory or file. A
  baseline file records existing violations so CI fails only on new ones.
- **Output:** everything runs on stock OpenEdge. oxabl reports and optionally autofixes; it never
  requires a different runtime.
- **Non-goals:** no runtime, no native codegen, and no new syntax in `oxabl_parser`'s default path.

## Placement rule: lint rule or strictabl

The test: if OpenEdge's compiler accepts the source, the check belongs in oxabl. If the rule
needs syntax OpenEdge would reject, it belongs in strictabl.

- **oxabl lint rule:** detects a problem in existing `.p`, `.cls` or `.i` files, and the fix is
  expressible in standard ABL (add `NO-UNDO`, an explicit lock, `DO TRANSACTION`, `DO FOR` strong
  scope, a `FINALLY` with `DELETE OBJECT`, a `BY` clause). Autofix is optional and allowed only
  when provably semantics-preserving.
- **oxabl intent annotations:** when the programmer must state intent the analyzer can't infer
  (effects, nullability, ownership), first try ABL annotations such as
  `@strict.effects(reads="Customer").`. They are legal ABL that OpenEdge ignores, so they stay in
  oxabl. Open question: which statements annotations may precede.
- **strictabl:** only when the intent can't be written readably as annotations: nullable types
  inside expressions, ownership blocks, typed procedure references, module imports.

| Concern | oxabl lint rule | strictabl layer |
|---|---|---|
| Transactions | Flag implicit transactions, procedure-wide scope, transactions spanning UI or calls | Nothing new; `DO TRANSACTION` is already explicit |
| Locks | Flag default `SHARE-LOCK` and locks outliving their transaction | Nothing new |
| Round trips | Infer per-procedure effects; flag per-row access and whole-index scans | Effects declared in signatures, checked at every call site |
| Handles | Flag creates without a delete on every path | `using` ownership blocks that always emit `FINALLY` + `DELETE OBJECT` |
| Unknown value | Dataflow warnings where `?` can reach comparisons, arithmetic, field access | Non-null by default; `integer?` marks nullable |
| Calls | Flag `RUN VALUE`, dynamic invokes, shared variables, signature mismatches | Typed procedure references, dispatch tables, imports instead of PROPATH lookup |

## New crates

Five analysis crates sit between `oxabl_semantic` and `oxabl_lint`. Each produces a model that
rules, `oxabl analyze` and the LSP all read, so nothing is computed twice.

| Crate | Responsibility | Depends on | Produces |
|---|---|---|---|
| `oxabl_flow` | Control-flow graph per block, with ABL block semantics: `UNDO`, `LEAVE`, `NEXT`, `RETRY`, `ON ERROR`, `CATCH`/`FINALLY`, `RETURN ERROR` | `oxabl_semantic` | CFG with block properties and error edges |
| `oxabl_scope` | Replicates the compiler's buffer scoping (strong, weak, free) and transaction block assignment, including scope widening | `oxabl_flow`, `oxabl_schema` | Buffer scopes, transaction spans, lock state per buffer |
| `oxabl_handles` | Lifetimes of dynamic objects: `CREATE` (buffer, query, temp-table, widget), `NEW`, `RUN PERSISTENT`, `MEMPTR SET-SIZE`; tracks delete, escape and widget pools | `oxabl_flow` | Per-handle create, free and escape sites, with leaking paths |
| `oxabl_effects` | Per-procedure database effects: table, operation, lock, access shape (once, per-row, nested), likely index; propagated over the call graph | `oxabl_scope`, `oxabl_index`, `oxabl_schema` | Direct and transitive effect summaries |
| `oxabl_nulls` | Unknown-value dataflow from sources (`?`, `INITIAL ?`, failed `FIND NO-ERROR`, non-mandatory fields, function returns) to sinks | `oxabl_flow`, `oxabl_schema` | Possibly-unknown facts per expression |

Existing crates also change:

- `oxabl_index`: static call graph with `RUN` targets resolved over PROPATH; reads intent
  annotations and declaration files for legacy code.
- `oxabl_lint`: rule groups with per-directory enablement, severity config and a baseline file.
- `oxabl_analyze`: a new summaries section (below), additive with an envelope version bump.
- `oxabl_lsp`: hover and code lens showing each procedure's summary.

Any construct the analyses can't resolve (`RUN VALUE`, dynamic buffers, unresolved includes)
marks the summary **opaque** rather than guessing. This mirrors the conservative-fallback policy
of the dependency edge set.

## Rule catalog

Six opt-in groups, each enabled independently: `strict.transactions`, `strict.locks`,
`strict.access`, `strict.handles`, `strict.nulls`, `strict.calls`. A rule defaults to warning and
becomes an error only after its false-positive rate on a large real corpus is under 5%.

| Code | Rule | Detects | Autofix |
|---|---|---|---|
| TX001 | implicit-transaction | A database write in a block that becomes a transaction implicitly | No; suggest `DO TRANSACTION` |
| TX002 | procedure-wide-transaction | Transaction scope covering the whole procedure | No |
| TX003 | transaction-spans-ui | `WAIT-FOR`, `UPDATE`, `PROMPT-FOR`, `SET`, `PAUSE` inside a transaction | No |
| TX004 | transaction-spans-call | `RUN` inside a transaction where the callee's effects are unknown or opaque | No |
| TX005 | scope-widened | Buffer scope raised to the procedure by a free reference outside its block | No |
| TX006 | missing-no-undo | Variable or temp-table without `NO-UNDO` | Yes |
| LK001 | default-lock | `FIND`, `FOR EACH` or `CAN-FIND` without an explicit lock (defaults to `SHARE-LOCK`) | Yes, `NO-LOCK` when the buffer is never written in scope |
| LK002 | lock-outlives-transaction | Record still held `SHARE-LOCK` after its transaction ends because the buffer scope is wider | No |
| LK003 | lock-upgrade-in-loop | `NO-LOCK` read followed by `FIND CURRENT EXCLUSIVE-LOCK` on every iteration | No |
| DB001 | find-in-loop | `FIND` or `CAN-FIND` per iteration of a `FOR EACH` or query (N+1) | No |
| DB002 | whole-index-scan | `WHERE` clause not bracketed by any index | No |
| DB003 | order-from-index | `FOR EACH` without `BY` where the order is consumed (output, `FIRST-OF`, `FIRST`/`LAST`) | No |
| DB004 | missing-fields-list | Query without `FIELDS` in client/server code paths | Yes, from fields used in scope |
| HD001 | handle-not-deleted | Dynamic object not deleted on every path and not escaping | No |
| HD002 | delete-not-in-finally | `DELETE OBJECT` on the success path only | Yes, move into `FINALLY` |
| HD003 | persistent-procedure-leak | `RUN PERSISTENT` without a matching `DELETE PROCEDURE` or owner | No |
| HD004 | memptr-not-freed | `SET-SIZE` greater than zero without `SET-SIZE` 0 | No |
| HD005 | missing-widget-pool | Dynamic objects created with no widget pool scoped to the procedure | Yes |
| NL001 | unknown-in-comparison | Possibly-unknown value in a comparison or `IF` condition | No |
| NL002 | unknown-propagation | Possibly-unknown value in arithmetic or string concatenation | No |
| NL003 | field-after-failed-find | Buffer field used after `FIND NO-ERROR` without an `AVAILABLE` check | No |
| NL004 | nullable-db-field | Non-mandatory field used as if it can't be unknown | No |
| CL001 | run-value | `RUN VALUE(...)` | No |
| CL002 | dynamic-invoke | `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `CALL` object | No |
| CL003 | signature-mismatch | Static `RUN` arguments differ from callee parameters in count, type or mode | No |
| CL004 | unresolved-call | `RUN` target not found on PROPATH | No |
| CL005 | shared-state | `NEW SHARED` or `SHARED` variables, buffers, frames, streams | No |

Deferred: reference cycles between OO objects (ABL uses reference counting, so cycles leak). This
needs whole-program type flow and comes after the rest.

## Procedure summaries

Every procedure, internal procedure, function and method gets a summary answering "what does
this do?" without reading the code. It ships as a `summaries` section in
`oxabl analyze --format json`, a human-readable `oxabl explain <file>[:<procedure>]`, and an LSP
code lens such as "reads 3 tables · 1 transaction · per-row FIND on Item".

Each summary contains:

- **Signature:** parameters with type and mode.
- **Calls:** static, dynamic and unresolved, each with its target.
- **Effects:** per table, the operation (read, create, update, delete), lock mode, access shape
  (once, per-row, nested with depth), likely index, and whether the effect is direct or inherited
  from a callee.
- **Transactions:** each span with start and end lines, and whether it crosses UI or calls.
- **Handles:** created, freed and escaped, with leak paths.
- **Unknowns:** count of possibly-unknown sinks.
- **Opaque:** true when any dynamic construct makes the summary incomplete, with the reasons.

```json
{
  "procedure": "post-invoice",
  "effects": [
    { "table": "Item", "op": "read", "lock": "share", "shape": "per-row", "depth": 1, "via": "direct" },
    { "table": "InvLine", "op": "create", "lock": "exclusive", "shape": "per-row", "depth": 1, "via": "calc-tax.p" }
  ],
  "transactions": [{ "start": 42, "end": 118, "spans_ui": false, "spans_calls": ["calc-tax.p"] }],
  "handles": { "created": 2, "freed": 1, "leaks": [{ "line": 57, "path": "ON ERROR UNDO" }] },
  "opaque": false
}
```

## Strictness layers

Four layers, each adopted only after the one before it is in use. Layers 0 to 2 live in oxabl and
need no new syntax; layer 3 is strictabl.

1. **Layer 0, strict rules (oxabl):** the rule catalog above, run on unmodified code.
2. **Layer 1, intent annotations (oxabl):** ABL annotations such as
   `@strict.effects(reads="Customer", writes="Order").`, `@strict.nullable.` and `@strict.owns.`.
   oxabl checks the code against them; OpenEdge ignores them.
3. **Layer 2, declaration files (`oxabl_index`):** typed signatures and effects for procedures you
   can't edit (vendor or generated code), like TypeScript's `.d.ts`. They let strict code call
   legacy code safely.
4. **Layer 3, strictabl (separate front end):** a new file extension whose syntax OpenEdge would
   reject, compiled to plain, explicit ABL.

strictabl features, in priority order:

- Non-null by default, `integer?` for nullable.
- `using h = create query ...` ownership blocks, always emitting `FINALLY` + `DELETE OBJECT`.
- Effects in signatures, checked at every call site.
- Typed procedure references and dispatch tables in place of `RUN VALUE`.
- Module imports in place of PROPATH lookup.

Emitter requirements: deterministic, readable output; always explicit (`NO-UNDO`, lock modes,
`DO FOR` strong scope, `DO TRANSACTION`); a source map from emitted ABL lines back to strictabl
lines so OpenEdge errors point to the right place.

Crates: `strictabl_syntax` (parser extension behind a dialect flag, outside `oxabl_parser`'s
default path), `strictabl_check` (on the shared semantic and analysis crates), `strictabl_emit`,
and a `strictabl` CLI.

Entry gate: start layer 3 only after layers 0 to 2 run on a large real codebase and the places
where annotations proved clumsy are written down. Those notes define strictabl's syntax.

## Build order

Ordered by pain point (leaks first, then round trips, then unknowns), with cheap syntactic wins
up front. Each milestone ends with a run over a large real corpus.

1. **M1, foundation:** `oxabl_flow`; rule groups, severity config and baseline file in
   `oxabl_lint`; CL001, CL002, CL005 and TX006 (syntactic, no flow needed). Exit: CFGs for every
   corpus file with no panics.
2. **M2, leaks:** `oxabl_handles` and HD001 to HD005. Exit: leak count per directory reported;
   false positives triaged.
3. **M3, scopes:** `oxabl_scope`, then TX001 to TX005 and LK001 to LK003. Exit: the scope engine
   matches `COMPILE ... LISTING` output on a 1,000-file sample before any TX rule ships.
4. **M4, round trips:** `oxabl_effects`, call graph in `oxabl_index`, DB001 to DB004, summaries in
   `analyze`, `oxabl explain`, LSP code lens. Exit: DB002 agrees with XREF `WHOLE-INDEX` entries.
5. **M5, unknowns:** `oxabl_nulls` and NL001 to NL004.
6. **M6, intent:** annotations, declaration files, CL003 and CL004.
7. **M7, strictabl:** only after the layer 3 entry gate.

## Testing and guardrails

Every analysis is checked against the real OpenEdge compiler wherever it gives an answer, because
a wrong scope or effect summary is worse than none.

Oracles:

- `COMPILE ... LISTING` reports buffer scopes and transaction blocks: the ground truth for
  `oxabl_scope`.
- `XREF` reports table references and `WHOLE-INDEX` searches: the ground truth for
  `oxabl_effects` and DB002. One harness serves this and the dependency-edge XREF audit.

Tests:

- One fixture directory per rule, with positive and negative cases and snapshot-tested
  diagnostics.
- A regular run over a large real corpus tracking violation counts per rule; a rule is promoted to
  error only below 5% false positives.
- Whole-tree check time tracked alongside the existing benchmarks.

Rules for contributors (human or agent):

- Changes to the analyze envelope are additive only, with a version bump.
- No new syntax in `oxabl_parser`'s default path.
- When something can't be resolved, mark it opaque; never guess.
- Autofix only when provably semantics-preserving; otherwise suggest.
- Each rule gets docs with a bad example, a fixed example and the reason.
- Conventional Commits, as release-please expects.

## Open questions

- Which statements can ABL annotations legally precede? Answered in
  [`strict-analysis-open-questions.md`](strict-analysis-open-questions.md); needs a check against
  a licensed install.
- Does oxabl's PROPATH and include resolution exactly match the AVM's?
- Should diagnostics inside include files be reported in the include, at each use site, or both?
  A recommendation is in [`strict-analysis-open-questions.md`](strict-analysis-open-questions.md).
