# Open questions — #130 post-#128 plan

Planning artifact:
`docs/plans/2026-07-24-001-fix-credit-table-read-in-define-buffer-plan.md`

That plan is gitignored by the repository's point-in-time artifact policy.
This tracked file is its durable summary.

## What issue #128 changed

#128 shipped through PR #137. Recognized-but-unmodelled statements now produce:

```rust
StatementKind::Skipped { names }
```

The resolve pass best-effort-resolves those names only in
`NamespaceId::Values` and applies
`TOUCHED_BY_UNMODELLED_STATEMENT`. It deliberately does not increment read or
write counts and does not consult `Buffers` or `Tables`.

That is the right contract for #128, but it leaves every table-specific #130
case open:

| Form | Post-#128 state |
|---|---|
| `DEFINE BUFFER b FOR tt.` | Real AST node; resolve use-walk is still a no-op |
| `DEFINE PARAMETER BUFFER b FOR tt.` | Real AST node; resolve use-walk is still a no-op |
| `EMPTY TEMP-TABLE tt.` | Still hand-walked to `StatementKind::Empty`; table name discarded |
| `DEFINE QUERY q FOR tt.` | Now `Skipped`; names harvested, but never offered to table namespaces |
| `OPEN QUERY q FOR EACH tt.` | Now `Skipped`; names harvested, but never offered to table namespaces |

The old decision to leave the bottom three to #128 is retired.

## Rescoped implementation

1. Keep direct AST-backed resolution for `DEFINE BUFFER` and
   `DEFINE PARAMETER BUFFER`, including the same-name guard for
   `DEFINE BUFFER Customer FOR Customer`.
2. Extend `StatementKind::Skipped` with a narrow
   `may_reference_tables: bool` marker.
3. Keep ordinary #128 forms unmarked. Only `DEFINE QUERY`, `OPEN QUERY`, and
   `EMPTY TEMP-TABLE` opt into table lookup.
4. Preserve `EMPTY TEMP-TABLE`'s exactly parsed table identifier instead of
   returning `Empty`.
5. For marked nodes, retain #128's `Values` flagging and additionally resolve
   the same candidates in `[Buffers, Tables]` as `AccessMode::Read`.
6. Leave full query head parsing to #136 and skipped modelled-statement tails to
   #134.

This gets #128 and #130 to complement one another: #128 says ordinary value
counts are unjudgeable inside skipped forms; #130 supplies the concrete backing
table read count needed by the table-parameter redirect.

## Settled decisions

- Fix the semantic fact at its source; do not weaken LINT0002's
  `backing_read_count`.
- `AccessMode::Read` is required because the redirect sums `read_count`.
- Do not globally search table namespaces for every skipped statement. The
  marker bounds both behavior and cost to the three #130 forms.
- Keep query candidates lexical until #136. Conservative over-credit can lose a
  diagnostic but cannot invent one.
- Do not add temporary partial query AST variants that #136 would immediately
  replace.
- Do not comment on closed #128; update #130 with the corrected representation
  and completion evidence when the implementation lands.
- Schema-table/default-buffer synthesis remains out of scope.
- `DEFINE PARAMETER BUFFER` remains included: it is the same defect, the same
  direct fix, and no other issue owns it.

## Remaining product follow-ups

1. Decide whether unused buffer symbols deserve their own diagnostic. Crediting
   a buffer definition's target removes a table-parameter false positive, but a
   buffer that is bound and never used remains silent under the current rule set.
2. Bind synthesized schema default buffers into the scope model so statement
   table references can credit schema tables consistently. That is broader than
   #130 because it also changes `FIND` and `FOR EACH`.

Neither follow-up blocks #130.
