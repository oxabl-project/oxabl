# Open items — #130 shipped

Planning artifact:
`docs/plans/2026-07-24-001-fix-credit-table-read-in-define-buffer-plan.md`

That plan is gitignored by the repository's point-in-time artifact policy. This
tracked file is its durable summary. #130 is now implemented; what remains below
is the follow-up work it deliberately did not do, plus one defect found on the
way.

## What shipped

Five forms name a table without reading a field of it, so nothing in the
expression walk ever saw them and the backing symbol's `read_count` stayed at
zero. That is what made LINT0002's table-parameter redirect report a `TABLE FOR
tt` parameter whose temp-table was used only that way.

| Form | How it credits now |
|---|---|
| `DEFINE BUFFER b FOR tt.` | Direct AST resolution of the target |
| `DEFINE PARAMETER BUFFER b FOR tt.` | Same, via `ParameterType::Buffer` |
| `EMPTY TEMP-TABLE tt.` | Marked `Skipped` carrying its exactly-parsed table name |
| `DEFINE QUERY q FOR tt.` | Marked `Skipped`, lexical harvest |
| `OPEN QUERY q FOR EACH tt.` | Marked `Skipped`, lexical harvest |

`StatementKind::Skipped` gained `may_reference_tables`. A marked node keeps
#128's value-namespace treatment unchanged and additionally resolves the same
names in `[Buffers, Tables]` as `AccessMode::Read`. The two paths are
independent because a token can resolve in both namespaces under shadowing, and
they record different facts: the value side records that counts cannot be
judged, the table side records a real read.

The buffer forms are guarded against the `DEFINE BUFFER Customer FOR Customer`
idiom — the declare pass has already bound the new buffer under that folded
name, so an unguarded lookup would credit the buffer a read for existing.

## Settled decisions, unchanged

- Fix the semantic fact at its source; do not weaken LINT0002's
  `backing_read_count`.
- `AccessMode::Read`, because the redirect sums `read_count`.
- Do not globally search table namespaces for every skipped statement. The
  marker bounds both behavior and cost to the three forms.
- Query candidates stay lexical until #136. Conservative over-credit can lose a
  diagnostic but cannot invent one.
- No temporary partial query AST variants that #136 would immediately replace.
- Do not comment on closed #128. #130 owns the table-specific extension.

## Found during implementation — needs a decision

**The parser accepted only the invalid spelling of a buffer parameter.** Buffer
parameters carry no direction in ABL, but only `DEFINE INPUT PARAMETER BUFFER b
FOR tt` parsed; the valid directionless `DEFINE PARAMETER BUFFER b FOR tt` was a
parse error. Fixed here, because R3's end-to-end pin is meaningless against a
source form real code never contains. Both spellings now produce the same node.

**`ParameterType::Buffer` records the wrong target in procedure signatures.**
`crates/oxabl_parser/src/parser/mod.rs:1567` — the inline parameter-list path
(`PROCEDURE p (BUFFER b FOR tt)`) parses the table name and then discards it with
`.ok()`, setting `target` to the *buffer's own name*. Two consequences: the
declare pass calls `schema_table_id` on the buffer name, so that shape never
links its schema table; and #130's new credit is skipped by the same-name guard,
so it credits nothing. Pre-existing, distinct from #130, and deliberately not
fixed here — it wants its own issue.

## Remaining product follow-ups

1. Decide whether unused buffer symbols deserve their own diagnostic. Crediting
   a buffer definition's target removes a table-parameter false positive, but a
   buffer that is bound and never used remains silent under the current rule set.
2. Bind synthesized schema default buffers into the scope model so statement
   table references can credit schema tables consistently. That is broader than
   #130 because it also changes `FIND` and `FOR EACH`. The boundary now has a
   test (`schema_only_targets_retain_current_no_credit_behavior`) rather than
   being folklore.
3. #136 retires the query approximation by head-parsing those forms, at which
   point `may_reference_tables` narrows to whatever is left.
