# Open questions — #130 planning

Planning artifact: `docs/plans/2026-07-24-001-fix-credit-table-read-in-define-buffer-plan.md`

That path is gitignored (`.gitignore:6`, the point-in-time artifact policy), so it is **local to this worktree only**. Everything below is the durable summary. If the worktree is discarded before the work lands, this file is what survives.

## What changed about the issue's premise

#130 lists four ABL forms and asserts all of them "are parsed and do have AST nodes." Only one is:

| Form | Reality |
|---|---|
| `DEFINE BUFFER b FOR tt.` | Real AST node; `crates/oxabl_semantic/src/resolve.rs:1281` is a genuine no-op arm. **Fixable here.** |
| `EMPTY TEMP-TABLE tt.` | `crates/oxabl_parser/src/parser/statements.rs:528-544` hand-walks the tokens and **discards the table name** → `StatementKind::Empty`. |
| `DEFINE QUERY q FOR tt.` | `skip_to_statement_end()` → `Empty`. |
| `OPEN QUERY q FOR EACH tt` | `Kind::Open` bulk arm at `statements.rs:608,641` → `skip_to_statement_end()` → `Empty`. |

The bottom three carry no table name past parsing, so there is no resolve arm to fix — they are #128's territory by #128's own definition. Scope was narrowed accordingly (user-directed, in session).

## Decisions already settled — do not re-litigate

1. **Scope: `DEFINE BUFFER` only in this plan.** Rejected: holding #130 until #128 lands; planning all four forms sequenced behind #128. Rationale: the `DEFINE BUFFER` half has zero dependency on #128, while `DEFINE QUERY`/`OPEN QUERY` both route through `skip_to_statement_end()` and will be fixed free by #128's lexical fallback. Planning them here would duplicate parser work and put two branches in the same skip list.

2. **`DEFINE PARAMETER BUFFER b FOR tt.` is folded in as R6** — a fifth form neither issue mentions, found in review. It has a real AST node (`crates/oxabl_ast/src/statement.rs:1011-1014`), the same no-op defect at `resolve.rs:1271`, and the same one-line fix. It is **structurally unreachable by #128** (never routes through `skip_to_statement_end`), so no issue would own it. Verified independently, then confirmed decisive by a Fable pass. Strike R6 + its U1 bullet if you disagree — it is labeled as a post-settlement addition in the plan.

3. **`AccessMode::Read`, not `Write`.** `backing_read_count` (`crates/oxabl_lint/src/rules/unused_variable.rs:163-185`) sums `read_count` only, so `Write` would leave the false positive in place.

4. **Fix in `resolve.rs`, not in the lint rule.** Widening `backing_read_count`'s `Some(0)` case into the silent path would delete the true positive that `fires_on_table_parameter_whose_table_is_never_referenced` pins.

## Genuinely open — needs your call

1. **Should the plan doc be durable?** It is gitignored by policy, so it exists only in this worktree. If you want it to survive, the natural home is a comment on #130. I did not post it — that is outward-facing and you only authorized a branch push.

2. **The buffer-warning follow-up needs an issue.** This change trades a false positive for a false negative: after it lands, a procedure that takes `TABLE FOR ttItem`, binds `DEFINE BUFFER bItem FOR ttItem.`, and touches neither is silent in **every** rule (today LINT0002 reports it). `is_candidate` (`crates/oxabl_lint/src/rules/unused_symbol_shared.rs:22-24`) excludes `Buffer`/`TempTable`, so nothing can pick it back up. The trade is right — that shape is rare, the false positive fires on ordinary code — but "should an unused buffer warn?" is now the only recovery path, not a nice-to-have. Worth its own issue.

3. **Schema-table targets are still never credited, and that is pre-existing.** `synth_table_buffer_symbol` (`resolve.rs:2386-2412`) inserts into the `SymbolTable` but never binds into the `ScopeTree`, and nothing anywhere declares into `NamespaceId::Tables`. So `DEFINE BUFFER bCust FOR Customer.` under a loaded schema credits nothing — exactly as `FIND Customer` does today. Fixing it means giving statement-position table references the same default-buffer synthesis the expression path has: a semantic-model change with visible `oxabl analyze` consequences, and it would fix `FIND`/`FOR EACH` too. Out of scope here; worth an issue.

## Things to tell #128 (scheduled as U3 in the plan)

- **`EMPTY TEMP-TABLE` will be missed by avenue 2.** It does not call `skip_to_statement_end()` — `statements.rs:528-544` hand-walks with `self.advance()`. A lexical read-crediting fallback hung off `skip_to_statement_end` will silently skip it. `DEFINE QUERY` and `OPEN QUERY` *do* route through it and are covered.
- **Double-credit boundary.** Keep the fallback confined to statements that actually reach `StatementKind::Empty`. If it ends up token-scoped instead, `DEFINE BUFFER` targets get credited twice once both changes land.

## Review state

Four reviewers ran (coherence, feasibility, scope-guardian, adversarial). Coherence: clean. The other three produced 12 findings; all were applied. The load-bearing ones — each verified against source before applying:

- The original plan claimed a schema-table target would be credited. It cannot be. Corrected to assert the opposite.
- `KTD3`'s justification for including `NamespaceId::Tables` was invented — nothing declares into it, and `TABLE FOR` params land in `NamespaceId::Values` as `SymbolKind::Parameter` (`resolve.rs:679-696`). Rewritten to the honest reason (sibling-arm consistency).
- `run_full` (`resolve.rs:4167-4178`) discards both diagnostic vectors, so the R5 no-diagnostics test must call `declare_pass`/`resolve_pass` directly.
- **`DEFINE BUFFER Customer FOR Customer.`** — the standard ABL block-scoping idiom — would self-credit without a guard, since the declare pass already put the buffer in `Buffers` under that folded name. The plan now requires a folded-name-equality guard. This is the one finding where two reviewers proposed different remedies (accept-and-document vs. guard); the guard won because this idiom is common and is the only database-table shape that resolves to anything at all.
