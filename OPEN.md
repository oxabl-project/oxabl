# Open questions — LINT0007 output dead-store advisory (#125)

Plan: `docs/plans/2026-07-24-001-feat-lint0007-output-dead-store-plan.md` — **not committed**, because `.gitignore` excludes `docs/plans/` by design (plans are point-in-time and get deleted once the work lands). It exists only in this worktree. Everything below is written to stand on its own if you read it somewhere the plan file isn't.

The plan is implementation-ready. Nothing below blocks starting it — each item is either a judgment call that only you should make, or a follow-up the plan deliberately defers. Delete this file once you have worked through it.

## Decided without you (flagging so you can reverse)

These came out of the review and were decisive enough to act on. Each is already reflected in the plan.

1. **Sequencing: #125 goes before #128/#130.** `STRATEGY.md` gates rule value on false-positive hardening, and `HANDOFF.md` ranks the uncredited-reads work above this issue, so this was a real fork. A Fable pass called it decisively for going now: LINT0007 ships dormant behind the opt-in tier, LINT0006 already ships *on* with the identical read blindness, so waiting removes no false positive anyone currently sees. Reverse this if you'd rather keep the handoff ordering intact.
2. **The opt-in tier is durable, not a one-off.** KTD3 now states the membership criterion (the compiler doesn't warn *and* the population is idiomatic) and records that `LintSeverityMap::is_enabled` is not authoritative for the tier — #57's rule-selection registry has to consult the gate instead.
3. **Scope stays `OUTPUT`-on-`RUN` only**, with the boundary stated in the diagnostic's own help text so a class-heavy codebase seeing zero hits reads it as scoped rather than broken.

## Genuinely yours

4. **Is anyone actually asking for this rule?** The only demand signal is the issue itself — no user report, no dogfood hit. The plan says so plainly in the Problem Frame and mitigates by shipping off-by-default, but if the honest answer is "nobody", the cheaper move is to close #125 as won't-do rather than spend six units and a permanent config key on it. This is the one question that could make the whole plan moot, which is why it's here rather than decided.

5. **Should the unresolvable-`{include}` case suppress every never-read rule, not just this one?** An unexpanded include hides readers from LINT0002, LINT0006, and LINT0007 alike. The plan accepts LINT0007 firing there (R3b) and pins it with a test, because `PREPROC007` already fires loudly at the true cause. But a shared "don't run never-read rules on a file that emitted `PREPROC007`" gate is defensible and would belong to its own issue covering all three rules. Not in scope here either way.

6. **Landing strategy.** The plan proposes one PR, commit-per-unit, with U3+U4 sharing a commit (split apart, clippy fails on dead code). Splitting U5 out as a standalone config-plumbing PR was considered and rejected because it would advertise a config key for a rule that doesn't exist yet. Override if you prefer smaller PRs.

## Follow-ups the plan deliberately defers

7. **The mixed-write dead store is unreported by anything.** For `x = 1. RUN p (OUTPUT x).` with no read, all three rules stay silent: LINT0002 bails on `write_count > 0`, LINT0006 bails on the `PASSED_AS_OUTPUT_ARG` exemption regardless of the symbol's other writes, and LINT0007 bails on `write_count != output_arg_write_count`. The original plan wrongly claimed LINT0006 owned this; the review caught it. Closing the gap means narrowing LINT0006's exemption, which changes an on-by-default warning rule — **worth its own issue.**

8. **Write-back parameters on function and method calls.** `PASSED_AS_OUTPUT_ARG` is populated only from the `StatementKind::Run` arm, so the rule's real coverage is one statement form. Widening the model helps LINT0002 and LINT0006 as much as LINT0007. Own issue.

9. **`INPUT-OUTPUT` coverage.** Dropped from the trigger because `InputOutput` maps to `ReadWrite` and bumps `read_count`, so "never read" is unsatisfiable for it by construction. Making it reachable means tracking output-argument *reads* symmetrically — a deliberate widening of what counts as a read. Worth raising with the issue author rather than folding in silently.

10. **`output_arg_write_count` is stopgap bookkeeping.** It joins `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE` in the family #126's CFG def-use records are meant to retire. The plan records this in Scope Boundaries and requires the field's doc comment to say it, so #126's scope picks it up. Check that LINT0007's predicate survives #126's rewrite when you get there.
