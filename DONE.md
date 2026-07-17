# DONE — #58 residual LINT0001 gaps

Branch: `feat/lint0001-residual-gaps`

## Shipped
- A: Full system-handle seed list + parser Kind allowlist for handle position
- E1: Kind::No boolean literal
- E3: SUBSTR in builtin registry (codegen)
- C: QUERY/BUFFER handle name-only parse
- D: Receiver soften NotInScope → External
- B: Property set_parameters AST + parse + declare
- Tests: lint0001_residual_gaps.rs (9 tests)

## Review notes
Deepseek agent landed system handles then timed out mid-E1. Finished A–E locally.
Workspace tests + clippy clean.

## Deferred
Ambient GLOBAL SHARED / super-procedure (~61% unique) — multi-file work.
