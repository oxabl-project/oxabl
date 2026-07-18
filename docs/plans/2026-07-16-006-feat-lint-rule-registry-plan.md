---
title: "feat: Public lint rule registry and selection API"
type: feat
status: ready
date: 2026-07-16
origin: GitHub #57
branch: feat/lint-rule-registry
---

# feat: Public lint rule registry and selection API

## Problem Statement

`oxabl_lint` ships four built-in rules composed by a hardcoded `lint_file`
dispatcher. Downstream consumers cannot:

1. Register custom rules that run alongside built-ins through one entry point.
2. Enable/disable a subset of rules without reimplementing the dispatcher.
3. Enumerate rules with metadata (code, name, description, severity, stability)
   for CLI `--list`, docs, and config validation.

Individual rule modules are re-exported, so a consumer *can* call each `run` by
hand — but there is no trait, registry, or enumeration.

## Goals

- **G1.** A `Rule` trait (or equivalent object-safe surface) with metadata + run.
- **G2.** A `RuleSet` that enumerates built-ins, accepts consumer rules, and
  runs a selected subset.
- **G3.** Keep `lint_file` as "run all built-ins" convenience — zero break for
  current callers.
- **G4.** Metadata enumerable without executing rules.

## Non-Goals

- Full config file schema for rule options (future; thread via
  `AnalysisContext` when it lands).
- Auto-discovery of rules via inventory/linkme plugins.
- Changing diagnostic codes of existing rules.
- Merging AST walks for throughput (see perf plan) — orthogonal; registry
  should not prevent a later shared walker.

## Design

### Trait sketch

```rust
pub trait Rule: Send + Sync {
    fn meta(&self) -> RuleMeta;
    fn run(
        &self,
        program: &[Statement],
        sem: &Semantic,
        ctx: &AnalysisContext<'_>,
    ) -> Vec<Diagnostic>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuleMeta {
    pub code: &'static str,          // "LINT0001"
    pub name: &'static str,          // "undefined-symbol"
    pub description: &'static str,
    pub default_severity: Severity,
    pub stability: RuleStability,    // Stable | Experimental
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleStability { Stable, Experimental }
```

### Built-in wrappers

Each existing free function becomes a unit struct implementing `Rule`:

```rust
pub struct UndefinedSymbol;
impl Rule for UndefinedSymbol {
    fn meta(&self) -> RuleMeta { /* LINT0001 ... */ }
    fn run(...) -> Vec<Diagnostic> { undefined_symbol::run(...) }
}
```

Keep the free functions `pub` so existing direct callers stay valid.

### RuleSet

```rust
pub struct RuleSet {
    rules: Vec<Box<dyn Rule>>,
}

impl RuleSet {
    pub fn builtins() -> Self;                 // LINT0001..=0004
    pub fn empty() -> Self;
    pub fn register(&mut self, rule: impl Rule + 'static);
    pub fn with(mut self, rule: impl Rule + 'static) -> Self;
    pub fn select_by_code(&self, codes: &[&str]) -> RuleSet;
    pub fn disable(&self, codes: &[&str]) -> RuleSet;
    pub fn iter_meta(&self) -> impl Iterator<Item = RuleMeta> + '_;
    pub fn run(&self, program, sem, ctx) -> Vec<Diagnostic>;
}

pub fn lint_file(...) -> Vec<Diagnostic> {
    RuleSet::builtins().run(program, sem, ctx)
}
```

Selection is by rule **code** (`LINT0001`) primarily; name alias optional.
Unknown codes in `select_by_code` / `disable` should return a clear error type
or be ignored with a documented choice — prefer **hard error on unknown code**
for config validation.

### Ordering

Stable order: registration order. Built-ins keep LINT0001 → LINT0004.
Custom rules append after built-ins unless the consumer builds a custom set.

### Object safety note

If `Rule` cannot be object-safe cleanly, use an enum for built-ins + a
`Custom(Box<dyn Rule>)` variant, or store `fn` pointers + static meta. Prefer
trait + `Box<dyn Rule>` for the extension story the issue asks for.

## Implementation Steps

1. Add `RuleMeta`, `RuleStability`, `Rule` trait in `crates/oxabl_lint/src/lib.rs`
   (or `rule.rs`).
2. Wrap four built-ins; re-export meta constants next to `LINT0001` etc.
3. Implement `RuleSet` + make `lint_file` a thin wrapper.
4. Tests:
   - `builtins().iter_meta()` returns exactly 4, stable order.
   - `disable(&["LINT0002"])` runs 3 rules; unused vars not reported.
   - Custom rule registers and fires.
   - Unknown code in `select_by_code` errors (or documents ignore).
   - Existing `lint_file` tests still pass unchanged.
5. Docs: crate-level rustdoc example of custom rule + subset selection.
6. Optional CLI follow-up (separate PR): `oxabl lint --list-rules` once a
   dedicated lint subcommand exists. Not required to close #57.

## Risk

**Low–medium.** Purely additive API. Watch for:

- Breaking change if free-function signatures were changed (they should not be).
- `AnalysisContext` lifetime in trait method — keep exact current signature.
- Object-safety friction with generics — solve with `Box<dyn Rule>`.

## Effort

~1–2 days including tests and docs.

## Why it matters now

The next product focus is **lint accuracy + ecosystem**. Custom domain rules
(ERP-specific naming, buffer lock policy, etc.) are a primary reason to build
on `oxabl_lint` rather than reimplement. Registry also unblocks future
`oxabl.toml` rule toggles without another API redesign.
