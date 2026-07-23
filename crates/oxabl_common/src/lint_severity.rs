//! Resolved, application-form lint severity overrides.
//!
//! This is the *leaf* half of the `[lint]` configuration surface (KTD6). The
//! user-facing config shape (`oxabl.toml [lint]`, kebab rule keys, the
//! `off | hint | info | warn | error` enum) lives in `oxabl_workspace`; it is
//! lowered into this plain code→severity map before reaching the pipeline.
//!
//! Placing the *application* type here — in the crate every analysis layer
//! already depends on — lets `oxabl_lint` consume it without a
//! `oxabl_workspace → oxabl_lint` dependency cycle.
//!
//! Semantics of a lookup for a lint code:
//! - absent (`None`) → the rule keeps its built-in default severity;
//! - present as `Some(None)` → the rule is **off** (dropped entirely);
//! - present as `Some(Some(sev))` → the rule runs, but every diagnostic it
//!   emits is remapped to `sev`.

use std::collections::HashMap;

use crate::Severity;

/// A resolved map from lint diagnostic code (e.g. `"LINT0001"`) to an optional
/// severity override. `None` for a code means *off*; `Some(sev)` remaps the
/// rule's emitted severity. A code with no entry uses its built-in default.
///
/// The map is intentionally keyed by the `&'static str` diagnostic code rather
/// than the kebab rule name so the pipeline can look up by
/// [`DiagnosticCode`](crate::DiagnosticCode) directly, and so a future rule
/// registry (#57) can populate it without a name→code translation table.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LintSeverityMap {
    overrides: HashMap<&'static str, Option<Severity>>,
}

impl LintSeverityMap {
    /// An empty map — every rule keeps its built-in default severity.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record an override for `code`: `None` disables the rule, `Some(sev)`
    /// remaps every diagnostic it emits to `sev`.
    pub fn set(&mut self, code: &'static str, severity: Option<Severity>) {
        self.overrides.insert(code, severity);
    }

    /// True if no override is configured (all rules at their defaults).
    pub fn is_empty(&self) -> bool {
        self.overrides.is_empty()
    }

    /// Look up the configured disposition for a lint `code`.
    ///
    /// Returns:
    /// - `None` — no override; the rule uses its built-in default;
    /// - `Some(None)` — the rule is off;
    /// - `Some(Some(sev))` — remap emitted diagnostics to `sev`.
    pub fn get(&self, code: &str) -> Option<Option<Severity>> {
        self.overrides.get(code).copied()
    }

    /// Whether the rule identified by `code` is enabled (default or remapped).
    /// A code explicitly set to *off* returns `false`; everything else `true`.
    pub fn is_enabled(&self, code: &str) -> bool {
        !matches!(self.overrides.get(code), Some(None))
    }

    /// The effective severity for a diagnostic already emitted at `default`
    /// under `code`: the override if one is configured (and not off), else the
    /// diagnostic's own `default`. Callers should have already dropped off
    /// rules via [`is_enabled`](Self::is_enabled) before emitting.
    pub fn effective(&self, code: &str, default: Severity) -> Severity {
        match self.overrides.get(code) {
            Some(Some(sev)) => *sev,
            _ => default,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_map_uses_defaults() {
        let m = LintSeverityMap::new();
        assert!(m.is_empty());
        assert_eq!(m.get("LINT0001"), None);
        assert!(m.is_enabled("LINT0001"));
        assert_eq!(m.effective("LINT0001", Severity::Error), Severity::Error);
    }

    #[test]
    fn off_disables_rule() {
        let mut m = LintSeverityMap::new();
        m.set("LINT0001", None);
        assert_eq!(m.get("LINT0001"), Some(None));
        assert!(!m.is_enabled("LINT0001"));
    }

    #[test]
    fn remap_changes_severity() {
        let mut m = LintSeverityMap::new();
        m.set("LINT0002", Some(Severity::Info));
        assert!(m.is_enabled("LINT0002"));
        assert_eq!(m.effective("LINT0002", Severity::Warning), Severity::Info);
    }
}
