//! Resolved, application-form lint configuration.
//!
//! This is the *leaf* half of the `[workspace.lint]` configuration surface
//! (KTD6). The user-facing config shape (`oxabl.toml`, kebab rule keys, the
//! `off | hint | info | warn | error` enum, glob overrides) lives in
//! `oxabl_workspace`; it is lowered into this value before reaching the
//! pipeline.
//!
//! Placing the *application* type here — in the crate every analysis layer
//! already depends on — lets `oxabl_lint` consume it without a
//! `oxabl_workspace → oxabl_lint` dependency cycle.
//!
//! # Layers and precedence
//!
//! A map holds a **base** [`LintLayer`] (the global `[workspace.lint]` table)
//! and any number of **scoped** layers, each guarded by glob patterns
//! (`[[workspace.lint.overrides]]`). [`LintSeverityMap::for_path`] picks the
//! scoped layers that match one file and returns a map that no longer depends
//! on the path.
//!
//! A rule's setting is then decided by walking the layers in order — base
//! first, then each matching scope in the order it was added — so a later
//! layer wins. Within one layer a setting for the rule itself (by code or by
//! name) beats a setting for its group, and `disable` beats `enable`.
//!
//! Enablement and severity are separate: a group toggle turns a rule on or off
//! but keeps the severity an earlier layer chose. Setting a rule to a severity
//! turns it on even when its group is off; setting it to *off* turns it off.

use std::path::{Path, PathBuf};

use crate::Severity;

/// One layer of lint settings: per-rule severities and group toggles.
///
/// Rule selectors are a diagnostic code (`LINT0002`) or a kebab rule name
/// (`unused-variable`), compared case-insensitively. Group selectors name a
/// group (`strict.calls`) or a dotted prefix of one (`strict` selects every
/// `strict.*` group).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LintLayer {
    /// Lower-cased selector → `None` for *off*, `Some(sev)` for on at `sev`.
    rules: Vec<(String, Option<Severity>)>,
    enable: Vec<String>,
    disable: Vec<String>,
}

impl LintLayer {
    /// An empty layer, which changes nothing.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a rule by code or name: `None` turns it off, `Some(sev)` turns it on
    /// and remaps every diagnostic it emits to `sev`. A later call for the same
    /// selector replaces an earlier one.
    pub fn set_rule(&mut self, selector: &str, severity: Option<Severity>) {
        let key = selector.trim().to_ascii_lowercase();
        match self.rules.iter_mut().find(|(k, _)| *k == key) {
            Some(entry) => entry.1 = severity,
            None => self.rules.push((key, severity)),
        }
    }

    /// Turn on every rule in the groups `selector` names.
    pub fn enable_group(&mut self, selector: &str) {
        self.enable.push(selector.trim().to_ascii_lowercase());
    }

    /// Turn off every rule in the groups `selector` names.
    pub fn disable_group(&mut self, selector: &str) {
        self.disable.push(selector.trim().to_ascii_lowercase());
    }

    /// True if the layer changes nothing.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty() && self.enable.is_empty() && self.disable.is_empty()
    }

    fn rule(&self, selector: &str) -> Option<Option<Severity>> {
        self.rules
            .iter()
            .find(|(k, _)| k.eq_ignore_ascii_case(selector))
            .map(|(_, s)| *s)
    }
}

/// Whether group selector `selector` (already lower-cased) names `group`:
/// equal, or a dotted prefix of it.
fn selects_group(selector: &str, group: &str) -> bool {
    let (group, selector) = (group.as_bytes(), selector.as_bytes());
    group.len() >= selector.len()
        && group[..selector.len()].eq_ignore_ascii_case(selector)
        && (group.len() == selector.len() || group[selector.len()] == b'.')
}

/// A [`LintLayer`] that applies only to files matching its patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
struct LintScope {
    root: PathBuf,
    patterns: Vec<glob::Pattern>,
    layer: LintLayer,
}

impl LintScope {
    fn matches(&self, path: &Path) -> bool {
        let Ok(relative) = path.strip_prefix(&self.root) else {
            return false;
        };
        let parts: Vec<&str> = relative
            .components()
            .filter_map(|c| match c {
                std::path::Component::Normal(s) => s.to_str(),
                _ => None,
            })
            .collect();
        // The file itself and each directory above it, so `legacy` and
        // `legacy/` select everything under that directory.
        (1..=parts.len()).rev().any(|n| {
            let candidate = parts[..n].join("/");
            self.patterns
                .iter()
                .any(|p| p.matches_with(&candidate, MATCH_OPTIONS))
        })
    }
}

/// Case-insensitive because ABL file names are case-insensitive on Windows and
/// a pattern that silently misses on case would quietly drop the override.
const MATCH_OPTIONS: glob::MatchOptions = glob::MatchOptions {
    case_sensitive: false,
    require_literal_separator: true,
    require_literal_leading_dot: false,
};

/// What [`LintSeverityMap::severity_for`] needs to know about one rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintRuleRef<'a> {
    /// The diagnostic code, for example `LINT0002`.
    pub code: &'a str,
    /// The kebab name, for example `unused-variable`.
    pub name: &'a str,
    /// The rule's group, or `None` when group toggles must not reach it.
    pub group: Option<&'a str>,
    /// Whether the rule runs when nothing configures it or its group.
    pub enabled_by_default: bool,
    /// The severity it reports at when nothing configures it.
    pub default_severity: Severity,
}

/// Resolved lint configuration: one base layer plus glob-scoped layers.
///
/// An empty map means every rule runs as its registry entry says by default.
/// The map is *not* tied to a rule registry: selectors are plain strings, so a
/// consumer-registered rule is configured the same way as a built-in one.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LintSeverityMap {
    base: LintLayer,
    /// Scopes not yet tested against a path.
    scopes: Vec<LintScope>,
    /// Scoped layers already selected by [`for_path`](Self::for_path).
    applied: Vec<LintLayer>,
}

impl LintSeverityMap {
    /// An empty map — every rule keeps its default.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a base-layer setting for `code` (or a rule name): `None` disables
    /// the rule, `Some(sev)` remaps every diagnostic it emits to `sev`.
    pub fn set(&mut self, code: &str, severity: Option<Severity>) {
        self.base.set_rule(code, severity);
    }

    /// The base layer, for adding group toggles or rule settings.
    pub fn base_mut(&mut self) -> &mut LintLayer {
        &mut self.base
    }

    /// Add a layer that applies to files matching any of `patterns`, after
    /// every layer added before it.
    ///
    /// Patterns are globs (`*`, `**`, `?`, `[...]`) relative to `root`, written
    /// with `/`, matched case-insensitively. A pattern that matches a directory
    /// selects every file under it. Returns the patterns that are not valid
    /// globs, each with the reason; those are dropped.
    pub fn add_scope(
        &mut self,
        root: &Path,
        patterns: &[String],
        layer: LintLayer,
    ) -> Vec<(String, String)> {
        let mut compiled = Vec::new();
        let mut rejected = Vec::new();
        for raw in patterns {
            let normalized = raw.trim().replace('\\', "/");
            let normalized = normalized.trim_start_matches("./").trim_end_matches('/');
            match glob::Pattern::new(normalized) {
                Ok(p) if !normalized.is_empty() => compiled.push(p),
                Ok(_) => rejected.push((raw.clone(), "empty pattern".to_string())),
                Err(e) => rejected.push((raw.clone(), e.to_string())),
            }
        }
        if !compiled.is_empty() {
            self.scopes.push(LintScope {
                root: root.to_path_buf(),
                patterns: compiled,
                layer,
            });
        }
        rejected
    }

    /// The settings that apply to the file at `path`: the scopes it matches are
    /// fixed in order, and the result no longer depends on a path.
    ///
    /// `path` is compared against each scope's root, so pass it in the same
    /// form (normally absolute). A path outside a scope's root matches none of
    /// its patterns.
    pub fn for_path(&self, path: &Path) -> LintSeverityMap {
        let mut applied = self.applied.clone();
        applied.extend(
            self.scopes
                .iter()
                .filter(|s| s.matches(path))
                .map(|s| s.layer.clone()),
        );
        LintSeverityMap {
            base: self.base.clone(),
            scopes: Vec::new(),
            applied,
        }
    }

    /// True if nothing is configured (all rules at their defaults).
    pub fn is_empty(&self) -> bool {
        self.base.is_empty() && self.scopes.is_empty() && self.applied.is_empty()
    }

    /// Look up the base-layer setting for a rule `code` (or name).
    ///
    /// Returns:
    /// - `None` — no setting; the rule uses its default;
    /// - `Some(None)` — the rule is off;
    /// - `Some(Some(sev))` — remap emitted diagnostics to `sev`.
    pub fn get(&self, code: &str) -> Option<Option<Severity>> {
        self.base.rule(code)
    }

    /// Whether the base layer leaves `code` enabled. Ignores groups and
    /// scopes; [`severity_for`](Self::severity_for) is the full answer.
    pub fn is_enabled(&self, code: &str) -> bool {
        !matches!(self.get(code), Some(None))
    }

    /// The base-layer severity for a diagnostic emitted at `default` under
    /// `code`. Ignores groups and scopes; [`severity_for`](Self::severity_for)
    /// is the full answer.
    pub fn effective(&self, code: &str, default: Severity) -> Severity {
        match self.get(code) {
            Some(Some(sev)) => sev,
            _ => default,
        }
    }

    /// The severity `rule` reports at, or `None` when it is off.
    ///
    /// Walks the base layer and then each applied scope; see the module docs
    /// for the precedence. Scopes not yet fixed by
    /// [`for_path`](Self::for_path) are ignored.
    pub fn severity_for(&self, rule: &LintRuleRef<'_>) -> Option<Severity> {
        let mut enabled = rule.enabled_by_default;
        let mut severity = rule.default_severity;
        for layer in std::iter::once(&self.base).chain(&self.applied) {
            if let Some(group) = rule.group {
                if layer.enable.iter().any(|s| selects_group(s, group)) {
                    enabled = true;
                }
                if layer.disable.iter().any(|s| selects_group(s, group)) {
                    enabled = false;
                }
            }
            match layer.rule(rule.code).or_else(|| layer.rule(rule.name)) {
                Some(None) => enabled = false,
                Some(Some(sev)) => {
                    enabled = true;
                    severity = sev;
                }
                None => {}
            }
        }
        enabled.then_some(severity)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const STRICT: LintRuleRef<'static> = LintRuleRef {
        code: "CL001",
        name: "run-value",
        group: Some("strict.calls"),
        enabled_by_default: false,
        default_severity: Severity::Warning,
    };

    const CORE: LintRuleRef<'static> = LintRuleRef {
        code: "LINT0002",
        name: "unused-variable",
        group: None,
        enabled_by_default: true,
        default_severity: Severity::Warning,
    };

    fn scoped(map: &mut LintSeverityMap, pattern: &str, layer: LintLayer) {
        let rejected = map.add_scope(Path::new("/ws"), &[pattern.to_string()], layer);
        assert!(rejected.is_empty(), "{rejected:?}");
    }

    #[test]
    fn empty_map_uses_defaults() {
        let m = LintSeverityMap::new();
        assert!(m.is_empty());
        assert_eq!(m.get("LINT0001"), None);
        assert!(m.is_enabled("LINT0001"));
        assert_eq!(m.effective("LINT0001", Severity::Error), Severity::Error);
        assert_eq!(m.severity_for(&CORE), Some(Severity::Warning));
        assert_eq!(m.severity_for(&STRICT), None);
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
        assert_eq!(m.severity_for(&CORE), Some(Severity::Info));
    }

    #[test]
    fn a_rule_is_selected_by_code_or_name_in_any_case() {
        let mut m = LintSeverityMap::new();
        m.set("Unused-Variable", None);
        assert_eq!(m.severity_for(&CORE), None);
        let mut m = LintSeverityMap::new();
        m.set("lint0002", Some(Severity::Error));
        assert_eq!(m.severity_for(&CORE), Some(Severity::Error));
    }

    #[test]
    fn a_group_or_its_prefix_turns_strict_rules_on() {
        for selector in ["strict.calls", "strict", "STRICT"] {
            let mut m = LintSeverityMap::new();
            m.base_mut().enable_group(selector);
            assert_eq!(
                m.severity_for(&STRICT),
                Some(Severity::Warning),
                "{selector}"
            );
        }
        let mut m = LintSeverityMap::new();
        m.base_mut().enable_group("strict.call");
        assert_eq!(m.severity_for(&STRICT), None, "not a dotted prefix");
    }

    #[test]
    fn a_rule_setting_beats_its_group_in_the_same_layer() {
        let mut m = LintSeverityMap::new();
        m.base_mut().enable_group("strict");
        m.set("run-value", None);
        assert_eq!(m.severity_for(&STRICT), None);

        let mut m = LintSeverityMap::new();
        m.set("CL001", Some(Severity::Error));
        assert_eq!(
            m.severity_for(&STRICT),
            Some(Severity::Error),
            "a severity turns the rule on with its group off"
        );
    }

    #[test]
    fn scopes_apply_only_to_matching_files() {
        let mut m = LintSeverityMap::new();
        let mut layer = LintLayer::new();
        layer.enable_group("strict.calls");
        scoped(&mut m, "src/new/**", layer);

        let inside = m.for_path(Path::new("/ws/src/new/a.p"));
        assert_eq!(inside.severity_for(&STRICT), Some(Severity::Warning));
        let outside = m.for_path(Path::new("/ws/src/old/a.p"));
        assert_eq!(outside.severity_for(&STRICT), None);
        let elsewhere = m.for_path(Path::new("/other/src/new/a.p"));
        assert_eq!(elsewhere.severity_for(&STRICT), None);
        assert_eq!(m.severity_for(&STRICT), None, "unresolved scopes are inert");
    }

    #[test]
    fn a_directory_pattern_selects_everything_under_it() {
        for pattern in ["src/new", "src/new/", "./src/new", "SRC/New", "src\\new"] {
            let mut m = LintSeverityMap::new();
            let mut layer = LintLayer::new();
            layer.set_rule("LINT0002", None);
            scoped(&mut m, pattern, layer);
            let resolved = m.for_path(Path::new("/ws/src/new/deep/a.p"));
            assert_eq!(resolved.severity_for(&CORE), None, "{pattern}");
        }
    }

    #[test]
    fn a_single_star_does_not_cross_directories() {
        let mut m = LintSeverityMap::new();
        let mut layer = LintLayer::new();
        layer.set_rule("LINT0002", None);
        scoped(&mut m, "src/*.p", layer);
        assert_eq!(
            m.for_path(Path::new("/ws/src/a.p")).severity_for(&CORE),
            None
        );
        assert_eq!(
            m.for_path(Path::new("/ws/src/sub/a.p")).severity_for(&CORE),
            Some(Severity::Warning)
        );
    }

    #[test]
    fn a_later_layer_wins_and_a_group_toggle_keeps_the_chosen_severity() {
        let mut m = LintSeverityMap::new();
        m.base_mut().enable_group("strict");
        m.set("CL001", Some(Severity::Error));
        let mut legacy = LintLayer::new();
        legacy.disable_group("strict");
        scoped(&mut m, "legacy/**", legacy);
        let mut revived = LintLayer::new();
        revived.enable_group("strict.calls");
        scoped(&mut m, "legacy/new/**", revived);

        let global = m.for_path(Path::new("/ws/src/a.p"));
        assert_eq!(global.severity_for(&STRICT), Some(Severity::Error));
        let legacy = m.for_path(Path::new("/ws/legacy/a.p"));
        assert_eq!(legacy.severity_for(&STRICT), None);
        let revived = m.for_path(Path::new("/ws/legacy/new/a.p"));
        assert_eq!(revived.severity_for(&STRICT), Some(Severity::Error));
    }

    #[test]
    fn group_toggles_do_not_reach_a_rule_without_a_group() {
        let mut m = LintSeverityMap::new();
        m.base_mut().disable_group("default");
        let core = LintRuleRef {
            group: None,
            ..CORE
        };
        assert_eq!(m.severity_for(&core), Some(Severity::Warning));
    }

    #[test]
    fn invalid_and_empty_patterns_are_reported_and_dropped() {
        let mut m = LintSeverityMap::new();
        let rejected = m.add_scope(
            Path::new("/ws"),
            &["[".to_string(), "".to_string()],
            LintLayer::new(),
        );
        assert_eq!(rejected.len(), 2);
        assert!(m.is_empty(), "a scope with no usable pattern is not added");
    }
}
