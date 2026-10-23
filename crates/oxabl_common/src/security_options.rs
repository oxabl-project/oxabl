//! Resolved options of the `security` lint rules: the stage each rule runs,
//! and what the taint stage counts as a source and a sanitiser.
//!
//! The user-facing shape is `[workspace.lint.security]` in `oxabl.toml`
//! (`oxabl_workspace`). It is lowered into this value and carried on
//! [`LintSeverityMap`](crate::LintSeverityMap), so every client that passes
//! lint settings to the rules passes these too.

/// How a security rule decides that a sink is a finding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SecurityMode {
    /// Stage 1: every sink whose argument is not a literal.
    #[default]
    Sink,
    /// Stage 2: a sink that a value from an untrusted source can reach, inside
    /// one procedure, without a sanitiser for that sink.
    Taint,
}

/// Which input parameters the taint stage counts as untrusted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ParameterSources {
    /// Parameters of a unit that code outside the file can call: the main
    /// block, a procedure or function that is not `PRIVATE`, a method or
    /// constructor that is not `PRIVATE`.
    #[default]
    External,
    /// Every input parameter.
    All,
    /// No parameter.
    None,
}

/// A project function that validates or cleans a value for some sink kinds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SecurityValidator {
    /// Function or method name, compared case-insensitively.
    pub name: String,
    /// Sink kinds it protects, lower-cased (`query`, `command`, ..., or
    /// `all`). The lint crate rejects a kind it does not know.
    pub protects: Vec<String>,
}

/// The options of the security rules.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SecurityOptions {
    /// The mode of every rule that [`modes`](Self::modes) does not name.
    pub mode: SecurityMode,
    /// Per-rule modes, keyed by lower-cased code or kebab name.
    pub modes: Vec<(String, SecurityMode)>,
    pub parameters: ParameterSources,
    /// Whether a database field read is an untrusted source.
    pub database_fields: bool,
    /// Project functions and methods whose result is untrusted.
    pub sources: Vec<String>,
    pub validators: Vec<SecurityValidator>,
}

impl SecurityOptions {
    /// The mode of the rule with this code and name.
    pub fn mode_for(&self, code: &str, name: &str) -> SecurityMode {
        self.modes
            .iter()
            .rev()
            .find(|(k, _)| k.eq_ignore_ascii_case(code) || k.eq_ignore_ascii_case(name))
            .map_or(self.mode, |(_, m)| *m)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_rule_mode_beats_the_global_mode() {
        let options = SecurityOptions {
            mode: SecurityMode::Taint,
            modes: vec![("path-traversal".into(), SecurityMode::Sink)],
            ..SecurityOptions::default()
        };
        assert_eq!(
            options.mode_for("ABLSEC005", "path-traversal"),
            SecurityMode::Sink
        );
        assert_eq!(
            options.mode_for("ABLSEC001", "dynamic-query"),
            SecurityMode::Taint
        );
        assert_eq!(
            SecurityOptions::default().mode_for("ABLSEC001", "dynamic-query"),
            SecurityMode::Sink
        );
    }
}
