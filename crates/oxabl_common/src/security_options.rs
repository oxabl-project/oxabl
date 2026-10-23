//! Resolved options of the `security` lint rules: the stage each rule runs,
//! what the taint stage counts as a source and a sanitiser, and the entry
//! points that reachability starts from.
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

/// What an entry point runs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryTarget {
    /// A program, as a `RUN` names it, and optionally one of its internal
    /// procedures.
    Program {
        program: String,
        procedure: Option<String>,
    },
    /// A class, and optionally one of its methods. With no method, its
    /// constructors and every method that is not `PRIVATE`.
    Class {
        class: String,
        method: Option<String>,
    },
}

impl EntryTarget {
    /// The target as written: `web/orders.p`, `web/orders.p:get-order`,
    /// `app.OrderService:Create`.
    pub fn describe(&self) -> String {
        let (outer, inner) = match self {
            EntryTarget::Program { program, procedure } => (program, procedure),
            EntryTarget::Class { class, method } => (class, method),
        };
        match inner {
            Some(i) => format!("{outer}:{i}"),
            None => outer.clone(),
        }
    }
}

/// One remotely reachable entry point: a REST route, a WebSpeed program, a
/// service method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryPoint {
    /// Free text naming the channel: `REST`, `WebSpeed`, `SOAP`.
    pub kind: String,
    /// Free text naming the entry within it: `GET /orders/{id}`.
    pub name: String,
    pub target: EntryTarget,
    /// The framework authorises the caller before the entry runs.
    pub auth: bool,
}

impl EntryPoint {
    /// `REST GET /orders/{id}`.
    pub fn label(&self) -> String {
        match (self.kind.is_empty(), self.name.is_empty()) {
            (true, _) => self.name.clone(),
            (false, true) => self.kind.clone(),
            (false, false) => format!("{} {}", self.kind, self.name),
        }
    }
}

/// The options of entry-point reachability.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ReachOptions {
    /// An entry-point file is configured. The rules that need entry points
    /// are silent without one.
    pub configured: bool,
    pub entries: Vec<EntryPoint>,
    /// Routines and include files whose call (or inclusion) authorises the
    /// rest of a request.
    pub auth_routines: Vec<String>,
    /// Routines whose call is a sensitive operation.
    pub sensitive_calls: Vec<String>,
    /// A finding reachable from an entry point reports one level more severe.
    pub raise_reachable: bool,
    /// A finding no entry point can reach reports one level less severe.
    pub lower_unreachable: bool,
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
    /// Words that make a request key an identity (`user`, `company`, ...).
    /// Empty: the built-in list.
    pub identity_keys: Vec<String>,
    /// Project functions and methods whose result is an identity the client
    /// supplied.
    pub identity_sources: Vec<String>,
    pub reach: ReachOptions,
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
