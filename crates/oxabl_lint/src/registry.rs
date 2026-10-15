//! The rule registry: rule metadata, rule groups, and the dispatcher that runs
//! an enabled subset under a [`LintSeverityMap`].

use std::fmt;
use std::sync::OnceLock;

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, LintRuleRef, LintSeverityMap, Severity};
use oxabl_handles::FileHandles;
use oxabl_scope::FileScopes;
use oxabl_semantic::{AnalysisContext, Semantic};

use crate::rules::{
    CL001, CL002, CL005, HD001, HD002, HD003, HD004, HD005, LINT0001, LINT0002, LINT0003, LINT0004,
    LINT0005, LINT0006, LK001, LK002, LK003, TX001, TX002, TX003, TX004, TX005, TX006,
    assigned_but_never_read, block_var_used_outside, default_lock, delete_not_in_finally,
    dynamic_invoke, handle_not_deleted, implicit_transaction, lock_outlives_transaction,
    lock_upgrade_in_loop, memptr_not_freed, missing_no_undo, missing_widget_pool,
    persistent_procedure_leak, procedure_wide_transaction, run_value, scope_widened, shared_state,
    transaction_spans_call, transaction_spans_ui, type_mismatch_assignment, undefined_symbol,
    unknown_table_or_field, unused_variable,
};
use crate::shared::RunShared;

/// Names of the built-in rule groups.
pub mod groups {
    /// The always-on group holding the original `LINT` rules.
    pub const DEFAULT: &str = "default";
    /// Transaction scope rules (`TX`).
    pub const STRICT_TRANSACTIONS: &str = "strict.transactions";
    /// Record lock rules (`LK`).
    pub const STRICT_LOCKS: &str = "strict.locks";
    /// Database access shape rules (`DB`).
    pub const STRICT_ACCESS: &str = "strict.access";
    /// Dynamic object lifetime rules (`HD`).
    pub const STRICT_HANDLES: &str = "strict.handles";
    /// Unknown-value rules (`NL`).
    pub const STRICT_NULLS: &str = "strict.nulls";
    /// Call and shared-state rules (`CL`).
    pub const STRICT_CALLS: &str = "strict.calls";
}

/// How settled a rule is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Stability {
    /// Behaviour and code are settled.
    Stable,
    /// May change behaviour, or false-positive more than a stable rule.
    Preview,
}

/// A rule's metadata: everything about it except how it runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub struct RuleMeta {
    /// Diagnostic code, for example `LINT0002` or `CL001`.
    pub code: &'static str,
    /// Kebab-case name, for example `unused-variable`.
    pub name: &'static str,
    /// The group the rule belongs to.
    pub group: &'static str,
    /// One-line description of what the rule reports.
    pub description: &'static str,
    /// The severity every diagnostic of the rule is reported at when nothing
    /// configures it.
    pub default_severity: Severity,
    /// How settled the rule is.
    pub stability: Stability,
    /// Whether the rule needs the whole compilation unit, so it is skipped on
    /// an include fragment opened without its includer.
    pub requires_compilation_unit: bool,
    /// Whether the rule's answer depends only on the text of the file a
    /// finding sits in, not on the code around an include. A context-free
    /// finding inside an include can be reported once, in the include. That
    /// text is expanded with no arguments and no includer defines, so a finding
    /// on a statement that an includer can change is lower confidence
    /// ([`fragment`](crate::fragment)).
    pub context_free: bool,
    /// Whether a finding anchored in an include that the include's own run
    /// cannot produce is reported at the `{...}` site in the includer, instead
    /// of being dropped. The rule's answer needs the includer: it creates,
    /// names or frees an object that the include alone does not see.
    pub use_site_in_includer: bool,
}

impl RuleMeta {
    /// Metadata for a stable rule reporting at `Warning` that runs on every
    /// file. Adjust with the builder methods.
    pub const fn new(
        code: &'static str,
        name: &'static str,
        group: &'static str,
        description: &'static str,
    ) -> Self {
        RuleMeta {
            code,
            name,
            group,
            description,
            default_severity: Severity::Warning,
            stability: Stability::Stable,
            requires_compilation_unit: false,
            context_free: false,
            use_site_in_includer: false,
        }
    }

    /// Set the default severity.
    pub const fn with_default_severity(mut self, severity: Severity) -> Self {
        self.default_severity = severity;
        self
    }

    /// Set the stability.
    pub const fn with_stability(mut self, stability: Stability) -> Self {
        self.stability = stability;
        self
    }

    /// Mark the rule as context-free.
    pub const fn context_free(mut self) -> Self {
        self.context_free = true;
        self
    }

    /// Mark the rule's findings in an include as reportable at the use site
    /// when the include's own run cannot produce them.
    pub const fn reported_at_use_site(mut self) -> Self {
        self.use_site_in_includer = true;
        self
    }

    /// Mark the rule as needing the whole compilation unit.
    pub const fn requiring_compilation_unit(mut self) -> Self {
        self.requires_compilation_unit = true;
        self
    }
}

/// A lint rule: metadata plus a run over one analysed file.
///
/// The run receives the same inputs as every built-in rule function. It
/// reports at any severity; the registry remaps every diagnostic to the
/// configured or default severity.
pub trait Rule: Send + Sync {
    /// The rule's metadata.
    fn meta(&self) -> &RuleMeta;

    /// Run the rule over `program` and its semantic model.
    fn run(&self, program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic>;

    /// Whether the rule reads the dynamic objects of the file
    /// ([`RunShared::handles`]). The default is `false`.
    fn reads_handles(&self) -> bool {
        false
    }

    /// Whether the rule reads the buffer scopes of the file
    /// ([`RunShared::scopes`]). When a rule over the dynamic objects runs
    /// before such a rule, the run builds the CFGs of every unit once, for
    /// both. The default is `false`.
    fn reads_scopes(&self) -> bool {
        false
    }

    /// [`run`](Self::run) with the facts that one registry run shares between
    /// rules. The default ignores them.
    fn run_shared(
        &self,
        program: &[Statement],
        sem: &Semantic,
        ctx: &AnalysisContext,
        _shared: &RunShared,
    ) -> Vec<Diagnostic> {
        self.run(program, sem, ctx)
    }
}

/// The signature of a rule written as a plain function.
pub type RuleFn = fn(&[Statement], &Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// The signature of a rule that reads the lifetimes of dynamic objects
/// ([`oxabl_handles`]) and nothing else of the analysis.
pub type HandlesFn = fn(&FileHandles, &AnalysisContext) -> Vec<Diagnostic>;

/// The signature of a rule that reads the buffer scopes, transaction blocks
/// and record locks of the file ([`oxabl_scope`]), with the statements they
/// were found in.
pub type ScopesFn = fn(&FileScopes, &[Statement], &AnalysisContext) -> Vec<Diagnostic>;

#[derive(Debug, Clone, Copy)]
enum Body {
    Plain(RuleFn),
    Handles(HandlesFn),
    Scopes(ScopesFn),
}

/// A [`Rule`] made of metadata and a plain function — how every built-in rule
/// is registered.
#[derive(Debug, Clone, Copy)]
pub struct FnRule {
    meta: RuleMeta,
    body: Body,
}

impl FnRule {
    /// Pair `meta` with `run`.
    pub const fn new(meta: RuleMeta, run: RuleFn) -> Self {
        FnRule {
            meta,
            body: Body::Plain(run),
        }
    }

    /// Pair `meta` with a query over the file's dynamic objects. One registry
    /// run analyses the file once for every such rule.
    pub const fn over_handles(meta: RuleMeta, check: HandlesFn) -> Self {
        FnRule {
            meta,
            body: Body::Handles(check),
        }
    }

    /// Pair `meta` with a query over the file's buffer scopes, transactions
    /// and locks. One registry run analyses the file once for every such rule.
    pub const fn over_scopes(meta: RuleMeta, check: ScopesFn) -> Self {
        FnRule {
            meta,
            body: Body::Scopes(check),
        }
    }
}

impl Rule for FnRule {
    fn meta(&self) -> &RuleMeta {
        &self.meta
    }

    fn run(&self, program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
        self.run_shared(program, sem, ctx, &RunShared::default())
    }

    fn reads_handles(&self) -> bool {
        matches!(self.body, Body::Handles(_))
    }

    fn reads_scopes(&self) -> bool {
        matches!(self.body, Body::Scopes(_))
    }

    fn run_shared(
        &self,
        program: &[Statement],
        sem: &Semantic,
        ctx: &AnalysisContext,
        shared: &RunShared,
    ) -> Vec<Diagnostic> {
        match self.body {
            Body::Plain(run) => run(program, sem, ctx),
            Body::Handles(check) => check(shared.handles(program, ctx), ctx),
            Body::Scopes(check) => check(shared.scopes(program, ctx), program, ctx),
        }
    }
}

/// A rule group: a set of rules enabled or disabled together.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub struct RuleGroup {
    /// Dotted group name, for example `strict.calls`.
    pub name: &'static str,
    /// One-line description.
    pub description: &'static str,
    /// Whether the group's rules run when the configuration does not mention
    /// the group.
    pub enabled_by_default: bool,
    /// Whether `enable`/`disable` can switch the group. An always-on group's
    /// rules can still be turned off one by one.
    pub toggleable: bool,
}

impl RuleGroup {
    /// An opt-in group: off until enabled.
    pub const fn opt_in(name: &'static str, description: &'static str) -> Self {
        RuleGroup {
            name,
            description,
            enabled_by_default: false,
            toggleable: true,
        }
    }

    /// An always-on group.
    pub const fn always_on(name: &'static str, description: &'static str) -> Self {
        RuleGroup {
            name,
            description,
            enabled_by_default: true,
            toggleable: false,
        }
    }
}

/// The built-in groups, in documentation order.
pub const BUILTIN_GROUPS: &[RuleGroup] = &[
    RuleGroup::always_on(groups::DEFAULT, "Core correctness rules; always on."),
    RuleGroup::opt_in(
        groups::STRICT_TRANSACTIONS,
        "Implicit and over-wide transactions.",
    ),
    RuleGroup::opt_in(groups::STRICT_LOCKS, "Default and long-lived record locks."),
    RuleGroup::opt_in(
        groups::STRICT_ACCESS,
        "Per-row database access and index use.",
    ),
    RuleGroup::opt_in(groups::STRICT_HANDLES, "Leaked dynamic objects."),
    RuleGroup::opt_in(groups::STRICT_NULLS, "Unknown values reaching sinks."),
    RuleGroup::opt_in(groups::STRICT_CALLS, "Dynamic calls and shared state."),
];

/// The built-in rules, in run order.
pub const BUILTIN_RULES: &[FnRule] = &[
    FnRule::new(
        RuleMeta::new(
            LINT0001,
            "undefined-symbol",
            groups::DEFAULT,
            "A name that resolves to nothing in scope.",
        )
        .with_default_severity(Severity::Error),
        undefined_symbol::run,
    ),
    FnRule::new(
        RuleMeta::new(
            LINT0002,
            "unused-variable",
            groups::DEFAULT,
            "A variable or parameter that nothing references.",
        )
        .requiring_compilation_unit(),
        unused_variable::run,
    ),
    FnRule::new(
        RuleMeta::new(
            LINT0003,
            "unknown-table-or-field",
            groups::DEFAULT,
            "A table, field or sequence the loaded schema does not declare.",
        ),
        unknown_table_or_field::run,
    ),
    FnRule::new(
        RuleMeta::new(
            LINT0004,
            "type-mismatch-assignment",
            groups::DEFAULT,
            "An assignment whose value type does not fit the target.",
        ),
        type_mismatch_assignment::run,
    ),
    FnRule::new(
        RuleMeta::new(
            LINT0005,
            "block-var-used-outside",
            groups::DEFAULT,
            "A block-defined variable read outside its block before it is set.",
        )
        .with_default_severity(Severity::Info)
        .requiring_compilation_unit(),
        block_var_used_outside::run,
    ),
    FnRule::new(
        RuleMeta::new(
            LINT0006,
            "assigned-but-never-read",
            groups::DEFAULT,
            "A variable that is written but never read.",
        )
        .requiring_compilation_unit(),
        assigned_but_never_read::run,
    ),
    strict_rule(
        CL001,
        run_value::NAME,
        run_value::GROUP,
        run_value::DESCRIPTION,
        run_value::DEFAULT_SEVERITY,
        run_value::run,
    ),
    strict_rule(
        CL002,
        dynamic_invoke::NAME,
        dynamic_invoke::GROUP,
        dynamic_invoke::DESCRIPTION,
        dynamic_invoke::DEFAULT_SEVERITY,
        dynamic_invoke::run,
    ),
    strict_rule(
        CL005,
        shared_state::NAME,
        shared_state::GROUP,
        shared_state::DESCRIPTION,
        shared_state::DEFAULT_SEVERITY,
        shared_state::run,
    ),
    strict_rule(
        TX006,
        missing_no_undo::NAME,
        missing_no_undo::GROUP,
        missing_no_undo::DESCRIPTION,
        missing_no_undo::DEFAULT_SEVERITY,
        missing_no_undo::run,
    ),
    handles_rule(
        HD001,
        handle_not_deleted::NAME,
        handle_not_deleted::GROUP,
        handle_not_deleted::DESCRIPTION,
        handle_not_deleted::DEFAULT_SEVERITY,
        handle_not_deleted::check,
    ),
    handles_rule(
        HD002,
        delete_not_in_finally::NAME,
        delete_not_in_finally::GROUP,
        delete_not_in_finally::DESCRIPTION,
        delete_not_in_finally::DEFAULT_SEVERITY,
        delete_not_in_finally::check,
    ),
    handles_rule(
        HD003,
        persistent_procedure_leak::NAME,
        persistent_procedure_leak::GROUP,
        persistent_procedure_leak::DESCRIPTION,
        persistent_procedure_leak::DEFAULT_SEVERITY,
        persistent_procedure_leak::check,
    ),
    handles_rule(
        HD004,
        memptr_not_freed::NAME,
        memptr_not_freed::GROUP,
        memptr_not_freed::DESCRIPTION,
        memptr_not_freed::DEFAULT_SEVERITY,
        memptr_not_freed::check,
    ),
    handles_rule(
        HD005,
        missing_widget_pool::NAME,
        missing_widget_pool::GROUP,
        missing_widget_pool::DESCRIPTION,
        missing_widget_pool::DEFAULT_SEVERITY,
        missing_widget_pool::check,
    ),
    scopes_rule(
        LK001,
        default_lock::NAME,
        default_lock::GROUP,
        default_lock::DESCRIPTION,
        default_lock::DEFAULT_SEVERITY,
        default_lock::check,
    ),
    scopes_rule(
        LK002,
        lock_outlives_transaction::NAME,
        lock_outlives_transaction::GROUP,
        lock_outlives_transaction::DESCRIPTION,
        lock_outlives_transaction::DEFAULT_SEVERITY,
        lock_outlives_transaction::check,
    ),
    scopes_rule(
        LK003,
        lock_upgrade_in_loop::NAME,
        lock_upgrade_in_loop::GROUP,
        lock_upgrade_in_loop::DESCRIPTION,
        lock_upgrade_in_loop::DEFAULT_SEVERITY,
        lock_upgrade_in_loop::check,
    ),
    scopes_rule(
        TX001,
        implicit_transaction::NAME,
        implicit_transaction::GROUP,
        implicit_transaction::DESCRIPTION,
        implicit_transaction::DEFAULT_SEVERITY,
        implicit_transaction::check,
    ),
    scopes_rule(
        TX002,
        procedure_wide_transaction::NAME,
        procedure_wide_transaction::GROUP,
        procedure_wide_transaction::DESCRIPTION,
        procedure_wide_transaction::DEFAULT_SEVERITY,
        procedure_wide_transaction::check,
    ),
    scopes_rule(
        TX003,
        transaction_spans_ui::NAME,
        transaction_spans_ui::GROUP,
        transaction_spans_ui::DESCRIPTION,
        transaction_spans_ui::DEFAULT_SEVERITY,
        transaction_spans_ui::check,
    ),
    scopes_rule(
        TX004,
        transaction_spans_call::NAME,
        transaction_spans_call::GROUP,
        transaction_spans_call::DESCRIPTION,
        transaction_spans_call::DEFAULT_SEVERITY,
        transaction_spans_call::check,
    ),
    scopes_rule(
        TX005,
        scope_widened::NAME,
        scope_widened::GROUP,
        scope_widened::DESCRIPTION,
        scope_widened::DEFAULT_SEVERITY,
        scope_widened::check,
    ),
];

/// A syntactic strict rule: context-free, and in preview until its
/// false-positive rate on a real corpus is measured.
const fn strict_rule(
    code: &'static str,
    name: &'static str,
    group: &'static str,
    description: &'static str,
    severity: Severity,
    run: RuleFn,
) -> FnRule {
    FnRule::new(
        RuleMeta::new(code, name, group, description)
            .with_default_severity(severity)
            .with_stability(Stability::Preview)
            .context_free(),
        run,
    )
}

/// A strict rule over the lifetimes of dynamic objects: context-free and in
/// preview, like [`strict_rule`].
const fn handles_rule(
    code: &'static str,
    name: &'static str,
    group: &'static str,
    description: &'static str,
    severity: Severity,
    check: HandlesFn,
) -> FnRule {
    FnRule::over_handles(
        RuleMeta::new(code, name, group, description)
            .with_default_severity(severity)
            .with_stability(Stability::Preview)
            .context_free()
            .reported_at_use_site(),
        check,
    )
}

/// A strict rule over buffer scopes, transactions and locks: routed like
/// [`handles_rule`], because its answer can depend on the includer.
const fn scopes_rule(
    code: &'static str,
    name: &'static str,
    group: &'static str,
    description: &'static str,
    severity: Severity,
    check: ScopesFn,
) -> FnRule {
    FnRule::over_scopes(
        RuleMeta::new(code, name, group, description)
            .with_default_severity(severity)
            .with_stability(Stability::Preview)
            .context_free()
            .reported_at_use_site(),
        check,
    )
}

/// Why a rule or group could not be registered.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum RegistryError {
    /// The code or name is already taken by another rule.
    DuplicateRule(String),
    /// The group name is already registered.
    DuplicateGroup(String),
    /// The rule names a group that is not registered.
    UnknownGroup {
        /// The rule's code.
        rule: String,
        /// The group it names.
        group: String,
    },
}

impl fmt::Display for RegistryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegistryError::DuplicateRule(s) => write!(f, "rule `{s}` is already registered"),
            RegistryError::DuplicateGroup(s) => write!(f, "group `{s}` is already registered"),
            RegistryError::UnknownGroup { rule, group } => {
                write!(f, "rule `{rule}` names unregistered group `{group}`")
            }
        }
    }
}

impl std::error::Error for RegistryError {}

/// A set of rules and groups, and the dispatcher that runs them.
///
/// [`Registry::builtin`] holds every built-in rule; [`register`](Self::register)
/// adds a consumer rule beside them. Codes and names are unique across the
/// registry, compared case-insensitively.
pub struct Registry {
    groups: Vec<RuleGroup>,
    rules: Vec<Box<dyn Rule>>,
}

impl fmt::Debug for Registry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Registry")
            .field("groups", &self.groups)
            .field(
                "rules",
                &self.rules.iter().map(|r| r.meta().code).collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl Registry {
    /// The built-in groups and no rules.
    pub fn empty() -> Self {
        Registry {
            groups: BUILTIN_GROUPS.to_vec(),
            rules: Vec::new(),
        }
    }

    /// Every built-in group and rule.
    pub fn builtin() -> Self {
        let mut registry = Self::empty();
        for rule in BUILTIN_RULES {
            registry
                .register(*rule)
                .expect("built-in rules are unique and name built-in groups");
        }
        registry
    }

    /// A shared [`Registry::builtin`], built once per process.
    pub fn builtin_shared() -> &'static Registry {
        static BUILTIN: OnceLock<Registry> = OnceLock::new();
        BUILTIN.get_or_init(Registry::builtin)
    }

    /// Add a group.
    pub fn register_group(&mut self, group: RuleGroup) -> Result<(), RegistryError> {
        if self.group(group.name).is_some() {
            return Err(RegistryError::DuplicateGroup(group.name.to_string()));
        }
        self.groups.push(group);
        Ok(())
    }

    /// Add a rule. It runs after every rule registered before it.
    pub fn register(&mut self, rule: impl Rule + 'static) -> Result<(), RegistryError> {
        self.register_boxed(Box::new(rule))
    }

    /// Add an already-boxed rule.
    pub fn register_boxed(&mut self, rule: Box<dyn Rule>) -> Result<(), RegistryError> {
        let meta = *rule.meta();
        for selector in [meta.code, meta.name] {
            if self.rule(selector).is_some() {
                return Err(RegistryError::DuplicateRule(selector.to_string()));
            }
        }
        if self.group(meta.group).is_none() {
            return Err(RegistryError::UnknownGroup {
                rule: meta.code.to_string(),
                group: meta.group.to_string(),
            });
        }
        self.rules.push(rule);
        Ok(())
    }

    /// Every rule, in run order.
    pub fn rules(&self) -> impl Iterator<Item = &dyn Rule> {
        self.rules.iter().map(|r| &**r)
    }

    /// Every group, built-in first.
    pub fn groups(&self) -> &[RuleGroup] {
        &self.groups
    }

    /// The rule with this code or name, compared case-insensitively.
    pub fn rule(&self, selector: &str) -> Option<&dyn Rule> {
        let selector = selector.trim();
        self.rules().find(|r| {
            r.meta().code.eq_ignore_ascii_case(selector)
                || r.meta().name.eq_ignore_ascii_case(selector)
        })
    }

    /// The group with this exact name, compared case-insensitively.
    pub fn group(&self, name: &str) -> Option<&RuleGroup> {
        self.groups
            .iter()
            .find(|g| g.name.eq_ignore_ascii_case(name.trim()))
    }

    /// The groups a configuration selector names: the group itself, or every
    /// group under a dotted prefix (`strict` names every `strict.*` group).
    pub fn groups_selected_by(&self, selector: &str) -> Vec<&RuleGroup> {
        let selector = selector.trim().to_ascii_lowercase();
        self.groups
            .iter()
            .filter(|g| {
                let name = g.name.to_ascii_lowercase();
                name == selector
                    || name
                        .strip_prefix(&selector)
                        .is_some_and(|rest| rest.starts_with('.'))
            })
            .collect()
    }

    /// The severity `rule` reports at under `map`, or `None` when it is off.
    pub fn severity_for(&self, rule: &dyn Rule, map: &LintSeverityMap) -> Option<Severity> {
        let meta = rule.meta();
        let group = self.group(meta.group);
        map.severity_for(&LintRuleRef {
            code: meta.code,
            name: meta.name,
            group: group.filter(|g| g.toggleable).map(|g| g.name),
            enabled_by_default: group.is_none_or(|g| g.enabled_by_default),
            default_severity: meta.default_severity,
        })
    }

    /// Run every rule that `ctx.lint_severities` enables, in run order.
    pub fn run(
        &self,
        program: &[Statement],
        sem: &Semantic,
        ctx: &AnalysisContext,
    ) -> Vec<Diagnostic> {
        self.run_where(program, sem, ctx, |_| true)
    }

    /// Run the rules that both `select` accepts and `ctx.lint_severities`
    /// enables, in run order.
    ///
    /// Each rule's diagnostics are remapped to its resolved severity. A rule
    /// that needs the whole compilation unit is skipped on an include fragment.
    pub fn run_where(
        &self,
        program: &[Statement],
        sem: &Semantic,
        ctx: &AnalysisContext,
        select: impl Fn(&RuleMeta) -> bool,
    ) -> Vec<Diagnostic> {
        let fragment = ctx.source_context.is_include_fragment();
        let enabled = |rule: &dyn Rule| {
            let meta = rule.meta();
            if !select(meta) || (fragment && meta.requires_compilation_unit) {
                return None;
            }
            self.severity_for(rule, &ctx.lint_severities)
        };
        let mut shared = RunShared::default();
        let mut decided = false;
        let mut out = Vec::new();
        for (i, rule) in self.rules().enumerate() {
            let Some(severity) = enabled(rule) else {
                continue;
            };
            // The first rule over the dynamic objects asks whether the
            // scopes will be analysed too.
            if !decided && rule.reads_handles() {
                decided = true;
                let scopes = self
                    .rules()
                    .skip(i + 1)
                    .any(|r| r.reads_scopes() && enabled(r).is_some());
                shared.share_flow(scopes);
            }
            out.extend(
                rule.run_shared(program, sem, ctx, &shared)
                    .into_iter()
                    .map(|mut d| {
                        d.severity = severity;
                        d
                    }),
            );
        }
        out
    }
}
