//! The fail-closed rules: why a unit has no compile key.

use std::collections::BTreeMap;

use rustc_hash::{FxHashMap, FxHashSet};

use super::compile_key::EntryDigest;
use crate::kinds::{EdgeKind, degraded_reason, unresolved_reason};
use crate::paths::propath::is_dlc;
use crate::refresh::{Content, Outcome};
use crate::store::Unresolved;

/// What the worker reported about one PROPATH entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolved {
    Known(EntryDigest),
    /// A library entry that is a directory: its bytes have no digest.
    Unhashable,
}

/// One `[build.external_packages]` row: a class name prefix, lower-cased, and
/// the library entries that supply it.
#[derive(Debug, Clone)]
pub struct ExternalPackage {
    pub prefix: String,
    pub libraries: Vec<String>,
}

impl ExternalPackage {
    pub fn lower(packages: &BTreeMap<String, Vec<String>>) -> Vec<ExternalPackage> {
        packages
            .iter()
            .map(|(prefix, libraries)| ExternalPackage {
                prefix: prefix.to_ascii_lowercase(),
                libraries: libraries.clone(),
            })
            .collect()
    }
}

/// A class name that K3 accepted because a hashed library supplies it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Accepted {
    pub name: String,
    pub entry: String,
}

/// The rules apply to one unit at a time with this around them.
pub struct Rules<'a> {
    pub outcome: &'a Outcome,
    pub external: &'a [ExternalPackage],
    /// Why every unit is unkeyable, from the environment.
    pub environment_unknown: &'a [String],
    pub digests: &'a FxHashMap<String, Resolved>,
    /// The `$DLC/...` closure files whose worker copy differs from the host
    /// mirror.
    pub mirror_mismatch: &'a FxHashSet<String>,
    pub class_sources: &'a FxHashSet<u32>,
    /// Whether units have more than one distinct PROPATH.
    pub several_propaths: bool,
}

/// The verdict on one unit.
#[derive(Debug, Default)]
pub struct Verdict {
    /// One list per rule K1 to K9, each sorted.
    reasons: [Vec<String>; 9],
    /// The class names K3 accepted in the unit's closure.
    pub accepted: Vec<Accepted>,
}

impl Verdict {
    /// Adds a reason under rule `rule` (1 to 9).
    pub fn add_reason(&mut self, rule: usize, reason: String) {
        self.reasons[rule - 1].push(reason);
    }

    /// Adds a K8 reason, when the closure holds an accepted name that a
    /// workspace class also has.
    pub fn add_conflict(&mut self, name: &str) {
        self.add_reason(8, format!("class-conflict: {name}"));
    }

    /// Every reason, by rule, each rule's reasons sorted without repeats.
    pub fn into_reasons(self) -> Vec<String> {
        self.reasons
            .into_iter()
            .flat_map(|mut list| {
                list.sort();
                list.dedup();
                list
            })
            .collect()
    }

    pub fn is_keyable(&self) -> bool {
        self.reasons.iter().all(Vec::is_empty)
    }
}

fn degraded_name(code: u8) -> &'static str {
    match code {
        degraded_reason::PARSE_RECOVERED => "parse-recovered",
        degraded_reason::UNRESOLVED_INCLUDE => "unresolved-include",
        degraded_reason::UNREADABLE_INCLUDE => "unreadable-include",
        degraded_reason::PREPROCESSOR_LOSS => "preprocessor-loss",
        degraded_reason::UNMODELLED_STATEMENT => "unmodelled-statement",
        _ => "analysis-failed",
    }
}

fn is_computed(row: &Unresolved) -> bool {
    row.reason == unresolved_reason::COMPUTED_NAME || row.name.contains(['{', '&'])
}

impl Rules<'_> {
    /// The library entry on `propath` that supplies the class `name`, when a
    /// hashed library does. No prefix is trusted by name alone.
    fn supplier(&self, name: &str, propath: &[String]) -> Option<String> {
        let lower = name.to_ascii_lowercase();
        let hashed = |entry: &String| {
            matches!(
                self.digests.get(entry),
                Some(Resolved::Known(EntryDigest::Library(_)))
            )
        };
        self.external
            .iter()
            .filter(|package| lower.starts_with(&package.prefix))
            .flat_map(|package| &package.libraries)
            .find_map(|library| {
                propath
                    .iter()
                    .filter(|entry| match library.as_str() {
                        "$DLC" => is_dlc(entry),
                        _ => *entry == library,
                    })
                    .find(|entry| hashed(entry))
                    .cloned()
            })
    }

    fn check_references(
        &self,
        verdict: &mut Verdict,
        path: &str,
        unresolved: &[Unresolved],
        propath: &[String],
    ) {
        for row in unresolved {
            let name = &row.name;
            match row.kind {
                EdgeKind::DirectInclude
                | EdgeKind::TransitiveInclude
                | EdgeKind::ConditionalInclude => {
                    let (rule, label) = match is_computed(row) {
                        true => (4, "computed-name"),
                        false => (2, "unresolved-include"),
                    };
                    verdict.add_reason(rule, format!("{label}: {name} in {path}"));
                }
                EdgeKind::Class | EdgeKind::ConditionalClass => {
                    if is_computed(row) {
                        verdict.add_reason(4, format!("computed-name: {name} in {path}"));
                        continue;
                    }
                    let external = matches!(
                        row.reason,
                        unresolved_reason::ABSENT_FROM_WORKSPACE | unresolved_reason::EXTERNAL
                    );
                    match external.then(|| self.supplier(name, propath)).flatten() {
                        Some(entry) => verdict.accepted.push(Accepted {
                            name: name.clone(),
                            entry,
                        }),
                        None => {
                            verdict.add_reason(3, format!("unresolved-class: {name} in {path}"))
                        }
                    }
                }
                EdgeKind::Program | EdgeKind::SharedProducer | EdgeKind::ConditionalProgram => {}
            }
        }
    }

    fn check_file(&self, verdict: &mut Verdict, id: u32) {
        let path = self.outcome.name(id);
        if path.starts_with('/') {
            verdict.add_reason(5, format!("outside-workspace: {path}"));
        } else if !matches!(self.outcome.content_of(id), Content::Hash(_)) {
            verdict.add_reason(5, format!("unreadable: {path}"));
        }
        if path.starts_with("$DLC/") && self.mirror_mismatch.contains(path) {
            verdict.add_reason(6, format!("dlc-mirror-mismatch: {path}"));
        }
    }

    /// Applies K1 to K6 and K9 to `unit`, whose closure is `closure`, with
    /// `propath` as its search path.
    pub fn check(&self, unit: u32, closure: &[u32], propath: &[String]) -> Verdict {
        let mut verdict = Verdict::default();
        let mut reaches_class = false;
        for node in std::iter::once(unit).chain(closure.iter().copied()) {
            self.check_file(&mut verdict, node);
            reaches_class |= node != unit && self.class_sources.contains(&node);
            let Some(analysis) = self.outcome.analyses.get(node) else {
                continue;
            };
            let path = self.outcome.name(node);
            for code in &analysis.completeness {
                verdict.add_reason(1, format!("degraded: {path}: {}", degraded_name(*code)));
            }
            self.check_references(&mut verdict, path, &analysis.unresolved, propath);
        }
        for entry in propath.iter().filter(|entry| entry.starts_with('/')) {
            verdict.add_reason(5, format!("outside-workspace: {entry}"));
        }
        for reason in self.environment_unknown {
            verdict.add_reason(6, reason.clone());
        }
        for entry in propath {
            if self.digests.get(entry) == Some(&Resolved::Unhashable) {
                verdict.add_reason(6, format!("unhashable-library: {entry}"));
            }
        }
        if self.several_propaths && (reaches_class || self.class_sources.contains(&unit)) {
            verdict.add_reason(9, "per-unit-propath-classes".to_string());
        }
        verdict
    }
}
