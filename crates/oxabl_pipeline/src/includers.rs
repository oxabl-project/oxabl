//! Which roots of a walk include a file, and whether each leaves the dynamic
//! objects that the file creates with no widget pool.
//!
//! An include fragment is checked alone, so `missing-widget-pool` (HD005) cannot
//! know whether an includer provides the pool. [`IncluderPools`] answers that
//! for a walk that checked every root, under one conservative rule: the finding
//! is dropped only when the full set of files that can include the file is
//! known, and every one of them is a root analysed in this run that gives the
//! objects a pool. Any doubt keeps the finding as the fragment run gave it.
//!
//! # What the answer says
//!
//! [`IncluderPools::judge`] changes the HD005 findings of one fragment:
//!
//! - every includer is a root that gives the objects a pool: the finding is
//!   dropped;
//! - the includers are known, and some give none: the finding stays and its
//!   help names those roots instead of saying that the includer was not seen;
//! - anything else: the finding stays unchanged.
//!
//! # How the set is established
//!
//! A root includes the file when its expansion reaches it. A file *names* the
//! file when an include reference in its text, `{name ...}`, has the file's base
//! name (case-insensitive). The set is established when all of these hold:
//!
//! - the walk starts at the directory of `oxabl.toml`, and every include path
//!   lies below it;
//! - the roots that reach the file are exactly the roots that name it. This
//!   rules out an include reached through another include, an inactive `&IF`
//!   branch, and a computed name;
//! - no other file of the workspace directory names it. Every file is read:
//!   include files, files with another extension (trigger files), files in a
//!   hidden directory, notes. The `oxabl.toml`, the baseline file and binary
//!   files (a NUL byte in the first 4 KiB, unless the file starts with a UTF-16
//!   mark) are not read. A file over 8 MiB that is not binary, or that the
//!   configured encoding cannot decode, cannot be read and keeps every finding;
//! - no file that is not a root has an include whose name is computed
//!   (`{{&n}.i}`) or longer than 1 KiB, and the workspace holds no symbolic
//!   link;
//! - no root that a file which is not a root names has a computed include
//!   name, which that file can steer to any include;
//! - every root was read, preprocessed and parsed without errors, and no include
//!   of any root failed to resolve, could not be read or did not arrive;
//! - every root that reaches the file creates at least one of the objects that
//!   the fragment reports.
//!
//! Includers outside the workspace directory, such as other applications on the
//! `PROPATH`, are not seen. The analysis is built once, by
//! [`IncluderPools::prepare`], before the walk starts its own workers.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use oxabl_common::{Diagnostic, catch_panic};
use oxabl_flow::FlowOptions;
use oxabl_index::search::normalize_lexically;
use oxabl_lint::fragment::{FRAGMENT_NOTE, uncovered_includers_note};
use oxabl_lint::missing_widget_pool;

use oxabl_analyze::{CollectedDiagnostic, DiagnosticSource};

use crate::{LintPipeline, LintResult, ROOT_FILE_ID};

/// What a root's own analysis says about the objects that one include
/// creates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Creates {
    /// The root saw no object created by the include.
    None,
    /// The root saw objects, and gives all of them a pool or lets them escape.
    Pooled,
    /// The root counts at least one object with no pool.
    Unpooled,
}

/// What one root says.
enum Root {
    /// The root could not be read, preprocessed or analysed.
    Failed,
    Done {
        /// Each wanted include the root expands, with what the root creates
        /// from it.
        includes: HashMap<PathBuf, Creates>,
        /// The wanted includes whose name the root's text holds.
        named: HashSet<PathBuf>,
        /// The root parsed with errors, so a statement may be missing.
        recovered: bool,
        /// An include of the root did not arrive, or an include file lies
        /// outside the workspace.
        gap: bool,
        /// The root's text has an include whose name is computed, which an
        /// includer that is not a root can steer to any file.
        computed: bool,
    },
}

/// A root that expands an include.
struct Includer {
    root: PathBuf,
    creates: Creates,
    recovered: bool,
}

/// The merged answers.
#[derive(Default)]
struct Facts {
    includers: HashMap<PathBuf, Vec<Includer>>,
    /// The roots whose text names each include.
    named_by_roots: HashMap<PathBuf, HashSet<PathBuf>>,
    /// Includes that a file which is not a root names.
    named_elsewhere: HashSet<PathBuf>,
    /// The lower-case names that every file which is not a root writes in an
    /// include reference. A root in this set may be included by an unseen file.
    names_elsewhere: HashSet<String>,
    /// Some root or file gave no answer, or the set cannot be established.
    blind: bool,
}

/// What the roots of a walk say about one include.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PoolVerdict {
    /// The includer set is not established.
    Unknown,
    /// Every includer is a root that gives the objects a widget pool.
    Covered,
    /// These roots do not.
    Uncovered(Vec<PathBuf>),
}

/// The roots of one walk and what they say about its include fragments. See the
/// module docs.
pub struct IncluderPools<'a> {
    roots: &'a [PathBuf],
    /// The include fragments the walk judges, lexically normalised.
    wanted: HashSet<PathBuf>,
    /// The directory of `oxabl.toml`, when the walk starts there and judging is
    /// possible.
    workspace: Option<PathBuf>,
    /// Files that are not read as source, such as the baseline file.
    skip: HashSet<PathBuf>,
    jobs: usize,
    run: Box<dyn Fn() -> LintPipeline<'a> + Sync + 'a>,
    facts: OnceLock<Facts>,
}

/// The most that is read of a file that is not a root.
const SCAN_LIMIT: u64 = 8 << 20;

impl<'a> IncluderPools<'a> {
    /// The pools of a walk over `roots` that judges `fragments`.
    ///
    /// `run` makes one run handle per worker thread, configured as the walk
    /// configures its own. `workspace` is the directory of `oxabl.toml` when
    /// the walk starts there and every include path lies below it, and `None`
    /// otherwise: nothing is judged then. `skip` lists files that are not
    /// source.
    pub fn new(
        roots: &'a [PathBuf],
        fragments: &[PathBuf],
        jobs: usize,
        workspace: Option<PathBuf>,
        skip: &[PathBuf],
        run: impl Fn() -> LintPipeline<'a> + Sync + 'a,
    ) -> Self {
        IncluderPools {
            roots,
            wanted: fragments.iter().map(|f| normalize_lexically(f)).collect(),
            workspace,
            skip: skip.iter().map(|f| normalize_lexically(f)).collect(),
            jobs,
            run: Box::new(run),
            facts: OnceLock::new(),
        }
    }

    /// Build the analysis now, on `jobs` threads. Call it before the walk
    /// starts its own workers, so the two do not share the machine.
    pub fn prepare(&self) {
        if self.workspace.is_some() && !self.wanted.is_empty() {
            self.facts();
        }
    }

    fn facts(&self) -> &Facts {
        self.facts.get_or_init(|| self.build())
    }

    /// What the roots say about `include`.
    pub fn verdict(&self, include: &Path) -> PoolVerdict {
        if self.workspace.is_none() {
            return PoolVerdict::Unknown;
        }
        let include = normalize_lexically(include);
        let facts = self.facts();
        let Some(includers) = facts.includers.get(&include).filter(|i| !i.is_empty()) else {
            return PoolVerdict::Unknown;
        };
        let reaching: HashSet<&PathBuf> = includers.iter().map(|i| &i.root).collect();
        let named_by_roots = facts.named_by_roots.get(&include);
        if facts.blind
            || facts.named_elsewhere.contains(&include)
            || named_by_roots.is_none_or(|named| {
                named.len() != reaching.len() || !named.iter().all(|r| reaching.contains(r))
            })
            || includers.iter().any(|i| {
                i.recovered
                    || i.creates == Creates::None
                    || base_name(&i.root).is_some_and(|n| facts.names_elsewhere.contains(&n))
            })
        {
            return PoolVerdict::Unknown;
        }
        let uncovered: Vec<PathBuf> = includers
            .iter()
            .filter(|i| i.creates == Creates::Unpooled)
            .map(|i| i.root.clone())
            .collect();
        if uncovered.is_empty() {
            PoolVerdict::Covered
        } else {
            PoolVerdict::Uncovered(uncovered)
        }
    }

    /// `result`, the context-free run of the include at `include`, with its
    /// HD005 findings judged by the includers. The analysis is built on the
    /// first finding if [`prepare`](Self::prepare) did not build it.
    pub fn judge(&self, include: &Path, mut result: LintResult) -> LintResult {
        let Some(diagnostics) = result.diagnostics_mut() else {
            return result;
        };
        let is_pool = |d: &CollectedDiagnostic| {
            d.source == DiagnosticSource::Lint
                && d.diagnostic.code.0 == oxabl_lint::HD005
                && d.diagnostic.span.file == ROOT_FILE_ID
        };
        if !diagnostics.diagnostics.iter().any(is_pool) {
            return result;
        }
        let verdict = self.verdict(include);
        if verdict == PoolVerdict::Unknown {
            return result;
        }
        let cwd = std::env::current_dir().ok();
        diagnostics.diagnostics.retain_mut(|d| {
            if !is_pool(d) {
                return true;
            }
            match &verdict {
                PoolVerdict::Covered => false,
                PoolVerdict::Uncovered(roots) => {
                    let names: Vec<String> =
                        roots.iter().map(|r| shown(r, cwd.as_deref())).collect();
                    name_includers(&mut d.diagnostic, &names);
                    true
                }
                PoolVerdict::Unknown => true,
            }
        });
        result
    }

    fn build(&self) -> Facts {
        let jobs = self.jobs.clamp(1, self.roots.len().max(1));
        let next = AtomicUsize::new(0);
        let mut parts: Vec<(usize, Root)> = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..jobs)
                .map(|_| {
                    let next = &next;
                    oxabl_common::stack::builder()
                        .spawn_scoped(scope, move || {
                            let pipeline = (self.run)();
                            let mut mine = Vec::new();
                            loop {
                                let i = next.fetch_add(1, Ordering::Relaxed);
                                let Some(root) = self.roots.get(i) else { break };
                                mine.push((i, self.analyse(&pipeline, root)));
                            }
                            mine
                        })
                        .expect("failed to spawn an analysis thread")
                })
                .collect();
            workers
                .into_iter()
                .flat_map(|w| match w.join() {
                    Ok(mine) => mine,
                    Err(panic) => std::panic::resume_unwind(panic),
                })
                .collect()
        });
        parts.sort_unstable_by_key(|(i, _)| *i);

        let mut facts = Facts::default();
        let mut computing = Vec::new();
        for (i, part) in parts {
            let Root::Done {
                includes,
                named,
                recovered,
                gap,
                computed,
            } = part
            else {
                facts.blind = true;
                continue;
            };
            facts.blind |= gap;
            let root = normalize_lexically(&self.roots[i]);
            if computed {
                computing.push(root.clone());
            }
            for include in named {
                facts
                    .named_by_roots
                    .entry(include)
                    .or_default()
                    .insert(root.clone());
            }
            for (include, creates) in includes {
                facts.includers.entry(include).or_default().push(Includer {
                    root: root.clone(),
                    creates,
                    recovered,
                });
            }
        }
        if !facts.blind {
            self.scan_elsewhere(&(self.run)(), &mut facts);
        }
        // A root with a computed include name that an unseen file includes
        // may include any file.
        facts.blind |= computing
            .iter()
            .any(|root| base_name(root).is_some_and(|n| facts.names_elsewhere.contains(&n)));
        facts
    }

    fn analyse(&self, pipeline: &LintPipeline<'_>, root: &Path) -> Root {
        let Ok(source) = pipeline.file_system().read(root) else {
            return Root::Failed;
        };
        catch_panic(|| {
            let handle = pipeline.with_file(root);
            let expansion = handle.expand(&source);
            let Some(expanded) = expansion.expanded() else {
                return Root::Failed;
            };
            // An include that did not arrive may include anything.
            let gap = !expanded.unresolved_includes().is_empty()
                || !expanded.nested_unresolved_includes().is_empty()
                || !expanded.unreadable_includes().is_empty()
                || !expanded.elided_include_offsets().is_empty()
                || self.workspace.as_ref().is_some_and(|ws| {
                    expanded
                        .include_expansions()
                        .iter()
                        .any(|e| !normalize_lexically(e.path).starts_with(ws))
                });
            let IncludeNames { names, computed } = include_names(&source);
            let named: HashSet<PathBuf> = self
                .wanted
                .iter()
                .filter(|w| base_name(w).is_some_and(|n| names.contains(&n)))
                .cloned()
                .collect();
            let mut includes: HashMap<PathBuf, Creates> = expanded
                .include_expansions()
                .iter()
                .map(|e| normalize_lexically(e.path))
                .filter(|p| self.wanted.contains(p))
                .map(|p| (p, Creates::None))
                .collect();
            if includes.is_empty() {
                return Root::Done {
                    includes,
                    named,
                    recovered: false,
                    gap,
                    computed,
                };
            }

            let tokens = oxabl_lexer::tokenize(&expanded.text);
            let program = oxabl_parser::Parser::new(&tokens, &expanded.text).parse_program();
            let handles = oxabl_handles::analyze_tokens(
                oxabl_flow::FileInput::new(
                    &program.statements,
                    &program.pools,
                    &expanded.text,
                    &tokens,
                ),
                &FlowOptions::default(),
            );
            for (_, object) in handles.objects() {
                if !object.kind.uses_widget_pool() {
                    continue;
                }
                let Some((path, _, _)) = expanded.resolve_include_span(object.create.span) else {
                    continue;
                };
                let Some(seen) = includes.get_mut(&normalize_lexically(path)) else {
                    continue;
                };
                if missing_widget_pool::counts(object) {
                    *seen = Creates::Unpooled;
                } else if *seen == Creates::None {
                    *seen = Creates::Pooled;
                }
            }
            Root::Done {
                includes,
                named,
                recovered: !program.errors.is_empty(),
                gap,
                computed,
            }
        })
        .unwrap_or(Root::Failed)
    }

    /// Read the files of the workspace directory that are not roots, for the
    /// includes they name. Anything that cannot be read, or that may name an
    /// include without writing it, blinds the analysis.
    fn scan_elsewhere(&self, pipeline: &LintPipeline<'_>, facts: &mut Facts) {
        let Some(workspace) = &self.workspace else {
            return;
        };
        let names: Vec<(String, &PathBuf)> = self
            .wanted
            .iter()
            .filter_map(|p| Some((base_name(p)?, p)))
            .collect();
        let roots: HashSet<PathBuf> = self.roots.iter().map(|r| normalize_lexically(r)).collect();
        let mut pending = vec![workspace.clone()];
        while let Some(dir) = pending.pop() {
            let Ok(entries) = std::fs::read_dir(&dir) else {
                facts.blind = true;
                return;
            };
            for entry in entries {
                let Ok(entry) = entry else {
                    facts.blind = true;
                    return;
                };
                let path = entry.path();
                let Ok(kind) = entry.file_type() else {
                    facts.blind = true;
                    return;
                };
                if kind.is_symlink() {
                    // The walk follows links and this scan does not.
                    facts.blind = true;
                    return;
                }
                if kind.is_dir() {
                    if entry.file_name() != ".git" {
                        pending.push(path);
                    }
                    continue;
                }
                let key = normalize_lexically(&path);
                if !kind.is_file()
                    || roots.contains(&key)
                    || self.skip.contains(&key)
                    || entry.file_name() == "oxabl.toml"
                {
                    continue;
                }
                let Some(text) = read_source_text(pipeline, &path) else {
                    facts.blind = true;
                    return;
                };
                let found = include_names(&text);
                if found.computed {
                    facts.blind = true;
                    return;
                }
                facts.names_elsewhere.extend(found.names.iter().cloned());
                for (name, include) in &names {
                    if found.names.contains(name) && **include != key {
                        facts.named_elsewhere.insert((*include).clone());
                    }
                }
            }
        }
    }
}

/// The lower-case file name of `path`.
fn base_name(path: &Path) -> Option<String> {
    Some(path.file_name()?.to_string_lossy().to_lowercase())
}

/// The text of a file for the scan, decoded as the roots are. `None` when it
/// cannot be read or is too large; an empty text for a binary file.
fn read_source_text(pipeline: &LintPipeline<'_>, path: &Path) -> Option<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path).ok()?;
    let len = file.metadata().ok()?.len();
    let mut head = vec![0u8; 4096.min(len as usize)];
    file.read_exact(&mut head).ok()?;
    let utf16 = head.starts_with(&[0xFF, 0xFE]) || head.starts_with(&[0xFE, 0xFF]);
    if !utf16 && head.contains(&0) {
        return Some(String::new());
    }
    if len > SCAN_LIMIT {
        return None;
    }
    if utf16 {
        let mut rest = Vec::new();
        file.read_to_end(&mut rest).ok()?;
        head.extend(rest);
        head.retain(|b| *b != 0);
        return Some(String::from_utf8_lossy(&head).into_owned());
    }
    pipeline
        .file_system()
        .read(path)
        .ok()
        .map(|t| t.to_string())
}

/// The include names that a text writes.
#[derive(Default)]
struct IncludeNames {
    /// The lower-case file name of each `{name ...}` reference.
    names: HashSet<String>,
    /// A reference whose name is computed, such as `{{&n}.i}`, or too long
    /// to be a file name.
    computed: bool,
}

/// The longest reference read as a file name.
const NAME_LIMIT: usize = 1024;

/// Read the `{name ...}` references of `text`. A reference to a preprocessor
/// name (`{&name}`) is not an include. A name in quotes may hold spaces. The
/// scan does not skip comments or strings, so it can name more than the
/// compiler includes, never less. Each byte is passed over a bounded number of
/// times, also in a run of `{`.
fn include_names(text: &str) -> IncludeNames {
    let mut out = IncludeNames::default();
    // The end of the unquoted name that starts at `start`: the next space or
    // `}`. The starts only grow, so one search serves every start before it.
    let mut name_end = 0;
    for (open, _) in text.match_indices('{') {
        let rest = &text[open + 1..];
        let start = open + 1 + (rest.len() - rest.trim_start().len());
        let end = match text[start..].chars().next() {
            Some(q @ ('"' | '\'')) => {
                let inner = start + 1;
                let limit = floor_char_boundary(text, inner + NAME_LIMIT + 1);
                inner + text[inner..limit].find([q, '}']).unwrap_or(limit - inner)
            }
            _ => {
                if name_end < start {
                    name_end = start
                        + text[start..]
                            .find(|c: char| c.is_whitespace() || c == '}')
                            .unwrap_or(text.len() - start);
                }
                name_end
            }
        };
        let quoted = matches!(text[start..].chars().next(), Some('"' | '\''));
        let token = &text[if quoted { start + 1 } else { start }..end];
        if token.len() > NAME_LIMIT || names_by_reference(token) || token.contains('\u{FFFD}') {
            out.computed = true;
        } else if !token.starts_with('&')
            && let Some(last) = token.rsplit(['/', '\\']).next()
        {
            out.names.insert(last.to_lowercase());
        }
    }
    out
}

/// The largest char boundary of `text` at or below `at`.
fn floor_char_boundary(text: &str, at: usize) -> usize {
    let mut at = at.min(text.len());
    while !text.is_char_boundary(at) {
        at -= 1;
    }
    at
}

/// Whether `token` holds a reference that can build a file name: `{&name}`,
/// `{1}` or `{*}`.
fn names_by_reference(token: &str) -> bool {
    token.match_indices('{').any(|(i, _)| {
        token[i + 1..]
            .chars()
            .next()
            .is_some_and(|c| c == '&' || c == '*' || c.is_ascii_digit())
    })
}

/// `path` as the walk shows it: relative to the working directory when it is
/// below it.
fn shown(path: &Path, cwd: Option<&Path>) -> String {
    cwd.filter(|cwd| cwd.parent().is_some())
        .and_then(|cwd| path.strip_prefix(cwd).ok())
        .unwrap_or(path)
        .display()
        .to_string()
}

/// Replace the includer note that ends the help of `d` with one that names
/// `includers`.
fn name_includers(d: &mut Diagnostic, includers: &[String]) {
    let note = uncovered_includers_note(includers);
    d.help = Some(match d.help.take() {
        Some(help) => {
            let base = help.strip_suffix(FRAGMENT_NOTE).unwrap_or(&help).trim_end();
            format!("{base} {note}")
        }
        None => note,
    });
}
