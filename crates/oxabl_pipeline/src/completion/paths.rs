//! Paths and preprocessor names: the targets of `RUN`, the files after `{`,
//! the names after `{&`, and the arguments `{n}` of an include.
//!
//! The directories are listed through the request's file system, in each
//! entry of the document's include path. A directory that cannot be listed
//! makes the list incomplete; it never reads as empty.

use std::collections::{HashMap, HashSet};
use std::path::Path;

use oxabl_analyze::DefineDirective;
use oxabl_ast::NodeId;
use oxabl_lexer::Kind;
use oxabl_semantic::{NamespaceId, ScopeId};

use super::rank::{fit, nearness};
use super::{Candidate, Engine, ItemData, ItemKind};

/// The names the preprocessor defines itself.
const PREDEFINED: [&str; 8] = [
    "BATCH-MODE",
    "FILE-NAME",
    "LINE-NUMBER",
    "OPSYS",
    "PROCESS-ARCHITECTURE",
    "PROVERSION",
    "SEQUENCE",
    "WINDOW-SYSTEM",
];

impl Engine<'_, '_, '_, '_> {
    /// After `RUN`: the internal procedures of the unit, and the programs and
    /// directories on the include path.
    pub(super) fn run_targets(&self) -> Option<(Vec<Candidate>, bool)> {
        let mut items = Vec::new();
        let mut incomplete = match self.based() {
            Some((base, _)) => {
                let semantic = &base.navigation.model.semantic;
                let symbols = &semantic.symbols;
                let mut procedures: Vec<_> = semantic.scope_tree.get(ScopeId::ROOT).bindings
                    [NamespaceId::Procedures.index()]
                .iter()
                .map(|(_, sym)| sym)
                .collect();
                procedures.sort_unstable_by_key(|sym| sym.raw());
                for sym in procedures {
                    let symbol = symbols.get(sym);
                    if symbol.declaration == NodeId::DUMMY
                        || self
                            .mapped
                            .is_some_and(|m| m.in_changed(base, symbol.name_span.start))
                    {
                        continue;
                    }
                    let Some(label) = self.spelling(base, symbol) else {
                        continue;
                    };
                    items.push(Candidate {
                        data: ItemData::Symbol(sym.raw()),
                        ..procedure(label)
                    });
                }
                self.base_incomplete()
            }
            None => true,
        };
        // The statement at the cursor is not complete, so the parser can read
        // the next `PROCEDURE` as its target and lose the declaration: a
        // `PROCEDURE` that starts a statement or a line of the current text
        // names one too.
        let text = self.request.text;
        let mut known: HashSet<String> = items
            .iter()
            .map(|item| item.label.to_ascii_lowercase())
            .collect();
        let mut previous = None;
        for (at, token) in self.tokens.iter().enumerate() {
            if token.kind == Kind::Comment {
                continue;
            }
            let starts = || {
                previous.is_none_or(|kind| matches!(kind, Kind::Period | Kind::Colon))
                    || text[..token.start]
                        .rfind('\n')
                        .map_or(&text[..token.start], |at| &text[at + 1..token.start])
                        .trim()
                        .is_empty()
            };
            if token.kind == Kind::Procedure
                && starts()
                && let Some(name) = self.tokens[at + 1..]
                    .iter()
                    .find(|next| next.kind != Kind::Comment)
                    .filter(|next| next.kind == Kind::Identifier)
            {
                let label = &text[name.start..name.end];
                if known.insert(label.to_ascii_lowercase()) {
                    items.push(procedure(label));
                }
            }
            previous = Some(token.kind);
        }
        let extensions = &self.request.config.run_extensions;
        incomplete |= self.list_paths(&mut items, |name| {
            let extension = Path::new(name).extension()?.to_str()?;
            (extension.eq_ignore_ascii_case("r")
                || extensions
                    .iter()
                    .any(|known| known.eq_ignore_ascii_case(extension)))
            .then_some(nearness::PATH)
        });
        Some((items, incomplete))
    }

    /// After `{`: the arguments of the include when the word is empty, then
    /// the files and directories on the include path, include files (`.i`)
    /// first.
    pub(super) fn include_files(&self) -> Option<(Vec<Candidate>, bool)> {
        let mut items = Vec::new();
        let mut incomplete = false;
        if self.found.word_start == self.request.offset {
            match self.argument_items() {
                Some(arguments) => items.extend(arguments),
                // An include analysed alone: its arguments are not known.
                None => incomplete |= self.base.is_some_and(|base| base.fragment),
            }
        }
        incomplete |= self.list_paths(&mut items, |name| {
            let include = Path::new(name)
                .extension()
                .is_some_and(|extension| extension.eq_ignore_ascii_case("i"));
            Some(if include {
                nearness::INCLUDE
            } else {
                nearness::OTHER_FILE
            })
        });
        Some((items, incomplete))
    }

    /// After `{` and digits: the arguments of the include. `None` outside an
    /// include in a selected root.
    pub(super) fn include_arguments(&self) -> Option<(Vec<Candidate>, bool)> {
        let items = self.argument_items()?;
        Some((items, self.base_incomplete()))
    }

    /// The items `1` up to the number of positional arguments of the
    /// references that include the document, with the argument as the detail
    /// when they all pass the same text. `None` when the document is not an
    /// include in a selected root.
    fn argument_items(&self) -> Option<Vec<Candidate>> {
        let (base, mapped) = self.based()?;
        let expanded = &base.navigation.expanded;
        if base.fragment || mapped.ids.contains(&expanded.root_file()) {
            return None;
        }
        let lists: Vec<&[String]> = mapped
            .ids
            .iter()
            .map(|id| {
                expanded
                    .include_arguments(*id)
                    .map_or(&[][..], |arguments| &arguments.positional[..])
            })
            .collect();
        let count = lists.iter().map(|list| list.len()).min()?;
        Some(
            (1..=count)
                .map(|n| {
                    let value = &lists[0][n - 1];
                    let same = lists.iter().all(|list| list[n - 1] == *value);
                    constant(n.to_string(), same.then(|| value.clone()))
                })
                .collect(),
        )
    }

    /// After `{&`: the names defined at the cursor and not undefined, the
    /// named arguments of the reference that includes the document, and the
    /// names the preprocessor defines itself.
    pub(super) fn preprocessor_names(&self) -> Option<(Vec<Candidate>, bool)> {
        let (base, mapped) = self.based()?;
        let expanded = &base.navigation.expanded;
        // Lower-case name, then the name as written, its value and where the
        // last directive that set it lies.
        type Defined<'d> = HashMap<String, Option<(&'d str, &'d str, u32)>>;
        let mut each: Vec<Defined<'_>> = Vec::new();
        for place in &mapped.places {
            let mut defined: Defined<'_> = HashMap::new();
            for define in expanded.defines() {
                if define.offset > *place || *place >= define.scope_end {
                    continue;
                }
                let state = match define.directive {
                    DefineDirective::Undefine => None,
                    DefineDirective::Global | DefineDirective::Scoped => {
                        Some((define.name.as_str(), define.value.as_str(), define.offset))
                    }
                };
                defined.insert(define.name.to_ascii_lowercase(), state);
            }
            each.push(defined);
        }
        let (first, others) = each.split_first()?;
        let mut names: Vec<(String, Option<String>)> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();
        for (key, state) in first {
            let Some((name, value, _)) = state else {
                seen.insert(key.clone());
                continue;
            };
            seen.insert(key.clone());
            if others
                .iter()
                .all(|other| other.get(key).is_some_and(Option::is_some))
            {
                names.push((name.to_string(), Some(value.to_string())));
            }
        }
        // The named arguments of the reference: a define in the include
        // after its start sets the name again.
        for id in &mapped.ids {
            let Some(arguments) = expanded.include_arguments(*id) else {
                continue;
            };
            let start = expanded
                .include_expansions()
                .iter()
                .find(|expansion| expansion.file == Some(*id))
                .map_or(0, |expansion| expansion.expanded.start);
            for (name, value) in &arguments.named {
                let key = name.to_ascii_lowercase();
                let later = first
                    .get(&key)
                    .is_some_and(|state| state.is_none_or(|(_, _, at)| at >= start));
                if later {
                    continue;
                }
                names.retain(|(known, _)| !known.eq_ignore_ascii_case(name));
                seen.insert(key);
                names.push((name.clone(), Some(value.clone())));
            }
        }
        for name in PREDEFINED {
            if seen.insert(name.to_ascii_lowercase()) {
                names.push((name.to_string(), None));
            }
        }
        let items = names
            .into_iter()
            .map(|(name, value)| constant(name, value.filter(|value| !value.is_empty())))
            .collect();
        Some((items, self.base_incomplete()))
    }

    /// Add the entries of the directory part of the word, under each entry of
    /// the include path, to `items`: directories, and the files `keep` gives
    /// a nearness. A name that two entries give counts once. True when the
    /// list can miss entries: no include path, or a directory that cannot be
    /// listed.
    fn list_paths(&self, items: &mut Vec<Candidate>, keep: impl Fn(&str) -> Option<u8>) -> bool {
        let request = self.request;
        let word = &request.text[self.found.word_start as usize..request.offset as usize];
        let dir = &word[..word.rfind(['/', '\\']).map_or(0, |at| at + 1)];
        let entries = request
            .config
            .include_paths_for(Some(request.path), request.text);
        if entries.is_empty() {
            return true;
        }
        let mut incomplete = false;
        let mut seen: HashSet<String> = items
            .iter()
            .filter(|item| matches!(item.kind, ItemKind::File | ItemKind::Folder))
            .map(|item| item.label.clone())
            .collect();
        for entry in entries.iter() {
            let listed = entry.join(dir);
            let Some(mut listing) = request.files.list_dir(&listed) else {
                incomplete |= !self.missing(&listed);
                continue;
            };
            listing.sort_by(|a, b| a.name.cmp(&b.name));
            for found in listing {
                if found.name.starts_with('.') {
                    continue;
                }
                let (label, kind, near, detail) = if found.is_dir {
                    (
                        format!("{dir}{}/", found.name),
                        ItemKind::Folder,
                        nearness::PATH,
                        None,
                    )
                } else {
                    let Some(near) = keep(&found.name) else {
                        continue;
                    };
                    (
                        format!("{dir}{}", found.name),
                        ItemKind::File,
                        near,
                        Some(listed.join(&found.name).display().to_string()),
                    )
                };
                if !seen.insert(label.clone()) {
                    continue;
                }
                items.push(Candidate {
                    insert: label.clone(),
                    data: ItemData::Path {
                        path: label.clone(),
                        entry: entry.clone(),
                    },
                    label,
                    kind,
                    detail,
                    snippet: false,
                    fit: fit::OTHER,
                    nearness: near,
                });
            }
        }
        incomplete
    }

    /// Whether `dir` is known not to exist: its parent lists, and holds no
    /// directory of its name.
    fn missing(&self, dir: &Path) -> bool {
        let (Some(parent), Some(name)) = (dir.parent(), dir.file_name()) else {
            return false;
        };
        self.request.files.list_dir(parent).is_some_and(|listing| {
            !listing
                .iter()
                .any(|entry| entry.is_dir && std::ffi::OsStr::new(&entry.name) == name)
        })
    }
}

/// The item of an internal procedure.
fn procedure(label: &str) -> Candidate {
    Candidate {
        label: label.to_string(),
        insert: label.to_string(),
        kind: ItemKind::Function,
        detail: Some("PROCEDURE".to_string()),
        snippet: false,
        data: ItemData::None,
        fit: fit::OTHER,
        nearness: nearness::SCOPE,
    }
}

/// The item of a preprocessor name or an include argument.
fn constant(label: String, detail: Option<String>) -> Candidate {
    Candidate {
        insert: label.clone(),
        label,
        kind: ItemKind::Constant,
        detail,
        snippet: false,
        data: ItemData::None,
        fit: fit::OTHER,
        nearness: nearness::SCOPE,
    }
}
