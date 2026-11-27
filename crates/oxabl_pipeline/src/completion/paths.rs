//! Paths and preprocessor names: the targets of `RUN`, the files after `{`,
//! the names after `{&`, and the arguments `{n}` of an include.
//!
//! The directories are listed through the request's file system, in each
//! entry of the document's include path. A directory that cannot be listed
//! makes the list incomplete; it never reads as empty.

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_workspace::DirEntry;

use oxabl_analyze::{DefineDirective, PlacedDefine};
use oxabl_ast::{NodeId, Span};
use oxabl_lexer::Kind;
use oxabl_semantic::{NamespaceId, ScopeId};

use super::rank::{self, Quality, fit, nearness};
use super::{Candidate, Engine, ItemData, ItemKind, PathData};

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
                let mut procedures: Vec<_> = semantic
                    .scope_tree
                    .get(ScopeId::ROOT)
                    .bindings
                    .get(NamespaceId::Procedures)
                    .into_iter()
                    .flat_map(|map| map.iter())
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
        // The statement at the cursor is not complete, so the parser reads a
        // `PROCEDURE` right after it as the target of `RUN` and loses that
        // one declaration. Only that one: a `PROCEDURE` anywhere else is in
        // the base, or in an `&IF` branch the preprocessor did not take.
        let text = self.request.text;
        let mut after = self
            .tokens
            .iter_from(self.found.before)
            .filter(|token| token.start >= self.found.word_end)
            .filter(|token| token.kind != Kind::Comment);
        if after
            .next()
            .is_some_and(|token| token.kind == Kind::Procedure)
            && let Some(name) = after.next().filter(|token| token.kind == Kind::Identifier)
        {
            let label = name.slice(text);
            if !items
                .iter()
                .any(|item| item.label.eq_ignore_ascii_case(label))
            {
                items.push(procedure(label));
            }
        }
        let extensions = &self.request.config.run_extensions;
        incomplete |= self.list_paths(&mut items, |name| {
            let extension = extension(name)?;
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
            let include = extension(name).is_some_and(|ext| ext.eq_ignore_ascii_case("i"));
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
        // The directives the current text no longer holds (a stale base) are
        // not replayed.
        let defines: Vec<&PlacedDefine> = expanded
            .defines()
            .iter()
            .filter(|define| {
                !self.mapped.is_some_and(|m| {
                    m.touches_changed(define.site.file, name_span(base.text, define))
                })
            })
            .collect();
        let each: Vec<HashMap<String, Visible<'_>>> = mapped
            .places
            .iter()
            .map(|place| visible_at(&defines, *place))
            .collect();
        let (first, others) = each.split_first()?;
        let mut names: Vec<(String, Option<String>)> = Vec::new();
        // A name a directive set or undefined; a predefined name of the same
        // spelling is not offered again.
        let mut seen: HashSet<String> = defines
            .iter()
            .map(|define| define.name.to_ascii_lowercase())
            .collect();
        for (key, visible) in first {
            if others
                .iter()
                .all(|other| other.get(key).is_some_and(|o| o.value == visible.value))
            {
                names.push((visible.name.to_string(), Some(visible.value.to_string())));
            }
        }
        let first: HashMap<&str, u32> = first
            .iter()
            .map(|(key, visible)| (key.as_str(), visible.offset))
            .collect();
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
                // A directive in the include after its start sets the name
                // again, or undefines it.
                let later = defines
                    .iter()
                    .any(|define| define.offset >= start && define.name.eq_ignore_ascii_case(name))
                    && first.get(key.as_str()).is_none_or(|at| *at >= start);
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
    /// a nearness, whose name matches the last part of the word. A name that
    /// two entries give counts once. True when the list can miss entries: no
    /// include path, a directory that cannot be listed, or more than
    /// [`MAX_MATCHES`] matches in one directory.
    fn list_paths(&self, items: &mut Vec<Candidate>, keep: impl Fn(&str) -> Option<u8>) -> bool {
        let request = self.request;
        let word = &request.text[self.found.word_start as usize..request.offset as usize];
        let split = word.rfind(['/', '\\']).map_or(0, |at| at + 1);
        let (dir, typed) = word.split_at(split);
        let entries = request
            .config
            .include_paths_for(Some(request.path), request.text);
        if entries.is_empty() {
            return true;
        }
        // Each directory is listed once in a request.
        let mut listed: HashMap<PathBuf, Option<Arc<[DirEntry]>>> = HashMap::new();
        let mut list = |path: &Path| {
            listed
                .entry(path.to_path_buf())
                .or_insert_with(|| request.listing.list(path))
                .clone()
        };
        let mut incomplete = false;
        let mut seen: HashSet<String> = items
            .iter()
            .filter(|item| matches!(item.kind, ItemKind::File | ItemKind::Folder))
            .map(|item| item.label.clone())
            .collect();
        let mut done: HashSet<PathBuf> = HashSet::new();
        for entry in entries.iter() {
            let path = entry.join(dir);
            if !done.insert(path.clone()) {
                continue;
            }
            let Some(listing) = list(&path) else {
                incomplete |= !missing(&path, &mut list);
                continue;
            };
            // Each match with its rank key, as the final order reads it, and
            // its place in the listing, which is in name order.
            let mut matches: Vec<(u32, u32)> = Vec::new();
            for (at, found) in listing.iter().enumerate() {
                if found.name.starts_with('.') {
                    continue;
                }
                let Some(quality) = rank::quality(typed, &found.name) else {
                    continue;
                };
                let near = if found.is_dir {
                    nearness::PATH
                } else {
                    let Some(near) = keep(&found.name) else {
                        continue;
                    };
                    near
                };
                let inner = u32::from(quality == Quality::Inner);
                matches.push((
                    inner << 16 | (quality as u32) << 8 | u32::from(near),
                    at as u32,
                ));
            }
            // The cap keeps the matches that rank first, not the first in the
            // alphabet: every match of a rank before the last one kept, and
            // the first of that rank in name order.
            if matches.len() > MAX_MATCHES {
                let mut counts: Vec<(u32, usize)> = Vec::new();
                for (key, _) in &matches {
                    match counts.iter_mut().find(|(known, _)| known == key) {
                        Some((_, count)) => *count += 1,
                        None => counts.push((*key, 1)),
                    }
                }
                counts.sort_unstable();
                let (mut last, mut room) = (0, MAX_MATCHES);
                for (key, count) in counts {
                    last = key;
                    if count >= room {
                        break;
                    }
                    room -= count;
                }
                matches.retain(|(key, _)| match key.cmp(&last) {
                    Ordering::Less => true,
                    Ordering::Equal if room > 0 => {
                        room -= 1;
                        true
                    }
                    _ => false,
                });
                incomplete = true;
            }
            for (key, at) in matches {
                let (found, near) = (&listing[at as usize], (key & 0xff) as u8);
                let (label, kind) = if found.is_dir {
                    (format!("{dir}{}/", found.name), ItemKind::Folder)
                } else {
                    (format!("{dir}{}", found.name), ItemKind::File)
                };
                if !seen.insert(label.clone()) {
                    continue;
                }
                items.push(Candidate {
                    insert: label.clone(),
                    detail: (kind == ItemKind::File)
                        .then(|| path.join(&found.name).display().to_string()),
                    data: ItemData::Path(Box::new(PathData {
                        path: label.clone(),
                        entry: entry.clone(),
                    })),
                    label,
                    kind,
                    snippet: false,
                    fit: fit::OTHER,
                    nearness: near,
                });
            }
        }
        incomplete
    }
}

/// The most entries of one directory that match the word and are offered:
/// the best ranked. More make the list incomplete, and the client asks again
/// as the word grows.
const MAX_MATCHES: usize = 1000;

/// The extension of a file name from a listing, as `Path::extension` reads it
/// for a name with no separator, without its cost.
fn extension(name: &str) -> Option<&str> {
    name.rsplit_once('.')
        .filter(|(stem, _)| !stem.is_empty())
        .map(|(_, ext)| ext)
}

/// Whether `dir` is known not to exist: its parent lists, and holds no entry
/// of its name. An entry that is a file (a procedure library) cannot be
/// listed, and is not missing.
fn missing(dir: &Path, list: &mut impl FnMut(&Path) -> Option<Arc<[DirEntry]>>) -> bool {
    let (Some(parent), Some(name)) = (dir.parent(), dir.file_name()) else {
        return false;
    };
    list(parent).is_some_and(|listing| {
        !listing
            .iter()
            .any(|entry| std::ffi::OsStr::new(&entry.name) == name)
    })
}

/// The bytes of the name a directive writes, in the text of the document,
/// when the document is the file that writes it: the first word after the
/// directive's keyword. The whole directive when the text does not hold it.
fn name_span(text: &str, define: &PlacedDefine) -> Span {
    let span = define.site.span;
    let found = text
        .get(span.start as usize..span.end as usize)
        .and_then(|directive| {
            let keyword = directive.find(char::is_whitespace)?;
            let rest = &directive[keyword..];
            let name = rest.find(|c: char| !c.is_whitespace())?;
            let start = span.start + (keyword + name) as u32;
            let len = rest[name..]
                .find(char::is_whitespace)
                .unwrap_or(rest.len() - name);
            (rest[name..name + len].eq_ignore_ascii_case(&define.name)).then_some(Span {
                start,
                end: start + len as u32,
            })
        });
    found.unwrap_or(span)
}

/// The definition of a preprocessor name that `{&name}` reads.
#[derive(Debug, Clone, Copy)]
struct Visible<'d> {
    /// As the directive writes it.
    name: &'d str,
    value: &'d str,
    /// Where the directive lies in the expanded text.
    offset: u32,
    /// Where it stops to hold: the end of the file of a `&SCOPED-DEFINE`.
    end: u32,
    scoped: bool,
}

/// The names `{&name}` reads at expanded offset `place`, by lower-case name:
/// the directives before it replayed as the preprocessor's table does. Each
/// name has a stack. A `&SCOPED-DEFINE` pushes a definition that ends with
/// its file, or replaces the one its file made; a `&GLOBAL-DEFINE` sets the
/// global one, and is ignored over a scoped one; an `&UNDEFINE` removes the
/// newest, and the one under it shows again.
fn visible_at<'d>(defines: &[&'d PlacedDefine], place: u32) -> HashMap<String, Visible<'d>> {
    let mut stacks: HashMap<String, Vec<Visible<'d>>> = HashMap::new();
    let prune = |stack: &mut Vec<Visible<'d>>, at: u32| {
        while stack.last().is_some_and(|top| top.end <= at) {
            stack.pop();
        }
    };
    for define in defines.iter().take_while(|define| define.offset <= place) {
        let stack = stacks.entry(define.name.to_ascii_lowercase()).or_default();
        prune(stack, define.offset);
        let entry = Visible {
            name: &define.name,
            value: &define.value,
            offset: define.offset,
            end: define.scope_end,
            scoped: define.directive == DefineDirective::Scoped,
        };
        match define.directive {
            DefineDirective::Undefine => {
                stack.pop();
            }
            DefineDirective::Scoped => match stack.last_mut() {
                Some(top) if top.scoped && top.end == entry.end => *top = entry,
                _ => stack.push(entry),
            },
            DefineDirective::Global => match stack.last_mut() {
                None => stack.push(entry),
                Some(top) if !top.scoped => *top = entry,
                Some(_) => {}
            },
        }
    }
    stacks
        .into_iter()
        .filter_map(|(key, mut stack)| {
            prune(&mut stack, place);
            Some((key, stack.pop()?))
        })
        .collect()
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
