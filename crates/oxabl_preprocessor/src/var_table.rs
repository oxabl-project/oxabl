use std::collections::HashMap;
use std::sync::Arc;

use rustc_hash::FxBuildHasher;

/// Preprocessor variable table tracking `&SCOPED-DEFINE` and `&GLOBAL-DEFINE` values.
///
/// `&SCOPED-DEFINE` variables are file-local — they do not propagate to the
/// file that included them. `&GLOBAL-DEFINE` variables propagate upward to all
/// callers in the include chain, except into a scope that has a scoped name or an
/// include argument of the same name: that one takes precedence there.
///
/// All lookups are case-insensitive (ABL preprocessor variables are
/// case-insensitive).
#[derive(Debug, Clone, Default)]
pub struct PreprocVarTable {
    /// The names the table started with, shared by every table made over them.
    base: Option<BaseVars>,
    /// The changes made to `base`, keyed by the lowercase form of the name.
    vars: HashMap<Arc<[u8]>, Slot, FxBuildHasher>,
    /// What each name held in `vars` before it changed (`None`: nothing), newest
    /// last, while a scope is open. Undoing it restores the table without
    /// copying it.
    journal: Vec<(Arc<[u8]>, Option<Slot>)>,
    /// How many scopes are open; nothing is journaled at zero.
    scopes: usize,
    /// The global defines and undefines made while a scope is open (`None`: the
    /// name was undefined), newest last. Closing a scope applies its share to the
    /// scope around it, which the journal has just restored.
    globals: Vec<GlobalChange>,
}

/// Where a scope started, as [`PreprocVarTable::begin_scope`] gives it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ScopeMark {
    journal: usize,
    globals: usize,
}

/// A change to a name of the base: its new entry, or `None` for a name of the
/// base that was undefined.
type Slot = Option<Entry>;

/// A global define (with its value) or undefine (`None`) of a folded name.
type GlobalChange = (Arc<[u8]>, Option<Arc<str>>);

/// The names a table starts with.
#[derive(Debug, Clone)]
pub(crate) struct BaseVars(Arc<HashMap<Arc<[u8]>, Entry, FxBuildHasher>>);

#[derive(Debug, Clone)]
struct Entry {
    value: Arc<str>,
    origin: Origin,
    /// The `&GLOBAL-DEFINE` value held behind a scoped name or an include
    /// argument of the same name. It shows when that entry ends.
    global: Option<Arc<str>>,
}

/// How a name came to be defined; the `DEFINED` function tells them apart.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Origin {
    /// `&GLOBAL-DEFINE` or a predefined name.
    Global,
    /// `&SCOPED-DEFINE`.
    Scoped,
    /// A named argument of the include being expanded.
    Argument,
    /// More than one of the above at once.
    Mixed,
}

/// The longest name folded on the stack; a longer one is folded on the heap.
const STACK_NAME: usize = 64;

/// Run `f` on the ASCII-lowercase form of `name`.
fn with_folded<R>(name: &str, f: impl FnOnce(&[u8]) -> R) -> R {
    let bytes = name.as_bytes();
    if bytes.len() > STACK_NAME {
        return f(&bytes.to_ascii_lowercase());
    }
    let mut buf = [0u8; STACK_NAME];
    let folded = &mut buf[..bytes.len()];
    folded.copy_from_slice(bytes);
    folded.make_ascii_lowercase();
    f(folded)
}

impl PreprocVarTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// An empty table that starts with the names of `base`.
    pub(crate) fn over(base: &BaseVars) -> Self {
        PreprocVarTable {
            base: Some(base.clone()),
            ..Self::default()
        }
    }

    /// The names of this table, for [`Self::over`]. Anything it started from is
    /// not kept.
    pub(crate) fn into_base(self) -> BaseVars {
        let names = self
            .vars
            .into_iter()
            .filter_map(|(key, entry)| Some((key, entry?)))
            .collect();
        BaseVars(Arc::new(names))
    }

    /// Define a preprocessor variable as `&SCOPED-DEFINE` does.
    pub fn define(&mut self, name: &str, value: &str) {
        self.insert(name, value, Origin::Scoped);
    }

    /// Define a preprocessor variable as `&GLOBAL-DEFINE` does.
    pub fn define_global(&mut self, name: &str, value: &str) {
        let value: Arc<str> = Arc::from(value);
        with_folded(name, |key| {
            self.set_global(key, None, Arc::clone(&value));
            if self.scopes > 0
                && let Some((shared, _)) = self.vars.get_key_value(key)
            {
                self.globals.push((Arc::clone(shared), Some(value)));
            }
        });
    }

    /// Define the folded name `key` as a global with `value`. A scoped name or an
    /// include argument keeps its value, and the global waits behind it.
    fn set_global(&mut self, key: &[u8], shared: Option<&Arc<[u8]>>, value: Arc<str>) {
        let entry = match self.entry(key) {
            Some(e) if e.origin != Origin::Global => Entry {
                value: Arc::clone(&e.value),
                origin: Origin::Mixed,
                global: Some(value),
            },
            _ => Entry {
                value,
                origin: Origin::Global,
                global: None,
            },
        };
        self.store(key, shared, Some(entry));
    }

    /// Define a named include argument (`{file.i &name=value}`).
    pub(crate) fn define_argument(&mut self, name: &str, value: &str) {
        self.insert(name, value, Origin::Argument);
    }

    fn insert(&mut self, name: &str, value: &str, origin: Origin) {
        with_folded(name, |key| {
            let prev = self.entry(key);
            let global = prev.and_then(|e| e.global.clone());
            let origin = match prev.map(|e| e.origin) {
                Some(prev) if prev != origin => Origin::Mixed,
                _ => origin,
            };
            let entry = Entry {
                value: Arc::from(value),
                origin,
                global,
            };
            self.store(key, None, Some(entry));
        });
    }

    /// The entry of the folded name `key`, if it is defined.
    fn entry(&self, key: &[u8]) -> Option<&Entry> {
        match self.vars.get(key) {
            Some(changed) => changed.as_ref(),
            None => self.base.as_ref()?.0.get(key),
        }
    }

    /// Set the entry under the folded name `key` (`None` removes it), recording
    /// the old one when a scope is open. `shared` is a ready `Arc` of `key`, to
    /// avoid allocating one for a new name.
    fn store(&mut self, key: &[u8], shared: Option<&Arc<[u8]>>, new: Option<Entry>) {
        // A name of the base is hidden rather than removed.
        let slot = match new {
            Some(entry) => Some(Some(entry)),
            None if self.base.as_ref().is_some_and(|b| b.0.contains_key(key)) => Some(None),
            None => None,
        };
        if self.scopes == 0 {
            match slot {
                Some(slot) => match self.vars.get_mut(key) {
                    Some(current) => *current = slot,
                    None => {
                        let key = shared.map_or_else(|| Arc::from(key), Arc::clone);
                        self.vars.insert(key, slot);
                    }
                },
                None => {
                    self.vars.remove(key);
                }
            }
            return;
        }
        let (key, old) = match self.vars.remove_entry(key) {
            Some((key, old)) => (key, Some(old)),
            None if slot.is_none() => return,
            None => (shared.map_or_else(|| Arc::from(key), Arc::clone), None),
        };
        self.journal.push((Arc::clone(&key), old));
        if let Some(slot) = slot {
            self.vars.insert(key, slot);
        }
    }

    /// Open a scope: every change from here until [`Self::end_scope`] is undone
    /// by it, except the global defines and undefines. Scopes nest.
    pub(crate) fn begin_scope(&mut self) -> ScopeMark {
        self.scopes += 1;
        ScopeMark {
            journal: self.journal.len(),
            globals: self.globals.len(),
        }
    }

    /// Close the scope that [`Self::begin_scope`] opened as `mark`, restoring
    /// the table to how it was then. The names the scope defined with
    /// `&GLOBAL-DEFINE` stay defined, and the ones it undefined stay undefined,
    /// unless the table has a scoped name or an include argument of that name.
    pub(crate) fn end_scope(&mut self, mark: ScopeMark) {
        while self.journal.len() > mark.journal {
            let Some((key, old)) = self.journal.pop() else {
                break;
            };
            match old {
                Some(slot) => {
                    self.vars.insert(key, slot);
                }
                None => {
                    self.vars.remove(&*key);
                }
            }
        }
        self.scopes -= 1;
        for n in mark.globals..self.globals.len() {
            let (key, value) = self.globals[n].clone();
            self.apply_global(&key, value);
        }
        // An outer scope applies them again when it ends; nothing else will.
        if self.scopes == 0 {
            self.globals.clear();
        }
    }

    /// Define the folded name `key` as a global with `value` (`None` undefines
    /// it). A scoped name or an include argument that holds it keeps its value;
    /// the global waits behind it.
    fn apply_global(&mut self, key: &Arc<[u8]>, value: Option<Arc<str>>) {
        let Some(current) = self.entry(key) else {
            if let Some(value) = value {
                self.set_global(key, Some(key), value);
            }
            return;
        };
        let shadowed = current.origin != Origin::Global;
        match value {
            Some(value) => {
                let held = if shadowed {
                    &current.global
                } else {
                    &Some(Arc::clone(&current.value))
                };
                if held.as_ref() != Some(&value) {
                    self.set_global(key, Some(key), value);
                }
            }
            None if !shadowed => self.store(key, None, None),
            None if current.global.is_some() => {
                let entry = Entry {
                    global: None,
                    ..current.clone()
                };
                self.store(key, Some(key), Some(entry));
            }
            None => {}
        }
    }

    /// How `name` was defined; `None` if it is not defined.
    pub(crate) fn origin(&self, name: &str) -> Option<Origin> {
        with_folded(name, |key| self.entry(key).map(|e| e.origin))
    }

    /// Remove a preprocessor variable (from `&UNDEFINE`).
    pub fn undefine(&mut self, name: &str) {
        with_folded(name, |key| {
            let current = self.entry(key);
            if let Some(global) = current.and_then(|e| e.global.clone()) {
                // The shadowing entry ends and the global behind it shows.
                self.store(
                    key,
                    None,
                    Some(Entry {
                        value: global,
                        origin: Origin::Global,
                        global: None,
                    }),
                );
                return;
            }
            if self.scopes > 0 && current.is_some_and(|e| e.origin == Origin::Global) {
                self.globals.push((Arc::from(key), None));
            }
            self.store(key, None, None);
        });
    }

    /// Look up a variable's value. Returns `None` if not defined.
    pub fn get(&self, name: &str) -> Option<&Arc<str>> {
        with_folded(name, |key| self.entry(key).map(|e| &e.value))
    }

    /// Check whether a variable is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        with_folded(name, |key| self.entry(key).is_some())
    }

    /// Copy every variable of `other` into `self`, replacing the ones it holds.
    ///
    /// The preprocessor no longer calls this: it applies the global defines of an
    /// include itself, and a scoped name or an include argument keeps its
    /// precedence over them.
    #[deprecated(note = "the preprocessor applies global defines itself; define each name instead")]
    pub fn merge_globals(&mut self, other: &PreprocVarTable) {
        for (key, theirs) in other.names() {
            let unchanged = self
                .entry(key)
                .is_some_and(|ours| ours.origin == theirs.origin && ours.value == theirs.value);
            if !unchanged {
                self.store(key, Some(key), Some(theirs.clone()));
            }
        }
    }

    /// Every defined name, folded, with its entry.
    fn names(&self) -> impl Iterator<Item = (&Arc<[u8]>, &Entry)> {
        let base = self
            .base
            .iter()
            .flat_map(|base| base.0.iter())
            .filter(|(key, _)| !self.vars.contains_key(&***key));
        let changed = self
            .vars
            .iter()
            .filter_map(|(key, entry)| Some((key, entry.as_ref()?)));
        base.chain(changed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define_and_get() {
        let mut table = PreprocVarTable::new();
        table.define("FOO", "bar");
        assert_eq!(table.get("FOO").map(|v| &**v), Some("bar"));
    }

    #[test]
    fn case_insensitive_lookup() {
        let mut table = PreprocVarTable::new();
        table.define("MyVar", "123");
        assert_eq!(table.get("myvar").map(|v| &**v), Some("123"));
        assert_eq!(table.get("MYVAR").map(|v| &**v), Some("123"));
    }

    #[test]
    fn undefine() {
        let mut table = PreprocVarTable::new();
        table.define("FOO", "bar");
        table.undefine("foo");
        assert!(!table.is_defined("FOO"));
        assert!(table.get("FOO").is_none());
    }

    #[test]
    fn is_defined() {
        let mut table = PreprocVarTable::new();
        assert!(!table.is_defined("X"));
        table.define("X", "1");
        assert!(table.is_defined("X"));
        assert!(table.is_defined("x"));
    }

    #[test]
    #[allow(deprecated)]
    fn merge_globals() {
        let mut parent = PreprocVarTable::new();
        parent.define("A", "1");

        let mut child = PreprocVarTable::new();
        child.define("B", "2");
        child.define("A", "overridden");

        parent.merge_globals(&child);
        assert_eq!(parent.get("A").map(|v| &**v), Some("overridden"));
        assert_eq!(parent.get("B").map(|v| &**v), Some("2"));
    }

    #[test]
    fn end_scope_restores_changed_removed_and_added_names() {
        let mut table = PreprocVarTable::new();
        table.define("KEPT", "1");
        table.define("GONE", "2");
        let outer = table.begin_scope();
        table.define("kept", "changed");
        table.undefine("GONE");
        table.define("ADDED", "3");
        let inner = table.begin_scope();
        table.define("ADDED", "4");
        table.undefine("kept");
        table.end_scope(inner);
        assert_eq!(table.get("ADDED").map(|v| &**v), Some("3"));
        assert_eq!(table.get("KEPT").map(|v| &**v), Some("changed"));
        table.end_scope(outer);
        assert_eq!(table.get("KEPT").map(|v| &**v), Some("1"));
        assert_eq!(table.get("GONE").map(|v| &**v), Some("2"));
        assert!(!table.is_defined("ADDED"));
        assert_eq!(table.origin("KEPT"), Some(Origin::Scoped));
        assert_eq!(table.origin("GONE"), Some(Origin::Scoped));
    }

    fn value<'t>(table: &'t PreprocVarTable, name: &str) -> Option<&'t str> {
        table.get(name).map(|v| &**v)
    }

    #[test]
    fn a_global_define_in_a_scope_outlives_it() {
        let mut table = PreprocVarTable::new();
        table.define_global("G", "old");
        let scope = table.begin_scope();
        table.define_global("G", "new");
        table.define_global("H", "added");
        table.define("S", "scoped");
        table.end_scope(scope);
        assert_eq!(value(&table, "G"), Some("new"));
        assert_eq!(value(&table, "H"), Some("added"));
        assert_eq!(table.origin("H"), Some(Origin::Global));
        assert!(!table.is_defined("S"));
    }

    #[test]
    fn a_global_undefine_in_a_scope_outlives_it() {
        let mut table = PreprocVarTable::new();
        table.define_global("G", "1");
        table.define("S", "1");
        let scope = table.begin_scope();
        table.undefine("G");
        table.undefine("S");
        table.end_scope(scope);
        assert!(!table.is_defined("G"));
        assert_eq!(value(&table, "S"), Some("1"));
    }

    #[test]
    fn the_last_global_change_of_a_scope_wins() {
        let mut table = PreprocVarTable::new();
        let scope = table.begin_scope();
        table.define_global("A", "1");
        table.undefine("A");
        table.define_global("B", "1");
        table.undefine("B");
        table.define_global("B", "2");
        table.end_scope(scope);
        assert!(!table.is_defined("A"));
        assert_eq!(value(&table, "B"), Some("2"));
    }

    #[test]
    fn a_global_from_a_scope_does_not_replace_a_scoped_name_or_an_argument() {
        let mut table = PreprocVarTable::new();
        table.define("S", "scoped");
        table.define_argument("ARG", "argument");
        table.define_global("M", "global");
        table.define("M", "mixed");
        let scope = table.begin_scope();
        for name in ["S", "ARG", "M", "FREE"] {
            table.define_global(name, "global");
        }
        table.end_scope(scope);
        assert_eq!(value(&table, "S"), Some("scoped"));
        assert_eq!(value(&table, "ARG"), Some("argument"));
        assert_eq!(value(&table, "M"), Some("mixed"));
        assert_eq!(value(&table, "FREE"), Some("global"));
        assert_eq!(table.origin("S"), Some(Origin::Mixed));
        assert_eq!(table.origin("ARG"), Some(Origin::Mixed));
        assert_eq!(table.origin("M"), Some(Origin::Mixed));
    }

    #[test]
    fn a_global_undefine_does_not_remove_a_scoped_name_or_an_argument() {
        let mut table = PreprocVarTable::new();
        table.define_global("S", "global");
        let scope = table.begin_scope();
        table.undefine("S");
        table.end_scope(scope);
        assert!(!table.is_defined("S"));

        table.define("S", "scoped");
        table.define_argument("ARG", "argument");
        let scope = table.begin_scope();
        table.undefine("S");
        table.undefine("ARG");
        table.end_scope(scope);
        assert_eq!(value(&table, "S"), Some("scoped"));
        assert_eq!(value(&table, "ARG"), Some("argument"));
    }

    #[test]
    fn a_global_shows_again_where_the_scoped_name_that_hid_it_ends() {
        let mut table = PreprocVarTable::new();
        let outer = table.begin_scope();
        table.define("G", "scoped");
        let middle = table.begin_scope();
        let inner = table.begin_scope();
        table.define_global("G", "global");
        table.end_scope(inner);
        assert_eq!(value(&table, "G"), Some("scoped"));
        table.end_scope(middle);
        assert_eq!(value(&table, "G"), Some("scoped"));
        table.end_scope(outer);
        assert_eq!(value(&table, "G"), Some("global"));
        assert_eq!(table.origin("G"), Some(Origin::Global));
    }

    #[test]
    fn a_global_outlives_every_scope_around_it() {
        let mut table = PreprocVarTable::new();
        let outer = table.begin_scope();
        let inner = table.begin_scope();
        table.define_global("G", "global");
        table.end_scope(inner);
        assert_eq!(value(&table, "G"), Some("global"));
        table.end_scope(outer);
        assert_eq!(value(&table, "G"), Some("global"));
        assert_eq!(table.origin("G"), Some(Origin::Global));
    }

    #[test]
    fn a_global_that_a_scope_outlives_is_not_applied_again_after_the_last_scope() {
        let mut table = PreprocVarTable::new();
        let scope = table.begin_scope();
        table.define_global("G", "1");
        table.end_scope(scope);
        // The root scope redefines it as scoped; a later scope must not undo that.
        table.define("G", "2");
        let scope = table.begin_scope();
        table.end_scope(scope);
        assert_eq!(value(&table, "G"), Some("2"));
        assert_eq!(table.origin("G"), Some(Origin::Mixed));
    }

    #[test]
    fn end_scope_restores_the_origin_of_a_redefined_name() {
        let mut table = PreprocVarTable::new();
        table.define("X", "1");
        let scope = table.begin_scope();
        table.define_global("X", "2");
        assert_eq!(table.origin("X"), Some(Origin::Mixed));
        table.end_scope(scope);
        // The global of the scope is applied again behind the scoped name.
        assert_eq!(value(&table, "X"), Some("1"));
        assert_eq!(table.origin("X"), Some(Origin::Mixed));
    }

    #[test]
    fn a_global_define_over_a_scoped_name_or_an_argument_waits_behind_it() {
        let mut table = PreprocVarTable::new();
        table.define("S", "scoped");
        table.define_argument("ARG", "argument");
        table.define_global("S", "global");
        table.define_global("ARG", "global");
        assert_eq!(value(&table, "S"), Some("scoped"));
        assert_eq!(value(&table, "ARG"), Some("argument"));
        assert_eq!(table.origin("S"), Some(Origin::Mixed));
        assert_eq!(table.origin("ARG"), Some(Origin::Mixed));
        table.undefine("S");
        table.undefine("ARG");
        assert_eq!(value(&table, "S"), Some("global"));
        assert_eq!(value(&table, "ARG"), Some("global"));
        assert_eq!(table.origin("S"), Some(Origin::Global));
    }

    #[test]
    fn a_global_behind_a_scoped_name_shows_when_the_scope_of_the_name_ends() {
        let mut table = PreprocVarTable::new();
        let scope = table.begin_scope();
        table.define("S", "scoped");
        table.define_global("S", "global");
        assert_eq!(value(&table, "S"), Some("scoped"));
        table.end_scope(scope);
        assert_eq!(value(&table, "S"), Some("global"));
        assert_eq!(table.origin("S"), Some(Origin::Global));
    }

    #[test]
    fn a_later_define_keeps_the_global_behind_it() {
        let mut table = PreprocVarTable::new();
        table.define("S", "1");
        table.define_global("S", "global");
        table.define("S", "2");
        assert_eq!(value(&table, "S"), Some("2"));
        table.undefine("S");
        assert_eq!(value(&table, "S"), Some("global"));
    }

    #[test]
    fn a_name_longer_than_the_stack_buffer_is_case_insensitive() {
        let name = "Long-Name".repeat(12);
        let mut table = PreprocVarTable::new();
        table.define(&name, "v");
        assert_eq!(
            table.get(&name.to_ascii_uppercase()).map(|v| &**v),
            Some("v")
        );
    }

    #[test]
    #[allow(deprecated)]
    fn merge_globals_inside_a_scope_is_undone_with_it() {
        let mut globals = PreprocVarTable::new();
        globals.define_global("G", "1");
        let mut table = PreprocVarTable::new();
        let scope = table.begin_scope();
        table.merge_globals(&globals);
        assert!(table.is_defined("g"));
        table.end_scope(scope);
        assert!(!table.is_defined("g"));
    }

    fn base_of(names: &[(&str, &str)]) -> BaseVars {
        let mut table = PreprocVarTable::new();
        for (name, value) in names {
            table.define_global(name, value);
        }
        table.into_base()
    }

    #[test]
    fn a_table_over_a_base_starts_with_its_names() {
        let base = base_of(&[("OPSYS", "UNIX")]);
        let table = PreprocVarTable::over(&base);
        assert_eq!(table.get("opsys").map(|v| &**v), Some("UNIX"));
        assert!(table.is_defined("OpSys"));
        assert_eq!(table.origin("OPSYS"), Some(Origin::Global));
    }

    #[test]
    fn undefining_a_base_name_hides_it_until_it_is_defined_again() {
        let base = base_of(&[("OPSYS", "UNIX")]);
        let mut table = PreprocVarTable::over(&base);
        table.undefine("opsys");
        assert!(!table.is_defined("OPSYS"));
        assert!(table.get("OPSYS").is_none());
        table.define("OPSYS", "mine");
        assert_eq!(table.origin("OPSYS"), Some(Origin::Scoped));
        assert_eq!(table.get("OPSYS").map(|v| &**v), Some("mine"));
    }

    #[test]
    fn defining_over_a_base_name_mixes_the_origin() {
        let base = base_of(&[("OPSYS", "UNIX")]);
        let mut table = PreprocVarTable::over(&base);
        table.define("OPSYS", "mine");
        assert_eq!(table.origin("OPSYS"), Some(Origin::Mixed));
    }

    #[test]
    fn end_scope_restores_a_base_name_that_was_changed() {
        let base = base_of(&[("A", "1"), ("B", "2")]);
        let mut table = PreprocVarTable::over(&base);
        let scope = table.begin_scope();
        table.define("B", "changed");
        table.define("C", "3");
        table.end_scope(scope);
        assert_eq!(table.get("A").map(|v| &**v), Some("1"));
        assert_eq!(table.get("B").map(|v| &**v), Some("2"));
        assert_eq!(table.origin("B"), Some(Origin::Global));
        assert!(!table.is_defined("C"));
    }

    #[test]
    fn a_base_name_that_a_scope_undefines_stays_hidden() {
        let base = base_of(&[("A", "1")]);
        let mut table = PreprocVarTable::over(&base);
        let scope = table.begin_scope();
        table.undefine("A");
        table.end_scope(scope);
        assert!(!table.is_defined("A"));
    }

    #[test]
    fn tables_over_one_base_do_not_see_each_others_changes() {
        let base = base_of(&[("A", "1")]);
        let mut first = PreprocVarTable::over(&base);
        first.undefine("A");
        first.define("B", "2");
        let second = PreprocVarTable::over(&base);
        assert_eq!(second.get("A").map(|v| &**v), Some("1"));
        assert!(!second.is_defined("B"));
    }

    #[test]
    #[allow(deprecated)]
    fn merge_globals_carries_the_names_of_a_base_too() {
        let base = base_of(&[("A", "1")]);
        let from = PreprocVarTable::over(&base);
        let mut table = PreprocVarTable::new();
        table.merge_globals(&from);
        assert_eq!(table.get("A").map(|v| &**v), Some("1"));
    }
}
