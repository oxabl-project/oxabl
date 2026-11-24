use std::collections::HashMap;
use std::sync::Arc;

use rustc_hash::FxBuildHasher;

/// Preprocessor variable table tracking `&SCOPED-DEFINE`, `&GLOBAL-DEFINE` and
/// include-argument values, as the AVM keeps them.
///
/// Each name holds a stack of definitions, and the newest one is the visible
/// one: `{&name}` and `DEFINED(name)` read it. A definition hides the one under
/// it until it ends.
/// - `&SCOPED-DEFINE` and an include argument end with the file that made them.
///   A second `&SCOPED-DEFINE` of a name in the same file replaces the first.
/// - `&GLOBAL-DEFINE` never ends, and `&UNDEFINE` removes the visible
///   definition wherever it came from, in any file. A global define of a name
///   that has a scoped name or an include argument is an error in the AVM and
///   is ignored here.
///
/// All lookups are case-insensitive (ABL preprocessor variables are
/// case-insensitive).
#[derive(Debug, Clone, Default)]
pub struct PreprocVarTable {
    /// The names the table started with, shared by every table made over them.
    base: Option<BaseVars>,
    /// The changes made to `base`, keyed by the lowercase form of the name.
    vars: HashMap<Arc<[u8]>, Slot, FxBuildHasher>,
    /// The names that open scopes defined as scoped names or include arguments,
    /// newest last. Closing a scope ends what it defined.
    frame: Vec<Arc<[u8]>>,
    /// How many scopes are open.
    scopes: u32,
}

/// Where a scope started, as [`PreprocVarTable::begin_scope`] gives it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ScopeMark {
    frame: usize,
}

/// A change to a name of the base: its new entry, or `None` for a name of the
/// base that was undefined.
type Slot = Option<Entry>;

/// The names a table starts with.
#[derive(Debug, Clone)]
pub(crate) struct BaseVars(Arc<HashMap<Arc<[u8]>, Entry, FxBuildHasher>>);

#[derive(Debug, Clone)]
struct Entry {
    value: Arc<str>,
    origin: Origin,
    /// The scope that made the entry; it ends with that scope. A global define
    /// is at zero and does not end.
    depth: u32,
    /// The definition this one hides.
    behind: Option<Arc<Entry>>,
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
        self.push(name, value, Origin::Scoped);
    }

    /// Define a preprocessor variable as `&GLOBAL-DEFINE` does.
    pub fn define_global(&mut self, name: &str, value: &str) {
        with_folded(name, |key| match self.entry(key) {
            None => self.store(
                key,
                None,
                Some(Entry {
                    value: Arc::from(value),
                    origin: Origin::Global,
                    depth: 0,
                    behind: None,
                }),
            ),
            Some(current) if current.origin == Origin::Global => {
                let entry = Entry {
                    value: Arc::from(value),
                    ..current.clone()
                };
                self.store(key, None, Some(entry));
            }
            // The AVM rejects a global define over a scoped name or an argument.
            Some(_) => {}
        });
    }

    /// Define a named include argument (`{file.i &name=value}`).
    pub(crate) fn define_argument(&mut self, name: &str, value: &str) {
        self.push(name, value, Origin::Argument);
    }

    /// Define `name` as a scoped name or an include argument of the open scope.
    fn push(&mut self, name: &str, value: &str, origin: Origin) {
        with_folded(name, |key| {
            let depth = self.scopes;
            let entry = match self.entry(key) {
                // The same file defines it again: the new value replaces the old.
                Some(e) if e.origin == origin && e.depth == depth => Entry {
                    value: Arc::from(value),
                    ..e.clone()
                },
                prev => Entry {
                    value: Arc::from(value),
                    origin,
                    depth,
                    behind: prev.map(|e| Arc::new(e.clone())),
                },
            };
            self.store(key, None, Some(entry));
            if depth > 0 {
                self.record(key);
            }
        });
    }

    /// Note that the open scope defined the folded name `key`.
    fn record(&mut self, key: &[u8]) {
        if let Some((shared, _)) = self.vars.get_key_value(key) {
            self.frame.push(Arc::clone(shared));
        }
    }

    /// The entry of the folded name `key`, if it is defined.
    fn entry(&self, key: &[u8]) -> Option<&Entry> {
        match self.vars.get(key) {
            Some(changed) => changed.as_ref(),
            None => self.base.as_ref()?.0.get(key),
        }
    }

    /// Set the entry under the folded name `key` (`None` removes it). `shared`
    /// is a ready `Arc` of `key`, to avoid allocating one for a new name.
    fn store(&mut self, key: &[u8], shared: Option<&Arc<[u8]>>, new: Option<Entry>) {
        match new {
            Some(entry) => match self.vars.get_mut(key) {
                Some(current) => *current = Some(entry),
                None => {
                    let key = shared.map_or_else(|| Arc::from(key), Arc::clone);
                    self.vars.insert(key, Some(entry));
                }
            },
            // A name of the base is hidden rather than removed.
            None if self.base.as_ref().is_some_and(|b| b.0.contains_key(key)) => {
                match self.vars.get_mut(key) {
                    Some(current) => *current = None,
                    None => {
                        let key = shared.map_or_else(|| Arc::from(key), Arc::clone);
                        self.vars.insert(key, None);
                    }
                }
            }
            None => {
                self.vars.remove(key);
            }
        }
    }

    /// Remove the visible definition of the folded name `key`; the one it hid
    /// shows again.
    fn pop(&mut self, key: &[u8], shared: Option<&Arc<[u8]>>) {
        let below = self.entry(key).and_then(|e| e.behind.as_deref().cloned());
        self.store(key, shared, below);
    }

    /// Open a scope: what it defines as a scoped name or an include argument
    /// ends with [`Self::end_scope`]. Scopes nest.
    pub(crate) fn begin_scope(&mut self) -> ScopeMark {
        self.scopes += 1;
        ScopeMark {
            frame: self.frame.len(),
        }
    }

    /// Close the scope that [`Self::begin_scope`] opened as `mark`. What the
    /// scope defined ends. What it defined globally stays, and so does what it
    /// undefined, whoever defined it.
    pub(crate) fn end_scope(&mut self, mark: ScopeMark) {
        let depth = self.scopes;
        self.scopes -= 1;
        while self.frame.len() > mark.frame {
            let Some(key) = self.frame.pop() else {
                break;
            };
            while self.entry(&key).is_some_and(|e| e.depth == depth) {
                self.pop(&key, Some(&key));
            }
        }
    }

    /// How `name` is defined now; `None` if it is not defined.
    pub(crate) fn origin(&self, name: &str) -> Option<Origin> {
        with_folded(name, |key| self.entry(key).map(|e| e.origin))
    }

    /// Remove a preprocessor variable (from `&UNDEFINE`): the visible
    /// definition of the name ends, and an older one shows again.
    pub fn undefine(&mut self, name: &str) {
        with_folded(name, |key| self.pop(key, None));
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
    /// The preprocessor no longer calls this: it defines each name itself.
    #[deprecated(note = "the preprocessor defines each name itself; define each name instead")]
    pub fn merge_globals(&mut self, other: &PreprocVarTable) {
        let depth = self.scopes;
        for (key, theirs) in other.names() {
            let unchanged = self
                .entry(key)
                .is_some_and(|ours| ours.origin == theirs.origin && ours.value == theirs.value);
            if !unchanged {
                let entry = Entry {
                    depth,
                    ..theirs.clone()
                };
                self.store(key, Some(key), Some(entry));
                if depth > 0 {
                    self.frame.push(Arc::clone(key));
                }
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
    fn end_scope_ends_what_the_scope_defined_and_restores_what_it_hid() {
        let mut table = PreprocVarTable::new();
        table.define("KEPT", "1");
        table.define("GONE", "2");
        let outer = table.begin_scope();
        table.define("kept", "changed");
        table.define("ADDED", "3");
        let inner = table.begin_scope();
        table.define("ADDED", "4");
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
        let scope = table.begin_scope();
        table.undefine("G");
        table.end_scope(scope);
        assert!(!table.is_defined("G"));
    }

    #[test]
    fn an_undefine_in_a_scope_removes_a_scoped_name_of_the_file_around_it() {
        let mut table = PreprocVarTable::new();
        table.define("S", "scoped");
        let scope = table.begin_scope();
        table.undefine("S");
        assert!(!table.is_defined("S"));
        table.end_scope(scope);
        assert!(!table.is_defined("S"));
    }

    #[test]
    fn what_a_scope_defines_after_an_undefine_ends_with_it() {
        let mut table = PreprocVarTable::new();
        table.define("S", "outer");
        let scope = table.begin_scope();
        table.undefine("S");
        table.define("S", "inner");
        assert_eq!(value(&table, "S"), Some("inner"));
        table.end_scope(scope);
        assert!(!table.is_defined("S"));
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
    fn a_global_define_over_a_scoped_name_or_an_argument_is_ignored() {
        let mut table = PreprocVarTable::new();
        table.define("S", "scoped");
        table.define_argument("ARG", "argument");
        table.define_global("M", "global");
        table.define("M", "scoped");
        for name in ["S", "ARG", "M"] {
            table.define_global(name, "ignored");
        }
        assert_eq!(value(&table, "S"), Some("scoped"));
        assert_eq!(value(&table, "ARG"), Some("argument"));
        assert_eq!(value(&table, "M"), Some("scoped"));
        assert_eq!(table.origin("S"), Some(Origin::Scoped));
        assert_eq!(table.origin("ARG"), Some(Origin::Argument));
        table.undefine("S");
        table.undefine("ARG");
        table.undefine("M");
        assert!(!table.is_defined("S"));
        assert!(!table.is_defined("ARG"));
        assert_eq!(value(&table, "M"), Some("global"));
        assert_eq!(table.origin("M"), Some(Origin::Global));
    }

    #[test]
    fn the_visible_definition_gives_the_origin() {
        let mut table = PreprocVarTable::new();
        table.define_global("N", "g");
        assert_eq!(table.origin("N"), Some(Origin::Global));
        table.define("N", "s");
        assert_eq!(table.origin("N"), Some(Origin::Scoped));
        let scope = table.begin_scope();
        table.define_argument("N", "a");
        assert_eq!(table.origin("N"), Some(Origin::Argument));
        table.define("N", "s2");
        assert_eq!(table.origin("N"), Some(Origin::Scoped));
        table.undefine("N");
        assert_eq!(table.origin("N"), Some(Origin::Argument));
        assert_eq!(value(&table, "N"), Some("a"));
        table.end_scope(scope);
        assert_eq!(table.origin("N"), Some(Origin::Scoped));
        assert_eq!(value(&table, "N"), Some("s"));
        table.undefine("N");
        assert_eq!(table.origin("N"), Some(Origin::Global));
        assert_eq!(value(&table, "N"), Some("g"));
        table.undefine("N");
        assert_eq!(table.origin("N"), None);
    }

    #[test]
    fn a_scoped_define_replaces_one_of_the_same_file_only() {
        let mut table = PreprocVarTable::new();
        table.define("S", "1");
        table.define("S", "2");
        let scope = table.begin_scope();
        table.define("S", "3");
        table.define("S", "4");
        assert_eq!(value(&table, "S"), Some("4"));
        table.undefine("S");
        assert_eq!(value(&table, "S"), Some("2"));
        table.define("S", "5");
        table.end_scope(scope);
        assert_eq!(value(&table, "S"), Some("2"));
        table.undefine("S");
        assert!(!table.is_defined("S"));
    }

    #[test]
    fn a_global_shows_again_where_the_scoped_name_that_hid_it_ends() {
        let mut table = PreprocVarTable::new();
        table.define_global("G", "global");
        let outer = table.begin_scope();
        table.define("G", "scoped");
        let inner = table.begin_scope();
        table.define("G", "deeper");
        table.end_scope(inner);
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

    /// The ADM `xp-assign` pattern: a scoped name of the file, a global define
    /// after an undefine in an include.
    #[test]
    fn a_global_defined_after_undefining_the_scoped_name_of_the_file_around_is_global() {
        let mut table = PreprocVarTable::new();
        let file = table.begin_scope();
        table.define("X", "");
        assert_eq!(table.origin("X"), Some(Origin::Scoped));
        let include = table.begin_scope();
        table.undefine("X");
        table.define_global("X", "");
        table.end_scope(include);
        assert_eq!(table.origin("X"), Some(Origin::Global));
        let include = table.begin_scope();
        assert_eq!(table.origin("X"), Some(Origin::Global));
        table.end_scope(include);
        table.undefine("X");
        assert_eq!(table.origin("X"), None);
        table.end_scope(file);
        assert_eq!(table.origin("X"), None);
    }

    #[test]
    fn an_argument_and_a_scoped_name_in_the_same_scope_are_two_definitions() {
        let mut table = PreprocVarTable::new();
        let scope = table.begin_scope();
        table.define_argument("A", "argument");
        table.define("A", "scoped");
        table.undefine("A");
        assert_eq!(value(&table, "A"), Some("argument"));
        assert_eq!(table.origin("A"), Some(Origin::Argument));
        table.end_scope(scope);
        assert!(!table.is_defined("A"));
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
    fn defining_over_a_base_name_hides_it_until_the_name_is_undefined() {
        let base = base_of(&[("OPSYS", "UNIX")]);
        let mut table = PreprocVarTable::over(&base);
        table.define("OPSYS", "mine");
        assert_eq!(table.origin("OPSYS"), Some(Origin::Scoped));
        table.undefine("OPSYS");
        assert_eq!(table.origin("OPSYS"), Some(Origin::Global));
        assert_eq!(table.get("OPSYS").map(|v| &**v), Some("UNIX"));
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
