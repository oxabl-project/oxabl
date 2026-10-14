use std::collections::HashMap;
use std::sync::Arc;

use rustc_hash::FxBuildHasher;

/// Preprocessor variable table tracking `&SCOPED-DEFINE` and `&GLOBAL-DEFINE` values.
///
/// `&SCOPED-DEFINE` variables are file-local — they do not propagate to the
/// file that included them. `&GLOBAL-DEFINE` variables propagate upward to all
/// callers in the include chain.
///
/// All lookups are case-insensitive (ABL preprocessor variables are
/// case-insensitive).
#[derive(Debug, Clone, Default)]
pub struct PreprocVarTable {
    /// Keyed by the lowercase form of the name.
    vars: HashMap<Arc<str>, Entry, FxBuildHasher>,
    /// What each name held before it changed, newest last, while a scope is
    /// open. Undoing it restores the table without copying it.
    journal: Vec<(Arc<str>, Option<Entry>)>,
    /// How many scopes are open; nothing is journaled at zero.
    scopes: usize,
}

#[derive(Debug, Clone)]
struct Entry {
    value: Arc<str>,
    origin: Origin,
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
fn with_folded<R>(name: &str, f: impl FnOnce(&str) -> R) -> R {
    let bytes = name.as_bytes();
    if bytes.len() > STACK_NAME {
        return f(&name.to_ascii_lowercase());
    }
    let mut buf = [0u8; STACK_NAME];
    let folded = &mut buf[..bytes.len()];
    folded.copy_from_slice(bytes);
    folded.make_ascii_lowercase();
    // Folding ASCII letters keeps valid UTF-8 valid; `name` is a `str`.
    f(std::str::from_utf8(folded).unwrap_or(name))
}

impl PreprocVarTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// An empty table with room for `capacity` names.
    pub(crate) fn with_capacity(capacity: usize) -> Self {
        PreprocVarTable {
            vars: HashMap::with_capacity_and_hasher(capacity, FxBuildHasher),
            ..Self::default()
        }
    }

    /// Define a preprocessor variable as `&SCOPED-DEFINE` does.
    pub fn define(&mut self, name: &str, value: &str) {
        self.insert(name, value, Origin::Scoped);
    }

    /// Define a preprocessor variable as `&GLOBAL-DEFINE` does.
    pub fn define_global(&mut self, name: &str, value: &str) {
        self.insert(name, value, Origin::Global);
    }

    /// Define a named include argument (`{file.i &name=value}`).
    pub(crate) fn define_argument(&mut self, name: &str, value: &str) {
        self.insert(name, value, Origin::Argument);
    }

    fn insert(&mut self, name: &str, value: &str, origin: Origin) {
        with_folded(name, |key| {
            let origin = match self.vars.get(key).map(|e| e.origin) {
                Some(prev) if prev != origin => Origin::Mixed,
                _ => origin,
            };
            let entry = Entry {
                value: Arc::from(value),
                origin,
            };
            self.store(key, None, Some(entry));
        });
    }

    /// Set the entry under the folded name `key` (`None` removes it), recording
    /// the old one when a scope is open. `shared` is a ready `Arc` of `key`, to
    /// avoid allocating one for a new name.
    fn store(&mut self, key: &str, shared: Option<&Arc<str>>, new: Option<Entry>) {
        if self.scopes == 0 {
            match new {
                Some(entry) => match self.vars.get_mut(key) {
                    Some(slot) => *slot = entry,
                    None => {
                        let key = shared.map_or_else(|| Arc::from(key), Arc::clone);
                        self.vars.insert(key, entry);
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
            None if new.is_none() => return,
            None => (shared.map_or_else(|| Arc::from(key), Arc::clone), None),
        };
        self.journal.push((Arc::clone(&key), old));
        if let Some(entry) = new {
            self.vars.insert(key, entry);
        }
    }

    /// Open a scope: every change from here until [`Self::end_scope`] is undone
    /// by it. Scopes nest.
    pub(crate) fn begin_scope(&mut self) -> usize {
        self.scopes += 1;
        self.journal.len()
    }

    /// Close the scope that [`Self::begin_scope`] opened as `mark`, restoring
    /// the table to how it was then.
    pub(crate) fn end_scope(&mut self, mark: usize) {
        while self.journal.len() > mark {
            let Some((key, old)) = self.journal.pop() else {
                break;
            };
            match old {
                Some(entry) => {
                    self.vars.insert(key, entry);
                }
                None => {
                    self.vars.remove(&*key);
                }
            }
        }
        self.scopes -= 1;
    }

    /// How `name` was defined; `None` if it is not defined.
    pub(crate) fn origin(&self, name: &str) -> Option<Origin> {
        with_folded(name, |key| self.vars.get(key).map(|e| e.origin))
    }

    /// Remove a preprocessor variable (from `&UNDEFINE`).
    pub fn undefine(&mut self, name: &str) {
        with_folded(name, |key| self.store(key, None, None));
    }

    /// Look up a variable's value. Returns `None` if not defined.
    pub fn get(&self, name: &str) -> Option<&Arc<str>> {
        with_folded(name, |key| self.vars.get(key).map(|e| &e.value))
    }

    /// Check whether a variable is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        with_folded(name, |key| self.vars.contains_key(key))
    }

    /// Merge all variables from `other` into `self`.
    ///
    /// Used to propagate `&GLOBAL-DEFINE` variables from an included file
    /// back to the parent.
    pub fn merge_globals(&mut self, other: &PreprocVarTable) {
        for (key, theirs) in &other.vars {
            let unchanged = self
                .vars
                .get(&**key)
                .is_some_and(|ours| ours.origin == theirs.origin && ours.value == theirs.value);
            if !unchanged {
                self.store(key, Some(key), Some(theirs.clone()));
            }
        }
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
        table.define_global("GONE", "2");
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
        assert_eq!(table.origin("GONE"), Some(Origin::Global));
    }

    #[test]
    fn end_scope_restores_the_origin_of_a_redefined_name() {
        let mut table = PreprocVarTable::new();
        table.define("X", "1");
        let scope = table.begin_scope();
        table.define_global("X", "2");
        assert_eq!(table.origin("X"), Some(Origin::Mixed));
        table.end_scope(scope);
        assert_eq!(table.origin("X"), Some(Origin::Scoped));
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
}
