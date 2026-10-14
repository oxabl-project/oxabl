use std::collections::HashMap;
use std::sync::Arc;

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
    vars: HashMap<AsciiLowerName, Entry>,
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

/// Case-insensitive key: stores the lowercase form for hashing/comparison.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct AsciiLowerName(String);

impl AsciiLowerName {
    fn new(name: &str) -> Self {
        AsciiLowerName(name.to_ascii_lowercase())
    }
}

impl PreprocVarTable {
    pub fn new() -> Self {
        Self::default()
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
        let key = AsciiLowerName::new(name);
        let origin = match self.vars.get(&key).map(|e| e.origin) {
            Some(prev) if prev != origin => Origin::Mixed,
            _ => origin,
        };
        self.vars.insert(
            key,
            Entry {
                value: Arc::from(value),
                origin,
            },
        );
    }

    /// How `name` was defined; `None` if it is not defined.
    pub(crate) fn origin(&self, name: &str) -> Option<Origin> {
        self.vars.get(&AsciiLowerName::new(name)).map(|e| e.origin)
    }

    /// Remove a preprocessor variable (from `&UNDEFINE`).
    pub fn undefine(&mut self, name: &str) {
        self.vars.remove(&AsciiLowerName::new(name));
    }

    /// Look up a variable's value. Returns `None` if not defined.
    pub fn get(&self, name: &str) -> Option<&Arc<str>> {
        self.vars.get(&AsciiLowerName::new(name)).map(|e| &e.value)
    }

    /// Check whether a variable is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.vars.contains_key(&AsciiLowerName::new(name))
    }

    /// Merge all variables from `other` into `self`.
    ///
    /// Used to propagate `&GLOBAL-DEFINE` variables from an included file
    /// back to the parent.
    pub fn merge_globals(&mut self, other: &PreprocVarTable) {
        for (k, v) in &other.vars {
            self.vars.insert(k.clone(), v.clone());
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
}
