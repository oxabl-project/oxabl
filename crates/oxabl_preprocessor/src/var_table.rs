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
    vars: HashMap<AsciiLowerName, Arc<str>>,
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

    /// Define a preprocessor variable (from `&SCOPED-DEFINE` or `&GLOBAL-DEFINE`).
    pub fn define(&mut self, name: &str, value: &str) {
        self.vars
            .insert(AsciiLowerName::new(name), Arc::from(value));
    }

    /// Remove a preprocessor variable (from `&UNDEFINE`).
    pub fn undefine(&mut self, name: &str) {
        self.vars.remove(&AsciiLowerName::new(name));
    }

    /// Look up a variable's value. Returns `None` if not defined.
    pub fn get(&self, name: &str) -> Option<&Arc<str>> {
        self.vars.get(&AsciiLowerName::new(name))
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
