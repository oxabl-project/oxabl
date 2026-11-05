use glob::{MatchOptions, Pattern};

const OPTIONS: MatchOptions = MatchOptions {
    case_sensitive: true,
    require_literal_separator: true,
    require_literal_leading_dot: false,
};

/// The `[build].exclude` globs. A pattern that matches a directory excludes
/// every file under it.
#[derive(Debug, Default)]
pub struct Excludes {
    patterns: Vec<Pattern>,
}

impl Excludes {
    /// Compile `patterns`, each relative to the root.
    pub fn new(patterns: &[String]) -> Result<Self, String> {
        let patterns = patterns
            .iter()
            .map(|pattern| {
                Pattern::new(pattern.trim_start_matches("./"))
                    .map_err(|e| format!("[build].exclude: `{pattern}` is not a glob: {e}"))
            })
            .collect::<Result<_, _>>()?;
        Ok(Excludes { patterns })
    }

    /// Whether the canonical (root-relative) path `path` is excluded.
    pub fn excludes(&self, path: &str) -> bool {
        if self.patterns.is_empty() {
            return false;
        }
        let mut end = 0;
        for part in path.split('/') {
            end += part.len();
            let prefix = &path[..end];
            if self
                .patterns
                .iter()
                .any(|p| p.matches_with(prefix, OPTIONS))
            {
                return true;
            }
            end += 1;
        }
        false
    }
}
