//! The worker command template.

use std::collections::BTreeMap;

/// The values a placeholder takes.
pub struct Values<'a> {
    pub program: &'a str,
    pub root: &'a str,
    pub worker: usize,
    pub build: &'a str,
}

const PLACEHOLDERS: [&str; 4] = ["program", "root", "worker", "build"];

fn expand_arg(arg: &str, values: Option<&BTreeMap<&str, String>>) -> Result<String, String> {
    let mut out = String::new();
    let mut rest = arg;
    while let Some(at) = rest.find('{') {
        out.push_str(&rest[..at]);
        rest = &rest[at + 1..];
        if let Some(after) = rest.strip_prefix('{') {
            out.push('{');
            rest = after;
            continue;
        }
        let end = rest.find('}').ok_or_else(|| {
            format!("`{arg}` has a `{{` with no closing `}}`; write `{{{{` for a literal `{{`")
        })?;
        let name = &rest[..end];
        if !PLACEHOLDERS.contains(&name) {
            return Err(format!(
                "`{{{name}}}` is not a placeholder; use {{program}}, {{root}}, {{worker}} or {{build}}"
            ));
        }
        if let Some(values) = values {
            out.push_str(&values[name]);
        }
        rest = &rest[end + 1..];
    }
    out.push_str(rest);
    Ok(out)
}

/// Checks that the command is not empty and uses only known placeholders.
pub fn validate(argv: &[String]) -> Result<(), String> {
    if argv.first().is_none_or(|program| program.is_empty()) {
        return Err("must name a program".into());
    }
    argv.iter()
        .try_for_each(|arg| expand_arg(arg, None).map(|_| ()))
}

/// The argument list with every placeholder replaced.
pub fn expand(argv: &[String], values: &Values<'_>) -> Result<Vec<String>, String> {
    let map = BTreeMap::from([
        ("program", values.program.to_string()),
        ("root", values.root.to_string()),
        ("worker", values.worker.to_string()),
        ("build", values.build.to_string()),
    ]);
    argv.iter().map(|arg| expand_arg(arg, Some(&map))).collect()
}
