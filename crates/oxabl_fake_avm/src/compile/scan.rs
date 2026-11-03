//! What a source text refers to and which faults it asks for.

/// A fault a source asks for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Fault {
    Fail,
    FailOnce,
    Warn,
    Crash,
    CrashOnce,
    Hang,
    Slow,
    NoOutput,
}

/// The references and faults of one text.
#[derive(Default)]
pub struct Scan {
    /// Include names, from `{name}` and `{name args}`.
    pub includes: Vec<String>,
    /// Class names, from `INHERITS`, `IMPLEMENTS` and `NEW`.
    pub classes: Vec<String>,
    /// Paths named by `FAKE-READ`.
    pub reads: Vec<String>,
    /// Each fault with the line it is on, counted from 1.
    pub faults: Vec<(Fault, usize)>,
}

const FAULTS: [(&str, Fault); 8] = [
    ("FAKE-CRASH-ONCE", Fault::CrashOnce),
    ("FAKE-FAIL-ONCE", Fault::FailOnce),
    ("FAKE-CRASH", Fault::Crash),
    ("FAKE-FAIL", Fault::Fail),
    ("FAKE-WARN", Fault::Warn),
    ("FAKE-HANG", Fault::Hang),
    ("FAKE-SLOW", Fault::Slow),
    ("FAKE-NO-OUTPUT", Fault::NoOutput),
];

fn include_name(text: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut rest = text;
    while let Some(open) = rest.find('{') {
        let after = &rest[open + 1..];
        let Some(close) = after.find('}') else { break };
        let first = after[..close].split_whitespace().next().unwrap_or("");
        let usable = !first.is_empty()
            && !first.starts_with(['&', '{', '*'])
            && !first.chars().next().is_some_and(|c| c.is_ascii_digit())
            && !first.contains('&');
        if usable {
            names.push(first.to_string());
        }
        rest = &after[close + 1..];
    }
    names
}

fn class_name(word: &str) -> Option<String> {
    let name = word
        .split(['(', ',', ':'])
        .next()
        .unwrap_or("")
        .trim_end_matches('.');
    (!name.is_empty()).then(|| name.to_string())
}

fn classes_of(line: &str) -> Vec<String> {
    let mut classes = Vec::new();
    let words: Vec<&str> = line.split_whitespace().collect();
    for (at, word) in words.iter().enumerate() {
        match word.to_ascii_uppercase().as_str() {
            "INHERITS" | "NEW" => {
                classes.extend(words.get(at + 1).and_then(|next| class_name(next)));
            }
            "IMPLEMENTS" => {
                let rest = words[at + 1..].join(" ");
                classes.extend(rest.split(',').filter_map(|part| class_name(part.trim())));
            }
            _ => {}
        }
    }
    classes
}

pub fn scan(text: &str) -> Scan {
    let mut found = Scan::default();
    for (index, line) in text.lines().enumerate() {
        for (marker, fault) in FAULTS {
            if line.contains(marker) {
                found.faults.push((fault, index + 1));
                break;
            }
        }
        if let Some((_, path)) = line.split_once("FAKE-READ") {
            let path = path.trim().trim_end_matches("*/").trim();
            found.reads.push(path.to_string());
        }
        found.includes.extend(include_name(line));
        found.classes.extend(classes_of(line));
    }
    found
}
