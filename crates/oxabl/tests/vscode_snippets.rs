//! The snippets of the VS Code extension (`clients/vscode/snippets/abl.json`)
//! are templates. After the keyword markers and the placeholders are filled,
//! every body must be ABL that oxabl parses, in both keyword cases.

use std::collections::BTreeMap;
use std::path::PathBuf;

use oxabl::lexer::Kind;
use oxabl::style::KeywordCase;
use oxabl::workspace::WorkspaceConfig;
use serde_json::Value;

const PREFIXES: [&str; 9] = [
    "find", "foreach", "defvar", "deftt", "proc", "function", "method", "class", "trycatch",
];

struct Snippet {
    prefix: String,
    body: String,
}

fn load() -> Vec<Snippet> {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../clients/vscode/snippets/abl.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let doc: Value = serde_json::from_str(&text).expect("snippets file is JSON");
    doc.as_object()
        .expect("snippets file is an object")
        .iter()
        .map(|(prefix, entry)| {
            let body = match &entry["body"] {
                Value::String(s) => s.clone(),
                Value::Array(lines) => lines
                    .iter()
                    .map(|l| l.as_str().expect("body line is a string"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                other => panic!("{prefix}: body is {other}"),
            };
            Snippet {
                prefix: prefix.clone(),
                body,
            }
        })
        .collect()
}

/// Replace each `<<KEYWORDS>>` marker with its words in the chosen case.
fn render_keywords(body: &str, lower: bool) -> String {
    let mut out = String::new();
    let mut rest = body;
    while let Some(open) = rest.find("<<") {
        out.push_str(&rest[..open]);
        let after = &rest[open + 2..];
        let close = after.find(">>").expect("unclosed keyword marker");
        let words = &after[..close];
        out.push_str(&if lower {
            words.to_lowercase()
        } else {
            words.to_string()
        });
        rest = &after[close + 2..];
    }
    out.push_str(rest);
    out
}

/// Fill the placeholders: the default text, the first choice, and nothing for
/// a bare tab stop. Every use of one number must give the same text.
fn fill_placeholders(prefix: &str, body: &str) -> String {
    fill(prefix, body, false)
}

/// With `blank`, every placeholder gives no text at all.
fn fill(prefix: &str, body: &str, blank: bool) -> String {
    let chars: Vec<char> = body.chars().collect();
    let mut out = String::new();
    let mut seen: BTreeMap<String, String> = BTreeMap::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] != '$' {
            out.push(chars[i]);
            i += 1;
            continue;
        }
        i += 1;
        let (number, text);
        if chars[i] == '{' {
            let close = chars[i..]
                .iter()
                .position(|c| *c == '}')
                .expect("unclosed placeholder")
                + i;
            let inner: String = chars[i + 1..close].iter().collect();
            i = close + 1;
            let digits: String = inner.chars().take_while(char::is_ascii_digit).collect();
            let tail = &inner[digits.len()..];
            text = if let Some(default) = tail.strip_prefix(':') {
                default.to_string()
            } else if let Some(choices) = tail.strip_prefix('|') {
                let choices = choices.strip_suffix('|').expect("choice ends with |");
                choices.split(',').next().unwrap().to_string()
            } else {
                assert!(
                    tail.is_empty(),
                    "{prefix}: unsupported placeholder ${{{inner}}}"
                );
                String::new()
            };
            number = digits;
        } else {
            let digits: String = chars[i..]
                .iter()
                .take_while(|c| c.is_ascii_digit())
                .collect();
            assert!(!digits.is_empty(), "{prefix}: `$` without a number");
            i += digits.len();
            number = digits;
            text = String::new();
        }
        // A later use may repeat the number alone, as a mirror.
        if number != "0"
            && !text.is_empty()
            && let Some(first) = seen.insert(number.clone(), text.clone())
        {
            assert_eq!(
                first, text,
                "{prefix}: placeholder {number} has two defaults"
            );
        }
        if !blank {
            out.push_str(&text);
        }
    }
    out
}

/// A `method` belongs in a class; every other snippet is a whole program.
fn as_source(prefix: &str, filled: &str) -> String {
    if prefix == "method" {
        format!("CLASS Demo:\n{filled}\nEND CLASS.\n")
    } else {
        format!("{filled}\n")
    }
}

fn check_parses(snippet: &Snippet, lower: bool) {
    let rendered = render_keywords(&snippet.body, lower);
    assert!(
        !rendered.contains("<<") && !rendered.contains(">>"),
        "{}: keyword marker left in the body",
        snippet.prefix
    );
    let source = as_source(
        &snippet.prefix,
        &fill_placeholders(&snippet.prefix, &rendered),
    );
    let program = oxabl::try_parse(&source).expect("no internal panic");
    assert!(
        program.is_ok(),
        "{} ({}): {:?}\n{source}",
        snippet.prefix,
        if lower { "lowercase" } else { "uppercase" },
        program.first_error()
    );
}

#[test]
fn the_file_holds_the_documented_snippets() {
    let mut found: Vec<String> = load().into_iter().map(|s| s.prefix).collect();
    found.sort();
    let mut expected: Vec<String> = PREFIXES.iter().map(|s| s.to_string()).collect();
    expected.sort();
    assert_eq!(found, expected);
}

#[test]
fn every_body_parses_with_uppercase_keywords() {
    for snippet in load() {
        check_parses(&snippet, false);
    }
}

#[test]
fn every_body_parses_with_lowercase_keywords() {
    for snippet in load() {
        check_parses(&snippet, true);
    }
}

/// The body with every marker and every placeholder taken out. What is left
/// is the text that no keyword case changes.
fn unmarked_text(snippet: &Snippet) -> String {
    let mut out = String::new();
    let mut rest = snippet.body.as_str();
    while let Some(open) = rest.find("<<") {
        out.push_str(&rest[..open]);
        let after = &rest[open + 2..];
        let close = after.find(">>").expect("unclosed keyword marker");
        out.push(' ');
        rest = &after[close + 2..];
    }
    out.push_str(rest);
    fill(&snippet.prefix, &out, true)
}

/// The words of `text` that the lexer takes as keywords.
fn keywords_in(text: &str) -> Vec<String> {
    oxabl::lexer::tokenize(text)
        .iter()
        .filter(|t| !matches!(t.kind, Kind::Identifier | Kind::Eof | Kind::Invalid))
        .map(|t| t.slice(text))
        .filter(|word| word.starts_with(|c: char| c.is_ascii_alphabetic()))
        .map(str::to_string)
        .collect()
}

#[test]
fn no_keyword_is_left_outside_a_marker() {
    for snippet in load() {
        let left = keywords_in(&unmarked_text(&snippet));
        assert!(
            left.is_empty(),
            "{}: {left:?} is a keyword outside a keyword marker; the project's keyword case cannot reach it",
            snippet.prefix
        );
    }
}

#[test]
fn the_keyword_finder_sees_a_keyword() {
    assert_eq!(keywords_in("find x no-lock ."), ["find", "no-lock"]);
    assert!(keywords_in("Customer.CustNum = 1 .").is_empty());
}

#[derive(serde::Deserialize)]
struct CaseTable {
    cases: Vec<KeywordCaseText>,
}

#[derive(serde::Deserialize)]
struct KeywordCaseText {
    name: String,
    toml: Vec<String>,
    expected: String,
    #[serde(default)]
    rejected: bool,
}

/// The case of the snippets that the server's reader gives for `toml`: lower
/// for `Lowercase`, upper for anything else, including a rejected file (the
/// server then uses its defaults).
fn server_case(toml: &str) -> (&'static str, bool) {
    match WorkspaceConfig::from_toml(toml) {
        Ok(config) if config.workspace.style.keyword_case == KeywordCase::Lowercase => {
            ("lower", false)
        }
        Ok(_) => ("upper", false),
        Err(_) => ("upper", true),
    }
}

/// `clients/vscode/test/keyword-case.json` is shared with the extension's own
/// reader (`keywordCaseOf`, checked by vitest). Each case says what the server
/// does, so the two readers cannot drift without a test going red.
#[test]
fn the_server_agrees_with_the_shared_keyword_case_table() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../clients/vscode/test/keyword-case.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let table: CaseTable = serde_json::from_str(&text).expect("the table is JSON");
    assert!(table.cases.len() >= 10, "the table lost its cases");
    for case in &table.cases {
        let (got, rejected) = server_case(&case.toml.join("\n"));
        assert_eq!(got, case.expected, "{}: keyword case", case.name);
        assert_eq!(
            rejected, case.rejected,
            "{}: rejected by the server",
            case.name
        );
    }
}
