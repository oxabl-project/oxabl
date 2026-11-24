//! Every token of a statement that may name a value or run code is held by its
//! node, or the parser marked it unmodelled (`SkippedTail::unmodelled`, or a
//! `Skipped` statement). The taint analysis reads the statements of
//! [`FROM_NODE`] from their node, so a clause the parser drops there without
//! a mark is a write, a call or a decision it never sees. It reads every other
//! statement from its tokens.
//!
//! The check reads the parse of every ABL file of the repository's fixtures,
//! the security corpus and the parser's bench programs that parses cleanly
//! (the analysis does not read a file whose parse recovered). The spans a
//! node holds are read from its `Debug` text, so a field added to any node is
//! counted without a change here. A name that holds no value (a type, a label,
//! a record, field or index name, a condition of an `ON` phrase) is not
//! checked.

use std::path::{Path, PathBuf};

use oxabl_lexer::{Kind, Token, is_reserved_keyword, tokenize};
use oxabl_parser::Parser;

/// The statements whose operands the taint analysis and ABLSEC010 read from
/// the node, by the name of their kind.
const FROM_NODE: [&str; 19] = [
    "Assignment",
    "Assign",
    "BufferCopy",
    "ExpressionStatement",
    "Return",
    "Run",
    "Import",
    "Set",
    "Update",
    "Message",
    "VariableDeclaration",
    "DefineParameter",
    "Do",
    "Repeat",
    "ForEach",
    "If",
    "Case",
    "Catch",
    "Find",
];

fn repo() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// The ABL files under `dirs`, skipping `src` and `target` directories.
fn files(dirs: &[PathBuf]) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = dirs.to_vec();
    while let Some(d) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&d) else {
            continue;
        };
        for e in entries {
            let p = e.unwrap().path();
            if p.is_dir() {
                if !p.file_name().is_some_and(|n| n == "target" || n == "src") {
                    stack.push(p);
                }
            } else if p
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| matches!(e, "p" | "cls" | "w" | "i" | "abl"))
            {
                out.push(p);
            }
        }
    }
    out.sort();
    out
}

/// A statement in the `Debug` text: its kind and span.
struct Stmt {
    kind: String,
    span: (u32, u32),
}

/// The index just past the value that starts at `i` in `Debug` text: at the
/// `,` or the closing bracket that ends it.
fn value_end(s: &[u8], i: usize) -> usize {
    end_at(s, i, true)
}

/// The index of the bracket that closes the list whose items start at `i`.
fn list_end(s: &[u8], i: usize) -> usize {
    end_at(s, i, false)
}

fn end_at(s: &[u8], mut i: usize, comma: bool) -> usize {
    let mut depth = 0usize;
    while i < s.len() {
        match s[i] {
            b'"' => {
                i += 1;
                while i < s.len() && s[i] != b'"' {
                    i += 1 + usize::from(s[i] == b'\\');
                }
            }
            b'{' | b'(' | b'[' => depth += 1,
            b'}' | b')' | b']' if depth == 0 => return i,
            b'}' | b')' | b']' => depth -= 1,
            b',' if comma && depth == 0 => return i,
            _ => {}
        }
        i += 1;
    }
    i
}

/// The `Span { start: a, end: b }` at `at`.
fn span_at(s: &str, at: usize) -> Option<(u32, u32)> {
    let rest = s.get(at..)?.strip_prefix("Span { start: ")?;
    let (start, rest) = rest.split_once(", end: ")?;
    let (end, _) = rest.split_once(' ')?;
    Some((start.parse().ok()?, end.parse().ok()?))
}

/// The statements of a parse, and the spans their nodes hold besides their
/// own: expressions, names, and unmodelled ranges when `marks`.
fn read(debug: &str, marks: bool) -> (Vec<Stmt>, Vec<(u32, u32)>) {
    let bytes = debug.as_bytes();
    let mut stmts = Vec::new();
    let mut own = std::collections::HashSet::new();
    for (at, _) in debug.match_indices("Statement { id: ") {
        let Some(k) = debug[at..].find("kind: ") else {
            continue;
        };
        let kind_at = at + k + "kind: ".len();
        let kind: String = debug[kind_at..]
            .chars()
            .take_while(char::is_ascii_alphanumeric)
            .collect();
        let end = value_end(bytes, kind_at);
        let span_pos = end + ", span: ".len();
        if debug[end..].starts_with(", span: ")
            && let Some(span) = span_at(debug, span_pos)
        {
            own.insert(span_pos);
            stmts.push(Stmt { kind, span });
        }
    }
    let mut marked = std::collections::HashSet::new();
    for (at, _) in debug.match_indices("unmodelled: [") {
        let start = at + "unmodelled: [".len();
        let end = list_end(bytes, start);
        marked.extend(
            debug[start..end]
                .match_indices("Span { start: ")
                .map(|(i, _)| start + i),
        );
    }
    let held = debug
        .match_indices("Span { start: ")
        .filter(|(at, _)| !own.contains(at) && (marks || !marked.contains(at)))
        .filter_map(|(at, _)| span_at(debug, at))
        .collect();
    (stmts, held)
}

fn word<'s>(src: &'s str, t: &Token) -> &'s str {
    &src[t.start..t.end]
}

fn is_word(src: &str, t: Option<&Token>, words: &[&str]) -> bool {
    t.is_some_and(|t| words.iter().any(|w| w.eq_ignore_ascii_case(word(src, t))))
}

/// Whether the name at `i` holds no value: a type, a label, a record, field,
/// index, frame, widget or event name, a condition of an `ON` phrase, `ERROR`
/// of `RETURN ERROR`, an enum member, `FORWARD`.
fn inert(src: &str, toks: &[Token], i: usize) -> bool {
    let at = |j: usize| toks.get(j);
    let is_word_at = |j: usize| {
        toks[j].kind != Kind::StringLiteral
            && word(src, &toks[j])
                .bytes()
                .next()
                .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
    };
    // A name, or a word that is not reserved, as a field named like a keyword.
    let name =
        |j: usize| is_word_at(j) && !is_reserved_keyword(&word(src, &toks[j]).to_ascii_lowercase());
    // The first token of the run of touching names and periods:
    // `Progress.Lang.Object`, `setup_1.2_data`.
    let mut first = i;
    while first >= 1
        && toks[first - 1].end == toks[first].start
        && (is_word_at(first - 1)
            || matches!(
                toks[first - 1].kind,
                Kind::Period | Kind::Preprop | Kind::Minus
            ))
    {
        first -= 1;
    }
    let before = first.checked_sub(1).and_then(at);
    let next = at(i + 1);
    // A type.
    if is_word(
        src,
        before,
        &[
            "AS",
            "INHERITS",
            "IMPLEMENTS",
            "CLASS",
            "INTERFACE",
            "ENUM",
            "RETURNS",
            "VAR",
            "PROCEDURE",
            "FUNCTION",
            "PUBLIC",
            "PRIVATE",
            "PROTECTED",
            "STATIC",
            "OVERRIDE",
            "ABSTRACT",
            "FINAL",
        ],
    ) {
        return true;
    }
    // A label, its block or a jump to it.
    if next.is_some_and(|t| t.kind == Kind::Colon && t.start == toks[i].end)
        || is_word(src, before, &["LEAVE", "NEXT", "UNDO", "RETRY"])
    {
        return true;
    }
    // A record, buffer, index, frame or widget name.
    if is_word(
        src,
        before,
        &[
            "USE-INDEX",
            "OF",
            "EACH",
            "FIRST",
            "LAST",
            "NEXT",
            "PREV",
            "FOR",
            "PRESELECT",
            "BUFFER",
            "FRAME",
            "BROWSE",
            "VIEW-AS",
        ],
    ) {
        return true;
    }
    // A list of names: `EXCEPT a b`, `DO FOR a, b`, `RELATION-FIELDS(a, b)`.
    const LISTS: [&str; 5] = ["EXCEPT", "USING", "FOR", "FIELDS", "RELATION-FIELDS"];
    let mut j = first;
    while j > 0 {
        j -= 1;
        if is_word(src, at(j), &LISTS) {
            return true;
        }
        if !(name(j) || matches!(toks[j].kind, Kind::Comma | Kind::Period | Kind::LeftParen)) {
            break;
        }
    }
    // The events of `WAIT-FOR GO, ENTER OF w` and the keys of `ON F4, PF4
    // ANYWHERE`.
    let mut k = i + 1;
    while k < toks.len() && (name(k) || toks[k].kind == Kind::Comma) {
        k += 1;
    }
    if is_word(src, at(k), &["OF", "ANYWHERE"]) {
        return true;
    }
    // `ON ERROR`, `RETURN ERROR`, `FORWARD`, `VIEW-AS` options.
    is_word(src, before, &["ON", "RETURN", "ALERT-BOX"])
        || is_word(src, Some(&toks[i]), &["FORWARD", "ERROR", "STOP-AFTER"])
}

/// The tokens of `src` that may name a value and that no node holds and no
/// unmodelled mark covers (or no mark at all, unless `marks`), with the kind
/// of the innermost statement around each. `None` when the file does not
/// parse cleanly: the taint analysis does not read such a file.
fn uncovered(src: &str, marks: bool) -> Option<Vec<(String, String)>> {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    if !program.errors.is_empty() {
        return None;
    }
    let (stmts, mut held) = read(&format!("{:?}", program.statements), marks);
    held.extend(stmts.iter().filter(|s| s.kind == "Skipped").map(|s| s.span));
    let toks: Vec<Token> = tokens
        .into_iter()
        .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof))
        .collect();
    let mut out = Vec::new();
    for (i, t) in toks.iter().enumerate() {
        if t.kind != Kind::Identifier || inert(src, &toks, i) {
            continue;
        }
        let (start, end) = (t.start as u32, t.end as u32);
        if held.iter().any(|&(a, b)| a <= start && end <= b) {
            continue;
        }
        let Some(owner) = stmts
            .iter()
            .filter(|s| s.span.0 <= start && end <= s.span.1)
            .min_by_key(|s| s.span.1 - s.span.0)
        else {
            continue;
        };
        if !FROM_NODE.contains(&owner.kind.as_str()) {
            continue;
        }
        let line_start = src[..t.start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = src[t.start..].find('\n').map_or(src.len(), |i| t.start + i);
        out.push((
            owner.kind.clone(),
            format!(
                "`{}` in `{}`",
                word(src, t),
                src[line_start..line_end].trim()
            ),
        ));
    }
    Some(out)
}

#[test]
fn every_operand_of_a_statement_is_in_its_node_or_marked_unmodelled() {
    let root = repo();
    let dirs = [root.join("crates"), root.join("resources")];
    let mut misses = Vec::new();
    let mut read_files = 0;
    for path in files(&dirs) {
        let Ok(src) = std::fs::read_to_string(&path) else {
            continue;
        };
        let Some(found) = uncovered(&src, true) else {
            continue;
        };
        read_files += 1;
        let rel = path
            .strip_prefix(&root)
            .unwrap_or(&path)
            .display()
            .to_string();
        misses.extend(
            found
                .into_iter()
                .map(|(kind, what)| format!("{rel}: {kind}: {what}")),
        );
    }
    assert!(read_files > 100, "only {read_files} files parse cleanly");
    assert!(misses.is_empty(), "{}", misses.join("\n"));
}

/// Without the marks, the check names each operand that the parser drops.
#[test]
fn the_check_sees_a_dropped_operand() {
    let src = "\
ASSIGN x = 1 WHEN mode = \"a\".
FOR EACH tt NO-LOCK, EACH t2 WHERE t2.f = pick() BY sortKey() WHILE more():
END.
";
    assert_eq!(uncovered(src, true).unwrap(), []);
    let names: Vec<String> = uncovered(src, false)
        .unwrap()
        .into_iter()
        .map(|(_, what)| what.split('`').nth(1).unwrap().to_string())
        .collect();
    assert_eq!(names, ["mode", "t2", "f", "pick", "sortKey", "more"]);
}
