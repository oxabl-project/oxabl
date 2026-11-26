//! Whether a file may need its text expanded, read from its tokens.
//!
//! [`index_file`](crate::index_file) expands a file that does not parse as
//! written, that declares a class or interface, or that is nothing but includes
//! and directives. The producer scan asks the same question for each file of a
//! tree, so a parse of each file is too much when the tokens answer.
//!
//! The scan answers in three ways. `Yes` and `No` are final. `Unsure` sends the
//! file to the parse, so a file with a shape the scan does not know costs what
//! it always cost.

use oxabl_ast::{Statement, StatementKind};
use oxabl_lexer::{Kind, Token};
use oxabl_parser::Parser;

/// What the tokens of a file say.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scan {
    Yes,
    No,
    Unsure,
}

/// Read the tokens of `source`.
///
/// `Yes` when a token may start a class or interface header, or is a character
/// the lexer rejects. `No` when the file has code of its own and nothing that
/// expansion changes, or only includes and directives that stand as whole
/// statements. A file with a `{&name}` reference, with an include or a
/// directive inside a statement, with a brace in a comment or a string, or with
/// no code of its own, is `Unsure`.
///
/// The scan cannot see a syntax error. For a file of code whose includes stand
/// as whole statements it says `No` where the parse says `true` only when the
/// text as written does not parse and an include supplies the context that
/// mends it, such as the opening of a block that the file closes.
pub(crate) fn scan(source: &str, tokens: &[Token]) -> Scan {
    // A reference inside a statement: the tokens before it cannot decide.
    if source.contains("{&") {
        return Scan::Unsure;
    }
    let mut code = false;
    let mut expandable = false;
    // The last token that is not a comment, and the token the scan skips to.
    let mut previous: Option<&Token> = None;
    let mut skip_to: Option<Kind> = None;
    let mut at = 0;
    while at < tokens.len() {
        let token = &tokens[at];
        at += 1;
        if let Some(end) = skip_to {
            if token.kind == Kind::Eof {
                return Scan::Unsure;
            }
            if token.kind == end {
                skip_to = None;
                previous = Some(token);
            }
            continue;
        }
        match token.kind {
            Kind::Eof => break,
            // A reference in a comment or a string is expanded as well.
            Kind::Comment | Kind::StringLiteral => {
                if source[token.start..token.end].contains('{') {
                    return Scan::Unsure;
                }
                if token.kind == Kind::Comment {
                    continue;
                }
                code = true;
            }
            Kind::Invalid => return Scan::Yes,
            Kind::Class | Kind::Interface => {
                if previous.is_none_or(|p| p.kind != Kind::KwAs) {
                    return Scan::Yes;
                }
                code = true;
            }
            Kind::IncludeReference | Kind::IncludeArgReference => {
                expandable = true;
                if !starts_statement(previous, token) || !ends_line(source, token, tokens.get(at)) {
                    return Scan::Unsure;
                }
            }
            Kind::PreprocScopedDefine | Kind::PreprocGlobalDefine => {
                expandable = true;
                if !starts_statement(previous, token) {
                    return Scan::Unsure;
                }
                skip_to = Some(Kind::PreprocEnd);
                continue;
            }
            Kind::PreprocIf | Kind::PreprocElseif => {
                expandable = true;
                if !starts_statement(previous, token) {
                    return Scan::Unsure;
                }
                skip_to = Some(Kind::PreprocThen);
                continue;
            }
            Kind::PreprocElse | Kind::PreprocEndif => {
                expandable = true;
                if !starts_statement(previous, token) || !ends_line(source, token, tokens.get(at)) {
                    return Scan::Unsure;
                }
            }
            Kind::Preprop
            | Kind::LeftBrace
            | Kind::RightBrace
            | Kind::PreprocEnd
            | Kind::PreprocThen
            | Kind::PreprocUndefine
            | Kind::PreprocMessage
            | Kind::PreprocWebstream => return Scan::Unsure,
            _ => code = true,
        }
        previous = Some(token);
    }
    // No code of its own: a file of includes and directives, or an empty one.
    // The parse says which.
    if code || !expandable {
        Scan::No
    } else {
        Scan::Unsure
    }
}

/// Whether `token`, an include or a directive, stands where a statement starts:
/// at the start of the file, after a period or a colon that ends a line of
/// code, or after another include or directive.
fn starts_statement(previous: Option<&Token>, token: &Token) -> bool {
    match previous {
        None => true,
        Some(p) => match p.kind {
            Kind::Period | Kind::Colon => p.end < token.start,
            Kind::IncludeReference
            | Kind::IncludeArgReference
            | Kind::PreprocEnd
            | Kind::PreprocThen
            | Kind::PreprocElse
            | Kind::PreprocEndif => true,
            _ => false,
        },
    }
}

/// Whether `next` starts on a later line than `token` ends, or does not exist.
fn ends_line(source: &str, token: &Token, next: Option<&Token>) -> bool {
    next.is_none_or(|n| {
        n.kind == Kind::Eof || source[token.end..n.start].bytes().any(|b| b == b'\n')
    })
}

/// The answer of the parse, as [`crate::index_file`] judges a file: it does
/// not parse, is nothing but includes and directives, or declares a class or
/// interface at file level.
pub(crate) fn parsed(source: &str, tokens: &[Token]) -> bool {
    oxabl_common::counters::count_expansion_parse();
    let program = Parser::new(tokens, source).parse_program();
    !program.is_ok()
        || crate::only_includes(&program.statements)
        || program.statements.iter().any(is_header)
}

fn is_header(stmt: &Statement) -> bool {
    matches!(
        stmt.kind,
        StatementKind::Class { .. } | StatementKind::Interface { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The answer the parse gives, with no scan before it.
    fn by_parse(source: &str) -> bool {
        parsed(source, &oxabl_lexer::tokenize(source))
    }

    fn by_scan(source: &str) -> Scan {
        scan(source, &oxabl_lexer::tokenize(source))
    }

    /// The scan never says `No` for a file the parse says may be expanded, and
    /// never `Yes` for one it says may not: the answer through the scan and
    /// the parse is the answer of the parse.
    fn agrees(source: &str) {
        if let Some(message) = disagreement(source, "") {
            panic!("{message}");
        }
    }

    /// The failure of [`agrees`] as text, with `label` in it instead of the
    /// source when the label is not empty.
    fn disagreement(source: &str, label: &str) -> Option<String> {
        // The producer scan asks only about a file that holds a `{`.
        if !source.contains('{') {
            return None;
        }
        let answer = match by_scan(source) {
            Scan::Yes => true,
            Scan::No => false,
            Scan::Unsure => by_parse(source),
        };
        let expected = by_parse(source);
        // `Yes` may be a superset: it only costs an expansion.
        if answer == expected || (answer && by_scan(source) == Scan::Yes) {
            return None;
        }
        let shown = if label.is_empty() { source } else { label };
        Some(format!(
            "scan {:?}, parse {expected}:\n{shown}",
            by_scan(source)
        ))
    }

    #[test]
    fn a_class_behind_a_directive_is_found_or_left_to_the_parse() {
        for text in [
            "&IF DEFINED(x) &THEN\nCLASS a.b:\nEND CLASS.\n&ENDIF\n",
            "&IF DEFINED(x) &THEN CLASS a.b: END CLASS. &ENDIF",
            "&IF DEFINED(x) &THEN\nINTERFACE a.b:\nEND INTERFACE.\n&ELSE\n{b.i}\n&ENDIF\n",
        ] {
            assert_ne!(by_scan(text), Scan::No, "{text}");
            agrees(text);
        }
    }

    #[test]
    fn a_class_header_is_found_from_the_tokens_with_no_parse() {
        for text in [
            "CLASS a.b:\nEND CLASS.\n",
            "USING x.*.\n{inc.i}\nFINAL CLASS a.b INHERITS c:\nEND CLASS.\n",
            "INTERFACE a.b:\nEND INTERFACE.\n",
            "USING Progress.Lang.Class.\nDEFINE VARIABLE x AS INTEGER.\n",
        ] {
            assert_eq!(by_scan(text), Scan::Yes, "{text}");
        }
        assert_eq!(
            by_scan("DEFINE VARIABLE c AS CLASS a.b NO-UNDO.\n{inc.i}\nx = 1.\n"),
            Scan::No
        );
    }

    #[test]
    fn a_file_of_includes_with_comments_between_is_left_to_the_parse() {
        for text in [
            "{a.i}\n/* between */\n{b.i}\n",
            "// first\n{a.i}\n&SCOPED-DEFINE x 1\n/* c */\n{b.i x}\n",
            "&IF DEFINED(x) &THEN\n{a.i}\n&ELSE\n{b.i}\n&ENDIF\n",
        ] {
            assert_eq!(by_scan(text), Scan::Unsure, "{text}");
            assert!(by_parse(text), "{text}");
            agrees(text);
        }
    }

    #[test]
    fn a_file_that_does_not_parse_is_not_called_clean_when_expansion_may_mend_it() {
        for text in [
            "DEFINE VARIABLE x AS {type.i}.\n",
            "FOR EACH {&table}:\nEND.\n",
            "x = 1 {inc.i}\ny = 2.\n",
            "DEFINE VARIABLE x AS\n{type.i}\n.\n",
            "DEFINE VARIABLE x AS &IF DEFINED(a) &THEN INTEGER &ELSE CHARACTER &ENDIF NO-UNDO.\n",
            "DEFINE VARIABLE x.{inc.i}\n",
            "x = 1. {inc.i} y = 2.\n",
        ] {
            assert_eq!(by_scan(text), Scan::Unsure, "{text}");
            agrees(text);
        }
        // A character the lexer rejects, with no expansion around it.
        assert_eq!(by_scan("x = `y`.\n{inc.i}\n"), Scan::Yes);
    }

    #[test]
    fn code_with_statements_of_includes_needs_no_parse() {
        for text in [
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n{inc.i}\nx = 1.\n",
            "/* header */\n{defs.i &a=1}\nRUN p.p.\n{1}\nMESSAGE x.\n",
            "&SCOPED-DEFINE a 1\n&IF DEFINED(a) &THEN\nDEFINE VARIABLE x AS INTEGER.\n&ENDIF\nDISPLAY x.\n",
            "FOR EACH t:\n    {body.i}\nEND.\n",
        ] {
            assert_eq!(by_scan(text), Scan::No, "{text}");
            agrees(text);
        }
    }

    #[test]
    fn a_brace_in_a_string_or_a_comment_is_left_to_the_parse() {
        for text in [
            "MESSAGE \"{&name}\".\nx = 1.\n",
            "/* {&name} */\nx = 1.\n",
            "// {inc.i}\nx = 1.\n",
        ] {
            assert_eq!(by_scan(text), Scan::Unsure, "{text}");
            agrees(text);
        }
    }

    /// Every file under the directories this test reads agrees: the sources
    /// of the repository's own fixtures, and the tree `OXABL_SCAN_CORPUS`
    /// names, when it is set.
    #[test]
    fn the_scan_agrees_with_the_parse_on_files() {
        let manifest = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut roots = vec![manifest.join("../../resources"), manifest.join("../")];
        roots.extend(std::env::var_os("OXABL_SCAN_CORPUS").map(Into::into));
        let mut seen = 0;
        let mut failures = Vec::new();
        let mut stack = roots;
        while let Some(path) = stack.pop() {
            if path.is_dir() {
                if path
                    .file_name()
                    .is_some_and(|n| n == "target" || n == ".git")
                {
                    continue;
                }
                stack.extend(
                    std::fs::read_dir(&path)
                        .unwrap()
                        .flatten()
                        .map(|e| e.path()),
                );
            } else if path.extension().is_some_and(|e| {
                ["p", "w", "cls", "i", "v", "abl"]
                    .iter()
                    .any(|x| e.eq_ignore_ascii_case(x))
            }) && let Ok(source) = std::fs::read_to_string(&path)
            {
                seen += 1;
                if let Some(message) = disagreement(&source, &path.display().to_string()) {
                    failures.push(message);
                }
            }
        }
        assert!(
            failures.is_empty(),
            "{} of {seen} files:\n{}",
            failures.len(),
            failures.join("\n")
        );
        assert!(seen > 50, "only {seen} files read");
    }
}
