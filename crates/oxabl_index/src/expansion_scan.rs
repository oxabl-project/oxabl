//! Whether a file may need its text expanded, read from its tokens where they
//! can say.
//!
//! [`index_file`](crate::index_file) expands a file that does not parse as
//! written, that declares a class or interface, or that is nothing but includes
//! and directives. The producer scan asks this for each file with a `{`.
//!
//! The tokens can only say yes. An include or a reference can hold any part of
//! a block or a statement (the `END.` of a block the file opens, the rest of a
//! statement on the next line), and a directive can choose between two block
//! headers, so a file with either may fail to parse as written and be mended by
//! its expansion. Only the parse knows. A class or interface header is the
//! answer the tokens give without it.

use oxabl_ast::{Statement, StatementKind};
use oxabl_lexer::{Kind, Token};
use oxabl_parser::Parser;

/// Whether a token of `tokens` may start a class or interface header: a
/// `CLASS` or `INTERFACE` that is not a type after `AS`, not an `END CLASS`
/// and not part of a dotted or colon name (`Progress.Lang.Class`). A superset of the files whose
/// parse holds a header at file level: a header inside an `&IF` counts too,
/// which costs only an expansion.
pub(crate) fn may_declare_a_type(tokens: &[Token]) -> bool {
    let mut previous: Option<&Token> = None;
    for token in tokens {
        match token.kind {
            Kind::Comment => continue,
            Kind::Class | Kind::Interface => {
                let not_a_header = previous.is_some_and(|p| {
                    matches!(p.kind, Kind::KwAs | Kind::End)
                        || (matches!(p.kind, Kind::Period | Kind::Colon) && p.end == token.start)
                });
                if !not_a_header {
                    return true;
                }
            }
            _ => {}
        }
        previous = Some(token);
    }
    false
}

/// The answer of the parse, as [`crate::index_file`] judges a file: it does
/// not parse, is nothing but includes and directives, or declares a class or
/// interface at file level.
pub(crate) fn parsed(source: &str, tokens: &[Token]) -> bool {
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

    fn by_parse(source: &str) -> bool {
        parsed(source, &oxabl_lexer::tokenize(source))
    }

    fn by_tokens(source: &str) -> bool {
        may_declare_a_type(&oxabl_lexer::tokenize(source))
    }

    /// The scan says yes only where the parse says yes, apart from a header
    /// inside a directive, which costs one expansion and nothing else.
    fn disagreement(source: &str, label: &str) -> Option<String> {
        if by_tokens(source) && !by_parse(source) && !source.contains("&IF") {
            let shown = if label.is_empty() { source } else { label };
            return Some(format!(
                "the tokens say a type, the parse does not:\n{shown}"
            ));
        }
        None
    }

    #[test]
    fn a_class_or_interface_header_is_found_from_the_tokens() {
        for text in [
            "CLASS a.b:\nEND CLASS.\n",
            "USING x.*.\n{inc.i}\nFINAL CLASS a.b INHERITS c:\nEND CLASS.\n",
            "INTERFACE a.b:\nEND INTERFACE.\n",
            "/* c */ CLASS a.b:\nEND CLASS.\n",
            "&IF DEFINED(x) &THEN\nCLASS a.b:\nEND CLASS.\n&ENDIF\n",
        ] {
            assert!(by_tokens(text), "{text}");
            assert!(by_parse(text) || text.contains("&IF"), "{text}");
        }
    }

    #[test]
    fn a_class_word_in_a_name_or_a_type_is_no_header() {
        for text in [
            "USING Progress.Lang.Class.\nDEFINE VARIABLE x AS INTEGER.\n",
            "DEFINE VARIABLE c AS CLASS a.b NO-UNDO.\n{inc.i}\nx = 1.\n",
            "x = obj:Class.\n",
            "{header.i}\nMETHOD PUBLIC VOID run():\nEND METHOD.\nEND CLASS.\n",
            "{inc.i}\nMESSAGE 1.\n",
        ] {
            assert!(!by_tokens(text), "{text}");
        }
    }

    #[test]
    fn every_file_that_expansion_may_mend_goes_to_the_parse() {
        for text in [
            "&IF DEFINED(a) &THEN\nFOR EACH t WHERE t.a = 1:\n&ELSE\nFOR EACH t:\n&ENDIF\n  DISPLAY t.\nEND.\n{s.i}\n",
            "x = obj:\n{m.i}\n",
            "&IF DEFINED(x) &THEN\nDO:\n&ENDIF\n x = 1.\n&IF DEFINED(x) &THEN\nEND.\n&ENDIF\n{s.i}",
            "PROCEDURE p:\n{a.i}\n",
            "{a.i}\n  NO-UNDO.\n",
        ] {
            assert!(!by_tokens(text), "{text}");
            assert!(by_parse(text), "the written text parses: {text}");
        }
    }

    /// The synthetic fixtures of this crate, and the tree `OXABL_SCAN_CORPUS`
    /// names, when it is set. Every file with a `{` is compared; a file that is
    /// not UTF-8 is read with its bad bytes replaced.
    #[test]
    fn the_scan_agrees_with_the_parse_on_files() {
        let manifest = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut stack = vec![
            manifest.join("tests/fixtures/expansion_scan"),
            manifest.join("../../resources"),
        ];
        let corpus = std::env::var_os("OXABL_SCAN_CORPUS");
        stack.extend(corpus.clone().map(Into::into));
        let (mut compared, mut types, mut lossy) = (0, 0, 0);
        let mut failures = Vec::new();
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
                continue;
            }
            let abl = path.extension().is_some_and(|e| {
                ["p", "w", "cls", "i", "v", "abl"]
                    .iter()
                    .any(|x| e.eq_ignore_ascii_case(x))
            });
            if !abl {
                continue;
            }
            let bytes = std::fs::read(&path).unwrap();
            let source = String::from_utf8_lossy(&bytes);
            lossy += usize::from(matches!(source, std::borrow::Cow::Owned(_)));
            if !source.contains('{') {
                continue;
            }
            compared += 1;
            types += usize::from(by_tokens(&source));
            if let Some(message) = disagreement(&source, &path.display().to_string()) {
                failures.push(message);
            }
        }
        if corpus.is_some() {
            eprintln!("compared {compared} files with a brace, {types} types, {lossy} not UTF-8");
        }
        assert!(
            failures.is_empty(),
            "{} of {compared} files:\n{}",
            failures.len(),
            failures.join("\n")
        );
        assert!(
            compared >= 25,
            "only {compared} files with a brace compared"
        );
        assert!(types >= 5, "only {types} files where the tokens say a type");
    }
}
