//! Include references read as `COMPILE ... PREPROCESS` reads them (OpenEdge
//! 12.8.9). Each expected text is the compiler's output for the source. The
//! compiler also writes a line break and a space after the text of an include;
//! oxabl does not, so the expected texts leave them out.
//!
//! The reference ends at the first `}`, also inside double or single quotes,
//! and a `~` escapes the next byte, so `~}` does not end it. In the arguments,
//! a `~` is removed and the byte after it keeps its meaning, `~{` opens a
//! nested reference, and `""` is one `"` inside a group or outside one.
//!
//! The compiler reads an argument's nested reference where the include uses
//! the argument: for `"a~{b"` it writes the include text before `{3}` and the
//! `a`, then stops with error 374 (cases G and P).

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_preprocessor::{PreprocessedFile, Preprocessor, TextLossKind};
use oxabl_workspace::InMemoryFileSystem;

const PROBE: &str = "LINE-{1}: one=<{2}> two=<{3}> three=<{4}>";
const NAMED: &str = "n=<{&n}> 1=<{1}> 2=<{2}>";
const MSG: &str = "MESSAGE {&text}.";

fn preprocess(source: &str) -> PreprocessedFile {
    preprocess_with(source, &[])
}

fn preprocess_with(source: &str, more: &[(&str, &str)]) -> PreprocessedFile {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in more {
        fs.insert(PathBuf::from(*path), *text);
    }
    fs.insert(PathBuf::from("/inc/probe4.i"), PROBE);
    fs.insert(PathBuf::from("/inc/named.i"), NAMED);
    fs.insert(PathBuf::from("/inc/msg.i"), MSG);
    let paths = vec![PathBuf::from("/inc")];
    let result = Preprocessor::new(&fs, &paths)
        .process(FileId::new(1), source)
        .unwrap();
    // The private-use character that stands for `~{` while an argument is
    // held must never reach the text.
    assert!(
        !result.to_text().contains('\u{E000}'),
        "marker in the text for {source:?}: {:?}",
        result.to_text()
    );
    result
}

fn assert_text(source: &str, expected: &str) {
    let result = preprocess(source);
    assert_eq!(&*result.to_text(), expected, "for {source:?}");
    assert!(result.unresolved_includes.is_empty(), "for {source:?}");
    assert!(result.diagnostics.is_empty(), "for {source:?}");
    assert!(result.text_losses.is_empty(), "for {source:?}");
}

#[test]
fn a_tilde_before_a_quote_in_a_group_reads_as_a_doubled_quote() {
    assert_text(
        "{probe4.i A x \"~\"}\" y}Z",
        "LINE-A: one=<x> two=<\"> three=<>\" y}Z",
    );
}

#[test]
fn a_single_quote_does_not_hide_the_closing_brace() {
    assert_text(
        "{probe4.i B x '}' y}Z",
        "LINE-B: one=<x> two=<'> three=<>' y}Z",
    );
}

#[test]
fn a_tilde_before_a_quote_does_not_escape_it() {
    assert_text(
        "{probe4.i C x \"a~\"b\" y}Z",
        "LINE-C: one=<x> two=<ab y> three=<>Z",
    );
}

#[test]
fn a_double_quote_does_not_hide_the_closing_brace() {
    assert_text(
        "{probe4.i D x \"a}b\" y}Z",
        "LINE-D: one=<x> two=<a> three=<>b\" y}Z",
    );
}

#[test]
fn a_tilde_escapes_a_closing_brace_inside_quotes_or_not() {
    assert_text(
        "{probe4.i E x \"a~}b\" y}Z",
        "LINE-E: one=<x> two=<a}b> three=<y>Z",
    );
    assert_text(
        "{probe4.i F x a~}b y}Z",
        "LINE-F: one=<x> two=<a}b> three=<y>Z",
    );
}

#[test]
fn a_brace_in_a_quoted_named_argument_ends_the_reference() {
    // The group never closes, so the argument runs to the `}`, its trailing
    // space included. The rest of the line is source text.
    assert_text(
        "{msg.i &text=\"'Press } to go on'\"}\nDISPLAY \"next\".",
        "MESSAGE 'Press . to go on'\"}\nDISPLAY \"next\".",
    );
}

#[test]
fn an_escaped_brace_in_a_quoted_named_argument_is_text() {
    assert_text(
        "{msg.i &text=\"'Press ~} to go on'\"}\nDISPLAY \"next\".",
        "MESSAGE 'Press } to go on'.\nDISPLAY \"next\".",
    );
}

#[test]
fn an_escaped_open_brace_opens_a_nested_reference() {
    assert_text(
        "&GLOBAL-DEFINE n v\n{probe4.i J x \"~{&n~}\" y}Z",
        "LINE-J: one=<x> two=<v> three=<y>Z",
    );
}

#[test]
fn a_nested_include_reference_in_an_argument_is_included() {
    // The compiler looks for an include file `b` and, not finding it, gives
    // the argument `a`.
    let result = preprocess("{probe4.i K x a~{b~} y}Z");
    assert_eq!(&*result.to_text(), "LINE-K: one=<x> two=<a> three=<y>Z");
    let names: Vec<&str> = result
        .unresolved_includes
        .iter()
        .map(|u| u.name.as_str())
        .collect();
    assert_eq!(names, ["b"]);
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC007"]);
}

#[test]
fn a_nested_include_that_exists_is_expanded_into_the_argument() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/inc/probe4.i"), PROBE);
    fs.insert(PathBuf::from("/inc/b.i"), "B{&q}");
    let paths = vec![PathBuf::from("/inc")];
    let result = Preprocessor::new(&fs, &paths)
        .process(FileId::new(1), "{probe4.i K x a~{b.i &q=1~} y}Z")
        .unwrap();
    assert_eq!(&*result.to_text(), "LINE-K: one=<x> two=<aB1> three=<y>Z");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    assert!(result.text_losses.is_empty());
}

#[test]
fn an_unclosed_escaped_open_brace_is_an_error() {
    // The compiler stops with error 374 (unmatched braces).
    let source = "{probe4.i P x \"~{\" y}Z";
    let result = preprocess(source);
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC012"]);
    assert_eq!(result.diagnostics[0].span.span.start, 0);
    assert_eq!(
        result.diagnostics[0].span.span.end as usize,
        source.len() - 1
    );
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::UnmatchedArgumentBrace]);
}

#[test]
fn a_doubled_quote_is_one_quote_inside_or_outside_a_group() {
    assert_text(
        "{probe4.i L x \"\"\"\" y}Z",
        "LINE-L: one=<x> two=<\"\"> three=<y>Z",
    );
    assert_text(
        "{probe4.i M x \"\" y}Z",
        "LINE-M: one=<x> two=<\"> three=<y>Z",
    );
    assert_text(
        "{probe4.i N x \"a\"\"b\" y}Z",
        "LINE-N: one=<x> two=<a\"b> three=<y>Z",
    );
    assert_text(
        "{probe4.i O x a\"\"b y}Z",
        "LINE-O: one=<x> two=<a\"b> three=<y>Z",
    );
}

#[test]
fn doubled_single_quotes_stay_as_written() {
    assert_text(
        "{probe4.i Q x 'a''b' y}Z",
        "LINE-Q: one=<x> two=<'a''b'> three=<y>Z",
    );
}

#[test]
fn a_doubled_tilde_is_one_tilde() {
    assert_text(
        "{probe4.i S x \"a~~b\" y}Z",
        "LINE-S: one=<x> two=<a~b> three=<y>Z",
    );
}

#[test]
fn an_argument_ending_in_a_tilde_is_reported_as_a_guess() {
    // The compiler gives `two=<~>>`: the `~` reads with the `>` after `{3}`.
    // oxabl does not model that, so it says the text is a guess.
    let result = preprocess("{probe4.i R x ~~ y}Z");
    assert_eq!(&*result.to_text(), "LINE-R: one=<x> two=<~> three=<y>Z");
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC013"]);
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::ArgumentEndsWithTilde]);
}

#[test]
fn a_single_quote_does_not_group() {
    assert_text(
        "{probe4.i D 'b c':U}",
        "LINE-D: one=<'b> two=<c':U> three=<>",
    );
    assert_text(
        "{probe4.i E 'b,c':U}",
        "LINE-E: one=<'b,c':U> two=<> three=<>",
    );
}

#[test]
fn a_double_quote_groups_and_is_removed() {
    assert_text(
        "{probe4.i F a\"b c\"d}",
        "LINE-F: one=<ab cd> two=<> three=<>",
    );
    assert_text(
        "{probe4.i G \"b c\":U}",
        "LINE-G: one=<b c:U> two=<> three=<>",
    );
}

#[test]
fn a_doubled_quote_in_a_group_is_one_quote() {
    assert_text(
        "{probe4.i H \"a\"\"b\"}",
        "LINE-H: one=<a\"b> two=<> three=<>",
    );
}

#[test]
fn a_named_argument_follows_the_same_rules() {
    assert_text("{named.i &n='b,c':U}", "n=<'b,c':U> 1=<'b,c':U> 2=<>");
    assert_text("{named.i &n=\"b c\":U}", "n=<b c:U> 1=<b c:U> 2=<>");
    assert_text("{named.i &n=a\"b c\"d}", "n=<ab cd> 1=<ab cd> 2=<>");
}

#[test]
fn a_plain_brace_after_an_escaped_open_brace_ends_the_reference() {
    // Not probed. The compiler finds the end of the reference first (cases G
    // and P), so the `}` after `&n` ends it and `~{&n` is never closed.
    let result = preprocess("&GLOBAL-DEFINE n v\n{probe4.i J x ~{&n} y}Z");
    assert_eq!(&*result.to_text(), "LINE-J: one=<x> two=<{&n> three=<> y}Z");
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC012"]);
}

#[test]
fn an_argument_the_include_does_not_use_is_not_read() {
    // The include uses only arguments 1 to 4.
    for source in [
        "{probe4.i U x y z \"a~{b\"}Z",
        "{probe4.i U x y z ~{gone.i~}}Z",
    ] {
        let result = preprocess(source);
        assert_eq!(
            &*result.to_text(),
            "LINE-U: one=<x> two=<y> three=<z>Z",
            "for {source:?}"
        );
        assert!(result.diagnostics.is_empty(), "for {source:?}");
        assert!(result.unresolved_includes.is_empty(), "for {source:?}");
        assert!(result.text_losses.is_empty(), "for {source:?}");
    }
}

#[test]
fn a_nested_name_reads_the_definition_where_the_include_uses_it() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE n v\n{/inc/redef.i ~{&n~}}",
        &[("/inc/redef.i", "&SCOPED-DEFINE n w\n<{1}>")],
    );
    assert_eq!(&*result.to_text(), "<w>");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn a_nested_argument_reads_the_arguments_where_the_include_uses_it() {
    // `~{1~}` is passed on unread and reads the argument of `inner.i`.
    let result = preprocess_with(
        "{/inc/outer.i A}",
        &[
            ("/inc/outer.i", "{/inc/inner.i B ~{1~}}"),
            ("/inc/inner.i", "<{2}>"),
        ],
    );
    assert_eq!(&*result.to_text(), "<B>");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn nested_references_past_the_depth_limit_are_reported_and_dropped() {
    let deep = format!("{}&n{}", "~{".repeat(65), "~}".repeat(65));
    let result = preprocess(&format!("{{probe4.i D x {deep} y}}Z"));
    assert_eq!(&*result.to_text(), "LINE-D: one=<x> two=<> three=<y>Z");
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC003"]);
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::IncludeDepthLimit]);

    let inside = format!("{}&n{}", "~{".repeat(64), "~}".repeat(64));
    let result = preprocess(&format!("{{probe4.i D x {inside} y}}Z"));
    assert!(
        !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC003"),
        "{:?}",
        result.diagnostics
    );
}

#[test]
fn a_condition_that_reads_a_nested_argument_is_a_guess() {
    let result = preprocess_with(
        "{/inc/cond.i ~{&n~}}",
        &[(
            "/inc/cond.i",
            "&IF \"{1}\" = \"\" &THEN\nE\n&ELSE\nF\n&ENDIF\n",
        )],
    );
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC010"]);
}

#[test]
fn a_nested_reference_in_a_define_value_is_read_at_the_define() {
    // The define stores `v`; a later `{&x}` does not read `n` again.
    let result = preprocess_with(
        "&GLOBAL-DEFINE n v\n{/inc/defonly.i ~{&n~}}\n&GLOBAL-DEFINE n late\n[{&x}]",
        &[("/inc/defonly.i", "&GLOBAL-DEFINE x {1}")],
    );
    assert_eq!(&*result.to_text(), "\n[v]");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    assert!(result.text_losses.is_empty());
}

#[test]
fn a_nested_argument_in_a_define_value_reads_the_arguments_at_the_define() {
    // `~{1~}` names argument 1 of `def.i`, not of the include that reads `{&x}`.
    let result = preprocess_with(
        "{/inc/def.i A ~{1~}}\n{/inc/use.i Z}",
        &[
            ("/inc/def.i", "&GLOBAL-DEFINE x [{2}]"),
            ("/inc/use.i", "{&x}"),
        ],
    );
    assert_eq!(&*result.to_text(), "\n[A]");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn a_nested_include_in_a_define_value_is_included_at_the_define() {
    let result = preprocess_with(
        "{/inc/defonly.i ~{/inc/b.i~}}\n[{&x}][{&x}]",
        &[
            ("/inc/defonly.i", "&GLOBAL-DEFINE x {1}"),
            ("/inc/b.i", "B"),
        ],
    );
    assert_eq!(&*result.to_text(), "\n[B][B]");
    let paths: Vec<&str> = result
        .resolved_includes
        .iter()
        .map(|i| i.name.as_str())
        .collect();
    assert_eq!(paths, ["/inc/defonly.i", "/inc/b.i"]);
}

#[test]
fn a_nested_reference_in_a_computed_include_name_is_read() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE nm b.i\n{/inc/dyn.i ~{&nm~}}Z",
        &[
            ("/inc/dyn.i", "&GLOBAL-DEFINE x {1}\n{{&x} Q}"),
            ("/inc/b.i", "B"),
        ],
    );
    assert_eq!(&*result.to_text(), "BZ");
    assert!(result.unresolved_includes.is_empty());
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn an_argument_fault_used_many_times_is_reported_once() {
    for (argument, code, kind) in [
        ("a~{b", "PREPROC012", TextLossKind::UnmatchedArgumentBrace),
        ("a~~", "PREPROC013", TextLossKind::ArgumentEndsWithTilde),
    ] {
        let result = preprocess_with(
            &format!("{{/inc/rep.i {argument}}}"),
            &[("/inc/rep.i", "{1}{1}{1}{1}")],
        );
        let found: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
        assert_eq!(found, [code], "for {argument:?}");
        let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
        assert_eq!(kinds, [kind], "for {argument:?}");
    }
}

#[test]
fn a_nested_argument_that_names_itself_in_a_define_value_is_reported() {
    // The compiler stops with error 2936 (nested recursion limit) and the
    // value is `}`. oxabl reports the depth limit and stores an empty value.
    let result = preprocess_with(
        "{/inc/outer.i OUTERARG}\n{/inc/reader.i READERARG}",
        &[
            ("/inc/outer.i", "{/inc/def2.i ~{1~}}"),
            ("/inc/def2.i", "&GLOBAL-DEFINE y {1}"),
            ("/inc/reader.i", "MESSAGE \"<{&y}>\"."),
        ],
    );
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC003"]);
    assert!(!result.to_text().contains("OUTERARG"));
    assert!(!result.to_text().contains('\u{E000}'));
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::IncludeDepthLimit]);
}

#[test]
fn a_nested_include_used_only_in_a_comment_is_not_read() {
    // The compiler does not substitute `{1}` in a comment, so it never reads
    // the file: no edge, no error for a missing file.
    for name in ["zz.i", "zq.i"] {
        let result = preprocess_with(
            &format!("{{/inc/cmt.i ~{{/inc/{name}~}}}}Z"),
            &[("/inc/cmt.i", "/* {1} */ X"), ("/inc/zz.i", "ZZ")],
        );
        assert_eq!(&*result.to_text(), "/* {1} */ XZ", "for {name}");
        assert!(result.diagnostics.is_empty(), "for {name}");
        assert!(result.unresolved_includes.is_empty(), "for {name}");
        assert!(result.text_losses.is_empty(), "for {name}");
        let paths: Vec<&str> = result
            .resolved_includes
            .iter()
            .map(|i| i.name.as_str())
            .collect();
        assert_eq!(paths, ["/inc/cmt.i"], "for {name}");
    }
}

#[test]
fn a_nested_reference_in_a_named_argument_is_read_where_the_include_uses_it() {
    let result = preprocess_with("&GLOBAL-DEFINE m M\n{/inc/named.i &n=~{&m~} a}", &[]);
    assert_eq!(&*result.to_text(), "n=<M> 1=<M> 2=<a>");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
    assert!(result.text_losses.is_empty());
}

#[test]
fn a_named_argument_reads_the_definition_at_the_use() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE m first\n{/inc/late.i &n=~{&m~}}",
        &[("/inc/late.i", "&GLOBAL-DEFINE m second\n[{&n}]")],
    );
    assert_eq!(&*result.to_text(), "[second]");
}

#[test]
fn a_nested_include_in_a_named_argument_is_included_where_it_is_used() {
    let result = preprocess_with("{/inc/named.i &n=~{/inc/b.i~}}", &[("/inc/b.i", "B")]);
    assert_eq!(&*result.to_text(), "n=<B> 1=<B> 2=<>");
    let names: Vec<&str> = result
        .resolved_includes
        .iter()
        .map(|i| i.name.as_str())
        .collect();
    // Read at each use of the argument: `{&n}` and `{1}`.
    assert_eq!(names, ["/inc/named.i", "/inc/b.i", "/inc/b.i"]);
}

#[test]
fn a_nested_reference_in_a_named_argument_is_read_at_a_define() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE m M\n{/inc/defn.i &n=~{&m~}}\n[{&x}]",
        &[("/inc/defn.i", "&GLOBAL-DEFINE x <{&n}> {&k}")],
    );
    assert_eq!(&*result.to_text(), "\n[<M> ]");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn a_condition_that_reads_a_nested_named_argument_is_a_guess() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE m M\n{/inc/condn.i &n=~{&m~}}",
        &[(
            "/inc/condn.i",
            "&IF \"{&n}\" = \"M\" &THEN\nYES\n&ELSE\nNO\n&ENDIF\n",
        )],
    );
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC010"]);
}

#[test]
fn a_named_argument_that_names_itself_is_reported_not_looped() {
    let result = preprocess_with("{/inc/named.i &n=~{&n~}}", &[]);
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC003"]);
}

#[test]
fn a_nested_reference_next_to_a_defined_name_in_a_define_value_keeps_its_text() {
    // The value mixes a `{&name}` read at the define with a nested reference
    // and a non-ASCII byte run, which a byte-wise copy would corrupt.
    let result = preprocess_with(
        "&GLOBAL-DEFINE m M\n&GLOBAL-DEFINE k é\n{/inc/defonly.i ~{&m~}}\n[{&x}]",
        &[("/inc/defonly.i", "&GLOBAL-DEFINE x {&k}-{1}-{&k}")],
    );
    assert_eq!(&*result.to_text(), "\n[é-M-é]");
}

#[test]
fn a_nested_named_argument_passed_on_to_another_include_is_read_there() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE m M\n{/inc/pass.i &n=~{&m~}}",
        &[("/inc/pass.i", "{/inc/named.i &n={&n}}")],
    );
    assert_eq!(&*result.to_text(), "n=<M> 1=<M> 2=<>");
    assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics);
}

#[test]
fn a_nested_named_argument_in_an_include_name_is_never_text() {
    let result = preprocess_with(
        "&GLOBAL-DEFINE nm b.i\n{/inc/byname.i &n=~{&nm~}}Z",
        &[("/inc/byname.i", "{{&n} Q}"), ("/inc/b.i", "B")],
    );
    // The marker must not reach the text; whether the name resolves is secondary.
    assert!(result.to_text().ends_with('Z'), "{:?}", result.to_text());
}

const B_ONE: &str = "MESSAGE \"B[{1}]\".";
const B_NAMED: &str = "MESSAGE \"B[{&x}]\".";
const B_NAMED_ONE: &str = "MESSAGE \"B[{&x}|{1}]\".";
const B_TWO: &str = "MESSAGE \"B[{1}|{2}]\".";

/// The text of `source` with the include files `more`, which must come out
/// as `expected` without any finding.
fn assert_passes(source: &str, more: &[(&str, &str)], expected: &str) {
    let result = preprocess_with(source, more);
    assert_eq!(&*result.to_text(), expected, "for {source:?}");
    assert!(result.unresolved_includes.is_empty(), "for {source:?}");
    assert!(result.diagnostics.is_empty(), "for {source:?}");
    assert!(result.text_losses.is_empty(), "for {source:?}");
}

#[test]
fn an_argument_of_an_include_fills_a_nested_reference() {
    assert_passes(
        "{/inc/pass.i hello}",
        &[("/inc/b.i", B_ONE), ("/inc/pass.i", "{b.i {1}}")],
        "MESSAGE \"B[hello]\".",
    );
}

#[test]
fn an_argument_read_as_a_name_in_a_nested_reference_is_the_value_of_the_name() {
    // `{&{1}}` reads `{1}` first, then the name it gives.
    assert_passes(
        "&GLOBAL-DEFINE foo FOO\n{/inc/pass2.i foo}",
        &[("/inc/bx.i", B_NAMED), ("/inc/pass2.i", "{bx.i &x={&{1}}}")],
        "MESSAGE \"B[FOO]\".",
    );
}

#[test]
fn an_argument_builds_the_name_of_a_nested_include() {
    assert_passes(
        "{/inc/pass3.i c}",
        &[("/inc/c.i", "C{1}"), ("/inc/pass3.i", "{{1}.i x}")],
        "Cx",
    );
}

#[test]
fn a_passed_on_escaped_reference_is_read_where_the_last_include_uses_it() {
    // Not where the first include got it: `x` is `MAINX` there and `OWN` in
    // the last include.
    assert_passes(
        "&GLOBAL-DEFINE x MAINX\n{/inc/pass4.i ~{&x~}}",
        &[
            ("/inc/b4.i", B_NAMED_ONE),
            ("/inc/pass4.i", "{b4.i &x=OWN {1}}"),
        ],
        "MESSAGE \"B[OWN|OWN]\".",
    );
}

#[test]
fn a_quoted_argument_with_a_space_passes_on_as_two_arguments() {
    assert_passes(
        "{/inc/pass5.i \"a b\"}",
        &[("/inc/b5.i", B_TWO), ("/inc/pass5.i", "{b5.i {1}}")],
        "MESSAGE \"B[a|b]\".",
    );
}

#[test]
fn an_argument_reference_in_an_include_reference_at_the_top_is_empty() {
    assert_passes("{/inc/b.i {1}}", &[("/inc/b.i", B_ONE)], "MESSAGE \"B[]\".");
}

#[test]
fn a_value_ending_in_a_tilde_leaves_the_nested_reference_unmatched() {
    // The compiler stops with error 374 (unmatched braces) and writes no text.
    let result = preprocess_with(
        "{/inc/pass7.i a~~}",
        &[("/inc/b.i", B_ONE), ("/inc/pass7.i", "{b.i {1}}")],
    );
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC012"]);
    assert_eq!(&*result.to_text(), "");
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::UnmatchedArgumentBrace]);
}

#[test]
fn a_named_argument_takes_a_positional_slot() {
    assert_passes(
        "{/inc/p12.i &x=zzz}",
        &[("/inc/p12.i", "MESSAGE \"P[{1}|{2}]\".")],
        "MESSAGE \"P[zzz|]\".",
    );
}

#[test]
fn a_value_with_a_closing_brace_is_one_argument_where_it_is_only_used() {
    assert_passes(
        "{/inc/p14.i a~}b}",
        &[("/inc/p14.i", "MESSAGE \"P[{1}]\".")],
        "MESSAGE \"P[a}b]\".",
    );
}

#[test]
fn a_value_with_a_closing_brace_ends_the_nested_reference_there() {
    // The compiler ends the reference at the `}` of the value and reads the
    // text after it as code (a compile error). oxabl stops instead of giving
    // the value as one argument.
    let result = preprocess_with(
        "{/inc/p13.i a~}b}",
        &[("/inc/b13.i", B_TWO), ("/inc/p13.i", "{b13.i {1}}")],
    );
    let codes: Vec<&str> = result.diagnostics.iter().map(|d| d.code.0).collect();
    assert_eq!(codes, ["PREPROC012"]);
    assert_eq!(&*result.to_text(), "");
    let kinds: Vec<TextLossKind> = result.text_losses.iter().map(|l| l.kind).collect();
    assert_eq!(kinds, [TextLossKind::UnmatchedArgumentBrace]);
}

const P9: &str = "MESSAGE \"P[{&x}]\".\n{b9.i}";

#[test]
fn a_nested_include_does_not_see_the_named_argument_of_its_includer() {
    assert_passes(
        "{/inc/p9.i &x=zzz}",
        &[("/inc/p9.i", P9), ("/inc/b9.i", B_NAMED)],
        "MESSAGE \"P[zzz]\".\nMESSAGE \"B[]\".",
    );
}

#[test]
fn a_nested_include_sees_only_its_own_named_arguments() {
    assert_passes(
        "{/inc/p10.i &x=zzz}",
        &[
            ("/inc/p10.i", "{b10.i &y=yyy}"),
            ("/inc/b10.i", "MESSAGE \"B[{&x}|{&y}]\"."),
        ],
        "MESSAGE \"B[|yyy]\".",
    );
}

#[test]
fn a_scoped_name_of_the_unit_stays_visible_and_the_argument_does_not_replace_it_outside() {
    assert_passes(
        "&SCOPED-DEFINE x mainx\n{/inc/p9.i &x=zzz}\nMESSAGE \"M[{&x}]\".",
        &[("/inc/p9.i", P9), ("/inc/b9.i", B_NAMED)],
        "MESSAGE \"P[zzz]\".\nMESSAGE \"B[mainx]\".\nMESSAGE \"M[mainx]\".",
    );
}

#[test]
fn a_named_argument_passed_on_as_a_positional_value_defines_no_name_below() {
    // `{1}` is `zzz`, so `{bx.i zzz}` defines no `&x` in `bx.i`.
    assert_passes(
        "{/inc/pass8.i &x=zzz}",
        &[("/inc/pass8.i", "{bx.i {1}}"), ("/inc/bx.i", B_NAMED)],
        "MESSAGE \"B[]\".",
    );
}
