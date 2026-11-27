//! A completion request lexes only what differs from the tokens of its base,
//! and answers as if it had lexed the whole text. All sources are synthetic.
//!
//! The lex counters belong to the process, so the cases that read them run in
//! one test, one after the other.

use std::fmt::Write;
use std::path::PathBuf;
use std::sync::Arc;

use oxabl_lexer::lex_counts;
use oxabl_pipeline::completion::{
    CompletionBase, CompletionList, CompletionRequest, FreshListing, Prior, complete, lex,
    wants_base,
};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_style::KeywordCase;
use oxabl_workspace::InMemoryFileSystem;

const PROCEDURES: usize = 200;

fn document() -> String {
    let mut text = String::from("DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
    for k in 0..PROCEDURES {
        write!(
            text,
            "&SCOPED-DEFINE limit{k} {k}\n\
             DEFINE TEMP-TABLE tt{k} NO-UNDO\n  FIELD a AS INTEGER\n  FIELD b AS CHARACTER.\n\
             PROCEDURE p{k}:\n\
             \x20 DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.\n\
             \x20 /* a comment\n     over two lines */\n\
             \x20 FOR EACH tt{k} WHERE tt{k}.a > piLimit NO-LOCK:\n\
             \x20   total = total + 1.\n\
             \x20 END.\n\
             \x20 RUN p{next} (INPUT piLimit).\n\
             END PROCEDURE.\n",
            next = (k + 1) % PROCEDURES
        )
        .unwrap();
    }
    text
}

/// The answers of one request with a base made from `base_text`: with the
/// base's tokens and with none (a full lex of the request's text).
struct Asked {
    list: Option<CompletionList>,
    plain: Option<CompletionList>,
    wants: bool,
    wants_plain: bool,
}

fn ask(base_text: &str, text: &str, offset: usize, keyword_case: KeywordCase) -> Asked {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let path = PathBuf::from("/ws/main.p");
    let run = LintPipeline::new(&config, &fs).with_define_record(true);
    let handle = run.with_file(&path);
    let expansion = handle.expand(base_text);
    let model = Arc::new(
        handle
            .navigation_model(&expansion)
            .expect("it preprocesses"),
    );
    let none = |_| None;
    let navigation = handle
        .navigation_with(&expansion, model, &none)
        .expect("it preprocesses");
    let tokens = lex(base_text);
    let request = CompletionRequest {
        text,
        offset: offset as u32,
        path: &path,
        keyword_case,
        snippets: true,
        trigger: None,
        config: &config,
        listing: &FreshListing(&fs),
    };
    let base = |tokens| CompletionBase {
        navigation: &navigation,
        text: base_text,
        fragment: false,
        stale: false,
        tokens,
        classes: None,
    };
    Asked {
        list: complete(&request, Some(&base(Some(&tokens)))),
        plain: complete(&request, Some(&base(None))),
        wants: wants_base(
            &request,
            Some(Prior {
                text: base_text,
                tokens: &tokens,
            }),
        ),
        wants_plain: wants_base(&request, None),
    }
}

/// The text with `typed` put at the line that starts at `line`.
fn typed_at(source: &str, line: usize, typed: &str) -> (String, usize) {
    let at = source
        .match_indices('\n')
        .nth(line)
        .map_or(source.len(), |(at, _)| at + 1);
    let text = format!("{}{typed}\n{}", &source[..at], &source[at..]);
    (text, at + typed.len())
}

#[test]
fn a_request_answers_as_with_a_full_lex_and_lexes_only_the_edit() {
    let source = document();
    let lines = source.lines().count();
    // The base is the document with a statement being typed; the request
    // reads a newer text, one or more edits later.
    let typings = [
        "  FI",
        "  DEFINE ",
        "  total = c",
        "  FIND FIRST ",
        "  DISPLAY tt3.",
        "  FOR EACH tt1 NO-LOCK WHERE tt1.a > ",
        "  RUN ",
        "  RUN p1",
        "  DEFINE VARIABLE x AS ",
        "  /* open",
        "  MESSAGE \"open",
        "  {&limit",
    ];
    for line in [1, 9, lines / 2 + 3, lines - 3] {
        for typed in typings {
            let (text, offset) = typed_at(&source, line, typed);
            for case in [KeywordCase::Preserve, KeywordCase::Uppercase] {
                // Fresh: the base is of the request's text.
                let fresh = ask(&text, &text, offset, case);
                assert_eq!(fresh.list, fresh.plain, "fresh {typed:?} at line {line}");
                assert_eq!(fresh.wants, fresh.wants_plain, "wants {typed:?}");
                // Stale: the base is of the text before this line.
                let (base, _) = typed_at(&source, line, "  total = 1.");
                let stale = ask(&base, &text, offset, case);
                assert_eq!(stale.list, stale.plain, "stale {typed:?} at line {line}");
                assert_eq!(stale.wants, stale.wants_plain, "wants stale {typed:?}");
            }
        }
    }

    // The lexed bytes of one request, with a base made before the count.
    let (text, offset) = typed_at(&source, lines / 2, "  DEFINE ");
    let (base_text, _) = typed_at(&source, lines / 2, "  DEFINE VARIABLE");
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let path = PathBuf::from("/ws/main.p");
    let run = LintPipeline::new(&config, &fs).with_define_record(true);
    let handle = run.with_file(&path);
    let expansion = handle.expand(&base_text);
    let model = Arc::new(
        handle
            .navigation_model(&expansion)
            .expect("it preprocesses"),
    );
    let none = |_| None;
    let navigation = handle
        .navigation_with(&expansion, model, &none)
        .expect("it preprocesses");
    let tokens = lex(&base_text);
    let listing = FreshListing(&fs);
    let request = |text, offset| CompletionRequest {
        text,
        offset,
        path: &path,
        keyword_case: KeywordCase::Uppercase,
        snippets: true,
        trigger: None,
        config: &config,
        listing: &listing,
    };
    let base = CompletionBase {
        navigation: &navigation,
        text: &base_text,
        fragment: false,
        stale: false,
        tokens: Some(&tokens),
        classes: None,
    };
    let (_, before, _) = lex_counts();
    let stale = request(&text, offset as u32);
    assert!(complete(&stale, Some(&base)).is_some());
    let (_, after, _) = lex_counts();
    let lexed = after - before;
    assert!(
        lexed < text.len() / 50,
        "a stale request lexed {lexed} of {} bytes",
        text.len()
    );
    // The same text as the base: no lexing at all.
    let same = request(&base_text, (offset + "VARIABLE".len()) as u32);
    let (_, before, _) = lex_counts();
    assert!(complete(&same, Some(&base)).is_some());
    let (_, after, _) = lex_counts();
    assert_eq!(after - before, 0, "a fresh request lexed");
}
