//! Code completion in a large procedure file: the 10,000 lines of
//! `semantic_tokens_bench`. A request lexes what differs from the tokens of
//! its base (nothing, when the text is the base's), places the cursor in the
//! base and collects the items; it never analyses when a base exists.
//! `request_fresh` and `request_stale` are the whole request of the daemon: it asks whether the
//! request needs a base, then answers. `fresh` reads a base of the current text; `stale` reads a base of
//! the text before a one-line insert in the middle of the file. The contexts
//! are a statement start inside a procedure, the words after `DEFINE`, an
//! expression (the names in scope and the built-in functions), the records
//! after `FIND FIRST`, the fields after `Customer.`, the fields and values of a
//! `WHERE`, and the procedures and programs after `RUN`.

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::completion::{
    CompletionBase, CompletionRequest, FreshListing, Prior, complete, lex, wants_base,
};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_schema::test_support::schema_from_df;
use oxabl_style::KeywordCase;
use oxabl_workspace::InMemoryFileSystem;

const PROCEDURES: usize = 720;

const SCHEMA: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer
ADD FIELD "Name" OF "Customer" AS character
ADD INDEX "CustNum" ON "Customer"
  UNIQUE
  PRIMARY
  INDEX-FIELD "CustNum" ASCENDING
"#;

fn procedure_file() -> String {
    let mut s = String::from("DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
    for k in 0..PROCEDURES {
        write!(
            s,
            "DEFINE TEMP-TABLE tt{k} NO-UNDO\n\
                 FIELD a AS INTEGER\n\
                 FIELD b AS CHARACTER.\n\
             PROCEDURE p{k}:\n\
                 DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.\n\
                 DEFINE VARIABLE cText AS CHARACTER NO-UNDO.\n\
                 DEFINE BUFFER b{k} FOR Customer.\n\
                 FOR EACH Customer WHERE Customer.CustNum > piLimit NO-LOCK:\n\
                     cText = Customer.Name.\n\
                     total = total + 1.\n\
                 END.\n\
                 FIND FIRST b{k} WHERE b{k}.CustNum = {k} NO-LOCK NO-ERROR.\n\
                 CREATE tt{k}.\n\
                 tt{k}.a = piLimit.\n\
                 tt{k}.b = cText.\n\
                 RUN p{next} (INPUT piLimit).\n\
                 total = Twice(total) + noSuchName.\n\
             END PROCEDURE.\n",
            next = (k + 1) % PROCEDURES
        )
        .unwrap();
    }
    s.push_str(
        "FUNCTION Twice RETURNS INTEGER (INPUT piValue AS INTEGER):\n    RETURN piValue * 2.\nEND FUNCTION.\n",
    );
    s
}

/// The contexts each bench asks for: what is typed at the cursor.
const CONTEXTS: &[(&str, &str)] = &[
    ("statement", "    FI"),
    ("define", "    DEFINE "),
    ("expression", "    total = c"),
    ("find_first", "    FIND FIRST "),
    ("qualified", "    DISPLAY Customer."),
    (
        "where",
        "    FOR EACH Customer NO-LOCK WHERE Customer.CustNum > ",
    ),
    ("run", "    RUN "),
];

fn bench_completion(c: &mut Criterion) {
    let source = procedure_file();
    assert!(source.lines().count() >= 10_000);
    let mut fs = InMemoryFileSystem::new();
    for k in 0..40 {
        fs.insert(PathBuf::from(format!("/ws/prog{k}.p")), "MESSAGE 1.");
    }
    let config = PipelineConfig {
        schema: schema_from_df(SCHEMA),
        schema_loaded: true,
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws/big.p");
    let listing = FreshListing(&fs);
    let run = LintPipeline::new(&config, &fs).with_define_record(true);
    let handle = run.with_file(&path);
    // The line after `total = total + 1.` of the procedure in the middle.
    let anchor = "total = total + 1.\n";
    let middle = source.len() / 2;
    let at = middle + source[middle..].find(anchor).expect("an anchor") + anchor.len();
    let none = |_| None;

    let mut group = c.benchmark_group("completion");
    group.sample_size(20);
    for (name, typed) in CONTEXTS {
        // Fresh: the base is of the text the request reads.
        let text = format!("{}{typed}\n{}", &source[..at], &source[at..]);
        let offset = (at + typed.len()) as u32;
        let expansion = handle.expand(&text);
        let model = Arc::new(
            handle
                .navigation_model(&expansion)
                .expect("it preprocesses"),
        );
        let request = CompletionRequest {
            text: &text,
            offset,
            path: &path,
            keyword_case: KeywordCase::Uppercase,
            snippets: true,
            trigger: None,
            config: &config,
            listing: &listing,
        };
        let base_tokens = lex(&text);
        let ask = |request: &CompletionRequest<'_>, base_text: &str| {
            let navigation = handle
                .navigation_with(&expansion, Arc::clone(&model), &none)
                .expect("it preprocesses");
            let base = CompletionBase {
                navigation: &navigation,
                text: base_text,
                fragment: false,
                stale: false,
                tokens: Some(&base_tokens),
            };
            complete(request, Some(&base)).map_or(0, |list| list.items.len())
        };
        assert!(ask(&request, &text) > 0, "{name}: no items");
        group.bench_function(format!("fresh/{name}"), |b| {
            b.iter(|| black_box(ask(&request, &text)))
        });
        // Stale: one line inserted above the cursor since the base was made.
        let edited = format!(
            "{}    total = 2.\n{typed}\n{}",
            &source[..at],
            &source[at..]
        );
        let stale = CompletionRequest {
            text: &edited,
            offset: (at + "    total = 2.\n".len() + typed.len()) as u32,
            ..request
        };
        let list_len = ask(&stale, &text);
        assert!(list_len > 0, "{name}: no stale items");
        group.bench_function(format!("stale/{name}"), |b| {
            b.iter(|| black_box(ask(&stale, &text)))
        });
        // The whole request of the daemon.
        let prior = Prior {
            text: &text,
            tokens: &base_tokens,
        };
        for (kind, request) in [("fresh", &request), ("stale", &stale)] {
            if !matches!(*name, "define" | "expression") {
                continue;
            }
            group.bench_function(format!("request_{kind}/{name}"), |b| {
                b.iter(|| {
                    black_box(wants_base(request, Some(prior)));
                    black_box(ask(request, &text))
                })
            });
        }
    }
    group.finish();
}

/// The files of `RUN` with one include path entry of 20,000 programs, named
/// three times in the include path, and a small document: what one request
/// pays to list and filter a large directory, with no cache.
fn bench_large_directory(c: &mut Criterion) {
    const PROGRAMS: usize = 20_000;
    let mut fs = InMemoryFileSystem::new();
    for k in 0..PROGRAMS {
        fs.insert(PathBuf::from(format!("/big/prog{k}.p")), "MESSAGE 1.");
    }
    let config = PipelineConfig {
        include_paths: vec![
            PathBuf::from("/ws"),
            PathBuf::from("/big"),
            PathBuf::from("/big"),
            PathBuf::from("/big"),
        ],
        ..PipelineConfig::default()
    };
    let listing = FreshListing(&fs);
    let path = PathBuf::from("/ws/small.p");
    let run = LintPipeline::new(&config, &fs).with_define_record(true);
    let handle = run.with_file(&path);
    let none = |_| None;
    let mut group = c.benchmark_group("completion");
    group.sample_size(20);
    for (name, typed) in [("run_large", "RUN "), ("run_large_word", "RUN prog1")] {
        let text = format!("DEFINE VARIABLE total AS INTEGER NO-UNDO.\n{typed}");
        let expansion = handle.expand(&text);
        let model = Arc::new(
            handle
                .navigation_model(&expansion)
                .expect("it preprocesses"),
        );
        let request = CompletionRequest {
            text: &text,
            offset: text.len() as u32,
            path: &path,
            keyword_case: KeywordCase::Uppercase,
            snippets: true,
            trigger: None,
            config: &config,
            listing: &listing,
        };
        let tokens = lex(&text);
        let ask = || {
            let navigation = handle
                .navigation_with(&expansion, Arc::clone(&model), &none)
                .expect("it preprocesses");
            let base = CompletionBase {
                navigation: &navigation,
                text: &text,
                fragment: false,
                stale: false,
                tokens: Some(&tokens),
            };
            complete(&request, Some(&base)).map_or(0, |list| list.items.len())
        };
        assert!(ask() > 0, "{name}: no items");
        group.bench_function(format!("fresh/{name}"), |b| b.iter(|| black_box(ask())));
    }
    group.finish();
}

criterion_group!(benches, bench_completion, bench_large_directory);
criterion_main!(benches);
