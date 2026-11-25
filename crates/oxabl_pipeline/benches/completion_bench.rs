//! Code completion in a large procedure file: the 10,000 lines of
//! `semantic_tokens_bench`. A request lexes the current text, places the
//! cursor in the base and collects the items; it never analyses when a base
//! exists. `fresh` reads a base of the current text; `stale` reads a base of
//! the text before a one-line insert in the middle of the file. The contexts
//! are a statement start inside a procedure, the words after `DEFINE`, and an
//! expression (the names in scope and the built-in functions).

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::completion::{CompletionBase, CompletionRequest, complete};
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
];

fn bench_completion(c: &mut Criterion) {
    let source = procedure_file();
    assert!(source.lines().count() >= 10_000);
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig {
        schema: schema_from_df(SCHEMA),
        schema_loaded: true,
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws/big.p");
    let run = LintPipeline::new(&config, &fs);
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
        };
        let ask = |request: &CompletionRequest<'_>, base_text: &str| {
            let navigation = handle
                .navigation_with(&expansion, Arc::clone(&model), &none)
                .expect("it preprocesses");
            let base = CompletionBase {
                navigation: &navigation,
                text: base_text,
                fragment: false,
                stale: false,
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
    }
    group.finish();
}

criterion_group!(benches, bench_completion);
criterion_main!(benches);
