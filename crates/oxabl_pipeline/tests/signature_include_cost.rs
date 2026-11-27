//! The work of a signature help request into a class whose methods all start
//! with the same parameter include. All sources are synthetic.
//!
//! The lex counters belong to the process, so the two cases take the lock.

use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use oxabl_lexer::lex_counts;
use oxabl_pipeline::{LintPipeline, Navigation, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

/// The lex counters are global: one case reads them at a time.
static COUNTERS: Mutex<()> = Mutex::new(());

fn class_file(methods: usize) -> String {
    let mut text = String::from("CLASS app.Big:\n");
    for k in 0..methods {
        write!(
            text,
            "    METHOD PUBLIC VOID M{k}({{inc/std-params.i}}, INPUT piId AS INTEGER):\n    \
                 END METHOD.\n"
        )
        .unwrap();
    }
    text.push_str("END CLASS.\n");
    text
}

/// Runs `ask` on a navigation over a caller with `source`, and the calls of
/// `tokenize` it made.
fn tokenizes_of(methods: usize, source: &str, ask: impl FnOnce(&Navigation, &Path)) -> usize {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/ws/app/Big.cls"), class_file(methods));
    fs.insert(
        PathBuf::from("/ws/inc/std-params.i"),
        "INPUT pcCompany AS CHARACTER,\nINPUT pcUser AS CHARACTER\n",
    );
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws/caller.p");
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(&path);
    let expansion = handle.expand(source);
    let none = |_| None;
    let model = Arc::new(
        handle
            .navigation_model(&expansion)
            .expect("the unit preprocesses"),
    );
    let navigation = handle
        .navigation_with(&expansion, model, &none)
        .expect("the unit preprocesses");
    let (_, _, before) = lex_counts();
    ask(&navigation, &path);
    let (_, _, after) = lex_counts();
    after - before
}

const HEAD: &str = "DEFINE VARIABLE o AS app.Big NO-UNDO.\n";

/// The calls of `tokenize` of one signature help request into a class of
/// `methods` methods.
fn help_tokenizes(methods: usize) -> usize {
    let source = format!("{HEAD}o:M3(1, 2, 3).\n");
    let call = source.find("o:M3(").unwrap() + "o:M3(".len();
    tokenizes_of(methods, &source, |navigation, path| {
        let help = navigation
            .signature_help(path, call as u32)
            .expect("a signature");
        assert_eq!(
            help.signatures[0].label,
            "M3({inc/std-params.i} (2 parameters), INPUT piId AS INTEGER): VOID"
        );
    })
}

/// The calls of `tokenize` of the hints of a caller with one call into each of
/// `calls` methods of a class of 100 methods.
fn hints_tokenizes(calls: usize) -> usize {
    let mut source = String::from(HEAD);
    for k in 0..calls {
        writeln!(source, "o:M{k}(1, 2, {k}).").unwrap();
    }
    tokenizes_of(100, &source, |navigation, path| {
        // One hint for the include and one for `piId` in each call.
        assert_eq!(navigation.parameter_hints(path).len(), 2 * calls);
    })
}

#[test]
fn the_lexing_of_a_request_does_not_follow_the_methods_of_the_class() {
    let _alone = COUNTERS.lock().unwrap_or_else(|held| held.into_inner());
    let (few, many) = (help_tokenizes(20), help_tokenizes(200));
    assert_eq!(
        few, many,
        "a request reads the methods it asks about, not every method of the class"
    );
}

#[test]
fn an_include_file_is_lexed_once_whatever_the_methods_that_write_it() {
    let _alone = COUNTERS.lock().unwrap_or_else(|held| held.into_inner());
    let (few, many) = (hints_tokenizes(10), hints_tokenizes(40));
    // Each call lexes its own include reference once.
    assert!(
        many - few <= 30,
        "{few} lexes for 10 calls and {many} for 40: the include file is lexed again for each"
    );
}
