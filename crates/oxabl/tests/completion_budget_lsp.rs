//! The budget of completion in a file of 10,000 lines: a request after an edit
//! does not wait for the analysis of the new text. The marker
//! `OXABL-TEST-HOLD:completion.refresh` holds the background analysis for
//! three seconds, in test builds only.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use std::thread::sleep;
use std::time::{Duration, Instant};

use completion::{Case, Files, KEYWORD, VARIABLE};

const NEW_DECLARATION: &str = "DEFINE VARIABLE zNewName AS INTEGER NO-UNDO.\n";

/// The marker line, 2,500 procedures of four lines, and `last` as the last line.
fn large(last: &str) -> String {
    let mut text = String::from("/* OXABL-TEST-HOLD:completion.refresh */\n");
    for k in 0..2500 {
        text.push_str(&format!(
            "PROCEDURE p{k}:\n    DEFINE VARIABLE i{k} AS INTEGER NO-UNDO.\n    i{k} = i{k} + 1.\nEND PROCEDURE.\n"
        ));
    }
    text.push_str(last);
    text
}

/// The first request, an edit, and the request at once after it.
struct Run {
    ws: completion::Ws,
    /// The answer to the request after the edit, and how long it took.
    stale: completion::Answer,
    took: Duration,
    cursor: completion::Cursor,
}

fn run(word: &str) -> Run {
    let mut ws = Files::config_n(Case::Upper).start();
    let text = large(word);
    let first_line = text.matches('\n').count() as u32;
    let cursor = ws.open_marked("large.p", &format!("{text}|"));
    assert_eq!(cursor.line, first_line);

    // 1. The first request makes a base, and has no time limit.
    let first = ws.complete_at("large.p", cursor, None);
    first.list();
    if word == "DE" {
        first.list().contains(KEYWORD, &["DEFINE"]);
    }

    // 2. The edit adds a declaration above the last line.
    ws.insert_at_line_start("large.p", 2, first_line, NEW_DECLARATION);

    // 3. The request at once after it.
    let cursor = completion::Cursor {
        line: first_line + 1,
        character: word.len() as u32,
    };
    let sent = Instant::now();
    let stale = ws.complete_at("large.p", cursor, None);
    let took = sent.elapsed();
    Run {
        ws,
        stale,
        took,
        cursor,
    }
}

/// B1.
#[test]
fn a_completion_after_an_edit_does_not_wait_for_an_analysis() {
    let Run {
        mut ws,
        stale,
        took,
        cursor,
    } = run("DE");
    assert!(
        took < Duration::from_millis(1500),
        "the request after the edit took {took:?}; the analysis is held for 3 s"
    );
    let list = stale.list();
    assert!(
        list.incomplete,
        "a stale base is incomplete: {}",
        list.summary()
    );
    list.contains(KEYWORD, &["DEFINE"]);

    // 4. The refresh ends after the hold.
    let fresh = settled(&mut ws, cursor);
    fresh.list().contains(KEYWORD, &["DEFINE"]);
}

/// B2.
#[test]
fn a_stale_base_misses_the_new_name_and_the_fresh_base_has_it() {
    let Run {
        mut ws,
        stale,
        took,
        cursor,
    } = run("zN");
    assert!(
        took < Duration::from_millis(1500),
        "the request after the edit took {took:?}; the analysis is held for 3 s"
    );
    let list = stale.list();
    assert!(
        list.incomplete,
        "a stale base is incomplete: {}",
        list.summary()
    );
    list.lacks(&["zNewName"]);

    let fresh = settled(&mut ws, cursor);
    fresh.list().contains(VARIABLE, &["zNewName"]);
}

/// Wait 4 seconds, as the hold lasts 3, and ask again at `cursor`. A slow
/// machine can need longer for the analysis itself, so ask again until the list
/// is complete, for at most 30 more seconds. The first answer after the wait
/// and each later one must be a list.
fn settled(ws: &mut completion::Ws, cursor: completion::Cursor) -> completion::Answer {
    sleep(Duration::from_secs(4));
    let end = Instant::now() + Duration::from_secs(30);
    loop {
        let answer = ws.complete_at("large.p", cursor, None);
        if !answer.list().incomplete {
            return answer;
        }
        assert!(
            Instant::now() < end,
            "the list is still incomplete 34 s after the edit: {}",
            answer.summary()
        );
        sleep(Duration::from_millis(250));
    }
}
