//! The document outline of a large file: a class and a procedure file of several
//! thousand lines each. `parse/*` lex and parse the same text and stop, so the
//! cost of the walk shows as the gap to `outline/*`.

use std::fmt::Write;
use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_pipeline::outline;

const MEMBERS: usize = 300;

fn class_file() -> String {
    let mut s = String::from("USING Progress.Lang.*.\n\nCLASS app.Big IMPLEMENTS app.IThing:\n");
    for k in 0..MEMBERS {
        write!(
            s,
            "    DEFINE PUBLIC PROPERTY prop{k} AS CHARACTER NO-UNDO GET. SET.\n\
             DEFINE PRIVATE VARIABLE v{k} AS INTEGER NO-UNDO.\n\
             DEFINE PUBLIC EVENT changed{k} SIGNATURE VOID (INPUT n AS INTEGER).\n\
             CONSTRUCTOR PUBLIC Big{k}(INPUT a AS INTEGER):\n\
                 v{k} = a.\n\
             END CONSTRUCTOR.\n\
             METHOD PUBLIC INTEGER total{k}(INPUT a AS INTEGER, INPUT b AS INTEGER):\n\
                 DEFINE VARIABLE sum AS INTEGER NO-UNDO.\n\
                 DEFINE BUFFER bcust FOR Customer.\n\
                 main-loop: DO a = 1 TO b:\n\
                     DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n\
                     IF a > 2 THEN DO:\n\
                         DEFINE VARIABLE deeper AS INTEGER NO-UNDO.\n\
                         sum = sum + a.\n\
                     END.\n\
                 END.\n\
                 RETURN sum.\n\
             END METHOD.\n\
             DESTRUCTOR PUBLIC Big():\n\
             END DESTRUCTOR.\n"
        )
        .unwrap();
    }
    s.push_str("END CLASS.\n");
    s
}

fn procedure_file() -> String {
    let mut s = String::from("{inc/global.i}\n&GLOBAL-DEFINE APP \"big\"\n");
    for k in 0..MEMBERS {
        write!(
            s,
            "&SCOPED-DEFINE LOCAL{k} {k}\n\
             DEFINE TEMP-TABLE tt{k} NO-UNDO\n\
                 FIELD id AS INTEGER\n\
                 FIELD name AS CHARACTER\n\
                 FIELD qty AS DECIMAL\n\
                 INDEX pk IS PRIMARY UNIQUE id.\n\
             DEFINE VARIABLE v{k} AS INTEGER NO-UNDO.\n\
             DEFINE STREAM s{k}.\n\
             DEFINE QUERY q{k} FOR tt{k}.\n\
             PROCEDURE proc{k}:\n\
                 DEFINE INPUT PARAMETER ip AS INTEGER NO-UNDO.\n\
                 DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
                 FOR EACH tt{k}:\n\
                     DEFINE VARIABLE y AS INTEGER NO-UNDO.\n\
                     x = x + tt{k}.qty.\n\
                 END.\n\
             END PROCEDURE.\n\
             FUNCTION fn{k} RETURNS INTEGER (INPUT a AS INTEGER):\n\
                 RETURN a + {k}.\n\
             END FUNCTION.\n\
             ON CHOOSE OF btn{k} IN FRAME f DO:\n\
                 RUN proc{k}(INPUT 1).\n\
             END.\n\
             &IF DEFINED(TRACE) &THEN\n\
             DEFINE VARIABLE traced{k} AS INTEGER NO-UNDO.\n\
             &ELSE\n\
             DEFINE VARIABLE quiet{k} AS INTEGER NO-UNDO.\n\
             &ENDIF\n"
        )
        .unwrap();
    }
    s
}

fn bench_outline(c: &mut Criterion) {
    for (name, source) in [("class", class_file()), ("procedure", procedure_file())] {
        c.bench_function(&format!("parse/{name}"), |b| {
            b.iter(|| {
                let tokens = tokenize(black_box(&source));
                black_box(Parser::new(&tokens, &source).parse_program());
            })
        });
        c.bench_function(&format!("outline/{name}"), |b| {
            b.iter(|| black_box(outline(black_box(&source))))
        });
    }
}

criterion_group!(benches, bench_outline);
criterion_main!(benches);
