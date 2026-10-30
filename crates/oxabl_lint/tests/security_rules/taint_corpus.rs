//! Precision and recall of stage 1 and stage 2 over `tests/security_corpus/taint`.
//!
//! Each program marks every sink line that a rule reports in either mode:
//! `/* taint: real */` when untrusted input reaches it, `/* taint: safe */`
//! when it does not (sanitised, constant, guarded). A finding on a line with no
//! mark fails the test, so the marks are the whole truth of the corpus.
//!
//! `limits.p` programs hold what stage 2 is known to get wrong; the expected
//! counts below pin those misses and false alarms. Run with `--nocapture` to
//! see the table.

use std::path::{Path, PathBuf};

use oxabl_common::{Diagnostic, SecurityOptions};

use oxabl_semantic::SourceContext;

use super::taint::{run_in, taint};

/// The rules with a taint mode, and the expected stage 2 result:
/// (true positives, false positives, false negatives).
const RULES: [(&str, (usize, usize, usize)); 10] = [
    ("ABLSEC001", (6, 1, 0)),
    ("ABLSEC002", (6, 0, 0)),
    ("ABLSEC003", (48, 1, 0)),
    ("ABLSEC004", (7, 0, 0)),
    ("ABLSEC005", (7, 0, 0)),
    ("ABLSEC006", (7, 0, 0)),
    ("ABLSEC007", (7, 0, 0)),
    ("ABLSEC014", (3, 0, 0)),
    ("ABLSEC015", (2, 0, 0)),
    ("ABLSEC016", (2, 0, 0)),
];

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/security_corpus/taint")
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Score {
    tp: usize,
    fp: usize,
    fn_: usize,
}

impl Score {
    fn add(&mut self, o: Score) {
        self.tp += o.tp;
        self.fp += o.fp;
        self.fn_ += o.fn_;
    }

    fn precision(self) -> f64 {
        ratio(self.tp, self.tp + self.fp)
    }

    fn recall(self) -> f64 {
        ratio(self.tp, self.tp + self.fn_)
    }
}

fn ratio(a: usize, b: usize) -> f64 {
    if b == 0 { 1.0 } else { a as f64 / b as f64 }
}

/// 1-based line of each finding.
fn lines(src: &str, diags: &[Diagnostic]) -> Vec<usize> {
    diags
        .iter()
        .map(|d| src[..d.span.span.start as usize].matches('\n').count() + 1)
        .collect()
}

fn score(path: &Path, src: &str, found: &[usize]) -> Score {
    let mark = |line: usize| {
        let text = src.lines().nth(line - 1).unwrap_or("");
        if text.contains("/* taint: real */") {
            Some(true)
        } else if text.contains("/* taint: safe */") {
            Some(false)
        } else {
            None
        }
    };
    let mut s = Score::default();
    for &line in found {
        match mark(line) {
            Some(true) => s.tp += 1,
            Some(false) => s.fp += 1,
            None => panic!(
                "{}:{line}: a finding on a line with no mark",
                path.display()
            ),
        }
    }
    for (i, _) in src
        .lines()
        .enumerate()
        .filter(|(_, l)| l.contains("/* taint: real */"))
    {
        if !found.contains(&(i + 1)) {
            s.fn_ += 1;
        }
    }
    s
}

fn programs(code: &str) -> Vec<PathBuf> {
    let dir = root().join(code.to_ascii_lowercase());
    let mut out: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("{}: {e}", dir.display()))
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "p" || e == "i"))
        .collect();
    out.sort();
    out
}

#[test]
fn stage_two_meets_the_marks_and_beats_stage_one() {
    let mut table =
        String::from("rule       | stage 1 TP FP FN  prec  rec | stage 2 TP FP FN  prec  rec\n");
    let (mut all1, mut all2) = (Score::default(), Score::default());
    for (code, expected) in RULES {
        let (mut s1, mut s2) = (Score::default(), Score::default());
        for path in programs(code) {
            let src = std::fs::read_to_string(&path).unwrap();
            // An include file is checked on its own, without its includer.
            let context = if path.extension().is_some_and(|e| e == "i") {
                SourceContext::IncludeFragment
            } else {
                SourceContext::CompilationUnit
            };
            let sink = run_in(code, &src, SecurityOptions::default(), context);
            let tainted = run_in(code, &src, taint(), context);
            s1.add(score(&path, &src, &lines(&src, &sink)));
            let found = lines(&src, &tainted);
            // Where stage 2 and the marks disagree, for the table reader.
            for (i, l) in src.lines().enumerate() {
                let hit = found.contains(&(i + 1));
                if l.contains("/* taint: real */") && !hit || l.contains("/* taint: safe */") && hit
                {
                    let name = path.file_name().unwrap().to_string_lossy();
                    let what = if hit { "reports" } else { "misses" };
                    println!("{code} {name}:{}: stage 2 {what}", i + 1);
                    assert_eq!(name, "limits.p", "only a limits program may disagree");
                }
            }
            s2.add(score(&path, &src, &found));
        }
        table.push_str(&format!(
            "{code}  | {:>10} {:>2} {:>2}  {:.2} {:.2} | {:>10} {:>2} {:>2}  {:.2} {:.2}\n",
            s1.tp,
            s1.fp,
            s1.fn_,
            s1.precision(),
            s1.recall(),
            s2.tp,
            s2.fp,
            s2.fn_,
            s2.precision(),
            s2.recall()
        ));
        assert_eq!((s2.tp, s2.fp, s2.fn_), expected, "{code}\n{table}");
        assert!(s2.precision() >= s1.precision(), "{code}\n{table}");
        all1.add(s1);
        all2.add(s2);
    }
    table.push_str(&format!(
        "all        | {:>10} {:>2} {:>2}  {:.2} {:.2} | {:>10} {:>2} {:>2}  {:.2} {:.2}\n",
        all1.tp,
        all1.fp,
        all1.fn_,
        all1.precision(),
        all1.recall(),
        all2.tp,
        all2.fp,
        all2.fn_,
        all2.precision(),
        all2.recall()
    ));
    println!("{table}");
}
