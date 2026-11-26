//! `oxabl check` lexes and parses each text a fixed number of times.
//!
//! A test build of the binary counts the bytes it lexes and parses, and prints
//! the counts when `OXABL_TEST_LEX_PARSE` is set. A count is in bytes of text,
//! so a full lex of a file of `n` bytes adds `n`, and the short texts that some
//! passes lex alone add little.
//!
//! - A root is lexed for its analysis and for its format check, and parsed for
//!   each. A root that is already formatted is not lexed a third time for the
//!   format check's candidate.
//! - A file that many roots `RUN` is read, parsed and declared once for the
//!   run, whatever the number of threads.
//!
//! All fixtures are synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

/// What one run lexed and parsed, in bytes of text.
#[derive(Debug)]
struct Counts {
    lexed: usize,
    parsed: usize,
}

/// Run `oxabl` with `args` in `dir` and read the counts it printed.
fn counted(dir: &Path, args: &[&str]) -> (Counts, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_LEX_PARSE", "1")
        .args(args)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr);
    let line = stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: lexers: "))
        .unwrap_or_else(|| panic!("the test build printed no counts; stderr:\n{stderr}"));
    let field = |name: &str| -> usize {
        line.split(", ")
            .find_map(|part| part.strip_prefix(name))
            .unwrap_or_else(|| panic!("no `{name}` in {line:?}"))
            .parse()
            .unwrap()
    };
    let counts = Counts {
        lexed: field("lexed bytes: "),
        parsed: field("parsed bytes: "),
    };
    (counts, stdout)
}

/// A procedure file of `units` repeated blocks, already formatted. It imports
/// a package and writes an attribute of a name it does not define
/// (`hThing:LABEL`), so the resolve pass asks whether the name follows a widget
/// keyword, which it reads from the tokens of the file.
fn program(prefix: &str, units: usize) -> String {
    let mut source = String::from("USING Progress.Lang.*.\n\n");
    for n in 0..units {
        source.push_str(&format!(
            "\
DEFINE VARIABLE {prefix}Count{n} AS INTEGER NO-UNDO.
DEFINE VARIABLE {prefix}Name{n} AS CHARACTER NO-UNDO.

PROCEDURE {prefix}work{n}:
    DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.

    DO {prefix}Count{n} = 1 TO piLimit:
        IF {prefix}Count{n} MODULO 2 = 0 THEN
            {prefix}Name{n} = \"even\".
        ELSE
            {prefix}Name{n} = \"odd\".
        hThing{n}:LABEL = {prefix}Name{n}.
    END.
END PROCEDURE.

"
        ));
    }
    source
}

/// Format `file` in `dir` and fail when `oxabl check` would still report drift.
fn format_in_place(dir: &Path, file: &str) {
    let status = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["format", file])
        .status()
        .unwrap();
    assert!(status.success(), "oxabl format {file} failed");
}

#[test]
fn a_root_is_lexed_and_parsed_once_for_its_analysis_and_once_for_its_format_check() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("root.p"), program("r", 60)).unwrap();
    format_in_place(tmp.path(), "root.p");
    let size = fs::read(tmp.path().join("root.p")).unwrap().len();

    let (counts, stdout) = counted(tmp.path(), &["check", "-j", "1", "root.p"]);
    assert!(
        stdout.contains("checked 1 file: no findings, no drift"),
        "the fixture must be clean: {stdout}"
    );
    // Two lexes of the file, and the short texts the passes lex alone. A third
    // lex of the file (the format check's candidate, or a pass that lexes the
    // whole text again) passes the bound.
    assert!(
        counts.lexed < 2 * size + size / 2,
        "a root of {size} bytes, already formatted, was lexed more than two times: {counts:?}"
    );
    assert!(
        counts.parsed <= 2 * size,
        "a root of {size} bytes was parsed more than two times: {counts:?}"
    );

    // A root with drift: its candidate is lexed too, so three lexes.
    let drifted = fs::read_to_string(tmp.path().join("root.p"))
        .unwrap()
        .replace("\n    ", "\n");
    fs::write(tmp.path().join("root.p"), &drifted).unwrap();
    let (counts, _) = counted(tmp.path(), &["check", "-j", "1", "root.p"]);
    let size = drifted.len();
    assert!(
        counts.lexed > 2 * size + size / 2,
        "the candidate of a root with drift must be lexed: {counts:?}"
    );
}

#[test]
fn a_file_that_many_roots_run_is_indexed_once_for_the_run() {
    let tmp = TempDir::new().unwrap();
    let (src, lib) = (tmp.path().join("src"), tmp.path().join("lib"));
    fs::create_dir_all(&src).unwrap();
    fs::create_dir_all(&lib).unwrap();
    fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"counts\"\n\n[workspace.sources]\ninclude_paths = [\"lib\"]\n",
    )
    .unwrap();
    // Large targets beside small roots, so the parses of the targets are most
    // of the count.
    let targets = ["alpha", "beta", "gamma"];
    for target in targets {
        fs::write(lib.join(format!("{target}.p")), program(target, 40)).unwrap();
    }
    let roots = 24;
    for n in 0..roots {
        let mut source = String::new();
        for target in targets {
            source.push_str(&format!("RUN {target}.p.\n"));
        }
        source.push_str(&format!("MESSAGE \"root {n}\".\n"));
        fs::write(src.join(format!("root{n}.p")), source).unwrap();
    }
    let target_bytes: usize = targets
        .iter()
        .map(|t| fs::read(lib.join(format!("{t}.p"))).unwrap().len())
        .sum();
    let root_bytes: usize = (0..roots)
        .map(|n| fs::read(src.join(format!("root{n}.p"))).unwrap().len())
        .sum();

    for jobs in ["1", "4"] {
        let (counts, stdout) = counted(tmp.path(), &["check", "-j", jobs, "--no-format", "src"]);
        assert!(
            stdout.contains(&format!("checked {roots} files: no findings")),
            "the fixture must be clean and its targets found (-j {jobs}): {stdout}"
        );
        // Each root parsed once for its analysis; each target once for the run.
        // A target parsed a second time, as by a second thread, passes the bound.
        let budget = root_bytes + target_bytes + target_bytes / 2;
        assert!(
            counts.parsed <= budget,
            "-j {jobs}: the targets ({target_bytes} bytes) were parsed more than once for the \
             run, or the roots ({root_bytes} bytes) more than once each: {counts:?}"
        );
        assert!(
            counts.parsed >= root_bytes + target_bytes,
            "-j {jobs}: the targets must be indexed for the run: {counts:?}"
        );
    }
}
