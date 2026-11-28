//! `oxabl check` lexes and parses each text a fixed number of times.
//!
//! A test build of the binary counts the bytes it lexes and parses, and prints
//! the counts when `OXABL_TEST_LEX_PARSE` is set. A count is in bytes of text,
//! so a full lex of a file of `n` bytes adds `n`, and the short texts that some
//! passes lex alone add little.
//!
//! - A root that expands to itself (no include, directive or reference) is
//!   lexed and parsed once, for its analysis and its format check together.
//!   A root that is already formatted is not lexed again for the format
//!   check's candidate.
//! - A root that does not expand to itself is lexed and parsed as expanded, for
//!   its analysis, and as written, for its format check.
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
fn a_root_that_expands_to_itself_is_lexed_and_parsed_once_for_its_analysis_and_its_format_check() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("root.p"), program("r", 60)).unwrap();
    format_in_place(tmp.path(), "root.p");
    let size = fs::read(tmp.path().join("root.p")).unwrap().len();

    let (counts, stdout) = counted(tmp.path(), &["check", "-j", "1", "root.p"]);
    assert!(
        stdout
            .lines()
            .any(|line| line == "checked 1 file: no findings, no drift"),
        "the fixture must be clean: {stdout}"
    );
    // One lex of the file for both stages, and the short texts the passes lex
    // alone (about 30% of the file here). A second lex of the file (the format
    // check on its own tokens, the analysis on the expanded text, or a pass
    // that lexes the whole text again) passes the bound.
    assert!(
        counts.lexed < size + size / 2,
        "a root of {size} bytes that expands to itself, already formatted, was lexed more than \
         once: {counts:?}"
    );
    assert!(
        counts.parsed <= size,
        "a root of {size} bytes that expands to itself was parsed more than once: {counts:?}"
    );

    // A root with drift: its candidate is a new text and is lexed too.
    let drifted = fs::read_to_string(tmp.path().join("root.p"))
        .unwrap()
        .replace("\n    ", "\n");
    fs::write(tmp.path().join("root.p"), &drifted).unwrap();
    let (counts, _) = counted(tmp.path(), &["check", "-j", "1", "root.p"]);
    assert!(
        counts.lexed >= drifted.len() + size,
        "the candidate of a root with drift must be lexed: {counts:?}"
    );
    assert!(
        counts.parsed <= drifted.len(),
        "a root with drift was parsed more than once: {counts:?}"
    );
}

#[test]
fn a_root_is_lexed_and_parsed_once_for_either_stage_alone() {
    let source = program("r", 60);
    for (name, toml, summary) in [
        (
            "format only",
            "[workspace]\nname = \"counts\"\n\n[workspace.features]\ndiagnostics = false\n",
            "checked 1 file: diagnostics off, no drift",
        ),
        (
            "analysis only",
            "[workspace]\nname = \"counts\"\n\n[workspace.features]\nformat = false\n",
            "checked 1 file: no findings, format off",
        ),
    ] {
        let tmp = TempDir::new().unwrap();
        fs::write(tmp.path().join("root.p"), &source).unwrap();
        format_in_place(tmp.path(), "root.p");
        fs::write(tmp.path().join("oxabl.toml"), toml).unwrap();
        let size = fs::read(tmp.path().join("root.p")).unwrap().len();
        let (counts, stdout) = counted(tmp.path(), &["check", "-j", "1", "root.p"]);
        assert!(
            stdout.lines().any(|line| line == summary),
            "{name}: {stdout}"
        );
        assert!(
            counts.lexed < size + size / 2 && counts.parsed <= size,
            "{name}: a root of {size} bytes was lexed or parsed more than once: {counts:?}"
        );
    }
}

#[test]
fn a_root_with_an_include_is_lexed_and_parsed_as_written_and_as_expanded() {
    let tmp = TempDir::new().unwrap();
    let include = program("i", 30);
    fs::write(tmp.path().join("part.i"), &include).unwrap();
    let root = "{part.i}\nMESSAGE \"done\".\n";
    fs::write(tmp.path().join("root.p"), root).unwrap();
    format_in_place(tmp.path(), "root.p");
    let root = fs::read_to_string(tmp.path().join("root.p")).unwrap();

    let (counts, stdout) = counted(tmp.path(), &["check", "-j", "1", "-I", ".", "root.p"]);
    assert!(
        stdout
            .lines()
            .any(|line| line == "checked 1 file: no findings, no drift"),
        "the root must be clean and formatted: {stdout}"
    );
    let expanded = include.len() + root.len();
    // The raw text of the root is short; the expanded text is most of the
    // count and is lexed and parsed once, by the analysis. The bounds are
    // two-sided: when the include does not resolve, they say nothing.
    assert!(
        counts.parsed >= expanded,
        "the include must be expanded and parsed: {counts:?}"
    );
    assert!(
        counts.parsed <= expanded + root.len(),
        "a root with an include was parsed more than once as expanded and once as written: \
         {counts:?}"
    );
    assert!(
        counts.lexed < expanded + root.len() + expanded / 2,
        "a root with an include was lexed more than once as expanded and once as written: \
         {counts:?}"
    );
}

/// `program`, with a parameter on its main block, so that a `RUN` with
/// arguments asks for the list of the target (`strict.calls`).
fn target(prefix: &str, units: usize) -> String {
    program(prefix, units).replacen(
        "USING Progress.Lang.*.\n",
        "USING Progress.Lang.*.\nDEFINE INPUT PARAMETER piTop AS INTEGER NO-UNDO.\n",
        1,
    )
}

/// A workspace of `roots` roots that each `RUN` three large targets, and the
/// bytes of the targets and of the roots.
fn run_workspace(dir: &Path, lint: &str, roots: usize) -> (usize, usize) {
    let (src, lib) = (dir.join("src"), dir.join("lib"));
    fs::create_dir_all(&src).unwrap();
    fs::create_dir_all(&lib).unwrap();
    fs::write(
        dir.join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"counts\"\n\n[workspace.sources]\ninclude_paths = [\"lib\"]\n\n{lint}"
        ),
    )
    .unwrap();
    // Large targets beside small roots, so the parses of the targets are most
    // of the count.
    let targets = ["alpha", "beta", "gamma"];
    for name in targets {
        fs::write(lib.join(format!("{name}.p")), target(name, 40)).unwrap();
    }
    for n in 0..roots {
        let mut source = String::new();
        for name in targets {
            source.push_str(&format!("RUN {name}.p ({n}).\n"));
        }
        source.push_str(&format!("MESSAGE \"root {n}\".\n"));
        fs::write(src.join(format!("root{n}.p")), source).unwrap();
    }
    let size = |path: std::path::PathBuf| fs::read(path).unwrap().len();
    let target_bytes = targets
        .iter()
        .map(|t| size(lib.join(format!("{t}.p"))))
        .sum();
    let root_bytes = (0..roots)
        .map(|n| size(src.join(format!("root{n}.p"))))
        .sum();
    (target_bytes, root_bytes)
}

#[test]
fn a_file_that_many_roots_run_is_indexed_once_for_the_run() {
    let roots = 24;
    // `strict.calls` reads the parameter list of each target, which the index
    // takes from the parse that gave its facts.
    for (lint, rule) in [
        ("", "default rules"),
        (
            "[workspace.lint]\nenable = [\"strict.calls\"]\n",
            "strict.calls",
        ),
    ] {
        let tmp = TempDir::new().unwrap();
        let (target_bytes, root_bytes) = run_workspace(tmp.path(), lint, roots);
        for jobs in ["1", "4"] {
            let (counts, stdout) =
                counted(tmp.path(), &["check", "-j", jobs, "--no-format", "src"]);
            assert!(
                stdout.contains(&format!("checked {roots} files: no findings")),
                "the fixture must be clean and its targets found ({rule}, -j {jobs}): {stdout}"
            );
            // Each root parsed once for its analysis; each target once for the
            // run. A target parsed a second time, as by a second thread or for
            // its parameter list, passes the bound.
            let budget = root_bytes + target_bytes + target_bytes / 2;
            assert!(
                counts.parsed <= budget,
                "{rule}, -j {jobs}: the targets ({target_bytes} bytes) were parsed more than once \
                 for the run, or the roots ({root_bytes} bytes) more than once each: {counts:?}"
            );
            assert!(
                counts.parsed >= root_bytes + target_bytes,
                "{rule}, -j {jobs}: the targets must be indexed for the run: {counts:?}"
            );
        }
    }
}
