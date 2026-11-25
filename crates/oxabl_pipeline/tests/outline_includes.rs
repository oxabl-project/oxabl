//! The outline with the declarations of include files, through the real
//! preprocessor and file system.

use std::fmt::Write as _;
use std::fs;

use oxabl_pipeline::{
    ConfigOverrides, IncludeOutline, LintPipeline, MAX_INCLUDED_SYMBOLS, OutlineNode,
    PipelineConfig, outline_with_includes,
};
use oxabl_workspace::RealFileSystem;

/// The outline of `main.p` in a directory with `files`, one line for each
/// symbol: indentation, kind, name, detail, the file it opens, and its own
/// file when it is not the buffer.
fn outline_of(files: &[(&str, &str)], main: &str) -> String {
    let bytes: Vec<(&str, &[u8])> = files
        .iter()
        .map(|(name, text)| (*name, text.as_bytes()))
        .collect();
    outline_of_bytes(&bytes, main)
}

fn outline_of_bytes(files: &[(&str, &[u8])], main: &str) -> String {
    outline_as("main.p", files, main)
}

/// As [`outline_of`], with the buffer in a file named `name`.
fn outline_as(name: &str, files: &[(&str, &[u8])], main: &str) -> String {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    for (name, bytes) in files {
        fs::write(root.join(name), bytes).unwrap();
    }
    fs::write(root.join(name), main).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let (config, warnings) = PipelineConfig::resolve(&root.join(name), &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs)
        .with_preprocess(true)
        .with_branch_tracking(true);
    let expansion = run.with_file(root.join(name)).expand(main);
    let outline = outline_with_includes(main, &expansion).unwrap();
    render(&outline)
}

fn render(outline: &IncludeOutline) -> String {
    fn walk(out: &mut String, outline: &IncludeOutline, nodes: &[OutlineNode], depth: usize) {
        for node in nodes {
            let _ = write!(out, "{}{:?} {}", "  ".repeat(depth), node.kind, node.name);
            if let Some(detail) = &node.detail {
                let _ = write!(out, " [{detail}]");
            }
            if let Some(opens) = node.opens {
                let _ = write!(
                    out,
                    " -> {}",
                    outline.files[opens]
                        .path
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                );
            }
            if let Some(file) = node.file {
                let _ = write!(
                    out,
                    " @{}",
                    outline.files[file]
                        .path
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                );
            }
            out.push('\n');
            walk(out, outline, &node.children, depth + 1);
        }
    }
    let mut out = String::new();
    walk(&mut out, outline, &outline.symbols, 0);
    out
}

#[test]
fn an_include_holds_its_declarations() {
    let got = outline_of(
        &[(
            "defs.i",
            "DEFINE TEMP-TABLE ttOrder FIELD id AS INTEGER.\nPROCEDURE helper:\nEND PROCEDURE.\n",
        )],
        "{defs.i}\nPROCEDURE own:\nEND PROCEDURE.\n",
    );
    assert_eq!(
        got,
        "Include defs.i -> defs.i\n\
         \x20 TempTable ttOrder @defs.i\n\
         \x20   Field id [INTEGER] @defs.i\n\
         \x20 Procedure helper @defs.i\n\
         Procedure own\n"
    );
}

#[test]
fn nested_includes_nest_in_the_order_of_their_references() {
    let got = outline_of(
        &[
            (
                "a.i",
                "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n{b.i}\nDEFINE VARIABLE va2 AS INTEGER NO-UNDO.\n",
            ),
            ("b.i", "DEFINE VARIABLE vb AS INTEGER NO-UNDO.\n"),
        ],
        "{a.i}\n",
    );
    assert_eq!(
        got,
        "Include a.i -> a.i\n\
         \x20 Variable va [INTEGER] @a.i\n\
         \x20 Include b.i -> b.i @a.i\n\
         \x20   Variable vb [INTEGER] @b.i\n\
         \x20 Variable va2 [INTEGER] @a.i\n"
    );
}

#[test]
fn an_include_that_is_not_found_is_marked() {
    assert_eq!(
        outline_of(&[], "{gone.i}\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n"),
        "Include gone.i [not found]\nVariable x [INTEGER]\n"
    );
}

#[test]
fn an_include_that_cannot_be_read_is_marked() {
    let tmp = tempfile::TempDir::new().unwrap();
    fs::create_dir(tmp.path().join("dir.i")).unwrap();
    // A directory exists and cannot be read as a file.
    let got = outline_of_dir_entry("dir.i");
    assert_eq!(got, "Include dir.i [cannot be read]\n");
}

fn outline_of_dir_entry(name: &str) -> String {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    fs::create_dir(root.join(name)).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let main = format!("{{{name}}}\n");
    let (config, _) = PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs)
        .with_preprocess(true)
        .with_branch_tracking(true);
    let expansion = run.with_file(root.join("main.p")).expand(&main);
    render(&outline_with_includes(&main, &expansion).unwrap())
}

#[test]
fn an_encrypted_include_is_marked() {
    let header = [0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];
    assert_eq!(
        outline_of_bytes(&[("secret.i", &header)], "{secret.i}\n"),
        "Include secret.i [encrypted]\n"
    );
}

#[test]
fn a_branch_that_is_not_compiled_adds_nothing() {
    let got = outline_of(
        &[
            ("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n"),
            (
                "cond.i",
                "&IF FALSE &THEN\nDEFINE VARIABLE hidden AS INTEGER NO-UNDO.\n&ELSE\n\
                 DEFINE VARIABLE shown AS INTEGER NO-UNDO.\n&ENDIF\n{a.i}\n",
            ),
        ],
        "&IF FALSE &THEN\n{a.i}\n&ENDIF\n{cond.i}\n",
    );
    assert_eq!(
        got,
        "Include a.i [not compiled]\n\
         Include cond.i -> cond.i\n\
         \x20 Variable shown [INTEGER] @cond.i\n\
         \x20 Include a.i -> a.i @cond.i\n\
         \x20   Variable va [INTEGER] @a.i\n"
    );
}

#[test]
fn an_include_inside_a_branch_of_an_include_that_is_not_compiled_is_marked() {
    let got = outline_of(
        &[
            ("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n"),
            ("cond.i", "&IF FALSE &THEN\n{a.i}\n&ENDIF\n"),
        ],
        "{cond.i}\n",
    );
    assert_eq!(
        got,
        "Include cond.i -> cond.i\n  Include a.i [not compiled] @cond.i\n"
    );
}

#[test]
fn a_cycle_is_marked_where_it_closes() {
    let got = outline_of(
        &[
            ("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n{b.i}\n"),
            ("b.i", "{a.i}\nDEFINE VARIABLE vb AS INTEGER NO-UNDO.\n"),
        ],
        "{a.i}\n",
    );
    assert_eq!(
        got,
        "Include a.i -> a.i\n\
         \x20 Variable va [INTEGER] @a.i\n\
         \x20 Include b.i -> b.i @a.i\n\
         \x20   Include a.i [not expanded] @b.i\n\
         \x20   Variable vb [INTEGER] @b.i\n"
    );
}

#[test]
fn an_include_read_twice_shows_under_each_reference() {
    let got = outline_of(
        &[("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n")],
        "{a.i}\nPROCEDURE p:\n  {a.i}\nEND PROCEDURE.\n",
    );
    assert_eq!(
        got,
        "Include a.i -> a.i\n\
         \x20 Variable va [INTEGER] @a.i\n\
         Procedure p\n\
         \x20 Include a.i -> a.i\n\
         \x20   Variable va [INTEGER] @a.i\n"
    );
}

#[test]
fn an_include_in_the_middle_of_a_statement_gets_a_symbol_for_its_declarations() {
    let got = outline_of(
        &[("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n")],
        "DEFINE VARIABLE x AS INTEGER NO-UNDO {a.i}.\n",
    );
    assert_eq!(got, "Include a.i -> a.i\n  Variable va [INTEGER] @a.i\n");
}

#[test]
fn an_include_with_no_declaration_keeps_its_symbol() {
    let got = outline_of(
        &[("empty.i", ""), ("defs.i", "&GLOBAL-DEFINE K 1\n")],
        "{empty.i}\n{defs.i}\n",
    );
    assert_eq!(
        got,
        "Include empty.i -> empty.i\n\
         Include defs.i -> defs.i\n\
         \x20 PreprocessorDefine K [GLOBAL] @defs.i\n"
    );
}

#[test]
fn a_name_that_an_argument_writes_shows_as_written() {
    let got = outline_of(
        &[(
            "a.i",
            "DEFINE {1} SHARED VARIABLE {&p}x AS INTEGER NO-UNDO.\n",
        )],
        "{a.i NEW &p=q}\n",
    );
    assert_eq!(got, "Include a.i -> a.i\n  Variable {&p}x [INTEGER] @a.i\n");
}

fn variables(count: usize) -> String {
    let mut text = String::new();
    for n in 0..count {
        text.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    text
}

fn top_level(outline: &str) -> Vec<&str> {
    outline
        .lines()
        .filter(|line| !line.starts_with("  "))
        .collect()
}

#[test]
fn an_include_that_alone_passes_the_limit_is_too_large() {
    let big = variables(MAX_INCLUDED_SYMBOLS + 1);
    let got = outline_of(
        &[
            ("big.i", &big),
            ("small.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n"),
        ],
        "{big.i}\n{small.i}\n",
    );
    assert_eq!(
        top_level(&got),
        ["Include big.i [too large]", "Include small.i -> small.i"]
    );
}

#[test]
fn includes_after_the_limit_say_that_the_limit_was_reached_not_that_they_are_large() {
    let big = variables(MAX_INCLUDED_SYMBOLS);
    let got = outline_of(
        &[
            ("big.i", &big),
            ("small.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n"),
        ],
        "{big.i}\n{small.i}\n{big.i}\n",
    );
    assert_eq!(
        top_level(&got),
        [
            "Include big.i -> big.i",
            "Include small.i [outline limit reached]",
            "Include big.i [outline limit reached]"
        ]
    );
}

#[test]
fn an_include_that_fits_the_rest_of_the_limit_shows_and_a_larger_one_is_marked() {
    let part = variables(MAX_INCLUDED_SYMBOLS - 10);
    let got = outline_of(
        &[
            ("part.i", &part),
            ("ten.i", &variables(10)),
            ("eleven.i", &variables(11)),
        ],
        "{part.i}\n{eleven.i}\n{ten.i}\n",
    );
    assert_eq!(
        top_level(&got),
        [
            "Include part.i -> part.i",
            "Include eleven.i [outline limit reached]",
            "Include ten.i -> ten.i"
        ]
    );
}

#[test]
fn an_include_that_an_if_leaves_empty_shows_nothing() {
    let got = outline_of(
        &[(
            "cond.i",
            "&IF FALSE &THEN\nDEFINE VARIABLE hidden AS INTEGER NO-UNDO.\n&ENDIF\n",
        )],
        "{cond.i}\n",
    );
    assert_eq!(got, "Include cond.i -> cond.i\n");
}

const VAR: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";

#[test]
fn a_branch_that_the_preprocessor_only_guessed_is_not_called_not_compiled() {
    // `missing.i` did not arrive, so the preprocessor cannot know `useb`.
    let got = outline_of(
        &[
            ("b.i", VAR),
            (
                "a.i",
                "&IF DEFINED(useb) &THEN\nDEFINE VARIABLE guessed AS INTEGER NO-UNDO.\n&ENDIF\n",
            ),
        ],
        "{missing.i}\n&IF DEFINED(useb) &THEN\n{b.i}\n&ENDIF\n{a.i}\n",
    );
    assert_eq!(
        got,
        "Include missing.i [not found]\n\
         Include b.i [may not be compiled]\n\
         Include a.i -> a.i\n\
         \x20 Variable guessed [INTEGER] @a.i\n"
    );
}

#[test]
fn an_include_opened_by_itself_keeps_what_its_includer_may_define() {
    let got = outline_as(
        "frag.i",
        &[("b.i", VAR.as_bytes())],
        "&IF DEFINED(x) &THEN\n{b.i}\nDEFINE VARIABLE fragv AS INTEGER NO-UNDO.\n&ENDIF\n",
    );
    assert_eq!(
        got,
        "Include b.i [may not be compiled]\nVariable fragv [INTEGER]\n"
    );
}

#[test]
fn a_branch_that_is_decided_stays_not_compiled_beside_one_that_is_guessed() {
    let got = outline_of(
        &[("b.i", VAR), ("c.i", VAR)],
        "{gone.i}\n&IF DEFINED(useb) &THEN\n{b.i}\n&ENDIF\n&IF FALSE &THEN\n{c.i}\n&ENDIF\n",
    );
    assert_eq!(
        got,
        "Include gone.i [not found]\n\
         Include b.i [may not be compiled]\n\
         Include c.i [not compiled]\n"
    );
}

#[test]
fn an_include_written_through_a_value_shows_at_its_reference() {
    let got = outline_of(
        &[("c.i", VAR)],
        "&SCOPED-DEFINE inc {c.i}\n&SCOPED-DEFINE bad {nothere.i}\n{&inc}\n{&bad}\n",
    );
    assert_eq!(
        got,
        "PreprocessorDefine inc [SCOPED]\n\
         PreprocessorDefine bad [SCOPED]\n\
         Include {&inc} -> c.i\n\
         \x20 Variable x [INTEGER] @c.i\n\
         Include {&bad} [not found]\n"
    );
}

#[test]
fn an_unresolved_include_in_the_middle_of_a_statement_is_marked() {
    let got = outline_of(&[], "DEFINE VARIABLE y AS INTEGER NO-UNDO {gone.i}.\n");
    assert_eq!(got, "Include gone.i [not found]\n");
}

#[test]
fn an_include_that_a_version_shim_splits_shows_that_it_is_partial() {
    let got = outline_of(
        &[(
            "shim.i",
            "&IF DEFINED(OLDAPI) &THEN\nPROCEDURE oldp:\n&ELSE\nPROCEDURE newp:\n&ENDIF\n\
             \x20 DEFINE VARIABLE inner AS INTEGER NO-UNDO.\nEND PROCEDURE.\n\
             DEFINE VARIABLE after AS INTEGER NO-UNDO.\n",
        )],
        "{shim.i}\n",
    );
    assert!(
        got.starts_with("Include shim.i [partial] -> shim.i"),
        "got: {got}"
    );
}
