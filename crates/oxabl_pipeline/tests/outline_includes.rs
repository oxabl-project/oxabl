//! The outline with the declarations of include files, through the real
//! preprocessor and file system.

use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

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
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    for (name, bytes) in files {
        fs::write(root.join(name), bytes).unwrap();
    }
    fs::write(root.join("main.p"), main).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let (config, warnings) =
        PipelineConfig::resolve(&root.join("main.p"), &ConfigOverrides::default());
    assert!(warnings.is_empty(), "{warnings:?}");
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs)
        .with_preprocess(true)
        .with_branch_tracking(true);
    let expansion = run.with_file(root.join("main.p")).expand(main);
    let sources: HashMap<PathBuf, Arc<str>> = expansion
        .dependency_paths()
        .iter()
        .filter_map(|path| Some((path.clone(), Arc::from(fs::read_to_string(path).ok()?))))
        .collect();
    let outline = outline_with_includes(main, &expansion, &sources).unwrap();
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
    render(&outline_with_includes(&main, &expansion, &HashMap::new()).unwrap())
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

#[test]
fn an_include_that_would_pass_the_limit_is_marked_and_the_next_one_fits() {
    let mut big = String::new();
    for n in 0..MAX_INCLUDED_SYMBOLS {
        big.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    let got = outline_of(
        &[
            ("big.i", &big),
            ("small.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n"),
        ],
        "{big.i}\n{small.i}\n{big.i}\n",
    );
    let lines: Vec<&str> = got.lines().filter(|line| !line.starts_with("  ")).collect();
    assert_eq!(
        lines,
        [
            "Include big.i -> big.i",
            "Include small.i [too large]",
            "Include big.i [too large]"
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
