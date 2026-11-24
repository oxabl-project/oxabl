//! `oxabl format --stdout` on inputs where a second pass once changed the
//! output: each case has an exact expected output, and formatting that output
//! again must give the same bytes. The inputs of `fuzz/findings/format` are
//! replayed the same way. All fixtures are synthetic ABL.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;

/// Run `oxabl format --stdout` on `bytes`, written to a file named `name`.
fn format_once(dir: &Path, name: &str, bytes: &[u8]) -> Vec<u8> {
    let file = dir.join(name);
    fs::write(&file, bytes).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("format")
        .arg("--stdout")
        .arg(&file)
        .output()
        .unwrap();
    assert!(out.status.success(), "oxabl format failed: {out:?}");
    out.stdout
}

/// `input` formats to `expected`, and formatting `expected` changes nothing.
fn assert_formats(name: &str, input: &[u8], expected: &[u8]) {
    assert_formats_in(&TempDir::new().unwrap(), name, input, expected);
}

/// [`assert_formats`] in `tmp`, which can hold an `oxabl.toml`.
fn assert_formats_in(tmp: &TempDir, name: &str, input: &[u8], expected: &[u8]) {
    let once = format_once(tmp.path(), name, input);
    assert_eq!(
        String::from_utf8_lossy(&once),
        String::from_utf8_lossy(expected),
        "first format of {input:?}"
    );
    let twice = format_once(tmp.path(), name, &once);
    assert_eq!(
        String::from_utf8_lossy(&twice),
        String::from_utf8_lossy(&once),
        "second format of {input:?} changed the output"
    );
}

// --- a carriage return that no line feed follows --------------------------

#[test]
fn carriage_returns_before_a_line_feed_end_the_line() {
    assert_formats("only-blanks.p", b"\r\r\n\n\n", b"");
    assert_formats("one-statement.p", b"D\n\r\r\n\n\n", b"D\n");
}

#[test]
fn carriage_returns_in_a_crlf_file_do_not_make_new_lines() {
    assert_formats(
        "crlf.p",
        b"DO:\r\r\n\rMESSAGE 1.\r\n\r\r\nEND.\r\n",
        b"DO:\r\n    MESSAGE 1.\r\nEND.\r\n",
    );
}

#[test]
fn every_format_fuzz_finding_is_stable() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fuzz/findings/format");
    let mut inputs: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("{}: {e}", dir.display()))
        .map(|e| e.unwrap().path())
        .collect();
    inputs.sort();
    assert!(!inputs.is_empty(), "no inputs in {}", dir.display());
    let tmp = TempDir::new().unwrap();
    for input in &inputs {
        let name = input.file_name().unwrap().to_string_lossy();
        let once = format_once(tmp.path(), &name, &fs::read(input).unwrap());
        let twice = format_once(tmp.path(), &name, &once);
        assert_eq!(
            String::from_utf8_lossy(&twice),
            String::from_utf8_lossy(&once),
            "{}: the second format changed the output",
            input.display()
        );
    }
}

#[test]
fn carriage_returns_inside_a_string_keep_their_value() {
    // The line ending inside the string is part of its value; only the
    // `\r\n` that the output writes replaces one `\r` of the run.
    assert_formats(
        "string-crlf.p",
        b"DO:\r\nMESSAGE \"a\r\r\nb\".\r\nEND.\r\n",
        b"DO:\r\n    MESSAGE \"a\r\r\nb\".\r\nEND.\r\n",
    );
    assert_formats(
        "double-crlf.p",
        b"DO:\r\r\nMESSAGE \"a\r\n\r\r\nb\".\r\r\nEND.\r\r\n",
        b"DO:\r\n    MESSAGE \"a\r\n\r\r\nb\".\r\nEND.\r\n",
    );
}

// --- a statement that starts on a line inside a multi-line string ----------

#[test]
fn a_statement_after_a_multi_line_string_keeps_its_continuation_line() {
    assert_formats("lone-period.p", b"\"\n \".F\n  .", b"\"\n \".F\n  .\n");
}

#[test]
fn a_continuation_line_in_a_block_does_not_move_on_each_pass() {
    // The second line begins inside the string, so it keeps its bytes, and
    // `x = 1` starts on it: its continuation line moves by nothing.
    assert_formats(
        "in-block.p",
        b"DO:\nMESSAGE \"a\nb\". x = 1\n    + 2.\nEND.\n",
        b"DO:\n    MESSAGE \"a\nb\". x = 1\n    + 2.\nEND.\n",
    );
}

#[test]
fn a_statement_that_starts_before_the_string_still_aligns_its_continuation() {
    assert_formats(
        "message.p",
        b"DO:\nMESSAGE \"one\n two\"\n    VIEW-AS ALERT-BOX.\nEND.\n",
        b"DO:\n    MESSAGE \"one\n two\"\n        VIEW-AS ALERT-BOX.\nEND.\n",
    );
}

// --- property accessors -----------------------------------------------------

/// A directory whose `oxabl.toml` sets an indent of 3, as in the inputs below.
fn indent_3() -> TempDir {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.style]\nindent_size = 3\n",
    )
    .unwrap();
    tmp
}

const GET_AND_SET: &str = "\
class Probe:
   define public property Size as integer no-undo
      get():
         return 1.
      end get.
      set(input arg as integer):
         arg = arg + 1.
      end set.
end class.
";

const GETTER_ONLY: &str = "\
class Probe:
   define public property Size as integer no-undo
      get():
         def var n as integer no-undo.
         n = 1.
         return n.
      end get.
end class.
";

const STATIC: &str = "\
class Probe:
   define public static property Size as integer no-undo
      get():
         return 1.
      end get.
      set(input arg as integer):
         arg = arg + 1.
      end set.
end class.
";

#[test]
fn accessor_bodies_are_one_level_below_their_head() {
    let tmp = indent_3();
    for (name, src) in [
        ("GetAndSet.cls", GET_AND_SET),
        ("GetterOnly.cls", GETTER_ONLY),
        ("Static.cls", STATIC),
    ] {
        assert_formats_in(&tmp, name, src.as_bytes(), src.as_bytes());
    }
}

#[test]
fn accessor_bodies_and_ends_move_back_under_their_head() {
    let tmp = indent_3();
    assert_formats_in(
        &tmp,
        "GetAndSet.cls",
        b"\
class Probe:
   define public property Size as integer no-undo
      get():
      return 1.
      end get.
      set(input arg as integer):
      arg = arg + 1.
   end set.
end class.
",
        GET_AND_SET.as_bytes(),
    );
    assert_formats_in(
        &tmp,
        "GetterOnly.cls",
        b"\
class Probe:
   define public property Size as integer no-undo
      get():
      def var n as integer no-undo.
      n = 1.
      return n.
   end get.
end class.
",
        GETTER_ONLY.as_bytes(),
    );
}
