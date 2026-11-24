//! Definitions written inside a block, through `oxabl check`.
//!
//! ABL scopes a `DEFINE` statement, a `FUNCTION` and a `PROCEDURE` to the
//! external procedure, internal procedure, user-defined function or method
//! that holds it, never to a `DO`, `REPEAT` or `FOR` block. So a name defined
//! inside a block is visible after the block's `END`, but never in another
//! routine. All fixtures are synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// One table, `orders`, with the fields `order-num` and `cust-name`.
const SCHEMA: &str = r#"ADD TABLE "orders"
  AREA "Data"
  DUMP-NAME "orders"

ADD FIELD "order-num" OF "orders" AS integer
  FORMAT ">>>>9"
  INITIAL "0"
  ORDER 10

ADD FIELD "cust-name" OF "orders" AS character
  FORMAT "x(30)"
  INITIAL ""
  ORDER 20

ADD INDEX "order-num" ON "orders"
  AREA "Index"
  UNIQUE
  PRIMARY
  INDEX-FIELD "order-num" ASCENDING

.
"#;

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// A project with the schema and `files`. Checks `main`, with the project
/// directory as the include path, and returns the lint findings as
/// `line:column CODE message`, in order.
fn findings(files: &[(&str, &str)], main: &str) -> Vec<String> {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("db.df"), SCHEMA);
    for (name, source) in files {
        write(&tmp.path().join(name), source);
    }
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("check")
        .arg("--json")
        .arg("--no-format")
        .arg("--schema")
        .arg(tmp.path().join("db.df"))
        .arg("-I")
        .arg(tmp.path())
        .arg(tmp.path().join(main))
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let v: Value = serde_json::from_str(&stdout).unwrap_or_else(|e| {
        panic!(
            "stdout is not JSON ({e}):\n{stdout}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stderr)
        )
    });
    assert!(
        v["failures"].as_array().is_some_and(|f| f.is_empty()),
        "the run failed: {v}"
    );
    let mut found: Vec<(u64, u64, String)> = v["diagnostics"]
        .as_array()
        .expect("diagnostics array")
        .iter()
        .map(|d| {
            (
                d["start"]["line"].as_u64().unwrap(),
                d["start"]["column"].as_u64().unwrap(),
                format!(
                    "{} {}",
                    d["code"].as_str().unwrap(),
                    d["message"].as_str().unwrap()
                ),
            )
        })
        .collect();
    found.sort();
    found
        .into_iter()
        .map(|(line, col, rest)| format!("{line}:{col} {rest}"))
        .collect()
}

fn assert_clean(files: &[(&str, &str)], main: &str) {
    let found = findings(files, main);
    assert!(
        found.is_empty(),
        "expected no findings in {main}, got:\n{}",
        found.join("\n")
    );
}

#[test]
fn a_buffer_defined_in_a_do_block_is_visible_after_the_block() {
    let source = "\
DO:
  DEFINE BUFFER b-orders FOR orders.
END.
FIND FIRST b-orders NO-LOCK NO-ERROR.
IF AVAILABLE b-orders THEN
  MESSAGE b-orders.order-num.
";
    assert_clean(&[("buf.p", source)], "buf.p");
}

#[test]
fn a_parameter_defined_in_a_do_block_is_visible_after_the_block() {
    let source = "\
DO:
  DEFINE INPUT PARAMETER pi-num AS INTEGER NO-UNDO.
END.
MESSAGE pi-num.
";
    assert_clean(&[("param.p", source)], "param.p");
}

#[test]
fn a_temp_table_defined_in_a_do_block_is_visible_after_the_block() {
    let source = "\
DO:
  DEFINE TEMP-TABLE tt-box NO-UNDO
    FIELD order-num LIKE orders.order-num
    FIELD weight    AS DECIMAL.
END.
CREATE tt-box.
ASSIGN tt-box.order-num = 1
       tt-box.weight    = 2.
";
    assert_clean(&[("ttfld2.p", source)], "ttfld2.p");
}

#[test]
fn a_like_temp_table_from_an_include_expanded_in_do_blocks_is_visible_after_them() {
    let include = "\
DEFINE {&isnew} SHARED TEMP-TABLE tt-glob NO-UNDO LIKE orders
  FIELD extra AS CHARACTER.
";
    let source = "\
&IF DEFINED(bootstrap) = 0 &THEN
DO:
  {glob2.i &isnew=\"NEW\"}
END.
&ELSE
DO:
  {glob2.i}
END.
&ENDIF
FIND FIRST tt-glob NO-ERROR.
IF AVAILABLE tt-glob THEN MESSAGE tt-glob.cust-name tt-glob.extra.
";
    assert_clean(&[("glob2.i", include), ("f.p", source)], "f.p");
}

#[test]
fn a_function_defined_in_a_do_block_is_callable_after_the_block() {
    let source = "\
DEFINE VARIABLE lOk AS LOGICAL NO-UNDO INITIAL TRUE.
IF lOk THEN
DO:
  FUNCTION fmt-amount RETURNS CHARACTER (pAmt AS DECIMAL, pCur AS CHARACTER):
    RETURN pCur + STRING(pAmt).
  END FUNCTION.
END.

MESSAGE fmt-amount(1.5, \"EUR\").
";
    assert_clean(&[("fndo.p", source)], "fndo.p");
}

#[test]
fn a_temp_table_defined_in_any_block_header_is_visible_after_the_block() {
    let headers = [
        "DO:",
        "DO ON ERROR UNDO, LEAVE:",
        "DO WHILE FALSE:",
        "DO TRANSACTION:",
        "REPEAT:",
        "FOR EACH orders NO-LOCK:",
        "IF TRUE THEN DO:",
    ];
    for header in headers {
        let source = format!(
            "{header}\n    DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.\nEND.\ntt.a = 1.\n"
        );
        let found = findings(&[("h.p", &source)], "h.p");
        assert!(
            found.is_empty(),
            "expected no findings after `{header}`, got:\n{}",
            found.join("\n")
        );
    }
}

#[test]
fn a_temp_table_defined_in_a_block_of_an_internal_procedure_is_visible_later_in_it() {
    let source = "\
PROCEDURE p:
  DO:
    DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
  END.
  tt.a = 1.
END PROCEDURE.
";
    assert_clean(&[("p.p", source)], "p.p");
}

#[test]
fn widgets_and_other_parameter_forms_defined_in_a_do_block_are_visible_after_the_block() {
    let source = "\
DO:
  DEFINE BUTTON bt LABEL \"x\".
  DEFINE FRAME fr bt.
  DEFINE PARAMETER BUFFER pb FOR orders.
  DEFINE INPUT PARAMETER TABLE-HANDLE ph.
END.
ENABLE bt WITH FRAME fr.
MESSAGE AVAILABLE pb VALID-HANDLE(ph).
";
    assert_clean(&[("w.p", source)], "w.p");
}

#[test]
fn streams_datasets_data_sources_and_procedures_in_a_do_block_belong_to_the_routine() {
    let source = "\
DO:
  DEFINE STREAM s-out.
  DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
  DEFINE DATASET ds FOR tt.
  DEFINE QUERY q FOR orders.
  DEFINE DATA-SOURCE src FOR QUERY q.
  PROCEDURE inner:
  END PROCEDURE.
END.
OUTPUT STREAM s-out TO \"x.txt\".
MESSAGE PAGE-NUMBER(s-out) DATASET ds:NAME.
RUN inner.
DEFINE DATA-SOURCE src FOR QUERY q.
";
    // The compiler rejects the second DATA-SOURCE: both are in one routine.
    let found = findings(&[("kinds.p", source)], "kinds.p");
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].starts_with("13:20 SEM0001 "), "{found:?}");
}

#[test]
fn a_temp_table_defined_in_catch_and_finally_is_visible_after_the_block() {
    let source = "\
DO ON ERROR UNDO, LEAVE:
  MESSAGE 1.
  CATCH e AS Progress.Lang.Error:
    DEFINE TEMP-TABLE tt-c NO-UNDO FIELD a AS INTEGER.
  END CATCH.
  FINALLY:
    DEFINE TEMP-TABLE tt-f NO-UNDO FIELD a AS INTEGER.
  END FINALLY.
END.
tt-c.a = 1.
tt-f.a = 2.
";
    assert_clean(&[("catch.p", source)], "catch.p");
}

/// A trigger is its own routine: what it defines is not visible after it.
#[test]
fn a_definition_in_a_trigger_does_not_leak_out_of_it() {
    let source = "\
DEFINE BUTTON bt LABEL \"x\".
ON CHOOSE OF bt DO:
  DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
  tt.a = 1.
END.
tt.a = 2.
";
    let found = findings(&[("trig.p", source)], "trig.p");
    assert_eq!(
        found,
        [
            "6:1 LINT0001 undefined symbol `tt`",
            "6:4 LINT0003 unknown field `a`",
        ],
    );
}

/// The definitions go to the routine, never further: a name defined in a block
/// of one routine stays undefined in the main block and in another routine.
#[test]
fn a_definition_in_a_block_does_not_leak_out_of_its_routine() {
    let source = "\
PROCEDURE p:
  DO:
    DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
    DEFINE BUFFER b-p FOR orders.
    DEFINE INPUT PARAMETER pi-num AS INTEGER NO-UNDO.
  END.
  tt.a = pi-num.
  FIND FIRST b-p NO-ERROR.
END PROCEDURE.
FUNCTION f RETURNS INTEGER ():
  DO:
    DEFINE BUFFER b-f FOR orders.
  END.
  FIND FIRST b-f NO-ERROR.
  RETURN 1.
END FUNCTION.
tt.a = 2.
MESSAGE pi-num AVAILABLE b-p AVAILABLE b-f f().
";
    let found = findings(&[("leak.p", source)], "leak.p");
    assert_eq!(
        found,
        [
            "17:1 LINT0001 undefined symbol `tt`",
            "17:4 LINT0003 unknown field `a`",
            "18:9 LINT0001 undefined symbol `pi-num`",
            "18:26 LINT0001 undefined symbol `b-p`",
            "18:40 LINT0001 undefined symbol `b-f`",
        ],
    );
}

#[test]
fn a_definition_in_a_block_of_a_method_does_not_leak_into_another_method() {
    let source = "\
CLASS Box:
  METHOD PUBLIC VOID Fill():
    DO:
      DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
      DEFINE BUFFER b-m FOR orders.
    END.
    tt.a = 1.
    FIND FIRST b-m NO-ERROR.
  END METHOD.
  METHOD PUBLIC VOID Show():
    tt.a = 2.
    FIND FIRST b-m NO-ERROR.
    MESSAGE AVAILABLE b-m.
  END METHOD.
END CLASS.
";
    let found = findings(&[("Box.cls", source)], "Box.cls");
    assert_eq!(
        found,
        [
            "11:5 LINT0001 undefined symbol `tt`",
            "11:8 LINT0003 unknown field `a`",
            "13:23 LINT0001 undefined symbol `b-m`",
        ],
    );
}
