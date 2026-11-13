//! Black-box tests of `oxabl_pipeline::outline`, written from
//! `docs/design/document-outline.md`.
//!
//! A tree renders as one line per symbol, indented two spaces per depth:
//! `Kind name [detail] @"selection text"`. A test compares the whole text, so
//! a diff names the wrong line. Every outline also goes through
//! `check_rules`, the six rules of the spec.

use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_pipeline::{OutlineKind, OutlineSymbol, outline};

fn slice(source: &str, span: Span) -> &str {
    &source[span.start as usize..span.end as usize]
}

fn collapse(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Every violation of rules 2, 3 and 4 in `symbols`, as readable text.
fn rule_violations(source: &str, symbols: &[OutlineSymbol]) -> Vec<String> {
    fn walk(
        source: &str,
        parent: Option<&OutlineSymbol>,
        symbols: &[OutlineSymbol],
        out: &mut Vec<String>,
    ) {
        let mut previous_end = 0u32;
        for (index, sym) in symbols.iter().enumerate() {
            let at = format!("{:?} {:?}", sym.kind, sym.name);
            let (span, sel) = (sym.span, sym.selection_span);
            let len = source.len() as u32;
            if !(span.start <= sel.start && sel.start < sel.end && sel.end <= span.end) {
                out.push(format!(
                    "{at}: selection {sel:?} is not inside span {span:?}"
                ));
            }
            if span.end > len {
                out.push(format!("{at}: span {span:?} is past the end ({len})"));
            }
            for offset in [span.start, span.end, sel.start, sel.end] {
                if offset <= len && !source.is_char_boundary(offset as usize) {
                    out.push(format!("{at}: offset {offset} is not on a char boundary"));
                }
            }
            if let Some(parent) = parent
                && !(parent.span.start <= span.start && span.end <= parent.span.end)
            {
                out.push(format!(
                    "{at}: span {span:?} is outside parent {:?} {:?}",
                    parent.name, parent.span
                ));
            }
            if index > 0 && span.start < previous_end {
                out.push(format!(
                    "{at}: span {span:?} overlaps or precedes the sibling before it"
                ));
            }
            previous_end = span.end;
            if sym.name.is_empty() {
                out.push(format!("{at}: empty name"));
            }
            if sym.name.contains(['\n', '\r']) {
                out.push(format!("{at}: name has a line break"));
            }
            if sym.name != sym.name.trim() {
                out.push(format!("{at}: name has leading or trailing whitespace"));
            }
            walk(source, Some(sym), &sym.children, out);
        }
    }
    let mut out = Vec::new();
    walk(source, None, symbols, &mut out);
    out
}

fn check_rules(source: &str, symbols: &[OutlineSymbol]) {
    let violations = rule_violations(source, symbols);
    assert!(
        violations.is_empty(),
        "outline breaks the rules:\n{}\nsource:\n{source}",
        violations.join("\n")
    );
}

/// The outline of `source`, checked against the rules and for determinism.
fn run(source: &str) -> Vec<OutlineSymbol> {
    let first = outline(source).expect("outline must not panic");
    let second = outline(source).expect("outline must not panic");
    assert_eq!(first, second, "the same input gave two outlines");
    check_rules(source, &first);
    first
}

fn render_with(
    source: &str,
    symbols: &[OutlineSymbol],
    depth: usize,
    out: &mut Vec<String>,
    line: &dyn Fn(&str, &OutlineSymbol) -> String,
) {
    for sym in symbols {
        out.push(format!("{}{}", "  ".repeat(depth), line(source, sym)));
        render_with(source, &sym.children, depth + 1, out, line);
    }
}

fn render_by(source: &str, line: &dyn Fn(&str, &OutlineSymbol) -> String) -> String {
    let mut out = Vec::new();
    render_with(source, &run(source), 0, &mut out, line);
    out.join("\n")
}

/// `Kind name [detail] @"selection"`.
fn render(source: &str) -> String {
    render_by(source, &|source, sym| {
        let detail = sym
            .detail
            .as_ref()
            .map(|d| format!(" [{d}]"))
            .unwrap_or_default();
        format!(
            "{:?} {}{detail} @{:?}",
            sym.kind,
            sym.name,
            slice(source, sym.selection_span)
        )
    })
}

/// `Kind name => "whole span text, whitespace collapsed"`.
fn render_spans(source: &str) -> String {
    render_by(source, &|source, sym| {
        format!(
            "{:?} {} => {:?}",
            sym.kind,
            sym.name,
            collapse(slice(source, sym.span))
        )
    })
}

#[track_caller]
fn assert_tree(source: &str, expected: &str) {
    assert_eq!(render(source), expected.trim_matches('\n'));
}

#[track_caller]
fn assert_spans(source: &str, expected: &str) {
    assert_eq!(render_spans(source), expected.trim_matches('\n'));
}

fn flatten<'a>(symbols: &'a [OutlineSymbol], out: &mut Vec<&'a OutlineSymbol>) {
    for sym in symbols {
        out.push(sym);
        flatten(&sym.children, out);
    }
}

fn names(symbols: &[OutlineSymbol]) -> Vec<&str> {
    symbols.iter().map(|s| s.name.as_str()).collect()
}

// ---------------------------------------------------------------- symbols

#[test]
fn class_members_are_children_of_the_class() {
    let src = r#"CLASS A.B.Cust FINAL:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC EVENT Changed SIGNATURE VOID (INPUT n AS INTEGER).
    CONSTRUCTOR PUBLIC Cust(INPUT x AS INTEGER):
    END CONSTRUCTOR.
    DESTRUCTOR PUBLIC Cust():
    END DESTRUCTOR.
    METHOD PUBLIC STATIC CHARACTER Hello(INPUT p AS INTEGER):
        RETURN "".
    END METHOD.
    METHOD PUBLIC VOID Run():
    END METHOD.
END CLASS.
"#;
    assert_tree(
        src,
        r#"
Class A.B.Cust [FINAL] @"A.B.Cust"
  Property Name [PUBLIC CHARACTER] @"Name"
  Event Changed [PUBLIC] @"Changed"
    Parameter n [INPUT INTEGER] @"n"
  Constructor Cust [PUBLIC] @"CONSTRUCTOR"
    Parameter x [INPUT INTEGER] @"x"
  Destructor Cust [DESTRUCTOR] @"DESTRUCTOR"
  Method Hello [PUBLIC STATIC CHARACTER] @"Hello"
    Parameter p [INPUT INTEGER] @"p"
  Method Run [PUBLIC VOID] @"Run"
"#,
    );
}

#[test]
fn abstract_class_and_method_details() {
    let src = "CLASS Shape ABSTRACT:\n\
        METHOD PUBLIC ABSTRACT VOID Draw().\n\
        METHOD PUBLIC OVERRIDE VOID Fill():\nEND METHOD.\n\
        METHOD PROTECTED STATIC INTEGER Count():\nRETURN 1.\nEND METHOD.\n\
        END CLASS.\n";
    assert_tree(
        src,
        r#"
Class Shape [ABSTRACT] @"Shape"
  Method Draw [PUBLIC ABSTRACT VOID] @"Draw"
  Method Fill [PUBLIC OVERRIDE VOID] @"Fill"
  Method Count [PROTECTED STATIC INTEGER] @"Count"
"#,
    );
}

#[test]
fn interface_has_no_detail() {
    let src = "INTERFACE I.Foo:\n  METHOD PUBLIC VOID M().\nEND INTERFACE.\n";
    assert_tree(
        src,
        r#"
Interface I.Foo @"I.Foo"
  Method M [PUBLIC VOID] @"M"
"#,
    );
}

#[test]
fn class_without_a_modifier_has_no_detail() {
    assert_tree(
        "CLASS Plain:\nEND CLASS.\n",
        r#"
Class Plain @"Plain"
"#,
    );
}

#[test]
fn static_property_detail() {
    let src = "CLASS P:\n  DEFINE PUBLIC STATIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.\nEND CLASS.\n";
    assert_tree(
        src,
        r#"
Class P @"P"
  Property Count [PUBLIC STATIC INTEGER] @"Count"
"#,
    );
}

#[test]
fn procedures_with_parameters_and_variables() {
    let src = r#"PROCEDURE doit:
    DEFINE INPUT PARAMETER pId AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttOut.
    DEFINE PARAMETER BUFFER bCust FOR Customer.
    DEFINE VARIABLE x AS INTEGER NO-UNDO.
END PROCEDURE.

PROCEDURE empty:
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Procedure doit @"doit"
  Parameter pId [INPUT INTEGER] @"pId"
  Parameter ttOut [OUTPUT TABLE] @"ttOut"
  Parameter bCust [BUFFER FOR Customer] @"bCust"
  Variable x [INTEGER] @"x"
Procedure empty @"empty"
"#,
    );
}

#[test]
fn external_and_in_procedures() {
    let src = "PROCEDURE ext EXTERNAL \"lib.dll\":\n  DEFINE INPUT PARAMETER a AS LONG.\nEND PROCEDURE.\n\
        PROCEDURE proto IN hProc.\n";
    assert_tree(
        src,
        r#"
Procedure ext [EXTERNAL] @"ext"
  Parameter a [INPUT LONG] @"a"
Procedure proto [PROTOTYPE] @"proto"
"#,
    );
}

#[test]
fn function_parameters_come_first() {
    let src = r#"FUNCTION f RETURNS CHARACTER (INPUT a AS INTEGER, INPUT b AS CHARACTER):
    DEFINE VARIABLE local AS INTEGER NO-UNDO.
    RETURN "".
END FUNCTION.
"#;
    assert_tree(
        src,
        r#"
Function f [CHARACTER] @"f"
  Parameter a [INPUT INTEGER] @"a"
  Parameter b [INPUT CHARACTER] @"b"
  Variable local [INTEGER] @"local"
"#,
    );
}

#[test]
fn forward_prototype_is_omitted_when_the_function_is_defined() {
    let src = r#"FUNCTION calc RETURNS INTEGER (INPUT a AS INTEGER) FORWARD.
PROCEDURE p:
END PROCEDURE.
FUNCTION CALC RETURNS INTEGER (INPUT a AS INTEGER):
    RETURN a.
END FUNCTION.
"#;
    assert_tree(
        src,
        r#"
Procedure p @"p"
Function CALC [INTEGER] @"CALC"
  Parameter a [INPUT INTEGER] @"a"
"#,
    );
}

#[test]
fn undefined_prototypes_show_with_parameters() {
    let src = "FUNCTION lonely RETURNS INTEGER (INPUT a AS INTEGER, INPUT b AS INTEGER) FORWARD.\n\
        FUNCTION remote RETURNS CHARACTER (INPUT s AS CHARACTER) IN hSuper.\n";
    assert_tree(
        src,
        r#"
Function lonely [INTEGER] @"lonely"
  Parameter a [INPUT INTEGER] @"a"
  Parameter b [INPUT INTEGER] @"b"
Function remote [CHARACTER] @"remote"
  Parameter s [INPUT CHARACTER] @"s"
"#,
    );
}

#[test]
fn variable_types() {
    let src = r#"DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE arr AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE nm LIKE Customer.Name NO-UNDO.
DEFINE VARIABLE o AS Foo.Bar NO-UNDO.
"#;
    assert_tree(
        src,
        r#"
Variable i [INTEGER] @"i"
Variable arr [CHARACTER EXTENT 3] @"arr"
Variable nm [LIKE Customer.Name] @"nm"
Variable o [Foo.Bar] @"o"
"#,
    );
}

#[test]
fn var_statement_gives_one_symbol_for_each_name() {
    let src = "VAR INTEGER a, b, c.\nVAR CHARACTER s = \"x\".\n";
    assert_tree(
        src,
        r#"
Variable a [INTEGER] @"a"
Variable b [INTEGER] @"b"
Variable c [INTEGER] @"c"
Variable s [CHARACTER] @"s"
"#,
    );
}

#[test]
fn temp_table_fields_are_children() {
    let src = r#"DEFINE TEMP-TABLE tt NO-UNDO
    FIELD id AS INTEGER
    FIELD nm AS CHARACTER
    INDEX ix IS PRIMARY id.
DEFINE TEMP-TABLE tt2 LIKE tt.
"#;
    assert_tree(
        src,
        r#"
TempTable tt @"tt"
  Field id [INTEGER] @"id"
  Field nm [CHARACTER] @"nm"
TempTable tt2 [LIKE tt] @"tt2"
"#,
    );
}

#[test]
fn buffers_datasets_sources_and_streams() {
    let src = r#"DEFINE BUFFER bCust FOR Customer.
DEFINE BUFFER bTt FOR TEMP-TABLE tt.
DEFINE DATASET ds FOR tt, tt2.
DEFINE DATA-SOURCE src FOR Customer.
DEFINE STREAM sOut.
"#;
    assert_tree(
        src,
        r#"
Buffer bCust [FOR Customer] @"bCust"
Buffer bTt [FOR TEMP-TABLE tt] @"bTt"
Dataset ds [FOR tt, tt2] @"ds"
DataSource src @"src"
Stream sOut @"sOut"
"#,
    );
}

#[test]
fn query_frame_and_widgets() {
    let src = r#"DEFINE QUERY q FOR Customer.
DEFINE FRAME f1 x WITH 1 DOWN.
DEFINE BUTTON btnOk LABEL "OK".
DEFINE RECTANGLE rect1 SIZE 10 BY 2.
DEFINE IMAGE img1 FILE "a.bmp".
DEFINE BROWSE br1 QUERY q DISPLAY Customer.Name WITH 5 DOWN.
DEFINE MENU mb1 MENUBAR MENU-ITEM a LABEL "A".
DEFINE SUB-MENU sm1 MENU-ITEM b LABEL "B".
"#;
    assert_tree(
        src,
        r#"
Query q @"q"
Frame f1 @"f1"
Widget btnOk @"btnOk"
Widget rect1 @"rect1"
Widget img1 @"img1"
Widget br1 @"br1"
Widget mb1 @"mb1"
Widget sm1 @"sm1"
"#,
    );
}

#[test]
fn on_block_is_a_trigger_and_on_without_a_block_is_not() {
    let src = r#"ON CHOOSE OF btnOk DO:
    MESSAGE "x".
END.
ON F1 HELP.
ON CHOOSE   OF
   btnCancel DO:
    DEFINE VARIABLE inner AS INTEGER NO-UNDO.
END.
"#;
    assert_tree(
        src,
        "
Trigger CHOOSE OF btnOk @\"CHOOSE OF btnOk\"
Trigger CHOOSE OF btnCancel @\"CHOOSE   OF\\n   btnCancel\"
  Variable inner [INTEGER] @\"inner\"
",
    );
}

#[test]
fn long_trigger_name_is_cut_at_80_characters() {
    let events: Vec<String> = (0..12).map(|i| format!("CHOOSE OF button{i}")).collect();
    let src = format!("ON {} DO:\nEND.\n", events.join(" OR "));
    let symbols = run(&src);
    assert_eq!(symbols.len(), 1, "{symbols:?}");
    assert_eq!(symbols[0].kind, OutlineKind::Trigger);
    let count = symbols[0].name.chars().count();
    assert!(
        (1..=80).contains(&count),
        "name has {count} characters: {}",
        symbols[0].name
    );
    assert!(symbols[0].name.starts_with("CHOOSE OF button0"));
}

#[test]
fn trigger_procedure_header() {
    let src = "TRIGGER PROCEDURE FOR WRITE OF Customer.\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n";
    assert_tree(
        src,
        r#"
Trigger WRITE OF Customer [TRIGGER PROCEDURE] @"WRITE OF Customer"
Variable x [INTEGER] @"x"
"#,
    );
}

#[test]
fn statement_level_includes() {
    let src = "{file.i}\n{inc/other.i &a=1 &b=2}\nPROCEDURE p:\n    {nested.i}\nEND PROCEDURE.\n";
    assert_tree(
        src,
        r#"
Include file.i @"file.i"
Include inc/other.i @"inc/other.i"
Procedure p @"p"
  Include nested.i @"nested.i"
"#,
    );
}

#[test]
fn preprocessor_defines() {
    let src = "&SCOPED-DEFINE foo 1\n&GLOBAL-DEFINE bar \"x\"\n&scoped-define low 2\n";
    assert_tree(
        src,
        r#"
PreprocessorDefine foo [SCOPED] @"foo"
PreprocessorDefine bar [GLOBAL] @"bar"
PreprocessorDefine low [SCOPED] @"low"
"#,
    );
}

#[test]
fn labelled_blocks() {
    let src = r#"main-loop: DO:
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
END.
PROCEDURE p:
    blk: REPEAT:
        DEFINE VARIABLE b AS INTEGER NO-UNDO.
        LEAVE blk.
    END.
    lp: FOR EACH Customer:
    END.
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Label main-loop @"main-loop"
  Variable a [INTEGER] @"a"
Procedure p @"p"
  Label blk @"blk"
    Variable b [INTEGER] @"b"
  Label lp @"lp"
"#,
    );
}

#[test]
fn unlabelled_blocks_are_not_symbols() {
    let src = "DO:\nEND.\nREPEAT:\n  LEAVE.\nEND.\nFOR EACH Customer:\nEND.\n";
    assert_eq!(render(src), "");
}

// ----------------------------------------------------- hierarchy and lifting

#[test]
fn declarations_in_blocks_lift_to_the_top_level() {
    let src = r#"DO:
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
END.
REPEAT:
    DEFINE VARIABLE b AS INTEGER NO-UNDO.
    LEAVE.
END.
FOR EACH Customer:
    DEFINE VARIABLE c AS INTEGER NO-UNDO.
END.
"#;
    assert_tree(
        src,
        r#"
Variable a [INTEGER] @"a"
Variable b [INTEGER] @"b"
Variable c [INTEGER] @"c"
"#,
    );
}

#[test]
fn declarations_lift_to_the_nearest_symbol() {
    let src = r#"PROCEDURE p:
    DO:
        REPEAT:
            FOR EACH Customer:
                DEFINE VARIABLE deep AS INTEGER NO-UNDO.
            END.
        END.
    END.
    IF TRUE THEN DO:
        DEFINE VARIABLE yes AS INTEGER NO-UNDO.
    END.
    ELSE DO:
        DEFINE VARIABLE other AS INTEGER NO-UNDO.
    END.
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Procedure p @"p"
  Variable deep [INTEGER] @"deep"
  Variable yes [INTEGER] @"yes"
  Variable other [INTEGER] @"other"
"#,
    );
}

#[test]
fn else_if_chain_branches_are_walked() {
    let src = r#"DEFINE VARIABLE x AS INTEGER NO-UNDO.
IF x = 1 THEN DO:
    DEFINE VARIABLE one AS INTEGER NO-UNDO.
END.
ELSE IF x = 2 THEN DO:
    DEFINE VARIABLE two AS INTEGER NO-UNDO.
END.
ELSE DO:
    DEFINE VARIABLE other AS INTEGER NO-UNDO.
END.
"#;
    assert_tree(
        src,
        r#"
Variable x [INTEGER] @"x"
Variable one [INTEGER] @"one"
Variable two [INTEGER] @"two"
Variable other [INTEGER] @"other"
"#,
    );
}

#[test]
fn every_case_branch_is_walked() {
    let src = r#"PROCEDURE p:
    DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.
    CASE n:
        WHEN 1 THEN DO:
            DEFINE VARIABLE v1 AS INTEGER NO-UNDO.
        END.
        WHEN 2 THEN DO:
            DEFINE VARIABLE v2 AS INTEGER NO-UNDO.
        END.
        OTHERWISE DO:
            DEFINE VARIABLE v3 AS INTEGER NO-UNDO.
        END.
    END CASE.
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Procedure p @"p"
  Parameter n [INPUT INTEGER] @"n"
  Variable v1 [INTEGER] @"v1"
  Variable v2 [INTEGER] @"v2"
  Variable v3 [INTEGER] @"v3"
"#,
    );
}

#[test]
fn catch_and_finally_are_walked() {
    let src = r#"PROCEDURE p:
    DO ON ERROR UNDO, THROW:
        DEFINE VARIABLE body AS INTEGER NO-UNDO.
        CATCH e AS Progress.Lang.Error:
            DEFINE VARIABLE caught AS INTEGER NO-UNDO.
        END CATCH.
        FINALLY:
            DEFINE VARIABLE last AS INTEGER NO-UNDO.
        END FINALLY.
    END.
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Procedure p @"p"
  Variable body [INTEGER] @"body"
  Variable caught [INTEGER] @"caught"
  Variable last [INTEGER] @"last"
"#,
    );
}

#[test]
fn both_preprocessor_branches_show() {
    let src = r#"&IF DEFINED(flag) &THEN
DEFINE VARIABLE a AS INTEGER NO-UNDO.
&ELSE
DEFINE VARIABLE b AS INTEGER NO-UNDO.
&ENDIF
PROCEDURE p:
&IF DEFINED(flag) &THEN
    DEFINE VARIABLE c AS INTEGER NO-UNDO.
&ELSE
    DEFINE VARIABLE d AS INTEGER NO-UNDO.
&ENDIF
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Variable a [INTEGER] @"a"
Variable b [INTEGER] @"b"
Procedure p @"p"
  Variable c [INTEGER] @"c"
  Variable d [INTEGER] @"d"
"#,
    );
}

#[test]
fn trigger_block_contains_its_declarations() {
    let src = r#"PROCEDURE p:
    ON CHOOSE OF btn DO:
        DEFINE VARIABLE v AS INTEGER NO-UNDO.
    END.
    DEFINE VARIABLE after AS INTEGER NO-UNDO.
END PROCEDURE.
"#;
    assert_tree(
        src,
        r#"
Procedure p @"p"
  Trigger CHOOSE OF btn @"CHOOSE OF btn"
    Variable v [INTEGER] @"v"
  Variable after [INTEGER] @"after"
"#,
    );
}

#[test]
fn members_inside_blocks_of_a_method_lift_to_the_method() {
    let src = r#"CLASS C:
    METHOD PUBLIC VOID M(INPUT a AS INTEGER):
        DO:
            DEFINE VARIABLE v AS INTEGER NO-UNDO.
        END.
    END METHOD.
END CLASS.
"#;
    assert_tree(
        src,
        r#"
Class C @"C"
  Method M [PUBLIC VOID] @"M"
    Parameter a [INPUT INTEGER] @"a"
    Variable v [INTEGER] @"v"
"#,
    );
}

// ------------------------------------------------------------------- spans

#[test]
fn spans_cover_the_whole_declaration() {
    let src = r#"CLASS C:
    METHOD PUBLIC VOID M():
    END METHOD.
    CONSTRUCTOR PUBLIC C():
    END CONSTRUCTOR.
END CLASS.
PROCEDURE p:
    DEFINE VARIABLE v AS INTEGER NO-UNDO.
END PROCEDURE.
FUNCTION f RETURNS INTEGER ():
    RETURN 1.
END FUNCTION.
DEFINE TEMP-TABLE tt FIELD a AS INTEGER.
main: DO:
END.
ON CHOOSE OF b DO:
END.
"#;
    assert_spans(
        src,
        r#"
Class C => "CLASS C: METHOD PUBLIC VOID M(): END METHOD. CONSTRUCTOR PUBLIC C(): END CONSTRUCTOR. END CLASS."
  Method M => "METHOD PUBLIC VOID M(): END METHOD."
  Constructor C => "CONSTRUCTOR PUBLIC C(): END CONSTRUCTOR."
Procedure p => "PROCEDURE p: DEFINE VARIABLE v AS INTEGER NO-UNDO. END PROCEDURE."
  Variable v => "DEFINE VARIABLE v AS INTEGER NO-UNDO."
Function f => "FUNCTION f RETURNS INTEGER (): RETURN 1. END FUNCTION."
TempTable tt => "DEFINE TEMP-TABLE tt FIELD a AS INTEGER."
  Field a => "FIELD a AS INTEGER"
Label main => "main: DO: END."
Trigger CHOOSE OF b => "ON CHOOSE OF b DO: END."
"#,
    );
}

#[test]
fn var_names_span_name_and_initial_value() {
    assert_spans(
        "VAR INTEGER a, b = 5, c.\n",
        r#"
Variable a => "a"
Variable b => "b = 5"
Variable c => "c"
"#,
    );
}

#[test]
fn selection_span_is_the_name_text() {
    let src = "PROCEDURE  spaced :\nEND PROCEDURE.\nDEFINE   VARIABLE   v   AS INTEGER NO-UNDO.\n\
        CLASS A.B.C:\nEND CLASS.\n";
    let symbols = run(src);
    let mut all = Vec::new();
    flatten(&symbols, &mut all);
    assert_eq!(all.len(), 3, "{all:?}");
    for sym in all {
        assert_eq!(slice(src, sym.selection_span), sym.name, "{:?}", sym.kind);
    }
}

#[test]
fn constructor_and_destructor_select_their_first_keyword() {
    let src = "CLASS A.B.Cust:\n  CONSTRUCTOR PUBLIC Cust():\n  END CONSTRUCTOR.\n  DESTRUCTOR PUBLIC Cust():\n  END DESTRUCTOR.\nEND CLASS.\n";
    let symbols = run(src);
    let members = &symbols[0].children;
    assert_eq!(members.len(), 2, "{members:?}");
    assert_eq!(slice(src, members[0].selection_span), "CONSTRUCTOR");
    assert_eq!(slice(src, members[1].selection_span), "DESTRUCTOR");
    assert_eq!(names(members), ["Cust", "Cust"]);
}

#[test]
fn constructor_outside_a_class_has_the_keyword_as_name() {
    let src = "CONSTRUCTOR PUBLIC Orphan():\nEND CONSTRUCTOR.\nDESTRUCTOR PUBLIC Orphan():\nEND DESTRUCTOR.\n";
    let symbols = run(src);
    let mut all = Vec::new();
    flatten(&symbols, &mut all);
    let found: Vec<(OutlineKind, &str)> = all.iter().map(|s| (s.kind, s.name.as_str())).collect();
    assert_eq!(
        found,
        [
            (OutlineKind::Constructor, "CONSTRUCTOR"),
            (OutlineKind::Destructor, "DESTRUCTOR")
        ],
        "{symbols:?}"
    );
}

#[test]
fn spans_after_multibyte_text_stay_on_boundaries() {
    let src = "/* é — 😀 */\nMESSAGE \"ünï😀\".\nPROCEDURE after:\n    DEFINE VARIABLE v AS INTEGER NO-UNDO.\nEND PROCEDURE.\n";
    assert_tree(
        src,
        r#"
Procedure after @"after"
  Variable v [INTEGER] @"v"
"#,
    );
    let symbols = run(src);
    assert_eq!(
        collapse(slice(src, symbols[0].span)),
        "PROCEDURE after: DEFINE VARIABLE v AS INTEGER NO-UNDO. END PROCEDURE."
    );
}

#[test]
fn multibyte_text_around_names_on_one_line() {
    let src = "MESSAGE \"é😀\". DEFINE VARIABLE x AS INTEGER NO-UNDO. /* ü */ DEFINE VARIABLE y AS INTEGER NO-UNDO. // 😀é\nPROCEDURE p: /* ñ */ DEFINE VARIABLE z AS INTEGER NO-UNDO. END PROCEDURE.\n";
    assert_tree(
        src,
        r#"
Variable x [INTEGER] @"x"
Variable y [INTEGER] @"y"
Procedure p @"p"
  Variable z [INTEGER] @"z"
"#,
    );
}

#[test]
fn comma_lists_in_on_phrases_are_one_trigger() {
    assert_tree(
        "ON CHOOSE OF b1, b2 DO:\nEND.\n",
        r#"
Trigger CHOOSE OF b1, b2 @"CHOOSE OF b1, b2"
"#,
    );
    assert_tree(
        "ON LEAVE, RETURN OF f1 DO:\nEND.\n",
        r#"
Trigger LEAVE, RETURN OF f1 @"LEAVE, RETURN OF f1"
"#,
    );
}

// ---------------------------------------------------------------- recovery

#[test]
fn empty_and_comment_only_files_have_no_symbols() {
    for src in [
        "",
        "\n\n",
        "   \t\n",
        "/* only a comment */\n",
        "// line\n/* é */\n",
    ] {
        let result = outline(src).expect("no panic");
        assert_eq!(result, Vec::new(), "{src:?}");
    }
}

#[test]
fn a_syntax_error_keeps_the_other_procedures() {
    let src = r#"PROCEDURE first:
    DEFINE VARIABLE x AS INTEGER NO-UNDO.
END PROCEDURE.

PROCEDURE broken:
    DEFINE VARIABLE = = .
    MESSAGE ( ) ) .
    IF THEN THEN.
END PROCEDURE.

PROCEDURE last:
    DEFINE VARIABLE y AS INTEGER NO-UNDO.
END PROCEDURE.
"#;
    let symbols = run(src);
    let top: Vec<&str> = names(&symbols);
    assert!(top.contains(&"first"), "{top:?}");
    assert!(top.contains(&"last"), "{top:?}");
    let first = symbols.iter().find(|s| s.name == "first").unwrap();
    assert_eq!(names(&first.children), ["x"]);
    let last = symbols.iter().find(|s| s.name == "last").unwrap();
    assert_eq!(names(&last.children), ["y"]);
}

#[test]
fn an_unterminated_procedure_still_yields_what_came_before() {
    let src = "PROCEDURE ok:\nEND PROCEDURE.\nPROCEDURE open:\n    DEFINE VARIABLE v AS INTEGER NO-UNDO.\n";
    let symbols = run(src);
    assert!(names(&symbols).contains(&"ok"), "{symbols:?}");
}

// ------------------------------------------------------------------- rules

fn symbol(name: &str, span: (u32, u32), sel: (u32, u32)) -> OutlineSymbol {
    OutlineSymbol {
        name: name.to_string(),
        detail: None,
        kind: OutlineKind::Procedure,
        span: Span {
            start: span.0,
            end: span.1,
        },
        selection_span: Span {
            start: sel.0,
            end: sel.1,
        },
        children: Vec::new(),
    }
}

#[track_caller]
fn assert_violation(source: &str, symbols: &[OutlineSymbol], needle: &str) {
    let violations = rule_violations(source, symbols);
    assert!(
        violations.iter().any(|v| v.contains(needle)),
        "wanted a violation with {needle:?}, got {violations:?}"
    );
}

#[test]
fn the_rules_checker_can_fail() {
    let src = "PROCEDURE p:\nEND PROCEDURE.\n";
    let good = symbol("p", (0, 26), (10, 11));
    assert_eq!(
        rule_violations(src, std::slice::from_ref(&good)),
        Vec::<String>::new()
    );

    let mut child_outside = good.clone();
    child_outside.children.push(symbol("c", (20, 40), (22, 23)));
    assert_violation(src, &[child_outside], "outside parent");

    assert_violation(src, &[symbol("", (0, 26), (10, 11))], "empty name");
    assert_violation(src, &[symbol("a\nb", (0, 26), (10, 11))], "line break");
    assert_violation(src, &[symbol(" p", (0, 26), (10, 11))], "whitespace");
    assert_violation(src, &[symbol("p", (0, 26), (27, 28))], "not inside span");
    assert_violation(src, &[symbol("p", (0, 26), (10, 10))], "not inside span");
    assert_violation(src, &[symbol("p", (0, 99), (10, 11))], "past the end");
    assert_violation(
        src,
        &[
            symbol("p", (0, 12), (10, 11)),
            symbol("q", (11, 20), (12, 13)),
        ],
        "overlaps",
    );

    let multibyte = "é";
    assert_violation(multibyte, &[symbol("x", (0, 2), (1, 2))], "char boundary");
}

#[test]
fn rules_hold_for_every_test_fixture_file() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = Vec::new();
    for dir in fixture_roots(&root) {
        collect(&dir, &mut files);
    }
    assert!(!files.is_empty(), "no fixture files found under {root:?}");
    for file in &files {
        let bytes = std::fs::read(file).unwrap_or_else(|e| panic!("{}: {e}", file.display()));
        let source = String::from_utf8_lossy(&bytes);
        let first = outline(&source).unwrap_or_else(|e| panic!("{}: {e}", file.display()));
        let second = outline(&source).unwrap();
        assert_eq!(first, second, "{}: not deterministic", file.display());
        let violations = rule_violations(&source, &first);
        assert!(
            violations.is_empty(),
            "{}:\n{}",
            file.display(),
            violations.join("\n")
        );
    }
    eprintln!("outline rules checked on {} files", files.len());
}

fn fixture_roots(root: &Path) -> Vec<PathBuf> {
    let mut roots = vec![root.join("fuzz/findings")];
    let crates = std::fs::read_dir(root.join("crates")).expect("crates dir");
    for entry in crates {
        roots.push(entry.unwrap().path().join("tests/fixtures"));
    }
    roots
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut entries: Vec<_> = entries.map(|e| e.unwrap().path()).collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            collect(&path, out);
        } else if let Some(ext) = path.extension().and_then(|e| e.to_str())
            && ["p", "cls", "i", "w"].contains(&ext.to_ascii_lowercase().as_str())
        {
            out.push(path);
        }
    }
}

const NESTING: usize = 250;

/// The parser recurses and the harness provides its stack: every parsing
/// thread has 64 MiB, the nesting limit is 256 levels.
fn on_big_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(f)
        .expect("spawn")
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
}

#[test]
fn deeply_nested_blocks_do_not_overflow() {
    on_big_stack(|| {
        let mut src = String::new();
        for _ in 0..NESTING {
            src.push_str("DO:\n");
        }
        src.push_str("DEFINE VARIABLE bottom AS INTEGER NO-UNDO.\n");
        for _ in 0..NESTING {
            src.push_str("END.\n");
        }
        let symbols = run(&src);
        assert_eq!(render(&src), "Variable bottom [INTEGER] @\"bottom\"");
        assert_eq!(symbols.len(), 1);
    });
}

#[test]
fn deeply_nested_blocks_in_a_procedure_lift_to_it() {
    on_big_stack(|| {
        let mut src = String::from("PROCEDURE p:\n");
        for _ in 0..NESTING {
            src.push_str("REPEAT:\n");
        }
        src.push_str("DEFINE VARIABLE bottom AS INTEGER NO-UNDO.\n");
        for _ in 0..NESTING {
            src.push_str("END.\n");
        }
        src.push_str("END PROCEDURE.\n");
        assert_tree(
            &src,
            r#"
Procedure p @"p"
  Variable bottom [INTEGER] @"bottom"
"#,
        );
    });
}

#[test]
fn long_else_if_chain_does_not_overflow() {
    on_big_stack(|| {
        let mut src = String::from("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
        for i in 0..NESTING {
            src.push_str(&format!("IF x = {i} THEN DO:\nEND.\nELSE "));
        }
        src.push_str("DO:\nDEFINE VARIABLE last AS INTEGER NO-UNDO.\nEND.\n");
        let symbols = run(&src);
        assert_eq!(names(&symbols), ["x", "last"]);
    });
}

#[test]
fn a_symbol_deep_in_labelled_blocks_is_not_lost() {
    on_big_stack(|| {
        let mut src = String::new();
        for i in 1..=NESTING {
            src.push_str(&format!("l{i}: DO:\n"));
        }
        src.push_str("DEFINE VARIABLE bottom AS INTEGER NO-UNDO.\n");
        for _ in 0..NESTING {
            src.push_str("END.\n");
        }
        let symbols = run(&src);
        let mut level = &symbols;
        for i in 1..=NESTING {
            assert_eq!(level.len(), 1, "level {i}");
            assert_eq!(level[0].kind, OutlineKind::Label, "level {i}");
            assert_eq!(level[0].name, format!("l{i}"));
            level = &level[0].children;
        }
        assert_eq!(level.len(), 1, "the bottom variable is lost");
        assert_eq!(level[0].kind, OutlineKind::Variable);
        assert_eq!(level[0].name, "bottom");
        assert!(level[0].children.is_empty());
    });
}

#[test]
fn many_siblings_stay_ordered() {
    let mut src = String::new();
    for i in 0..3000 {
        src.push_str(&format!("PROCEDURE p{i}:\nEND PROCEDURE.\n"));
    }
    let symbols = run(&src);
    assert_eq!(symbols.len(), 3000);
    assert_eq!(symbols[0].name, "p0");
    assert_eq!(symbols[2999].name, "p2999");
}

// ------------------------------------------------------------- panic guard

#[test]
fn a_contained_panic_is_an_error_not_an_empty_outline() {
    let plain = "PROCEDURE p:\nEND PROCEDURE.\n";
    let marked = format!("/* OXABL-TEST-PANIC:outline */\n{plain}");
    assert!(outline(&marked).is_err(), "the marker must give Err");
    let ok = outline(plain).expect("the same text without the marker must give Ok");
    check_rules(plain, &ok);
    assert!(!ok.is_empty(), "the outline of a procedure is not empty");
}

#[test]
fn a_marker_for_another_site_does_not_panic() {
    let src = "/* OXABL-TEST-PANIC:analyze */\nPROCEDURE p:\nEND PROCEDURE.\n";
    assert!(outline(src).is_ok());
}
