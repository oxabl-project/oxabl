//! Calls into classes that a file names: `NEW`, methods of a variable whose
//! class it declares, and static methods.

use oxabl_index::calls::{ObjectCall, object_calls};
use oxabl_parser::Parser;

fn calls(source: &str) -> Vec<ObjectCall> {
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    object_calls(&program.statements, source, &tokens)
}

fn shown(source: &str) -> Vec<(String, Option<String>, String)> {
    calls(source)
        .into_iter()
        .map(|c| {
            let at = source[c.at.start as usize..c.at.end as usize].to_string();
            (c.class, c.method, at)
        })
        .collect()
}

#[test]
fn new_typed_receivers_and_static_calls_name_their_class() {
    let src = "\
USING app.*.
DEFINE VARIABLE oSvc AS OrderService NO-UNDO.
DEFINE VARIABLE oRaw AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
oSvc = NEW OrderService(cName).
oSvc:Create(cName).
app.Audit:Write(\"created\").
oRaw = NEW app.util.Clock().
cName = oRaw:ToString().
";
    assert_eq!(
        shown(src),
        [
            ("OrderService".into(), None, "OrderService".into()),
            (
                "OrderService".into(),
                Some("Create".into()),
                "Create".into()
            ),
            ("app.Audit".into(), Some("Write".into()), "Write".into()),
            ("app.util.Clock".into(), None, "app.util.Clock".into()),
            (
                "Progress.Lang.Object".into(),
                Some("ToString".into()),
                "ToString".into()
            ),
        ]
    );
}

#[test]
fn a_parameter_or_property_type_types_its_receiver() {
    let src = "\
CLASS app.Handler:
    DEFINE PRIVATE PROPERTY Repo AS app.OrderRepo NO-UNDO GET. SET.
    METHOD PUBLIC VOID Handle(INPUT poSvc AS app.OrderService):
        poSvc:Create(\"x\").
        Repo:Save().
    END METHOD.
END CLASS.
";
    let found: Vec<_> = shown(src).into_iter().map(|(c, m, _)| (c, m)).collect();
    assert_eq!(
        found,
        [
            ("app.OrderService".into(), Some("Create".into())),
            ("app.OrderRepo".into(), Some("Save".into())),
        ]
    );
}

#[test]
fn handles_untyped_names_chains_and_dynamic_new_are_not_object_calls() {
    let src = "\
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE oAny AS Progress.Lang.Object NO-UNDO.
hQ:QUERY-PREPARE(\"FOR EACH Customer\").
oUnknown:Run().
oAny:GetClass():GetMethod(\"x\").
oAny = DYNAMIC-NEW \"app.X\" ().
SESSION:GET-PRINTERS().
";
    let found: Vec<_> = shown(src).into_iter().map(|(c, m, _)| (c, m)).collect();
    assert_eq!(
        found,
        [("Progress.Lang.Object".into(), Some("GetClass".into()))],
        "only the first link of a chain has a known receiver"
    );
}
