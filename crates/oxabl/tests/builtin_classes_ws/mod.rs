//! What `builtin_classes_cli.rs` and `builtin_classes_lsp.rs` share: the
//! configurations, the synthetic workspace classes and the programs whose
//! findings both clients must agree on, each row with the finding it gives or
//! the word "silent".
//!
//! Variable names are chosen on purpose: `oF` and `oN` lex as the keywords `OF`
//! and `ON`, so no variable has such a name.

use std::path::{Path, PathBuf};

use tempfile::TempDir;

/// One finding: the trimmed text of its line, its message and the text of its span.
pub type Finding = (String, String, String);

pub const CONFIG: &str = "[workspace]\nname = \"builtins\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

pub const CONFIG_STRICT: &str = "[workspace]\nname = \"builtins\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n\
    [workspace.lint]\nenable = [\"strict.calls\"]\n";

pub const CONFIG_NO_SOURCES: &str = "[workspace]\nname = \"builtins\"\n";

const THING: &str = r#"CLASS app.Thing:
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO GET. SET.
END CLASS.
"#;

const NAMED: &str = r#"CLASS app.Named:
    METHOD PUBLIC OVERRIDE CHARACTER ToString():
        RETURN "n".
    END METHOD.
END CLASS.
"#;

const ORDER_ERROR: &str = r#"CLASS app.OrderError INHERITS Progress.Lang.AppError:
    DEFINE PUBLIC PROPERTY OrderNum AS INTEGER NO-UNDO GET. SET.
    CONSTRUCTOR PUBLIC OrderError(INPUT pcMsg AS CHARACTER):
        SUPER(pcMsg, 0).
    END CONSTRUCTOR.
    METHOD PUBLIC CHARACTER Describe():
        RETURN "order " + STRING(OrderNum).
    END METHOD.
END CLASS.
"#;

const ORPHAN_ERROR: &str = r#"CLASS app.OrphanError INHERITS app.Missing:
    METHOD PUBLIC CHARACTER Describe():
        RETURN "orphan".
    END METHOD.
END CLASS.
"#;

const ERROR_USER: &str = r#"CLASS app.ErrorUser INHERITS Progress.Lang.AppError:
    METHOD PUBLIC VOID Check():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.
        l = THIS-OBJECT:GetMessage(1).
        c = THIS-OBJECT:GetMessage(1).
        l = SUPER:GetMessage(1).
        l = GetMessage(1).
        l = NumMessages.
        MESSAGE NumMessagez.
        MESSAGE l c.
    END METHOD.
END CLASS.
"#;

const ISHAPE: &str = r#"INTERFACE app.IShape:
    METHOD PUBLIC DECIMAL Area().
END INTERFACE.
"#;

const FILES: &[(&str, &str)] = &[
    ("app/Thing.cls", THING),
    ("app/Named.cls", NAMED),
    ("app/OrderError.cls", ORDER_ERROR),
    ("app/OrphanError.cls", ORPHAN_ERROR),
    ("app/ErrorUser.cls", ERROR_USER),
    ("app/IShape.cls", ISHAPE),
];

/// A temporary workspace with the files every case has.
pub struct Workspace {
    _dir: TempDir,
    pub root: PathBuf,
}

impl Workspace {
    /// The workspace with `config` as its `oxabl.toml` (none when `None`).
    pub fn new(config: Option<&str>) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let ws = Workspace { _dir: dir, root };
        for (name, text) in FILES {
            ws.write(name, text);
        }
        if let Some(config) = config {
            ws.write("oxabl.toml", config);
        }
        ws
    }

    pub fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }

    /// Write or replace `name` below the root.
    pub fn write(&self, name: &str, text: &str) {
        let path: &Path = &self.path(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
}

// ---------------------------------------------------------------------------
// Messages
// ---------------------------------------------------------------------------

pub fn mismatch(from: &str, to: &str) -> String {
    format!("type mismatch: cannot assign `{from}` to `{to}`")
}

pub fn narrowing(from: &str, to: &str) -> String {
    format!("narrowing conversion: `{from}` assigned to `{to}` may discard data")
}

fn arguments(n: usize) -> String {
    match n {
        1 => "1 argument".to_string(),
        n => format!("{n} arguments"),
    }
}

pub fn no_overload_count(class: &str, member: &str, n: usize) -> String {
    format!("no overload of `{class}:{member}` takes {}", arguments(n))
}

pub fn no_overload_types(class: &str, member: &str, types: &[&str]) -> String {
    let types: Vec<String> = types.iter().map(|t| format!("`{t}`")).collect();
    format!(
        "no overload of `{class}:{member}` takes arguments of these types: {}",
        types.join(", ")
    )
}

pub fn no_constructor(class: &str, n: usize) -> String {
    format!("no constructor of `{class}` takes {}", arguments(n))
}

pub fn no_member(class: &str, name: &str) -> String {
    format!("built-in type `{class}` has no member `{name}`")
}

// ---------------------------------------------------------------------------
// Rows
// ---------------------------------------------------------------------------

/// One line of a program and what `LINT0004` says about it.
pub struct Row {
    pub line: &'static str,
    /// `(message, span text)`; none for a silent row.
    pub finding: Option<(String, &'static str)>,
    /// The finding needs the workspace classes: it is silent with no search path.
    pub needs_sources: bool,
}

pub fn silent(line: &'static str) -> Row {
    Row {
        line,
        finding: None,
        needs_sources: false,
    }
}

pub fn mismatch_row(line: &'static str, from: &str, to: &str, span: &'static str) -> Row {
    Row {
        line,
        finding: Some((mismatch(from, to), span)),
        needs_sources: false,
    }
}

pub fn narrowing_row(line: &'static str, from: &str, to: &str, span: &'static str) -> Row {
    Row {
        line,
        finding: Some((narrowing(from, to), span)),
        needs_sources: false,
    }
}

impl Row {
    pub fn sources(mut self) -> Row {
        self.needs_sources = true;
        self
    }
}

/// The findings of `rows` in order. A row that needs the workspace classes is
/// left out when `sources` is false.
pub fn lint0004_of(rows: &[Row], sources: bool) -> Vec<Finding> {
    rows.iter()
        .filter(|row| sources || !row.needs_sources)
        .filter_map(|row| {
            row.finding
                .as_ref()
                .map(|(message, span)| (row.line.to_string(), message.clone(), span.to_string()))
        })
        .collect()
}

pub fn finding(line: &str, message: String, span: &str) -> Finding {
    (line.to_string(), message, span.to_string())
}

// ---------------------------------------------------------------------------
// B1. types.p
// ---------------------------------------------------------------------------

const TYPES_DECLS: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
    DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.\n\
    DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE oThing AS app.Thing NO-UNDO.\n\
    DEFINE VARIABLE oNamed AS app.Named NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE i64 AS INT64 NO-UNDO.\n\
    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n\
    DEFINE VARIABLE cArr AS CHARACTER EXTENT 2 NO-UNDO.\n";

const TYPES_USE: &str = "MESSAGE oErr oJson oStr oObj oThing oNamed l i i64 c lc cArr[1].\n";

pub fn types_rows() -> Vec<Row> {
    let narrow = narrowing_row;
    let bad = mismatch_row;
    vec![
        bad("l = oErr:GetMessage(1).", "CHARACTER", "LOGICAL", "l"),
        silent("c = oErr:GetMessage(1)."),
        bad("c = oErr:NumMessages.", "INTEGER", "CHARACTER", "c"),
        silent("i = oErr:NumMessages."),
        bad("l = oErr:ReturnValue.", "CHARACTER", "LOGICAL", "l"),
        bad("l = oErr:CallStack.", "CHARACTER", "LOGICAL", "l"),
        silent("i = oErr:Severity."),
        bad(
            "l = oJson:GetCharacter(\"a\").",
            "CHARACTER",
            "LOGICAL",
            "l",
        ),
        bad("i = oJson:Has(\"a\").", "LOGICAL", "INTEGER", "i"),
        narrow(
            "c = oJson:GetLongchar(\"a\").",
            "LONGCHAR",
            "CHARACTER",
            "c",
        ),
        silent("lc = oJson:GetLongchar(\"a\", \"UTF-8\")."),
        bad("i = oJson:Write(lc, TRUE).", "LOGICAL", "INTEGER", "i"),
        narrow("c = oStr:Value.", "LONGCHAR", "CHARACTER", "c"),
        silent("lc = oStr:Value."),
        bad("l = oStr:CompareTo(oStr).", "INTEGER", "LOGICAL", "l"),
        narrow(
            "c = OpenEdge.Core.String:Join(cArr, \",\").",
            "LONGCHAR",
            "CHARACTER",
            "c",
        ),
        bad(
            "oStr = OpenEdge.Core.String:Join(cArr, \",\").",
            "LONGCHAR",
            "OpenEdge.Core.String",
            "oStr",
        ),
        bad(
            "i = OpenEdge.Core.String:IsQuoted(lc).",
            "LOGICAL",
            "INTEGER",
            "i",
        ),
        bad(
            "c = Progress.Json.ObjectModel.JsonDataType:NUMBER.",
            "INTEGER",
            "CHARACTER",
            "c",
        ),
        bad("l = oObj:ToString().", "CHARACTER", "LOGICAL", "l"),
        bad("l = oObj:GetClass():TypeName.", "CHARACTER", "LOGICAL", "l"),
        bad("i = oObj:Equals(oObj).", "LOGICAL", "INTEGER", "i"),
        bad("l = oThing:ToString().", "CHARACTER", "LOGICAL", "l").sources(),
        bad("l = oNamed:ToString().", "CHARACTER", "LOGICAL", "l").sources(),
        bad(
            "l = oThing:GetClass():TypeName.",
            "CHARACTER",
            "LOGICAL",
            "l",
        )
        .sources(),
        silent("c = oThing:ToString()."),
        bad(
            "i = Progress.ApplicationServer.AdapterTypes:SOAP.",
            "Progress.ApplicationServer.AdapterTypes",
            "INTEGER",
            "i",
        ),
        silent("i64 = Progress.ApplicationServer.AdapterTypes:SOAP:GetValue()."),
        bad(
            "l = Progress.ApplicationServer.AdapterTypes:SOAP:GetValue().",
            "INT64",
            "LOGICAL",
            "l",
        ),
        bad(
            "i = Progress.IO.ByteOrder:BigEndian.",
            "Progress.IO.ByteOrder",
            "INTEGER",
            "i",
        ),
        bad(
            "i = NEW Progress.Lang.AppError().",
            "Progress.Lang.AppError",
            "INTEGER",
            "i",
        ),
        silent("oObj = NEW Progress.Lang.AppError()."),
        bad(
            "oJson = NEW OpenEdge.Core.String().",
            "OpenEdge.Core.String",
            "Progress.Json.ObjectModel.JsonObject",
            "oJson",
        ),
        silent("oObj = oThing."),
        silent("oObj = NEW app.Thing()."),
    ]
}

pub fn program(decls: &str, rows: &[Row], tail: &str) -> String {
    let mut text = decls.to_string();
    for row in rows {
        text.push_str(row.line);
        text.push('\n');
    }
    text.push_str(tail);
    text
}

pub fn types_program() -> String {
    program(TYPES_DECLS, &types_rows(), TYPES_USE)
}

// ---------------------------------------------------------------------------
// B2. errors.p
// ---------------------------------------------------------------------------

const ERRORS_DECLS: &str = "DEFINE VARIABLE oOE AS app.OrderError NO-UNDO.\n\
    DEFINE VARIABLE oPE AS Progress.Lang.ProError NO-UNDO.\n\
    DEFINE VARIABLE oEI AS Progress.Lang.Error NO-UNDO.\n\
    DEFINE VARIABLE oSys AS Progress.Lang.SysError NO-UNDO.\n\
    DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n";

const ERRORS_TAIL: &str = "DO ON ERROR UNDO, LEAVE:\n    i = 1.\n    CATCH e AS Progress.Lang.Error:\n        \
    l = e:GetMessage(1).\n        i = e:GetMessageNum(1).\n        l = e:ToString().\n    \
    END CATCH.\nEND.\nMESSAGE oOE oPE oEI oSys oErr oObj oJson l i.\n";

pub fn errors_rows() -> Vec<Row> {
    let bad = mismatch_row;
    vec![
        bad("l = oOE:GetMessage(1).", "CHARACTER", "LOGICAL", "l"),
        bad("l = oOE:Describe().", "CHARACTER", "LOGICAL", "l"),
        bad("l = oOE:OrderNum.", "INTEGER", "LOGICAL", "l"),
        bad("l = oOE:NumMessages.", "INTEGER", "LOGICAL", "l"),
        bad("l = oOE:ToString().", "CHARACTER", "LOGICAL", "l"),
        silent("oEI = oOE."),
        silent("oPE = oOE."),
        silent("oErr = oOE."),
        silent("oObj = oOE."),
        bad(
            "oSys = oOE.",
            "app.ordererror",
            "Progress.Lang.SysError",
            "oSys",
        ),
        bad(
            "oJson = oOE.",
            "app.ordererror",
            "Progress.Json.ObjectModel.JsonObject",
            "oJson",
        ),
        bad(
            "oOE = NEW Progress.Lang.AppError().",
            "Progress.Lang.AppError",
            "app.ordererror",
            "oOE",
        ),
        bad(
            "oErr = oSys.",
            "Progress.Lang.SysError",
            "Progress.Lang.AppError",
            "oErr",
        ),
        silent("oEI = oSys."),
        silent("oPE = NEW Progress.Lang.AppError()."),
    ]
}

/// The `CATCH` rows of `errors.p`, after the others.
pub fn errors_catch_rows() -> Vec<Row> {
    vec![
        mismatch_row("l = e:GetMessage(1).", "CHARACTER", "LOGICAL", "l"),
        silent("i = e:GetMessageNum(1)."),
        mismatch_row("l = e:ToString().", "CHARACTER", "LOGICAL", "l"),
    ]
}

pub fn errors_program() -> String {
    program(ERRORS_DECLS, &errors_rows(), ERRORS_TAIL)
}

pub fn errors_lint0004() -> Vec<Finding> {
    let mut all = lint0004_of(&errors_rows(), true);
    all.extend(lint0004_of(&errors_catch_rows(), true));
    all
}

// ---------------------------------------------------------------------------
// C1. closed.p
// ---------------------------------------------------------------------------

pub const CLOSED: &str = "DEFINE VARIABLE oList AS Progress.Collections.List<OpenEdge.Core.String> NO-UNDO.\n\
    DEFINE VARIABLE oCls AS Progress.Lang.Class NO-UNDO.\n\
    DEFINE VARIABLE oForm AS Progress.Windows.Form NO-UNDO.\n\
    DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.\n\
    DEFINE VARIABLE oOrphan AS app.OrphanError NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n\
    l = oList:Count.\n\
    i = oList:Get(1).\n\
    i = oCls:IsInterface().\n\
    l = oForm:ToString().\n\
    i = OpenEdge.Core.String:IsNullOrEmpty(lc).\n\
    i = oStr:Split(\",\").\n\
    i = OpenEdge.Net.UriSchemeEnum:http.\n\
    l = oOrphan:Describe().\n\
    l = oStr:Trim().\n\
    MESSAGE oList oCls oForm oStr oOrphan l i lc.\n";

// ---------------------------------------------------------------------------
// C3. sure.p
// ---------------------------------------------------------------------------

pub const SURE: &str = "USING Progress.Lang.*.\n\
    DEFINE VARIABLE oE AS AppError NO-UNDO.\n\
    DEFINE VARIABLE oQ AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    l = oE:GetMessage(1).\n\
    l = oQ:GetMessage(1).\n\
    MESSAGE oE oQ l.\n";

pub fn sure_lint0004() -> Vec<Finding> {
    vec![
        finding(
            "l = oE:GetMessage(1).",
            mismatch("CHARACTER", "LOGICAL"),
            "l",
        ),
        finding(
            "l = oQ:GetMessage(1).",
            mismatch("CHARACTER", "LOGICAL"),
            "l",
        ),
    ]
}

// ---------------------------------------------------------------------------
// D1. calls.p
// ---------------------------------------------------------------------------

const CALLS_DECLS: &str = "DEFINE VARIABLE oErr AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.\n\
    DEFINE VARIABLE oStr AS OpenEdge.Core.String NO-UNDO.\n\
    DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE oCls AS Progress.Lang.Class NO-UNDO.\n\
    DEFINE VARIABLE oList AS Progress.Collections.List<OpenEdge.Core.String> NO-UNDO.\n";

const CALLS_DECL_OE: &str = "DEFINE VARIABLE oOE AS app.OrderError NO-UNDO.\n";

const CALLS_DECLS_END: &str = "DEFINE VARIABLE i64 AS INT64 NO-UNDO.\n\
    DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.\n";

const CALLS_TAIL: &str = "DO ON ERROR UNDO, LEAVE:\n    lc = \"x\".\n    CATCH e AS Progress.Lang.Error:\n        \
    MESSAGE e:GetMessage().\n    END CATCH.\nEND.\n";

/// What a row of `calls.p` says: nothing, a `CL006` finding or a `CL007` finding.
pub enum Call {
    None,
    /// The message and the span text.
    Cl006(String, &'static str),
    Cl007(String, &'static str),
}

pub struct CallRow {
    pub line: &'static str,
    pub call: Call,
    /// The row uses `oOE` (a row of the workspace class).
    pub oe: bool,
}

fn quiet(line: &'static str) -> CallRow {
    CallRow {
        line,
        call: Call::None,
        oe: false,
    }
}

fn cl006(line: &'static str, message: String, span: &'static str) -> CallRow {
    CallRow {
        line,
        call: Call::Cl006(message, span),
        oe: false,
    }
}

fn cl007(line: &'static str, message: String, span: &'static str) -> CallRow {
    CallRow {
        line,
        call: Call::Cl007(message, span),
        oe: false,
    }
}

pub fn calls_rows() -> Vec<CallRow> {
    let app_error = "Progress.Lang.AppError";
    let json = "Progress.Json.ObjectModel.JsonObject";
    let oe = |mut row: CallRow| {
        row.oe = true;
        row
    };
    vec![
        cl006(
            "MESSAGE oErr:GetMessage(1, 2).",
            no_overload_count(app_error, "GetMessage", 2),
            "oErr:GetMessage(1, 2)",
        ),
        cl006(
            "MESSAGE oErr:GetMessage().",
            no_overload_count(app_error, "GetMessage", 0),
            "oErr:GetMessage()",
        ),
        quiet("MESSAGE oErr:GetMessage(1)."),
        cl006(
            "MESSAGE oErr:GetMessage(\"1\").",
            no_overload_types(app_error, "GetMessage", &["CHARACTER"]),
            "oErr:GetMessage(\"1\")",
        ),
        cl006(
            "MESSAGE oErr:GetMessage(oJson).",
            no_overload_types(app_error, "GetMessage", &[json]),
            "oErr:GetMessage(oJson)",
        ),
        quiet("MESSAGE oErr:GetMessage(i64)."),
        quiet("MESSAGE oErr:GetMessage(?)."),
        quiet("MESSAGE oErr:GetMessage(1.5)."),
        cl006(
            "oErr = NEW Progress.Lang.AppError(\"a\", 1, 2).",
            no_constructor(app_error, 3),
            "NEW Progress.Lang.AppError(\"a\", 1, 2)",
        ),
        quiet("oErr = NEW Progress.Lang.AppError()."),
        quiet("oErr = NEW Progress.Lang.AppError(\"a\")."),
        quiet("oErr = NEW Progress.Lang.AppError(\"a\", 1)."),
        quiet("oErr:AddMessage(\"x\", 1)."),
        cl006(
            "oErr:AddMessage(\"x\").",
            no_overload_count(app_error, "AddMessage", 1),
            "oErr:AddMessage(\"x\")",
        ),
        cl007(
            "MESSAGE oErr:GetMesage(1).",
            no_member(app_error, "GetMesage"),
            "GetMesage",
        ),
        cl007(
            "MESSAGE oErr:CallStak.",
            no_member(app_error, "CallStak"),
            "CallStak",
        ),
        quiet("MESSAGE oErr:CallStack."),
        cl007(
            "MESSAGE oObj:Lookup(\"x\").",
            no_member("Progress.Lang.Object", "Lookup"),
            "Lookup",
        ),
        cl007(
            "MESSAGE Progress.Json.ObjectModel.JsonDataType:Strings.",
            no_member("Progress.Json.ObjectModel.JsonDataType", "Strings"),
            "Strings",
        ),
        cl006(
            "MESSAGE OpenEdge.Core.String:IsNullOrEmpty(lc, 1).",
            no_overload_count("OpenEdge.Core.String", "IsNullOrEmpty", 2),
            "OpenEdge.Core.String:IsNullOrEmpty(lc, 1)",
        ),
        cl006(
            "MESSAGE oJson:GetCharacter(\"a\", \"b\", \"c\").",
            no_overload_count(json, "GetCharacter", 3),
            "oJson:GetCharacter(\"a\", \"b\", \"c\")",
        ),
        quiet("MESSAGE oJson:Add(\"k\", oStr)."),
        quiet("MESSAGE oErr:NumMessages()."),
        quiet("MESSAGE oCls:Invoke(oObj, \"x\")."),
        quiet("MESSAGE oCls:NoSuch()."),
        quiet("MESSAGE oList:NoSuch()."),
        oe(quiet("MESSAGE oOE:GetMesage(1).")),
        oe(quiet("MESSAGE oOE:GetMessage(1, 2).")),
        quiet("MESSAGE OpenEdge.Net.UriSchemeEnum:nosuch."),
        quiet("MESSAGE oErr:GetMessage({missing-arg.i})."),
    ]
}

/// `calls.p`, with the declaration and the rows of `oOE` when `with_oe`.
pub fn calls_program(with_oe: bool) -> String {
    let mut text = CALLS_DECLS.to_string();
    if with_oe {
        text.push_str(CALLS_DECL_OE);
    }
    text.push_str(CALLS_DECLS_END);
    for row in calls_rows().iter().filter(|row| with_oe || !row.oe) {
        text.push_str(row.line);
        text.push('\n');
    }
    text.push_str(CALLS_TAIL);
    text
}

/// The complete `CL006` and `CL007` lists of `calls.p`, in source order.
pub fn calls_findings(with_oe: bool) -> (Vec<Finding>, Vec<Finding>) {
    let (mut c6, mut c7) = (Vec::new(), Vec::new());
    for row in calls_rows().iter().filter(|row| with_oe || !row.oe) {
        match &row.call {
            Call::None => {}
            Call::Cl006(message, span) => c6.push(finding(row.line, message.clone(), span)),
            Call::Cl007(message, span) => c7.push(finding(row.line, message.clone(), span)),
        }
    }
    c6.push(finding(
        "MESSAGE e:GetMessage().",
        no_overload_count("Progress.Lang.Error", "GetMessage", 0),
        "e:GetMessage()",
    ));
    (c6, c7)
}
