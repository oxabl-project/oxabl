//! The workspace that `class_members_cli.rs` and `class_members_lsp.rs` share:
//! synthetic classes and one table of assignments through members, each with
//! the `LINT0004` finding it must give or the word "silent".
//!
//! Variable names are chosen on purpose: `oF` and `oN` lex as the keywords `OF`
//! and `ON`, so `oFac` and `oNamed` must keep their names.

use std::path::{Path, PathBuf};

use tempfile::TempDir;

pub const CONFIG: &str = "[workspace]\nname = \"members\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

pub const PERSON: &str = r#"CLASS app.Person:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Age AS INTEGER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Notes AS LONGCHAR NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Tags AS CHARACTER EXTENT 3 NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Spouse AS app.Person NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Window AS HANDLE NO-UNDO GET. SET.
    DEFINE PROTECTED PROPERTY Level AS INTEGER NO-UNDO GET. SET.
    DEFINE PRIVATE PROPERTY Secret AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC STATIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC CHARACTER Greet():
        RETURN "hi".
    END METHOD.
    METHOD PUBLIC app.Person Partner():
        RETURN Spouse.
    END METHOD.
    METHOD PUBLIC STATIC app.Person Create(INPUT pcName AS CHARACTER):
        RETURN NEW app.Person().
    END METHOD.
    METHOD PUBLIC CHARACTER EXTENT 2 Pair():
        DEFINE VARIABLE c AS CHARACTER EXTENT 2 NO-UNDO.
        RETURN c.
    END METHOD.
END CLASS.
"#;

pub const EMPLOYEE: &str = r#"CLASS app.Employee INHERITS app.Person:
    DEFINE PUBLIC PROPERTY Salary AS DECIMAL NO-UNDO GET. SET.
    METHOD PUBLIC OVERRIDE CHARACTER Greet():
        RETURN "hello".
    END METHOD.
    METHOD PUBLIC VOID Check():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        DEFINE VARIABLE d AS DECIMAL NO-UNDO.
        DEFINE VARIABLE oBase AS app.Person NO-UNDO.
        l = THIS-OBJECT:Salary.
        l = THIS-OBJECT:Age.
        l = THIS-OBJECT:Level.
        l = THIS-OBJECT:Secret.
        l = SUPER:Greet().
        l = SUPER:Salary.
        l = THIS-OBJECT:Greet().
        d = THIS-OBJECT:Salary.
        oBase = THIS-OBJECT.
        l = THIS-OBJECT.
        MESSAGE l d oBase.
    END METHOD.
END CLASS.
"#;

const MANAGER: &str = "CLASS app.Manager INHERITS app.Employee:\nEND CLASS.\n";

const INAMED: &str = r#"INTERFACE app.INamed:
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO GET.
END INTERFACE.
"#;

const ISHAPE: &str = r#"INTERFACE app.IShape INHERITS app.INamed:
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO GET.
    METHOD PUBLIC DECIMAL Area().
END INTERFACE.
"#;

const SQUARE: &str = r#"CLASS app.Square IMPLEMENTS app.IShape:
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO GET.
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO GET.
    METHOD PUBLIC DECIMAL Area():
        RETURN 1.0.
    END METHOD.
END CLASS.
"#;

const HOLDER: &str = r#"USING app.*.
CLASS app.Holder:
    DEFINE PUBLIC PROPERTY Owner AS Person NO-UNDO GET. SET.
END CLASS.
"#;

const OTHER_PERSON: &str = r#"CLASS other.Person:
    DEFINE PUBLIC PROPERTY Age AS LOGICAL NO-UNDO GET. SET.
END CLASS.
"#;

const SHADE: &str = "ENUM app.Shade:\n    DEFINE ENUM Light Dark.\nEND ENUM.\n";

const SCORER: &str = r#"CLASS app.Scorer:
    METHOD PUBLIC INTEGER Score(INPUT p AS INTEGER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC CHARACTER Score(INPUT p AS CHARACTER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC DECIMAL Score(INPUT p AS INTEGER, INPUT q AS INTEGER):
        RETURN p + q.
    END METHOD.
    METHOD PUBLIC INT64 Widen(INPUT p AS INT64):
        RETURN p.
    END METHOD.
    METHOD PUBLIC DECIMAL Widen(INPUT p AS DECIMAL):
        RETURN p.
    END METHOD.
    METHOD PUBLIC LOGICAL Same(INPUT p AS INTEGER):
        RETURN p > 0.
    END METHOD.
    METHOD PUBLIC LOGICAL Same(INPUT p AS CHARACTER):
        RETURN p > "".
    END METHOD.
    METHOD PUBLIC INTEGER Fill(INPUT p AS CHARACTER):
        RETURN LENGTH(p).
    END METHOD.
    METHOD PUBLIC CHARACTER Fill(OUTPUT p AS CHARACTER):
        p = "x".
        RETURN p.
    END METHOD.
    METHOD PUBLIC INTEGER Pick(INPUT o AS app.Person):
        RETURN o:Age.
    END METHOD.
    METHOD PUBLIC CHARACTER Pick(INPUT o AS app.IShape):
        RETURN o:Label.
    END METHOD.
    METHOD PUBLIC app.Person Who(INPUT p AS INTEGER):
        RETURN NEW app.Person().
    END METHOD.
    METHOD PUBLIC app.Scorer Who(INPUT p AS CHARACTER):
        RETURN THIS-OBJECT.
    END METHOD.
    METHOD PUBLIC app.Person Who2():
        RETURN NEW app.Person().
    END METHOD.
    METHOD PUBLIC app.Person Who2(INPUT p AS INTEGER):
        RETURN NEW app.Person().
    END METHOD.
END CLASS.
"#;

pub const SUB_SCORER: &str = r#"CLASS app.SubScorer INHERITS app.Scorer:
    METHOD PUBLIC DATE Score(INPUT p AS DATE):
        RETURN p.
    END METHOD.
    METHOD PUBLIC VOID T():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        l = Score(1).
        l = THIS-OBJECT:Score(1).
        l = Score(?).
        l = THIS-OBJECT:Score(1/1/2026).
        MESSAGE l.
    END METHOD.
END CLASS.
"#;

pub const LOCAL_SCORE: &str = r#"CLASS app.LocalScore:
    METHOD PUBLIC INTEGER Rank(INPUT p AS INTEGER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC CHARACTER Rank(INPUT p AS CHARACTER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC VOID T():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        DEFINE VARIABLE n AS INTEGER NO-UNDO.
        l = Rank(1).
        l = THIS-OBJECT:Rank("a").
        l = Rank(?).
        n = 2.
        MESSAGE l THIS-OBJECT:Rank(n).
    END METHOD.
END CLASS.
"#;

const FACTORY: &str = r#"CLASS app.Factory:
    METHOD PUBLIC app.Employee MakeEmployee():
        RETURN NEW app.Employee().
    END METHOD.
    METHOD PUBLIC app.Square MakeSquare():
        RETURN NEW app.Square().
    END METHOD.
    METHOD PUBLIC app.Bottom MakeBottom():
        RETURN NEW app.Bottom().
    END METHOD.
END CLASS.
"#;

const TOP: &str = "CLASS app.Top:\nEND CLASS.\n";

const BOTTOM: &str = "CLASS app.Bottom INHERITS app.Middle:\nEND CLASS.\n";

const APP_ERR: &str = r#"CLASS app.AppErr INHERITS Progress.Lang.AppError:
    DEFINE PUBLIC PROPERTY Code AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC CHARACTER Describe():
        RETURN "".
    END METHOD.
END CLASS.
"#;

/// A class whose parent no file supplies.
const ORPHAN_ERR: &str = r#"CLASS app.OrphanErr INHERITS app.NoSuchBase:
    DEFINE PUBLIC PROPERTY Code AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC CHARACTER Describe():
        RETURN "".
    END METHOD.
END CLASS.
"#;

/// A class file that does not parse.
const BAD: &str = r#"CLASS app.Bad:
    DEFINE PUBLIC PROPERTY X AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC oops oops oops
END CLASS.
"#;

const ON_BAD: &str = r#"CLASS app.OnBad INHERITS app.Bad:
    DEFINE PUBLIC PROPERTY Y AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC INTEGER Z():
        RETURN 1.
    END METHOD.
END CLASS.
"#;

const NAMED: &str = r#"CLASS app.Named:
    METHOD PUBLIC OVERRIDE CHARACTER ToString():
        RETURN "n".
    END METHOD.
END CLASS.
"#;

const CYC_A: &str = r#"CLASS app.CycA INHERITS app.CycB:
    DEFINE PUBLIC PROPERTY A AS INTEGER NO-UNDO GET. SET.
END CLASS.
"#;

const CYC_B: &str = r#"CLASS app.CycB INHERITS app.CycA:
    DEFINE PUBLIC PROPERTY B AS CHARACTER NO-UNDO GET. SET.
END CLASS.
"#;

const ORDER_PROPS: &str = "    DEFINE PUBLIC PROPERTY Total AS DECIMAL NO-UNDO GET. SET.\n";

const ORDER: &str = r#"CLASS app.Order:
{app/order-props.i}
    METHOD PUBLIC CHARACTER Code():
        RETURN "".
    END METHOD.
END CLASS.
"#;

const BROKEN: &str = r#"CLASS app.Broken:
{app/missing-part.i}
    METHOD PUBLIC CHARACTER Code():
        RETURN "".
    END METHOD.
END CLASS.
"#;

const INVOICE_PROPS: &str = "    DEFINE PUBLIC PROPERTY Due AS DATE NO-UNDO GET. SET.\n";

pub const INVOICE: &str = r#"CLASS app.Invoice:
{app/invoice-props.i}
    METHOD PUBLIC VOID T():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        l = THIS-OBJECT:Due.
        MESSAGE l.
    END METHOD.
END CLASS.
"#;

pub const PARTIAL: &str = r#"CLASS app.Partial:
{app/partial-missing.i}
    METHOD PUBLIC INTEGER Size():
        RETURN 1.
    END METHOD.
    METHOD PUBLIC VOID T():
        DEFINE VARIABLE l AS LOGICAL NO-UNDO.
        l = THIS-OBJECT:Size().
        l = Size().
        MESSAGE l.
    END METHOD.
END CLASS.
"#;

/// The files of the workspace: the configuration and the classes. A test
/// writes the program it checks itself.
const FILES: &[(&str, &str)] = &[
    ("oxabl.toml", CONFIG),
    ("app/Person.cls", PERSON),
    ("app/Employee.cls", EMPLOYEE),
    ("app/Manager.cls", MANAGER),
    ("app/INamed.cls", INAMED),
    ("app/IShape.cls", ISHAPE),
    ("app/Square.cls", SQUARE),
    ("app/Holder.cls", HOLDER),
    ("other/Person.cls", OTHER_PERSON),
    ("app/Shade.cls", SHADE),
    ("app/Scorer.cls", SCORER),
    ("app/SubScorer.cls", SUB_SCORER),
    ("app/LocalScore.cls", LOCAL_SCORE),
    ("app/Factory.cls", FACTORY),
    ("app/Top.cls", TOP),
    ("app/Bottom.cls", BOTTOM),
    ("app/AppErr.cls", APP_ERR),
    ("app/OrphanErr.cls", ORPHAN_ERR),
    ("app/Bad.cls", BAD),
    ("app/OnBad.cls", ON_BAD),
    ("app/Named.cls", NAMED),
    ("app/CycA.cls", CYC_A),
    ("app/CycB.cls", CYC_B),
    ("app/order-props.i", ORDER_PROPS),
    ("app/Order.cls", ORDER),
    ("app/Broken.cls", BROKEN),
    ("app/invoice-props.i", INVOICE_PROPS),
    ("app/Invoice.cls", INVOICE),
    ("app/Partial.cls", PARTIAL),
];

/// A temporary copy of the workspace.
pub struct Workspace {
    _dir: TempDir,
    pub root: PathBuf,
}

impl Workspace {
    pub fn new() -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let ws = Workspace { _dir: dir, root };
        for (name, text) in FILES {
            ws.write(name, text);
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

pub fn mismatch(from: &str, to: &str) -> String {
    format!("type mismatch: cannot assign `{from}` to `{to}`")
}

pub fn narrowing(from: &str, to: &str) -> String {
    format!("narrowing conversion: `{from}` assigned to `{to}` may discard data")
}

/// One line of an assignment program and what `LINT0004` says about it.
pub enum Row {
    Silent(&'static str),
    /// The line, the type assigned, the target type, the text of the span.
    Mismatch(&'static str, &'static str, &'static str, &'static str),
    Narrowing(&'static str, &'static str, &'static str, &'static str),
}

impl Row {
    pub fn line(&self) -> &'static str {
        match self {
            Row::Silent(line) | Row::Mismatch(line, ..) | Row::Narrowing(line, ..) => line,
        }
    }

    /// The finding the line must give: its message and span text.
    pub fn finding(&self) -> Option<(String, &'static str)> {
        match self {
            Row::Silent(_) => None,
            Row::Mismatch(_, from, to, span) => Some((mismatch(from, to), span)),
            Row::Narrowing(_, from, to, span) => Some((narrowing(from, to), span)),
        }
    }
}

use Row::{Mismatch as M, Narrowing as N, Silent as S};

/// The declarations the rows of `basic.p` use.
const BASIC_DECLS: &str = "DEFINE VARIABLE oP AS app.Person NO-UNDO.\n\
    DEFINE VARIABLE oE AS app.Employee NO-UNDO.\n\
    DEFINE VARIABLE oM AS app.Manager NO-UNDO.\n\
    DEFINE VARIABLE oS AS app.IShape NO-UNDO.\n\
    DEFINE VARIABLE oObj AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE l AS LOGICAL NO-UNDO.\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    DEFINE VARIABLE cTags AS CHARACTER EXTENT 3 NO-UNDO.\n";

const BASIC_USE: &str = "MESSAGE l i c cTags oP oE oM oS oObj.\n";

pub const PROPERTY_ROWS: &[Row] = &[
    M("i = oP:Name.", "CHARACTER", "INTEGER", "i"),
    S("c = oP:Name."),
    M("oP:Age = \"x\".", "CHARACTER", "INTEGER", "oP:Age"),
    S("oP:Age = 5."),
    N("c = oP:Notes.", "LONGCHAR", "CHARACTER", "c"),
];

pub const METHOD_ROWS: &[Row] = &[
    M("l = oP:Greet().", "CHARACTER", "LOGICAL", "l"),
    S("c = oP:Greet()."),
];

pub const CHAIN_ROWS: &[Row] = &[
    M("l = oP:Spouse:Name.", "CHARACTER", "LOGICAL", "l"),
    M("l = oP:Partner():Age.", "INTEGER", "LOGICAL", "l"),
    S("c = oP:Partner():Partner():Greet()."),
    M(
        "i = oP:Partner():Partner():Greet().",
        "CHARACTER",
        "INTEGER",
        "i",
    ),
];

pub const NEW_CAST_ROWS: &[Row] = &[
    M("l = NEW app.Person():Age.", "INTEGER", "LOGICAL", "l"),
    M(
        "l = CAST(oObj, app.Person):Name.",
        "CHARACTER",
        "LOGICAL",
        "l",
    ),
];

pub const INHERITED_ROWS: &[Row] = &[
    M("l = oE:Age.", "INTEGER", "LOGICAL", "l"),
    M("l = oE:Greet().", "CHARACTER", "LOGICAL", "l"),
    M("l = oM:Name.", "CHARACTER", "LOGICAL", "l"),
    M("l = oM:Salary.", "DECIMAL", "LOGICAL", "l"),
];

pub const INTERFACE_ROWS: &[Row] = &[
    M("l = oS:Area().", "DECIMAL", "LOGICAL", "l"),
    M("l = oS:Label.", "CHARACTER", "LOGICAL", "l"),
    M("l = oS:Title.", "CHARACTER", "LOGICAL", "l"),
];

pub const EXTENT_ROWS: &[Row] = &[
    S("cTags = oP:Tags."),
    M("l = oP:Tags[1].", "CHARACTER", "LOGICAL", "l"),
    M("c = oP:Tags.", "CHARACTER EXTENT 3", "CHARACTER", "c"),
    M("l = oP:Pair().", "CHARACTER EXTENT 2", "LOGICAL", "l"),
];

/// Every topic of `basic.p`: 19 findings and five silent lines.
pub const BASIC_ROWS: &[&[Row]] = &[
    PROPERTY_ROWS,
    METHOD_ROWS,
    CHAIN_ROWS,
    NEW_CAST_ROWS,
    INHERITED_ROWS,
    INTERFACE_ROWS,
    EXTENT_ROWS,
];

/// A program that runs the rows of these topics, one assignment per line.
pub fn basic_program(topics: &[&[Row]]) -> String {
    let mut text = String::from(BASIC_DECLS);
    for row in topics.iter().flat_map(|topic| topic.iter()) {
        text.push_str(row.line());
        text.push('\n');
    }
    text.push_str(BASIC_USE);
    text
}

/// The complete expected `LINT0004` list of the rows, as (line text, message,
/// span text).
pub fn expected(topics: &[&[Row]]) -> Vec<(String, String, String)> {
    topics
        .iter()
        .flat_map(|topic| topic.iter())
        .filter_map(|row| {
            let (message, span) = row.finding()?;
            Some((row.line().to_string(), message, span.to_string()))
        })
        .collect()
}
