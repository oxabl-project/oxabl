//! Helpers for the black-box tests of `textDocument/completion`: a synthetic
//! workspace on disk, one `oxabl lsp` session over it, a cursor marked with `|`
//! in the text, and checks on the answer. Each test file includes it with
//! `#[path]` after `session.rs`.

#![allow(dead_code)]

use std::path::{Path, PathBuf};

use crate::session::{Session, file_uri};
use serde_json::{Value, json};
use tempfile::TempDir;

pub const METHOD: u64 = 2;
pub const FUNCTION: u64 = 3;
pub const FIELD: u64 = 5;
pub const VARIABLE: u64 = 6;
pub const CLASS: u64 = 7;
pub const PROPERTY: u64 = 10;
pub const KEYWORD: u64 = 14;
pub const SNIPPET: u64 = 15;
pub const STRUCT: u64 = 22;

/// What the client says about itself, unless a case says otherwise.
pub fn capabilities() -> Value {
    capabilities_with_snippets(true)
}

pub fn capabilities_with_snippets(snippets: bool) -> Value {
    json!({
        "general": {"positionEncodings": ["utf-16"]},
        "textDocument": {
            "completion": {
                "completionItem": {
                    "snippetSupport": snippets,
                    "insertReplaceSupport": true,
                    "labelDetailsSupport": true,
                    "resolveSupport": {"properties": ["documentation", "detail"]}
                },
                "contextSupport": true
            }
        }
    })
}

/// The `keyword_case` of the `[workspace.style]` table.
#[derive(Clone, Copy)]
pub enum Case {
    Upper,
    Lower,
    Preserve,
    /// No `[workspace.style]` table.
    Unset,
}

fn style(case: Case) -> &'static str {
    match case {
        Case::Upper => "[workspace.style]\nkeyword_case = \"Uppercase\"\n",
        Case::Lower => "[workspace.style]\nkeyword_case = \"Lowercase\"\n",
        Case::Preserve => "[workspace.style]\nkeyword_case = \"Preserve\"\n",
        Case::Unset => "",
    }
}

pub const SHOP_DF: &str = "ADD TABLE \"Customer\"
  AREA \"Data\"

ADD FIELD \"CustNum\" OF \"Customer\" AS integer
  FORMAT \">>>>9\"

ADD FIELD \"Name\" OF \"Customer\" AS character
  FORMAT \"x(30)\"

ADD FIELD \"Balance\" OF \"Customer\" AS decimal
  FORMAT \"->>,>>9.99\"

ADD FIELD \"Phone\" OF \"Customer\" AS character

ADD TABLE \"Order\"
  AREA \"Data\"

ADD FIELD \"OrderNum\" OF \"Order\" AS integer
  FORMAT \">>>>9\"

ADD FIELD \"CustNum\" OF \"Order\" AS integer
  FORMAT \">>>>9\"

ADD TABLE \"Item\"
  AREA \"Data\"

ADD FIELD \"ItemNum\" OF \"Item\" AS integer

ADD FIELD \"Descr\" OF \"Item\" AS character
  FORMAT \"x(20)\"

";

pub const SHAPE_CLS: &str = "CLASS app.shapes.Shape ABSTRACT:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROTECTED VARIABLE iSides AS INTEGER NO-UNDO.
    DEFINE PRIVATE VARIABLE cSecret AS CHARACTER NO-UNDO.
    METHOD PUBLIC DECIMAL Area():
        RETURN 0.
    END METHOD.
    METHOD PUBLIC VOID Describe():
    END METHOD.
    METHOD PUBLIC VOID Describe(INPUT pcPrefix AS CHARACTER):
    END METHOD.
    METHOD PROTECTED VOID Recalc():
    END METHOD.
    METHOD PUBLIC STATIC app.shapes.Shape Unit():
        RETURN ?.
    END METHOD.
END CLASS.
";

pub const IDRAWABLE_CLS: &str = "INTERFACE app.shapes.IDrawable:
    METHOD PUBLIC VOID Draw().
END INTERFACE.
";

pub const CIRCLE_CLS: &str = "USING app.shapes.*.
CLASS app.shapes.Circle INHERITS Shape IMPLEMENTS IDrawable:
    DEFINE PUBLIC PROPERTY Radius AS DECIMAL NO-UNDO GET. SET.
    METHOD PUBLIC OVERRIDE DECIMAL Area():
        RETURN 3.14 * Radius * Radius.
    END METHOD.
    METHOD PUBLIC VOID Draw():
    END METHOD.
    METHOD PUBLIC Circle Grow():
        RETURN THIS-OBJECT.
    END METHOD.
    METHOD PUBLIC Circle Pick(INPUT i AS INTEGER):
        RETURN THIS-OBJECT.
    END METHOD.
    METHOD PUBLIC Shape Pick(INPUT c AS CHARACTER):
        RETURN THIS-OBJECT.
    END METHOD.
END CLASS.
";

/// The files of a workspace, before its session starts.
pub struct Files(Vec<(String, String)>);

impl Files {
    /// Config N: no schema and no include path.
    pub fn config_n(case: Case) -> Files {
        Files(vec![(
            "oxabl.toml".into(),
            format!("[workspace]\nname = \"bare\"\n{}", style(case)),
        )])
    }

    /// Config A: the include paths `.` and `lib` and the schema `db/shop.df`.
    pub fn config_a(case: Case) -> Files {
        Files(vec![
            (
                "oxabl.toml".into(),
                format!(
                    "[workspace]\nname = \"complete\"\n\
                     [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
                     [workspace.schema]\nfiles = [\"db/shop.df\"]\n{}",
                    style(case)
                ),
            ),
            ("db/shop.df".into(), SHOP_DF.into()),
        ])
    }

    pub fn file(mut self, path: &str, text: &str) -> Files {
        self.0.push((path.into(), text.into()));
        self
    }

    /// The classes of the package `app.shapes` under `lib/`.
    pub fn shapes(self) -> Files {
        self.file("lib/app/shapes/Shape.cls", SHAPE_CLS)
            .file("lib/app/shapes/IDrawable.cls", IDRAWABLE_CLS)
            .file("lib/app/shapes/Circle.cls", CIRCLE_CLS)
    }

    pub fn start(self) -> Ws {
        self.start_with(capabilities())
    }

    pub fn start_with(self, capabilities: Value) -> Ws {
        self.start_with_options(capabilities, Value::Null)
    }

    /// Start with `options` as the initialization options (`null`: none).
    pub fn start_with_options(self, capabilities: Value, options: Value) -> Ws {
        let dir = tempfile::tempdir().unwrap();
        for (path, text) in &self.0 {
            write(&dir.path().join(path), text);
        }
        let session = Session::start_with_options(dir.path(), capabilities, options);
        Ws { dir, session }
    }
}

fn write(path: &Path, text: &str) {
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

/// A line and a UTF-16 character.
#[derive(Clone, Copy, Debug)]
pub struct Cursor {
    pub line: u32,
    pub character: u32,
}

impl Cursor {
    pub fn json(self) -> Value {
        json!({"line": self.line, "character": self.character})
    }
}

/// The text without its one `|`, and where the `|` was.
pub fn split_cursor(marked: &str) -> (String, Cursor) {
    assert_eq!(marked.matches('|').count(), 1, "one `|` in {marked:?}");
    let at = marked.find('|').unwrap();
    let before = &marked[..at];
    let line = before.matches('\n').count() as u32;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let character = before[line_start..].encode_utf16().count() as u32;
    let text = format!("{before}{}", &marked[at + 1..]);
    (text, Cursor { line, character })
}

/// One workspace on disk and the session over it.
pub struct Ws {
    pub dir: TempDir,
    pub session: Session,
}

impl Ws {
    pub fn path(&self, rel: &str) -> PathBuf {
        self.dir.path().join(rel)
    }

    pub fn canonical(&self, rel: &str) -> PathBuf {
        std::fs::canonicalize(self.path(rel)).unwrap()
    }

    /// Write `marked` without its `|` to `rel`, open it with that text, and
    /// return the cursor.
    pub fn open_marked(&mut self, rel: &str, marked: &str) -> Cursor {
        let (text, cursor) = split_cursor(marked);
        let path = self.path(rel);
        write(&path, &text);
        self.session.open(&path, &text);
        cursor
    }

    /// The answer to a completion request at `cursor`.
    #[track_caller]
    pub fn complete_at(&mut self, rel: &str, cursor: Cursor, trigger: Option<&str>) -> Answer {
        let context = match trigger {
            None => json!({"triggerKind": 1}),
            Some(c) => json!({"triggerKind": 2, "triggerCharacter": c}),
        };
        let response = self.session.request(
            "textDocument/completion",
            json!({
                "textDocument": {"uri": file_uri(&self.path(rel))},
                "position": cursor.json(),
                "context": context
            }),
        );
        Answer::parse(&response)
    }

    /// Open `rel` with the text `marked` and ask at its `|`.
    #[track_caller]
    pub fn complete(&mut self, rel: &str, marked: &str) -> Answer {
        let cursor = self.open_marked(rel, marked);
        self.complete_at(rel, cursor, None)
    }

    /// As [`Ws::complete`], with the trigger character `trigger`.
    #[track_caller]
    pub fn complete_trigger(&mut self, rel: &str, marked: &str, trigger: &str) -> Answer {
        let cursor = self.open_marked(rel, marked);
        self.complete_at(rel, cursor, Some(trigger))
    }

    /// The answer to `completionItem/resolve` for `item`, as it came.
    #[track_caller]
    pub fn resolve(&mut self, item: &Value) -> Value {
        let response = self.session.request("completionItem/resolve", item.clone());
        assert!(
            response.get("error").is_none(),
            "resolve failed: {response}"
        );
        response["result"].clone()
    }

    /// An incremental `didChange` that inserts `text` at `line`, character 0.
    pub fn insert_at_line_start(&mut self, rel: &str, version: u32, line: u32, text: &str) {
        self.change(rel, version, (line, 0), (line, 0), text);
    }

    pub fn change(
        &mut self,
        rel: &str,
        version: u32,
        start: (u32, u32),
        end: (u32, u32),
        text: &str,
    ) {
        self.session.send(json!({
            "jsonrpc": "2.0", "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": file_uri(&self.path(rel)), "version": version},
                "contentChanges": [{
                    "range": {
                        "start": {"line": start.0, "character": start.1},
                        "end": {"line": end.0, "character": end.1}
                    },
                    "text": text
                }]
            }
        }));
    }
}

/// The text of a `documentation` field: a string or a Markdown `MarkupContent`.
#[track_caller]
pub fn documentation(item: &Value) -> String {
    match &item["documentation"] {
        Value::String(text) => text.clone(),
        Value::Object(content) => {
            assert_eq!(
                content.get("kind"),
                Some(&json!("markdown")),
                "documentation is Markdown: {item}"
            );
            content["value"].as_str().expect("a markup value").into()
        }
        other => panic!("no documentation ({other}) in {item}"),
    }
}

/// What a completion request answered.
#[derive(Debug)]
pub enum Answer {
    Null,
    List(List),
}

#[derive(Debug)]
pub struct List {
    pub incomplete: bool,
    pub items: Vec<Value>,
}

impl Answer {
    /// The answer to the response `response`: `null`, a list, or a failure.
    #[track_caller]
    pub fn parse(response: &Value) -> Answer {
        assert!(
            response.get("error").is_none(),
            "completion answered with an error: {response}"
        );
        let result = &response["result"];
        if result.is_null() {
            return Answer::Null;
        }
        let incomplete = result["isIncomplete"]
            .as_bool()
            .unwrap_or_else(|| panic!("`isIncomplete` must be a boolean: {response}"));
        let items = result["items"]
            .as_array()
            .unwrap_or_else(|| panic!("`items` must be an array: {response}"))
            .clone();
        Answer::List(List { incomplete, items })
    }

    #[track_caller]
    pub fn assert_null(&self) {
        assert!(
            matches!(self, Answer::Null),
            "expected `null`, got {}",
            self.summary()
        );
    }

    /// The list; a `null` answer fails the test.
    #[track_caller]
    pub fn list(&self) -> &List {
        match self {
            Answer::List(list) => list,
            Answer::Null => panic!("expected a list, got `null`"),
        }
    }

    pub fn summary(&self) -> String {
        match self {
            Answer::Null => "null".into(),
            Answer::List(list) => list.summary(),
        }
    }
}

pub fn label(item: &Value) -> &str {
    item["label"].as_str().expect("an item has a label")
}

/// The text the item inserts: `textEdit.newText`.
#[track_caller]
pub fn new_text(item: &Value) -> &str {
    item["textEdit"]["newText"]
        .as_str()
        .unwrap_or_else(|| panic!("the item has no `textEdit.newText`: {item}"))
}

impl List {
    pub fn summary(&self) -> String {
        let shown: Vec<String> = self
            .items
            .iter()
            .take(80)
            .map(|i| format!("{}:{}", i["kind"], i["label"].as_str().unwrap_or("?")))
            .collect();
        format!(
            "list(isIncomplete: {}, {} items: [{}{}])",
            self.incomplete,
            self.items.len(),
            shown.join(", "),
            if self.items.len() > 80 { ", ..." } else { "" }
        )
    }

    pub fn find(&self, kind: u64, label_: &str) -> Option<&Value> {
        self.items
            .iter()
            .find(|i| i["kind"] == json!(kind) && label(i) == label_)
    }

    /// The item with this kind and label; its absence fails the test.
    #[track_caller]
    pub fn item(&self, kind: u64, label_: &str) -> &Value {
        self.find(kind, label_).unwrap_or_else(|| {
            panic!(
                "no item of kind {kind} named `{label_}` in {}",
                self.summary()
            )
        })
    }

    /// Every named label is an item of `kind`.
    #[track_caller]
    pub fn contains(&self, kind: u64, labels: &[&str]) {
        for label_ in labels {
            self.item(kind, label_);
        }
    }

    /// No item of any kind has one of these labels.
    #[track_caller]
    pub fn lacks(&self, labels: &[&str]) {
        for label_ in labels {
            assert!(
                !self.items.iter().any(|i| label(i) == *label_),
                "an item named `{label_}` must not be in {}",
                self.summary()
            );
        }
    }

    /// No item of `kind` has one of these labels.
    #[track_caller]
    pub fn lacks_kind(&self, kind: u64, labels: &[&str]) {
        for label_ in labels {
            assert!(
                self.find(kind, label_).is_none(),
                "an item of kind {kind} named `{label_}` must not be in {}",
                self.summary()
            );
        }
    }

    /// No item is of this kind.
    #[track_caller]
    pub fn has_no_kind(&self, kind: u64) {
        assert!(
            !self.items.iter().any(|i| i["kind"] == json!(kind)),
            "no item of kind {kind} expected in {}",
            self.summary()
        );
    }

    pub fn labels_of(&self, kind: u64) -> Vec<&str> {
        self.items
            .iter()
            .filter(|i| i["kind"] == json!(kind))
            .map(label)
            .collect()
    }

    /// The index of the item of `label_` after sorting by `sortText`, then by
    /// label. Every item must have a `sortText`.
    #[track_caller]
    pub fn rank(&self, label_: &str) -> usize {
        let mut order: Vec<(&str, &str)> = self
            .items
            .iter()
            .map(|i| {
                let sort = i["sortText"]
                    .as_str()
                    .unwrap_or_else(|| panic!("an item needs a `sortText`: {i}"));
                (sort, label(i))
            })
            .collect();
        order.sort();
        order
            .iter()
            .position(|(_, l)| *l == label_)
            .unwrap_or_else(|| panic!("no item named `{label_}` in {}", self.summary()))
    }

    /// `first` is before `second` in the order of the client.
    #[track_caller]
    pub fn assert_before(&self, first: &str, second: &str) {
        let (a, b) = (self.rank(first), self.rank(second));
        assert!(
            a < b,
            "`{first}` (at {a}) must come before `{second}` (at {b}) in {}",
            self.summary()
        );
    }
}
