//! `class_name_span` places a class name from the head of a file. Where it
//! answers, it must give the selection that `class_decl` gives; where the
//! head does not show the name plainly, it must answer `None`.

use oxabl_pipeline::{class_decl, class_name_span};

const SHAPES: &[(&str, &str)] = &[
    (
        "CLASS app.Foo:\n  METHOD PUBLIC VOID M():\n  END METHOD.\nEND CLASS.\n",
        "app.Foo",
    ),
    (
        "class app.sub.Foo inherits app.Base:\nend class.\n",
        "app.sub.Foo",
    ),
    (
        "USING app.*.\nBLOCK-LEVEL ON ERROR UNDO, THROW.\n/* class app.Fake */\nFINAL CLASS app.Foo:\nEND CLASS.\n",
        "app.Foo",
    ),
    ("ABSTRACT CLASS Foo:\nEND CLASS.\n", "Foo"),
    (
        "// CLASS Other\nINTERFACE app.IFoo:\nEND INTERFACE.\n",
        "app.IFoo",
    ),
    (
        "ENUM app.Color:\n  DEFINE ENUM Red.\nEND ENUM.\n",
        "app.Color",
    ),
    (
        "USING Progress.Lang.*.\nCLASS app.Foo:\nEND CLASS.\n",
        "app.Foo",
    ),
];

#[test]
fn the_name_span_is_the_selection_of_the_outline() {
    for (text, name) in SHAPES {
        let decl = class_decl(text).unwrap_or_else(|| panic!("no class in {text:?}"));
        let span = class_name_span(text, name).unwrap_or_else(|| panic!("no span in {text:?}"));
        assert_eq!(span, decl.selection, "{text:?}");
    }
}

#[test]
fn a_head_that_does_not_show_the_name_gives_no_span() {
    let cases = [
        // An include or a condition may decide the name.
        ("{header.i}\nCLASS app.Foo:\nEND CLASS.\n", "app.Foo"),
        ("{header.i}.\nCLASS app.Foo:\nEND CLASS.\n", "app.Foo"),
        (
            "&IF DEFINED(X) &THEN\nCLASS app.Foo:\n&ENDIF\nEND CLASS.\n",
            "app.Foo",
        ),
        // The name is not the one asked for, or is only its prefix.
        ("CLASS app.Foo:\nEND CLASS.\n", "app.Bar"),
        ("CLASS app.FooBar:\nEND CLASS.\n", "app.Foo"),
        ("CLASS app.Foo.Bar:\nEND CLASS.\n", "app.Foo"),
        // A name inside a `USING` is not a header.
        (
            "USING Progress.Lang.Class.\nCLASS app.Foo:\nEND CLASS.\n",
            "app.Foo",
        ),
        // No statement.
        ("DEFINE VARIABLE x AS CLASS app.Foo NO-UNDO.\n", "app.Foo"),
    ];
    for (text, name) in cases {
        assert_eq!(class_name_span(text, name), None, "{text:?}");
    }
}

#[test]
fn a_header_past_the_head_gives_no_span() {
    let text = format!("/*{}*/\nCLASS app.Foo:\nEND CLASS.\n", " ".repeat(8000));
    assert!(class_decl(&text).is_some());
    assert_eq!(class_name_span(&text, "app.Foo"), None);
}
