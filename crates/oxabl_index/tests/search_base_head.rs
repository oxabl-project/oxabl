//! `search_base` reads the name of a class from the head of its source. The
//! base must be the one the whole source gives, wherever the header is.

use std::path::{Path, PathBuf};

use oxabl_index::search::search_base;

const FILE: &str = "/src/app/sub/Foo.cls";

fn base(text: &str) -> Option<PathBuf> {
    search_base(Path::new(FILE), text)
}

#[test]
fn the_package_root_is_found_wherever_the_header_is() {
    let root = Some(PathBuf::from("/src"));
    let header = "CLASS app.sub.Foo:\nEND CLASS.\n";
    for padding in [0, 100, 4000, 4090, 4096, 4097, 9000] {
        let text = format!("/*{}*/\n{header}", " ".repeat(padding));
        assert_eq!(base(&text), root, "padding {padding}");
    }
    // Several lines of comment, the header on the line that crosses the cut.
    let lines = "// padding padding padding padding padding\n".repeat(98);
    let text = format!("{lines}CLASS app.sub.Foo:\nEND CLASS.\n");
    assert_eq!(base(&text), root);
}

#[test]
fn a_name_on_the_line_that_crosses_the_cut_is_not_cut_in_two() {
    // The header line holds byte 4096, in the middle of the name.
    let header = "CLASS app.sub.Foo:\n";
    let before = 4096 - "CLASS app.s".len();
    let padding = "//\n".repeat(before / 3);
    let text = format!(
        "{padding}{}{header}END CLASS.\n",
        " ".repeat(before - padding.len())
    );
    assert!(text.find("app.sub").unwrap() < 4096 && text.find("Foo").unwrap() > 4096);
    assert_eq!(base(&text), Some(PathBuf::from("/src")));
}

#[test]
fn a_header_in_a_condition_keeps_the_directory_wherever_it_is() {
    let dir = Some(PathBuf::from("/src/app/sub"));
    for padding in [0, 5000] {
        let text = format!(
            "/*{}*/\n&IF DEFINED(X) &THEN\nCLASS app.sub.Foo:\n&ENDIF\nEND CLASS.\n",
            " ".repeat(padding)
        );
        assert_eq!(base(&text), dir, "padding {padding}");
    }
}

#[test]
fn a_source_with_no_header_keeps_the_directory() {
    let text = format!("/*{}*/\nMESSAGE \"x\".\n", " ".repeat(5000));
    assert_eq!(base(&text), Some(PathBuf::from("/src/app/sub")));
}
