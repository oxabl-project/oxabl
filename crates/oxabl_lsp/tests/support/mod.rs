//! A project for the tests of diagnostics and formatting: with no `oxabl.toml`
//! the server turns both off.

use std::path::Path;
use std::str::FromStr;

use lsp_types::{Uri, WorkspaceFolder};
use tempfile::TempDir;

/// A folder with an `oxabl.toml`. Keep it alive for the length of the test.
pub fn project() -> TempDir {
    let root = TempDir::new().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n",
    )
    .unwrap();
    root
}

/// `root` as the one workspace folder of an `initialize` request.
pub fn folders(root: &Path) -> Option<Vec<WorkspaceFolder>> {
    Some(vec![WorkspaceFolder {
        uri: Uri::from_str(&format!("file://{}", root.display())).unwrap(),
        name: "test".to_string(),
    }])
}
