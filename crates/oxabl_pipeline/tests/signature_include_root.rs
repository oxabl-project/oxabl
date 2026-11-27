//! A routine of the unit with a parameter include is shown as one entry only
//! when the text that is read for the unit is the text that was analysed. All
//! sources are synthetic.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

const ANALYSED: &str = "PROCEDURE Local:\n    {inc/std-input.i}\n\
    \x20   DEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\nEND PROCEDURE.\nRUN Local (1, 2, 3).\n";

/// The label of the signature at the call of `ANALYSED`, when the file system
/// holds `on_disk` for the unit.
fn label_with(on_disk: &str) -> Option<String> {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/ws/unit.p"), on_disk);
    fs.insert(
        PathBuf::from("/ws/inc/std-input.i"),
        "DEFINE INPUT PARAMETER pcCompany AS CHARACTER NO-UNDO.\n\
         DEFINE INPUT PARAMETER pcUser AS CHARACTER NO-UNDO.\n",
    );
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    };
    let path = Path::new("/ws/unit.p");
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(path);
    let expansion = handle.expand(ANALYSED);
    let none = |_| None;
    let model = Arc::new(handle.navigation_model(&expansion).expect("preprocesses"));
    let navigation = handle
        .navigation_with(&expansion, model, &none)
        .expect("preprocesses");
    let call = ANALYSED.find("Local (1").unwrap() + "Local (".len();
    let help = navigation.signature_help(path, call as u32)?;
    Some(help.signatures[0].label.clone())
}

#[test]
fn the_include_shows_as_one_entry_when_the_text_read_is_the_text_analysed() {
    assert_eq!(
        label_with(ANALYSED).as_deref(),
        Some("Local({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)")
    );
}

#[test]
fn no_signature_when_the_text_read_is_not_the_text_analysed() {
    // The same unit with a comment before it: the bytes of the reference moved.
    let stale = format!("/* an older revision of the unit */\n{ANALYSED}");
    assert_eq!(label_with(&stale), None);
}
