use std::path::{Path, PathBuf};

use oxabl_workspace::{WorkspaceConfig, normalize_root_extensions, normalize_run_extensions};

use super::probe::hash_file;
use crate::config::BuildSettings;
use crate::encode::Encoder;

const CONTEXT: &str = "oxabl 2026-11 analysis identity v1";

fn text(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

fn option<T>(encoder: &mut Encoder, value: Option<&T>, each: impl FnOnce(&mut Encoder, &T)) {
    match value {
        Some(value) => {
            encoder.u64(1);
            each(encoder, value);
        }
        None => {
            encoder.u64(0);
        }
    }
}

/// The hash of everything besides file contents that decides what an analysis
/// finds: the oxabl version, the hash of the running executable, the source
/// and preprocessor settings and the schema files. When it changes, every node
/// is analysed again.
pub fn analysis_identity(
    binary: &[u8; 32],
    config: &WorkspaceConfig,
    settings: &BuildSettings,
    schema_files: &[PathBuf],
) -> [u8; 32] {
    let sources = &config.workspace.sources;
    let preprocessor = &config.workspace.preprocessor;
    let mut encoder = Encoder::new();
    encoder.str(env!("CARGO_PKG_VERSION"));
    encoder.hash(binary);

    encoder.str("sources");
    let paths = |encoder: &mut Encoder, items: &[PathBuf]| {
        encoder.list(items, |e, p| {
            e.str(&text(p));
        });
    };
    paths(&mut encoder, &sources.directories);
    paths(&mut encoder, &sources.include_paths);
    paths(&mut encoder, &sources.root_relative_include_paths);
    encoder.strs(&normalize_run_extensions(&sources.run_extensions).extensions);
    encoder.bool(sources.case_insensitive_lookup);
    encoder.str(sources.encoding.name());
    option(&mut encoder, sources.fallback_encoding.as_ref(), |e, v| {
        e.str(v.name());
    });
    encoder.strs(&normalize_root_extensions(&sources.root_extensions).extensions);
    encoder.strs(&config.build.exclude);
    option(&mut encoder, settings.dlc.as_ref(), |e, v| {
        e.str(&text(v));
    });

    encoder.str("preprocessor");
    encoder.str(&preprocessor.opsys);
    encoder.str(&preprocessor.window_system);
    encoder.bool(preprocessor.batch_mode);
    encoder.u64(u64::from(preprocessor.process_architecture));
    option(&mut encoder, preprocessor.proversion.as_ref(), |e, v| {
        e.str(v);
    });
    option(&mut encoder, preprocessor.databases.as_ref(), |e, v| {
        e.strs(v);
    });
    encoder.u64(preprocessor.database_types.len() as u64);
    for (name, kind) in &preprocessor.database_types {
        encoder.str(name);
        encoder.str(kind);
    }

    encoder.str("schema");
    encoder.list(schema_files, |e, file| {
        e.str(&text(file));
        e.hash(&hash_file(file, false));
    });
    encoder.finish(CONTEXT)
}
