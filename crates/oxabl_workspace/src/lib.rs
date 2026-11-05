mod build_config;
mod cached;
mod config;
mod discovery;
mod encoding;
mod file_system;
mod include_paths;
mod observations;
mod observing;
mod workspace;

pub use build_config::{
    BuildCache, BuildCompile, BuildConfig, BuildOutputs, BuildVerify, BuildWorkers, Size, TimeSpan,
    XrefFormat,
};
pub use cached::CachedFileSystem;
pub use config::{
    LintConfig, LintOverride, LintRuleSettings, LintSeverity, PreprocessorConfig,
    RejectedLintPattern, RunExtensions, WorkspaceConfig, default_root_extensions,
    default_run_extensions, normalize_root_extensions, normalize_run_extension,
    normalize_run_extensions,
};
pub use discovery::{
    ROOT_EXTENSIONS, discover_path, discover_path_in, is_include_fragment, is_root_file,
    is_root_file_in, walk_directory, walk_directory_in, walk_include_fragments,
};
pub use encoding::{DecodedSource, SourceDecoding, SourceEncoding};
pub use file_system::{EncodedFileSystem, FileSystem, InMemoryFileSystem, RealFileSystem};
pub use include_paths::{
    DLC_SOURCES_VARIABLE, IncludeEntry, dlc_sources, find_workspace_root, include_entry,
};
pub use observations::{Observations, Observed, unobserved};
pub use observing::ObservingFileSystem;
pub use workspace::Workspace;
