mod condition;
mod preprocessor;
mod span_tree;
mod var_table;

pub use preprocessor::Preprocessor;
pub use span_tree::{PreprocessedFile, SpanNode, UnresolvedInclude};
pub use var_table::PreprocVarTable;
// Re-exported so a caller can describe the compile environment without also
// depending on the workspace crate.
pub use oxabl_workspace::PreprocessorConfig;
