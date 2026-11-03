//! What a caller asks of `oxabl build`.

use crate::cache::{ArtifactCache, NoCache};
use crate::events::Sink;
use crate::worker::Cancel;

/// The inputs of one build besides the workspace.
pub struct BuildRequest<'a> {
    /// Replaces `[build.workers].count`.
    pub workers: Option<usize>,
    /// Stop dispatching at the first failure.
    pub fail_fast: bool,
    pub cancel: Cancel,
    /// Hears the events of the build.
    pub sink: Option<Sink<'a>>,
    /// Takes the outputs of the units that compiled.
    pub cache: Box<dyn ArtifactCache>,
}

impl Default for BuildRequest<'_> {
    fn default() -> Self {
        BuildRequest {
            workers: None,
            fail_fast: false,
            cancel: Cancel::new(),
            sink: None,
            cache: Box::new(NoCache),
        }
    }
}
