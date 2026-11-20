//! What a caller asks of `oxabl build`.

use std::time::Duration;

use crate::cache::{ArtifactCache, Mode, NoCache};
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
    /// Serves and takes the outputs of the units.
    pub cache: Box<dyn ArtifactCache>,
    /// The cache mode, which decides how strictly the units are verified. The
    /// default is the strictest.
    pub mode: Mode,
    /// How long to wait for the workspace lock; none stops at once when
    /// another build holds it.
    pub wait_lock: Option<Duration>,
    /// Mark the workspace lock so a newer build may stop this one.
    pub yield_to_newer: bool,
}

impl Default for BuildRequest<'_> {
    fn default() -> Self {
        BuildRequest {
            workers: None,
            fail_fast: false,
            cancel: Cancel::new(),
            sink: None,
            cache: Box::new(NoCache),
            mode: Mode::Ci,
            wait_lock: None,
            yield_to_newer: false,
        }
    }
}
