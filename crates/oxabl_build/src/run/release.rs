//! Handing verified sets to the uploader, and the check that ends a build.

use super::context::State;
use super::pass::PassEnd;
use crate::cache::ArtifactSetRef;
use crate::worker::Pool;

/// Hands `sets` to the uploader, in order.
pub fn push(state: &mut State<'_>, sets: Vec<ArtifactSetRef>) {
    let Some(upload) = state.upload.as_ref() else {
        return;
    };
    for set in sets {
        upload.push(set);
    }
}

/// The last schema check: the databases are imaged once more (or a raw pass
/// runs), and the sets still held go to the uploader when the schema is the
/// one of the probe. A
/// cancel, a store error or an empty pool releases nothing.
pub fn finish(state: &mut State<'_>, pool: &mut Pool, end: PassEnd) {
    let Some(release) = state.release.as_mut() else {
        return;
    };
    release.abandon_round();
    if matches!(end, PassEnd::Cancelled | PassEnd::Fatal) || !release.holds() {
        release.discard();
        return;
    }
    let databases = release.databases();
    let sets = match release.is_raw() {
        true => {
            let digests = pool.raw_digests(&databases);
            release.conclude_with_raw(digests)
        }
        false => {
            let images = pool.image(&databases).map_err(|error| error.to_string());
            release.conclude_with(images)
        }
    };
    push(state, sets);
}
