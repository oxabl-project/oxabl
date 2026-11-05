//! Handing verified sets to the uploader, and the check that ends a build.

use oxabl_common::catch_panic;

use super::context::State;
use super::pass::PassEnd;
use crate::cache::ArtifactSetRef;
use crate::worker::Pool;

/// Pushes `sets` to the uploader, one at a time. An uploader that panics is
/// dropped from then on: its sets are not stored.
pub fn push(state: &mut State<'_>, sets: Vec<ArtifactSetRef>) {
    let Some(uploader) = state.uploader.as_mut().filter(|_| !state.uploader_broken) else {
        return;
    };
    for set in sets {
        if catch_panic(|| uploader.push(set)).is_err() {
            state.uploader_broken = true;
            state
                .notes
                .push("the cache failed while taking a set; no more sets are offered".to_string());
            return;
        }
        state.pushed += 1;
    }
}

/// The last schema check: the databases are imaged once more, and the sets
/// still held go to the uploader when the schema is the one of the probe. A
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
    let images = pool.image(&databases).map_err(|error| error.to_string());
    let sets = release.conclude_with(images);
    push(state, sets);
}
