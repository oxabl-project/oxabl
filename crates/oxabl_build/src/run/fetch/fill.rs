//! The filler: stores remote hits in the nearer cache, from their final
//! paths, one batch at a time, so the installer never waits on it.

use std::sync::mpsc::Receiver;

use oxabl_common::catch_panic;

use super::next_batch;
use crate::cache::{ArtifactSetRef, RemoteOrigin};
use crate::run::context::Ctx;

/// Fills every batch from `filled` until the installer leaves. After a
/// cancel, nothing more is filled.
pub fn run(ctx: &Ctx<'_>, filled: &Receiver<(ArtifactSetRef, RemoteOrigin)>) {
    while let Some(batch) = next_batch(filled) {
        if ctx.cancel.is_cancelled() {
            return;
        }
        let _ = catch_panic(|| ctx.cache.fill(batch));
    }
}
