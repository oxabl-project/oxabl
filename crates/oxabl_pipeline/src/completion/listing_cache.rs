//! A bounded cache of sorted directory listings for the daemon's requests.
//!
//! A request comes at each key and lists the same directories each time. The
//! cache keeps each listing, already sorted, for [`LISTING_TTL`]. The owner
//! drops the listings that a file event changes with [`ListingCache::forget`].

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use oxabl_workspace::{DirEntry, FileSystem};

use super::{DirListing, sorted_listing};

/// How long a listing is kept. A change with no file event (a directory, or a
/// file the client does not watch) shows after this.
pub const LISTING_TTL: Duration = Duration::from_secs(3);

/// The most directories kept. The oldest listing goes first.
pub const MAX_LISTINGS: usize = 256;

#[derive(Default)]
struct State {
    /// Counts the drops, so a listing that a drop overtook is not kept.
    epoch: u64,
    listed: HashMap<PathBuf, Listed>,
}

/// When a directory was listed, and its entries (`None`: it cannot be listed).
type Listed = (Instant, Option<Arc<[DirEntry]>>);

/// The directory listings of the recent requests.
#[derive(Default)]
pub struct ListingCache(Mutex<State>);

impl ListingCache {
    fn state(&self) -> std::sync::MutexGuard<'_, State> {
        self.0.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// Drop the listings that a change of `paths` makes wrong: the directory
    /// that holds each path, and the directories under it.
    pub fn forget(&self, paths: &[PathBuf]) {
        let mut state = self.state();
        state.epoch += 1;
        state.listed.retain(|dir, _| {
            !paths
                .iter()
                .any(|path| dir.starts_with(path) || path.parent() == Some(dir.as_path()))
        });
    }

    /// Drop every listing.
    pub fn clear(&self) {
        let mut state = self.state();
        state.epoch += 1;
        state.listed.clear();
    }

    /// The listings kept now.
    pub fn len(&self) -> usize {
        self.state().listed.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn list(&self, fs: &dyn FileSystem, dir: &Path) -> Option<Arc<[DirEntry]>> {
        let now = Instant::now();
        let epoch;
        {
            let mut state = self.state();
            match state.listed.get(dir) {
                Some((at, listing)) if now.duration_since(*at) < LISTING_TTL => {
                    return listing.clone();
                }
                // Freed when found expired.
                Some(_) => {
                    state.listed.remove(dir);
                }
                None => {}
            }
            epoch = state.epoch;
        }
        // Listed with no lock held: a slow directory does not stop the others.
        let listing = sorted_listing(fs, dir);
        let mut state = self.state();
        if state.epoch != epoch {
            return listing;
        }
        // Every insert frees the other listings that expired.
        state
            .listed
            .retain(|_, (at, _)| now.duration_since(*at) < LISTING_TTL);
        while state.listed.len() >= MAX_LISTINGS {
            let Some(oldest) = state
                .listed
                .iter()
                .min_by_key(|(_, (at, _))| *at)
                .map(|(dir, _)| dir.clone())
            else {
                break;
            };
            state.listed.remove(&oldest);
        }
        state
            .listed
            .insert(dir.to_path_buf(), (now, listing.clone()));
        listing
    }
}

/// A [`DirListing`] that reads a [`ListingCache`] before the file system.
pub struct CachedListing<'c> {
    pub fs: &'c dyn FileSystem,
    pub cache: &'c ListingCache,
}

impl DirListing for CachedListing<'_> {
    fn list(&self, dir: &Path) -> Option<Arc<[DirEntry]>> {
        self.cache.list(self.fs, dir)
    }
}
