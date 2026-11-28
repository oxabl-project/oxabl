//! The bounded listing cache of the daemon: what it keeps, what it drops.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_pipeline::completion::{CachedListing, DirListing, ListingCache, MAX_LISTINGS};
use oxabl_workspace::InMemoryFileSystem;

fn names(listing: Option<Arc<[oxabl_workspace::DirEntry]>>) -> Vec<String> {
    listing
        .expect("listed")
        .iter()
        .map(|e| e.name.to_string())
        .collect()
}

#[test]
fn a_listing_is_sorted_and_kept_until_forgotten() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/d/b.p"), "");
    fs.insert(PathBuf::from("/d/a.p"), "");
    let cache = ListingCache::default();
    let dirs = CachedListing {
        fs: &fs,
        cache: &cache,
    };
    assert_eq!(names(dirs.list(Path::new("/d"))), ["a.p", "b.p"]);
    assert_eq!(cache.len(), 1);

    // A file that appears is not seen while the listing is kept...
    fs.insert(PathBuf::from("/d/c.p"), "");
    let dirs = CachedListing {
        fs: &fs,
        cache: &cache,
    };
    assert_eq!(names(dirs.list(Path::new("/d"))), ["a.p", "b.p"]);
    // ...and is seen after the event of that file.
    cache.forget(&[PathBuf::from("/d/c.p")]);
    assert!(cache.is_empty());
    assert_eq!(names(dirs.list(Path::new("/d"))), ["a.p", "b.p", "c.p"]);
}

#[test]
fn the_cache_never_keeps_more_than_its_cap() {
    let mut fs = InMemoryFileSystem::new();
    for k in 0..MAX_LISTINGS + 10 {
        fs.insert(PathBuf::from(format!("/d{k}/f.p")), "");
    }
    let cache = ListingCache::default();
    let dirs = CachedListing {
        fs: &fs,
        cache: &cache,
    };
    for k in 0..MAX_LISTINGS + 10 {
        dirs.list(Path::new(&format!("/d{k}")));
        assert!(cache.len() <= MAX_LISTINGS);
    }
    assert_eq!(cache.len(), MAX_LISTINGS);
}
