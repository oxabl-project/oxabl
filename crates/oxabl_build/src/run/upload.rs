//! The uploader of a build on a thread of its own: copying a set into a cache
//! never holds up the thread that hands out jobs.

use std::sync::mpsc::{self, Sender};
use std::thread::{self, JoinHandle};

use oxabl_common::catch_panic;

use crate::cache::{ArtifactSetRef, ArtifactUploader, CompileKey, StoreOutcome};

/// What the uploader did with the sets of a build.
#[derive(Default)]
pub struct Uploaded {
    pub outcomes: Vec<(CompileKey, StoreOutcome)>,
    /// Sets the uploader took.
    pub pushed: usize,
    /// The uploader panicked while taking a set: no later set went to it.
    pub broken: bool,
}

/// Takes sets in order and passes them to the uploader.
pub struct Upload {
    sets: Sender<ArtifactSetRef>,
    thread: JoinHandle<Uploaded>,
}

impl Upload {
    pub fn start(mut uploader: Box<dyn ArtifactUploader>) -> Upload {
        let (sets, received) = mpsc::channel::<ArtifactSetRef>();
        let thread = thread::Builder::new()
            .name("oxabl-upload".to_string())
            .spawn(move || {
                let mut uploaded = Uploaded::default();
                for set in received {
                    if uploaded.broken {
                        continue;
                    }
                    match catch_panic(|| uploader.push(set)) {
                        Ok(()) => uploaded.pushed += 1,
                        Err(_) => uploaded.broken = true,
                    }
                }
                uploaded.outcomes = catch_panic(|| uploader.finish()).unwrap_or_default();
                uploaded
            })
            .expect("the upload thread starts");
        Upload { sets, thread }
    }

    pub fn push(&self, set: ArtifactSetRef) {
        let _ = self.sets.send(set);
    }

    /// Waits for every set and the uploader's finish.
    pub fn finish(self) -> Uploaded {
        drop(self.sets);
        self.thread.join().unwrap_or(Uploaded {
            broken: true,
            ..Uploaded::default()
        })
    }
}
