//! Two caches in a chain: the nearer one is asked first.

use std::path::Path;

use super::seam::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, CompileKey, FetchOutcome, FetchRequest,
    RemoteOrigin, StoreOutcome,
};

/// `near` answers first; only the keys it misses go to `far`. Sets are pushed
/// to both, and a set that `far` gave is stored in `near`.
pub struct Layered {
    near: Box<dyn ArtifactCache>,
    far: Box<dyn ArtifactCache>,
    label: String,
}

impl Layered {
    pub fn new(near: Box<dyn ArtifactCache>, far: Box<dyn ArtifactCache>) -> Layered {
        let label = format!("{}+{}", near.label(), far.label());
        Layered { near, far, label }
    }
}

struct Both {
    near: Box<dyn ArtifactUploader>,
    far: Box<dyn ArtifactUploader>,
}

impl ArtifactUploader for Both {
    fn push(&mut self, set: ArtifactSetRef) {
        self.near.push(set.clone());
        self.far.push(set);
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        let Both { near, far } = *self;
        let mut outcomes = near.finish();
        outcomes.extend(far.finish());
        outcomes
    }
}

impl ArtifactCache for Layered {
    fn label(&self) -> &str {
        &self.label
    }

    fn writes(&self) -> bool {
        self.near.writes() || self.far.writes()
    }

    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    ) {
        let mut missed = Vec::new();
        {
            let by_key: std::collections::HashMap<CompileKey, &FetchRequest> = requests
                .iter()
                .map(|request| (request.key, request))
                .collect();
            let mut answered = std::collections::HashSet::new();
            self.near
                .fetch(requests.clone(), stage, &mut |outcome| match &outcome {
                    FetchOutcome::Installed { key, .. } => {
                        answered.insert(*key);
                        on_outcome(outcome);
                    }
                    FetchOutcome::Miss { key, .. } => {
                        answered.insert(*key);
                        missed.extend(by_key.get(key).map(|request| (*request).clone()));
                    }
                });
            for request in &requests {
                if !answered.contains(&request.key) {
                    missed.push(request.clone());
                }
            }
        }
        if !missed.is_empty() {
            self.far.fetch(missed, stage, on_outcome);
        }
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(Both {
            near: self.near.uploader(),
            far: self.far.uploader(),
        })
    }

    fn cancel_fetch(&self) {
        self.near.cancel_fetch();
        self.far.cancel_fetch();
    }

    fn notes(&self) -> Vec<String> {
        let mut notes = self.near.notes();
        notes.extend(self.far.notes());
        notes
    }

    fn fill(&self, set: ArtifactSetRef, origin: &RemoteOrigin) {
        self.near.fill(set, origin);
    }
}
