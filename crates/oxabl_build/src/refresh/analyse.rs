use std::path::PathBuf;

use oxabl_pipeline::{ObservedEdges, PipelineConfig, SharedRun};
use oxabl_workspace::{Observations, ObservingFileSystem};

use super::convert::Converter;
use super::intern::Interner;
use super::parallel;
use super::snapshot::SnapshotFs;
use crate::paths::PathMap;
use crate::store::Analysis;

/// What the analysis of a batch of nodes reads through.
pub struct Analyser<'a> {
    pub config: &'a PipelineConfig,
    pub fs: &'a ObservingFileSystem<SnapshotFs>,
    pub paths: &'a PathMap,
    pub interner: &'a Interner,
    pub generation: u64,
}

/// A node to analyse: its path id and its host path.
pub struct Node {
    pub id: u32,
    pub path: PathBuf,
}

impl Analyser<'_> {
    /// Analyse `nodes` on all cores through one shared run, so what the nodes
    /// share is read and analysed once. The results are in the order of
    /// `nodes`.
    pub fn analyse(&self, nodes: &[Node]) -> Vec<(u32, Analysis)> {
        let run = SharedRun::new(self.config, self.fs);
        let converter = Converter {
            paths: self.paths,
            interner: self.interner,
            run: &run,
        };
        parallel::map(nodes, |node| {
            let (text, hash) = self.fs.inner().read_hashed(&node.path);
            let analysis = match text {
                Ok(text) => {
                    let handle = run.handle().with_conditional_include_closure(true);
                    let pipeline = handle.with_file(node.path.clone());
                    match pipeline.edges_of_observed(&text) {
                        Ok(ObservedEdges {
                            edges,
                            observations,
                        }) => converter.analysis(self.generation, hash, edges, &observations),
                        Err(not_observing) => converter.analysis(
                            self.generation,
                            hash,
                            Err(not_observing.to_string()),
                            &Observations::default(),
                        ),
                    }
                }
                Err(error) => converter.analysis(
                    self.generation,
                    hash,
                    Err(error.to_string()),
                    &Observations::default(),
                ),
            };
            (node.id, analysis)
        })
    }
}
