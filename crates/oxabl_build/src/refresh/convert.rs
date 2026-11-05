use std::path::{Path, PathBuf};

use oxabl_pipeline::{
    ClosureReference, DegradedReason, DependencyEdges, EdgeKind as IndexEdgeKind, EdgeTarget,
    SharedRun,
};
use oxabl_semantic::UnresolvedReason;
use oxabl_workspace::Observations;

use super::intern::Interner;
use crate::kinds::{EdgeKind, ObservationKind, degraded_reason, unresolved_reason};
use crate::paths::PathMap;
use crate::store::{Analysis, Edge, Observation, Unresolved};

/// Turns what the pipeline found into the rows of the store.
pub struct Converter<'a> {
    pub paths: &'a PathMap,
    pub interner: &'a Interner,
    pub run: &'a SharedRun<'a>,
}

impl Converter<'_> {
    fn id_of(&self, path: &Path) -> u32 {
        self.interner.intern(&self.paths.canonical(path))
    }

    fn observations(&self, observed: &Observations) -> Vec<Observation> {
        let sets = [
            (ObservationKind::Read, observed.read()),
            (ObservationKind::Absent, observed.absent()),
            (ObservationKind::Listed, observed.listed()),
        ];
        sets.into_iter()
            .flat_map(|(kind, set)| {
                set.iter().map(move |path| Observation {
                    kind,
                    path: self.id_of(path),
                })
            })
            .collect()
    }

    /// The analysis of a node whose own bytes hash to `content_hash`.
    pub fn analysis(
        &self,
        generation: u64,
        content_hash: [u8; 32],
        edges: Result<DependencyEdges, String>,
        observed: &Observations,
    ) -> Analysis {
        let observations = self.observations(observed);
        let mut analysis = Analysis {
            analysed_gen: generation,
            content_hash,
            completeness: Vec::new(),
            edges: Vec::new(),
            unresolved: Vec::new(),
            observations,
        };
        match edges {
            Err(_) => analysis.completeness.push(degraded_reason::ANALYSIS_FAILED),
            Ok(edges) => self.fill(&mut analysis, &edges),
        }
        analysis
    }

    fn fill(&self, analysis: &mut Analysis, edges: &DependencyEdges) {
        analysis.completeness = edges
            .completeness()
            .reasons()
            .iter()
            .map(|reason| degraded_code(*reason))
            .collect();
        for edge in edges.edges() {
            let Some(kind) = edge_kind(edge.kind) else {
                continue;
            };
            match self.target_path(&edge.target) {
                Some(path) => analysis.edges.push(Edge {
                    kind,
                    target: self.id_of(&path),
                    name: kind.carries_name().then(|| edge.target.key().to_string()),
                }),
                None => analysis.unresolved.push(Unresolved {
                    kind,
                    name: edge.target.key().to_string(),
                    reason: unresolved_reason::UNKNOWABLE,
                }),
            }
        }
        for row in edges.unresolved() {
            if let Some(kind) = edge_kind(row.kind) {
                analysis.unresolved.push(Unresolved {
                    kind,
                    name: row.name.clone(),
                    reason: reason_code(row.reason),
                });
            }
        }
        self.conditional(analysis, edges);
    }

    /// The conditional includes and the names of branches the analysis did not
    /// take, which the compile may take.
    fn conditional(&self, analysis: &mut Analysis, edges: &DependencyEdges) {
        for closure in edges.conditional_include_closure() {
            let reached = closure.path.iter().chain(&closure.reaches);
            for path in reached {
                analysis.edges.push(Edge {
                    kind: EdgeKind::ConditionalInclude,
                    target: self.id_of(path),
                    name: None,
                });
            }
            if closure.path.is_none() {
                let computed = closure.name.contains(['{', '&']);
                analysis.unresolved.push(Unresolved {
                    kind: EdgeKind::ConditionalInclude,
                    name: closure.name.clone(),
                    reason: if computed {
                        unresolved_reason::COMPUTED_NAME
                    } else {
                        unresolved_reason::ABSENT_FROM_WORKSPACE
                    },
                });
            }
            for name in &closure.unresolved {
                analysis.unresolved.push(Unresolved {
                    kind: EdgeKind::ConditionalInclude,
                    name: name.clone(),
                    reason: unresolved_reason::ABSENT_FROM_WORKSPACE,
                });
            }
            for reference in &closure.references {
                self.reference(analysis, reference);
            }
        }
        for reference in edges.conditional_references() {
            let closure_reference = ClosureReference::new(
                reference.kind,
                true,
                reference.name.clone(),
                reference.target.clone(),
            );
            self.reference(analysis, &closure_reference);
        }
    }

    fn reference(&self, analysis: &mut Analysis, reference: &ClosureReference) {
        let kind = match reference.kind {
            IndexEdgeKind::ClassReference => EdgeKind::ConditionalClass,
            IndexEdgeKind::ProgramReference => EdgeKind::ConditionalProgram,
            _ => return,
        };
        let path = reference.target.as_ref().and_then(|t| self.target_path(t));
        match path {
            Some(path) => analysis.edges.push(Edge {
                kind,
                target: self.id_of(&path),
                name: kind.carries_name().then(|| {
                    reference
                        .target
                        .as_ref()
                        .map_or_else(|| reference.name.clone(), |t| t.key().to_string())
                }),
            }),
            None => analysis.unresolved.push(Unresolved {
                kind,
                name: reference.name.clone(),
                reason: unresolved_reason::ABSENT_FROM_WORKSPACE,
            }),
        }
    }

    fn target_path(&self, target: &EdgeTarget) -> Option<PathBuf> {
        match target {
            EdgeTarget::IncludePath(path) => Some(path.clone()),
            EdgeTarget::IndexedFile { file, .. } => self.run.indexed_path(*file),
            EdgeTarget::SchemaTable(_) => None,
        }
    }
}

/// The stored kind of an index edge kind; a schema table is not stored, the
/// key covers each database as a whole.
fn edge_kind(kind: IndexEdgeKind) -> Option<EdgeKind> {
    Some(match kind {
        IndexEdgeKind::DirectInclude => EdgeKind::DirectInclude,
        IndexEdgeKind::TransitiveInclude => EdgeKind::TransitiveInclude,
        IndexEdgeKind::ClassReference => EdgeKind::Class,
        IndexEdgeKind::ProgramReference => EdgeKind::Program,
        IndexEdgeKind::SharedProducer => EdgeKind::SharedProducer,
        IndexEdgeKind::SchemaTable => return None,
    })
}

/// A degraded reason this version does not know counts as a failed analysis.
fn degraded_code(reason: DegradedReason) -> u8 {
    match reason {
        DegradedReason::ParseRecovered => degraded_reason::PARSE_RECOVERED,
        DegradedReason::UnresolvedInclude => degraded_reason::UNRESOLVED_INCLUDE,
        DegradedReason::UnreadableInclude => degraded_reason::UNREADABLE_INCLUDE,
        DegradedReason::PreprocessorLoss => degraded_reason::PREPROCESSOR_LOSS,
        DegradedReason::UnmodelledStatement => degraded_reason::UNMODELLED_STATEMENT,
        _ => degraded_reason::ANALYSIS_FAILED,
    }
}

/// A reason that is not an answer about the workspace counts as unknowable.
fn reason_code(reason: UnresolvedReason) -> u8 {
    match reason {
        UnresolvedReason::AbsentFromWorkspace => unresolved_reason::ABSENT_FROM_WORKSPACE,
        UnresolvedReason::PresentButUnusable => unresolved_reason::PRESENT_BUT_UNUSABLE,
        UnresolvedReason::External => unresolved_reason::EXTERNAL,
        _ => unresolved_reason::UNKNOWABLE,
    }
}
