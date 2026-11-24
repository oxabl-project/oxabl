use crate::store::Analysis;

/// The compile-time edges of every analysed node, in compressed sparse row
/// form: the nodes that the compile of a node can read.
#[derive(Debug, Default)]
pub struct Graph {
    offsets: Vec<u32>,
    targets: Vec<u32>,
}

/// Visited marks for walks over one graph, reused between walks.
pub struct Scratch {
    stamps: Vec<u32>,
    epoch: u32,
}

impl Scratch {
    pub fn new(nodes: usize) -> Self {
        Scratch {
            stamps: vec![0; nodes],
            epoch: 0,
        }
    }

    fn start(&mut self) {
        self.epoch += 1;
    }

    fn visit(&mut self, node: u32) -> bool {
        let slot = &mut self.stamps[node as usize];
        let fresh = *slot != self.epoch;
        *slot = self.epoch;
        fresh
    }
}

impl Graph {
    /// The graph of `analyses` over the path ids below `nodes`.
    pub fn build<'a>(
        nodes: usize,
        analyses: impl Iterator<Item = (u32, &'a Analysis)> + Clone,
    ) -> Self {
        let mut offsets = vec![0u32; nodes + 1];
        for (node, analysis) in analyses.clone() {
            offsets[node as usize + 1] += analysis
                .edges
                .iter()
                .filter(|e| e.kind.is_compile_time())
                .count() as u32;
        }
        for i in 0..nodes {
            offsets[i + 1] += offsets[i];
        }
        let mut targets = vec![0u32; offsets[nodes] as usize];
        let mut fill = offsets.clone();
        for (node, analysis) in analyses {
            for edge in analysis.edges.iter().filter(|e| e.kind.is_compile_time()) {
                let at = &mut fill[node as usize];
                targets[*at as usize] = edge.target;
                *at += 1;
            }
        }
        Graph { offsets, targets }
    }

    /// The number of path ids the graph covers.
    pub fn nodes(&self) -> usize {
        self.offsets.len().saturating_sub(1)
    }

    /// The nodes the compile of `node` reads directly.
    pub fn edges(&self, node: u32) -> &[u32] {
        match self.offsets.get(node as usize..node as usize + 2) {
            Some(&[from, to]) => &self.targets[from as usize..to as usize],
            _ => &[],
        }
    }

    /// Every node the compile of `root` can read, sorted, without `root`.
    pub fn closure(&self, root: u32, scratch: &mut Scratch) -> Vec<u32> {
        scratch.start();
        scratch.visit(root);
        let mut stack = vec![root];
        let mut closure = Vec::new();
        while let Some(node) = stack.pop() {
            for &target in self.edges(node) {
                if scratch.visit(target) {
                    closure.push(target);
                    stack.push(target);
                }
            }
        }
        closure.sort_unstable();
        closure
    }

    pub(super) fn offsets(&self) -> &[u32] {
        &self.offsets
    }

    pub(super) fn targets(&self) -> &[u32] {
        &self.targets
    }
}
