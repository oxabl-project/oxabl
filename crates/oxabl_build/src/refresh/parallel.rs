use std::sync::atomic::{AtomicUsize, Ordering};

const CHUNK: usize = 64;

/// The number of worker threads for a pass over `items` items.
pub fn threads_for(items: usize) -> usize {
    let cores = std::thread::available_parallelism().map_or(4, |n| n.get());
    cores.min(items.div_ceil(CHUNK)).max(1)
}

/// Apply `work` to every item on scoped threads and return the results in the
/// order of `items`.
pub fn map<T: Sync, R: Send>(items: &[T], work: impl Fn(&T) -> R + Sync) -> Vec<R> {
    let threads = threads_for(items.len());
    if threads == 1 {
        return items.iter().map(work).collect();
    }
    let next = AtomicUsize::new(0);
    let mut parts: Vec<(usize, Vec<R>)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                scope.spawn(|| {
                    let mut mine = Vec::new();
                    loop {
                        let start = next.fetch_add(CHUNK, Ordering::Relaxed);
                        if start >= items.len() {
                            break;
                        }
                        let end = (start + CHUNK).min(items.len());
                        mine.push((start, items[start..end].iter().map(&work).collect()));
                    }
                    mine
                })
            })
            .collect();
        workers
            .into_iter()
            .flat_map(|worker| match worker.join() {
                Ok(parts) => parts,
                Err(panic) => std::panic::resume_unwind(panic),
            })
            .collect()
    });
    parts.sort_unstable_by_key(|(start, _)| *start);
    parts.into_iter().flat_map(|(_, results)| results).collect()
}
