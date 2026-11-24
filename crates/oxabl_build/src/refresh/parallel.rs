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
    map_with(items, || (), |(), item| work(item))
}

/// Like [`map`], with state that each thread makes once with `init` and hands
/// to every call it makes.
pub fn map_with<T: Sync, S, R: Send>(
    items: &[T],
    init: impl Fn() -> S + Sync,
    work: impl Fn(&mut S, &T) -> R + Sync,
) -> Vec<R> {
    let threads = threads_for(items.len());
    if threads == 1 {
        let mut state = init();
        return items.iter().map(|item| work(&mut state, item)).collect();
    }
    let next = AtomicUsize::new(0);
    let mut parts: Vec<(usize, Vec<R>)> = std::thread::scope(|scope| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                oxabl_common::stack::builder()
                    .spawn_scoped(scope, || {
                        let mut state = init();
                        let mut mine = Vec::new();
                        loop {
                            let start = next.fetch_add(CHUNK, Ordering::Relaxed);
                            if start >= items.len() {
                                break;
                            }
                            let end = (start + CHUNK).min(items.len());
                            let chunk = items[start..end].iter().map(|item| work(&mut state, item));
                            mine.push((start, chunk.collect()));
                        }
                        mine
                    })
                    .expect("failed to spawn an analysis thread")
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
