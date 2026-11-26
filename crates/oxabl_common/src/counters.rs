//! Counters a black-box test reads from a test build of the binary. They count
//! only with the test-only `test-panics` feature, which only a dev-dependency
//! turns on.

#[cfg(feature = "test-panics")]
static EXPANSION_PARSES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Count one parse that decides whether a file may need its text expanded.
#[inline]
#[doc(hidden)]
pub fn count_expansion_parse() {
    #[cfg(feature = "test-panics")]
    EXPANSION_PARSES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// How many parses of that kind this process ran, or `None` without the
/// `test-panics` feature.
#[doc(hidden)]
pub fn expansion_parses() -> Option<usize> {
    #[cfg(feature = "test-panics")]
    return Some(EXPANSION_PARSES.load(std::sync::atomic::Ordering::Relaxed));
    #[cfg(not(feature = "test-panics"))]
    None
}
