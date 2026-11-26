//! Counts of costly steps, kept only with the test-only `test-panics` feature,
//! so a test can see that a step ran as often as it should.

#[cfg(feature = "test-panics")]
static SCHEMA_FILES_PARSED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Note that a schema (`.df`) file was parsed.
pub fn schema_file_parsed() {
    #[cfg(feature = "test-panics")]
    SCHEMA_FILES_PARSED.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// How many schema files this process parsed, or `None` without the test-only
/// `test-panics` feature.
#[doc(hidden)]
pub fn schema_files_parsed() -> Option<usize> {
    #[cfg(feature = "test-panics")]
    return Some(SCHEMA_FILES_PARSED.load(std::sync::atomic::Ordering::Relaxed));
    #[cfg(not(feature = "test-panics"))]
    None
}
