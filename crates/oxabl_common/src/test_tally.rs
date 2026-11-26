//! Counts of named events for each labelled piece of work, in test builds
//! only (the `test-panics` feature). A test reads them to see that a pass does
//! a piece of work once and not again, such as one expansion of each file.
//!
//! Without the feature, [`within`] only runs its closure and [`count`] does
//! nothing.

#[cfg(feature = "test-panics")]
mod armed {
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::sync::Mutex;

    thread_local! {
        pub(super) static LABEL: RefCell<Option<String>> = const { RefCell::new(None) };
    }

    pub(super) static COUNTS: Mutex<BTreeMap<(String, &'static str), usize>> =
        Mutex::new(BTreeMap::new());
}

/// Run `work` with the events it [`count`]s on this thread charged to the
/// label `label` makes. The label is made only in a test build.
#[inline]
pub fn within<T>(label: impl FnOnce() -> String, work: impl FnOnce() -> T) -> T {
    #[cfg(feature = "test-panics")]
    {
        let outer = armed::LABEL.with(|current| current.replace(Some(label())));
        let result = work();
        armed::LABEL.with(|current| *current.borrow_mut() = outer);
        result
    }
    #[cfg(not(feature = "test-panics"))]
    {
        let _ = label;
        work()
    }
}

/// Count one `event` for the label of the enclosing [`within`] on this thread.
/// Outside one, nothing is counted.
#[inline]
pub fn count(event: &'static str) {
    #[cfg(feature = "test-panics")]
    armed::LABEL.with(|current| {
        if let Some(label) = current.borrow().as_ref() {
            *armed::COUNTS
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner())
                .entry((label.clone(), event))
                .or_default() += 1;
        }
    });
    #[cfg(not(feature = "test-panics"))]
    let _ = event;
}

/// Every count, as (label, event, count) in label order, or `None` without
/// the test-only feature.
pub fn counts() -> Option<Vec<(String, &'static str, usize)>> {
    #[cfg(feature = "test-panics")]
    return Some(
        armed::COUNTS
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .iter()
            .map(|((label, event), n)| (label.clone(), *event, *n))
            .collect(),
    );
    #[cfg(not(feature = "test-panics"))]
    None
}
