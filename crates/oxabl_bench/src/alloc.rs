//! A counting global allocator over the system allocator.
//!
//! A bench or test binary installs it with
//! `#[global_allocator] static A: Counting = Counting;`. [`measure`] then gives
//! the allocations, the bytes requested and the peak live bytes of one call.
//! The counters are global, so measure on one thread at a time.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

/// The allocator. It counts and forwards every call to [`System`].
pub struct Counting;

static ALLOCATIONS: AtomicU64 = AtomicU64::new(0);
static BYTES: AtomicU64 = AtomicU64::new(0);
static LIVE: AtomicU64 = AtomicU64::new(0);
static PEAK: AtomicU64 = AtomicU64::new(0);

fn grow(by: u64) {
    let live = LIVE.fetch_add(by, Relaxed) + by;
    PEAK.fetch_max(live, Relaxed);
}

// SAFETY: every call forwards to `System` with the caller's arguments.
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        BYTES.fetch_add(layout.size() as u64, Relaxed);
        grow(layout.size() as u64);
        unsafe { System.alloc(layout) }
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        BYTES.fetch_add(layout.size() as u64, Relaxed);
        grow(layout.size() as u64);
        unsafe { System.alloc_zeroed(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size() as u64, Relaxed);
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        BYTES.fetch_add(new_size as u64, Relaxed);
        LIVE.fetch_sub(layout.size() as u64, Relaxed);
        grow(new_size as u64);
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

/// What one call allocated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Usage {
    /// Calls of `alloc`, `alloc_zeroed` and `realloc`.
    pub allocations: u64,
    /// Bytes requested by those calls.
    pub bytes: u64,
    /// The most bytes live at one time during the call, above the live bytes
    /// at its start.
    pub peak: u64,
    /// Bytes still live after the call, above the live bytes at its start.
    /// Negative when the call freed more than it allocated.
    pub retained: i64,
}

/// Whether [`Counting`] is the global allocator of this binary: it is when an
/// allocation moves its counter.
pub fn installed() -> bool {
    let before = ALLOCATIONS.load(Relaxed);
    drop(std::hint::black_box(Box::new(0u64)));
    ALLOCATIONS.load(Relaxed) != before
}

/// Run `f` once and count what it allocates.
///
/// # Panics
///
/// When [`Counting`] is not the global allocator, because every count would
/// then read as zero.
pub fn measure<T>(f: impl FnOnce() -> T) -> (T, Usage) {
    assert!(
        installed(),
        "oxabl_bench::alloc::Counting is not the global allocator of this binary"
    );
    let allocations = ALLOCATIONS.load(Relaxed);
    let bytes = BYTES.load(Relaxed);
    let live = LIVE.load(Relaxed);
    PEAK.store(live, Relaxed);
    let value = f();
    let usage = Usage {
        allocations: ALLOCATIONS.load(Relaxed) - allocations,
        bytes: BYTES.load(Relaxed) - bytes,
        peak: PEAK.load(Relaxed).saturating_sub(live),
        retained: LIVE.load(Relaxed) as i64 - live as i64,
    };
    (value, usage)
}
