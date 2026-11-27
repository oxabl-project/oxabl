//! The lexer gives the text of a string literal, a date constant and an
//! include reference as its place in the source, and does not copy or intern
//! it. An interned text goes to the global atom set of the process, which all
//! threads share: on many threads that set is a cost of each literal of each
//! file. So a lex makes one allocation, the token vector, whatever the number
//! of such tokens. All text here is synthetic.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

use oxabl_lexer::{Kind, TokenValue, tokenize};

struct Counting;

static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);

// Implementing `GlobalAlloc` for the allocation-counting test harness requires `unsafe`.
#[allow(unsafe_code)]
// SAFETY: Every method delegates directly to `std::alloc::System` with identical invariants.
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        // SAFETY: Forwarded with caller-upheld `GlobalAlloc::alloc` preconditions.
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        // SAFETY: Forwarded with caller-upheld `GlobalAlloc::dealloc` preconditions.
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        // SAFETY: Forwarded with caller-upheld `GlobalAlloc::alloc_zeroed` preconditions.
        unsafe { System.alloc_zeroed(layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        // SAFETY: Forwarded with caller-upheld `GlobalAlloc::realloc` preconditions.
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// `count` statements, each with a string literal, a date constant and an
/// include reference that no other statement has.
fn source(count: usize) -> String {
    let mut text = String::new();
    for i in 0..count {
        text.push_str(&format!(
            "MESSAGE \"a message of its own, number {i}\":U {}/{}/{}.\n{{inc/part-{i}.i &name=\"value {i}\"}}\n",
            i % 12 + 1,
            i % 28 + 1,
            2000 + i
        ));
    }
    text
}

#[test]
fn a_lex_does_not_allocate_for_each_literal() {
    let count = 2_000;
    let text = source(count);
    let before = ALLOCATIONS.load(Relaxed);
    let tokens = tokenize(&text);
    let allocations = ALLOCATIONS.load(Relaxed) - before;
    assert_eq!(
        allocations, 1,
        "the lex of {count} string literals, dates and include references made \
         {allocations} allocations, not one (the token vector): the lexer copies \
         or interns their text"
    );

    let texts = |kind: Kind| {
        tokens
            .iter()
            .filter(|t| t.kind == kind)
            .map(|t| {
                assert!(
                    matches!(t.value, TokenValue::Text { .. }),
                    "{kind:?} has the value {:?}",
                    t.value
                );
                t.text(&text).expect("the text is in the source")
            })
            .collect::<Vec<_>>()
    };
    let strings = texts(Kind::StringLiteral);
    let dates = texts(Kind::DateLiteral);
    let includes = texts(Kind::IncludeReference);
    assert_eq!(strings.len(), count);
    assert_eq!(dates.len(), count);
    assert_eq!(includes.len(), count);
    assert_eq!(strings[7], "a message of its own, number 7");
    assert_eq!(dates[7], "8/8/2007");
    assert_eq!(includes[7], "inc/part-7.i &name=\"value 7\"");
}
