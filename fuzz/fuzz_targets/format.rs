#![no_main]

use libfuzzer_sys::fuzz_target;
use oxabl_formatter::{FormatFailure, try_format_source};
use oxabl_style::StyleGuide;

// Formatting never panics, and formatting its own output changes nothing.
fuzz_target!(|data: &[u8]| {
    // On the analysis stack, as in the CLI and the daemon: libFuzzer's own
    // thread is too small for the deepest tree the parser accepts.
    oxabl_common::stack::run(|| check(data));
});

fn check(data: &[u8]) {
    let source = String::from_utf8_lossy(data);
    let style = StyleGuide::default_base();
    let once = match try_format_source(&source, &style) {
        Ok(out) => out,
        Err(FormatFailure::Bail(_)) => return,
        Err(FormatFailure::Panic(p)) => panic!("formatter panicked: {p:?}"),
        Err(_) => return,
    };
    match try_format_source(&once, &style) {
        Ok(twice) => assert_eq!(once, twice, "formatting is not stable"),
        Err(e) => panic!("the formatted output no longer formats: {e:?}"),
    }
}
