mod blank_lines;
mod diagnostic;
mod file_id;
mod file_span;
mod lint_severity;
mod panic_guard;
mod source_map;
pub mod stack;
#[doc(hidden)]
pub mod test_tally;
mod virtual_span;

pub use blank_lines::blank_lines_between;
pub use diagnostic::{
    Diagnostic, DiagnosticCode, Label, Severity, SourceResolver, render_diagnostics,
};
pub use file_id::{FileId, FileSet};
pub use file_span::FileSpan;
pub use lint_severity::{LintLayer, LintRuleRef, LintSeverityMap};
pub use panic_guard::{
    HOLD_TIME, INJECTION_ENABLED, InternalPanic, TEST_ABORT_MARKER, TEST_HOLD_MARKER,
    TEST_LIMIT_MARKER, TEST_PANIC_MARKER, abort_if_injected, catch_panic, hold_if_injected,
    hold_if_injected_with, injected_limit, panic_if_injected, panic_sites,
};
pub use source_map::SourceMap;
pub use virtual_span::VirtualSpan;
