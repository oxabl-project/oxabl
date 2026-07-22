//! ABL style guide — typed, configurable formatting and diagnostic rules.
//!
//! Provides a single [`StyleGuide`] struct holding all rules from the
//! oestandards and consultingwerk ABL coding standards. Each rule carries
//! a [`Scope`] tagging whether it can be mechanically enforced by a
//! formatter ([`Scope::Formatting`]) or requires semantic analysis
//! ([`Scope::Diagnostic`]).
//!
//! Two named presets are available as first-class constructors:
//! - [`StyleGuide::oestandards`] — alextrs/oestandards profile
//! - [`StyleGuide::consultingwerk`] — consultingwerk/ABL-Coding-Standards profile
//!
//! User customizations are supported via TOML deserialization with partial
//! overrides. Missing fields fall back to [`StyleGuide::default_base`] via
//! `#[serde(default)]` + `Default`.
//!
//! # Example
//!
//! ```ignore
//! use oxabl_style::{StyleGuide, Scope};
//!
//! let guide = StyleGuide::oestandards();
//! assert_eq!(guide.keyword_case, oxabl_style::KeywordCase::Uppercase);
//! assert_eq!(StyleGuide::scope("keyword_case"), Some(Scope::Formatting));
//! assert_eq!(StyleGuide::scope("variable_case"), Some(Scope::Diagnostic));
//! assert_eq!(StyleGuide::scope("nonexistent"), None);
//! ```

mod rules;
mod style_guide;

pub use rules::{
    AndOrPlacement, BufferNaming, ClassNaming, CommentStyle, FileNameCasing, IndentStyle,
    KeywordAbbreviation, KeywordCase, MethodCase, ParameterPrefix, PeriodPlacement, Placement,
    StaticMemberRef, SubstitutePolicy, TempTablePrefix, VariableCase, VariableDeclAlignment,
};
pub use style_guide::{Scope, StyleGuide};
