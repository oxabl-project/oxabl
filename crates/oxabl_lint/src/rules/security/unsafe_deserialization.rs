//! `unsafe-deserialization` (ABLSEC014), group `security`.
//!
//! Fires where an object graph is rebuilt from bytes or text that is not a
//! literal. Deserialising data that a caller controls can create objects of
//! any class the session can load, and run their constructors and
//! deserialisation hooks.
//!
//! # The set
//!
//! | Form | Argument read |
//! |---|---|
//! | `serializer:Deserialize(input, ...)` (`Progress.IO.BinarySerializer`, `Progress.IO.JsonSerializer`) | the first argument |
//!
//! ```abl
//! oObj = oSerializer:Deserialize(mData).
//! ```
//!
//! Fixed — read data into a fixed expected type, with a known shape that you
//! check after the read, and never deserialise bytes from an untrusted source
//! into objects:
//!
//! ```abl
//! oJson = CAST(oParser:Parse(lcInput), Progress.Json.ObjectModel.JsonObject).
//! cName = oJson:GetCharacter("name").
//! ```
//!
//! # Class names from data
//!
//! A class name that comes from data (`Progress.Lang.Class:GetClass(cName)`,
//! `DYNAMIC-NEW`) is the same risk from the other side. It belongs to
//! `dynamic-code` (ABLSEC004), which already reports it. This rule does not
//! report those calls, so each finding has one owner.
//!
//! # Where the rule looks
//!
//! The method name is matched by spelling, since the parser has no types: a user
//! method named `Deserialize` on another object is also checked. The rule
//! reads modelled expressions and, by lexing, calls inside statements the
//! parser does not model. A call with no argument or a literal argument is not
//! reported. `READ-JSON` and `READ-XML` fill a dataset or temp-table and are not
//! object deserialisation, so they are not reported.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Site, scan};
use super::{ABLSEC014, finding, meta};
use crate::registry::RuleMeta;

/// Kebab-case rule name.
pub const NAME: &str = "unsafe-deserialization";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str = "objects are rebuilt from data that is not a literal";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Read untrusted data into a fixed expected type, such as a `JsonObject` that you then \
     check field by field, and do not deserialise it into objects. Never let the data choose the \
     class. `dynamic-code` (ABLSEC004) reports class names that come from data.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC014,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    scan(ctx, program, &["Deserialize"], &mut |site| {
        if let Site::Call(call) = site
            && call.is("Deserialize")
            && call.args.first().is_some_and(|a| !a.constant)
        {
            diags.push(finding(
                ctx,
                DEFAULT_SEVERITY,
                ABLSEC014,
                call.span,
                "`Deserialize` rebuilds objects from a non-literal value".to_string(),
                HELP,
            ));
        }
    });
    diags
}
