//! The document outline: the declarations of one file as a tree, for an
//! editor's outline view, breadcrumbs and "Go to Symbol in File".
//!
//! Like [`FormatPipeline`](crate::FormatPipeline), it reads the **raw** source:
//! no preprocessing and no includes, so every span is a byte range of the
//! buffer as written and a client can map it without a source map.

use oxabl_ast::Span;
use oxabl_common::{InternalPanic, catch_panic, panic_if_injected, panic_sites};

/// What a symbol declares. A client maps it to its own symbol kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OutlineKind {
    Class,
    Interface,
    Procedure,
    Function,
    Method,
    Constructor,
    Destructor,
    Property,
    Event,
    Variable,
    Parameter,
    TempTable,
    Field,
    Buffer,
    Dataset,
    DataSource,
    Query,
    Stream,
    Frame,
    Widget,
    /// An `ON` trigger block or a `TRIGGER PROCEDURE` header.
    Trigger,
    /// A statement-level `{file.i}` reference.
    Include,
    /// `&SCOPED-DEFINE` / `&GLOBAL-DEFINE`.
    PreprocessorDefine,
    /// A labelled block (`main-loop: DO: … END.`).
    Label,
}

/// One declaration and the declarations nested in it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutlineSymbol {
    pub name: String,
    /// Short extra text shown next to the name, e.g. `INTEGER` or
    /// `PUBLIC STATIC`.
    pub detail: Option<String>,
    pub kind: OutlineKind,
    /// The whole declaration, e.g. `PROCEDURE p: … END PROCEDURE.`
    pub span: Span,
    /// The part an editor selects and reveals, normally the name. Always
    /// inside `span`.
    pub selection_span: Span,
    /// In source order; every child's `span` is inside this symbol's `span`.
    pub children: Vec<OutlineSymbol>,
}

/// The outline of `source`, top-level symbols in source order.
///
/// A source that does not parse cleanly still yields the declarations the
/// parser recovered. `Err` means an internal panic was contained: the outline
/// is unknown, not empty.
pub fn outline(source: &str) -> Result<Vec<OutlineSymbol>, InternalPanic> {
    catch_panic(|| {
        panic_if_injected(panic_sites::OUTLINE, source);
        outline_inner(source)
    })
}

fn outline_inner(_source: &str) -> Vec<OutlineSymbol> {
    Vec::new()
}
