//! Hand-built ASTs for the rule tests. Names and expressions go into one
//! pool for each test thread, and [`with_ctx`] lends that pool to the
//! analysis, as `oxabl_parser::Program::pools` does for a parsed file.

use std::cell::RefCell;

use oxabl_ast::{AstPools, ExprId, ExprList, Expression, NameId};
use oxabl_common::FileId;
use oxabl_schema::Schema;
use oxabl_semantic::AnalysisContext;

thread_local! {
    static POOLS: RefCell<AstPools> = const { RefCell::new(AstPools::new()) };
}

/// Store `text` and return its handle.
pub(crate) fn name(text: &str) -> NameId {
    POOLS.with(|pools| pools.borrow_mut().names.push(text))
}

/// Store `expression` and return its handle.
pub(crate) fn expr(expression: Expression) -> ExprId {
    POOLS.with(|pools| pools.borrow_mut().exprs.push(expression))
}

/// Store `items` as one list and return its handle.
pub(crate) fn list(items: &[ExprId]) -> ExprList {
    POOLS.with(|pools| pools.borrow_mut().exprs.push_list(items))
}

/// Run `f` with a context over `source` and `schema` that reads this thread's
/// pool. Build the tree first: `f` cannot store names or expressions.
pub(crate) fn with_ctx<R>(
    source: &str,
    schema: &Schema,
    f: impl FnOnce(&mut AnalysisContext<'_>) -> R,
) -> R {
    POOLS.with(|pools| {
        let pools = pools.borrow();
        let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema, &pools);
        f(&mut ctx)
    })
}
