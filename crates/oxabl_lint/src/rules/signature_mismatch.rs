//! `signature-mismatch` lint (CL003), group `strict.calls`.
//!
//! Fires on a static `RUN` whose arguments do not fit the parameters of the
//! procedure it runs: a different count, another kind or mode, or a handle
//! where a value of another type is expected.
//!
//! # Why
//!
//! ```abl
//! PROCEDURE total-order:
//!     DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
//!     DEFINE OUTPUT PARAMETER dTotal AS DECIMAL NO-UNDO.
//! END PROCEDURE.
//! DEFINE VARIABLE dTotal AS DECIMAL NO-UNDO.
//! RUN total-order (INPUT 1, dTotal).
//! ```
//!
//! `dTotal` is passed as an `INPUT` argument where the procedure writes an
//! `OUTPUT` parameter. A procedure in the same file is checked when the file
//! compiles. A program run from another file is checked only when the call
//! runs, so a signature that changed after the caller was written fails in
//! production.
//!
//! Fixed:
//!
//! ```abl
//! PROCEDURE total-order:
//!     DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
//!     DEFINE OUTPUT PARAMETER dTotal AS DECIMAL NO-UNDO.
//! END PROCEDURE.
//! DEFINE VARIABLE dTotal AS DECIMAL NO-UNDO.
//! RUN total-order (INPUT 1, OUTPUT dTotal).
//! ```
//!
//! # What is and is not reported
//!
//! The callee must be known whole. A `RUN` is checked when it runs, without
//! `ASYNCHRONOUS` or `ON SERVER`:
//!
//! - an internal procedure of the file, also `IN THIS-PROCEDURE`;
//! - a program found on the search paths (`RUN post-order.p`, also
//!   `PERSISTENT`): its parameters are the `DEFINE ... PARAMETER` statements of
//!   its main block;
//! - a procedure of a program the file runs `PERSISTENT SET h`, called `IN h`
//!   when every `PERSISTENT SET h` of the file runs one program.
//!
//! Per `RUN`, a count that differs gives one diagnostic on the target. With
//! the same count, each argument gives at most one diagnostic, the first that
//! applies:
//!
//! 1. the kind: a `TABLE`, `TABLE-HANDLE`, `DATASET`, `DATASET-HANDLE`,
//!    `BUFFER` or plain argument where the parameter is another kind. A
//!    `TABLE` and a `TABLE-HANDLE` fit each other, as do a `DATASET` and a
//!    `DATASET-HANDLE`;
//! 2. the mode: the argument's `INPUT`, `OUTPUT` or `INPUT-OUTPUT` (`INPUT`
//!    when none is written) differs from the parameter's mode. A `BUFFER`
//!    parameter has no mode;
//! 3. an expression (a literal, an operator, `IF ... THEN ... ELSE`, `NEW`)
//!    passed as `OUTPUT` or `INPUT-OUTPUT`;
//! 4. the data type, only where a handle meets a type it has no conversion
//!    to or from: a `HANDLE` or `COM-HANDLE` on one side and, on the other, a
//!    type other than `HANDLE`, `COM-HANDLE`, `INTEGER`, `INT64`, `CHARACTER`
//!    or `LONGCHAR`. An `INPUT` value goes from the argument to the
//!    parameter, an `OUTPUT` value back, and an `INPUT-OUTPUT` value both
//!    ways. Every other pair of types may convert when the call runs, so it
//!    gives no finding: production code passes an `INTEGER` field to a
//!    `CHARACTER` parameter, and a `CHARACTER` value to a `DECIMAL` one.
//!
//! A callee whose parameter list an `{include}` or `&IF` supplies is read from
//! the expanded text, and another compile environment (another search path or
//! `&GLOBAL-DEFINE`, such as a character and a graphical build of one program)
//! may expand it to another list. A call with no text from an include inside
//! it (`RUN p ({globals-out.i}, x)` has some) is then not judged on the
//! count. Kind, mode and type are still compared when the counts agree.
//!
//! Never reported, because the callee or an argument is Unknown:
//!
//! - a target the call graph does not resolve (`RUN VALUE(...)`, a handle
//!   the file does not set once, a program not on the search paths, a name
//!   several files match, a file that does not parse);
//! - a callee whose parameter list an `{include}` that did not arrive, or an
//!   `&IF` that holds a parameter, may change;
//! - a parameter or argument of unknown type: `LIKE`, a class, an array
//!   (`EXTENT`), an unresolved name, the unknown value `?`;
//! - a `RUN` whose argument list holds an include reference or a gap the
//!   preprocessor left;
//! - in an include checked alone, and in a file with an include that did not
//!   arrive, a bare name that may be a procedure of the includer.
//!
//! The `TABLE` parameter's temp-table definition and the `BIND`, `APPEND` and
//! `BY-REFERENCE` options are not compared.

use oxabl_ast::{
    Expression, ExpressionKind, ParameterDirection, RunArgument, RunTarget, Span, Statement,
    StatementKind,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_index::calls::{CallKind, CallTarget};
use oxabl_lexer::{Kind, Token};
use oxabl_semantic::{
    AnalysisContext, IndexName, Param, ParamMode, ParamShape, PrimitiveTy, ProgramSignatures,
    ResolvedType, Semantic, Signature,
};
use std::sync::Arc;

use super::CL003;
use super::call_support::{for_each_run, holds_elided, is_bare};
use super::syntactic::file_span;
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "signature-mismatch";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A RUN whose arguments differ from the callee's parameters in count, kind, mode or type";

/// Entry point without shared facts.
pub fn run(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, sem, ctx, &RunShared::default())
}

/// Shared entry point.
pub fn check(
    program: &[Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
    shared: &RunShared,
) -> Vec<Diagnostic> {
    if ctx.parse_recovered {
        return Vec::new();
    }
    let tokens = shared.tokens(ctx);
    let fragment = ctx.source_context.is_include_fragment();
    let mut local: Option<ProgramSignatures> = None;
    let mut diags = Vec::new();
    for_each_run(program, shared.calls(program, ctx), |stmt, site| {
        if !matches!(site.kind, CallKind::Run | CallKind::Persistent) {
            return;
        }
        let StatementKind::Run {
            target: RunTarget::Literal { name_span, .. },
            arguments,
            ..
        } = &stmt.kind
        else {
            return;
        };
        let signature: Option<Arc<Signature>> = match &site.target {
            CallTarget::Procedure(name) => {
                if (fragment && is_bare(name)) || !ctx.elided_includes.is_empty() {
                    return;
                }
                let sigs = local.get_or_insert_with(|| ProgramSignatures::of(program, tokens));
                sigs.procedure(&IndexName::new(name)).cloned().map(Arc::new)
            }
            CallTarget::Program(name) => {
                if !ctx.index_loaded
                    || (is_bare(name) && (fragment || !ctx.elided_includes.is_empty()))
                {
                    return;
                }
                ctx.index.signature(&IndexName::new(name), None)
            }
            CallTarget::InProgram { program, procedure } => {
                if !ctx.index_loaded {
                    return;
                }
                ctx.index
                    .signature(&IndexName::new(program), Some(&IndexName::new(procedure)))
            }
            _ => None,
        };
        let Some(signature) = signature else {
            return;
        };
        if holds_elided(ctx, stmt.span) || arguments.iter().any(hides_arguments) {
            return;
        }
        let name = match &site.target {
            CallTarget::Procedure(n) | CallTarget::Program(n) => n.as_str(),
            CallTarget::InProgram { procedure, .. } => procedure.as_str(),
            _ => "",
        };
        compare(
            &Callee {
                name,
                target: *name_span,
                signature: &signature,
                include_in_call: include_in_call(ctx, stmt.span),
            },
            arguments,
            sem,
            ctx,
            tokens,
            &mut diags,
        );
    });
    diags
}

/// Whether an argument is text the parser cannot count: an include or a
/// preprocessor reference.
fn hides_arguments(arg: &RunArgument) -> bool {
    matches!(
        arg.expression.kind,
        ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_)
            | ExpressionKind::PreprocIf(_)
    )
}

struct Callee<'a> {
    name: &'a str,
    /// Where the call names its target.
    target: Span,
    signature: &'a Signature,
    /// An include file wrote part of the call's text.
    include_in_call: bool,
}

fn compare(
    callee: &Callee,
    arguments: &[RunArgument],
    sem: &Semantic,
    ctx: &AnalysisContext,
    tokens: &[Token],
    diags: &mut Vec<Diagnostic>,
) {
    let params = &callee.signature.params;
    let name = callee.name;
    if arguments.len() != params.len() {
        // An include supplies some of the callee's parameters and the call names
        // none from an include: another environment compiles the callee to
        // another list, so the count proves nothing.
        if callee.signature.from_include && !callee.include_in_call {
            return;
        }
        diags.push(report(
            ctx,
            callee.target,
            format!(
                "`RUN {name}` passes {} {}, but the procedure takes {}",
                arguments.len(),
                plural(arguments.len(), "argument"),
                params.len()
            ),
            "Pass one argument for each `DEFINE ... PARAMETER`, in order.",
        ));
        return;
    }
    for (i, (arg, param)) in arguments.iter().zip(params).enumerate() {
        let n = i + 1;
        let span = arg.expression.span;
        let shape = argument_shape(tokens, &arg.expression);
        if !shapes_fit(shape, param.shape) {
            diags.push(report(
                ctx,
                span,
                format!(
                    "argument {n} of `RUN {name}` is {}, but the parameter is {}",
                    shape_name(shape),
                    shape_name(param.shape)
                ),
                "Pass the argument in the form the parameter is defined in.",
            ));
            continue;
        }
        let mode = argument_mode(&arg.direction);
        if param.shape != ParamShape::Buffer && mode != param.mode {
            diags.push(report(
                ctx,
                span,
                format!(
                    "argument {n} of `RUN {name}` is passed as {}, but the parameter is {}",
                    mode_name(mode),
                    mode_name(param.mode)
                ),
                "Write the argument's mode to match the parameter: `INPUT`, `OUTPUT` or `INPUT-OUTPUT`.",
            ));
            continue;
        }
        if mode != ParamMode::Input && is_expression(&arg.expression) {
            diags.push(report(
                ctx,
                span,
                format!(
                    "argument {n} of `RUN {name}` is an expression, but {} needs a variable or field the procedure can write",
                    mode_name(mode)
                ),
                "Pass a variable, field or array element.",
            ));
            continue;
        }
        if let Some(message) = type_mismatch(sem, arg, mode, param, n, name) {
            diags.push(report(
                ctx,
                span,
                message,
                "Pass a value of the parameter's type, or change the parameter.",
            ));
        }
    }
}

/// Whether an include file wrote part of the text at `span` and not all of
/// it: `RUN p ({globals-out.i}, x)`. A statement that an include holds
/// whole does not count.
fn include_in_call(ctx: &AnalysisContext, span: Span) -> bool {
    let from = ctx.include_regions.partition_point(|r| r.end <= span.start);
    ctx.include_regions[from..]
        .iter()
        .take_while(|r| r.start < span.end)
        .any(|r| !(r.start <= span.start && r.end >= span.end))
}

fn report(ctx: &AnalysisContext, span: Span, message: String, help: &str) -> Diagnostic {
    Diagnostic::warning(CL003, message, file_span(ctx, span)).with_help(help.to_string())
}

fn plural(n: usize, word: &str) -> String {
    if n == 1 {
        word.to_string()
    } else {
        format!("{word}s")
    }
}

/// What the argument is, read from the keyword that precedes or starts it: the
/// parser keeps no record of `TABLE`, `TABLE-HANDLE`, `DATASET`,
/// `DATASET-HANDLE` or `BUFFER` on an argument.
fn argument_shape(tokens: &[Token], expr: &Expression) -> ParamShape {
    let start = expr.span.start as usize;
    let at = tokens.partition_point(|t| t.start < start);
    // `BUFFER b:HANDLE` is a handle value, not a buffer argument.
    let plain = matches!(expr.kind, ExpressionKind::Identifier(_));
    if plain
        && tokens
            .get(at)
            .is_some_and(|t| t.start == start && t.kind == Kind::Buffer)
    {
        return ParamShape::Buffer;
    }
    match at.checked_sub(1).map(|p| tokens[p].kind) {
        Some(Kind::Table) => ParamShape::Table,
        Some(Kind::TableHandle) => ParamShape::TableHandle,
        Some(Kind::Dataset) => ParamShape::Dataset,
        Some(Kind::DatasetHandle) => ParamShape::DatasetHandle,
        _ => ParamShape::Scalar(None),
    }
}

fn shapes_fit(arg: ParamShape, param: ParamShape) -> bool {
    use ParamShape::*;
    match (arg, param) {
        (Scalar(_), Scalar(_)) => true,
        (Table | TableHandle, Table | TableHandle) => true,
        (Dataset | DatasetHandle, Dataset | DatasetHandle) => true,
        (a, p) => a == p,
    }
}

fn shape_name(shape: ParamShape) -> &'static str {
    match shape {
        ParamShape::Scalar(_) => "a plain variable or value",
        ParamShape::Buffer => "a `BUFFER`",
        ParamShape::Table => "a `TABLE`",
        ParamShape::TableHandle => "a `TABLE-HANDLE`",
        ParamShape::Dataset => "a `DATASET`",
        ParamShape::DatasetHandle => "a `DATASET-HANDLE`",
    }
}

fn argument_mode(direction: &ParameterDirection) -> ParamMode {
    match direction {
        ParameterDirection::Output => ParamMode::Output,
        ParameterDirection::InputOutput => ParamMode::InputOutput,
        ParameterDirection::Input | ParameterDirection::Return => ParamMode::Input,
    }
}

fn mode_name(mode: ParamMode) -> &'static str {
    match mode {
        ParamMode::Input => "`INPUT`",
        ParamMode::Output => "`OUTPUT`",
        ParamMode::InputOutput => "`INPUT-OUTPUT`",
    }
}

/// Whether `expr` is a value that cannot be written to.
fn is_expression(expr: &Expression) -> bool {
    use ExpressionKind::*;
    matches!(
        expr.kind,
        Literal(_)
            | Add(..)
            | Minus(..)
            | Multiply(..)
            | Divide(..)
            | Modulo(..)
            | Equal(..)
            | NotEqual(..)
            | LessThan(..)
            | LessThanOrEqual(..)
            | GreaterThan(..)
            | GreaterThanOrEqual(..)
            | Begins(..)
            | Matches(..)
            | Contains(..)
            | And(..)
            | Or(..)
            | Negate(_)
            | Not(_)
            | IfThenElse(..)
            | New { .. }
    )
}

/// The message for an argument whose primitive type cannot convert to the
/// parameter's, or `None` when it may or either side is not known.
fn type_mismatch(
    sem: &Semantic,
    arg: &RunArgument,
    mode: ParamMode,
    param: &Param,
    n: usize,
    name: &str,
) -> Option<String> {
    let ParamShape::Scalar(Some(want)) = param.shape else {
        return None;
    };
    let Some(ResolvedType::Primitive(have)) = sem.types.get(arg.expression.id) else {
        return None;
    };
    let have = *have;
    // A value goes from the argument to the parameter for `INPUT`, back for
    // `OUTPUT`, and both ways for `INPUT-OUTPUT`.
    let clash = match mode {
        ParamMode::Input | ParamMode::Output => handle_clash(have, want),
        ParamMode::InputOutput => handle_clash(have, want) || handle_clash(want, have),
    };
    clash.then(|| {
        format!(
            "argument {n} of `RUN {name}` is {}, but the parameter is {}",
            type_name(have),
            type_name(want)
        )
    })
}

/// Whether a handle meets a type it has no conversion to or from. Every other
/// pair of scalar types may convert when the call runs: production code passes
/// an `INTEGER` field to a `CHARACTER` parameter, and a `CHARACTER` value to a
/// `DECIMAL` one.
fn handle_clash(a: PrimitiveTy, b: PrimitiveTy) -> bool {
    use PrimitiveTy::*;
    let handle = |t| matches!(t, Handle | ComHandle);
    let converts_with_handle = |t| {
        matches!(
            t,
            Handle | ComHandle | Integer | Int64 | Character | Longchar
        )
    };
    (handle(a) || handle(b)) && !(converts_with_handle(a) && converts_with_handle(b))
}

fn type_name(ty: PrimitiveTy) -> &'static str {
    match ty {
        PrimitiveTy::Integer => "INTEGER",
        PrimitiveTy::Int64 => "INT64",
        PrimitiveTy::Decimal => "DECIMAL",
        PrimitiveTy::Character => "CHARACTER",
        PrimitiveTy::Longchar => "LONGCHAR",
        PrimitiveTy::Logical => "LOGICAL",
        PrimitiveTy::Date => "DATE",
        PrimitiveTy::Datetime => "DATETIME",
        PrimitiveTy::DatetimeTz => "DATETIME-TZ",
        PrimitiveTy::Handle => "HANDLE",
        PrimitiveTy::Rowid => "ROWID",
        PrimitiveTy::Recid => "RECID",
        PrimitiveTy::Raw => "RAW",
        PrimitiveTy::Memptr => "MEMPTR",
        PrimitiveTy::Clob => "CLOB",
        PrimitiveTy::Blob => "BLOB",
        PrimitiveTy::ComHandle => "COM-HANDLE",
    }
}
