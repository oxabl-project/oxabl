//! Calls of a built-in method, and `NEW` of a built-in class, that no overload
//! can take (`member-call-mismatch`). See `docs/design/builtin-classes.md`,
//! "When the new rules stay silent": one fact that oxabl does not know gives
//! no answer.

use oxabl_ast::{Expression, ExpressionKind, Span};
use smallvec::SmallVec;

use crate::builtin::{BuiltinClass, BuiltinMemberKind, BuiltinShape, BuiltinType};
use crate::coercion::ClassLattice;
use crate::overload::{Candidate, NoFit, candidates_of, no_fit};
use crate::signature::{CallParam, CallParamKind, ParamMode};
use crate::symbol::BuiltinMemberRef;
use crate::{AnalysisContext, Resolution, ResolvedType, Semantic, SymbolTable};

/// A call that no overload of a built-in member or constructor can take.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallMisfit {
    /// The built-in type of the receiver, or the class of the `NEW`.
    pub class: &'static BuiltinClass,
    /// The method as documented; `None` for a `NEW`.
    pub member: Option<&'static str>,
    pub kind: MisfitKind,
    /// The whole call or `NEW` expression.
    pub span: Span,
}

/// Why no overload takes the call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MisfitKind {
    /// No overload has this number of arguments.
    Count(usize),
    /// Each overload with this number of arguments has one whose type never
    /// converts; the argument types, in order.
    Types(Vec<ResolvedType>),
}

/// The misfit of the call or `NEW` at `expr`, when every fact it needs is
/// known: the receiver or class is a built-in type the catalog has whole, no
/// argument is an include or a preprocessor reference that did not expand,
/// and the file is not an include analysed alone.
pub fn builtin_call_misfit(
    sem: &Semantic,
    ctx: &AnalysisContext,
    expr: &Expression,
) -> Option<CallMisfit> {
    if ctx.source_context.is_include_fragment() || has_unexpanded_text(ctx, expr) {
        return None;
    }
    match &expr.kind {
        ExpressionKind::MethodCall {
            object, arguments, ..
        } => {
            let Some(Resolution::Resolved(anchor)) = sem.references.get(expr.id) else {
                return None;
            };
            let class = receiver_builtin(sem, object)?;
            if !class.is_complete() {
                return None;
            }
            let ids = candidates_of(&sem.symbols, *anchor);
            let mut member = None;
            for &id in &ids {
                match sem.symbols.builtin_member(id) {
                    Some(BuiltinMemberRef::Member(m)) => member = member.or(Some(m.name())),
                    _ => return None,
                }
            }
            let candidates: SmallVec<[Candidate<'_>; 8]> = ids
                .iter()
                .map(|&id| Candidate {
                    ret: sem.symbols.get(id).data_type.as_ref(),
                    params: sem.symbols.method_params(id),
                    modes_known: !sem.symbols.modes_unknown(id),
                })
                .collect();
            let args = argument_types(sem, arguments);
            let loose = from_builtin_members(sem, arguments);
            let found = no_fit(&candidates, &args, &loose, ClassLattice::new(&sem.symbols))?;
            // A set with static and instance candidates is judged by count.
            if found == NoFit::Types && sem.symbols.is_untyped_member(expr.id) {
                return None;
            }
            verdict(ctx, class, member, found, args, expr.span)
        }
        ExpressionKind::New { arguments, .. } => {
            let Some(Resolution::Resolved(sym)) = sem.references.get(expr.id) else {
                return None;
            };
            let class = sem.symbols.builtin_class(*sym)?;
            if !class.is_complete() {
                return None;
            }
            let params: Vec<Vec<CallParam>> = class
                .constructors()
                .iter()
                .filter(|c| !c.is_static() && c.kind() == BuiltinMemberKind::Constructor)
                .map(|c| {
                    c.params()
                        .iter()
                        .map(|p| CallParam {
                            mode: ParamMode::Input,
                            kind: param_kind(&sem.symbols, p.ty),
                        })
                        .collect()
                })
                .collect();
            if params.is_empty() {
                return None;
            }
            let candidates: Vec<Candidate<'_>> = params
                .iter()
                .map(|p| Candidate {
                    ret: None,
                    params: Some(p),
                    modes_known: false,
                })
                .collect();
            let args = argument_types(sem, arguments);
            let loose = from_builtin_members(sem, arguments);
            let found = no_fit(&candidates, &args, &loose, ClassLattice::new(&sem.symbols))?;
            verdict(ctx, class, None, found, args, expr.span)
        }
        _ => None,
    }
}

/// The misfit, unless it is a count that a newer version may take.
fn verdict(
    ctx: &AnalysisContext,
    class: &'static BuiltinClass,
    member: Option<&'static str>,
    found: NoFit,
    args: Vec<ResolvedType>,
    span: Span,
) -> Option<CallMisfit> {
    // A newer OpenEdge may add an overload with another number of parameters.
    if found == NoFit::Count && ctx.builtins_may_lack_members() {
        return None;
    }
    Some(misfit(class, member, found, args, span))
}

fn misfit(
    class: &'static BuiltinClass,
    member: Option<&'static str>,
    found: NoFit,
    args: Vec<ResolvedType>,
    span: Span,
) -> CallMisfit {
    CallMisfit {
        class,
        member,
        kind: match found {
            NoFit::Count => MisfitKind::Count(args.len()),
            NoFit::Types => MisfitKind::Types(args),
        },
        span,
    }
}

/// The built-in type a receiver evaluates to: a value of it, or its type name.
fn receiver_builtin(sem: &Semantic, object: &Expression) -> Option<&'static BuiltinClass> {
    if let Some(ResolvedType::Class(class)) = sem.types.get(object.id)
        && let Some(builtin) = sem.symbols.builtin_class(*class)
    {
        return Some(builtin);
    }
    match sem.references.get(object.id) {
        Some(Resolution::Resolved(sym)) => sem.symbols.builtin_class(*sym),
        _ => None,
    }
}

/// For each argument, whether it is a scalar (a primitive or a class) that a
/// built-in method or property gives: the catalog may have lost the `EXTENT`
/// of that type.
fn from_builtin_members(sem: &Semantic, arguments: &[Expression]) -> Vec<bool> {
    arguments
        .iter()
        .map(|a| {
            matches!(
                a.kind,
                ExpressionKind::MethodCall { .. } | ExpressionKind::MemberAccess { .. }
            ) && matches!(
                sem.types.get(a.id),
                Some(ResolvedType::Primitive(_) | ResolvedType::Class(_))
            ) && matches!(
                sem.references.get(a.id),
                Some(Resolution::Resolved(sym)) if sem.symbols.builtin_member(*sym).is_some()
            )
        })
        .collect()
}

fn argument_types(sem: &Semantic, arguments: &[Expression]) -> Vec<ResolvedType> {
    arguments
        .iter()
        .map(|a| {
            sem.types
                .get(a.id)
                .cloned()
                .unwrap_or(ResolvedType::Unknown)
        })
        .collect()
}

/// A constructor parameter in this file's symbol space. A catalog type the
/// file has no symbol for is `Unknown`, which never removes a candidate.
fn param_kind(symbols: &SymbolTable, ty: BuiltinType) -> CallParamKind {
    let class = |row: u16| {
        symbols
            .builtin_symbol_of(BuiltinClass::at(row))
            .map_or(ResolvedType::Unknown, ResolvedType::Class)
    };
    let array = |element: ResolvedType| ResolvedType::Array {
        element: Box::new(element),
        extent: None,
    };
    CallParamKind::Value(match ty {
        BuiltinType::Primitive(p) => ResolvedType::Primitive(p),
        BuiltinType::PrimitiveArray(p) => array(ResolvedType::Primitive(p)),
        BuiltinType::Class(row) => class(row),
        BuiltinType::ClassArray(row) => array(class(row)),
        BuiltinType::Shape(shape) => {
            return match shape {
                BuiltinShape::Table => CallParamKind::Table,
                BuiltinShape::TableHandle => CallParamKind::TableHandle,
                BuiltinShape::Dataset => CallParamKind::Dataset,
                BuiltinShape::DatasetHandle => CallParamKind::DatasetHandle,
                BuiltinShape::Buffer => CallParamKind::Buffer,
            };
        }
        BuiltinType::Void | BuiltinType::TypeParam(_) | BuiltinType::Unknown => {
            ResolvedType::Unknown
        }
    })
}

/// Whether an argument is an include or a preprocessor reference, or the
/// preprocessor replaced a reference in the call with no text: the compiler
/// may see other arguments there.
fn has_unexpanded_text(ctx: &AnalysisContext, expr: &Expression) -> bool {
    let arguments = match &expr.kind {
        ExpressionKind::MethodCall { arguments, .. } | ExpressionKind::New { arguments, .. } => {
            arguments
        }
        _ => return false,
    };
    let unexpanded = arguments.iter().any(|a| {
        matches!(
            a.kind,
            ExpressionKind::IncludeReference { .. }
                | ExpressionKind::IncludeArgReference { .. }
                | ExpressionKind::PreprocReference(_)
        )
    });
    let start = ctx.elided.partition_point(|&o| o < expr.span.start);
    unexpanded || ctx.elided.get(start).is_some_and(|&o| o <= expr.span.end)
}
