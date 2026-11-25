//! The pick of a method overload: which declaration a call with these
//! argument types reaches, and so the type of the call.
//!
//! The rules are in `docs/design/class-members.md`, "Overloads". A fit that
//! oxabl is not sure of is `Maybe`, which never removes a candidate, so the
//! pick fails closed: when the arguments do not pick one declaration and the
//! candidates left do not agree on a type, the call is `Unknown`.

use smallvec::SmallVec;

use crate::coercion::{ClassLattice, Reach};
use crate::signature::{CallParam, CallParamKind, ParamMode};
use crate::symbol::{SymbolId, SymbolTable};
use crate::types::{PrimitiveTy, ResolvedType};

/// How one argument fits one parameter. The order is from worst to best, so
/// the worst fit of a candidate is the minimum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Fit {
    /// The two types never convert: the candidate cannot take the argument.
    No,
    /// oxabl is not sure.
    Maybe,
    /// The argument widens to the parameter (or back, for `OUTPUT`).
    Widen,
    /// The same type.
    Exact,
}

/// One declaration of a method: its return type and its parameter list
/// (`None` when the list is not known).
#[derive(Debug, Clone, Copy)]
pub(crate) struct Candidate<'a> {
    pub ret: Option<&'a ResolvedType>,
    pub params: Option<&'a [CallParam]>,
}

/// The candidates of the overload set that `anchor` resolves to: its own
/// declarations and, for a method of a class of the analysed file, the
/// declarations it inherits.
pub(crate) fn candidates_of(symbols: &SymbolTable, anchor: SymbolId) -> SmallVec<[SymbolId; 8]> {
    let mut out: SmallVec<[SymbolId; 8]> = SmallVec::new();
    match symbols.method_overloads(anchor) {
        Some(members) => out.extend_from_slice(members),
        None => out.push(anchor),
    }
    out.extend_from_slice(symbols.inherited_candidates(anchor));
    out
}

/// The type of a call to the overload set of `anchor` with `args`.
pub(crate) fn call_type(
    symbols: &SymbolTable,
    anchor: SymbolId,
    args: &[ResolvedType],
) -> ResolvedType {
    let ids = candidates_of(symbols, anchor);
    let candidates: SmallVec<[Candidate<'_>; 8]> = ids
        .iter()
        .map(|&id| Candidate {
            ret: symbols.get(id).data_type.as_ref(),
            params: symbols.method_params(id),
        })
        .collect();
    pick(&candidates, args, ClassLattice::new(symbols))
}

/// The candidates that `args` leave possible, each with its worst fit: rules
/// 1 and 2 of the design. `Candidate::params` of `None` fits `Maybe`.
fn fitted<'a>(
    candidates: &[Candidate<'a>],
    args: &[ResolvedType],
    lattice: ClassLattice<'_>,
) -> SmallVec<[(usize, Candidate<'a>, Fit); 8]> {
    // Rule 1: the parameter count must match.
    let counted = |c: &&Candidate<'_>| c.params.is_none_or(|p| p.len() == args.len());
    let mixed = mixed_modes(candidates.iter().filter(counted), args.len());
    let mut kept: SmallVec<[(usize, Candidate<'_>, Fit); 8]> = SmallVec::new();
    for (at, candidate) in candidates.iter().enumerate().filter(|(_, c)| counted(c)) {
        let fit = match candidate.params {
            Some(params) => params
                .iter()
                .zip(args)
                .enumerate()
                .map(|(at, (param, arg))| {
                    if mixed.get(at).copied().unwrap_or(false) {
                        fit_any_mode(param, arg, lattice)
                    } else {
                        fit(param, arg, lattice)
                    }
                })
                .min()
                .unwrap_or(Fit::Exact),
            None => Fit::Maybe,
        };
        // Rule 2: a candidate with a `No` cannot take the arguments.
        if fit != Fit::No {
            kept.push((at, *candidate, fit));
        }
    }
    kept
}

/// The index of the one candidate rule 3 or rule 4 picks.
fn picked_index(kept: &[(usize, Candidate<'_>, Fit)]) -> Option<usize> {
    let of = |f: Fit| kept.iter().filter(move |(_, _, fit)| *fit == f);
    let (exact, widen, maybe) = (
        of(Fit::Exact).count(),
        of(Fit::Widen).count(),
        of(Fit::Maybe).count(),
    );
    if exact == 1 && maybe == 0 {
        // Rule 3.
        of(Fit::Exact).next().map(|(at, _, _)| *at)
    } else if exact == 0 && widen == 1 && maybe == 0 {
        // Rule 4.
        of(Fit::Widen).next().map(|(at, _, _)| *at)
    } else {
        None
    }
}

/// Rules 1 to 6 of the design.
pub(crate) fn pick(
    candidates: &[Candidate<'_>],
    args: &[ResolvedType],
    lattice: ClassLattice<'_>,
) -> ResolvedType {
    let kept = fitted(candidates, args, lattice);
    if let Some(at) = picked_index(&kept) {
        return candidates[at].ret.cloned().unwrap_or(ResolvedType::Unknown);
    }
    // Rule 5: every candidate left returns the same type.
    let mut agreed: Option<&ResolvedType> = None;
    for (_, candidate, _) in &kept {
        let Some(ret) = candidate.ret else {
            return ResolvedType::Unknown;
        };
        match agreed {
            None => agreed = Some(ret),
            Some(seen) if seen == ret => {}
            Some(_) => return ResolvedType::Unknown,
        }
    }
    // Rule 6.
    agreed.cloned().unwrap_or(ResolvedType::Unknown)
}

/// The declarations of a call that its arguments leave possible, as indices
/// into the parameter lists given.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallReach {
    /// Every declaration the arguments do not rule out, in order.
    pub possible: Vec<usize>,
    /// The declaration the call reaches, when it is sure: the only one left,
    /// or the one rule 3 or 4 of the overload pick chooses.
    pub picked: Option<usize>,
}

/// Which of `params` (one list per declaration; `None` when a list is not
/// known, and whether the list states the modes of its parameters) a call
/// with arguments of type `args` can reach. Fails closed: a declaration is
/// dropped only when no argument could ever fit it, and one is picked only
/// when the compiler must pick it.
pub fn reachable_overloads(
    params: &[(Option<&[CallParam]>, bool)],
    args: &[ResolvedType],
    lattice: ClassLattice<'_>,
) -> CallReach {
    let candidates: Vec<Candidate<'_>> = params
        .iter()
        .map(|&(params, _modes_known)| Candidate { ret: None, params })
        .collect();
    let kept = fitted(&candidates, args, lattice);
    let picked = match kept.as_slice() {
        [(at, _, _)] => Some(*at),
        _ => picked_index(&kept),
    };
    CallReach {
        possible: kept.iter().map(|(at, _, _)| *at).collect(),
        picked,
    }
}

/// The declarations of the overload set that `anchor` resolves to, as
/// [`candidates_of`] finds them.
pub fn call_candidates(symbols: &SymbolTable, anchor: SymbolId) -> Vec<SymbolId> {
    candidates_of(symbols, anchor).into_vec()
}

/// For each argument position, whether the candidates do not all declare the
/// same mode there. The AST does not keep the mode written at the call, and
/// the compiler matches it, so at such a position oxabl does not know which
/// candidates the call can reach.
fn mixed_modes<'c, 'a: 'c>(
    candidates: impl Iterator<Item = &'c Candidate<'a>>,
    count: usize,
) -> SmallVec<[bool; 8]> {
    let mut seen: SmallVec<[Option<ParamMode>; 8]> = SmallVec::from_elem(None, count);
    let mut mixed: SmallVec<[bool; 8]> = SmallVec::from_elem(false, count);
    for params in candidates.filter_map(|c| c.params) {
        for (at, param) in params.iter().enumerate() {
            match seen[at] {
                None => seen[at] = Some(param.mode),
                Some(mode) if mode != param.mode => mixed[at] = true,
                Some(_) => {}
            }
        }
    }
    mixed
}

/// How an argument fits `param` when the call may write another mode than
/// the parameter's: only a `No` that holds in every mode, else `Maybe`.
fn fit_any_mode(param: &CallParam, arg: &ResolvedType, lattice: ClassLattice<'_>) -> Fit {
    let any = CallParam {
        mode: ParamMode::InputOutput,
        kind: param.kind.clone(),
    };
    fit(&any, arg, lattice).min(Fit::Maybe)
}

/// How an argument of type `arg` fits `param`.
pub(crate) fn fit(param: &CallParam, arg: &ResolvedType, lattice: ClassLattice<'_>) -> Fit {
    let CallParamKind::Value(ty) = &param.kind else {
        return Fit::Maybe;
    };
    if matches!(arg, ResolvedType::Unknown | ResolvedType::Error)
        || matches!(ty, ResolvedType::Unknown | ResolvedType::Error)
    {
        return Fit::Maybe;
    }
    if arg == ty {
        return Fit::Exact;
    }
    if never_converts(arg, ty, lattice, param.mode) {
        return Fit::No;
    }
    let widens = match param.mode {
        ParamMode::Input => widens(arg, ty, lattice),
        ParamMode::Output => widens(ty, arg, lattice),
        ParamMode::InputOutput => false,
    };
    if widens { Fit::Widen } else { Fit::Maybe }
}

/// Whether `from` widens to `to` for sure.
fn widens(from: &ResolvedType, to: &ResolvedType, lattice: ClassLattice<'_>) -> bool {
    use PrimitiveTy::*;
    match (from, to) {
        (ResolvedType::Primitive(f), ResolvedType::Primitive(t)) => matches!(
            (f, t),
            (Integer, Int64)
                | (Integer, Decimal)
                | (Int64, Decimal)
                | (Character, Longchar)
                | (Date, Datetime)
                | (Date, DatetimeTz)
                | (Datetime, DatetimeTz)
        ),
        (ResolvedType::Class(f), ResolvedType::Class(t)) => {
            lattice.reaches(*f, *t) == Reach::Widens
        }
        _ => false,
    }
}

/// Whether the two types are of families that never convert. Only answers
/// `true` when that is sure.
fn never_converts(
    arg: &ResolvedType,
    param: &ResolvedType,
    lattice: ClassLattice<'_>,
    mode: ParamMode,
) -> bool {
    use ResolvedType::{Array, Class, Primitive};
    match (arg, param) {
        (Class(_), Primitive(_)) | (Primitive(_), Class(_)) => true,
        (Array { .. }, Primitive(_) | Class(_)) | (Primitive(_) | Class(_), Array { .. }) => true,
        (Class(a), Class(p)) => {
            // The class that must widen to the other one.
            let (from, to) = match mode {
                ParamMode::Input => (*a, *p),
                ParamMode::Output => (*p, *a),
                // Either direction may compile for all oxabl knows.
                ParamMode::InputOutput => return false,
            };
            lattice.reaches(from, to) == Reach::DoesNot
        }
        (Primitive(a), Primitive(p)) => {
            let (fa, fp) = (family(*a), family(*p));
            fa.is_some() && fp.is_some() && fa != fp
        }
        _ => false,
    }
}

/// The family of a primitive that the never-converts rule knows.
fn family(ty: PrimitiveTy) -> Option<u8> {
    use PrimitiveTy::*;
    match ty {
        Integer | Int64 | Decimal => Some(0),
        Character | Longchar => Some(1),
        Date | Datetime | DatetimeTz => Some(2),
        Logical => Some(3),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prim(t: PrimitiveTy) -> ResolvedType {
        ResolvedType::Primitive(t)
    }

    fn input(ty: ResolvedType) -> CallParam {
        CallParam {
            mode: ParamMode::Input,
            kind: CallParamKind::Value(ty),
        }
    }

    fn with_mode(mode: ParamMode, ty: ResolvedType) -> CallParam {
        CallParam {
            mode,
            kind: CallParamKind::Value(ty),
        }
    }

    /// Run the pick over `(return type, parameters)` rows.
    fn pick_of(rows: &[(ResolvedType, Vec<CallParam>)], args: &[ResolvedType]) -> ResolvedType {
        let table = SymbolTable::new();
        let candidates: Vec<Candidate<'_>> = rows
            .iter()
            .map(|(ret, params)| Candidate {
                ret: Some(ret),
                params: Some(params),
            })
            .collect();
        pick(&candidates, args, ClassLattice::new(&table))
    }

    use PrimitiveTy::*;

    #[test]
    fn the_fit_of_an_argument() {
        let table = SymbolTable::new();
        let lattice = ClassLattice::new(&table);
        let cases = [
            (input(prim(Integer)), prim(Integer), Fit::Exact),
            (input(prim(Int64)), prim(Integer), Fit::Widen),
            (input(prim(Integer)), prim(Decimal), Fit::Maybe),
            (input(prim(Logical)), prim(Integer), Fit::No),
            (input(prim(Character)), prim(Date), Fit::No),
            (input(prim(Date)), prim(Decimal), Fit::No),
            (input(prim(Integer)), ResolvedType::Unknown, Fit::Maybe),
            (input(prim(Handle)), prim(Integer), Fit::Maybe),
            (
                with_mode(ParamMode::Output, prim(Integer)),
                prim(Decimal),
                Fit::Widen,
            ),
            (
                with_mode(ParamMode::Output, prim(Decimal)),
                prim(Integer),
                Fit::Maybe,
            ),
            (
                with_mode(ParamMode::InputOutput, prim(Decimal)),
                prim(Integer),
                Fit::Maybe,
            ),
            (
                input(ResolvedType::Array {
                    element: Box::new(prim(Integer)),
                    extent: Some(2),
                }),
                prim(Integer),
                Fit::No,
            ),
            (
                CallParam {
                    mode: ParamMode::Input,
                    kind: CallParamKind::Buffer,
                },
                prim(Integer),
                Fit::Maybe,
            ),
        ];
        for (param, arg, want) in cases {
            assert_eq!(fit(&param, &arg, lattice), want, "{param:?} <- {arg:?}");
        }
    }

    #[test]
    fn the_rows_of_the_design_example_table() {
        let score = [
            (prim(Integer), vec![input(prim(Integer))]),
            (prim(Character), vec![input(prim(Character))]),
        ];
        assert_eq!(pick_of(&score, &[prim(Integer)]), prim(Integer));
        assert_eq!(pick_of(&score, &[prim(Character)]), prim(Character));
        assert_eq!(
            pick_of(&score, &[ResolvedType::Unknown]),
            ResolvedType::Unknown
        );
        assert_eq!(
            pick_of(&score, &[prim(Integer), prim(Integer)]),
            ResolvedType::Unknown
        );

        let widen = [
            (prim(Int64), vec![input(prim(Int64))]),
            (prim(Decimal), vec![input(prim(Decimal))]),
        ];
        assert_eq!(pick_of(&widen, &[prim(Integer)]), ResolvedType::Unknown);
        assert_eq!(pick_of(&widen, &[prim(Int64)]), prim(Int64));

        let same = [
            (prim(Logical), vec![input(prim(Integer))]),
            (prim(Logical), vec![input(prim(Character))]),
        ];
        assert_eq!(pick_of(&same, &[ResolvedType::Unknown]), prim(Logical));

        let fill = [
            (prim(Integer), vec![input(prim(Character))]),
            (
                prim(Character),
                vec![with_mode(ParamMode::Output, prim(Character))],
            ),
        ];
        assert_eq!(pick_of(&fill, &[prim(Character)]), ResolvedType::Unknown);
    }

    #[test]
    fn one_widen_and_no_exact_picks_the_widen() {
        let rows = [
            (prim(Decimal), vec![input(prim(Decimal))]),
            (prim(Logical), vec![input(prim(Logical))]),
        ];
        assert_eq!(pick_of(&rows, &[prim(Integer)]), prim(Decimal));
        let rows = [
            (prim(Int64), vec![input(prim(Int64))]),
            (prim(Logical), vec![input(prim(Logical))]),
        ];
        assert_eq!(pick_of(&rows, &[prim(Integer)]), prim(Int64));
    }

    #[test]
    fn an_exact_with_a_maybe_beside_it_picks_nothing() {
        // The `HANDLE` parameter may take an `INTEGER` for all oxabl knows.
        let rows = [
            (prim(Integer), vec![input(prim(Integer))]),
            (prim(Character), vec![input(prim(Handle))]),
        ];
        assert_eq!(pick_of(&rows, &[prim(Integer)]), ResolvedType::Unknown);
    }

    #[test]
    fn candidates_of_other_modes_at_a_position_pick_nothing() {
        // `F(OUTPUT d)` reaches the `OUTPUT INTEGER` one, which the fit by
        // the parameter's own mode would rank below the `INPUT DECIMAL` one.
        let rows = [
            (prim(Integer), vec![input(prim(Decimal))]),
            (
                prim(Character),
                vec![with_mode(ParamMode::Output, prim(Integer))],
            ),
        ];
        assert_eq!(pick_of(&rows, &[prim(Decimal)]), ResolvedType::Unknown);
        assert_eq!(pick_of(&rows, &[prim(Integer)]), ResolvedType::Unknown);
        // A family that never converts still removes a candidate.
        assert_eq!(pick_of(&rows, &[prim(Logical)]), ResolvedType::Unknown);
        let rows = [
            (prim(Integer), vec![input(prim(Decimal))]),
            (
                prim(Character),
                vec![with_mode(ParamMode::Output, prim(Character))],
            ),
        ];
        assert_eq!(pick_of(&rows, &[prim(Character)]), prim(Character));
    }

    #[test]
    fn candidates_of_one_mode_keep_the_pick() {
        let rows = [
            (
                prim(Integer),
                vec![with_mode(ParamMode::Output, prim(Logical))],
            ),
            (
                prim(Character),
                vec![with_mode(ParamMode::Output, prim(Integer))],
            ),
        ];
        assert_eq!(pick_of(&rows, &[prim(Integer)]), prim(Character));
        // A position where the modes agree still picks when another differs.
        let rows = [
            (
                prim(Integer),
                vec![input(prim(Integer)), input(prim(Decimal))],
            ),
            (
                prim(Character),
                vec![
                    input(prim(Character)),
                    with_mode(ParamMode::Output, prim(Decimal)),
                ],
            ),
        ];
        assert_eq!(
            pick_of(&rows, &[prim(Integer), prim(Decimal)]),
            prim(Integer)
        );
    }

    #[test]
    fn an_unknown_parameter_list_is_a_maybe() {
        let table = SymbolTable::new();
        let int = prim(Integer);
        let chr = prim(Character);
        let params = [input(prim(Integer))];
        let candidates = [
            Candidate {
                ret: Some(&int),
                params: Some(&params),
            },
            Candidate {
                ret: Some(&chr),
                params: None,
            },
        ];
        assert_eq!(
            pick(&candidates, &[prim(Integer)], ClassLattice::new(&table)),
            ResolvedType::Unknown
        );
    }

    #[test]
    fn a_void_candidate_gives_no_type() {
        let table = SymbolTable::new();
        let candidates = [Candidate {
            ret: None,
            params: Some(&[]),
        }];
        assert_eq!(
            pick(&candidates, &[], ClassLattice::new(&table)),
            ResolvedType::Unknown
        );
    }
}
