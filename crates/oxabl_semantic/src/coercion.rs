//! Coercion predicates for assignment and parameter-passing sites.
//!
//! v1 exports two predicates:
//! - [`assignable`] — lenient; used at assignment sites
//!   (`x = expr`, `ASSIGN x = expr`, variable initial values). Permits the
//!   widening ladder, silent numeric narrowing, and Unknown as universal
//!   bottom.
//! - [`assignable_strict`] — parameter sites (`OUTPUT` / `INPUT-OUTPUT`).
//!   Exact type match only. `Unknown` still acts as universal bottom so
//!   ABL's `?` can pass through any parameter site.
//!
//! Both predicates take a [`ClassLattice`] because assignability between two
//! class types is not a property of the two types — it is a question about the
//! inheritance graph the file's symbol table records, and a free function over
//! two `ResolvedType`s cannot see it.
//!
//! See plan §Coercion catalog for the authoritative rule set.

use oxabl_lexer::oxabl_atom::OxablAtom;
use smallvec::SmallVec;

use crate::symbol::{SymbolId, SymbolKind, SymbolTable};
use crate::types::{PrimitiveTy, ResolvedType};

/// How many levels of an inheritance chain fit without touching the heap.
///
/// Both the visited set and the work stack are `SmallVec`s of this size, so an
/// ABL hierarchy up to eight ancestors deep — which is every real one — walks
/// with no allocation at all. Deeper chains still work; they just spill.
const CHAIN_INLINE: usize = 8;

/// The resolved class lattice: the inheritance graph [`assignable`] consults
/// when neither class type is the other's identical symbol.
///
/// A one-pointer `Copy` newtype over the symbol table rather than the table
/// itself, for two reasons. It names the *role* the table plays here — the
/// coercion predicates must not reach into a symbol for anything else — and it
/// leaves room for the lattice to become a precomputed, resolved
/// `SymbolId → parents` side map later without touching a single call site.
///
/// Reachability, not the full transitive closure: this answers one directed
/// question at a time and never materializes a set.
#[derive(Debug, Clone, Copy)]
pub struct ClassLattice<'a> {
    symbols: &'a SymbolTable,
}

impl<'a> ClassLattice<'a> {
    /// View `symbols` as the class lattice.
    #[inline]
    pub fn new(symbols: &'a SymbolTable) -> Self {
        ClassLattice { symbols }
    }

    /// Whether a value of class `from` may stand where class `to` is expected —
    /// i.e. whether `to` is `from` itself, one of its ancestors, or an interface
    /// it (or an ancestor) implements.
    ///
    /// **One-directional on purpose.** The walk only ever climbs from `from`, so
    /// a parent instance assigned into a child-typed location stays a mismatch.
    /// That asymmetry *is* the rule; making the walk symmetric would silently
    /// delete a whole class of real findings.
    ///
    /// **Termination is a visited set, not a depth bound**, the same choice
    /// `walk_member_surface` makes in the resolve pass and for the same reason:
    /// `CLASS a INHERITS a`, and cycles around several classes, are real shapes
    /// in broken code, and a set says exactly what happens (each class is asked
    /// about once) where a bound would only say when we gave up. `from` is
    /// seeded into it, so a self-inheriting class terminates on its first level.
    ///
    /// Both containers are inline up to [`CHAIN_INLINE`], and this function is
    /// only reached from the `(Class, Class)` arm *after* the identity fast
    /// path — so the ordinary assignment, and every assignment between
    /// primitives, allocates nothing and walks nothing.
    pub fn widens_to(self, from: SymbolId, to: SymbolId) -> bool {
        if from == to {
            return true;
        }
        let mut visited: SmallVec<[SymbolId; CHAIN_INLINE]> = SmallVec::new();
        visited.push(from);
        let mut stack: SmallVec<[SymbolId; CHAIN_INLINE]> = SmallVec::new();
        stack.push(from);
        // Depth-first: the question is reachability, so visit order is
        // immaterial, and a stack keeps both containers the same shape.
        while let Some(current) = stack.pop() {
            // A class whose header named no supertype has no entry at all —
            // which is what makes "declares no parent" distinguishable from
            // "declares one that resolved to nothing".
            let Some(supers) = self.symbols.supertypes(current) else {
                continue;
            };
            // `inherits` and `implements` are walked as one sequence: an
            // interface reached through `IMPLEMENTS` contributes assignability
            // exactly as a superclass reached through `INHERITS` does, and an
            // interface's own supertypes are recorded in `implements` too. So
            // there is no separate interface arm to keep in step.
            for supertype in supers.inherits.iter().chain(&supers.implements) {
                let Some(ancestor) = self.class_named(supertype.name.as_atom()) else {
                    // A supertype no symbol in this table stands for: nothing to
                    // compare against and nothing to climb through. Not an
                    // error here — whether an unresolvable parent deserves a
                    // diagnostic is a rule's decision, not a coercion
                    // predicate's.
                    continue;
                };
                if ancestor == to {
                    return true;
                }
                if !visited.contains(&ancestor) {
                    visited.push(ancestor);
                    stack.push(ancestor);
                }
            }
        }
        false
    }

    /// The class or interface symbol carrying the folded name `folded`, if the
    /// table holds one.
    ///
    /// A linear scan, deliberately. A supertype is recorded as a *name* (that is
    /// what a header spells, and resolving it at declare time would make the
    /// declare pass depend on the index), so identity has to be recovered from
    /// the name somewhere. The scan is affordable because it only ever runs on
    /// the chain-walk path — reached when two *different* class symbols meet at
    /// one assignment, which is rare — never on the per-assignment fast path.
    ///
    /// Matching is on the folded atom, so `Parent` and `PARENT` are one class,
    /// as everywhere else in the symbol table. A name spelled differently from
    /// the symbol it should reach (a simple name under a `USING` import, whose
    /// synthesized symbol carries the *qualified* spelling) finds nothing and
    /// simply does not widen — conservative in the safe direction, and moot
    /// while the `check.rs` firewall keeps index-synthesized classes at
    /// `Unknown`. Recording resolved supertype ids at resolve time is the fix,
    /// and it belongs with the unit that lifts that firewall.
    fn class_named(self, folded: &OxablAtom) -> Option<SymbolId> {
        self.symbols
            .iter()
            .find(|(_, s)| {
                matches!(s.kind, SymbolKind::Class | SymbolKind::Interface) && &s.name == folded
            })
            .map(|(id, _)| id)
    }
}

/// Whether a value of type `from` may be assigned to a location of type
/// `to`. See module docs for details.
pub fn assignable(from: &ResolvedType, to: &ResolvedType, lattice: ClassLattice<'_>) -> bool {
    use ResolvedType::*;
    // Unknown/Error are universal; they never produce a LINT0004 mismatch.
    if matches!(from, Unknown | Error) || matches!(to, Unknown | Error) {
        return true;
    }
    if from == to {
        return true;
    }
    match (from, to) {
        (Primitive(f), Primitive(t)) => primitive_assignable(*f, *t),
        (
            Array {
                element: fe,
                extent: fx,
            },
            Array {
                element: te,
                extent: tx,
            },
        ) => (fx == tx || fx.is_none() || tx.is_none()) && assignable(fe, te, lattice),
        // Class identity first, then the inheritance chain: a subclass stands
        // where a parent or an implemented interface is expected. Identity is
        // already settled by the `from == to` fast path above, so reaching here
        // means two genuinely different class symbols and the walk is worth it.
        (Class(a), Class(b)) => lattice.widens_to(*a, *b),
        (Buffer(a), Buffer(b)) => a == b,
        (Table(ra, ta), Table(rb, tb)) => ra == rb && ta == tb,
        _ => false,
    }
}

/// Strict assignment — parameter `OUTPUT` / `INPUT-OUTPUT` sites. Requires
/// exact primitive match (no widening, no narrowing). `Unknown` remains
/// universal.
///
/// Takes the lattice and deliberately does not consult it. Inheritance widening
/// is **unsound at a write-back site**: the callee writes whatever *it* declared
/// into the caller's location, so accepting a parent-typed out-parameter for a
/// child-typed variable would license storing a `Parent` into a `Child`. The
/// parameter is here so both predicates present one signature to their call
/// sites — the alternative, a caller that has to remember which of the two takes
/// a lattice, is how the wrong one gets called.
pub fn assignable_strict(
    from: &ResolvedType,
    to: &ResolvedType,
    _lattice: ClassLattice<'_>,
) -> bool {
    use ResolvedType::*;
    if matches!(from, Unknown | Error) || matches!(to, Unknown | Error) {
        return true;
    }
    from == to
}

/// Whether the conversion `from` → `to` is a narrowing one that `LINT0004`
/// should emit as `Warning`. Integer-from-Decimal/Int64 truncation is
/// **silent** (idiomatic ABL; callers should not treat it as a warning).
/// Longchar→Character, Datetime→Date, and DatetimeTz→Date are warnings
/// because the narrowing discards user-visible information (bytes past
/// 32K or the time component).
pub fn is_narrowing_warning(from: &ResolvedType, to: &ResolvedType) -> bool {
    use PrimitiveTy::*;
    if let (ResolvedType::Primitive(f), ResolvedType::Primitive(t)) = (from, to) {
        matches!(
            (f, t),
            (Longchar, Character) | (Datetime, Date) | (DatetimeTz, Date)
        )
    } else {
        false
    }
}

/// Widen two primitives to a common type for arithmetic/comparison. Returns
/// `None` if the two primitives don't share a widening ladder.
pub fn widen_primitive(a: PrimitiveTy, b: PrimitiveTy) -> Option<PrimitiveTy> {
    use PrimitiveTy::*;
    if a == b {
        return Some(a);
    }
    match (a, b) {
        // Numeric ladder: Integer ⊂ Int64 ⊂ Decimal.
        (Integer, Int64) | (Int64, Integer) => Some(Int64),
        (Integer, Decimal) | (Decimal, Integer) => Some(Decimal),
        (Int64, Decimal) | (Decimal, Int64) => Some(Decimal),
        // Date ladder.
        (Date, Datetime) | (Datetime, Date) => Some(Datetime),
        (Date, DatetimeTz) | (DatetimeTz, Date) => Some(DatetimeTz),
        (Datetime, DatetimeTz) | (DatetimeTz, Datetime) => Some(DatetimeTz),
        // Character ladder.
        (Character, Longchar) | (Longchar, Character) => Some(Longchar),
        _ => None,
    }
}

fn primitive_assignable(from: PrimitiveTy, to: PrimitiveTy) -> bool {
    use PrimitiveTy::*;
    if from == to {
        return true;
    }
    match (from, to) {
        // Widening (silent).
        (Integer, Int64) | (Integer, Decimal) => true,
        (Int64, Decimal) => true,
        (Date, Datetime) | (Date, DatetimeTz) => true,
        (Datetime, DatetimeTz) => true,
        (Character, Longchar) => true,
        // Narrowing — assignable in v1, with LINT0004 severity decided by
        // `is_narrowing_warning` above.
        (Decimal, Integer) | (Decimal, Int64) => true,
        (Int64, Integer) => true,
        (Longchar, Character) => true,
        (Datetime, Date) | (DatetimeTz, Date) => true,
        // Strip explicit tz: DatetimeTz → Datetime assignable.
        (DatetimeTz, Datetime) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{IndexName, NamespaceId, ScopeId, SupertypeRef, Supertypes, Symbol};
    use oxabl_ast::NodeId;
    use oxabl_common::VirtualSpan;

    fn p(t: PrimitiveTy) -> ResolvedType {
        ResolvedType::Primitive(t)
    }

    // The primitive, array, buffer and table rules have no class in play, so the
    // honest lattice for them is an empty one. These two shadow the crate
    // functions inside this module so those tests read exactly as they did
    // before the lattice parameter existed — the classes get their own tests
    // below, with a real table.
    fn assignable(from: &ResolvedType, to: &ResolvedType) -> bool {
        let table = SymbolTable::new();
        super::assignable(from, to, ClassLattice::new(&table))
    }

    fn assignable_strict(from: &ResolvedType, to: &ResolvedType) -> bool {
        let table = SymbolTable::new();
        super::assignable_strict(from, to, ClassLattice::new(&table))
    }

    #[test]
    fn unknown_is_universal_bottom() {
        assert!(assignable(&ResolvedType::Unknown, &p(PrimitiveTy::Integer)));
        assert!(assignable(&p(PrimitiveTy::Integer), &ResolvedType::Unknown));
    }

    #[test]
    fn error_poisons_without_cascade() {
        assert!(assignable(&ResolvedType::Error, &p(PrimitiveTy::Integer)));
    }

    #[test]
    fn integer_widens_to_int64() {
        assert!(assignable(&p(PrimitiveTy::Integer), &p(PrimitiveTy::Int64)));
    }

    #[test]
    fn integer_widens_to_decimal() {
        assert!(assignable(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Decimal)
        ));
    }

    #[test]
    fn decimal_narrows_to_integer_silently() {
        assert!(assignable(
            &p(PrimitiveTy::Decimal),
            &p(PrimitiveTy::Integer)
        ));
        assert!(!is_narrowing_warning(
            &p(PrimitiveTy::Decimal),
            &p(PrimitiveTy::Integer)
        ));
    }

    #[test]
    fn longchar_narrows_to_character_with_warning() {
        assert!(assignable(
            &p(PrimitiveTy::Longchar),
            &p(PrimitiveTy::Character)
        ));
        assert!(is_narrowing_warning(
            &p(PrimitiveTy::Longchar),
            &p(PrimitiveTy::Character)
        ));
    }

    #[test]
    fn datetime_narrows_to_date_with_warning() {
        assert!(assignable(&p(PrimitiveTy::Datetime), &p(PrimitiveTy::Date)));
        assert!(is_narrowing_warning(
            &p(PrimitiveTy::Datetime),
            &p(PrimitiveTy::Date)
        ));
    }

    #[test]
    fn logical_to_integer_is_not_assignable() {
        assert!(!assignable(
            &p(PrimitiveTy::Logical),
            &p(PrimitiveTy::Integer)
        ));
        assert!(!assignable(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Logical)
        ));
    }

    #[test]
    fn strict_rejects_widening() {
        assert!(!assignable_strict(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Int64)
        ));
    }

    #[test]
    fn strict_accepts_unknown() {
        assert!(assignable_strict(
            &ResolvedType::Unknown,
            &p(PrimitiveTy::Integer)
        ));
    }

    #[test]
    fn widen_same_type_is_identity() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Integer),
            Some(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn widen_integer_int64_is_int64() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Int64),
            Some(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn widen_integer_decimal_is_decimal() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Decimal),
            Some(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn widen_incompatible_is_none() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Logical, PrimitiveTy::Integer),
            None
        );
    }

    #[test]
    fn array_identity_is_assignable() {
        let arr = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: Some(5),
        };
        assert!(assignable(&arr, &arr));
    }

    #[test]
    fn array_dynamic_extent_is_assignable_to_fixed() {
        let fixed = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: Some(5),
        };
        let dynamic = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: None,
        };
        assert!(assignable(&dynamic, &fixed));
        assert!(assignable(&fixed, &dynamic));
    }

    // =======================================================================
    // The class lattice
    // =======================================================================
    //
    // These build a symbol table by hand rather than running the passes, for
    // one reason that matters: the `check.rs` firewall keeps an
    // index-synthesized class at `ResolvedType::Unknown`, so an end-to-end
    // *cross-file* scenario would go silent whether or not this walk works.
    // Here the walk is the only thing under test, so a cross-file-*shaped*
    // chain (a qualified name, a parent symbol with no declaration node) is
    // really asserted instead of inferred.

    use crate::SymbolFlags;

    /// One class or interface symbol, wired the way the declare pass wires one.
    /// `indexed` reproduces the index-synthesized shape — no declaration node in
    /// this file — which is what a cross-file parent looks like in the table.
    fn add_class(table: &mut SymbolTable, name: &str, kind: SymbolKind, indexed: bool) -> SymbolId {
        table.insert(Symbol {
            // Folded through `IndexName`, exactly as the real insertion paths
            // fold it, so a test cannot accidentally store an unfolded name that
            // the lookup could never match.
            name: IndexName::new(name).as_atom().clone(),
            namespace: NamespaceId::Types,
            kind,
            declared_in: ScopeId::ROOT,
            declaration: if indexed {
                NodeId::DUMMY
            } else {
                NodeId::from_u32(1)
            },
            name_span: VirtualSpan::new(0, 0),
            data_type: None,
            read_count: 0,
            write_count: 0,
            flags: SymbolFlags::empty(),
            table_id: None,
        })
    }

    fn sref(name: &str) -> SupertypeRef {
        SupertypeRef {
            name: IndexName::new(name),
            name_span: VirtualSpan::new(0, 0),
        }
    }

    /// Record `sym`'s header supertypes — names, as a header spells them.
    fn wire(table: &mut SymbolTable, sym: SymbolId, inherits: Option<&str>, implements: &[&str]) {
        table.record_supertypes(
            sym,
            Supertypes {
                inherits: inherits.map(sref),
                implements: implements.iter().copied().map(sref).collect(),
            },
        );
    }

    fn lattice_of(table: &SymbolTable) -> ClassLattice<'_> {
        ClassLattice::new(table)
    }

    #[test]
    fn a_class_widens_to_itself() {
        let mut t = SymbolTable::new();
        let a = add_class(&mut t, "solo", SymbolKind::Class, false);
        assert!(lattice_of(&t).widens_to(a, a));
    }

    #[test]
    fn a_child_widens_to_its_parent() {
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("parent-cls"), &[]);
        assert!(lattice_of(&t).widens_to(child, parent));
    }

    #[test]
    fn a_grandchild_widens_to_its_grandparent() {
        let mut t = SymbolTable::new();
        let grand = add_class(&mut t, "grand-cls", SymbolKind::Class, false);
        let mid = add_class(&mut t, "mid-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, mid, Some("grand-cls"), &[]);
        wire(&mut t, child, Some("mid-cls"), &[]);
        assert!(lattice_of(&t).widens_to(child, grand));
    }

    #[test]
    fn a_parent_does_not_widen_to_its_child() {
        // The direction that must stay a finding: the walk only climbs.
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("parent-cls"), &[]);
        assert!(!lattice_of(&t).widens_to(parent, child));
    }

    #[test]
    fn unrelated_classes_do_not_widen_in_either_direction() {
        let mut t = SymbolTable::new();
        let a = add_class(&mut t, "alpha-cls", SymbolKind::Class, false);
        let b = add_class(&mut t, "beta-cls", SymbolKind::Class, false);
        // Both have parents, so the walk really runs and still answers no.
        let base = add_class(&mut t, "base-cls", SymbolKind::Class, false);
        wire(&mut t, a, Some("base-cls"), &[]);
        let _ = base;
        assert!(!lattice_of(&t).widens_to(a, b));
        assert!(!lattice_of(&t).widens_to(b, a));
    }

    #[test]
    fn a_class_widens_to_an_interface_it_implements() {
        let mut t = SymbolTable::new();
        let iface = add_class(&mut t, "i-calc", SymbolKind::Interface, false);
        let cls = add_class(&mut t, "calc-cls", SymbolKind::Class, false);
        wire(&mut t, cls, None, &["i-calc"]);
        assert!(lattice_of(&t).widens_to(cls, iface));
    }

    #[test]
    fn a_class_widens_to_an_interface_its_interface_extends() {
        // An interface's own supertypes live in `implements` too, so one walk
        // covers both hops without an interface-specific arm.
        let mut t = SymbolTable::new();
        let outer = add_class(&mut t, "i-audit", SymbolKind::Interface, false);
        let inner = add_class(&mut t, "i-calc", SymbolKind::Interface, false);
        let cls = add_class(&mut t, "calc-cls", SymbolKind::Class, false);
        wire(&mut t, inner, None, &["i-audit"]);
        wire(&mut t, cls, None, &["i-calc"]);
        assert!(lattice_of(&t).widens_to(cls, outer));
    }

    #[test]
    fn a_class_widens_to_an_interface_its_parent_implements() {
        let mut t = SymbolTable::new();
        let iface = add_class(&mut t, "i-calc", SymbolKind::Interface, false);
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, parent, None, &["i-calc"]);
        wire(&mut t, child, Some("parent-cls"), &[]);
        let _ = parent;
        assert!(lattice_of(&t).widens_to(child, iface));
    }

    #[test]
    fn a_cross_file_shaped_chain_widens_through_a_qualified_name() {
        // The shape U6's firewall currently hides: a locally declared class whose
        // parent is an index-synthesized symbol carrying the qualified folded
        // name. The walk resolves it by that name and widens — which is what
        // must already be true on the day the firewall lifts.
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "orders.base", SymbolKind::Class, true);
        let child = add_class(&mut t, "orders.child", SymbolKind::Class, false);
        wire(&mut t, child, Some("orders.base"), &[]);
        assert_eq!(t.get(parent).declaration, NodeId::DUMMY);
        assert!(lattice_of(&t).widens_to(child, parent));
        assert!(!lattice_of(&t).widens_to(parent, child));
    }

    #[test]
    fn supertype_matching_is_case_insensitive() {
        // ABL is case-insensitive, and every other name in the table is folded.
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("PARENT-CLS"), &[]);
        assert!(lattice_of(&t).widens_to(child, parent));
    }

    #[test]
    fn a_supertype_no_symbol_stands_for_does_not_widen() {
        let mut t = SymbolTable::new();
        let other = add_class(&mut t, "other-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("nowhere-cls"), &[]);
        assert!(!lattice_of(&t).widens_to(child, other));
    }

    #[test]
    fn a_self_inheriting_class_terminates() {
        let mut t = SymbolTable::new();
        let other = add_class(&mut t, "other-cls", SymbolKind::Class, false);
        let loop_cls = add_class(&mut t, "loop-cls", SymbolKind::Class, false);
        wire(&mut t, loop_cls, Some("loop-cls"), &[]);
        // Terminating *is* the assertion; the answer is incidental.
        assert!(!lattice_of(&t).widens_to(loop_cls, other));
        assert!(lattice_of(&t).widens_to(loop_cls, loop_cls));
    }

    #[test]
    fn a_two_class_cycle_terminates() {
        let mut t = SymbolTable::new();
        let other = add_class(&mut t, "other-cls", SymbolKind::Class, false);
        let a = add_class(&mut t, "a-cls", SymbolKind::Class, false);
        let b = add_class(&mut t, "b-cls", SymbolKind::Class, false);
        wire(&mut t, a, Some("b-cls"), &[]);
        wire(&mut t, b, Some("a-cls"), &[]);
        assert!(!lattice_of(&t).widens_to(a, other));
        // The cycle still reports what it genuinely reaches.
        assert!(lattice_of(&t).widens_to(a, b));
        assert!(lattice_of(&t).widens_to(b, a));
    }

    #[test]
    fn a_chain_deeper_than_the_inline_capacity_still_answers() {
        // `CHAIN_INLINE` is a no-allocation threshold, not a depth limit.
        let mut t = SymbolTable::new();
        let names: Vec<String> = (0..CHAIN_INLINE * 2)
            .map(|i| format!("level{i}-cls"))
            .collect();
        let ids: Vec<SymbolId> = names
            .iter()
            .map(|n| add_class(&mut t, n, SymbolKind::Class, false))
            .collect();
        for i in 1..ids.len() {
            wire(&mut t, ids[i], Some(&names[i - 1]), &[]);
        }
        let leaf = *ids.last().unwrap();
        assert!(lattice_of(&t).widens_to(leaf, ids[0]));
        assert!(!lattice_of(&t).widens_to(ids[0], leaf));
    }

    // ---- Through `assignable` / `assignable_strict` ------------------------

    #[test]
    fn assignable_widens_a_subclass_to_a_parent_typed_location() {
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("parent-cls"), &[]);
        let l = lattice_of(&t);
        assert!(super::assignable(
            &ResolvedType::Class(child),
            &ResolvedType::Class(parent),
            l
        ));
        assert!(!super::assignable(
            &ResolvedType::Class(parent),
            &ResolvedType::Class(child),
            l
        ));
    }

    #[test]
    fn assignable_strict_rejects_a_subclass_at_a_write_back_site() {
        // Widening is unsound where the callee writes into the caller's
        // location, so strict stays exact even with the lattice in hand.
        let mut t = SymbolTable::new();
        let parent = add_class(&mut t, "parent-cls", SymbolKind::Class, false);
        let child = add_class(&mut t, "child-cls", SymbolKind::Class, false);
        wire(&mut t, child, Some("parent-cls"), &[]);
        let l = lattice_of(&t);
        assert!(!super::assignable_strict(
            &ResolvedType::Class(child),
            &ResolvedType::Class(parent),
            l
        ));
        assert!(super::assignable_strict(
            &ResolvedType::Class(child),
            &ResolvedType::Class(child),
            l
        ));
    }

    #[test]
    fn a_class_is_still_not_assignable_to_a_primitive() {
        // The lattice widens class-to-class only; the two shapes U6's firewall
        // keeps out of reach (a class into an INTEGER, a primitive into a
        // class-typed variable) must stay mismatches on their own merits.
        let mut t = SymbolTable::new();
        let cls = add_class(&mut t, "some-cls", SymbolKind::Class, false);
        let l = lattice_of(&t);
        assert!(!super::assignable(
            &ResolvedType::Class(cls),
            &p(PrimitiveTy::Integer),
            l
        ));
        assert!(!super::assignable(
            &p(PrimitiveTy::Integer),
            &ResolvedType::Class(cls),
            l
        ));
    }
}
