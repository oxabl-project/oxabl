# AST Invariants

This document enumerates the properties the `oxabl_ast` tree guarantees to its consumers —
primarily the coming semantic layer (`oxabl_semantic`, `oxabl_lint`, `oxabl_analyze`) but also
the existing parser test suite and any future formatter / LSP.

Every invariant below is load-bearing for at least one downstream pass. **Violations are
parser bugs, not resolver bugs.** If the resolver needs to defensively guard against a shape
this doc calls out as impossible, the right answer is to file a parser fix, not a resolver
workaround.

Future invariant changes must edit this file in the same PR. Reviewers enforce that.

---

## Scope and status

Baseline invariants as of 2026-04-17 (commit on `feat/ast-invariants-doc`). This pre-dates
Phase 1 of the semantic-layer v1 plan — invariants marked *(Phase 1)* land when NodeIds are
added to the AST. Invariants marked *(aspirational)* are contracts the semantic layer
currently has to code defensively around; they become targets for follow-up hardening.

**Primary references:**

- `crates/oxabl_ast/src/statement.rs` — `Statement`, `PreprocIf<T>`, declaration variants.
- `crates/oxabl_ast/src/expression.rs` — `Expression`, `Identifier`, precedence comment.
- `crates/oxabl_ast/src/span.rs` — `Span { start: u32, end: u32 }`.
- `crates/oxabl_parser/src/parser/mod.rs` — `Program`, `parse_program`, `synchronize`.
- `crates/oxabl_parser/src/parser/statements.rs` — `Statement::Empty` recovery sites.
- `crates/oxabl_preprocessor/src/span_tree.rs` — `PreprocessedFile::resolve`.

---

## 1. Span invariants

- `Span` is a byte range `[start, end)` into the **post-preprocessor expanded text** produced
  by `PreprocessedFile::to_text`. `end >= start`. Both fields are `u32`.
- `start == end` is legal for synthetic / zero-width nodes (for example, a `Statement::Empty`
  emitted by recovery at a period).
- The only translation from a virtual (expanded-text) offset to a real source
  `(FileId, Span)` is `PreprocessedFile::resolve(offset) -> FileSpan`. Consumers must not
  fabricate `FileSpan`s. The semantic layer will formalize this by wrapping expanded-text
  offsets in a `VirtualSpan` newtype; until then, treat raw `u32` offsets as virtual.
- **Uniform-span coverage is aspirational.** Only `Identifier`, `Statement::IncludeReference`,
  `Statement::IncludeArgReference`, `Statement::DefineFrame { raw_span }`, and
  `Statement::DefineEvent { value_span }` carry explicit spans today. Most `Statement` and
  `Expression` variants do not — a resolver that wants a span for diagnostics must reach into
  a known sub-node (e.g. the declaration's `Identifier.span`) or accept approximate
  coordinates. Hardening this is a follow-up (tracked as part of the "every node has a
  span" promise in the v1 semantic plan, likely resolved together with the Phase 1 NodeId
  rollout or shortly after).
- Span source order: where a node *does* carry a span, it covers only its own tokens; sibling
  spans in a `Vec<Statement>` are expected to be in source order and non-overlapping. This is
  not currently asserted and should not be relied on as a hard invariant until a
  `debug_assert!` enforces it.

## 2. NodeId invariants

Both `Statement` and `Expression` carry a stable `NodeId` as of Phase 1
(`crates/oxabl_ast/src/node_id.rs`).

- `NodeId(u32)` is a public, `Copy + Eq + Hash` handle. `NodeId::PROGRAM == NodeId(0)` is
  reserved for the `Program` root; `NodeId::DUMMY == NodeId(u32::MAX)` is reserved for
  hand-constructed nodes (tests, AST builders). `DUMMY` must never appear in a parser-produced
  tree.
- `NodeIdAllocator::new()` starts allocation at `NodeId(1)` and is monotonic: `alloc()`
  yields dense, unique, contiguous ids. The `Parser` owns one allocator per parse, shared by
  statement and expression allocation so the id space is a single dense range.
- `Statement { id, kind }` and `Expression { id, kind }` are wrapper structs; the original
  enums are `StatementKind` and `ExpressionKind`. `Statement::new(kind)` / `Expression::new(kind)`
  construct with `id = NodeId::DUMMY` for tests; the parser uses the `&mut self` helpers
  `Parser::stmt(kind)` and `Parser::expr(kind)` to allocate real ids.
- `PartialEq` on both wrappers is **implemented manually** to ignore `id`: structural value
  equality (`self.kind == other.kind`) is preserved. Cross-type `PartialEq<StatementKind> for
  Statement` and `PartialEq<ExpressionKind> for Expression` (and their symmetric partners) let
  tests assert against a bare `...Kind` value. No compare-ignoring helper is required at call
  sites.
- Recovery-generated `Statement { kind: StatementKind::Empty, .. }` nodes still get a NodeId
  like any other. Side tables (the future `references` / `types` in `oxabl_semantic`) are
  allowed to be absent at those NodeIds — consumers treat "no entry" as "not analyzed."

## 3. Identifier casing

- `Identifier.name: String` preserves the **exact source casing** of the identifier as it
  appeared in the input text. The AST layer performs no case folding.
- Case-insensitive comparison is the job of downstream consumers (lexer keyword matching,
  future `oxabl_semantic` symbol lookup). Those consumers use ASCII-folded byte comparison
  (`eq_ignore_ascii_case`) or atom interning with case folding at intern time — see
  `CLAUDE.md` § Lexer for the no-heap-allocation rule. A shared helper will live in
  `oxabl_common::atom` when semantic lands; it does not exist today.
- Equivalently: `Identifier` field equality in `PartialEq` is **case-sensitive**. Test
  fixtures that assert identifier equality must use the source casing.

## 4. Operator precedence is baked into tree shape

- Expressions are parsed with the precedence documented at the top of
  `crates/oxabl_ast/src/expression.rs`:

  `ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative > unary > postfix > primary`

- The tree shape *is* the precedence. A consumer does not need to reparse or consult a
  precedence table to know that `a + b * c` associates as `Add(a, Multiply(b, c))`.
- Comparison operators are non-associative in the grammar; chained comparisons
  (`a < b < c`) do not parse as a single node.

## 5. Postfix-chain left-nesting

A postfix chain `expr.f:m(a)[i].g` nests so that each step wraps the accumulated prefix:

- `FieldAccess { qualifier: <prefix>, field }`
- `MemberAccess { object: <prefix>, member }`
- `MethodCall { object: <prefix>, method, arguments }`
- `ArrayAccess { array: <prefix>, index }`

Reading the chain from outside in yields the steps in **right-to-left source order**; reading
inside out yields left-to-right source order. Semantic walkers that care about the receiver
should bottom out on the innermost non-postfix expression.

## 6. `PreprocIf<T>` never commits to preprocessor truth

- `PreprocIf<T>` appears at three levels: `Statement::PreprocIf(PreprocIf<Vec<Statement>>)`,
  `Expression::PreprocIf(Box<PreprocIf<Expression>>)`, and (per the plan) a future
  `DataType::PreprocIf(Box<PreprocIf<DataType>>)`.
- Every branch (`then_branch`, each `elseif_branches` branch, `else_branch` if present) is
  **parsed as though it were reachable**. The AST carries all branches; it does not evaluate
  the `&IF` condition.
- For `Expression::PreprocIf`, the parser enforces that `else_branch` is `Some(_)` (an
  expression-level `&IF` without an `&ELSE` has no defined value). Statement-level
  `else_branch` is optional.
- Consumers that must commit to a branch (type checking, lint) pick one conservatively and
  should document the choice. The semantic layer v1 walks all branches for symbol
  declaration (union) and picks the `then_branch` for type inference to avoid combinatorial
  blowup, treating mismatches between branches as `Unknown`.

## 7. Declaration nodes have a populated `Identifier`

- Every declaration variant — `VariableDeclaration`, `Procedure`, `DefineTempTable`,
  `DefineBuffer`, `DefineStream`, `DefineFrame`, `DefineEvent`, `Method`, `Property`,
  `Constructor` *(no name; uses enclosing class)*, `Class`, `Interface`, etc. — carries its
  own name via an `Identifier` field (or by borrowing the enclosing class name in the case of
  `Constructor`/`Destructor`).
- `Identifier.name` is never empty on a successfully parsed declaration. If the parser fails
  to find an identifier where one is required, the enclosing `parse_statement` returns an
  error and emits `Statement::Empty` on recovery (see §8); the partial declaration is not
  committed to the tree.

## 8. Error recovery uses `Statement::Empty`

- When `Parser::parse_statement` fails, `parse_program` records the error in `Program.errors`
  and calls `synchronize()` to advance to the next statement boundary
  (`crates/oxabl_parser/src/parser/mod.rs:119-145`, `153-164`).
- Fine-grained recovery sites inside `parse_statement` — `crates/oxabl_parser/src/parser/statements.rs`
  uses `return Ok(Statement::Empty)` at roughly a dozen recovery points — emit
  `Statement::Empty` in lieu of the construct that failed to parse. The parser never emits a
  partially constructed declaration or a "truncated" node.
- **Invariant:** any `Statement::Empty` in the tree was produced by recovery or by the bare
  period at end-of-file. Consumers may rely on this: an `Empty` node carries no user-facing
  declaration, reference, or expression.
- When Phase 1 lands, recovery-generated `Empty` nodes still get a NodeId. The semantic
  side tables (`references`, `types`) are allowed to be `None` at those NodeIds. See Flow-gap
  F5 in the v1 plan addendum.

## 9. Property body distinguishes absence from emptiness

`Statement::Property` represents the three GET/SET accessor shapes with
`Option<Vec<Statement>>`:

- `get_body: None` — no `GET` accessor.
- `get_body: Some(vec![])` — auto-getter (`GET.`). Backed by the implicit backing field.
- `get_body: Some(body)` — computed getter (`GET: body END GET.`).

The same pattern applies to `set_body`. The `None` / `Some(vec![])` / `Some(body)` tri-state
is observable and load-bearing: lint rules that need to tell "no getter" from "trivial
auto-getter" must not collapse them. (`Method.body`, in contrast, is always `Vec<Statement>`,
empty if the method is `ABSTRACT`; the "no body" case is carried by the `is_abstract` flag.)

`set_parameters: Vec<Statement>` holds the optional parenthesized SET parameter list
(`SET (INPUT pv AS CHARACTER):`). Entries are `DefineParameter` statements (same shape as
method parameters). Empty when the setter has no parameter list. Parameters bind in the
`PropertySet` scope so the computed SET body can reference them.

## 10. `Program.errors` non-empty does not invalidate `Program.statements`

- `Program.statements` contains every successfully-parsed statement in source order, even
  when `Program.errors` is non-empty. Recovery guarantees the two lists are independently
  meaningful.
- Semantic and lint passes run against `Program.statements` regardless of
  `Program.errors.is_empty()`. Poisoning the semantic output on parse error is explicitly
  not done; a file with ten parse errors can still have a useful partial symbol table and
  lint report for the recognized statements.
- Consumers that need the "clean" check use `Program::is_ok()` (returns `errors.is_empty()`)
  — for example, a CI pre-commit hook. Downstream analysis passes do not.

## 11. Ancillary invariants

- **No AST mutation after parse.** The AST is constructed once and read many times. Semantic
  analysis writes to *side tables* keyed by NodeId, never to the AST. This is the single
  design decision that keeps incrementality (Salsa) and cross-file analysis reachable — see
  the v1 semantic plan.
- **UTF-8 BOM and CRLF.** Input normalization happens at the `FileSystem::read` boundary, not
  the AST. A BOM-stripped, LF-normalized string is what the lexer (and later the `.df`
  parser) receive. AST offsets are relative to that normalized string.
- **Binary files misnamed `.p`.** The lexer emits parse diagnostics; `parse_program` still
  returns a `Program` with whatever statements recovery produces. Downstream passes run
  unchanged.

---

## 12. SHARED / NEW SHARED declaration flags

`VariableDeclaration`, `DefineTempTable`, `DefineBuffer`, and `DefineDataset` each carry
`is_shared`, `is_new_shared`, and `is_new_global_shared` booleans capturing the
`[NEW [GLOBAL]] SHARED` prefix of the originating `DEFINE`. All four variants carry the
identical triple — there is no longer a two-flag odd-one-out.

- **At most one is true.** The parser's capture grammar
  (`crates/oxabl_parser/src/parser/statements.rs`, the `NEW [GLOBAL] SHARED` block) produces
  `SHARED` (consumer), `NEW SHARED`, or `NEW GLOBAL SHARED` (producers) as mutually exclusive
  alternatives — the `GLOBAL` and non-`GLOBAL` producer paths are an `if`/`else`, and the
  consumer `is_shared` is guarded by both producer flags being false. A tree with two of these
  `true` on one node is a parser bug.
- **Non-shared defines set all three `false`** — the common case; behavior is identical to
  before these flags existed.
- **Semantic contract:** the declare pass maps these to `SymbolFlags::SHARED` / `NEW_SHARED` /
  `NEW_GLOBAL_SHARED` respectively via `shared_flags` (`flag_if` per bit). The flags are
  metadata; within a single file they do not change symbol resolution — a consumer's `SHARED`
  declaration still declares the symbol locally in its own scope.
- **`rebinding_scopes` is not populated by within-file analysis.** It is reserved for
  cross-file work that re-links a `SHARED` consumer to a `NEW SHARED` producer across files.
  Within-file v1 sets flags only.
- **Only these four `DEFINE` subtypes carry the flags.** A `SHARED` prefix on other subtypes
  (STREAM, FRAME, QUERY, WORKFILE, …) is still consumed and discarded — no regression from
  prior behavior, but not represented in the model.
- `DefineDataset` previously stored only `is_shared` / `is_new_shared` and collapsed
  `NEW GLOBAL SHARED` into `NEW_SHARED`. This change adds its third flag in lockstep so the
  `GLOBAL` distinction is observable and no dataset form regresses — all four `DEFINE`
  variants now behave identically.

---

## Debug assertions

Parser code opportunistically asserts a subset of these invariants via `debug_assert!` — for
example, `Parser::new` asserts that the token slice contains at least an EOF token. The
policy is: add a `debug_assert!` when the cost is genuinely zero and the invariant is
nontrivial to re-derive. Do not enumerate all invariants as asserts; the goal is a doc, not
a runtime contract.

Concrete follow-ups likely worth an assertion:

- Span end ≥ span start wherever a `Span` is constructed.
- NodeId allocator exhaustion (already asserted via `debug_assert!` in
  `NodeIdAllocator::alloc`).
- Parser cursor forward progress in `parse_program`'s recovery loop (already implicitly
  enforced by the `pos_before == self.current` force-advance; converting that branch to a
  `debug_assert!` once the force-advance is proven unreachable is the cleaner shape).

---

## Change control

- **Scope:** any change to `oxabl_ast/src/*.rs` that adds, removes, or reshapes a public type
  is an invariant change and must touch this doc.
- **Non-scope:** adding a new downstream consumer that merely reads the AST does not require
  a doc update — that is what the doc exists for.
