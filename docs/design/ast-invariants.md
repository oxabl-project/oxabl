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

Baselined 2026-04-17 (commit on `feat/ast-invariants-doc`) and maintained since — the file is current, not a snapshot. The *(Phase 1)* NodeId invariants have **landed**; §2 describes shipped behavior, including the two non-wrapper nodes (`StatementKind::Using`, `RunTarget::Literal`) that carry ids of their own so cross-file resolution has something to key a `references` entry on. No invariant below is aspirational any more — every one of them describes the tree the parser produces today, so a resolver that has to guard against a shape this file calls impossible has found a parser bug.

**Primary references:**

- `crates/oxabl_ast/src/statement.rs` — `Statement`, `PreprocIf<T>`, declaration variants.
- `crates/oxabl_ast/src/expression.rs` — `Expression`, `Identifier`, precedence comment.
- `crates/oxabl_ast/src/span.rs` — `Span { start: u32, end: u32 }`.
- `crates/oxabl_ast/src/comment.rs` — `Comment { span, kind }`, `CommentKind` (see §13).
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
- **Uniform-span coverage is guaranteed.** Every `Statement` and `Expression` wrapper carries
  a `span: Span` covering the node's full byte extent, seeded by the parser. A statement's span
  includes its trailing `.`/`:`; a parenthesized expression's span includes the enclosing
  parens (for round-trip fidelity). `Span::DUMMY` (zero-width) is the default on hand-built
  nodes and is legal on genuinely token-less synthetic recovery nodes (see the `start == end`
  bullet above). The wrapper `span` is **excluded from `PartialEq`** — structural equality
  compares `.kind` only, mirroring how `NodeId` is treated (§2). This slice targets
  no-preprocess mode, where the virtual offset equals the real source offset (the formatter's
  parse mode); the virtual-offset/`resolve` machinery above is unchanged. Data-type-level
  spans remain out of scope — a `DataType` is positioned within its owning statement's span.
- **A cross-file target's `name_span` covers the name, not the statement.** `StatementKind::Using`
  and `RunTarget::Literal` each carry a `name_span` alongside the target string: the byte extent
  of the *named target itself*, so a "could not be located" diagnostic points at the name rather
  than at the whole statement. For `USING` that extent stops at the last name segment (the `.*`
  wildcard is included; a trailing `FROM PROPATH` / `FROM ASSEMBLY` clause and the statement
  terminator are not). For a quoted `RUN` target the extent **includes the surrounding quotes**
  even though the stored `name` has them stripped, so the underline matches the literal as
  written — and, for the same reason, includes a trailing translation/width suffix (`:U`) when
  the literal carries one, which `name` likewise excludes. Unlike the wrapper `span` above, these are inline fields of a derived-`PartialEq`
  enum and therefore *do* participate in structural equality — see §2.
- **Span source order is asserted.** Where sibling `Statement`/`Expression` values are
  assembled (block bodies, the top-level program, argument/item lists), a `debug_assert!`
  enforces `prev.span.end <= next.span.start`: siblings are in source order and non-overlapping.
  The `<=` comparison tolerates zero-width (`start == end`) synthetic nodes abutting a
  neighbour. The check is debug-only, so release builds pay nothing.

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
- **Two non-wrapper nodes also carry a NodeId: `StatementKind::Using` and `RunTarget::Literal`.**
  Both name a cross-file target as a bare `String`, and workspace resolution records such a
  target as an entry in the `NodeId`-keyed `references` side table — so the target needs its own
  identity, distinct from the enclosing statement's. Ids come from the same parser allocator
  (`Parser::node_id()`), so the id space stays one dense range. `RunTarget::Dynamic` gets no id:
  it names nothing at parse time. **`Identifier` is deliberately id-free** — giving every
  identifier a NodeId would inflate the id space and the per-keystroke `NodeIndexVec`
  allocations to buy a uniformity no consumer needs.
- **Exception to the "ids are excluded from `PartialEq`" rule.** `StatementKind` and `RunTarget`
  derive `PartialEq`, so the `id` and `name_span` fields inside those two variants *are* compared.
  Whole-value equality against a hand-built target therefore does not hold; compare the
  `type_name` / `name` field instead (`RunTarget::literal_name()` is the accessor, and
  `RunTarget::literal(name)` builds a `DUMMY`-id target for hand-constructed test AST). Making
  these fields equality-invisible would mean hand-writing `PartialEq` for the whole of
  `StatementKind`, which is not worth the maintenance surface.
- Recovery-generated `Statement { kind: StatementKind::Empty, .. }` nodes still get a NodeId
  like any other. Side tables (the future `references` / `types` in `oxabl_semantic`) are
  allowed to be absent at those NodeIds — consumers treat "no entry" as "not analyzed."
- The same holds for `StatementKind::Skipped`, but "not analyzed" is *all* it means there.
  A `Skipped` node's harvested names never produce a `references` entry even when they
  resolve — the resolve pass credits them through a lookup that writes no side table (§8).
  So absence of an entry at a `Skipped` NodeId does not imply the names were unresolvable,
  and a rule that reads `references` must not conclude anything from the silence.

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

## 8. `Statement::Empty` is recovery; `Statement::Skipped` is unmodelled

Two distinct "the parser produced no structure here" cases, kept apart on purpose.

**`Empty` — nothing was recognized.**

- When `Parser::parse_statement` fails, `parse_program` records the error in `Program.errors`
  and calls `synchronize()` to advance to the next statement boundary
  (`crates/oxabl_parser/src/parser/mod.rs:119-145`, `153-164`).
- Fine-grained recovery sites inside `parse_statement` — `crates/oxabl_parser/src/parser/statements.rs`
  uses `return Ok(Statement::Empty)` at roughly a dozen recovery points — emit
  `Statement::Empty` in lieu of the construct that failed to parse. The parser never emits a
  partially constructed declaration or a "truncated" node.
- **Invariant:** any `Statement::Empty` in the tree was produced by recovery, by a genuinely
  empty statement (a bare period, an empty `ELSE` branch), or by a form the parser consumes
  token-by-token without a skip helper (`QUIT`, a bare `END`, a stray `&ENDIF`, an `ENUM`
  body). Consumers may rely on this: an `Empty` node carries no
  user-facing declaration, reference, or expression.
- Recovery-generated `Empty` nodes still get a NodeId. The semantic side tables
  (`references`, `types`) are allowed to be `None` at those NodeIds. See Flow-gap F5 in the
  v1 plan addendum.

**`Skipped { names, may_reference_tables }` — a form was recognized and then discarded.**

- Around thirty statement forms are matched by their leading keyword and then skipped
  wholesale by one of the four skip helpers (`skip_to_period`, `skip_to_statement_end`,
  `skip_to_statement_end_editing_aware`, `skip_to_statement_end_triggers_aware`): `PUT`,
  `EXPORT`, `UPDATE`, `SET`, `ENABLE`, `DISABLE`, `APPLY`, embedded SQL, and the rest.
- **Invariant:** these emit `Statement::Skipped`, never `Empty`. The distinction is
  load-bearing — these forms carry real variable traffic in both directions, so a consumer
  that reasons about whether a variable was touched must not read them as "nothing happened".
  Until this variant existed, §8 asserted `Empty` meant recovery, and roughly thirty forms
  quietly falsified it.
- `names` holds the identifier-shaped tokens the skip passed over, filtered lexically: the
  dispatch keyword is dropped, as is any token byte-adjacent to a preceding `.`, `:` or `/`.
  The filter is deliberately broad and admits unreserved option keywords as candidate names —
  ABL lexes a user variable named `value` as `Kind::Value`, so the parser cannot tell them
  apart inside a statement whose grammar it does not model. `names` may be empty.
- `names` is a **best-effort lexical harvest, not a reference list.** Consumers must resolve
  it through a path that records nothing (see `lookup_statement_ident` in
  `oxabl_semantic::resolve`) and must not emit a diagnostic for a name that fails to resolve.
- `may_reference_tables` is `false` on every ordinary unmodelled form. Only the three forms
  whose grammar names a table set it: `DEFINE QUERY`, `OPEN QUERY`, and `EMPTY TEMP-TABLE`.
  `EMPTY TEMP-TABLE` is the exception to the lexical-harvest rule above: the parser walks its
  grammar token by token, so it contributes exactly the one identifier it knows to be the
  table — never the `TEMP-TABLE` keyword or a trailing `NO-ERROR`. It emitted a recovery
  `Empty` before #130, which is why the `Empty` list above no longer names it.
  **Invariant:** it selects an *additional* lookup, never a replacement — a marked node still
  gets the value-namespace treatment every `Skipped` node gets, and the two paths are
  independent, so a name that resolves in both namespaces is credited on both sides.
- The two lookups record different things, and that asymmetry is deliberate. The value side
  records uncertainty only (`TOUCHED_BY_UNMODELLED_STATEMENT`) and leaves counts exact,
  because the parser cannot tell a read from a write inside a grammar it does not model. The
  table side increments a real `read_count`, because every form carrying the marker reads its
  table in every spelling the grammar admits. **Invariant:** neither path writes a
  `references` entry, and neither emits a diagnostic on a miss — the harvest is too broad to
  be evidence of a defect in either direction.
- The statement's full extent is `Statement.span`; there is no companion `raw_span`.
- The variant is scaffolding with a scheduled successor (#136): each form that gets
  head-parsed stops emitting `Skipped`.

**Two forms have left the `Skipped` population, and they left in opposite directions.**

- **`DELETE OBJECT` is head-parsed** into `StatementKind::DeleteObject { target, no_error }`.
  Its own variant rather than a spelling of `Delete`, because the operand is an
  **`Expression`**: `DELETE OBJECT ttbl:HANDLE.` and `DELETE OBJECT hArray[i].` are both
  ordinary ABL and neither fits `Delete`'s `buffer: Identifier`. That mismatch is exactly why
  the form skipped, and the skip was expensive — the harvest marked every name it passed over
  `TOUCHED_BY_UNMODELLED_STATEMENT`, silencing the count-gated rules for the whole file over a
  statement whose one operand is perfectly parseable.
  **Invariant:** `target` is resolved like any other expression, so the handle it names is
  credited an ordinary `read_count`, and no name in the statement is marked. Note the
  asymmetry this removes: `DELETE PROCEDURE`, `DELETE WIDGET`, and `DELETE SERVER` already
  fell through to a real `Delete` node — only the `OBJECT` spelling skipped.

- **`COMPILE` still emits `Skipped`, with an empty `names` list.** Its operand is a *file
  path* and its trailing words are grammar keywords, so no identifier in it is a symbol
  reference. Harvesting them credited nothing true while actively suppressing real variables
  whose names collided with a path word or with `SAVE`.
  **Invariant:** a form whose operands are paths or literals gets its harvest deleted rather
  than head-parsed. That is the second of the two shapes #136 chooses between: symbol-shaped
  operands earn a head-parse, path-shaped ones earn an empty name list.

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

## 13. Comment side-table (`Comment` / `CommentKind`)

`Comment { span: Span, kind: CommentKind }` (`Copy`) and `CommentKind { Line, Block }` are the
trivia vocabulary the future `oxabl_formatter` consumes. The comment *table* itself —
`Vec<Comment>` — is not an `oxabl_ast` type; it lives on `Program` in `oxabl_parser`, populated
in a single linear pass over the token slice at the end of `parse_program`. The vocabulary lives
in `oxabl_ast` (alongside `Span`) so the formatter depends on this crate only for the types.

- **Sorted and source-ordered.** The table is built by filtering the already-source-ordered
  token slice for `Kind::Comment`, so entries are ascending by `span.start` with no explicit
  sort.
- **Non-overlapping spans.** Comment spans are lexer token spans and never overlap each other
  or the node spans of the statements/expressions they sit between.
- **Admits only `//` / `/*` / `&`-origin trivia.** No current lexer route emits any other
  `Kind::Comment` shape: line comments (`//`) and AppBuilder `&`-directive lines classify to
  `CommentKind::Line`, block comments (`/* */`, nested-aware) classify to `CommentKind::Block`.
  The classifier `debug_assert!`s on any leading byte outside `/`/`&`/`{`, so a future lexer
  change that produces a new `Comment` shape trips the assert rather than silently dropping a
  comment. Include/preprocessor `{...}` references (`IncludeReference`/`IncludeArgReference`,
  `{&macro}`) are first-class AST nodes and never enter the comment path; unterminated block
  comments lex to `Kind::Invalid` and are excluded.
- **Text is not stored.** Only the span is kept; the formatter derives the comment text
  verbatim from source by span at format time. The type is therefore `Copy`.
- **Advisory only.** The table is fidelity data for the formatter; semantic, lint, and analyze
  passes never read it, so populating it changes no downstream behavior.

**Span-end convention (pinned).** The bytes a comment span owns differ by source shape, and
Slice 3's verbatim-by-span re-emit and blank-line gap math depend on knowing which:

- a `//` line comment span **includes** its trailing `\n` (the lexer's `skip_line_comment`
  consumes it);
- a `/* */` block comment span covers the full extent through `*/` and **excludes** any
  following newline;
- an AppBuilder `&`-directive line span **excludes** its trailing `\n` (the lexer stops before
  it).

Both `//` and `&`-directive comments are `CommentKind::Line` despite this tail asymmetry.

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
