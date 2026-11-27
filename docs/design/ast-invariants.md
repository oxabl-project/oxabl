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
- **A `USING` source clause is semantic AST data.** `StatementKind::Using::source`
  is `Unspecified`, `Propath`, or `Assembly`. The parser never discards a valid
  `FROM PROPATH` / `FROM ASSEMBLY` clause, because workspace resolution must not
  search source paths for an assembly-only type. Like the other inline fields,
  `source` participates in `StatementKind` structural equality.
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
- Ids are post-order: `Parser::stmt(kind)` and `Parser::expr(kind)` allocate after the `kind`
  (and so every child) is built. The last top-level statement therefore holds the highest id
  in the tree, and `NodeIndexVec::for_program` sizes the semantic side tables from it once.
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
- **A property's accessor spans are inline fields.** `StatementKind::Property` carries
  `get_span` and `set_span`: each covers `GET` or `SET` up to the period after its own `END`
  (or after `GET.`/`SET.`), with no access modifier, in expanded-text offsets. Like
  `name_span`, they take part in the derived `PartialEq`, so two parses of a property that
  differ only in whitespace are not equal.
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

- `Identifier.name` is a `NameId` into the program's `NamePool` (§14). The text it names
  keeps the **source casing** of the identifier; the pool does no case folding.
- **Escape-marker normalization is a property of `Identifier`, not of every name in the
  tree.** An `Identifier` built through the parser's identifier helper drops UNIX-shaped
  escape markers, because they are not part of the name the compiler sees: `ab\cd` has
  authored span `ab\cd` and AST name `abcd`. A name the parser instead slices verbatim from
  source keeps its authored spelling — most importantly `RunTarget::Literal`, and
  **deliberately so**: its operand is a file path, where a backslash is a path separator
  rather than an escape, so normalizing it would corrupt the path the author wrote. The
  consequence is that a `RUN` whose target name carries an escape marker does not link to
  the like-named procedure declaration, which is a known and accepted gap rather than an
  invariant violation. Do not read this bullet as licence to normalize a raw-sliced name
  without deciding which of the two cases it is.
- Case-insensitive comparison is the job of downstream consumers (lexer keyword matching,
  future `oxabl_semantic` symbol lookup). Those consumers use ASCII-folded byte comparison
  (`eq_ignore_ascii_case`) or atom interning with case folding at intern time — see
  `CLAUDE.md` § Lexer for the no-heap-allocation rule. A shared helper will live in
  `oxabl_common::atom` when semantic lands; it does not exist today.
- Equivalently: name equality is **case-sensitive**. The interner gives two spellings that
  differ only in case two different `NameId`s. Test fixtures that compare a name must read
  its text through the pools and use the source casing.

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
  `Expression::PreprocIf(Box<PreprocIf<ExprId>>)` (its branches are `ExprId`s, §14), and
  (per the plan) a future `DataType::PreprocIf(Box<PreprocIf<DataType>>)`.
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
  `skip_to_statement_end_editing_aware`, `skip_to_statement_end_triggers_aware`):
  `READKEY`, `NEXT-PROMPT`, `STATUS`, embedded SQL, and the rest.
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
  asymmetry this removes: `DELETE PROCEDURE`, `DELETE WIDGET`, and `DELETE SERVER` used to
  fall through to a record `Delete` node — only the `OBJECT` spelling skipped.

- **The handle-lifetime statements are head-parsed too.** `DELETE OBJECT | PROCEDURE |
  WIDGET | SERVER` all become `DeleteObject { kind, target, extra_targets, .. }`
  (`extra_targets` holds the further handles of `DELETE WIDGET h1 h2`). `CREATE` of every
  dynamic-object and widget type becomes `Create { target: CreateTarget::Handle { .. } }` with
  `handle` an `Expression`, plus `class` (`VALUE(expr)` or a COM class literal), the widget
  `ASSIGN` pairs, the `TRIGGERS` phrase and the COM `CONNECT` clause. `CREATE`/`DELETE
  WIDGET-POOL`, `RELEASE OBJECT` and `APPLY` have their own variants.
  **Invariant:** a `CREATE` whose tail does not parse falls back to `Skipped` from the first
  token after `CREATE`, so it never swallows the next statement. The parse error stays in the
  file, so the file has a `PARSE001` and the formatter leaves it alone. When the tail runs to
  the end of the text, the error is placed at the `CREATE` keyword. A `CREATE` form that
  oxabl does not try to model (`CREATE ALIAS`, `CREATE DATABASE`, a type keyword it does not
  know) is valid ABL and is skipped with no error. The parser keeps at most 50 errors
  (`MAX_ERRORS`) and then stops. A file with 50 failed `CREATE` statements loses the rest
  of its tree. A widget `TRIGGERS`
  body is a deferred trigger: semantic walks it in a `Trigger` scope, `oxabl_flow` does not
  inline it. The trigger `PERSISTENT RUN` takes a name or `VALUE(expr)` as its target
  (`PersistentRunTarget`); the readers walk the expression of `VALUE`.
  `APPLY` operands are read without a miss diagnostic, because a bare name there is usually a
  static widget or frame, not a symbol.

- **`COMPILE` still emits `Skipped`, with an empty `names` list.** Its operand is a *file
  path* and its trailing words are grammar keywords, so no identifier in it is a symbol
  reference. Harvesting them credited nothing true while actively suppressing real variables
  whose names collided with a path word or with `SAVE`.
  **Invariant:** a form whose operands are paths or literals gets its harvest deleted rather
  than head-parsed. That is the second of the two shapes #136 chooses between: symbol-shaped
  operands earn a head-parse, path-shaped ones earn an empty name list.

- **`PUT`, `EXPORT`, `IMPORT`, `PAUSE`, `DOWN`, `UP`, `FORM`, `UPDATE`, `SET`, `ENABLE`,
  `DISABLE`, `HIDE`, `VIEW`, `APPLY`, `WAIT-FOR`, `PROMPT-FOR`, `GET`, `CLOSE`, `REPOSITION`, `CLEAR`, `ACCUMULATE`, `COLOR` and `COPY-LOB` are head-parsed** into their own
  variants. The stream, the delimiter, the item expressions, the `PAUSE` duration
  and message and the `DOWN`/`UP` line count are modelled; the option clauses after them
  (`FORMAT`, `EXCEPT`, `NO-LOBS`, `IN WINDOW`, the frame phrase, ...) ride on
  `Statement::skipped_tail`. A `FORM` item list ends at the first token that does not
  start an item (`VIEW-AS`, `AT ROW`, an unclassified option such as `BGCOLOR 3`), so an
  option the parser does not know is never read as a name. A `FORM` item `name AS type`
  or `name LIKE field` declares a frame field: it goes to `fields`, not `items`, and the
  list goes on. In the other forms it still ends the list.
  **Invariant:** an `IMPORT` or `SET` target is credited as a write, an `UPDATE` target as
  a read and a write, and every other head operand (a `PROMPT-FOR` target too) as a read. The query of `GET` and `REPOSITION` and the target of `CLOSE QUERY` or `CLOSE STORED-PROCEDURE` are reads that make no diagnostic on a miss, because a static query or stored procedure name is not a declared symbol. The source of `COPY-LOB` is a read and its target, when it is not `FILE`, a read and a write (an `APPEND` or `OVERLAY` reads it). The color words of `COLOR` are not symbols and are dropped; a `DISPLAY` and a `PROMPT` phrase may both be given. A `COLOR` that is not `COLOR [STREAM s] DISPLAY | PROMPT`, or whose color does not parse, stays `Skipped`. The fields of `COLOR` name widgets: the null analysis does not count them as record reads, but a `VALUE(...)` color is one. An `ACCUMULATE` whose expression is not one name or an expression that ends before the parenthesis stays `Skipped`. The frame of `CLEAR FRAME f` is a read in the frame name space. The `field = PROC-STATUS` of `CLOSE STORED-PROCEDURE` is a write. A head the parser cannot model (`STREAM-HANDLE h`) leaves the rest of the statement
  in the tail; such a form does not fall back to `Skipped` (the `GET`, `CLOSE`, `REPOSITION`, `ACCUMULATE`, `COLOR` and `COPY-LOB` cases above do).

- **Frame fields are declarations.** The `name AS type` / `name LIKE field` items of a
  `FORM` and of a `DEFINE FRAME` body are `FrameField`s in the variant's `fields`. The
  rest of a `DEFINE FRAME` body stays unparsed. The semantic layer declares each one as a
  variable, scoped like `DEFINE VARIABLE`, unless a variable of that name is already in
  scope.

- **Static widget definitions declare their name.** `DEFINE BUTTON`, `BROWSE`,
  `RECTANGLE`, `IMAGE`, `MENU` and `SUB-MENU` parse to `StatementKind::DefineWidget`, and
  the name is declared in the value namespace as a `SymbolKind::Widget`, so a `FORM` or
  `DISPLAY` item naming the widget resolves. The rest of the definition is a skipped tail.

**`Statement::skipped_tail` — a modelled statement discarded a trailing range.**
`DISPLAY … WITH FRAME f TITLE x`, `RETURN ERROR expr`, the `VIEW-AS` tail of a `DEFINE
VARIABLE`, `RUN` trailing content, `ASSIGN FRAME f`, `FUNCTION … IN super`, `DEFINE
STREAM`/`DEFINE FRAME` options and the stream-I/O option tails keep their real
`StatementKind`. The identifiers of the range they skipped ride on the wrapper, not the
kind.

- **Invariant:** the field is `None` unless the lexical filter kept at least one name or the
  parser dropped a range (`SkippedTail::dropped`), and it never changes which
  `StatementKind` a form produces.
- `SkippedTail::dropped` holds the ranges that the parser read and did not keep: a `FIELDS`
  or `EXCEPT` list, an expression it parsed and dropped, the tokens a `DEFINE TEMP-TABLE`
  skips. The next statement built takes them, so a range can ride on a statement inside the
  block that it belongs to. The names in them are not harvested and not credited. Only the
  navigation gap of the references search reads them.
- **Invariant:** the names get the same treatment as a `Skipped` node's — a silent,
  non-counting lookup that can only mark a symbol as unjudgeable. They never produce a
  `references` entry, so they cannot reach LINT0001 or LINT0003, and they are not offered to
  the table namespaces.
- The tail's class-shaped runs (`name:member`, `NEW name(`) are offered to the class lookup,
  as a `Skipped` node's are, from the first harvested name to the end of the skipped range
  (`SkippedTail::span`, the bytes from its first token to its last). A run that
  resolves is a dependency edge; one that cannot be told from a handle or a field degrades
  the edge set.
- The field is excluded from `PartialEq`, like `id` and `span`.
- A `FOR` header keeps one buffer. Its joined tables and its `BY`, `WHILE` and `WITH`
  clauses are a skipped tail, from the end of the first buffer's phrase to the block
  opener, so a name in them is credited like the names of any other tail.

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

Every `DefineParameter` of a parenthesized list (`METHOD`, `CONSTRUCTOR`, `FUNCTION`, `DEFINE EVENT`,
`SET`) has a real `span`: from its direction, or its first token, to its last token.

A `TempTableField` has a `span` too: from `FIELD` to its last option.

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
- An error at the end of the text (a block with no `END.`) has no byte to point at. The
  collector reports it at the end of the root file; when the expansion ends in an include,
  that is the end of the include site. An include fragment analysed on its own is exempt: a
  fragment that opens a block is valid, because the file that includes it closes the block
  (the same exemption as `handle_support::not_judged`). A parse error inside an include file
  has no position in the root file. It is reported at the `{...}` site of the include in the
  root (the outermost site, for a nested include), with the file and the position in the
  include in its message, and also in the include file itself as a finding of include origin.
  It is never dropped: when no site leads to the text, it sits at the end of the root. An
  error in text that no file wrote (a `{&name}` value) is reported at the site only, and its
  message says "in the value of a preprocessor name or an include argument". An include fragment analysed alone holds its own errors in its own bytes.

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

## 14. Names and expressions live in the program's pools

`Program.pools` (`AstPools`) holds the text of every name and every expression node of one
parse. The tree holds only handles into it.

- **`NameId`** (4 bytes) is a handle into `NamePool`. `Identifier.name`, the value of a
  `StringLiteral` and the other names of the tree are `NameId`s. Read the text with
  `pools.name(id)` or `pools.ident(identifier)`. While it parses, the parser interns each
  text once (`NameInterner`, case-sensitive); `finish` drops the hash table, so a kept tree
  has no map.
- **`ExprId`** (4 bytes) is a handle into `ExprPool`. Every place that held an `Expression`,
  a `Box<Expression>` or an `Option<Expression>` holds an `ExprId` or an `Option<ExprId>`.
  Read the node with `pools.expr(id)`.
- **`ExprList`** (8 bytes, a start and a length) is a run of `ExprId`s in the pool. Every
  place that held a `Vec<Expression>` holds an `ExprList`. Read it with `pools.exprs(list)`.
  An empty list is `ExprList::EMPTY` and holds no storage.
- **A handle is valid only with the pools of its own parse.** A handle from one `Program`
  read through the pools of another gives a wrong node or a panic. Code that keeps
  statements of several files (the workspace index, the daemon, a cross-file view) keeps each
  file's pools beside its statements and passes them together. An analysis entry point takes
  `pools: &AstPools` directly after `program: &[Statement]`, and `AnalysisContext` carries
  the pools of the file it analyses.
- **`NameId::DUMMY` and `ExprId::DUMMY`** are placeholders for hand-built trees. They are
  in no pool: `get` panics on them and `try_get` gives `None`.
- **Hand-built trees** (unit tests) put their names and expressions in an `AstPools` of
  their own and give that pool to the analysis. `AstPools::new()` makes no allocation.
- **Sibling order.** The ids of an `ExprList` are in source order, as statement siblings
  are (§1); a debug build asserts this for each list the parser stores.

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
