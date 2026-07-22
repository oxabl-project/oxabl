---
title: "Flow Analysis Sketch (R11)"
status: draft
date: 2026-04-17
parent: docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md
---

# Flow Analysis Sketch (R11)

## Purpose

This appendix proves that R11 ("flow analysis — definite assignment, dead
code, reachability — remains reachable after v1 without an IR rewrite") is
viable. The v1 architecture commits to two facts that make a control-flow
graph (CFG) a local, additive extension:

1. Every `Statement` and `Expression` carries a stable parser-assigned
   `NodeId` (Phase 1).
2. `StatementKind::Block(Vec<Statement>)` / block bodies preserve source
   order, so a statement's textual-successor relationship is trivially
   reconstructible from the AST.

No implementation lands in v1 — this is an illustrated contract.

## The CFG that would attach to the AST

```rust
pub struct Cfg {
    pub blocks: Vec<BasicBlock>,
    /// Entry block for each scope-owning AST node (procedure, function,
    /// method, trigger, file root).
    pub entry: FxHashMap<NodeId, BlockId>,
    /// Stable identity for CFG blocks.
    pub block_of_stmt: NodeIndexVec<BlockId>,
}

pub struct BasicBlock {
    pub id: BlockId,
    pub stmts: Vec<NodeId>,      // indexes into AST by stable NodeId
    pub successors: SmallVec<[BlockId; 2]>,
}

pub struct CfgBuilder<'a> {
    scope_tree: &'a ScopeTree,
    symbols:   &'a SymbolTable,
}

impl<'a> CfgBuilder<'a> {
    pub fn build(program: &[Statement], sem: &Semantic) -> Cfg {
        // ... straightforward recursive walk matching on StatementKind.
        todo!()
    }
}
```

`Cfg` is a **separate value** alongside `Semantic`. It consumes NodeIds; it
does not modify the AST or the side tables. The AST's existing shape is
already the right thing:

| StatementKind                       | CFG-build rule                                        |
|-------------------------------------|-------------------------------------------------------|
| `Block(body)`                       | Sequence; last-stmt successor = enclosing successor   |
| `If { then, else }`                 | Split: current → {then_entry, else_entry or join}     |
| `Do { body, .. }`                   | Loop back-edge body_exit → body_entry                 |
| `Repeat { body, .. }`               | Same as Do                                            |
| `ForEach { body, .. }`              | Same as Do with an implicit break on iterator end     |
| `Case { when_branches, otherwise }` | Join over N+1 branches                                |
| `Leave / Next`                      | Jump to innermost loop's exit / latch                 |
| `Return`                            | Jump to enclosing function's exit                     |
| `Throw`                             | Jump to innermost CATCH's entry or function exit      |
| `Catch / Finally`                   | Extra predecessor from every preceding THROW site     |
| `ExpressionStatement`, declarations | Linear: current → single successor                    |

All of these are already representable today — the parser puts bodies in a
`Vec<Statement>` in source order, and every branch statement carries its own
NodeId. No AST changes needed.

## Definite-assignment pass, sketched

```rust
pub struct DefiniteAssignment {
    /// For each `SymbolId`, the set of blocks in which the symbol is
    /// provably assigned on *every* path that reaches the block's entry.
    pub must_assigned_at_entry: NodeIndexVec<BlockBitset<SymbolId>>,
}

pub fn definite_assignment(cfg: &Cfg, sem: &Semantic) -> DefiniteAssignment {
    // Forward dataflow. Meet = intersection. Gen = writes in-block.
    //   in[b]  = ∩ out[p] for p ∈ predecessors(b)
    //   out[b] = in[b] ∪ writes_in(b)
    // Converge via worklist.
    todo!()
}
```

Dataflow is parameterized purely over `Cfg` + `Semantic`. Nothing in the
passage reaches back into the AST; it reads:
- Writes in each block: existing `Symbol::write_count` has the count, but we
  need the NodeIds. Those live in `Semantic.references` — every *target* of
  an assignment is an `Identifier` expression whose NodeId is in the
  reference side table resolved to a `SymbolId`.
- Reads in each block: symmetric — the same reference side table.

So definite-assignment-for-READ-BEFORE-WRITE becomes:
```
for each resolved read ref r at NodeId n in block b:
    let sym = sem.references[n] === Resolution::Resolved(s) ⇒ s;
    if sym ∉ must_assigned_at_entry[b]:
        emit LINT0005 read-before-definite-assignment at n
```

## What's *not* reachable without changes

Genuinely hard future work that the v1 shape does accommodate but doesn't
pre-solve:

- **Exception-sensitive flow**. ABL `CATCH` / `FINALLY` / `UNDO, THROW` need
  a subtle extra-edge model (every potentially-throwing stmt has an edge to
  the innermost CATCH). The AST preserves enough shape — `Throw(expr)` is a
  distinct node, `Catch { body }` is a distinct scope — but the CFG builder
  has to walk scope ancestors at each throw site. This is O(n·d) on depth
  `d` and is fine in practice (depth ≤ 5 in corpus samples).
- **Alias analysis via `BUFFER-COPY` / dynamic handles**. These are
  recorded as External in v1. Cross-file alias is a post-v1 problem and
  requires both the cross-file sketch (R10) and a live-variable dataflow
  on top.

## Breaking-change budget

Zero. `Cfg` is a new value; `DefiniteAssignment` is a new value;
`LINT0005 read-before-definite-assignment` is a new code in `oxabl_lint`.
No v1 public type changes.

---

**Reviewers**: stress-test by imagining a dataflow question that the v1
side-table model can't answer. If you find one that isn't "cross-file" or
"alias", the shape needs an adjustment.
