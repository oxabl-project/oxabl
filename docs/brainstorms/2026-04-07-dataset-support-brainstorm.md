# Brainstorm: Full Dataset Support

**Date:** 2026-04-07
**Status:** Draft

## What We're Building

Full parsing support for Progress ABL ProDataSet objects, covering:

1. **DEFINE DATASET** — static compile-time dataset definitions with all documented clauses
2. **CREATE DATASET** — dynamic runtime dataset creation via handles
3. **Dataset methods** — handled naturally by existing postfix method call parsing (no new AST nodes)

## Why This Approach

Datasets are a fundamental ABL construct that groups temp-tables with defined relationships. They're heavily used in data exchange (JSON/XML serialization), web services, and application server communication. Implementing full support in one pass avoids leaving partial gaps that would confuse users parsing real-world ABL code.

The method calls (SET-BUFFERS, ADD-RELATION, COPY-DATASET, EMPTY-DATASET, etc.) are already handled by the existing `object:method()` postfix parsing, so "full support" is really just DEFINE DATASET + CREATE DATASET + ensuring all sub-clause keywords exist in the lexer.

## Key Decisions

1. **Dataset methods as regular method calls** — No dedicated AST nodes for SET-BUFFERS, ADD-RELATION, etc. These are postfix method calls on handle expressions, already supported by the parser.

2. **All documented clauses for DEFINE DATASET** — Including NAMESPACE-URI/PREFIX, XML-NODE-NAME/TYPE, SERIALIZE-NAME/HIDDEN, REFERENCE-ONLY, DATA-RELATION (with RELATION-FIELDS, REPOSITION, NESTED, FOREIGN-KEY-HIDDEN, NOT-ACTIVE, RECURSIVE), and PARENT-ID-RELATION (with PARENT-ID-FIELD, PARENT-FIELDS-BEFORE/AFTER).

3. **CREATE DATASET with IN WIDGET-POOL** — Requires restructuring `parse_create_statement()` to dispatch on `Kind::Dataset` rather than treating everything after CREATE as a plain identifier.

4. **Retrofit XML/serialize clauses on TEMP-TABLE and BUFFER** — NAMESPACE-URI, NAMESPACE-PREFIX, XML-NODE-NAME, XML-NODE-TYPE, SERIALIZE-NAME, and SERIALIZE-HIDDEN are shared across DEFINE DATASET, DEFINE TEMP-TABLE, and DEFINE BUFFER. The temp-table/buffer parsers currently skip these as unknown tokens (`statements.rs:771`). This work adds proper parsing for all three, removing the skip-unknown hack.

5. **Extend benchmarks** — Add dataset definitions to parser bench fixture for CodSpeed regression detection.

## Scope Details

### DEFINE DATASET Syntax

```
DEFINE {[[ NEW ] SHARED ]|[ PRIVATE | PROTECTED ][ STATIC ]
   [ SERIALIZABLE | NON-SERIALIZABLE ]} DATASET dataset-name
  [ NAMESPACE-URI namespace ][ NAMESPACE-PREFIX prefix ]
  [ XML-NODE-NAME node-name ][ SERIALIZE-NAME serialize-name ]
  [ XML-NODE-TYPE node-type ][ SERIALIZE-HIDDEN ]
  [ REFERENCE-ONLY ] FOR buffer-name [ , buffer-name ]...
  [ DATA-RELATION [name] FOR parent, child
      RELATION-FIELDS (pf1, cf1 [, pfN, cfN]...)
      [ REPOSITION ][ NESTED [ FOREIGN-KEY-HIDDEN ]][ NOT-ACTIVE ][ RECURSIVE ] ]...
  [ PARENT-ID-RELATION [name] FOR parent, child
      PARENT-ID-FIELD id-field
      [ PARENT-FIELDS-BEFORE (f1 [, fN]...) ]
      [ PARENT-FIELDS-AFTER (f1 [, fN]...) ] ]...
.
```

**Modifiers:**
- NEW SHARED / SHARED — reuse existing shared-variable patterns
- PRIVATE / PROTECTED / STATIC — currently only handled for PROPERTY in `parse_define_statement()` dispatch; need to extend dispatch to recognize access modifiers before DATASET (same pattern as the existing PROPERTY path)
- SERIALIZABLE / NON-SERIALIZABLE — need new tokens, dataset-specific

**Clauses needing new lexer tokens:**
- NAMESPACE-URI, NAMESPACE-PREFIX (shared with TEMP-TABLE, BUFFER)
- XML-NODE-NAME, XML-NODE-TYPE (shared with TEMP-TABLE, BUFFER)
- SERIALIZE-NAME, SERIALIZE-HIDDEN (shared with TEMP-TABLE, BUFFER)
- SERIALIZABLE, NON-SERIALIZABLE (dataset-specific)
- REFERENCE-ONLY (dataset-specific)
- RELATION-FIELDS (dataset-specific)
- NESTED, FOREIGN-KEY-HIDDEN, NOT-ACTIVE, RECURSIVE (dataset-specific)
- PARENT-ID-RELATION, PARENT-ID-FIELD (dataset-specific)
- PARENT-FIELDS-BEFORE, PARENT-FIELDS-AFTER (dataset-specific)

**Tokens that already exist:**
- Dataset, DataRelation, DatasetHandle, Reposition

### CREATE DATASET Syntax

```
CREATE DATASET handle [ IN WIDGET-POOL pool-name ].
```

The parser has `parse_create_statement()` but it currently treats CREATE as `CREATE identifier [NO-ERROR].` — it parses the next token as a plain identifier. For `CREATE DATASET handle`, the parser needs to dispatch on `Kind::Dataset` after CREATE, then parse the handle expression and optional `IN WIDGET-POOL` clause. This requires restructuring `parse_create_statement()` to check the token kind before falling back to the generic identifier path.

### AST Nodes Needed

**New `Statement` variant:**
- `DefineDataset` — name, modifiers (shared/access/static/serializable), namespace options, XML/serialize options, reference_only flag, buffer list, data relations, parent-id relations

**New supporting types:**
- `DataRelation` — optional name, parent buffer, child buffer, relation fields (Vec of field pairs), flags (reposition, nested, foreign_key_hidden, not_active, recursive)
- `ParentIdRelation` — optional name, parent buffer, child buffer, id field, parent_fields_before (Vec), parent_fields_after (Vec)

**Existing `Statement` variant to extend:**
- `Create` — add Dataset variant alongside existing Table/Widget/etc.

### Lexer Keywords to Add

Add to `keyword_overrides.toml` and regenerate:
- `namespace-uri`, `namespace-prefix`
- `xml-node-name`, `xml-node-type`
- `serialize-name`, `serialize-hidden`
- `serializable`, `non-serializable`
- `reference-only`
- `relation-fields`
- `nested`, `foreign-key-hidden`, `not-active`, `recursive`
- `parent-id-relation`, `parent-id-field`
- `parent-fields-before`, `parent-fields-after`
- `widget-pool` (for CREATE DATASET ... IN WIDGET-POOL)

## Implementation Order

1. **Lexer** — Add missing keywords to `keyword_overrides.toml`, regenerate
2. **AST** — Add `DefineDataset` variant, supporting types (DataRelation, ParentIdRelation), and shared XML/serialize fields to existing TEMP-TABLE/BUFFER AST nodes
3. **Parser — shared clauses** — Extract XML/serialize clause parsing into a reusable helper, retrofit DEFINE TEMP-TABLE and DEFINE BUFFER to use it (replacing the skip-unknown hack at `statements.rs:771`)
4. **Parser — DEFINE DATASET** — Implement `parse_define_dataset()` following temp-table loop pattern, add dispatch in `parse_define_statement()` (including access modifier path)
5. **Parser — CREATE DATASET** — Restructure `parse_create_statement()` to dispatch on `Kind::Dataset`, parse handle + optional IN WIDGET-POOL
6. **Tests** — Comprehensive test coverage for all clause combinations (dataset, and updated temp-table/buffer XML/serialize tests)
7. **Benchmarks** — Extend parser fixture with dataset definitions

## Open Questions

None — scope and approach are fully defined.
