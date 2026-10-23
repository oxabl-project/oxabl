# Security stage 3: entry-point reachability — Plan

## Goal

Stage 1 of the `security` group reports every non-literal sink. Stage 2
(`docs/design/security-taint.md`) keeps a sink only when untrusted input
reaches it inside one unit. Stage 3 asks how a finding is reached from the
outside:

1. Read a list of remotely reachable **entry points** (REST routes, WebSpeed
   programs, service methods, ...) from a JSON file named in `oxabl.toml`.
2. Build the static call graph from those entries over the workspace (`RUN`,
   `RUN ... IN h`, function and method calls, `NEW`, includes), with dynamic
   calls as edges to Unknown.
3. Label every security finding with how it is reached
   ("reachable from REST GET /orders/{id}"), in the text output, in the JSON
   output and in SARIF (`properties` and `relatedLocations`).
4. Add the three rules that need the graph or the identity of a value:
   - **ABLSEC010 `client-identity`**: a user, company or role taken from
     request input and used for a decision.
   - **ABLSEC011 `missing-authorization`**: an entry point reaches a
     sensitive operation without passing a configured authorisation routine.
   - **ABLSEC012 `cross-request-state`**: session-lifetime state (`NEW GLOBAL
     SHARED`, class statics) written in one request and read in another
     without a reset.
5. Close the main false-negative class of stage 2: taint across procedure,
   function and method calls of a file, and through `JsonObject` /
   `JsonArray` (`Add`/`Set` → `Write`/`GetJsonText`).

Nothing about a real code base enters the repository: the corpus is a
synthetic web application.

## Decisions

### Entry-point file

A JSON file, version 1, documented in `docs/overview.md` with synthetic
examples:

```json
{
  "version": 1,
  "entries": [
    { "kind": "REST", "name": "GET /orders/{id}",
      "program": "web/orders.p", "procedure": "get-order" },
    { "kind": "WebSpeed", "name": "/report.w", "program": "web/report.w" },
    { "kind": "REST", "name": "POST /orders",
      "class": "app.OrderService", "method": "Create", "auth": true }
  ]
}
```

- `kind` and `name` are free text and only label findings.
- The target is `program` (resolved over the run search path like a `RUN`),
  optionally with `procedure` (an internal procedure of that program), or
  `class` with an optional `method` (no method: every public method and the
  constructors).
- `auth: true` says the framework authorises the caller before the entry
  runs. The default is `false`.
- Unknown keys are errors; an entry whose target is not found is a config
  warning, not a failure.

Configuration:

```toml
[workspace.lint.security.reach]
entry-points = "security/entry-points.json"   # relative to oxabl.toml
auth-routines = ["check-access", "require-role", "auth/guard.i"]
sensitive-calls = ["post-ledger"]
raise-reachable = false
lower-unreachable = false
```

### The graph

- Nodes are units (main block, procedure, function, method, constructor,
  property accessors) of every file reached from an entry. Files are expanded
  and parsed once in the pipeline (`oxabl_pipeline::reach`), like the effect
  summaries TX004 reads, and the result is shared by the files of one run.
- Edges come from `oxabl_index::calls::FileCalls` (RUN, RUN IN h with a known
  persistent handle, functions, methods on `THIS-OBJECT`) plus a new
  extraction of object calls: `NEW pkg.Cls(...)`, `o:Method(...)` where `o`
  is declared `AS pkg.Cls`, and `pkg.Cls:Method(...)` (static). Class names
  are resolved through the file's `USING` imports and the run index.
- A built-in class (`Progress.*`, `OpenEdge.*`) is not a call into project
  code: no edge. Every other call that does not resolve is an edge to Unknown
  with the reason the call graph already names (`dynamic`, `handle`, `super`,
  `event`, `method`, `not-found`, ...).
- Includes need no edge: the rules run on expanded text, so an included body
  is part of the unit it is spliced into. An `auth-routines` entry that names
  an include file counts at the include site.

### Reachability and labels

A unit is **reachable** when an entry reaches it, **unknown** when no entry
reaches it but a reached unit has an Unknown edge, and **unreachable**
otherwise. A file outside the graph is unknown or unreachable by the same
test. Each finding of a `security` rule gets, through one hook in the
registry run:

- `Diagnostic::reach`: the status, up to four entries (kind, name, target),
  whether there are more, the Unknown reasons met, and one call path from the
  nearest entry.
- Text output: `(reachable from REST GET /orders/{id})` appended to the
  message, `(reachability unknown: dynamic call)` or `(unreachable from the
  entry points)`.
- SARIF: `properties.reachability` and one `relatedLocations` entry per call
  site on the path that sits in the checked file.

Severity changes only when configured: `raise-reachable` moves a reachable
finding one level up (info → warning → error), `lower-unreachable` moves an
unreachable one down. An unknown finding never moves.

### Gates: authorisation and resets

A **gate** is a site that establishes a fact for the rest of the request: a
call of an auth routine (ABLSEC011), or a write of a piece of session state
(ABLSEC012, a reset). The search runs over states `(unit, gated)` from every
entry (gated from the start when `auth: true`). A call edge keeps the gate
when the caller passes a gate before the call: a gate site whose CFG node
dominates the call's node. A sink is ungated when an ungated state reaches its
unit and no gate of its unit dominates it. Parents are kept, so a finding
names one ungated path.

### ABLSEC010 `client-identity`

Taint-based in every mode (it asks where a value comes from). A new label bit,
**identity**, is set on a request value whose key names an identity:
`get-value("user")`, `get-field("company")`, `poRequest:GetPathParameter("role")`,
`oBody:GetCharacter("userId")` on a tainted object. The key is split into
words and matched against `identity-keys` (default `user`, `username`,
`userid`, `login`, `company`, `tenant`, `role`, `roles`, `owner`, `admin`).
`identity-sources` names project functions whose result is a client identity.
No sanitiser clears the bit (a number or a comparison is still the client's
claim); a configured validator that `protects = ["identity"]` does.

A **decision** is an `IF`, `CASE`, `WHILE` or `REPEAT WHILE` condition, a
`WHERE` of `FOR EACH`/`FIND`/`CAN-FIND`, and an argument of an auth routine.

### ABLSEC011 `missing-authorization`

Needs the entry points (silent without them). A **sensitive operation** is a
database write the effect analysis finds (`CREATE`, `DELETE`, an assignment
to a database field; temp-tables excluded) or a call of a `sensitive-calls`
routine. One finding per unit: at its first ungated sensitive operation,
naming the entry and the path. A unit whose reach is unknown is not
reported.

### ABLSEC012 `cross-request-state`

Needs the entry points. Session state is a `NEW GLOBAL SHARED` variable (and
its `SHARED` consumers), and a `STATIC` variable or property of a class
(inside the class by name, outside as `Cls:name`). A state is cross-request
when reached code writes it and reached code reads it. A read is reported
when an ungated state reaches its unit, where a write of the same state is
the gate. One finding per state and unit.

### Taint across calls (stage 2 extension)

- Within a file, every procedure, function, method and constructor gets a
  **summary**: what its return value and each `OUTPUT`/`INPUT-OUTPUT`
  parameter hold at its exits, in terms of real origins and of placeholder
  origins standing for its input parameters.
- At a call (`RUN p (...)`, `f(...)`, `m(...)`, `THIS-OBJECT:m(...)`) the
  caller substitutes its argument values for the placeholders. Callers also
  add their argument values to the callee's parameter seeds, so a private
  procedure called with input is solved with it.
- Summaries and seeds join the existing round loop (at most `ROUNDS`).
- `JsonObject`/`JsonArray`: `o:Add(k, v)`, `o:Set(k, v)` and `o:AddNull`-like
  calls add their arguments to `o`; `o:Write(target, ...)` and
  `o:WriteFile`... put `o` into `target`; `o:GetJsonText()` and other reads
  already return `o`. Applied to a receiver declared as a JSON class, and to a
  receiver whose type is not declared.

## Tests first

Each slice starts with failing tests:

1. Taint: the two `limits.p` misses (function return, private procedure)
   become `cases.p` hits; new cases for OUTPUT parameters, methods, a
   sanitising callee, and the JSON shapes. The taint corpus counts are
   re-pinned.
2. Entry-point file: parse, errors, config table, warnings.
3. Graph: unit tests of the search (gates, dominance, unknown, parents).
4. Rules 010/011/012: rule tests per shape, vulnerable fires, safe does not.
5. Pipeline: the synthetic web application under
   `crates/oxabl_pipeline/tests/reach_corpus/` (front controller that `RUN
   VALUE`s handlers, gated and ungated handlers, a service class, a global
   shared cache, JSON responses), with `/* reach: real */` and
   `/* reach: safe */` marks and a precision table.
6. SARIF and text output of the labels.

A generated large project (an example, `crates/oxabl/examples/reach_scale.rs`)
measures the time of the graph build and the lint run.

## Known limits (to be documented)

- Gating is dominance in each unit plus "every edge of the path": an auth
  routine that only checks and does not stop the request still counts.
- A method call on an object whose type the file does not declare is
  Unknown; interface and inheritance dispatch is resolved to the declared
  class only.
- Taint does not cross files in stage 3: a program run with tainted input
  sees it through its external input parameters, which are sources already.
- Session state held inside a singleton instance (not in a static) is not
  seen by ABLSEC012.
