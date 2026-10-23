# Stage 3 of the security rules: entry-point reachability

The plan is `docs/plans/2026-10-23-001-feat-security-reachability-plan.md`.
The user guide is in `docs/overview.md` ("Entry points and reachability").
The code:

| Part | Where |
|---|---|
| Entry-point file and its config | `oxabl_workspace::entry_points`, `SecurityReachConfig`; `oxabl_common::ReachOptions` |
| Object calls (`NEW`, typed receivers, static calls) | `oxabl_index::calls::object_calls` |
| The searches over the graph | `oxabl_index::reach` |
| What one file says about itself | `oxabl_lint::security::reach::file_facts` |
| Reading the workspace and building the answers | `oxabl_pipeline::reach` |
| Labels on findings | `oxabl_lint::security::reach::annotate`, `oxabl_common::Reachability` |
| ABLSEC010, ABLSEC011, ABLSEC012 | `oxabl_lint::security::{client_identity, missing_authorization, cross_request_state}` |
| Output | `oxabl check` text, `--json` (`reach`), SARIF (`properties.reachability`, `relatedLocations`) |

## Context

Stage 1 reports every non-literal sink; stage 2 keeps the ones that untrusted
input reaches inside one file. Neither says whether a remote caller can get to
the sink at all, and some bugs are about the path, not the value: an
operation that no authorisation check guards, or state that one request leaves
for the next.

## The entry-point file

A JSON file, version 1, that the project writes or generates:

```json
{
  "version": 1,
  "entries": [
    { "kind": "REST", "name": "GET /orders/{id}", "program": "web/orders.p", "procedure": "get-order" },
    { "kind": "REST", "name": "POST /orders", "class": "app.OrderService", "method": "Create", "auth": true }
  ]
}
```

The format is generic on purpose: a project produces it from its own route
table, WebSpeed configuration or service catalogue, outside oxabl. `kind` and
`name` only label findings. The target is a program (resolved as `RUN`
resolves it, with an optional internal procedure) or a class (with an optional
method; no method means its constructors and every method that is not
`PRIVATE`). `auth: true` means that something outside the code authorises the
caller first.

The file is read when the configuration is resolved. A file that does not
parse is one config warning and gives no entries; an entry whose target is not
found is a warning of the run.

## The graph

Nodes are the units (main block, procedure, function, method, constructor,
property accessors) of every file an entry reaches. The pipeline expands and
parses each file once, through the run's index, and asks
`file_facts` for:

- the calls of each unit: the call graph's sites (`FileCalls`), with a
  method call on an object replaced by the class the file declares for its
  receiver, and `NEW` added;
- the gates of each unit (below), as offsets;
- the sensitive operations: database writes from the effect analysis, and
  calls of `sensitive-calls`;
- the session state the file declares, and each read and write of it;
- the class the file defines and its parent, for inherited methods.

A call resolves to units: an internal procedure, function or method by name; a
program's main block; a procedure of a program a persistent handle runs; a
method of a class (following `INHERITS`) or its constructors. A call into a
`Progress.*`, `OpenEdge.*` or .NET class is no edge. Every other call that does
not resolve keeps the call graph's reason (`dynamic`, `handle`, `super`,
`event`, `method`, `not-found`, `ambiguous`, `no-routine`) as an edge to
Unknown.

Includes are not edges. The rules read the expanded text, so an included body
is part of the unit it is spliced into, and an `auth-routines` entry that names
an include counts at the include's first statement.

## Status and labels

`ReachGraph::reach` walks from every entry and keeps, per node, the entries
that reach it (at most four, and whether there are more), and the reasons of
the Unknown calls of the reached nodes. A node is:

- **reachable** when an entry reaches it;
- **unknown** when no entry does but some reached call is Unknown;
- **unreachable** otherwise.

A file outside the graph takes the same test. A finding is labelled in the
registry, after its rule runs, so every `security` rule is labelled the same
way: the innermost unit that holds the finding gives the status, the entries
and one path (the shortest found). The note goes on the message, the whole
`Reachability` on `Diagnostic::reach`. `raise-reachable` and
`lower-unreachable` move a finding one severity level; an unknown finding
never moves, since unknown is not proof.

## Gates

A **gate** is a site that establishes a fact for the rest of a request: a call
of an `auth-routines` routine or an include of one (ABLSEC011), and a write of
a piece of session state, which resets it (ABLSEC012).

A gate covers a site of its own unit when the gate's CFG node dominates the
site's node (`oxabl_flow::dominators`), or is the same node earlier in the
text. So `RUN check-access.`, `IF NOT allowed() THEN RETURN.` and an include
of a guard cover what follows them; a check inside one branch of an `IF` does
not cover the code after the `IF`.

Across calls, `ReachGraph::gated` walks states `(node, gated)` from every entry
(gated from the start when the entry is `auth: true`). A call edge leaves in
the gated state when its caller was gated, or a gate covers the call site. A
sink is ungated when an ungated state reaches its unit and no gate of its unit
covers it. One path from an entry that passes no gate is enough, and the
search keeps the first one it found for the message.

## The rules

- **ABLSEC010 `client-identity`** is a taint question, so it is in the taint
  analysis: a new kind, `identity`, outside the injection kinds. A request
  value read by a key with a word of `identity-keys` is tagged with it
  (`get-value("user")`, `poRequest:GetPathParameter("tenant")`, a `Get...`
  method with such a key on an object that holds request input), and so is a
  result of `identity-sources`. Nothing but a validator that protects
  `identity` clears it: a comparison or a number conversion leaves the
  client's claim in place. A decision is read in "decision mode", where a
  logical function passes its arguments on (`CAN-DO(list, x)` reads `x`). The
  rule needs no entry point.
- **ABLSEC011 `missing-authorization`**: per unit, the first sensitive
  operation that no gate of its unit covers, when an ungated state reaches the
  unit. One finding per unit.
- **ABLSEC012 `cross-request-state`**: session state is cross-request when
  reached code writes it and reached code reads it. For each such state the
  pipeline runs one gated search with the state's writes as the gates; a read
  that no write of its unit covers, in a unit an ungated state reaches, is a
  finding. One per state and unit.

ABLSEC011 and ABLSEC012 are not context-free, and are silent without entry
points.

## Taint across calls and JSON objects

Stage 3 also closes the main false-negative class of stage 2: taint now
follows the calls of a file's own routines (summaries with placeholder
origins for the parameters, and seeds from the callers), and the
`JsonObject`/`JsonArray` model (`Add`/`Set`/`Insert` into the object,
`Write(target)` out of it). See `docs/design/security-taint.md`, "Calls of the
file" and "JSON objects".

## Measurement

The corpus is a synthetic web application:
`crates/oxabl_pipeline/tests/reach_corpus` (handlers reached from REST and
WebSpeed entries, a front controller that runs pages with `RUN VALUE`, checks
by program, internal procedure and include, an entry marked `auth`, service
classes reached through `NEW` and a typed receiver, a global shared cache and a
static of a class, and a JSON response). Each line a graph rule reports in some
configuration has a mark, `/* reach: real CODE */` or `/* reach: safe CODE */`.

| Rule | TP | FP | FN | Precision | Recall |
|---|---|---|---|---|---|
| ABLSEC010 | 2 | 0 | 0 | 1.00 | 1.00 |
| ABLSEC011 | 4 | 0 | 0 | 1.00 | 1.00 |
| ABLSEC012 | 2 | 0 | 0 | 1.00 | 1.00 |
| ABLSEC011 with no check known | 4 | 4 | 0 | 0.50 | 1.00 |

The last row is the same corpus with no `auth-routines` and no `auth` entry:
what a rule that ignored the checks would report. The corpus was written with
the analysis, so it shows that the analysis does what it was designed to do;
it does not measure real code.

`crates/oxabl/examples/reach_scale.rs` generates a project and times it
(release build, one thread):

| Files | Graph build | Lint, no entry points | Lint with entry points (graph included) |
|---|---|---|---|
| 6 101 | 0.48 s | 0.61 s | 1.54 s |
| 30 501 | 1.8-2.7 s | 3.0 s | 6.1-7.1 s |

## Known limits

- A check is trusted by name and position. One that does not stop a failed
  request still counts.
- Gating is "every path through each routine, some path through the graph":
  the graph walk is not path-sensitive across calls.
- A method call on an object whose class the file does not declare is
  Unknown. A call goes to the declared class (and its parents), not to an
  override or an interface implementation.
- Taint does not cross files. The labels say how a routine is reached, not
  that the tainted value came from that entry.
- Session state inside a singleton's instance, a persistent procedure's
  variables and `SESSION` attributes are not seen. Statics are named by the
  last segment of their class name.
- The editor and the daemon do not build the graph yet: their findings carry
  no label, and ABLSEC011 and ABLSEC012 are silent there.
