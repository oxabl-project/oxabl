# Stage 2 of the security rules: intra-procedural taint

The code is in `crates/oxabl_lint/src/rules/security/taint/`, the user guide in
`docs/overview.md` ("Taint mode"), and the corpus in
`crates/oxabl_lint/tests/security_corpus/taint/`.

## Context

Stage 1 of the `security` group (ABLSEC001-009, ABLSEC013-017) reports every
sink whose argument is not a literal. It is complete and noisy: a value that
was cleaned (`QUOTER(x)`), a constant held in a variable, a `CASE` result and a
value that no caller can steer are all findings.

Stage 2 keeps the same sinks and asks one more question: **can a value from an
untrusted source reach this argument, on some path, without a sanitiser that
protects this kind of sink?** It looks inside one procedure, function or method
at a time. Stage 3 (later) adds the call graph and entry-point reachability.

## Goals

- A taint mode for each sink rule where taint is the right question. Stage 1
  stays the default until precision is measured on real code.
- Sanitisers that are specific to a sink kind: `QUOTER()` protects a query and
  never a command.
- A short source → sink path in each message, as diagnostic labels, and as SARIF
  `codeFlows`.
- A corpus that measures precision and recall of both modes per rule.

## Non-goals

- Taint across files or the network.
- Proving that a sanitiser is correct. A configured validator is trusted.
- Taint inside objects: a field of an object is not a variable.

## Which rules have a taint mode

| Rule | Sink kind | Taint mode |
|---|---|---|
| ABLSEC001 `dynamic-query` | `query` | yes |
| ABLSEC002 `foreign-sql` | `sql` | yes |
| ABLSEC003 `os-command` | `command` | yes |
| ABLSEC004 `dynamic-code` | `code` | yes |
| ABLSEC005 `path-traversal` | `path` | yes |
| ABLSEC006 `xss-output` | `html` | yes |
| ABLSEC007 `header-injection` | `header` | yes |
| ABLSEC014 `unsafe-deserialization` | `deserialize` | yes |
| ABLSEC015 `untrusted-xml` | `xml` | yes (the `X-DOCUMENT:LOAD` source argument; `SAX-PARSE` stays a review) |
| ABLSEC016 `resource-exhaustion` | `size` | yes (the `SET-SIZE` length; socket reads and `FILL` stay reviews) |
| ABLSEC008 `hardcoded-secret` | — | no: it reports a literal |
| ABLSEC009 `weak-crypto` | — | no: it reads names, not origins |
| ABLSEC013 `information-leak` | — | no: the source is error text, not input |
| ABLSEC017 `integrity-bypass` | — | no: it reports a statement |

A rule without a taint mode behaves the same in both modes.

## Design

### Where the code lives

- `oxabl_flow` gets two generic pieces:
  - `dataflow::Provenance`, a fact type: for each slot (a variable), the
    origins that may reach it, each with a mask of properties it still has.
    Join is union. Taint is the first user; the type knows nothing of taint.
  - `Cfg::nodes_at(span)`: the nodes of the innermost statement or block
    header whose span holds `span`. A rule knows a span; the analysis needs a
    program point.
- `oxabl_lint::rules::security::taint` holds the analysis: sources,
  sanitisers, the per-unit solve, the witness path, and the gate the rules ask.
- `RunShared::taint` computes the file's taint once per registry run, and only
  when a rule in taint mode asks for it.

### The value model

A value is a set of **labels**. A label is `(origin, kinds)`: the source the
value came from, and the sink kinds that the value is still dangerous for. A
sanitiser removes kinds; a label with no kinds is gone.

Sink kinds: `query`, `sql`, `command`, `code`, `path`, `html`, `header`,
`deserialize`, `xml`, `size`.

Each unit is compiled once into a small IR: for each CFG node, the slots it
sets and the value expression of each. A value expression is a union of slot
reads, origins, and "clear these kinds of that sub-expression". The solver
evaluates the IR. Two front ends build it: one from the AST (modelled
statements), one from tokens (statements the parser skips, and the operand a
rule hands over by span).

### Slots

- A local variable or parameter: strong update (an assignment replaces).
- An array element, a temp-table or database field `t.f`, and a write through
  a function-call target (`SUBSTRING(c, 1, 2) = x`): weak update (an
  assignment adds). A field slot is keyed by the table name, so a buffer
  `DEFINE BUFFER b FOR TEMP-TABLE tt` writes `tt.f`. A whole-table slot `tt.*`
  holds taint that reached every field (an `INPUT TABLE` parameter, `IMPORT`
  into the record).
- A property set through `obj:prop = v` adds the value to the slot of `obj`.
- A variable that the unit reads but does not define is **file-level** (a main
  block variable read in a procedure, a class member read in a method). Its
  value at the unit entry is a flow-insensitive summary: the union of what every
  unit of the file writes to it (a main block counts for its own variables).
  The summaries and the unit solves iterate to a fixed point (at most eight
  rounds; the summaries only grow).
- At a `RUN` of an internal procedure, a main block adds to each of its
  variables what other units write to it, so `RUN read-input. OS-COMMAND
  VALUE(cCmd).` sees the procedure's write.

### Sources

| Source | Default |
|---|---|
| `INPUT` and `INPUT-OUTPUT` parameters of an externally callable unit | on |
| `WEB-CONTEXT:<anything>`, `get-value`, `get-field`, `get-cgi`, `get-user-field`, `get-cookie` | on |
| a member or method of an object whose name says request (`poRequest`, `oWebRequest`) | on |
| `IMPORT` targets, `SET`/`UPDATE`/`MESSAGE ... UPDATE` targets | on |
| `COPY-LOB FROM FILE ... TO target`, socket `READ(memptr, ...)` | on |
| `OS-GETENV(...)`, `SESSION:PARAMETER`, `SESSION:STARTUP-PARAMETERS` | on |
| database fields | off (`database-fields = true`) |
| project functions and methods (`sources = [...]`) | none |

Externally callable: the main block of a procedure file, a `PROCEDURE` or
`FUNCTION` that is not `PRIVATE`, a `METHOD` or `CONSTRUCTOR` that is not
`PRIVATE`. `parameters = "all"` taints every input parameter,
`parameters = "none"` none.

An unknown call (a user function, a method, `RUN`, `NEW`) returns the union of
its receiver and its arguments: it is tainted when any argument is. Its
`OUTPUT` arguments get the same value.

### Sanitisers

| Sanitiser | Clears |
|---|---|
| `QUOTER(x)` | `query` |
| `INTEGER`, `INT64`, `DECIMAL`, `DATE`, `DATETIME`, `DATETIME-TZ`, `LOGICAL` conversion, arithmetic, and functions that return a number (`LENGTH`, `NUM-ENTRIES`, `LOOKUP`, `INDEX`, ...) | every kind but `size` |
| a function that returns a logical, a comparison | every kind |
| `html-encode(x)` | `html` |
| `IF x = "lit"`, `IF LOOKUP(x, "a,b") > 0`, `IF CAN-DO("a,b", x)`, `CASE x: WHEN "a"` — on the guarded edge | every kind, for `x` |
| a configured validator `v(x)`: as a wrapper, its result; as a guard `IF v(x)`, `x` on the true edge | the kinds it is configured for |

No built-in function protects a header: `html-encode` keeps CR and LF, so a
value can still split the response, and neither it nor `url-encode` stops a
redirect to another site. `url-encode` protects no kind; a project that has a
header check names it as a validator with `protects = ["header"]`.

A guard narrows on the CFG edge: the `True`/`False` edges of an `IF` and the
`When(n)` edges of a `CASE` whose values are all literals. `NOT`, `AND` and `OR`
combine guards.

### Constant folding

A literal has no labels, and a slot assigned a literal is clean on that path.
The rules run on preprocessed text, so a `{&NAME}` that expands to a literal is
a literal. A sink whose argument is constant is never a stage 1 finding, so it
never reaches the taint question.

### The gate

A taint-capable rule asks the gate at each stage 1 finding, with the operand
span and the sink kind:

- stage 1 mode: report, as now;
- taint mode: find the node (`Cfg::nodes_at`), take the fact before it (plus
  the earlier pairs of an `ASSIGN`), evaluate the operand. Report when a label
  with the sink's kind is there, with the path; drop the finding when none is.
- **Unknown**: the operand holds an unexpanded reference, no node holds it, the
  unit ran out of its work budget, or the file parse recovered. The finding is
  reported as in stage 1. Stage 2 never hides a finding it cannot judge.

### The path

From the sink, the witness walks back: the nearest earlier node that wrote the
slot with this origin, then the slot that write read, until the origin. At most
eight steps. Each step is a diagnostic label in order (source first), and the
message names the source and the slots: `` from untrusted input: input
parameter `pcName` → `cWhere` ``. SARIF output turns the labels of a security
finding into one `threadFlow`, ending at the sink.

### Configuration

```toml
[workspace.lint]
enable = ["security"]

[workspace.lint.security]
mode = "taint"                 # "sink" (default) or "taint"
parameters = "external"        # "external" (default), "all", "none"
database-fields = false
sources = ["read-request-body"]

[workspace.lint.security.modes]
ABLSEC005 = "sink"             # by code or name; beats `mode`

[[workspace.lint.security.validators]]
name = "is-valid-code"
protects = ["query", "path"]   # sink kinds, or "all"
```

The table is global: it is not read inside `[[workspace.lint.overrides]]`. The
resolved value travels on `LintSeverityMap`, which every client already passes
to the rules, so the CLI, the daemon, the LSP and wasm need no new plumbing.

### Calls of the file

Each procedure, function and method of a file has a **summary**: the labels
of its result (a `$return` slot that each `RETURN expr` sets) and of each
`OUTPUT`/`INPUT-OUTPUT` parameter at its exit. An input parameter has an
origin of its own: the source "input parameter `pc`" for a unit that code
outside the file can call, and a **placeholder** for any other. A call
replaces the callee's parameter origins in the summary with its argument
values, so the summary serves every caller: `f(pc)` is tainted when `f`
returns `pc`, `f("x")` is not. A placeholder is never a source at a sink.

A caller also passes its argument values to the callee's parameters (the
**seeds**), so the sinks inside a `PRIVATE` procedure see what the file runs
it with. The summary itself is solved without the seeds, so one caller's
input does not leak into another caller's result. Summaries and seeds join
the file-level summaries in the round loop, which stops when nothing changes
and after at most eight rounds, so a chain of calls deeper than that may stop
short.

A call is read in modelled statements and expressions, in block headers, and
in the tokens of the statements that are not modelled. A method is called on
`THIS-OBJECT` or with no receiver; a call on another object is unknown.

### JSON objects

The object model is a common way for request values to reach a response
without passing a variable the analysis can see. A variable declared as a
`Json...` class (`JsonObject`, `JsonArray`), or one the file does not
declare, takes what `o:Add(...)`, `o:Set(...)` and `o:Insert(...)` give it
(a weak update), and `o:Write(target, ...)` writes it into `target`. A read
through the object (`o:GetJsonText()`, `o:GetCharacter("k")`) is already the
union of the receiver and the arguments. JSON serialisation clears no kind: a
JSON text written into an HTML response is still an XSS finding.

## Measurement

`tests/security_corpus/taint/<rule>/*.p` holds synthetic programs. Each sink line
has a marker comment: `/* taint: real */` for a sink that untrusted input
reaches, `/* taint: safe */` for one that stage 1 reports and that is safe
(sanitised, constant, sanitised for this kind, guarded). Cases per rule:
sanitised, sanitised for the wrong sink, constant through a variable, tainted
through an assignment chain, through a temp-table field, through an unknown
call. A test computes precision and recall of both modes per rule from the
markers and prints the table. Taint mode must match every marker of a
`cases.p`; a `limits.p` holds the known errors of stage 2, and the test pins
their counts.

`crates/oxabl/examples/security_corpus.rs` runs both modes over a tree
(`OXABL_SECURITY_CORPUS`) and prints the finding count per rule and mode.

## Known limits

- Calls cross the units of one file only (see "Calls of the file"). A
  program run with `RUN other.p` returns what an unknown call returns.
- A main block sees what an internal procedure writes to its variables only
  after a `RUN` of that procedure; a function or method that writes one is not
  seen.
- Statements the parser skips change no slot, except `COPY-LOB`.
- `SHARED` variables are clean.
- A validator is trusted by name.

## Result

On the taint corpus, precision goes from 0.56 (stage 1) to 0.96 (stage 2) and
recall stays at 1.00 (it was 0.96 before calls of the file were followed);
the corpus README has the table per rule. Over the whole security corpus,
stage 2 reports 30% fewer findings.

