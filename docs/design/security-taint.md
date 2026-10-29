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
| `INPUT` and `INPUT-OUTPUT` parameters of an externally callable unit, `TABLE` and `DATASET` parameters too | on |
| a `PUBLIC` or `PROTECTED` class variable, and such a property with an automatic `SET` that is not `PRIVATE` | on |
| `WEB-CONTEXT:<anything>`, `get-value`, `get-field`, `get-cgi`, `get-user-field`, `get-cookie` | on |
| a member or method of an object whose name says request (`poRequest`, `oWebRequest`) | on |
| `IMPORT` targets, `SET`/`UPDATE`/`MESSAGE ... UPDATE` targets | on |
| `COPY-LOB FROM FILE ... TO target`, `READ-JSON` and `READ-XML` with a source type of `"FILE"` or one that is not a literal, socket `READ(memptr, ...)` | on |
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

Request data often arrives as tables. An `INPUT DATASET` parameter taints
every table of the dataset. `READ-JSON`, `READ-XML`, `COPY-TEMP-TABLE`,
`COPY-DATASET` and the `BUFFER-COPY` method add their arguments to their
receiver: the tables of `TEMP-TABLE tt:`, `DATASET ds:` or `BUFFER b:`, or the
handle variable the chain starts at and every table that the file assigns to
it as `TEMP-TABLE tt:HANDLE`, `DATASET ds:HANDLE` or `BUFFER b:HANDLE` (or
`DEFAULT-BUFFER-HANDLE`). The `BUFFER-COPY` statement adds what
every field of its source holds to its target, and its `ASSIGN` pairs are
assignments.

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
  unit ran out of its work budget or holds an include that did not arrive
  (which can write any slot of the unit), or the file parse recovered. The
  finding is reported as in stage 1. Stage 2 never hides a finding it cannot
  judge.

Unknown is also a label, with an origin that is never a source, so it flows
like taint and a sanitiser for the sink's kind removes it. A sink that it
reaches, and no real source does, is reported as in stage 1. It comes from:

- a unit past its work budget: every file-level slot it writes, every
  parameter it passes to a routine of the file, and its result as a callee;
- the round loop ending before the summaries stop changing: every read of a
  file-level slot, a routine result or a parameter, in one more solve;
- code outside the file that may run before a unit: every read of a
  file-level slot (a field included) or a parameter, in every unit of a file
  where an include did not arrive, and in the routines of an include checked
  on its own, which its includer can run after it sets their variables;
- a variable or a temp-table that the file binds as `SHARED` or `NEW GLOBAL
  SHARED`: code outside the file set it before the file runs. Every read of it
  holds Unknown, and its definition sets no value. After each call that may
  run code the file does not hold, every name that such code may write holds
  Unknown: a shared variable and temp-table (`NEW SHARED` too), and a class
  member that code outside the class may set (a `PUBLIC` or `PROTECTED`
  variable, a property with a `SET` that is not `PRIVATE`). The same call may
  run back any routine or trigger of the file (a callback through
  `THIS-PROCEDURE` or `SOURCE-PROCEDURE`, a trigger during `WAIT-FOR`, a
  method of an object that was passed on), so the file-level variables that
  a routine of the file writes hold Unknown after it too, by the same rule.
  Only a file with such names, or with a routine or trigger, looks for these
  calls. Such a call is:
  - a `RUN` of anything but an internal procedure of the file in this
    procedure: `RUN x.p`, `RUN VALUE(...)`, `RUN SUPER`, `RUN p IN h` (also
    when the file has a procedure `p`), a `PERSISTENT` or `ASYNCHRONOUS` run;
  - a call of a function the file does not define with a body (`FUNCTION f
    ... IN h` or `IN SUPER`, or one it does not declare), and
    `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `DYNAMIC-PROPERTY`
    and `SUPER`;
  - `NEW` of a class, a method or a property (read or set) of an object of a
    class other than a JSON class, a static member of a class
    (`pkg.Cls:m()`, `Cls:p`), and `INVOKE` of a handle. A built-in attribute
    or method of a handle, a system handle, a buffer or a table is not;
  - `PUBLISH`, which runs the subscribers; `APPLY`, `WAIT-FOR`,
    `PROCESS EVENTS` and the statements that wait for input (`UPDATE`, `SET`,
    `PROMPT-FOR`, `CHOOSE`, `INSERT`), which run triggers; `DELETE OBJECT` of
    an object of a class, which runs its destructor;
  - a call of a routine of the file that makes one of these calls, itself or
    through the routines it calls.

  A sink in the same statement, after the call, sees the Unknown;
- a variable that no unit of the file declares, when its name may come from
  outside the file: an include checked on its own, a file where an include did
  not arrive, or a class that inherits. A system handle, a built-in function
  that can be called without parentheses (`TODAY`), a reserved keyword and a
  name the file defines (a main block or class variable, a table, a field, a
  buffer, a stream, ...) are not variables; an unreserved keyword (`SERVER`)
  and a local of another routine are. In a whole program that has all its
  includes, such a name compiles only as a database field, which is no source
  by default;
- in such a file, a field `q.f` whose qualifier the file does not define as a
  table, buffer or other name and the schema (with the metaschema) does not
  know as a table: a table of the includer. A qualifier that names a
  database table is that table only when every field read through it is a
  field of the table; one that abbreviates a table name is Unknown, since the
  includer may define it as a buffer of a table with the same fields. Without a schema, a database
  field read in an include file checked on its own is Unknown too.

What the first two leave, and a unit past its budget itself, has an origin of
its own, `BUDGET`: the analysis did not finish the value. A sink rule reads it
as Unknown. ABLSEC010, which reports no Unknown, reports a decision that it
reaches as not judged, once per routine ("authorization could not be judged:
analysis budget exceeded"), so the budget hides no decision.

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
it with. The summary itself is solved without the seeds, so one caller's input
does not leak into another caller's result. After the call, the caller's
file-level variables that a routine of the file writes take what the routines
write to them, so a function that sets a main block variable is seen.
Summaries and seeds join the file-level summaries in the round loop, which
stops when nothing changes and after at most eight rounds. A chain of calls or
of file-level variables deeper than that leaves the summaries unconverged, and
its sinks Unknown (see "The gate").

A call is read in modelled statements and expressions, in block headers, and
in the tokens of the statements that are not modelled. Every operand of every
expression is read for its calls, whatever the expression makes of its value:
one walk over the operands of each expression kind covers the operands of a
comparison, a logical operator, `CAN-FIND` and a subscript, and the condition
of an `IF` expression. The same holds for the `WHEN` values of a `CASE`, the
subscripts of an assignment target, and the `TO`, `BY`, `WHILE` and `ON`
actions of a block header. An expression kind that the analysis does not
model is Unknown. A method is called on `THIS-OBJECT` or with no receiver; a
call on another object is unknown.

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
- A sink sees what a call in its own statement writes (a routine of the
  file, or Unknown from code elsewhere) when the call starts inside the sink's
  operand or ends before it; a call around the operand (`f(VALUE(c))`) writes
  after it.
- These run code elsewhere and do not make the names that code may write
  Unknown, nor count as a call of the routines and triggers of the file:
  database triggers (a `CREATE`, `DELETE`, `FIND` or field assignment of a
  database record), a destructor that garbage collection runs, a trigger that
  a statement not listed in "The gate" fires (`ENABLE`, `VIEW`, a
  widget attribute set), and a member whose name is a keyword of
  a name the file does not declare.
- Statements the parser skips change no slot, except `COPY-LOB`.
- A shared buffer or dataset holds what the file writes to it; only shared
  variables and temp-tables are Unknown.
- `RAW-TRANSFER`, a dynamic buffer field assignment
  (`h:BUFFER-FIELD("f"):BUFFER-VALUE = x`), a table bound to a handle
  parameter (`BIND`) and a handle that gets its table other than by
  `h = TEMP-TABLE tt:HANDLE` (a parameter, `GET-BUFFER-HANDLE`, an `ASSIGN`
  through a function) do not carry taint into the table. A table field read
  without its table name holds no taint, and in a file that is open (see "The
  gate") it is a defined name, not Unknown.
- An include that did not arrive makes its whole unit Unknown, and every
  file-level read and parameter of the file: the analysis does not know what
  it writes. A missing include path so gives stage 1 findings back.
- Which built-in functions can be called without parentheses is a fixed
  list; a bare name not on it in an open file is Unknown, never clean.
- A validator is trusted by name.

## Result

On the taint corpus, precision goes from 0.62 (stage 1) to 0.97 (stage 2) and
recall stays at 1.00 (it was 0.96 before calls of the file were followed,
0.89 on the cases of an include file, request tables and header encoders
before they were modelled, 0.93 on the cases of an includer's table, an
unreserved keyword as a variable and `READ-JSON` through a handle or from a
file, 0.94 on the cases of shared variables and of a function that writes
a main block variable, and 0.96 on the cases of code that runs elsewhere and
of a call in the statement of its sink, and 0.93 on the cases of a call under
a comparison, a `NOT`, a loop or a `WHERE` clause); the corpus README has the table per rule. Over the
whole security corpus, stage 2 reports 22% fewer findings.

