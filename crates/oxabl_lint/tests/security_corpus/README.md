# Security corpus

Synthetic ABL for the `security` rules ABLSEC001 to ABLSEC005. `tests/security_rules/corpus.rs` runs it.

- `ablsecNNN/vulnerable/*.p` starts with `/* expect: N */`. The rule gives exactly N findings.
- `ablsecNNN/safe/*.p` gives no finding from any security rule.
- `include/*.i` holds the include files that the programs use. Each program is preprocessed against this directory, so the include-file and `&GLOBAL-DEFINE` cases run as a client runs them.

Every rule has at least four vulnerable and four safe programs.

## Precision note

No real code base has measured these rules yet. The classes below come from the design of each rule and from the fixtures.

False positives (a finding on code that is safe):

- A value that was cleaned before use. Stage 1 knows no sanitiser, so `QUOTER(x)`, a path check or an allow-list lookup does not make an argument a literal.
- A value that cannot be steered, such as a constant held in a variable, a `CASE` result, or a field that the program wrote itself.
- ABLSEC001 and ABLSEC002 match `FIND-FIRST`, `FIND-LAST`, `FIND-UNIQUE` and `CommandText` by spelling, so a user method of the same name is checked.
- ABLSEC005 reports the helper that checks a path (`FILE-INFO:FILE-NAME = path`), because it assigns a non-literal.
- ABLSEC004 reports a `DYNAMIC-INVOKE` whose method name is a variable even when the program fills it from a fixed table.

False negatives (a sink the rules do not see):

- A sink whose whole text is the body of a `&GLOBAL-DEFINE` or `&SCOPED-DEFINE`: findings that start in expansion text are not reported by `oxabl check`.
- A query, command, SQL text or path that is built in one procedure and used in another.
- Sinks that the rules do not name: `EXEC SQL` embedded SQL, `System.Diagnostics.Process`, `System.IO` classes, `COMPILE ... SAVE INTO VALUE(dir)`, `DYNAMIC-PROPERTY`, reflection through `Progress.Reflect`.
- `DYNAMIC-INVOKE(cClassName, "Method")` where the first argument is a character variable that names the class.
- A clause that a modelled statement discards, and a `FILL-WHERE-STRING` assignment inside a statement that the parser does not model.
- An include argument that is empty in a fragment: `QUERY-PREPARE({1})` expands to `QUERY-PREPARE()` when the include is checked alone.
