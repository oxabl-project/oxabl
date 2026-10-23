# Security corpus

Synthetic ABL for the `security` rules ABLSEC001 to ABLSEC009 and ABLSEC013 to ABLSEC017. `tests/security_rules/corpus.rs` runs it.

- `ablsecNNN/vulnerable/*.p` starts with `/* expect: N */`. The rule gives exactly N findings.
- `ablsecNNN/safe/*.p` gives no finding from any security rule.
- A program must parse cleanly, unless its name ends in `_unparsed`: it holds a construct that the parser rejects, and the rule reads its tokens.
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

### ABLSEC006 to ABLSEC009, ABLSEC013 to ABLSEC017

False positives (a finding on code that is safe):

- ABLSEC006: a value that was HTML-encoded (stage 1 knows no sanitiser); a `Write` on an object whose name holds `resp`, `response` or `writer` but that is not a web response (for example `oJsonWriter` or `oLogWriter`); a variable that holds a JSON object assigned to `Entity`.
- ABLSEC007: `SetHeader`, `AddHeader`, `SetCookie` and `SetLocation` are matched by spelling, so a user method of the same name is checked; a header passed as an object built elsewhere (`SetHeader(oHeader)`).
- ABLSEC008: a literal assigned to a name with a password word that is not a secret (a default of a policy field, a test value); a placeholder such as `"changeme"` is still a literal and is reported.
- ABLSEC009: a digest of a value whose name holds `token`, `session`, `salt`, `password` and that is not secret (a cache key); `RANDOM` for an identifier that is only a display number.
- ABLSEC013: a `CATCH` variable and an `OUTPUT` parameter share one set of names for the whole file, so a local variable that reuses the name of another procedure's output parameter is reported.
- ABLSEC014: `Deserialize` is matched by spelling, so a user method of that name is checked.
- ABLSEC015 and ABLSEC016 are reviews: every `SAX-PARSE` is reported, whatever the reader's configuration; a `SET-SIZE` after a guard is reported; one `BATCH-SIZE` assignment in a file covers every `FILL` of the file, and a file with none is reported even if the caller sets it.
- ABLSEC017: `DISABLE TRIGGERS` in a load utility, which is where it belongs. List those programs in an override.

False negatives (a sink the rules do not see):

- ABLSEC006 and ABLSEC013: a `{&OUT}` that no include defines. The preprocessor expands the name to nothing, so the write is gone from the text that the rules read. `EXPORT STREAM WebStream`, `DISPLAY STREAM WebStream`, `{&OUT-LONG}` and a response that is written through a helper.
- ABLSEC007: a header that `{&OUT}` writes, and a redirect that a procedure the rule does not name takes.
- ABLSEC008: a secret passed as a procedure or method argument, in a temp-table `INITIAL` value or in a property; a secret in a file that the program reads at run time.
- ABLSEC009: a hash of a password that has a neutral name; `RANDOM` whose result goes through a parameter to a token.
- ABLSEC013: error text that is kept in a local variable and returned later; a response that is written through a helper; the receiver of `GetMessage` that is a parameter with a type other than a `...Error` class.
- ABLSEC014: a class name from data is reported by ABLSEC004, not here. `READ-JSON` and `READ-XML` are not object deserialisation and are not reported.
- ABLSEC015: `X-DOCUMENT:LOAD` with a literal source; the XML library of a .NET class.
- ABLSEC016: sizes that reach `SET-SIZE` through a `MEMPTR` copy; a `FILL` with a `BATCH-SIZE` that is set on another object; other unbounded reads (`COPY-LOB`, `IMPORT`).
- ABLSEC017: `UPDATE`, `SET`, `IMPORT` and `INSERT` into `_User`, and a buffer of `_User` that a handle reaches.

## Stage 2: the taint corpus

`taint/ablsecNNN/*.p` holds the cases of the taint mode, for the ten rules that have one. Each program marks every line where a rule reports in either mode: `/* taint: real */` when untrusted input reaches the sink, `/* taint: safe */` when it does not. A finding on a line with no mark fails the test, so the marks are the whole truth of the corpus. `tests/security_rules/taint_corpus.rs` scores both modes against the marks.

Each `cases.p` has, where the rule allows it: a tainted value used directly, through an assignment chain, through a temp-table field and through an unknown call; a value sanitised for the sink, and one sanitised for the wrong sink; a constant held in a variable; an unknown call with constant arguments; and a guard. Stage 2 must match every mark of a `cases.p`. A `limits.p` holds what stage 2 is known to get wrong, and the test pins those counts.

The result, from `cargo test -p oxabl_lint --test security_rules taint_corpus -- --nocapture`:

| Rule | Stage 1 TP / FP / FN | Precision | Recall | Stage 2 TP / FP / FN | Precision | Recall |
|---|---|---|---|---|---|---|
| ABLSEC001 | 6 / 7 / 0 | 0.46 | 1.00 | 6 / 1 / 0 | 0.86 | 1.00 |
| ABLSEC002 | 6 / 4 / 0 | 0.60 | 1.00 | 6 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC003 | 6 / 6 / 0 | 0.50 | 1.00 | 6 / 1 / 0 | 0.86 | 1.00 |
| ABLSEC004 | 6 / 4 / 0 | 0.60 | 1.00 | 6 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC005 | 7 / 4 / 0 | 0.64 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC006 | 7 / 5 / 0 | 0.58 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC007 | 5 / 4 / 0 | 0.56 | 1.00 | 5 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC014 | 3 / 2 / 0 | 0.60 | 1.00 | 3 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC015 | 2 / 1 / 0 | 0.67 | 1.00 | 2 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC016 | 2 / 2 / 0 | 0.50 | 1.00 | 2 / 0 / 0 | 1.00 | 1.00 |
| All | 50 / 39 / 0 | 0.56 | 1.00 | 50 / 2 / 0 | 0.96 | 1.00 |

The corpus is small and was written with the analysis, so these numbers say that the analysis does what it was designed to do. They do not measure real code. The two false alarms are the known limits of stage 2:

- Reported: a check in a project function that `validators` does not name, and an allow-list held in a variable instead of a literal.

The cases of calls inside one file (a function that returns web input, a function that quotes its argument, a `PRIVATE` procedure run with untrusted input, an `OUTPUT` parameter) and of JSON objects (`Add` then `Write`, an array that holds the object, an object of constants) are in the `cases.p` of ABLSEC001, ABLSEC003, ABLSEC005 and ABLSEC006. Before calls were followed, stage 2 missed the first and the third.

Other classes, not in the corpus:

- False negatives: what a program of another file returns; what a function or method writes to a main block variable; `SHARED` variables; statements the parser does not model (only `COPY-LOB` is read); a value inside an object (`oFilter:Where = pc` taints `oFilter` as a whole, so a later read of another member is tainted too).
- False positives: an unknown call whose result does not carry its argument (`LENGTH` and other number functions are known, and so are the routines of the same file; a function of another file that returns a fixed string from a tainted key is not); a guard the analysis does not read (`IF INDEX(x, "..") > 0 THEN RETURN.`, a regular-expression check, `MATCHES`); and a request-named object that is not a web request.

`crates/oxabl/examples/security_corpus.rs` runs both modes over a tree (`OXABL_SECURITY_CORPUS=<dir>`). Over this directory, stage 2 reports 160 findings where stage 1 reports 230 (30% fewer); the rules without a taint mode do not change. Most stage 1 programs here have no untrusted source at all, because stage 1 does not need one.
