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
