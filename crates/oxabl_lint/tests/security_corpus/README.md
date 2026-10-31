# Security corpus

Synthetic ABL for the `security` rules ABLSEC001 to ABLSEC010 and ABLSEC013 to ABLSEC017. `tests/security_rules/corpus.rs` runs it. ABLSEC011 and ABLSEC012 need the files an entry point reaches: their corpus is the synthetic web application in `crates/oxabl_pipeline/tests/reach_corpus`, scored by `crates/oxabl_pipeline/tests/reach.rs`.

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

### ABLSEC010

ABLSEC010 reports only in taint mode, so `corpus.rs` runs it in taint mode, and checks that every vulnerable program gives nothing in sink mode. It reports once per routine and identity source: `many_decisions.p` (one source, four decisions) expects 1, `two_origins.p` expects 2, and `company_filter.p` (one source, two decisions) went from 2 to 1.

False positives: a decision on a request key that names an identity but is not one (`get-value("username-hint")`); a value the session set that a project function returns under a name in `identity-sources`; a check of an identity against the session (`IF cUser <> session-user() THEN RETURN.`), which is a correct check but reads the client's claim.

False negatives: a key that is not a literal (`get-value(cKey)`); a key with no word of `identity-keys` (`get-value("branch")` until it is configured); an identity read in another file and passed in (the parameter is a source, but not an identity); a decision in a statement the parser does not model.

### ABLSEC006 to ABLSEC009, ABLSEC013 to ABLSEC017

False positives (a finding on code that is safe):

- ABLSEC006: a value that was HTML-encoded (stage 1 knows no sanitiser); a `Write` on an object whose name holds `resp`, `response` or `writer` but that is not a web response (for example `oJsonWriter` or `oLogWriter`); a variable that holds a JSON object assigned to `Entity`.
- ABLSEC007: `SetHeader`, `AddHeader`, `SetCookie` and `SetLocation` are matched by spelling, so a user method of the same name is checked; a header passed as an object built elsewhere (`SetHeader(oHeader)`).
- ABLSEC008: a literal assigned to a name with a password word that is not a secret (a default of a policy field, a test value); a placeholder such as `"changeme"` is still a literal and is reported. Markup, templates and labels are not reported (`safe/password_markup.p`, `safe/password_templates.p`); a real secret with a lone `<`, `&` or `{` still is (`vulnerable/password_lookalikes.p`). Names that mark a role beside the password (`cForgotPasswordHref`, `cHideChngPwd`) and links without credentials are not reported (`safe/password_page_links.p`); a URL with `user:password@` or a `pwd=` parameter is (`vulnerable/password_urls.p`). A secret that looks like a template (`"ab&1cd"`, `"x%sy"`) is missed.
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

`taint/ablsecNNN/*.p` holds the cases of the taint mode, for the ten rules that have one, and `taint/ablsecNNN/*.i` include files that are checked on their own, without their includer. Each program marks every line where a rule reports in either mode: `/* taint: real */` when untrusted input reaches the sink, `/* taint: safe */` when it does not. A finding on a line with no mark fails the test, so the marks are the whole truth of the corpus. `tests/security_rules/taint_corpus.rs` scores both modes against the marks.

Each `cases.p` has, where the rule allows it: a tainted value used directly, through an assignment chain, through a temp-table field and through an unknown call; a value sanitised for the sink, and one sanitised for the wrong sink; a constant held in a variable; an unknown call with constant arguments; and a guard. Stage 2 must match every mark of a `cases.p`. A `limits.p` holds what stage 2 is known to get wrong, and the test pins those counts.

The result, from `cargo test -p oxabl_lint --test security_rules taint_corpus -- --nocapture`:

| Rule | Stage 1 TP / FP / FN | Precision | Recall | Stage 2 TP / FP / FN | Precision | Recall |
|---|---|---|---|---|---|---|
| ABLSEC001 | 6 / 7 / 0 | 0.46 | 1.00 | 6 / 1 / 0 | 0.86 | 1.00 |
| ABLSEC002 | 6 / 4 / 0 | 0.60 | 1.00 | 6 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC003 | 85 / 25 / 0 | 0.77 | 1.00 | 85 / 1 / 0 | 0.99 | 1.00 |
| ABLSEC004 | 7 / 5 / 0 | 0.58 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC005 | 7 / 4 / 0 | 0.64 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC006 | 7 / 5 / 0 | 0.58 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC007 | 7 / 3 / 0 | 0.70 | 1.00 | 7 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC014 | 3 / 2 / 0 | 0.60 | 1.00 | 3 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC015 | 2 / 1 / 0 | 0.67 | 1.00 | 2 / 0 / 0 | 1.00 | 1.00 |
| ABLSEC016 | 2 / 2 / 0 | 0.50 | 1.00 | 2 / 0 / 0 | 1.00 | 1.00 |
| All | 132 / 58 / 0 | 0.69 | 1.00 | 132 / 2 / 0 | 0.99 | 1.00 |

Six of the cases came with a review of the taint mode, and the analysis before it missed them (stage 2 was 50 / 2 / 6, recall 0.89): a sink in an include file whose operand the includer declares (`ablsec003/includer_operand.i`), an `INPUT DATASET`, a `BUFFER-COPY` and a `READ-JSON` (`ablsec003/request_data.p`), and a header value encoded with `html-encode` or `url-encode` (`ablsec007/cases.p`, which is now reported). Four more came with a second review, and the analysis before it missed them (stage 2 was 56 / 2 / 4, recall 0.93): a field of the includer's table in an include file (`ablsec003/includer_table.i`), an includer variable named like an unreserved keyword (`ablsec003/includer_operand.i`), and a `READ-JSON` through a handle of the table and one from a file (`ablsec003/request_data.p`). Four more came with a third review (stage 2 was 60 / 2 / 4, recall 0.94): a `SHARED` and a `NEW GLOBAL SHARED` variable, a `NEW SHARED` variable after a `RUN` of another program, and a main block variable that a function writes (`ablsec003/shared.p`). Three more came with a fourth review (stage 2 was 64 / 2 / 3, recall 0.96): a `NEW SHARED` variable after a `RUN p IN h` of a procedure name the file also defines and after a `PUBLISH`, and a main block variable that a function writes, read in the statement that calls the function (`ablsec003/shared.p`). Six more came with a later review, and the analysis before it missed every one (stage 2 was 72 / 2 / 6, recall 0.92): clauses that the parser did not put in their statement's node, the `WHEN` of an `ASSIGN` pair (a "default when empty" pair, one that a request value decides, a call in the condition), and a call in a joined record phrase, a `WHILE` and a `BY` of `FOR EACH` (`ablsec003/dropped_clauses.p`). Seven more came with the review after it, and the analysis before it missed six (stage 2 was 80 / 2 / 6, recall 0.93): values that the runtime carries between statements, outside any variable: `RETURN-VALUE` after a `RUN` of a procedure that returns web input, returns its argument or raises it with `RETURN ERROR`, and after a `RUN` of another program; the message of a caught error; `ERROR-STATUS:GET-MESSAGE`; and `BUFFER-VALUE` through a handle (`ablsec003/runtime_values.p`, with two `safe` controls). Twenty-three more came with the review after that, and the analysis before it missed eighteen (stage 2 was 91 / 2 / 18, recall 0.83): `RETURN-VALUE` that the `RETURN` action of a block's `ON ERROR` or `ON ENDKEY` phrase sets, also where nothing in the block is seen to raise the condition; what a built-in method writes into its argument (a node that `GET-DOCUMENT-ELEMENT` and `GET-CHILD` write, `NODE-VALUE-TO-LONGCHAR`, `SAVE` and `WRITE-JSON` to a `LONGCHAR`, and the `NAME` of such a node, in `ablsec004/runtime_values.p` too); a copy of `SOURCE-PROCEDURE`, `SELF` or `THIS-PROCEDURE` and the `PRIVATE-DATA` of `SESSION` and `THIS-PROCEDURE` that the file does not set (`ablsec003/runtime_values.p`); and `DYNAMIC-FUNCTION` of a function of the file and `IN` a handle (`ablsec003/dynamic_function.p`). Four of them pin what the analysis already did: `RETURN-VALUE` at the start of a unit and after a call that may run other code (`ablsec003/return_value_calls.p`); removing either write fails the test. One more, a field read through `BUFFER b:` in a sink, was Unknown before and now has its path (`tests/security_rules/taint.rs` checks the path). Twenty-three more came with the review after that, and the analysis before it missed twenty-one (stage 2 on `ablsec003` was 64 / 2 / 21): a variable that is not `NO-UNDO` and that an `UNDO` gives back its value from the start of the block, also in the `RETURN` action of the `ON` phrase (`ablsec003/undo.p`, with two `safe` controls); a handle that a foreign handle, `SESSION` or `THIS-PROCEDURE` reaches (`SELF:HANDLE`, `SOURCE-PROCEDURE:NEXT-SIBLING`, `SESSION:FIRST-CHILD`, in `ablsec003/runtime_values.p`); a document that a node was appended to or an attribute set on, a copy of a handle and a list that `ADD-LAST` filled (`ablsec003/handle_aliases.p`); what another program, a function `IN` a handle, `DYNAMIC-INVOKE` and a method of an object give back in a result or an `OUTPUT` argument, and a function of the includer in an include file (`ablsec003/calls_elsewhere.p`, `ablsec003/includer_operand.i`); and `DYNAMIC-FUNCTION` of a function of the file with arguments (`ablsec003/dynamic_function.p`, where the `safe` control with a literal argument was reported before). A function that a closed file does not declare stays a `safe` control (`ablsec003/calls_elsewhere.p`): ABL compiles no such call. A `RUN ... ON SERVER h` whose server a request value chose (`ablsec003/dropped_clauses.p`) pins the part of a statement that its node does not hold and that is not a condition: it adds its value to what the statement writes.

The corpus is small and was written with the analysis, so these numbers say that the analysis does what it was designed to do. They do not measure real code. The two false alarms are the known limits of stage 2:

- Reported: a check in a project function that `validators` does not name, and an allow-list held in a variable instead of a literal.

The cases of calls inside one file (a function that returns web input, a function that quotes its argument, a `PRIVATE` procedure run with untrusted input, an `OUTPUT` parameter) and of JSON objects (`Add` then `Write`, an array that holds the object, an object of constants) are in the `cases.p` of ABLSEC001, ABLSEC003, ABLSEC005 and ABLSEC006. Before calls were followed, stage 2 missed the first and the third.

Other classes, not in the corpus:

- False negatives: what a program of another file returns; statements the parser does not model (only `COPY-LOB` is read); `RAW-TRANSFER`, a dynamic `BUFFER-VALUE` assignment, a table bound with `BIND` and a handle that gets its table other than by `h = TEMP-TABLE tt:HANDLE`; a temp-table field read without its table name; a value inside an object (`oFilter:Where = pc` taints `oFilter` as a whole, so a later read of another member is tainted too).
- False positives: an unknown call whose result does not carry its argument (`LENGTH` and other number functions are known, and so are the routines of the same file; a function of another file that returns a fixed string from a tainted key is not); a guard the analysis does not read (`IF INDEX(x, "..") > 0 THEN RETURN.`, a regular-expression check, `MATCHES`); a request-named object that is not a web request; and a sink stage 2 cannot judge, which it reports as stage 1 does: in a unit past its work budget or reading what one writes, after a chain of calls or file-level variables deeper than the eight rounds, in a unit that holds an include that did not arrive, on a variable or temp-table field bound as `SHARED` or `NEW GLOBAL SHARED`, on a shared variable after a call of code the file does not hold (a `RUN` of another program, a function `IN h`, `PUBLISH`, a member of an object, ...), on any file-level variable, field or parameter of a file where an include did not arrive or of a routine of an include file checked on its own, and, in an include file checked on its own, on a variable, an unqualified database field or a field of a table of the includer (and on any database field, when no schema is loaded).

`crates/oxabl/examples/security_corpus.rs` runs both modes over a tree (`OXABL_SECURITY_CORPUS=<dir>`). Over this directory, stage 2 reports 204 findings where stage 1 reports 261 (22% fewer); the rules without a taint mode do not change. Most stage 1 programs here have no untrusted source at all, because stage 1 does not need one.
