#!/usr/bin/env bash
# Seeds every target's corpus with the ABL files of the repository that fit the
# input limit. Run from anywhere; the corpus lands in fuzz/corpus/<target>.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
max=${MAX_LEN:-65536}
for target in lex preprocess_parse lint format outline navigate; do
  dest="$here/corpus/$target"
  mkdir -p "$dest"
  find "$root/resources" "$root/crates" -type f \
    \( -name '*.p' -o -name '*.w' -o -name '*.cls' -o -name '*.i' -o -name '*.v' -o -name '*.abl' \) \
    -not -path '*/target/*' -size -"${max}"c -print0 |
    while IFS= read -r -d '' f; do
      cp -n "$f" "$dest/$(sha1sum "$f" | cut -c1-16)"
    done
done

# Deep and exponential shapes a coverage-guided run does not reach by itself.
python3 - "$here/corpus" <<'PY'
import sys
from pathlib import Path

shapes = {
    "parens": "x = " + "(" * 3000 + "1" + ")" * 3000 + ".\n",
    "do-nest": "DO:\n" * 2000 + "END.\n" * 2000,
    "if-nest": "IF TRUE THEN " * 1500 + "MESSAGE 1.\n",
    "else-if-chain": "IF a THEN x = 1.\n" + "ELSE IF a THEN x = 1.\n" * 3000,
    "braces": "{" * 5000,
    "macro-doubling": "&SCOPED-DEFINE a0 x\n"
    + "".join(f"&SCOPED-DEFINE a{i + 1} {{&a{i}}}{{&a{i}}}\n" for i in range(24))
    + "{&a24}\n",
}
for target in ("lex", "preprocess_parse", "lint", "format", "outline"):
    for name, text in shapes.items():
        (Path(sys.argv[1]) / target / f"shape-{name}").write_text(text)

# navigate: `source \0 schema \0 include`. Names a table and its field share,
# names that are keywords, hyphens, digits, non-ASCII and very long names, a
# table in two `ADD TABLE`s, abbreviations, and broken `.df` text.
names = ["company", "warehouse", "cust-num", "for", "each", "Größe", "x" * 300,
         "a#b", "pct%", "amp&", "_file", "t1", "T1", "name", "nam"]
df = "".join(f'ADD TABLE "{n}"\n  AREA "Data"\n\n' + "".join(
    f'ADD FIELD "{f}" OF "{n}" AS character\n  FORMAT "x(8)"\n\n' for f in names[:6] + [n])
    for n in names)
uses = "".join(
    f"FIND FIRST {n} NO-LOCK NO-ERROR.\nDISPLAY {n}.{n} {n}.name {n}.nam.\n"
    f"DEFINE TEMP-TABLE tt{i} NO-UNDO FIELD {n} LIKE {n}.{n} FIELD y LIKE {n}.nosuch.\n"
    f"DEFINE BUFFER b{i} FOR {n}.\nFOR EACH {n}: DISPLAY b{i}.{n} tt{i}.{n}. END.\n"
    for i, n in enumerate(names))
nav = Path(sys.argv[1]) / "navigate"
nav.mkdir(parents=True, exist_ok=True)
(nav / "shape-names").write_text(uses + "\0" + df + "\0" + uses)
(nav / "shape-calls").write_text(
    "USING app.*.\nUSING app.C.\n{inc.i}\n{inc.i &a=1 2}\nDEFINE VARIABLE o AS CLASS app.C NO-UNDO.\n"
    "o = NEW app.C().\no:M().\no:N().\nRUN p.p.\nRUN p.\nRUN two.p.\nRUN \"p.p\".\nRUN VALUE(\"p\").\n"
    "RUN Local.\nPROCEDURE Local:\nEND PROCEDURE.\n"
    "\0" + df + "\0DEFINE VARIABLE fromInc AS INTEGER NO-UNDO.\nfromInc = 1.\n")
(nav / "shape-broken-df").write_text(
    "FIND FIRST company.\nDISPLAY company.company.\n\0ADD TABLE \"company\nADD FIELD \"company\" OF\n\0")
PY
