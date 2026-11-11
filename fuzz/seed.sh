#!/usr/bin/env bash
# Seeds every target's corpus with the ABL files of the repository that fit the
# input limit. Run from anywhere; the corpus lands in fuzz/corpus/<target>.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
max=${MAX_LEN:-65536}
for target in lex preprocess_parse lint format; do
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
for target in ("lex", "preprocess_parse", "lint", "format"):
    for name, text in shapes.items():
        (Path(sys.argv[1]) / target / f"shape-{name}").write_text(text)
PY
