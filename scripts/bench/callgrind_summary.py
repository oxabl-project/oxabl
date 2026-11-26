#!/usr/bin/env python3
"""Summarise a callgrind profile of the oxabl binary as one JSON object.

Usage: callgrind_summary.py [--row] <callgrind.out>

With --row, prints the numbers as one row of `|`-separated cells instead.

Runs callgrind_annotate (on PATH) three times: the inclusive costs, the self
costs and the caller tree. Prints the instruction total, the inclusive share
and the call count of `tokenize` and `parse_program`, and the self share of
the allocator and of memcpy/memmove. A C library with no symbols shows its
functions as addresses (memcpy among them): their self share is
`unnamed_self`.

A call counts as "full" when its caller's cost for each call is a tenth of the
most expensive caller's or more, and "small" otherwise: `full_calls` is about the number of
times the command lexes or parses a whole file, and `full_callers` names them. Standard library only; run it with `python3 -I`.
"""

import json
import re
import subprocess
import sys

ROW = re.compile(r"^\s*([\d,]+) \(\s*([\d.]+)%\)\s+(<|\*|>)?\s*(.*?)(?: \((\d+)x\))?(?: \[.*\])?$")
TOTAL = re.compile(r"^\s*([\d,]+) \(100\.0%\)\s+PROGRAM TOTALS")

STAGES = {
    "tokenize": re.compile(r"(^|::)oxabl_lexer::tokenize$"),
    "parse_program": re.compile(r"(^|::|>::)parse_program$"),
}
ALLOCATOR = re.compile(
    r"(_rjem_|\bje_|jemalloc|__rust_(alloc|dealloc|realloc|alloc_zeroed)$|"
    r"(^|:)(malloc|free|realloc|calloc|sdallocx|_int_malloc|_int_free|cfree)$)"
)
MEMCPY = re.compile(r"(^|:)(__)?mem(cpy|move)")
UNNAMED = re.compile(r"^0x[0-9a-fA-F]+$")


def annotate(path, *args):
    out = subprocess.run(
        ["callgrind_annotate", "--threshold=100", *args, path],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    return out.splitlines()


def function(name):
    # "file:function" with the file "???" when it has no debug information.
    return name.split(":", 1)[1] if name.startswith("???:") else name


def costs(lines):
    """function -> cost, from a flat listing."""
    found = {}
    for line in lines:
        m = ROW.match(line)
        if m and m.group(3) is None:
            name = function(m.group(4).strip())
            found[name] = found.get(name, 0) + int(m.group(1).replace(",", ""))
    return found


def total(lines):
    for line in lines:
        m = TOTAL.match(line)
        if m:
            return int(m.group(1).replace(",", ""))
    raise SystemExit("callgrind_summary: no PROGRAM TOTALS line")


# A caller whose calls cost less than this part of the most expensive caller,
# for each call, is "small": the formatter lexes short pieces of text many
# times, which is not a lex of a whole file. Per call, so a walk of many small
# roots still counts each root's lex as full.
FULL_PART = 0.1


def calls(lines, pattern):
    """(all calls, full calls, [(caller, calls, share)] of the full callers)
    of each function that matches."""
    rows_of = []
    block = []
    for line in lines + [""]:
        if line.strip():
            block.append(line)
            continue
        rows = [m for m in map(ROW.match, block) if m]
        if any(m.group(3) == "*" and pattern.search(function(m.group(4).strip())) for m in rows):
            for m in rows:
                if m.group(3) == "<" and m.group(5):
                    cost = int(m.group(1).replace(",", ""))
                    rows_of.append((function(m.group(4).strip()), int(m.group(5)), cost, float(m.group(2))))
        block = []
    if not rows_of:
        return 0, 0, []
    largest = max(cost / n for _, n, cost, _ in rows_of)
    full = [(c, n, share) for c, n, cost, share in rows_of if cost / n >= FULL_PART * largest]
    return sum(n for _, n, _, _ in rows_of), sum(n for _, n, _ in full), full


def main():
    args = sys.argv[1:]
    row = args[:1] == ["--row"]
    if row:
        args = args[1:]
    if len(args) != 1:
        raise SystemExit(__doc__)
    path = args[0]
    inclusive_lines = annotate(path, "--inclusive=yes")
    instructions = total(inclusive_lines)
    inclusive = costs(inclusive_lines)
    own = costs(annotate(path))
    tree = annotate(path, "--tree=caller")
    summary = {"instructions": instructions}
    for stage, pattern in STAGES.items():
        cost = sum(c for name, c in inclusive.items() if pattern.search(name))
        every, full, callers = calls(tree, pattern)
        summary[stage] = {
            "share": round(100.0 * cost / instructions, 1),
            "calls": every,
            "full_calls": full,
            "full_callers": [{"caller": c, "calls": n, "share": sh} for c, n, sh in callers],
        }
    for key, pattern in (
        ("allocator_self", ALLOCATOR),
        ("memcpy_self", MEMCPY),
        ("unnamed_self", UNNAMED),
    ):
        cost = sum(c for name, c in own.items() if pattern.search(name))
        summary[key] = round(100.0 * cost / instructions, 1)
    if not row:
        print(json.dumps(summary))
        return

    def stage(key):
        st = summary[key]
        small = st["calls"] - st["full_calls"]
        return f"{st['share']}% ({st['full_calls']} full + {small} small)"

    cells = [
        f"{instructions:,}",
        stage("tokenize"),
        stage("parse_program"),
        f"{summary['allocator_self']}%",
        f"{summary['memcpy_self']}%",
        f"{summary['unnamed_self']}%",
    ]
    print("|" + "|".join(cells))


if __name__ == "__main__":
    main()
