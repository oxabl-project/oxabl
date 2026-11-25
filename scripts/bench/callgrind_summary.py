#!/usr/bin/env python3
"""Summarise a callgrind profile of the oxabl binary as one JSON object.

Usage: callgrind_summary.py [--row] <callgrind.out>

With --row, prints the numbers as one row of `|`-separated cells instead.

Runs callgrind_annotate (on PATH) three times: the inclusive costs, the self
costs and the caller tree. Prints the instruction total, the inclusive share
and the call count of `tokenize` and `parse_program`, and the self share of
the allocator and of memcpy/memmove. A C library with no symbols shows its
functions as addresses (memcpy among them): their self share is
`unnamed_self`. Standard library only; run it with `python3 -I`.
"""

import json
import re
import subprocess
import sys

ROW = re.compile(r"^\s*([\d,]+) \(\s*[\d.]+%\)\s+(<|\*|>)?\s*(.*?)(?: \((\d+)x\))?(?: \[.*\])?$")
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
        if m and m.group(2) is None:
            name = function(m.group(3).strip())
            found[name] = found.get(name, 0) + int(m.group(1).replace(",", ""))
    return found


def total(lines):
    for line in lines:
        m = TOTAL.match(line)
        if m:
            return int(m.group(1).replace(",", ""))
    raise SystemExit("callgrind_summary: no PROGRAM TOTALS line")


def calls(lines, pattern):
    """Sum of the (Nx) of the callers of each function that matches."""
    count = 0
    block = []
    for line in lines + [""]:
        if line.strip():
            block.append(line)
            continue
        callers = 0
        hit = False
        for row in block:
            m = ROW.match(row)
            if not m:
                continue
            if m.group(2) == "<" and m.group(4):
                callers += int(m.group(4))
            if m.group(2) == "*" and pattern.search(function(m.group(3).strip())):
                hit = True
        if hit:
            count += callers
        block = []
    return count


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
        summary[stage] = {
            "share": round(100.0 * cost / instructions, 1),
            "calls": calls(tree, pattern),
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
        return f"{summary[key]['share']}% ({summary[key]['calls']} calls)"

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
