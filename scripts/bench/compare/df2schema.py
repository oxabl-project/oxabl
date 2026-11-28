"""Write the flat schema file of the Joanju/Consultingwerk Proparse from .df files.

Format: ':: <db> <n>', ': <table> <recid>', '<field> <recid> <TYPE> <extent>'.
Usage: python3 -I df2schema.py out.schema [--meta metaschema.df] a.df b.df ...
--meta adds the dictionary tables (_file, _field, ...) to each database.
"""
import re
import sys
from pathlib import Path

TABLE = re.compile(r'^ADD TABLE "([^"]+)"', re.I)
FIELD = re.compile(r'^ADD FIELD "([^"]+)" OF "([^"]+)" AS (\S+)', re.I)
EXTENT = re.compile(r'^\s+EXTENT (\d+)', re.I)


def main() -> None:
    out = open(sys.argv[1], "w", encoding="utf-8")
    recid = 1
    args = sys.argv[2:]
    meta = None
    if args[0] == "--meta":
        meta, args = args[1], args[2:]
    for n, df in enumerate(args, start=1):
        tables: dict[str, list[list]] = {}
        order: list[str] = []
        field = None
        text = Path(df).read_text(encoding="latin-1")
        if meta:
            text += "\n" + Path(meta).read_text(encoding="latin-1")
        for line in text.splitlines():
            if m := TABLE.match(line):
                tables.setdefault(m[1], [])
                order.append(m[1])
                field = None
            elif m := FIELD.match(line):
                field = [m[1], m[3].upper(), 0]
                tables.setdefault(m[2], []).append(field)
            elif (m := EXTENT.match(line)) and field is not None:
                field[2] = int(m[1])
            elif line.startswith("ADD ") or line.startswith("UPDATE "):
                field = None
        out.write(f":: {Path(df).stem} {n}\n")
        for t in order:
            out.write(f": {t} {recid}\n")
            recid += 1
            for name, typ, ext in tables[t]:
                out.write(f"{name} {recid} {typ} {ext}\n")
                recid += 1
    out.close()


main()
