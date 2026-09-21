#!/usr/bin/env python3
"""Turn a dump-metaschema.p dump into crates/oxabl_schema/resources/metaschema.df.

Reads the pipe-delimited text that scripts/dump-metaschema.p writes and emits
a `.df` covering table names, field names, data types, extents, mandatory
flags and index shapes -- and nothing else. Labels, formats, help text and
descriptions are dropped: oxabl never reads them, and they would triple the
size of a file that ships in every build.

Usage: gen-metaschema-df.py <dump.txt> <out.df> <openedge-version>
"""

import collections
import os
import sys

# Floors the dump must clear. A metaschema only ever grows, so a dump that
# comes back materially smaller than the one in the repo is a truncated dump,
# not a smaller OpenEdge. Failing here is the point: the alternative is
# overwriting a good catalog with a partial one that still looks plausible.
MIN_TABLES = 120
MIN_FIELDS = 1600

HEADER = """# OpenEdge metaschema and virtual system tables.
#
# GENERATED FILE - DO NOT EDIT. Regenerate with scripts/gen-metaschema-df.sh;
# see crates/oxabl_schema/METASCHEMA.md.
#
# Every OpenEdge database carries these tables, so a `.df` dump of an
# application schema never mentions them. oxabl parses this file once and
# every loaded schema borrows it, resolving `_file`, `_field` and friends
# like any other table -- behind the user's own tables, which always win.
#
# Names, data types, extents, mandatory flags and index shapes only: no
# labels, formats, help text or descriptions. Layout as of OpenEdge {version}.
"""


def read_dump(path):
    """Parse a dump, refusing anything that is not demonstrably complete.

    Every rejection here guards the same failure: the ABL side writes to a
    named stream and stops on error, but it cannot set an exit code, so a run
    cut short leaves a file that parses fine and is simply missing the rest.
    """
    tables = collections.OrderedDict()
    trailer = None
    with open(path, encoding="utf-8") as fh:
        for lineno, raw in enumerate(fh, 1):
            line = raw.rstrip("\n")
            if not line:
                continue
            parts = line.split("|")
            tag = parts[0]
            if trailer is not None:
                raise ValueError("line %d follows the END trailer" % lineno)
            if tag == "END":
                trailer = int(parts[1])
            elif tag == "T":
                if parts[1] in tables:
                    raise ValueError("line %d redeclares table %s" % (lineno, parts[1]))
                tables[parts[1]] = {
                    "number": int(parts[2]),
                    "frozen": parts[3] == "1",
                    "fields": [],
                    "indexes": collections.OrderedDict(),
                }
            elif tag == "F":
                tables[parts[1]]["fields"].append(
                    (parts[2], parts[3], int(parts[4]), parts[5] == "1")
                )
            elif tag == "I":
                tables[parts[1]]["indexes"][parts[2]] = {
                    "unique": parts[3] == "1",
                    "primary": parts[4] == "1",
                    "fields": [],
                }
            elif tag == "X":
                tables[parts[1]]["indexes"][parts[2]]["fields"].append(
                    (parts[3], parts[4] == "1", parts[5] == "1")
                )
            else:
                # Anything else is runtime chatter that reached the dump, or a
                # format change. Either way, do not quietly drop it.
                raise ValueError("line %d has unknown tag %r" % (lineno, tag))

    if trailer is None:
        raise ValueError(
            "%s has no END trailer -- the dump did not run to completion" % path
        )
    if trailer != len(tables):
        raise ValueError(
            "%s declares %d tables but carries %d" % (path, trailer, len(tables))
        )
    return tables


def render(tables, version):
    out = [HEADER.format(version=version)]
    # Descending _File-Number puts the dictionary tables (_File is -1) first
    # and the virtual system tables last, which is the order a reader expects.
    for name, table in sorted(tables.items(), key=lambda kv: -kv[1]["number"]):
        out.append('ADD TABLE "%s"' % name)
        if table["frozen"]:
            out.append("  FROZEN")
        out.append("")

        for field, data_type, extent, mandatory in table["fields"]:
            out.append('ADD FIELD "%s" OF "%s" AS %s' % (field, name, data_type))
            if extent:
                out.append("  EXTENT %d" % extent)
            if mandatory:
                out.append("  MANDATORY")
            out.append("")

        for index, meta in table["indexes"].items():
            out.append('ADD INDEX "%s" ON "%s"' % (index, name))
            if meta["unique"]:
                out.append("  UNIQUE")
            if meta["primary"]:
                out.append("  PRIMARY")
            for field, ascending, abbreviated in meta["fields"]:
                out.append(
                    '  INDEX-FIELD "%s" %s%s'
                    % (
                        field,
                        "ASCENDING" if ascending else "DESCENDING",
                        " ABBREVIATED" if abbreviated else "",
                    )
                )
            out.append("")
    return "\n".join(out) + "\n"


def main(argv):
    if len(argv) != 4:
        sys.exit(__doc__)
    tables = read_dump(argv[1])
    fields = sum(len(t["fields"]) for t in tables.values())
    if len(tables) < MIN_TABLES or fields < MIN_FIELDS:
        sys.exit(
            "refusing to write: %d tables / %d fields is below the %d / %d floor"
            % (len(tables), fields, MIN_TABLES, MIN_FIELDS)
        )
    empty = [name for name, t in tables.items() if not t["fields"]]
    if empty:
        sys.exit("refusing to write: tables with no fields: %s" % ", ".join(empty))

    # Render first, then replace atomically: a crash part-way must leave the
    # checked-in catalog untouched rather than truncated.
    text = render(tables, argv[3])
    tmp = argv[2] + ".tmp"
    with open(tmp, "w", encoding="utf-8") as fh:
        fh.write(text)
    os.replace(tmp, argv[2])
    print("wrote %s: %d tables, %d fields" % (argv[2], len(tables), fields))


if __name__ == "__main__":
    main(sys.argv)
