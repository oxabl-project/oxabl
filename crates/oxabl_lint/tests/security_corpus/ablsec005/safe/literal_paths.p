DEFINE VARIABLE lcData AS LONGCHAR NO-UNDO.

INPUT FROM "input.txt".
INPUT FROM VALUE("/data/input.txt").
OUTPUT TO VALUE("/data/" + "out.txt").
OS-DELETE "old.txt".
OS-DELETE VALUE("/tmp/old.txt").
OS-CREATE-DIR "archive".
OS-COPY "a.txt" "b.txt".
COPY-LOB FROM lcData TO FILE "/tmp/out.txt".
FILE-INFO:FILE-NAME = "/tmp".
