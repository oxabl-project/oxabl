/* A batch job that no entry point runs. */
DEFINE INPUT PARAMETER pcDir AS CHARACTER NO-UNDO.
OS-COMMAND SILENT VALUE("rm -rf " + pcDir).
