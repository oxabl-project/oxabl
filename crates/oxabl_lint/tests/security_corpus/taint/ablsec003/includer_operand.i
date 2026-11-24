/* os-command (ABLSEC003): an include file checked on its own. Its includer
   declares pcFile, maybe as an input parameter, so the include cannot judge
   it. */
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
OS-COMMAND SILENT VALUE("gzip " + pcFile). /* taint: real */

/* SERVER is a keyword, but not a reserved one, so it can name a variable. */
OS-COMMAND SILENT VALUE("ping " + server). /* taint: real */

cCmd = "df -k".
OS-COMMAND SILENT VALUE(cCmd). /* taint: safe */

/* A function of the includer: its code is not in this file. */
OS-COMMAND SILENT VALUE(includer-cmd("ls")). /* taint: real */
