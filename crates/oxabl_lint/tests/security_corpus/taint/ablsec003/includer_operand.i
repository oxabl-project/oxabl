/* os-command (ABLSEC003): an include file checked on its own. Its includer
   declares pcFile, maybe as an input parameter, so the include cannot judge
   it. */
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
OS-COMMAND SILENT VALUE("gzip " + pcFile). /* taint: real */

cCmd = "df -k".
OS-COMMAND SILENT VALUE(cCmd). /* taint: safe */
