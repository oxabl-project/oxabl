/* Run only through the front controller's RUN VALUE. */
DEFINE INPUT PARAMETER pcTheme AS CHARACTER NO-UNDO.
CREATE PageHit.
OS-COMMAND SILENT VALUE("touch /tmp/" + pcTheme).
