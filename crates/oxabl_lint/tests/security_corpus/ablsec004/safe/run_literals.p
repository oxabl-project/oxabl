DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

RUN post-invoice.p (INPUT 1).
RUN VALUE("post-invoice.p") (INPUT 1).
RUN VALUE("post-" + "invoice.p") (INPUT 1).
RUN inner-proc IN hProc.
