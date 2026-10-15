DEFINE VARIABLE hA AS HANDLE NO-UNDO.
DEFINE VARIABLE hB AS HANDLE NO-UNDO.
DEFINE VARIABLE hC AS HANDLE NO-UNDO.
DEFINE VARIABLE hD AS HANDLE NO-UNDO.

RUN a.p PERSISTENT SET hA.
RUN DestroyThis IN hA.

RUN b.p PERSISTENT SET hB.
RUN destroyObject IN hB.

/* ADM dispatch with the destroy method. */
RUN c.p PERSISTENT SET hC.
RUN dispatch IN hC ('destroy').

/* Not a destroy name: still a leak. */
RUN d.p PERSISTENT SET hD.
RUN undestroy IN hD.
RUN dispatch IN hD ('initialize').
