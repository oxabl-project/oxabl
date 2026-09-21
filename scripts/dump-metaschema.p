/* dump-metaschema.p -- dump an OpenEdge database's metaschema and virtual
   system tables to a pipe-delimited text file.

   Run against any OpenEdge database (the shipped `empty.db` is ideal, since
   it has no application schema). Every table with a negative _File-Number is
   part of the dictionary or the VST set, i.e. present in every database.

   Output is consumed by scripts/gen-metaschema-df.py. See METASCHEMA.md. */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE STREAM sDump.

DEFINE VARIABLE cOut   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTable AS INTEGER   NO-UNDO.

ASSIGN cOut = OS-GETENV("OXABL_METASCHEMA_OUT":U).
IF cOut EQ ? OR cOut EQ "":U THEN
  ASSIGN cOut = "metaschema.txt":U.

/* A named stream, so that anything the runtime prints on its own cannot land
   in the dump and be mistaken for a record. */
OUTPUT STREAM sDump TO VALUE(cOut).
DO ON ERROR UNDO, THROW:

  FOR EACH _file WHERE _file._File-Number < 0 NO-LOCK
      BY _file._File-Number DESCENDING:

    ASSIGN iTable = iTable + 1.

    PUT STREAM sDump UNFORMATTED
      "T|":U _file._File-Name
      "|":U _file._File-Number
      "|":U (IF _file._Frozen THEN "1":U ELSE "0":U) SKIP.

    FOR EACH _field OF _file NO-LOCK BY _field._Order:
      PUT STREAM sDump UNFORMATTED
        "F|":U _file._File-Name
        "|":U _field._Field-Name
        "|":U _field._Data-Type
        "|":U _field._Extent
        "|":U (IF _field._Mandatory THEN "1":U ELSE "0":U) SKIP.
    END.

    FOR EACH _index OF _file NO-LOCK BY _index._Index-Name:
      PUT STREAM sDump UNFORMATTED
        "I|":U _file._File-Name
        "|":U _index._Index-Name
        "|":U (IF _index._Unique THEN "1":U ELSE "0":U)
        "|":U (IF RECID(_index) EQ _file._Prime-Index THEN "1":U ELSE "0":U) SKIP.

      FOR EACH _index-field OF _index NO-LOCK,
          FIRST _field OF _index-field NO-LOCK
          BY _index-field._Index-Seq:
        PUT STREAM sDump UNFORMATTED
          "X|":U _file._File-Name
          "|":U _index._Index-Name
          "|":U _field._Field-Name
          "|":U (IF _index-field._Ascending THEN "1":U ELSE "0":U)
          "|":U (IF _index-field._Abbreviate THEN "1":U ELSE "0":U) SKIP.
      END.
    END.
  END.

  /* A trailer the renderer requires. Without it a dump cut short by a runtime
     error is indistinguishable from a complete one, and would quietly
     overwrite the checked-in catalog with a truncated copy. */
  PUT STREAM sDump UNFORMATTED "END|":U iTable SKIP.

  /* `QUIT` cannot carry an exit code, so a failure is signalled by the
     absence of the trailer above: the renderer refuses a dump without one
     rather than overwriting a good catalog with a partial copy. */
  CATCH eAny AS Progress.Lang.Error:
    MESSAGE "dump-metaschema.p failed: ":U eAny:GetMessage(1).
  END CATCH.
END.

OUTPUT STREAM sDump CLOSE.

QUIT.
