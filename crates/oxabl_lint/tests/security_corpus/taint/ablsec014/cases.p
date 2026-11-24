/* unsafe-deserialization (ABLSEC014): stage 2 cases. */
CLASS Acme.Store:
    DEFINE PRIVATE VARIABLE oSer AS Progress.IO.JsonSerializer NO-UNDO.
    DEFINE PRIVATE VARIABLE mCache AS MEMPTR NO-UNDO.
    DEFINE PRIVATE VARIABLE hSock AS HANDLE NO-UNDO.

    /* Tainted, directly: a public method's parameter. */
    METHOD PUBLIC Progress.Lang.Object Load(INPUT pmData AS MEMPTR):
        RETURN oSer:Deserialize(pmData). /* taint: real */
    END METHOD.

    /* Tainted through a socket read and an assignment chain. */
    METHOD PUBLIC Progress.Lang.Object Receive():
        DEFINE VARIABLE mRaw AS MEMPTR NO-UNDO.
        DEFINE VARIABLE mCopy AS MEMPTR NO-UNDO.
        hSock:READ(mRaw, 1, 4096).
        mCopy = mRaw.
        RETURN oSer:Deserialize(mCopy). /* taint: real */
    END METHOD.

    /* Tainted through an unknown call. */
    METHOD PUBLIC Progress.Lang.Object Decode(INPUT pcText AS LONGCHAR):
        RETURN oSer:Deserialize(to-memptr(pcText)). /* taint: real */
    END METHOD.

    /* A private method: its parameter is not a source. */
    METHOD PRIVATE Progress.Lang.Object LoadOwn(INPUT pmData AS MEMPTR):
        RETURN oSer:Deserialize(pmData). /* taint: safe */
    END METHOD.

    /* A class member that no untrusted value reaches. */
    METHOD PUBLIC Progress.Lang.Object Cached():
        RETURN oSer:Deserialize(mCache). /* taint: safe */
    END METHOD.
END CLASS.
