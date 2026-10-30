/* expect: 1 */
CLASS Acme.Store:
    METHOD PUBLIC Progress.Lang.Object Load(INPUT pmData AS MEMPTR):
        RETURN oSer:Deserialize(pmData).
    END METHOD.
END CLASS.
