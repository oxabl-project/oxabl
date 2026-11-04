CLASS cyc.Pong:
    DEFINE PRIVATE VARIABLE other AS cyc.Ping NO-UNDO.

    METHOD PUBLIC CHARACTER Name():
        RETURN "pong".
    END METHOD.

    METHOD PUBLIC CHARACTER Peer():
        other = NEW cyc.Ping().
        RETURN other:Name().
    END METHOD.
END CLASS.
