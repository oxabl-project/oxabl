CLASS cyc.Ping:
    DEFINE PRIVATE VARIABLE other AS cyc.Pong NO-UNDO.

    METHOD PUBLIC CHARACTER Name():
        RETURN "ping".
    END METHOD.

    METHOD PUBLIC CHARACTER Peer():
        other = NEW cyc.Pong().
        RETURN other:Name().
    END METHOD.
END CLASS.
