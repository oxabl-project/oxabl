DEFINE VARIABLE ping AS cyc.Ping NO-UNDO.

ping = NEW cyc.Ping().
PUT UNFORMATTED ping:Name() " " ping:Peer() SKIP.
