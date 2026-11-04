DEFINE VARIABLE leaf AS pkg.Leaf NO-UNDO.

leaf = NEW pkg.Leaf().
PUT UNFORMATTED leaf:Describe() SKIP.
