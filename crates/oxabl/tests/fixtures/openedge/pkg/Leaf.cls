CLASS pkg.Leaf INHERITS pkg.Mid:
    METHOD OVERRIDE PUBLIC CHARACTER Describe():
        RETURN "leaf of " + Depth() + " of " + SUPER:Describe().
    END METHOD.
END CLASS.
