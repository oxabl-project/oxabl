// SYNTAX TEST "source.abl" "labels, terminators and separators"

    main-blk: DO:
//  ^^^^^^^^ entity.name.label.abl
//          ^ punctuation.separator.label.abl
//            ^^ keyword.control.abl
//            ^^ - entity.name.label.abl
//              ^ punctuation.separator.colon.abl
    blk: FOR EACH t NO-LOCK:
//  ^^^ entity.name.label.abl
//       ^^^ keyword.control.abl
    outer: REPEAT:
//  ^^^^^ entity.name.label.abl
    DO:
//  ^^ keyword.control.abl
//  ^^ - entity.name.label.abl
//    ^ punctuation.separator.colon.abl
    REPEAT:
//  ^^^^^^ keyword.control.abl
//  ^^^^^^ - entity.name.label.abl
    FINALLY:
//  ^^^^^^^ keyword.control.abl
//  ^^^^^^^ - entity.name.label.abl
    CASE x:
//        ^ punctuation.separator.colon.abl
    FOR EACH t:
//            ^ punctuation.separator.colon.abl
//           ^ - entity.name.label.abl
    IF x THEN DO:
//              ^ punctuation.separator.colon.abl
    x = h:NAME.
//      ^ - entity.name.label.abl
//      ^ variable.other.abl
    lbl:
//  ^^^ entity.name.label.abl
    LEAVE lbl.
//        ^^^ - entity.name.label.abl
//        ^^^ variable.other.abl

    x = 1.
//       ^ punctuation.terminator.statement.abl
    x = 1. y = 2.
//       ^ punctuation.terminator.statement.abl
//              ^ punctuation.terminator.statement.abl
    x = "a".
//         ^ punctuation.terminator.statement.abl
    x = a.b.
//       ^ punctuation.accessor.abl
//         ^ punctuation.terminator.statement.abl
    x = a.
//       ^ punctuation.terminator.statement.abl
//       ^ - punctuation.accessor.abl
    x = 1.5.
//       ^ - punctuation.terminator.statement.abl

    RUN p (INPUT 1, OUTPUT y).
//                ^ punctuation.separator.comma.abl
    x = SUBSTRING(a, 1, 2).
//                 ^ punctuation.separator.comma.abl
//                    ^ punctuation.separator.comma.abl
    DEFINE VARIABLE a AS INTEGER NO-UNDO:
//                                      ^ punctuation.separator.colon.abl

