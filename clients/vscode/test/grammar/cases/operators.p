// SYNTAX TEST "source.abl" "operators: symbolic operators and what they are not"

    x = a.
//    ^ keyword.operator.assignment.abl
    IF a <> b THEN.
//       ^^ keyword.operator.comparison.abl
    IF a <= b THEN.
//       ^^ keyword.operator.comparison.abl
    IF a >= b THEN.
//       ^^ keyword.operator.comparison.abl
    IF a < b THEN.
//       ^ keyword.operator.comparison.abl
    IF a > b THEN.
//       ^ keyword.operator.comparison.abl
    x = a + b.
//        ^ keyword.operator.arithmetic.abl
    x = a - b.
//        ^ keyword.operator.arithmetic.abl
    x = a * b.
//        ^ keyword.operator.arithmetic.abl
    x = a / b.
//        ^ keyword.operator.arithmetic.abl
    x += 1.
//    ^^ keyword.operator.assignment.compound.abl
    x -= 1.
//    ^^ keyword.operator.assignment.compound.abl
    x *= 2.
//    ^^ keyword.operator.assignment.compound.abl
    x /= 2.
//    ^^ keyword.operator.assignment.compound.abl
    x = a <> b.
//        ^^ keyword.operator.comparison.abl
//        ^^ - keyword.operator.assignment.abl
    x = 1.
//    ^ keyword.operator.assignment.abl
    x += 1.
//    ^^ keyword.operator.assignment.compound.abl
    x = a + b.
//        ^ keyword.operator.arithmetic.abl
    x = a-b.
//      ^^^ variable.other.abl
//       ^ - keyword.operator.arithmetic.abl
    x = a - b.
//      ^ variable.other.abl
//          ^ variable.other.abl
    x = (a + b) * 2.
//      ^ punctuation.section.parens.begin.abl
//            ^ punctuation.section.parens.end.abl
//              ^ keyword.operator.arithmetic.abl
    x = "a" + "b".
//          ^ keyword.operator.arithmetic.abl - string.quoted.double.abl

