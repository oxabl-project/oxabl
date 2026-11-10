// SYNTAX TEST "source.abl" "numbers and constants: integer, decimal, date, unknown, booleans"

    x = 10.
//      ^^ constant.numeric.integer.abl
//        ^ punctuation.terminator.statement.abl
    x = 1.
//      ^ constant.numeric.integer.abl - constant.numeric.decimal.abl
//       ^ punctuation.terminator.statement.abl
    x = 1. y = 2.
//      ^ constant.numeric.integer.abl
//       ^ punctuation.terminator.statement.abl
//             ^ constant.numeric.integer.abl
    x = 1.5.
//      ^^^ constant.numeric.decimal.abl
//         ^ punctuation.terminator.statement.abl
    x = .5.
//      ^^ constant.numeric.decimal.abl
    x = 123456789012.
//      ^^^^^^^^^^^^ constant.numeric.integer.abl
    x = 0.25 + 3.
//      ^^^^ constant.numeric.decimal.abl
//             ^ constant.numeric.integer.abl
    x = 12/31/2026.
//      ^^^^^^^^^^ constant.numeric.date.abl
    x = 1/2/26.
//      ^^^^^^ constant.numeric.date.abl
    x = a / b.
//        ^ - constant.numeric.date.abl
//        ^ keyword.operator.arithmetic.abl
    x = 10 / 2.
//         ^ keyword.operator.arithmetic.abl
//      ^^ - constant.numeric.date.abl
    x = a/b/c.
//      ^^^^^ - constant.numeric.date.abl

    x = abc123.
//         ^^^ - constant.numeric.integer.abl constant.numeric.decimal.abl constant.numeric.date.abl
//      ^^^^^^ variable.other.abl
    x = a-1.
//        ^ - constant.numeric.integer.abl constant.numeric.decimal.abl constant.numeric.date.abl
    x = var1.
//         ^ - constant.numeric.integer.abl constant.numeric.decimal.abl constant.numeric.date.abl

    x = ?.
//      ^ constant.language.unknown.abl
    IF x = ? THEN y = ?.
//         ^ constant.language.unknown.abl
//                    ^ constant.language.unknown.abl

    x = TRUE.
//      ^^^^ constant.language.boolean.abl
    x = FALSE.
//      ^^^^^ constant.language.boolean.abl
    x = YES.
//      ^^^ constant.language.boolean.abl
    x = NO.
//      ^^ constant.language.boolean.abl
    x = true.
//      ^^^^ constant.language.boolean.abl
    x = Yes.
//      ^^^ constant.language.boolean.abl
    x = no-yes.
//      ^^ - constant.language.boolean.abl
//         ^^^ - constant.language.boolean.abl
    x = my-true.
//         ^^^^ - constant.language.boolean.abl
    x = trueish.
//      ^^^^^^^ - constant.language.boolean.abl
    x = true-value.
//      ^^^^ - constant.language.boolean.abl
    x = customer.no.
//               ^^ - constant.language.boolean.abl
    x = no.name.
//      ^^ - constant.language.boolean.abl

