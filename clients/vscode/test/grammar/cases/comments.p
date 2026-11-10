// SYNTAX TEST "source.abl" "comments: block (nesting), line, and what they hide"

    /* a /* b */ c */ x = 1.
//  ^^^^^^^^^^^^^^^^^ comment.block.abl
//                    ^ - comment.block.abl comment.line.double-slash.abl
//                    ^ variable.other.abl

    /* one
    DEFINE VARIABLE hidden AS INTEGER.
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.block.abl
//  ^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
    still inside /* nested
    */ also inside */ y = 2.
//     ^^^^^^^^^^^ comment.block.abl
//                    ^ variable.other.abl

    // line comment DEFINE VARIABLE "not a string
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-slash.abl
//                  ^^^^^^ - keyword.control.abl keyword.other.abl keyword.other.define.abl keyword.operator.expression.logical.abl keyword.operator.expression.comparison.abl keyword.operator.expression.pattern.abl keyword.operator.expression.arithmetic.abl storage.type.abl storage.modifier.abl variable.language.abl constant.language.boolean.abl support.function.abl
//                                  ^^^^ - string.quoted.double.abl string.quoted.single.abl

    x = 1. // trailing
//         ^^^^^^^^^^^ comment.line.double-slash.abl
//  ^ variable.other.abl

    MESSAGE "a // b".
//          ^^^^^^^^ string.quoted.double.abl
//             ^^^^ - comment.block.abl comment.line.double-slash.abl

    MESSAGE "a /* b */ c".
//          ^^^^^^^^^^^^^ string.quoted.double.abl
//             ^^^^^^^ - comment.block.abl comment.line.double-slash.abl

    a = b / c.
//        ^ - comment.block.abl comment.line.double-slash.abl
//        ^ keyword.operator.arithmetic.abl

