// SYNTAX TEST "source.abl" "word boundaries: a name, keyword, number or directive after ², ³, ¹, ¼, ½ or ¾"

// Oniguruma's \b takes these six characters as word characters and its [\w] does not.
// The grammar starts a token after them, as it does after a space.
    x = a²b ¼DO ½1 ³&IF ¹.5 ¾c(1).
//      ^ variable.other.abl
//        ^ variable.other.abl
//           ^^ keyword.control.abl
//               ^ constant.numeric.integer.abl
//                  ^^^ keyword.control.directive.conditional.abl
//                       ^^ constant.numeric.decimal.abl
//                           ^ entity.name.function.abl
