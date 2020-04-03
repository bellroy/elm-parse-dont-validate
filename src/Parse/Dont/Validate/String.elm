module Parse.Dont.Validate.String exposing
    ( float
    , int
    , nonEmpty
    , uncons
    )

import Parse.Dont.Validate.Parser as Parser exposing (Parser)


nonEmpty : Parser String () String
nonEmpty =
    Parser.fromPredicate (not << String.isEmpty) () identity


uncons : Parser String () ( Char, String )
uncons =
    Parser.fromReader String.uncons ()


int : Parser String () Int
int =
    Parser.fromReader String.toInt ()


float : Parser String () Float
float =
    Parser.fromReader String.toFloat ()
