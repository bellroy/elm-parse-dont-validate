module Parse.Dont.Validate.String exposing
    ( nonEmpty
    , uncons
    , int
    , float
    )

{-| Set of primitive string parsers

@docs nonEmpty
@docs uncons
@docs int
@docs float

-}

import Parse.Dont.Validate.Parser as Parser exposing (Parser)


{-| Parse a string returning unit as an error if input is empty
-}
nonEmpty : Parser String () String
nonEmpty =
    Parser.fromPredicate (not << String.isEmpty) () identity


{-| Parse a string returning unit as an error if input is empty
or tuple of first character and the rest of the string otherwise.
-}
uncons : Parser String () ( Char, String )
uncons =
    Parser.fromReader String.uncons ()


{-| Parse a string returning unit as an error or Int as a result.
-}
int : Parser String () Int
int =
    Parser.fromReader String.toInt ()


{-| Parse a string returning unit as an error or Float as a result.
-}
float : Parser String () Float
float =
    Parser.fromReader String.toFloat ()
