module Parse.Dont.Validate.Parser exposing
    ( Parser
    , all
    , andThen
    , by
    , contramap
    , fail
    , fromReader
    , map
    , mapError
    , merge
    , ok
    , run
    , succeed
    , validateBy
    , fromPredicate
    , zip
    , note
    )

-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validateBy/

import Cons as Cons exposing (Cons)

type alias Parser inp err out =
    inp -> Result (Cons err) out


fromReader : (inp -> Maybe out) -> err -> Parser inp err out
fromReader reader error =
    reader >> Result.fromMaybe (Cons.singleton error)


fromPredicate : (inp -> Bool) -> err -> (inp -> out) -> Parser inp err out
fromPredicate predicate err constructor inp =
    if predicate inp then
        ok (constructor inp)

    else
        fail err


trimap :
    (inp1 -> inp0)
    -> (err0 -> err1)
    -> (out0 -> out1)
    -> Parser inp0 err0 out0
    -> Parser inp1 err1 out1
trimap before error after parser =
    Result.mapError (Cons.map error)
        << Result.map after
        << parser
        << before


dimap :
    (inp1 -> inp0)
    -> (out0 -> out1)
    -> Parser inp0 err out0
    -> Parser inp1 err out1
dimap before =
    trimap before identity


map : (out0 -> out1) -> Parser inp err out0 -> Parser inp err out1
map =
    dimap identity

note : err -> Parser inp () out -> Parser inp err out
note  = mapError << always 

mapError : (err0 -> err1) -> Parser inp err0 out -> Parser inp err1 out
mapError f =
    trimap identity f identity


contramap : (inp1 -> inp0) -> Parser inp0 err out -> Parser inp1 err out
contramap f =
    dimap f identity


andThen : Parser a err b -> Parser inp err a -> Parser inp err b
andThen f p inp =
    Result.andThen f (p inp)


merge : (a -> b -> c) -> Parser inp err a -> Parser inp err b -> Parser inp err c
merge join pa pb inp =
    case pa inp of
        Err errorsA ->
            case pb inp of
                Err errorsB ->
                    Err (Cons.append errorsA errorsB)

                Ok _ ->
                    Err errorsA

        Ok resA ->
            case pb inp of
                Err errorsB ->
                    Err errorsB

                Ok resB ->
                    Ok (join resA resB)


zip : Parser inp err a -> Parser inp err b -> Parser inp err ( a, b )
zip =
    merge Tuple.pair


all : Cons (Parser inp err a) -> Parser inp err (Cons a)
all parsers inp =
    let
        f :
            Result (Cons err) (Cons a)
            -> Result (Cons err) (Cons a)
            -> Result (Cons err) (Cons a)
        f r acc =
            case ( r, acc ) of
                ( Ok a, Ok aa ) ->
                    Ok (Cons.append a aa)

                ( Ok _, Err e ) ->
                    Err e

                ( Err e, Ok _ ) ->
                    Err e

                ( Err e, Err es ) ->
                    Err (Cons.append e es)
    in
    Cons.foldl1 f
        (Cons.map (\p -> Result.map Cons.singleton (p inp)) parsers)


by :
    (inp1 -> inp0)
    -> Parser inp0 err out
    -> Parser inp1 err (out -> b)
    -> Parser inp1 err b
by f p =
    merge (|>) (contramap f p)


validateBy :
    (inp1 -> inp0)
    -> Parser inp0 err ()
    -> Parser inp1 err b
    -> Parser inp1 err b
validateBy f p =
    by f p << map always


succeed : out -> Parser input err out
succeed =
    always << Ok


ok : out -> Result (Cons err) out
ok =
    Ok


fail : err -> Result (Cons err) out
fail =
    Err << Cons.singleton


run : Parser input err out -> input -> Result (Cons err) out
run =
    identity
