module Parse.Dont.Validate.Parser exposing
    ( Parser, all, andThen, by, contramap, fail, fromReader, map, mapError
    , merge, ok, run, succeed, validateBy, fromPredicate, zip, note
    )

{-| This library provides a bunch of parser combinators
and could be used for Web form validation.

It follows ideas described here:
<https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validateBy/>

@docs Parser, all, andThen, by, contramap, fail, fromReader, map, mapError
@docs merge, ok, run, succeed, validateBy, fromPredicate, zip, note

-}

import Cons as Cons exposing (Cons)


{-| Parser type

inp Input type (what is parsed/validated)
err Error type (Could be your custom error type)
out Output type (Could be your domain type)

-}
type alias Parser inp err out =
    inp -> Result (Cons err) out


{-| Create Parser from function of type `inp -> Maybe out`
-}
fromReader : (inp -> Maybe out) -> err -> Parser inp err out
fromReader reader error =
    reader >> Result.fromMaybe (Cons.singleton error)


{-| Create Parser from predicate function that returns a wrapped input value.
Parser fails with provided error in case if predicate evaluates to False.
-}
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


{-| Maps over parser result
-}
map : (out0 -> out1) -> Parser inp err out0 -> Parser inp err out1
map =
    dimap identity


{-| Given a parser without error decorates it with an error provided
-}
note : err -> Parser inp () out -> Parser inp err out
note =
    mapError << always


{-| Maps over parser error
-}
mapError : (err0 -> err1) -> Parser inp err0 out -> Parser inp err1 out
mapError f =
    trimap identity f identity


{-| Maps over Parser input
-}
contramap : (inp1 -> inp0) -> Parser inp0 err out -> Parser inp1 err out
contramap f =
    dimap f identity


{-| Parser composition: result produced by second parser is fed to the first
-}
andThen : Parser a err b -> Parser inp err a -> Parser inp err b
andThen f p inp =
    Result.andThen f (p inp)


{-| When given a function that merges parsing results
and two parsers producing such results - returns a parser
that produces a result of that function.
-}
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


{-| Given 2 parsers, feeds same input into both of them
and returns results as tuple
-}
zip : Parser inp err a -> Parser inp err b -> Parser inp err ( a, b )
zip =
    merge Tuple.pair


{-| Given a non-empty list of uniform parsers returns a parser
that produces a non empty list of their results.
If any of the provided parsers fails - resulting parser also fails.
-}
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


{-| Useful for a "pipeline" style:

    example : Parser { bar : String, baz : String } Error Foo
    example =
        succeed (\a b -> Foo a b)
            |> by .bar parseBar
            |> by .baz parseBaz

-}
by :
    (inp1 -> inp0)
    -> Parser inp0 err out
    -> Parser inp1 err (out -> b)
    -> Parser inp1 err b
by f p =
    merge (|>) (contramap f p)


{-| Useful for a "pipeline" style when validator emits an error message but
reurns () in order not to consume constructor argument:

    example : Parser { bar: String, baz: String } Error Foo
    example =
        succeed (\bar baz -> Foo bar baz)
            |> by .bar parseBar
            |> by .baz parseBaz
            |> validateBy identity barBazParser

    barBazParser : { bar: String, baz: String } Error Unit
    barBazParser { bar, baz } =
        if bar /= baz
            then Parser.fail "Bar != Baz"
            else Parser.ok ()

-}
validateBy :
    (inp1 -> inp0)
    -> Parser inp0 err ()
    -> Parser inp1 err b
    -> Parser inp1 err b
validateBy f p =
    by f p << map always


{-| Useful for a "pipeline" style:

    example : Parser { bar : String, baz : String } Error Foo
    example =
        succeed (\a b -> Foo a b)
            |> by .bar parseBar
            |> by .baz parseBaz
            |> validateBy identity barBazParser

-}
succeed : out -> Parser input err out
succeed =
    always << Ok


{-| Returns successfully parsed value
-}
ok : out -> Result (Cons err) out
ok =
    Ok


{-| Returns error
-}
fail : err -> Result (Cons err) out
fail =
    Err << Cons.singleton


{-| Syntactic abstraction: applies parser to its input
-}
run : Parser input err out -> input -> Result (Cons err) out
run =
    identity
