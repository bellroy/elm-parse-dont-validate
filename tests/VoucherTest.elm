module VoucherTest exposing (voucherSuite)

import Cons as Cons
import Expect
import Parse.Dont.Validate.Parser exposing (run)
import Set
import Test exposing (Test, describe, test)
import Voucher
    exposing
        ( Error(..)
        , Voucher(..)
        , VoucherType(..)
        , unsafeVoucherCode
        , voucherParser
        )


voucherSuite : Test
voucherSuite =
    describe "Voucher tests"
        [ test "Parses valid voucher" <|
            \_ ->
                let
                    voucher =
                        Voucher GiftCard (unsafeVoucherCode "12345")
                in
                run voucherParser "G12345"
                    |> Expect.equal (Ok voucher)
        , test "Rejects invalid voucher types" <|
            \_ ->
                let
                    errors =
                        Cons.singleton (InvalidTypeCode 'X')
                in
                run voucherParser "X12345"
                    |> Expect.equal (Err errors)
        , test "Rejects with all errors" <|
            \_ ->
                let
                    expected =
                        Set.fromList
                            [ Debug.toString (InvalidTypeCode 'X')
                            , Debug.toString CodeEmpty
                            ]
                in
                case run voucherParser "X" of
                    Err actual ->
                        Expect.equalSets expected
                            (Set.fromList <|
                                Cons.toList <|
                                    Cons.map Debug.toString actual
                            )

                    Ok _ ->
                        Expect.fail "Expected error"
        ]
