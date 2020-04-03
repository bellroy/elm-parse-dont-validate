module Voucher exposing
    ( Voucher(..)
    , VoucherType(..)
    , Error(..)
    , unsafeVoucherCode
    , voucherParser
    )

import Parse.Dont.Validate.Parser as Parser
    exposing
        ( Parser
        , by
        , fail
        , note
        , ok
        , succeed
        )
import Parse.Dont.Validate.String exposing (uncons)


type Voucher
    = Voucher VoucherType VoucherCode


type VoucherType
    = GiftCard
    | Certificate
    | Promotion


type VoucherCode
    = Code String


unsafeVoucherCode : String -> VoucherCode
unsafeVoucherCode =
    Code


type Error
    = CodeEmpty
    | InvalidTypeCode Char


voucherParser : Parser String Error Voucher
voucherParser =
    nonEmptyCode
        |> Parser.andThen
            (succeed Voucher
                |> by Tuple.first parseVoucherType
                |> by Tuple.second parseVoucherCode
            )


nonEmptyCode : Parser String Error ( Char, String )
nonEmptyCode =
    note CodeEmpty uncons


parseVoucherType : Parser Char Error VoucherType
parseVoucherType c =
    case c of
        'G' ->
            ok GiftCard

        'C' ->
            ok Certificate

        'P' ->
            ok Promotion

        chr ->
            fail (InvalidTypeCode chr)


parseVoucherCode : Parser String Error VoucherCode
parseVoucherCode =
    Parser.fromPredicate (not << String.isEmpty) CodeEmpty Code
