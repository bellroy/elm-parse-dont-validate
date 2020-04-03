# Parsing (Validation) done right for Elm

> ... in my mind, the difference between validation and parsing  
lies almost entirely in how information is preserved. 

[Lexi Lambda ©](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

## Basic types

Lets look at the `Parser` type and dissect it:

```elm
type alias Parser inp err out =
    inp -> Result (Cons err) out
```

Type parameter|Meaning|Example
:------------:|-----------|-------
`inp`|Input type<br/>  (what is parsed/validated)| User input typically its a `String`
`err`|Error type| Could be your custom error type<br/>  `type Error = Empty | Blank`
`out`|Output type| Could be your domain type<br/>  `type Email = Email String`


Graphically:
```
 inp ────────┬──────── ▶ Ok out
             │
             ▼
      Err (Cons err)
```

(`Cons` is a non-empty list from [this library](https://package.elm-lang.org/packages/hrldcpr/elm-cons/3.1.0/))

## Usage

Take a look at this examples: 
* [Voucher.elm](tests/Voucher.elm) module.
* [VoucherTest.elm](tests/VoucherTest.elm) test spec.