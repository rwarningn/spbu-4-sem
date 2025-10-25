module BracketSequence.Tests

open NUnit.Framework
open FsUnit
open BracketSequence

[<Test>]
let ``Empty string is valid`` () =
    checkBrackets "" |> should equal true

[<Test>]
let ``Single pair of round brackets`` () =
    checkBrackets "()" |> should equal true

[<Test>]
let ``Single pair of square brackets`` () =
    checkBrackets "[]" |> should equal true

[<Test>]
let ``Single pair of curly brackets`` () =
    checkBrackets "{}" |> should equal true

[<Test>]
let ``Multiple pairs in sequence`` () =
    checkBrackets "()[]{}" |> should equal true

[<Test>]
let ``Nested brackets`` () =
    checkBrackets "({[]})" |> should equal true

[<Test>]
let ``Deeply nested brackets`` () =
    checkBrackets "((((()))))" |> should equal true

[<Test>]
let ``Complex nested structure`` () =
    checkBrackets "({()[]{}})" |> should equal true

[<Test>]
let ``Mismatched bracket types`` () =
    checkBrackets "(]" |> should equal false

[<Test>]
let ``Wrong closing order`` () =
    checkBrackets "([)]" |> should equal false

[<Test>]
let ``Extra closing bracket`` () =
    checkBrackets "())" |> should equal false

[<Test>]
let ``Extra opening bracket`` () =
    checkBrackets "(()" |> should equal false

[<Test>]
let ``Only opening bracket`` () =
    checkBrackets "(" |> should equal false

[<Test>]
let ``Only closing bracket`` () =
    checkBrackets ")" |> should equal false

[<Test>]
let ``Multiple unmatched opening`` () =
    checkBrackets "(((" |> should equal false

[<Test>]
let ``Multiple unmatched closing`` () =
    checkBrackets ")))" |> should equal false

[<Test>]
let ``String with other characters`` () =
    checkBrackets "a(b[c]{d}e)f" |> should equal true

[<Test>]
let ``String with text and valid brackets`` () =
    checkBrackets "func(x, arr[0], {key: val})" |> should equal true

[<Test>]
let ``String with text and invalid brackets`` () =
    checkBrackets "func(x, arr[0}, {key: val)" |> should equal false

[<Test>]
let ``Spaces and brackets`` () =
    checkBrackets "( [ { } ] )" |> should equal true

[<Test>]
let ``Interleaved valid and invalid`` () =
    checkBrackets "{[}]" |> should equal false

[<Test>]
let ``Valid complex expression`` () =
    checkBrackets "([{()}][])" |> should equal true

[<Test>]
let ``Invalid complex expression`` () =
    checkBrackets "([{()][])" |> should equal false

[<Test>]
let ``Long valid sequence`` () =
    let longValid = String.replicate 1000 "()"
    checkBrackets longValid |> should equal true

[<Test>]
let ``Long invalid sequence`` () =
    let longInvalid = "(" + String.replicate 1000 ")"
    checkBrackets longInvalid |> should equal false

[<Test>]
let ``Only non-bracket characters`` () =
    checkBrackets "abcdef123" |> should equal true