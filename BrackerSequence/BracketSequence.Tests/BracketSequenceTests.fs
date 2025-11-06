module BracketSequence.Tests

open NUnit.Framework
open FsUnit
open BracketSequence

[<TestCase("", true)>]
[<TestCase("()", true)>]
[<TestCase("[]", true)>]
[<TestCase("{}", true)>]
[<TestCase("()[]{}", true)>]
[<TestCase("({[]})", true)>]
[<TestCase("((((()))))", true)>]
[<TestCase("({()[]{}})", true)>]
[<TestCase("(]", false)>]
[<TestCase("([)]", false)>]
[<TestCase("())", false)>]
[<TestCase("(()", false)>]
[<TestCase("(", false)>]
[<TestCase(")", false)>]
[<TestCase("(((", false)>]
[<TestCase(")))", false)>]
[<TestCase("a(b[c]{d}e)f", true)>]
[<TestCase("func(x, arr[0], {key: val})", true)>]
[<TestCase("func(x, arr[0}, {key: val)", false)>]
[<TestCase("( [ { } ] )", true)>]
[<TestCase("{[}]", false)>]
[<TestCase("([{()}][])", true)>]
[<TestCase("([{()][])", false)>]
let ``checkBrackets returns correct result`` (input: string) (expected: bool) =
    checkBrackets input |> should equal expected

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