module ParsingTree.Tests

open NUnit.Framework
open FsUnit
open ParsingTree

[<Test>]
let ``Eval constant should return value`` () =
    let expr = Const 5.0
    let result = eval expr
    result |> should equal (Some 5.0)

[<Test>]
let ``Eval plus should add two numbers`` () =
    let expr = Plus(Const 2.0, Const 3.0)
    let result = eval expr
    result |> should equal (Some 5.0)

[<Test>]
let ``Eval minus should subtract two numbers`` () =
    let expr = Minus(Const 5.0, Const 3.0)
    let result = eval expr
    result |> should equal (Some 2.0)

[<Test>]
let ``Eval mul should multiply two numbers`` () =
    let expr = Mul(Const 2.0, Const 3.0)
    let result = eval expr
    result |> should equal (Some 6.0)

[<Test>]
let ``Eval div should divide two numbers`` () =
    let expr = Div(Const 6.0, Const 3.0)
    let result = eval expr
    result |> should equal (Some 2.0)

[<Test>]
let ``Eval UMinus should negate number`` () =
    let expr = UMinus(Const 5.0)
    let result = eval expr
    result |> should equal (Some -5.0)

[<Test>]
let ``Eval division by zero should return None`` () =
    let expr = Div(Const 6.0, Const 0.0)
    let result = eval expr
    result |> should equal None

[<Test>]
let ``Eval division by very small number should calculate correctly`` () =
    let expr = Div(Const 6.0, Const 1e-15)
    let result = eval expr
    result |> should equal (Some 6.0e15)

[<Test>]
let ``Eval nested expression`` () =
    // (2 + 3) * 4 = 20
    let expr = Mul(Plus(Const 2.0, Const 3.0), Const 4.0)
    let result = eval expr
    result |> should equal (Some 20.0)

[<Test>]
let ``Eval complex expression`` () =
    // ((10 - 5) * 2) / 5 = 2
    let expr = Div(Mul(Minus(Const 10.0, Const 5.0), Const 2.0), Const 5.0)
    let result = eval expr
    result |> should equal (Some 2.0)

[<Test>]
let ``Eval expression with UMinus`` () =
    // -(5 + 3) = -8
    let expr = UMinus(Plus(Const 5.0, Const 3.0))
    let result = eval expr
    result |> should equal (Some -8.0)

[<Test>]
let ``Eval complex nested expression`` () =
    // (2 * 3) + (10 / 5) - 1 = 6 + 2 - 1 = 7
    let expr = 
        Minus(
            Plus(Mul(Const 2.0, Const 3.0), Div(Const 10.0, Const 5.0)),
            Const 1.0)
    let result = eval expr
    result |> should equal (Some 7.0)

[<Test>]
let ``Eval nested division by zero returns None`` () =
    // (5 + 3) / (2 - 2) = 8 / 0 = None
    let expr = Div(Plus(Const 5.0, Const 3.0), Minus(Const 2.0, Const 2.0))
    let result = eval expr
    result |> should equal None

[<Test>]
let ``Eval expression with multiple operations`` () =
    // ((5 + 3) * 2) - (10 / 2) = 16 - 5 = 11
    let expr = 
        Minus(
            Mul(Plus(Const 5.0, Const 3.0), Const 2.0),
            Div(Const 10.0, Const 2.0))
    let result = eval expr
    result |> should equal (Some 11.0)

[<Test>]
let ``Eval negative result`` () =
    // 5 - 10 = -5
    let expr = Minus(Const 5.0, Const 10.0)
    let result = eval expr
    result |> should equal (Some -5.0)

[<Test>]
let ``Eval very large numbers should return Infinity`` () =
    let expr = Mul(Const 1.0e308, Const 2.0)
    let result = eval expr
    result |> should equal (Some System.Double.PositiveInfinity)

[<Test>]
let ``Eval 0 divided by 0 should return None (NaN propagation)`` () =
    let expr = Div(Const 0.0, Const 0.0)
    let result = eval expr
    result |> should equal None

[<Test>]
let ``Eval 1 divided by 0 should return None (infinity handling)`` () =
    let expr = Div(Const 1.0, Const 0.0)
    let result = eval expr
    result |> should equal None

[<Test>]
let ``Eval very small numbers should not be treated as zero`` () =
    let expr = Div(Const 1e-300, Const 1e-300)
    let result = eval expr
    result |> should equal (Some 1.0)

[<Test>]
let ``Eval UMinus of zero should return negative zero`` () =
    let expr = UMinus(Const 0.0)
    let result = eval expr
    result |> should equal (Some -0.0)