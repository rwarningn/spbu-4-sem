module Factorial.Tests

open NUnit.Framework
open FsUnit
open Factorial

[<Test>]
let ``Factorial of 0 should be 1`` () =
    let result = factorial 0
    result |> should equal (Some 1I)

[<Test>]
let ``Factorial of 1 should be 1`` () =
    let result = factorial 1
    result |> should equal (Some 1I)

[<Test>]
let ``Factorial of 5 should be 120`` () =
    let result = factorial 5
    result |> should equal (Some 120I)

[<Test>]
let ``Factorial of 10 should be 3628800`` () =
    let result = factorial 10
    result |> should equal (Some 3628800I)

[<Test>]
let ``Factorial of negative number should return None`` () =
    let result = factorial -1
    result |> should equal None

[<Test>]
let ``Factorial of -5 should return None`` () =
    let result = factorial -5
    result |> should equal None

[<Test>]
let ``Factorial of 20 should work with bigint`` () =
    let result = factorial 20
    result |> should equal (Some 2432902008176640000I)

[<Test>]
let ``Factorial of 2 should be 2`` () =
    let result = factorial 2
    result |> should equal (Some 2I)

[<Test>]
let ``Factorial of 3 should be 6`` () =
    let result = factorial 3
    result |> should equal (Some 6I)

[<Test>]
let ``Factorial of 4 should be 24`` () =
    let result = factorial 4
    result |> should equal (Some 24I)

[<Test>]
let ``Factorial of 6 should be 720`` () =
    let result = factorial 6
    result |> should equal (Some 720I)

[<Test>]
let ``Factorial of -10 should return None`` () =
    let result = factorial -10
    result |> should equal None

[<Test>]
let ``Factorial of -100 should return None`` () =
    let result = factorial -100
    result |> should equal None
    
[<EntryPoint>]
let main _ = 0
