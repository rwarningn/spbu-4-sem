module Fibonacci.Tests

open NUnit.Framework
open FsUnit
open Fibonacci

[<Test>]
let ``Fibonacci of 0 should be 0`` () =
    let result = fibonacci 0
    result |> should equal (Ok 0I)

[<Test>]
let ``Fibonacci of 1 should be 1`` () =
    let result = fibonacci 1
    result |> should equal (Ok 1I)

[<Test>]
let ``Fibonacci of 2 should be 1`` () =
    let result = fibonacci 2
    result |> should equal (Ok 1I)

[<Test>]
let ``Fibonacci of 3 should be 2`` () =
    let result = fibonacci 3
    result |> should equal (Ok 2I)

[<Test>]
let ``Fibonacci of 4 should be 3`` () =
    let result = fibonacci 4
    result |> should equal (Ok 3I)

[<Test>]
let ``Fibonacci of 50 should work with bigint`` () =
    let result = fibonacci 50
    result |> should equal (Ok 12586269025I)

[<Test>]
let ``Fibonacci of 100 should work with bigint`` () =
    let result = fibonacci 100
    result |> should equal (Ok 354224848179261915075I)

[<Test>]
let ``Fibonacci of -1 should return Error`` () =
    let result = fibonacci -1
    match result with
    | Error msg -> msg |> should equal "n must be non-negative integer"
    | _ -> failwith "Should return Error"


[<EntryPoint>]
let main _ = 0
