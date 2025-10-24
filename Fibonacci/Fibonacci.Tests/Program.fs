module Fibonacci.Tests

open NUnit.Framework
open Fibonacci

[<Test>]
let ``Fibonacci of 0 should be 0`` () =
    match fibonacci 0 with
    | Ok value -> Assert.That(value, Is.EqualTo(0I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 1 should be 1`` () =
    match fibonacci 1 with
    | Ok value -> Assert.That(value, Is.EqualTo(1I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 2 should be 1`` () =
    match fibonacci 2 with
    | Ok value -> Assert.That(value, Is.EqualTo(1I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 3 should be 2`` () =
    match fibonacci 3 with
    | Ok value -> Assert.That(value, Is.EqualTo(2I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 4 should be 3`` () =
    match fibonacci 4 with
    | Ok value -> Assert.That(value, Is.EqualTo(3I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 50 should work with bigint`` () =
    match fibonacci 50 with
    | Ok value -> Assert.That(value, Is.EqualTo(12586269025I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of 100 should work with bigint`` () =
    match fibonacci 100 with
    | Ok value -> Assert.That(value, Is.EqualTo(354224848179261915075I))
    | Error msg -> Assert.Fail($"Expected Ok but got Error: {msg}")

[<Test>]
let ``Fibonacci of -1 should return Error`` () =
    match fibonacci -1 with
    | Error msg -> Assert.That(msg, Is.EqualTo("n must be non-negative integer"))
    | Ok value -> Assert.Fail($"Expected Error but got Ok: {value}")

[<EntryPoint>]
let main _ = 0