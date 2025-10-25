module EvenNumbers.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open EvenNumbers

[<Test>]
let ``Filter and Map are equivalent`` () =
    let property (list: int list) =
        countWithFilter list = countWithMap list
    Check.QuickThrowOnFailure property

[<Test>]
let ``Filter and Fold are equivalent`` () =
    let property (list: int list) =
        countWithFilter list = countWithFold list
    Check.QuickThrowOnFailure property

[<Test>]
let ``Map and Fold are equivalent`` () =
    let property (list: int list) =
        countWithMap list = countWithFold list
    Check.QuickThrowOnFailure property

[<Test>]
let ``All three functions are equivalent`` () =
    let property (list: int list) =
        let filterResult = countWithFilter list
        let mapResult = countWithMap list
        let foldResult = countWithFold list
        filterResult = mapResult && mapResult = foldResult
    Check.QuickThrowOnFailure property

[<Test>]
let ``Count even numbers in simple list`` () =
    let input = [1; 2; 3; 4; 5; 6]
    countWithFilter input |> should equal 3
    countWithMap input |> should equal 3
    countWithFold input |> should equal 3

[<Test>]
let ``Count even numbers in empty list`` () =
    let input = []
    countWithFilter input |> should equal 0
    countWithMap input |> should equal 0
    countWithFold input |> should equal 0

[<Test>]
let ``Count even numbers - all even`` () =
    let input = [2; 4; 6; 8]
    countWithFilter input |> should equal 4
    countWithMap input |> should equal 4
    countWithFold input |> should equal 4

[<Test>]
let ``Count even numbers - all odd`` () =
    let input = [1; 3; 5; 7]
    countWithFilter input |> should equal 0
    countWithMap input |> should equal 0
    countWithFold input |> should equal 0

[<Test>]
let ``Count even numbers with negative numbers`` () =
    let input = [-4; -3; -2; -1; 0; 1; 2]
    countWithFilter input |> should equal 4
    countWithMap input |> should equal 4
    countWithFold input |> should equal 4

[<Test>]
let ``Count even numbers - single element even`` () =
    let input = [42]
    countWithFilter input |> should equal 1
    countWithMap input |> should equal 1
    countWithFold input |> should equal 1

[<Test>]
let ``Count even numbers - single element odd`` () =
    let input = [41]
    countWithFilter input |> should equal 0
    countWithMap input |> should equal 0
    countWithFold input |> should equal 0

[<EntryPoint>]
let main _ = 0
