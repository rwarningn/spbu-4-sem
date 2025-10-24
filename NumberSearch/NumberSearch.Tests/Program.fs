module NumberSearch.Tests

open NUnit.Framework
open FsUnit
open NumberSearch

[<Test>]
let ``Find first element in list`` () =
    let result = findFirst [1; 2; 3; 4; 5] 3
    result |> should equal (Some 2)

[<Test>]
let ``Find first element at beginning`` () =
    let result = findFirst [1; 2; 3; 4; 5] 1
    result |> should equal (Some 0)

[<Test>]
let ``Find first element at end`` () =
    let result = findFirst [1; 2; 3; 4; 5] 5
    result |> should equal (Some 4)

[<Test>]
let ``Find element not in list`` () =
    let result = findFirst [1; 2; 3; 4; 5] 10
    result |> should equal None

[<Test>]
let ``Find in empty list`` () =
    let result = findFirst [] 1
    result |> should equal None

[<Test>]
let ``Find first occurrence when duplicates exist`` () =
    let result = findFirst [1; 2; 3; 2; 5] 2
    result |> should equal (Some 1)

[<Test>]
let ``Find in single element list - found`` () =
    let result = findFirst [42] 42
    result |> should equal (Some 0)

[<Test>]
let ``Find in single element list - not found`` () =
    let result = findFirst [42] 10
    result |> should equal None

[<Test>]
let ``Find string in list of strings`` () =
    let result = findFirst ["hello"; "world"; "test"] "world"
    result |> should equal (Some 1)

[<Test>]
let ``Find with multiple duplicates returns first`` () =
    let result = findFirst [5; 5; 5; 5] 5
    result |> should equal (Some 0)

[<Test>]
let ``Find negative number`` () =
    let result = findFirst [-3; -1; 0; 2; 5] -1
    result |> should equal (Some 1)

[<EntryPoint>]
let main _ = 0