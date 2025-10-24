module ListReversal.Tests

open NUnit.Framework
open FsUnit
open ListReversal

[<Test>]
let ``Reverse empty list should return empty list`` () =
    let result = reverse []
    result |> should equal []

[<Test>]
let ``Reverse single element list`` () =
    let result = reverse [1]
    result |> should equal [1]

[<Test>]
let ``Reverse two element list`` () =
    let result = reverse [1; 2]
    result |> should equal [2; 1]

[<Test>]
let ``Reverse three element list`` () =
    let result = reverse [1; 2; 3]
    result |> should equal [3; 2; 1]

[<Test>]
let ``Reverse list with five elements`` () =
    let result = reverse [1; 2; 3; 4; 5]
    result |> should equal [5; 4; 3; 2; 1]

[<Test>]
let ``Reverse list with strings`` () =
    let result = reverse ["a"; "b"; "c"; "d"]
    result |> should equal ["d"; "c"; "b"; "a"]

[<Test>]
let ``Reverse list with duplicate elements`` () =
    let result = reverse [1; 2; 2; 3; 1]
    result |> should equal [1; 3; 2; 2; 1]

[<Test>]
let ``Reverse large list`` () =
    let original = [1..100]
    let result = reverse original
    result |> should equal [100..-1..1]

[<Test>]
let ``Reverse twice returns original`` () =
    let original = [1; 2; 3; 4; 5]
    let result = reverse (reverse original)
    result |> should equal original

[<Test>]
let ``Reverse list with negative numbers`` () =
    let result = reverse [-3; -1; 0; 2; 5]
    result |> should equal [5; 2; 0; -1; -3]


[<EntryPoint>]
let main _ = 0
